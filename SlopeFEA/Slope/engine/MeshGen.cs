using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Shapes;
using System.Windows.Controls.Primitives;
using System.IO;
using System.Threading;
using System.Threading.Tasks;
using System.ComponentModel;

namespace SlopeFEA
{
    partial class SlopeCanvas : Canvas
    {
        public static List<fe4NodedQuadElement> MeshGenStructured4NodedQuad (SlopeCanvas canvas, double colWidth, double rowHeight, bool output)
        {
            // load material blocks from parent SlopeCanvas
            List<MaterialBlock> blocks = canvas.MaterialBlocks;

            // initialize lists for mesh gen procedure
            List<feNode> nodes = new List<feNode>(),
                            existBoundNodes = new List<feNode>(),
                            insertBoundNodes = new List<feNode>();
            List<List<feNode>> substructNodes = new List<List<feNode>>();

            // initialize lists for element generation
            List<fe4NodedQuadElement> elements = new List<fe4NodedQuadElement>(),
                                     substructElements = new List<fe4NodedQuadElement>();

            //// initialize list for boundary element generation
            List<fe2NodedBoundElement> boundElements = new List<fe2NodedBoundElement>();

            // get canvas dimensions/properties
            double originX = canvas.OriginOffsetX,
                   originY = canvas.OriginOffsetY,
                   dpiX = canvas.DpiX,
                   dpiY = canvas.DpiY,
                   scale = canvas.Scale,
                   yHeight = canvas.ActualHeight;
            Units units = canvas.Units;

            // get units dependent scaling factor
            double factor;
            switch (units)
            {
                case Units.Metres: factor = 0.0254; break;
                case Units.Millimetres: factor = 25.4; break;
                case Units.Feet: factor = 1.0 / 12.0; break;
                default: factor = 1.0; break;
            }

            // *****************************************
            // create list of feSubstructs from MaterialBlocks
            // *****************************************
            feSubstruct newSubstruct;
            List<feSubstruct> substructs = new List<feSubstruct>(blocks.Count);
            foreach (MaterialBlock block in blocks)
            {
                // create new feSubStruct and insert MaterialType
                newSubstruct = new feSubstruct(block.Material);

                // insert vertices converted to true coords
                foreach (DrawingPoint p in block.BoundaryPoints)
                {
                    newSubstruct.Points.Add(
                        new Point((p.Point.X - originX) / dpiX * factor * scale,
                                    (yHeight - p.Point.Y - originY) / dpiY * factor * scale));

                    newSubstruct.IsFixedX.Add(p.IsFixedX);
                    newSubstruct.IsFixedY.Add(p.IsFixedY);
                }

                // create line constraints
                int index1, index2;
                foreach (LineConstraint lc in block.LineConstraints)
                {
                    index1 = block.BoundaryPoints.FindIndex(delegate(DrawingPoint pt) { return pt == lc.Nodes[0]; });
                    index2 = block.BoundaryPoints.FindIndex(delegate(DrawingPoint pt) { return pt == lc.Nodes[1]; });

                    newSubstruct.LineConstraints.Add(new feLineConstraint(
                        newSubstruct.Points[index1], newSubstruct.Points[index2],
                        lc.IsFixedX, lc.IsFixedY));
                }

                // ensure vertices are ordered counterclockwise
                newSubstruct.SortPoints();

                // add to list of substructs
                substructs.Add(newSubstruct);

            }; // END OF CREATE SUBSTRUCTS FROM MATERIALBLOCKS LOOP
            canvas.FEASubstructs = substructs;

            
            // initialize node and element counters
            int nodeCount = 0, quadElementCount = 0;

            // for merging nodes at the same location
            double tolerDist = 1e-8 * Math.Sqrt(Math.Pow(colWidth, 2) + Math.Pow(rowHeight, 2));
            
            // *****************************************
            // mesh each feSubStruct
            // *****************************************
            foreach (feSubstruct substruct in substructs)
            {
                // create bounding box around substruct
                double xmin = substruct.XMin,
                        xmax = substruct.XMax,
                        ymin = substruct.YMin,
                        ymax = substruct.YMax;

                // compute nearest integer number of rows and columns
                int ncols = Math.Max((int)Math.Round((xmax - xmin) / colWidth), 1),
                    nrows = Math.Max((int)Math.Round((ymax - ymin) / rowHeight), 1);

                // compute actual row and column spacing
                double dx = (xmax - xmin) / ncols,
                        dy = (ymax - ymin) / nrows;


                // **********************************************
                // create grid of nodes and elements
                // **********************************************
                feNode node1, node2, node3, node4;
                int top, bot, lft, rgt;
                for (int i = 0; i <= ncols; i++)
                {
                    // create new column
                    substructNodes.Add(new List<feNode>());
                    for (int j = 0; j <= nrows; j++)
                    {
                        // create new node at appropriate location
                        substructNodes[i].Add(new feNode(++nodeCount, false, xmin + i * dx, ymin + j * dy));

                        // if not on the boundary, create new quad element
                        // NOTE: nodes are added in CCW order
                        if (i > 0 && j > 0)
                        {
                            // node indices
                            lft = i - 1; rgt = i; bot = j - 1; top = j;

                            // create element
                            node1 = substructNodes[lft][bot];
                            node2 = substructNodes[rgt][bot];
                            node3 = substructNodes[rgt][top];
                            node4 = substructNodes[lft][top];
                            substructElements.Add(new fe4NodedQuadElement(substruct, ++quadElementCount,
                                node1, node2, node3, node4,
                                substruct.Material, false));
                        }
                    }
                }   // END OF STRUCTURED MESH GENERATION LOOP


                // **********************************************
                // loop through substruct vertices:
                //      if an existing node is found within 
                //      tolerDist, identify it as a boundary
                //      node and lock it
                //      otherwise, create a boundary node and
                //      add it to the inserted boundary nodes
                //      list
                // **********************************************
                double dist;
                feNode vertexNode;
                bool foundVertex;
                //foreach (Point p in substruct.Points)
                for (int i = 0; i < substruct.Points.Count; i++)
                {
                    // create locked node at the vertex
                    vertexNode = new feNode(++nodeCount, true, substruct.Points[i].X, substruct.Points[i].Y);
                    vertexNode.IsLocked = true;
                    vertexNode.IsFixedX = substruct.IsFixedX[i];
                    vertexNode.IsFixedY = substruct.IsFixedY[i];

                    // see if a node is already at the vertex
                    foundVertex = false;
                    foreach (List<feNode> column in substructNodes)
                    {
                        foreach (feNode node in column)
                        {
                            dist = feNode.Dist(node, vertexNode);

                            if (dist < tolerDist)
                            {
                                node.IsBoundary = true;
                                node.IsLocked = true;
                                node.IsFixedX = substruct.IsFixedX[i];
                                node.IsFixedY = substruct.IsFixedY[i];
                                foundVertex = true;
                                break;
                            }
                        }
                        if (foundVertex) break;
                    }
                    if (!foundVertex)
                    {
                        insertBoundNodes.Add(vertexNode);
                    }

                }   // END OF VERTEX DETECTION/INSERTION LOOP


                // **********************************************
                // loop through substruct line segments finding
                // created nodes that are on each segment
                // and identifying them as boundary nodes and also
                // inserting boundary nodes along the segments
                // and locking them
                // **********************************************
                Point p0 = substruct.Points[substruct.Points.Count - 1], p1;
                double xn, yn, tolerCross = 1e-8;
                feNode boundNode;
                int numNodes;
                bool fixX = substruct.IsFixedX[substruct.IsFixedX.Count - 1],
                    fixY = substruct.IsFixedY[substruct.IsFixedY.Count - 1];
                feLineConstraint lc;
                for (int i = 0; i < substruct.Points.Count; i++)
                {
                    p1 = substruct.Points[i];

                    lc = substruct.LineConstraints.Find(delegate(feLineConstraint l) { return l.Points.Contains(p0) && l.Points.Contains(p1); });

                    fixX = (lc != null) && (lc.IsFixedX);
                    fixY = (lc != null) && (lc.IsFixedY);

                    // find rise and run of line segment
                    dx = p1.X - p0.X;
                    dy = p1.Y - p0.Y;

                    // look for existing nodes that are on the segment
                    foreach (List<feNode> column in substructNodes)
                    {
                        foreach (feNode node in column)
                        {
                            // skip the node if it was already identified
                            // in vertex loop
                            if (node.IsBoundary) continue;

                            xn = node.X;
                            yn = node.Y;

                            // if the node is within the bounding rectangle
                            // of the line segment and the cross product is
                            // within tolerance of zero, the point is on the
                            // line segment
                            if (xn >= Math.Min(p0.X, p1.X) && xn <= Math.Max(p0.X, p1.X)
                                && yn >= Math.Min(p0.Y, p1.Y) && yn <= Math.Max(p0.Y, p1.Y)
                                && Math.Abs((xn - p0.X) * dy - (yn - p0.Y) * dx) < tolerCross)
                            {
                                node.IsBoundary = true;
                                node.IsFixedX = fixX;
                                node.IsFixedY = fixY;
                                existBoundNodes.Add(node);
                            }
                        }
                    }   // END OF EXISTING BOUNDARY NODE DETECTION LOOP

                    dx = Math.Abs(dx); dy = Math.Abs(dy);

                    // compute nearest integer number of nodes on the line segment
                    numNodes = (int)(Math.Max(Math.Max(Math.Round(dx / colWidth), Math.Round(dy / rowHeight)), 1));

                    // grid spacing on boundary line
                    dx = (p1.X - p0.X) / numNodes;
                    dy = (p1.Y - p0.Y) / numNodes;

                    // insert locked boundary nodes
                    for (int j = 1; j < numNodes; j++)
                    {
                        boundNode = new feNode(++nodeCount, true, p0.X + j * dx, p0.Y + j * dy);
                        boundNode.IsLocked = true;
                        boundNode.IsFixedX = fixX;
                        boundNode.IsFixedY = fixY;
                        insertBoundNodes.Add(boundNode);
                    }

                    // step forward
                    p0 = p1;

                }   // END OF BOUNDARY NODE DETECTION/INSERTION LOOP



                // **********************************************
                // while the existing boundary nodes list still
                // contains nodes find the nearest inserted
                // boundary node and merge them, deleting both
                // from the list
                // **********************************************
                double maxDist = Math.Sqrt(Math.Pow(xmax - xmin, 2) + Math.Pow(ymax - ymin, 2));
                feNode nearestNode;
                double nearestDist;
                while (existBoundNodes.Count > 0)
                {
                    nearestDist = maxDist;
                    nearestNode = null;
                    
                    // find the nearest inserted boundary node
                    foreach (feNode node in insertBoundNodes)
                    {
                        dist = feNode.Dist(existBoundNodes[0], node);

                        if (dist < nearestDist)
                        {
                            nearestNode = node;
                            nearestDist = dist;
                        }
                    }

                    // merge the nodes and remove from both boundary node lists
                    existBoundNodes[0].Merge(nearestNode, false);
                    existBoundNodes.RemoveAt(0);
                    insertBoundNodes.Remove(nearestNode);

                }   // END OF EXISTING BOUNDARY NODE MERGE LOOP



                // **********************************************
                // while the inserted boundary nodes list still
                // contains nodes find the nearest existing
                // non-boundary node and merge them
                // **********************************************
                while (insertBoundNodes.Count > 0)
                {
                    boundNode = insertBoundNodes[0];

                    nearestDist = maxDist;
                    nearestNode = null;

                    // find the nearest unlocked non-boundary node
                    foreach (List<feNode> column in substructNodes)
                    {
                        foreach (feNode node in column)
                        {
                            // skip the node if it is a boundary or locked node
                            if (node.IsBoundary || node.IsLocked)
                                continue;

                            dist = feNode.Dist(node, boundNode);

                            if (dist < nearestDist)
                            {
                                nearestNode = node;
                                nearestDist = dist;
                            }
                        }
                    }

                    // merge the nodes and remove from inserted node list
                    nearestNode.Merge(boundNode, false);
                    insertBoundNodes.RemoveAt(0);

                }   // END OF INSERTED BOUND NODE MERGE LOOP



                // **********************************************
                // find elements that contain only one boundary
                // node outside the substruct (i.e. contain 2
                // boundary nodes and one internal node) and
                // move the external node to the same location
                // as one of the boundary nodes
                // **********************************************
                int numExtNodes, extNodeIndex, boundNodeIndex;
                foreach (fe4NodedQuadElement element in substructElements)
                {
                    // count how many external nodes there are
                    numExtNodes = element.Nodes.Count(delegate(feNode node) { return !node.IsInside(substruct); });

                    // if there is only one, this element should have the external node and one boundary node merged
                    if (numExtNodes == 1)
                    {
                        // get the external node
                        extNodeIndex = element.Nodes.FindIndex(delegate(feNode node) { return !node.IsInside(substruct); });

                        // get the first boundary node
                        boundNodeIndex = element.Nodes.FindIndex(delegate(feNode node) { return node.IsBoundary; });

                        // if the search was successful, merge the nodes
                        if (extNodeIndex != -1 && boundNodeIndex != -1)
                        {
                            element.Nodes[extNodeIndex].Merge(element.Nodes[boundNodeIndex], false);
                            element.Nodes[boundNodeIndex] = element.Nodes[extNodeIndex];
                            element.SortNodes(true);
                        }
                    }
                }   // END OF CREATION OF TRI ELEMENTS FROM QUADS WITH ONE NODE OUTSIDE


                // **********************************************
                // find elements that contain 4 boundary nodes
                // and if the nodes are all different, split
                // the element into 2 triangular elements; finally
                // check if the centroid of either element is
                // outside the substruct and if so, delete it
                // **********************************************
                int boundNodeCount;
                feNode centroidNode;
                for (int i = substructElements.Count - 1; i >= 0; i--)
                {
                    boundNodeCount = substructElements[i].Nodes.Count(delegate(feNode node) { return node.IsBoundary; });

                    // check if the element has 4 boundary nodes
                    if (boundNodeCount == 4)
                    {
                        // make sure the nodes are distinct (i.e. not already a triangular element) ...
                        if (substructElements[i].Nodes.Distinct().ToList().Count == 4)
                        {
                            // split by the shortest diagonal
                            if (feNode.Dist(substructElements[i].Nodes[0], substructElements[i].Nodes[2])
                                    < feNode.Dist(substructElements[i].Nodes[1], substructElements[i].Nodes[3]))
                            {
                                node1 = substructElements[i].Nodes[0];

                                // element 1
                                node2 = substructElements[i].Nodes[1];
                                node3 = substructElements[i].Nodes[2];
                                node4 = node1;
                                // make sure centroid is inside the substruct and then create the new element
                                centroidNode = new feNode(0, false, (node1.X + node2.X + node3.X) / 3.0,
                                    (node1.Y + node2.Y + node3.Y) / 3.0);
                                if (centroidNode.IsInside(substruct))
                                {
                                    substructElements.Add(new fe4NodedQuadElement(substruct, ++quadElementCount,
                                            node1, node2, node3, node4,
                                            substructElements[i].Material, false));
                                }

                                // element 2
                                node2 = node3;
                                node3 = substructElements[i].Nodes[3];
                                // make sure centroid is inside the substruct and then create the new element
                                centroidNode = new feNode(0, false, (node1.X + node2.X + node3.X) / 3.0,
                                    (node1.Y + node2.Y + node3.Y) / 3.0);
                                if (centroidNode.IsInside(substruct))
                                {
                                    substructElements.Add(new fe4NodedQuadElement(substruct, ++quadElementCount,
                                            node1, node2, node3, node4,
                                            substructElements[i].Material, false));
                                }
                            }
                            else
                            {
                                node1 = substructElements[i].Nodes[1];

                                // element 1
                                node2 = substructElements[i].Nodes[3];
                                node3 = substructElements[i].Nodes[0];
                                node4 = node1;
                                // make sure centroid is inside the substruct and then create the new element
                                centroidNode = new feNode(0, false, (node1.X + node2.X + node3.X) / 3.0,
                                    (node1.Y + node2.Y + node3.Y) / 3.0);
                                if (centroidNode.IsInside(substruct))
                                {
                                    substructElements.Add(new fe4NodedQuadElement(substruct, ++quadElementCount,
                                            node1, node2, node3, node4,
                                            substructElements[i].Material, false));
                                }

                                // element 2
                                node3 = node2;
                                node2 = substructElements[i].Nodes[2];
                                // make sure centroid is inside the substruct and then create the new element
                                centroidNode = new feNode(0, false, (node1.X + node2.X + node3.X) / 3.0,
                                    (node1.Y + node2.Y + node3.Y) / 3.0);
                                if (centroidNode.IsInside(substruct))
                                {
                                    substructElements.Add(new fe4NodedQuadElement(substruct, ++quadElementCount,
                                            node1, node2, node3, node4,
                                            substructElements[i].Material, false));
                                }
                            }

                            substructElements.RemoveAt(i);
                        }

                        // ... if the element is already triangular
                        else if (substructElements[i].Nodes.Distinct().ToList().Count == 3)
                        {
                            node1 = substructElements[i].Nodes[0];
                            node2 = substructElements[i].Nodes[1];
                            node3 = substructElements[i].Nodes[2];

                            // make sure centroid is inside the substruct and then create the new element
                            centroidNode = new feNode(0, false, (node1.X + node2.X + node3.X) / 3.0,
                                (node1.Y + node2.Y + node3.Y) / 3.0);

                            if (!centroidNode.IsInside(substruct)) substructElements.RemoveAt(i);
                        }
                    }
                }


                // **********************************************
                // eliminate non-boundary nodes that are not 
                // inside the substruct
                // **********************************************
                feNode deletedNode;
                for (int i = substructNodes.Count - 1; i >= 0; i--)
                {
                    for (int j = substructNodes[i].Count - 1; j >= 0; j--)
                    {
                        // get a reference to the current node
                        deletedNode = substructNodes[i][j];

                        // if the node is external ...
                        if (!deletedNode.IsInside(substruct))
                        {
                            // ... eliminate elements containing the node, ...
                            for (int k = substructElements.Count - 1; k >= 0; k--)
                            {
                                if (substructElements[k].Nodes.Contains(deletedNode))
                                {
                                    substructElements.RemoveAt(k);
                                }
                            }

                            // ... and finally eliminate the node
                            substructNodes[i].RemoveAt(j);
                        }
                    }
                }   // END OF DELETION OF NODES OUTSIDE SUBSTRUCT LOOP


                // **********************************************
                // move substructure nodes to global nodes list
                // **********************************************
                foreach (List<feNode> column in substructNodes)
                    nodes.AddRange(column);
                substructNodes.Clear();

                // **********************************************
                // move substructure elements to global elements list
                // **********************************************
                elements.AddRange(substructElements);
                substructElements.Clear();

            } // END OF SUBSTRUCTURES LOOP


            // **********************************************
            // eliminate multiple node numbers at the same
            // location (on substruct boundaries)
            // **********************************************
            int mergeNodeIndex;
            for (int i = nodes.Count - 1; i >= 0; i--)
            {
                for (int j = i - 1; j >= 0; j--)
                {
                    // check if the nodes are near enough to merge
                    if (feNode.Dist(nodes[i], nodes[j]) < tolerDist)
                    {
                        // replace occurrences of the node to be deleted
                        // with the node it is being merged with
                        foreach (fe4NodedQuadElement element in elements)
                        {
                            mergeNodeIndex = element.Nodes.FindIndex(delegate(feNode node) { return node == nodes[i]; });

                            while (mergeNodeIndex != -1)
                            {
                                element.Nodes[mergeNodeIndex] = nodes[j];
                                mergeNodeIndex = element.Nodes.FindIndex(delegate(feNode node) { return node == nodes[i]; });
                            }
                        }

                        nodes[j].Merge(nodes[i], false);
                        nodes.RemoveAt(i);
                        break;
                    }
                }
            }


            // **********************************************
            // eliminate repeated and overlapping elements
            // **********************************************
            List<feNode> ielementNodes = new List<feNode>(), jelementNodes = new List<feNode>();
            List<int> removeNodeIndices = new List<int>();
            int sharedNodeCount;
            for (int i = 0; i < elements.Count; i++)
            {
                if (removeNodeIndices.Contains(i)) continue;

                for (int j = i + 1; j < elements.Count; j++)
                {
                    if (removeNodeIndices.Contains(j)) continue;

                    // get element node lists
                    ielementNodes = elements[i].Nodes.Distinct().ToList();
                    jelementNodes = elements[j].Nodes.Distinct().ToList();

                    sharedNodeCount = 0;
                    if (ielementNodes.Count > jelementNodes.Count)
                    {
                        // determine how many nodes the elements share
                        for (int m = 0; m < elements[j].Nodes.Count; m++)
                        {
                            if (elements[i].Nodes.Contains(elements[j].Nodes[m]))
                                sharedNodeCount++;
                        }

                        // if there are more than 3 shared nodes, they overlap
                        if (sharedNodeCount > 3)
                        {
                            removeNodeIndices.Add(j);
                        }
                    }
                    else
                    {
                        // determine how many nodes the elements share
                        for (int m = 0; m < elements[i].Nodes.Count; m++)
                        {
                            if (elements[j].Nodes.Contains(elements[i].Nodes[m]))
                                sharedNodeCount++;
                        }

                        // if there are more than 3 shared nodes, they overlap
                        if (sharedNodeCount > 3)
                        {
                            removeNodeIndices.Add(i);
                            break;
                        }
                    }

                }   // END OF OVERLAPPING ELEMENT FINDER j LOOP

            }   // END OF OVERLAPPING ELEMENT FINDER i LOOP

            // remove the selected elements
            removeNodeIndices.Sort(); removeNodeIndices.Reverse();
            while (removeNodeIndices.Count > 0)
            {
                elements.RemoveAt(removeNodeIndices[0]);
                removeNodeIndices.RemoveAt(0);
            }

            //// *********************************************
            //// for printing total substruct area (debugging)
            //// *********************************************
            //double substructArea = 0;
            //foreach (feSubstruct substruct in substructs)
            //{
            //    substructArea += substruct.Area;
            //}
            //MessageBox.Show(String.Format("{0}", substructArea), "Substruct Area");



            // **********************************************
            // sort nodes and eliminate gaps in numbering
            // **********************************************
            
            /* INSERTION ORDER SORT */
            //nodes.Sort(feNode.CompareNodesByNumber);
            
            /* ORIGIN DISTANCE SORT */
            //nodes.Sort(feNode.CompareNodesByOriginDist);

            /* HORIZONTAL/VERTICAL DOMINANCE SORT */
            double xMaxDomain = substructs[0].XMax,
                    xMinDomain = substructs[0].XMin,
                    yMaxDomain = substructs[0].YMax,
                    yMinDomain = substructs[0].YMin;
            for (int i = 1; i < substructs.Count; i++)
            {
                if (substructs[i].XMax > xMaxDomain) xMaxDomain = substructs[i].XMax;
                if (substructs[i].XMin < xMinDomain) xMinDomain = substructs[i].XMin;
                if (substructs[i].YMax > yMaxDomain) yMaxDomain = substructs[i].YMax;
                if (substructs[i].YMin < yMinDomain) yMinDomain = substructs[i].YMin;
            }
            if ((xMaxDomain - xMinDomain) > (yMaxDomain - yMinDomain))
            {
                nodes.Sort(feNode.CompareNodesHorizontally);
            }
            else
            {
                nodes.Sort(feNode.CompareNodesVertically);
            }
            
            for (int i = 0; i < nodes.Count; i++)
            {
                nodes[i].Number = i + 1;
            }


            // **********************************************
            // sort boundary elements by number and eliminate
            // gaps in numbering
            // **********************************************
            boundElements.Sort(fe2NodedBoundElement.CompareElementsByNumber);
            for (int i = 0; i < boundElements.Count; i++)
            {
                boundElements[i].Number = i + 1;
            }

            // **********************************************
            // sort body elements by number and eliminate
            // gaps in numbering
            // **********************************************
            elements.Sort(fe4NodedQuadElement.CompareElementsByNumber);
            for (int i = 0; i < elements.Count; i++)
            {
                elements[i].Number = i + 1;
            }


            // **********************************************
            // create input files for FEA engine:
            //      - material types (.mtl)
            //      - node numbers and locations (.nod)
            //      - elements with node connectivity and 
            //          material type (.ele)
            // **********************************************
            string[] path = canvas.FilePath.Split('.');
            path[path.Length - 1] = "mtl";
            using (TextWriter tw = new StreamWriter(string.Join(".", path)))
            {
                tw.WriteLine(canvas.MaterialTypes.Count);
                foreach (MaterialType material in canvas.MaterialTypes)
                {
                    tw.WriteLine("{0}\t{1}\t{2}\t{3}",
                                    material.Phi,
                                    material.Cohesion,
                                    material.Emod,
                                    material.Nu);
                }
            }

            path[path.Length - 1] = "nod";
            using (TextWriter tw = new StreamWriter(string.Join(".", path)))
            {
                tw.WriteLine(nodes.Count);
                foreach (feNode node in nodes)
                {
                    tw.WriteLine("{0}\t{1}\t{2}\t{3}\t{4}",
                                    node.Number,
                                    node.X,
                                    node.Y,
                                    node.IsFixedX ? 0 : 1,
                                    node.IsFixedY ? 0 : 1);
                }
            }

            // only create the element file if this call is for final output
            // (i.e. it is not be used to subsequently create tri elements)
            if (output)
            {
                path[path.Length - 1] = "ele";
                using (TextWriter tw = new StreamWriter(string.Join(".", path)))
                {
                    tw.WriteLine("{0}\t{1}", /*nnodel*/4, /*ndof*/2);

                    tw.WriteLine(elements.Count);
                    int mtlIndex;
                    foreach (fe4NodedQuadElement element in elements)
                    {
                        mtlIndex = canvas.MaterialTypes.FindIndex(delegate(MaterialType m) { return m == element.Material; });
                        tw.WriteLine("{0}\t{1}\t{2}\t{3}\t{4}\t{5}",
                                        element.Number,
                                        element.Nodes[0].Number,
                                        element.Nodes[1].Number,
                                        element.Nodes[2].Number,
                                        element.Nodes[3].Number,
                                        mtlIndex);
                    }
                }

                //// *********************************************
                //// for printing total element area (debugging)
                //// *********************************************
                //double elementArea = 0;
                //foreach (fe4NodedQuadElement element in elements)
                //{
                //    elementArea += element.Area;
                //}
                //MessageBox.Show(String.Format("{0}", elementArea), "Element Area");
            }

            path[path.Length - 1] = "bel";
            using (TextWriter tw = new StreamWriter(string.Join(".", path)))
            {
                tw.WriteLine("{0}\t{1}", /*nnodel*/2, /*ndof*/2);
                tw.WriteLine(boundElements.Count);
                foreach (fe2NodedBoundElement element in boundElements)
                {
                    tw.WriteLine("{0}\t{1}\t{2}",
                                    element.Number,
                                    element.Nodes[0].Number,
                                    element.Nodes[1].Number);
                }
            }


            // **********************************************
            // add graphics polygons to elements for display
            // and add the polygons to the canvas (if the call
            // is not being used to subsequently create tri
            // elements)
            // **********************************************
            if (output)
            {
                Polygon newPolygon;
                double x, y;
                for (int i = 0; i < elements.Count; i++)
                {
                    newPolygon = new Polygon();
                    newPolygon.Stroke = Brushes.Black;
                    newPolygon.Opacity = /*1.0*/ 0.8;
                    newPolygon.Fill = /*Brushes.White*/ elements[i].Material.Fill;

                    foreach (feNode node in elements[i].Nodes)
                    {
                        x = node.X / (scale * factor) * dpiX + originX;
                        y = yHeight - (node.Y / (scale * factor) * dpiY + originY);
                        newPolygon.Points.Add(new Point(x, y));
                    }

                    elements[i].Boundary = newPolygon;
                    canvas.Children.Add(newPolygon);
                }
            }

            return elements;
        }


        /// <summary>
        /// Generates a mesh of 3-noded triangular elements for FEA analysis.
        /// </summary>
        /// <param name="canvas">Current drawing canvas.</param>
        /// <param name="colWidth">Desired width of structured mesh columns.</param>
        /// <param name="rowHeight">Desired height of structured mesh rows.</param>
        /// <returns>A list of 3-noded triangular elements.</returns>
        public static List<fe3NodedTriElement> MeshGenStructured3NodedTri (SlopeCanvas canvas, double colWidth, double rowHeight)
        {
            // run the 4-noded quad element generator (this forms the basis for splitting into 3-noded triangular elements)
            List<fe4NodedQuadElement> quadElements = MeshGenStructured4NodedQuad(canvas, colWidth, rowHeight, false);

            // list for storing the 3-noded triangular elements
            List<fe3NodedTriElement> elements = new List<fe3NodedTriElement>();

            // count of 3-noded triangular elements
            int triElementCount = 0;

            // for unboxing feNode object references
            feNode node1, node2, node3;

            // ******************************************
            // find quad elements that have repeated
            // nodes, delete the duplicate, and create
            // a tri element
            // ******************************************
            for (int i = quadElements.Count - 1; i >= 0; i--)
            {
                for (int j = 0; j < quadElements[i].Nodes.Count; j++)
                {
                    // check if the element contains a repeated node
                    if (quadElements[i].Nodes.Count(delegate(feNode node) { return node == quadElements[i].Nodes[j]; }) > 1)
                    {
                        // remove the repeated node
                        quadElements[i].Nodes.RemoveAt(j);

                        // get the three vertex nodes
                        node1 = quadElements[i].Nodes[0];
                        node2 = quadElements[i].Nodes[1];
                        node3 = quadElements[i].Nodes[2];

                        // create a new tri element
                        elements.Add(new fe3NodedTriElement(quadElements[i].Parent, ++triElementCount,
                            node1, node2, node3,
                            quadElements[i].Material, false));

                        // remove the old quad element
                        quadElements.RemoveAt(i);

                        break;
                    }
                }
            }


            // ******************************************
            // split remaining quad elements along their
            // short diagonal
            // ******************************************
            while (quadElements.Count > 0)
            {
                if (feNode.Dist(quadElements[0].Nodes[0], quadElements[0].Nodes[2])
                    < feNode.Dist(quadElements[0].Nodes[1], quadElements[0].Nodes[3]))
                {
                    node1 = quadElements[0].Nodes[0];

                    // element 1
                    node2 = quadElements[0].Nodes[1];
                    node3 = quadElements[0].Nodes[2];
                    elements.Add(new fe3NodedTriElement(quadElements[0].Parent, ++triElementCount,
                            node1, node2, node3,
                            quadElements[0].Material, false));

                    // element 2
                    node2 = node3;
                    node3 = quadElements[0].Nodes[3];
                    elements.Add(new fe3NodedTriElement(quadElements[0].Parent, ++triElementCount,
                            node1, node2, node3,
                            quadElements[0].Material, false));
                }
                else
                {
                    node1 = quadElements[0].Nodes[1];

                    // element 1
                    node2 = quadElements[0].Nodes[3];
                    node3 = quadElements[0].Nodes[0];
                    elements.Add(new fe3NodedTriElement(quadElements[0].Parent, ++triElementCount,
                            node1, node2, node3,
                            quadElements[0].Material, false));

                    // element 2
                    node3 = node2;
                    node2 = quadElements[0].Nodes[2];
                    elements.Add(new fe3NodedTriElement(quadElements[0].Parent, ++triElementCount,
                            node1, node2, node3,
                            quadElements[0].Material, false));
                }

                // delete the quad element
                quadElements.RemoveAt(0);
            }


            // **********************************************
            // eliminate elements with centroid outside
            // their corresponding substruct
            // **********************************************
            feNode centroidNode;
            foreach (feSubstruct substruct in canvas.FEASubstructs)
            {
                // check all elements
                for (int i = elements.Count - 1; i >= 0; i--)
                {
                    // if parent is not correct, skip this element
                    if (elements[i].Parent != substruct) continue;

                    // get node references
                    node1 = elements[i].Nodes[0];
                    node2 = elements[i].Nodes[1];
                    node3 = elements[i].Nodes[2];

                    // compute centroid
                    centroidNode = new feNode(0, false,
                        (node1.X + node2.X + node3.X) / 3.0,
                        (node1.Y + node2.Y + node3.Y) / 3.0);

                    // if centroid not inside substruct, delete the element
                    if (!centroidNode.IsInside(substruct)) elements.RemoveAt(i);
                }
            }

            // **********************************************
            // renumber elements in case of elimination
            // **********************************************
            for (int i = 0; i < elements.Count; i++)
            {
                elements[i].Number = i + 1;
            }


            // get canvas dimensions/properties
            double originX = canvas.OriginOffsetX,
                   originY = canvas.OriginOffsetY,
                   dpiX = canvas.DpiX,
                   dpiY = canvas.DpiY,
                   scale = canvas.Scale,
                   yHeight = canvas.ActualHeight;
            Units units = canvas.Units;

            // get units dependent scaling factor
            double factor;
            switch (units)
            {
                case Units.Metres: factor = 0.0254; break;
                case Units.Millimetres: factor = 25.4; break;
                case Units.Feet: factor = 1.0 / 12.0; break;
                default: factor = 1.0; break;
            }


            // **********************************************
            // create input files for FEA engine:
            //      - material types (.mtl) !!! CREATED IN MeshGen4NodedQuad
            //      - node numbers and locations (.nod) !!! CREATED IN MeshGen4NodedQuad
            //      - elements with node connectivity and 
            //          material type (.ele) !!! CREATED HERE
            // **********************************************
            string[] path = canvas.FilePath.Split('.');
            path[path.Length - 1] = "ele";
            using (TextWriter tw = new StreamWriter(string.Join(".", path)))
            {
                tw.WriteLine("{0}\t{1}", /*nnodel*/ 3, /*ndof*/ 2);

                tw.WriteLine(elements.Count);
                int mtlIndex;
                foreach (fe3NodedTriElement element in elements)
                {
                    mtlIndex = canvas.MaterialTypes.FindIndex(delegate(MaterialType m) { return m == element.Material; });
                    tw.WriteLine("{0}\t{1}\t{2}\t{3}\t{4}",
                                    element.Number,
                                    element.Nodes[0].Number,
                                    element.Nodes[1].Number,
                                    element.Nodes[2].Number,
                                    mtlIndex);
                }
            }


            // **********************************************
            // add graphics polygons to elements for display
            // and add the polygons to the canvas
            // **********************************************
            Polygon newPolygon;
            double x, y;
            for (int i = 0; i < elements.Count; i++)
            {
                newPolygon = new Polygon();
                newPolygon.Stroke = Brushes.Black;
                newPolygon.Opacity = /*1.0*/ 0.8;
                newPolygon.Fill = /*Brushes.White*/ elements[i].Material.Fill;

                foreach (feNode node in elements[i].Nodes)
                {
                    x = node.X / (scale * factor) * dpiX + originX;
                    y = yHeight - (node.Y / (scale * factor) * dpiY + originY);
                    newPolygon.Points.Add(new Point(x, y));
                }

                elements[i].Boundary = newPolygon;
                canvas.Children.Add(newPolygon);
            }


            //// *********************************************
            //// for printing total element area (debugging)
            //// *********************************************
            //double elementArea = 0;
            //foreach (fe3NodedTriElement element in elements)
            //{
            //    elementArea += element.Area;
            //}
            //MessageBox.Show(String.Format("{0}", elementArea), "Element Area");


            return elements;

        } // END OF MeshGenStructured3NodedTri
    }
}