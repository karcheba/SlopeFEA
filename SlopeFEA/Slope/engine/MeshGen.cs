/************************************************************************
 * PROJECT:     SlopeFEA (c) 2011 Brandon Karchewski
 *              Licensed under the Academic Free License version 3.0
 *                  http://www.opensource.org/licenses/afl-3.0.php
 * 
 * CONTACT:     Brandon Karchewski
 *              Department of Civil Engineering
 *              McMaster University, JHE-301
 *              1280 Main St W
 *              Hamilton, Ontario, Canada
 *              L8S 4L7
 *              p: 905-525-9140 x24287
 *              f: 905-529-9688
 *              e: karcheba@mcmaster.ca
 *              
 * 
 * SOURCE INFORMATION:
 * 
 * The repository for this software project is hosted on git at:
 *      
 *      git://github.com/karcheba/SlopeFEA
 *      
 * As such, the code for the project is free and open source.
 * The relevant license is AFLv3 (see link above). See the
 * README file in the root directory of the repository for a
 * detailed project description, acknowledgements, references,
 * and the revision history.
 ************************************************************************/

using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Media;
using System.Windows.Shapes;

namespace SlopeFEA
{
    partial class SlopeCanvas : Canvas
    {
        public static List<fe4NodedQuadElement> MeshGenStructured4NodedQuad ( SlopeCanvas canvas , double colWidth , double rowHeight , bool output )
        {
            // load material blocks from parent SlopeCanvas
            List<MaterialBlock> blocks = canvas.MaterialBlocks;

            // initialize lists for mesh gen procedure
            List<feNode> nodes = new List<feNode>() ,
                            existBoundNodes = new List<feNode>() ,
                            insertBoundNodes = new List<feNode>();
            List<List<feNode>> substructNodes = new List<List<feNode>>();

            // initialize lists for element generation
            List<fe4NodedQuadElement> elements = new List<fe4NodedQuadElement>() ,
                                     substructElements = new List<fe4NodedQuadElement>();

            //// initialize list for boundary element generation
            List<fe2NodedBoundElement> boundElements = new List<fe2NodedBoundElement>();

            // get canvas dimensions/properties
            double originX = canvas.OriginOffsetX ,
                   originY = canvas.OriginOffsetY ,
                   dpiX = canvas.DpiX ,
                   dpiY = canvas.DpiY ,
                   scale = canvas.Scale ,
                   yHeight = canvas.ActualHeight;
            Units units = canvas.Units;

            // get units dependent scaling factor
            double factor;
            switch ( units )
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
            List<feSubstruct> substructs = new List<feSubstruct>( blocks.Count );
            List<LineLoad> addedLLs = new List<LineLoad>();
            List<PointLoad> addedPLs = new List<PointLoad>();
            foreach ( MaterialBlock block in blocks )
            {
                // skip NULL material blocks
                //if ( block.Material.Name == "NULL" ) continue;

                // create new feSubStruct and insert MaterialType
                //newSubstruct = new feSubstruct( block.Material );
                newSubstruct = new feSubstruct( block.PhaseMaterials );
                newSubstruct.Material = block.Material;

                // insert vertices converted to true coords
                foreach ( DrawingPoint p in block.BoundaryPoints )
                {
                    newSubstruct.Points.Add(
                        new Point( (p.Point.X - originX) / dpiX * factor * scale ,
                                    (yHeight - p.Point.Y - originY) / dpiY * factor * scale ) );

                    newSubstruct.IsFixedX.Add( p.PhaseFixActiveX );
                    newSubstruct.IsFixedY.Add( p.PhaseFixActiveY );
                    newSubstruct.IsPrintPoint.Add( p.IsPrintPoint );
                }

                // create line constraints
                int index1 , index2;
                foreach ( LineConstraint lc in block.LineConstraints )
                {
                    index1 = block.BoundaryPoints.FindIndex( delegate( DrawingPoint pt ) { return pt == lc.Nodes[0]; } );
                    index2 = block.BoundaryPoints.FindIndex( delegate( DrawingPoint pt ) { return pt == lc.Nodes[1]; } );

                    newSubstruct.LineConstraints.Add( new feLineConstraint(
                        newSubstruct.Points[index1] , newSubstruct.Points[index2] ,
                        lc.PhaseFixedX , lc.PhaseFixedY ) );
                }

                // create line loads
                foreach ( LineLoad ll in block.LineLoads )
                {
                    index1 = block.BoundaryPoints.FindIndex( delegate( DrawingPoint pt ) { return pt == ll.Nodes[0]; } );
                    index2 = block.BoundaryPoints.FindIndex( delegate( DrawingPoint pt ) { return pt == ll.Nodes[1]; } );

                    // only create the line load once, and only for a block on which its indices are CCW
                    if ( addedLLs.Contains( ll )
                        || (((index1 > index2) && !(index2 == 0 && index1 == block.BoundaryPoints.Count - 1))
                            || (index1 == 0 && index2 == block.BoundaryPoints.Count - 1)) )
                    {
                        continue;
                    }

                    List<double> nLoad1 = new List<double>() , nLoad2 = new List<double>();
                    List<double> tLoad1 = new List<double>() , tLoad2 = new List<double>();
                    for ( int i = 0 ; i < ll.PhaseFactorN.Count ; i++ )
                    {
                        nLoad1.Add( ll.PhaseFactorN[i] * ll.NLoad1 );
                        nLoad2.Add( ll.PhaseFactorN[i] * ll.NLoad2 );

                        tLoad1.Add( ll.PhaseFactorT[i] * ll.TLoad1 );
                        tLoad2.Add( ll.PhaseFactorT[i] * ll.TLoad2 );
                    }

                    //newSubstruct.LineLoads.Add( new feLineLoad(
                    //    newSubstruct.Points[index1] , newSubstruct.Points[index2] ,
                    //    ll.IsLoadedN , ll.NLoad1 , ll.NLoad2 ,
                    //    ll.IsLoadedT , ll.TLoad1 , ll.TLoad2 ) );
                    newSubstruct.LineLoads.Add( new feLineLoad(
                        newSubstruct.Points[index1] , newSubstruct.Points[index2] ,
                        nLoad1 , nLoad2 , tLoad1 , tLoad2 ) );

                    addedLLs.Add( ll );
                }

                // create point loads
                foreach ( PointLoad pl in block.PointLoads )
                {
                    index1 = block.BoundaryPoints.FindIndex( delegate( DrawingPoint pt ) { return pt == pl.Node; } );

                    // only create point load once
                    if ( addedPLs.Contains( pl ) ) continue;

                    List<double> xLoad = new List<double>() , yLoad = new List<double>();

                    for ( int i = 0 ; i < pl.PhaseFactorX.Count ; i++ )
                    {
                        xLoad.Add( pl.PhaseFactorX[i] * pl.XLoad );
                        yLoad.Add( pl.PhaseFactorY[i] * pl.YLoad );
                    }

                    //newSubstruct.PointLoads.Add( new fePointLoad(
                    //    newSubstruct.Points[index1] ,
                    //    pl.IsLoadedX , pl.XLoad ,
                    //    pl.IsLoadedY , pl.YLoad ) );
                    newSubstruct.PointLoads.Add( new fePointLoad(
                        newSubstruct.Points[index1] ,
                        xLoad , yLoad ) );

                    addedPLs.Add( pl );
                }

                // ensure vertices are ordered counterclockwise
                newSubstruct.SortPoints();

                // add to list of substructs
                substructs.Add( newSubstruct );

            }; // END OF CREATE SUBSTRUCTS FROM MATERIALBLOCKS LOOP
            canvas.FEASubstructs = substructs;


            // initialize node and element counters
            int nodeCount = 0 , quadElementCount = 0 , boundElementCount = 0;

            // for merging nodes at the same location
            double tolerDist = 1e-8 * Math.Sqrt( Math.Pow( colWidth , 2 ) + Math.Pow( rowHeight , 2 ) );

            // *****************************************
            // mesh each feSubStruct
            // *****************************************
            int numPhases = canvas.AnalysisPhases.Count - 1;
            foreach ( feSubstruct substruct in substructs )
            {
                // create bounding box around substruct
                double xmin = substruct.XMin ,
                        xmax = substruct.XMax ,
                        ymin = substruct.YMin ,
                        ymax = substruct.YMax;

                // compute nearest integer number of rows and columns
                int ncols = Math.Max( (int) Math.Round( (xmax - xmin) / colWidth ) , 1 ) ,
                    nrows = Math.Max( (int) Math.Round( (ymax - ymin) / rowHeight ) , 1 );

                // compute actual row and column spacing
                double dx = (xmax - xmin) / ncols ,
                        dy = (ymax - ymin) / nrows;


                // **********************************************
                // create grid of nodes and elements
                // **********************************************
                feNode node1 , node2 , node3 , node4;
                int top , bot , lft , rgt;
                for ( int i = 0 ; i <= ncols ; i++ )
                {
                    // create new column
                    substructNodes.Add( new List<feNode>() );
                    for ( int j = 0 ; j <= nrows ; j++ )
                    {
                        // create new node at appropriate location
                        substructNodes[i].Add( new feNode( ++nodeCount , false , xmin + i * dx , ymin + j * dy , numPhases ) );

                        // if not on the boundary, create new quad element
                        // NOTE: nodes are added in CCW order
                        if ( i > 0 && j > 0 )
                        {
                            // node indices
                            lft = i - 1; rgt = i; bot = j - 1; top = j;

                            // create element
                            node1 = substructNodes[lft][bot];
                            node2 = substructNodes[rgt][bot];
                            node3 = substructNodes[rgt][top];
                            node4 = substructNodes[lft][top];
                            //substructElements.Add( new fe4NodedQuadElement( substruct , ++quadElementCount ,
                            //    node1 , node2 , node3 , node4 ,
                            //    substruct.Material , false ) );
                            substructElements.Add( new fe4NodedQuadElement( substruct , ++quadElementCount ,
                                node1 , node2 , node3 , node4 ,
                                substruct.Material , substruct.PhaseMaterials , false ) );
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
                for ( int i = 0 ; i < substruct.Points.Count ; i++ )
                {
                    // create locked node at the vertex
                    vertexNode = new feNode( ++nodeCount , true , substruct.Points[i].X , substruct.Points[i].Y , numPhases );
                    vertexNode.IsLocked = true;
                    vertexNode.IsPrintPoint = substruct.IsPrintPoint[i];
                    //vertexNode.IsFixedX = substruct.IsFixedX[i];
                    //vertexNode.IsFixedY = substruct.IsFixedY[i];
                    //vertexNode.PhaseFixedX.AddRange( substruct.IsFixedX[i] );
                    //vertexNode.PhaseFixedY.AddRange( substruct.IsFixedY[i] );
                    for ( int j = 0 ; j < vertexNode.PhaseFixedX.Count ; j++ )
                    {
                        vertexNode.PhaseFixedX[j] = substruct.IsFixedX[i][j];
                        vertexNode.PhaseFixedY[j] = substruct.IsFixedY[i][j];
                    }

                    // see if a node is already at the vertex
                    foundVertex = false;
                    foreach ( List<feNode> column in substructNodes )
                    {
                        foreach ( feNode node in column )
                        {
                            dist = feNode.Dist( node , vertexNode );

                            if ( dist < tolerDist )
                            {
                                node.X = vertexNode.X;
                                node.Y = vertexNode.Y;
                                node.IsBoundary = true;
                                node.IsLocked = true;
                                node.IsPrintPoint = substruct.IsPrintPoint[i];
                                //node.IsFixedX = substruct.IsFixedX[i];
                                //node.IsFixedY = substruct.IsFixedY[i];
                                //node.PhaseFixedX.AddRange( substruct.IsFixedX[i] );
                                //node.PhaseFixedY.AddRange( substruct.IsFixedY[i] );
                                for ( int j = 0 ; j < node.PhaseFixedX.Count ; j++ )
                                {
                                    node.PhaseFixedX[j] = node.PhaseFixedX[j] || substruct.IsFixedX[i][j];
                                    node.PhaseFixedY[j] = node.PhaseFixedY[j] || substruct.IsFixedY[i][j];
                                }

                                foreach ( fePointLoad pl in substruct.PointLoads )
                                {
                                    if ( Math.Abs( pl.Point.X - node.X ) < tolerDist && Math.Abs( pl.Point.Y - node.Y ) < tolerDist )
                                    {
                                        //node.XLoad = pl.XLoad;
                                        //node.YLoad = pl.YLoad;
                                        //node.XLoad.AddRange( pl.XLoad );
                                        //node.YLoad.AddRange( pl.YLoad );
                                        for ( int j = 0 ; j < node.XLoad.Count ; j++ )
                                        {
                                            node.XLoad[j] += pl.XLoad[j];
                                            node.YLoad[j] += pl.YLoad[j];
                                        }
                                    }
                                }

                                foundVertex = true;
                                break;
                            }
                        }
                        if ( foundVertex ) break;
                    }
                    if ( !foundVertex )
                    {
                        foreach ( fePointLoad pl in substruct.PointLoads )
                        {
                            if ( Math.Abs( pl.Point.X - vertexNode.X ) < tolerDist && Math.Abs( pl.Point.Y - vertexNode.Y ) < tolerDist )
                            {
                                //vertexNode.XLoad = pl.XLoad;
                                //vertexNode.YLoad = pl.YLoad;
                                //vertexNode.XLoad.AddRange( pl.XLoad );
                                //vertexNode.YLoad.AddRange( pl.YLoad );
                                for ( int j = 0 ; j < vertexNode.XLoad.Count ; j++ )
                                {
                                    vertexNode.XLoad[j] += pl.XLoad[j];
                                    vertexNode.YLoad[j] += pl.YLoad[j];
                                }
                            }
                        }

                        insertBoundNodes.Add( vertexNode );
                    }

                }   // END OF VERTEX DETECTION/INSERTION LOOP


                // **********************************************
                // loop through substruct line segments finding
                // created nodes that are on each segment
                // and identifying them as boundary nodes and also
                // inserting boundary nodes along the segments
                // and locking them
                // **********************************************
                Point p0 = substruct.Points[substruct.Points.Count - 1] , p1;
                double xn , yn , tolerCross = 1e-8 , distLL = 0 , interpLL = 0;// , dNLoad = 0 , dTLoad = 0 ;
                List<double> dNLoad = new List<double>() , dTLoad = new List<double>();
                feNode boundNode;
                int numNodes;
                //bool fixX = substruct.IsFixedX[substruct.IsFixedX.Count - 1] ,
                //    fixY = substruct.IsFixedY[substruct.IsFixedY.Count - 1];
                List<bool> fixX/* = substruct.IsFixedX[substruct.IsFixedX.Count - 1]*/ ,
                            fixY /*= substruct.IsFixedY[substruct.IsFixedY.Count - 1]*/;
                feLineConstraint lc;
                feLineLoad ll;
                fe2NodedBoundElement newBoundElement = null;
                for ( int i = 0 ; i < substruct.Points.Count ; i++ )
                {
                    p1 = substruct.Points[i];

                    lc = substruct.LineConstraints.Find( delegate( feLineConstraint l ) { return l.Points.Contains( p0 ) && l.Points.Contains( p1 ); } );
                    ll = substruct.LineLoads.Find( delegate( feLineLoad l ) { return l.Points[0] == p0 && l.Points[1] == p1; } );

                    if ( ll != null )
                    {
                        vertexNode = null;
                        vertexNode = insertBoundNodes.Find( delegate( feNode node )
                        {
                            return Math.Abs( node.X - p0.X ) < tolerDist && Math.Abs( node.Y - p0.Y ) < tolerDist;
                        } );
                        if ( vertexNode == null )
                        {
                            foreach ( List<feNode> column in substructNodes )
                            {
                                vertexNode = column.Find( delegate( feNode node )
                                {
                                    return Math.Abs( node.X - p0.X ) < tolerDist && Math.Abs( node.Y - p0.Y ) < tolerDist;
                                } );
                                if ( vertexNode != null )
                                {
                                    vertexNode.X = p0.X;
                                    vertexNode.Y = p0.Y;
                                    break;
                                }
                            }
                        }

                        //newBoundElement = new fe2NodedBoundElement( ++boundElementCount ,
                        //    vertexNode , null , ll.NLoad1 , 0 , ll.TLoad1 , 0 );
                        newBoundElement = new fe2NodedBoundElement( ++boundElementCount ,
                            vertexNode , null , ll.NLoad1 , null , ll.TLoad1 , null );

                        //dNLoad = ll.NLoad2 - ll.NLoad1;
                        //dTLoad = ll.TLoad2 - ll.TLoad1;
                        dNLoad.Clear();
                        dTLoad.Clear();
                        for ( int j = 0 ; j < ll.NLoad1.Count ; j++ )
                        {
                            dNLoad.Add( ll.NLoad2[j] - ll.NLoad1[j] );
                            dTLoad.Add( ll.TLoad2[j] - ll.TLoad1[j] );
                        }
                        distLL = 1 / (ll.Points[1] - ll.Points[0]).Length;  // precompute inverse for later efficiency (mult vs. div)
                        //distLL = Math.Sqrt( Math.Pow( ll.Points[1].X - ll.Points[0].X , 2 ) + Math.Pow( ll.Points[1].Y - ll.Points[0].Y , 2 ) );
                    }

                    fixX = new List<bool>();
                    fixY = new List<bool>();
                    for ( int j = 0 ; j < substruct.PhaseMaterials.Count ; j++ )
                    {
                        fixX.Add( (lc != null) && (lc.PhaseFixedX[j]) );
                        fixY.Add( (lc != null) && (lc.PhaseFixedY[j]) );
                    }
                    //fixX = (lc != null) && (lc.IsFixedX);
                    //fixY = (lc != null) && (lc.IsFixedY);

                    // find rise and run of line segment
                    dx = p1.X - p0.X;
                    dy = p1.Y - p0.Y;

                    // look for existing nodes that are on the segment
                    foreach ( List<feNode> column in substructNodes )
                    {
                        foreach ( feNode node in column )
                        {
                            // skip the node if it was already identified
                            // in vertex loop
                            if ( node.IsBoundary ) continue;

                            xn = node.X;
                            yn = node.Y;

                            // if the node is within the bounding rectangle
                            // of the line segment and the cross product is
                            // within tolerance of zero, the point is on the
                            // line segment
                            if ( xn >= Math.Min( p0.X , p1.X ) && xn <= Math.Max( p0.X , p1.X )
                                && yn >= Math.Min( p0.Y , p1.Y ) && yn <= Math.Max( p0.Y , p1.Y )
                                && Math.Abs( (xn - p0.X) * dy - (yn - p0.Y) * dx ) < tolerCross )
                            {
                                node.IsBoundary = true;
                                //node.IsFixedX = fixX;
                                //node.IsFixedY = fixY;
                                //node.PhaseFixedX.AddRange( fixX );
                                //node.PhaseFixedY.AddRange( fixY );
                                for ( int j = 0 ; j < node.PhaseFixedX.Count ; j++ )
                                {
                                    node.PhaseFixedX[j] = node.PhaseFixedX[j] || fixX[j];
                                    node.PhaseFixedY[j] = node.PhaseFixedY[j] || fixY[j];
                                }
                                existBoundNodes.Add( node );
                            }
                        }
                    }   // END OF EXISTING BOUNDARY NODE DETECTION LOOP

                    dx = Math.Abs( dx );
                    dy = Math.Abs( dy );

                    // compute nearest integer number of nodes on the line segment
                    numNodes = (int) (Math.Max( Math.Max( Math.Round( dx / colWidth ) , Math.Round( dy / rowHeight ) ) , 1 ));

                    // grid spacing on boundary line
                    dx = (p1.X - p0.X) / numNodes;
                    dy = (p1.Y - p0.Y) / numNodes;

                    // insert locked boundary nodes
                    for ( int j = 1 ; j < numNodes ; j++ )
                    {
                        xn = p0.X + j * dx;
                        yn = p0.Y + j * dy;
                        boundNode = new feNode( ++nodeCount , true , xn , yn , numPhases );
                        boundNode.IsLocked = true;
                        //boundNode.IsFixedX = fixX;
                        //boundNode.IsFixedY = fixY;
                        //boundNode.PhaseFixedX.AddRange( fixX );
                        //boundNode.PhaseFixedY.AddRange( fixY );
                        for ( int k = 0 ; k < boundNode.PhaseFixedX.Count ; k++ )
                        {
                            boundNode.PhaseFixedX[k] = fixX[k];
                            boundNode.PhaseFixedY[k] = fixY[k];
                        }
                        insertBoundNodes.Add( boundNode );

                        if ( ll != null )
                        {
                            newBoundElement.Nodes[1] = boundNode;

                            interpLL = ((new Point( xn , yn )) - ll.Points[0]).Length * distLL;
                            //interpLL = Math.Sqrt( Math.Pow( xn - ll.Points[0].X , 2 ) + Math.Pow( yn - ll.Points[0].Y , 2 ) ) / distLL;

                            //newBoundElement.NLoads[1] = ll.NLoad1 + dNLoad * interpLL;
                            //newBoundElement.TLoads[1] = ll.TLoad1 + dTLoad * interpLL;
                            for ( int k = 0 ; k < newBoundElement.NLoads[1].Count ; k++ )
                            {
                                newBoundElement.NLoads[1][k] = ll.NLoad1[k] + dNLoad[k] * interpLL;
                                newBoundElement.TLoads[1][k] = ll.TLoad1[k] + dTLoad[k] * interpLL;
                            }

                            boundElements.Add( newBoundElement );

                            //newBoundElement = new fe2NodedBoundElement( ++boundElementCount ,
                            //    boundNode , null , newBoundElement.NLoads[1] , 0 , newBoundElement.TLoads[1] , 0 );
                            newBoundElement = new fe2NodedBoundElement( ++boundElementCount ,
                                boundNode , null , newBoundElement.NLoads[1] , null , newBoundElement.TLoads[1] , null );
                        }
                    }   // END OF LOCKED BOUND NODE INSERTION

                    if ( ll != null )
                    {
                        vertexNode = null;
                        vertexNode = insertBoundNodes.Find( delegate( feNode node )
                        {
                            return Math.Abs( node.X - p1.X ) < tolerDist && Math.Abs( node.Y - p1.Y ) < tolerDist;
                        } );
                        if ( vertexNode == null )
                        {
                            foreach ( List<feNode> column in substructNodes )
                            {
                                vertexNode = column.Find( delegate( feNode node )
                                {
                                    return Math.Abs( node.X - p1.X ) < tolerDist && Math.Abs( node.Y - p1.Y ) < tolerDist;
                                } );
                                if ( vertexNode != null )
                                {
                                    vertexNode.X = p1.X;
                                    vertexNode.Y = p1.Y;
                                    break;
                                }
                            }
                        }

                        newBoundElement.Nodes[1] = vertexNode;
                        newBoundElement.NLoads[1] = ll.NLoad2;
                        newBoundElement.TLoads[1] = ll.TLoad2;

                        boundElements.Add( newBoundElement );
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
                double maxDist = Math.Sqrt( Math.Pow( xmax - xmin , 2 ) + Math.Pow( ymax - ymin , 2 ) );
                feNode nearestNode;
                double nearestDist;
                int boundElementNodeIndex;
                while ( existBoundNodes.Count > 0 )
                {
                    nearestDist = maxDist;
                    nearestNode = null;

                    // find the nearest inserted boundary node
                    foreach ( feNode node in insertBoundNodes )
                    {
                        dist = feNode.Dist( existBoundNodes[0] , node );

                        if ( dist < nearestDist )
                        {
                            nearestNode = node;
                            nearestDist = dist;
                        }
                    }

                    // merge the nodes and remove from both boundary node lists
                    foreach ( fe2NodedBoundElement boundElement in boundElements )
                    {
                        boundElementNodeIndex = boundElement.Nodes.FindIndex( delegate( feNode node ) { return node == nearestNode; } );
                        if ( boundElementNodeIndex != -1 ) boundElement.Nodes[boundElementNodeIndex] = existBoundNodes[0];
                    }
                    existBoundNodes[0].Merge( nearestNode , false );
                    existBoundNodes.RemoveAt( 0 );
                    insertBoundNodes.Remove( nearestNode );

                }   // END OF EXISTING BOUNDARY NODE MERGE LOOP



                // **********************************************
                // while the inserted boundary nodes list still
                // contains nodes find the nearest existing
                // non-boundary node and merge them
                // **********************************************
                while ( insertBoundNodes.Count > 0 )
                {
                    boundNode = insertBoundNodes[0];

                    nearestDist = maxDist;
                    nearestNode = null;

                    // find the nearest unlocked non-boundary node
                    foreach ( List<feNode> column in substructNodes )
                    {
                        foreach ( feNode node in column )
                        {
                            // skip the node if it is a boundary or locked node
                            if ( node.IsBoundary || node.IsLocked )
                                continue;

                            dist = feNode.Dist( node , boundNode );

                            if ( dist < nearestDist )
                            {
                                nearestNode = node;
                                nearestDist = dist;
                            }
                        }
                    }

                    // merge the nodes and remove from inserted node list
                    foreach ( fe2NodedBoundElement boundElement in boundElements )
                    {
                        boundElementNodeIndex = boundElement.Nodes.FindIndex( delegate( feNode node ) { return node == boundNode; } );
                        if ( boundElementNodeIndex != -1 ) boundElement.Nodes[boundElementNodeIndex] = nearestNode;
                    }
                    nearestNode.Merge( boundNode , false );
                    insertBoundNodes.RemoveAt( 0 );

                }   // END OF INSERTED BOUND NODE MERGE LOOP



                // **********************************************
                // find elements that contain only one boundary
                // node outside the substruct (i.e. contain 2
                // boundary nodes and one internal node) and
                // move the external node to the same location
                // as one of the boundary nodes
                // **********************************************
                int numExtNodes , extNodeIndex;
                int boundNodeIndex;
                foreach ( fe4NodedQuadElement element in substructElements )
                {
                    // count how many external nodes there are
                    numExtNodes = element.Nodes.Count( delegate( feNode node ) { return !node.IsInside( substruct ); } );

                    // if there is only one, this element should have the external node and one boundary node merged
                    if ( numExtNodes == 1 )
                    {
                        // get the external node
                        extNodeIndex = element.Nodes.FindIndex( delegate( feNode node ) { return !node.IsInside( substruct ); } );

                        // get the first boundary node
                        boundNodeIndex = element.Nodes.FindIndex( delegate( feNode node ) { return node.IsBoundary; } );

                        // if the search was successful, merge the nodes
                        if ( extNodeIndex != -1 && boundNodeIndex != -1 )
                        {
                            element.Nodes[extNodeIndex].Merge( element.Nodes[boundNodeIndex] , false );
                            foreach ( fe2NodedBoundElement boundElement in boundElements )
                            {
                                boundElementNodeIndex = boundElement.Nodes.FindIndex( delegate( feNode node ) { return node == element.Nodes[boundNodeIndex]; } );
                                if ( boundElementNodeIndex != -1 ) boundElement.Nodes[boundElementNodeIndex] = element.Nodes[extNodeIndex];
                            }
                            element.Nodes[boundNodeIndex] = element.Nodes[extNodeIndex];
                            element.SortNodes( true );
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
                for ( int i = substructElements.Count - 1 ; i >= 0 ; i-- )
                {
                    boundNodeCount = substructElements[i].Nodes.Count( delegate( feNode node ) { return node.IsBoundary; } );

                    // check if the element has 4 boundary nodes
                    if ( boundNodeCount == 4 )
                    {
                        // make sure the nodes are distinct (i.e. not already a triangular element) ...
                        if ( substructElements[i].Nodes.Distinct().ToList().Count == 4 )
                        {
                            // split by the shortest diagonal
                            if ( feNode.Dist( substructElements[i].Nodes[0] , substructElements[i].Nodes[2] )
                                    < feNode.Dist( substructElements[i].Nodes[1] , substructElements[i].Nodes[3] ) )
                            {
                                node1 = substructElements[i].Nodes[0];

                                // element 1
                                node2 = substructElements[i].Nodes[1];
                                node3 = substructElements[i].Nodes[2];
                                node4 = node1;
                                // make sure centroid is inside the substruct and then create the new element
                                centroidNode = new feNode( 0 , false , (node1.X + node2.X + node3.X) / 3.0 ,
                                    (node1.Y + node2.Y + node3.Y) / 3.0 ,
                                    0 );
                                if ( centroidNode.IsInside( substruct ) )
                                {
                                    substructElements.Add( new fe4NodedQuadElement( substruct , ++quadElementCount ,
                                            node1 , node2 , node3 , node4 ,
                                            substructElements[i].Material ,
                                            substructElements[i].PhaseMaterials , false ) );
                                }

                                // element 2
                                node2 = node3;
                                node3 = substructElements[i].Nodes[3];
                                // make sure centroid is inside the substruct and then create the new element
                                centroidNode = new feNode( 0 , false , (node1.X + node2.X + node3.X) / 3.0 ,
                                    (node1.Y + node2.Y + node3.Y) / 3.0 ,
                                    0 );
                                if ( centroidNode.IsInside( substruct ) )
                                {
                                    substructElements.Add( new fe4NodedQuadElement( substruct , ++quadElementCount ,
                                            node1 , node2 , node3 , node4 ,
                                            substructElements[i].Material ,
                                            substructElements[i].PhaseMaterials , false ) );
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
                                centroidNode = new feNode( 0 , false , (node1.X + node2.X + node3.X) / 3.0 ,
                                    (node1.Y + node2.Y + node3.Y) / 3.0 ,
                                    0 );
                                if ( centroidNode.IsInside( substruct ) )
                                {
                                    substructElements.Add( new fe4NodedQuadElement( substruct , ++quadElementCount ,
                                            node1 , node2 , node3 , node4 ,
                                            substructElements[i].Material ,
                                            substructElements[i].PhaseMaterials , false ) );
                                }

                                // element 2
                                node3 = node2;
                                node2 = substructElements[i].Nodes[2];
                                // make sure centroid is inside the substruct and then create the new element
                                centroidNode = new feNode( 0 , false , (node1.X + node2.X + node3.X) / 3.0 ,
                                    (node1.Y + node2.Y + node3.Y) / 3.0 ,
                                    0 );
                                if ( centroidNode.IsInside( substruct ) )
                                {
                                    substructElements.Add( new fe4NodedQuadElement( substruct , ++quadElementCount ,
                                            node1 , node2 , node3 , node4 ,
                                            substructElements[i].Material ,
                                            substructElements[i].PhaseMaterials , false ) );
                                }
                            }

                            substructElements.RemoveAt( i );
                        }

                        // ... if the element is already triangular
                        else if ( substructElements[i].Nodes.Distinct().ToList().Count == 3 )
                        {
                            node1 = substructElements[i].Nodes[0];
                            node2 = substructElements[i].Nodes[1];
                            node3 = substructElements[i].Nodes[2];

                            // if centroid is outside the substruct, delete the element
                            centroidNode = new feNode( 0 , false , (node1.X + node2.X + node3.X) / 3.0 ,
                                (node1.Y + node2.Y + node3.Y) / 3.0 ,
                                0 );

                            if ( !centroidNode.IsInside( substruct ) ) substructElements.RemoveAt( i );
                        }
                    }
                }


                // **********************************************
                // eliminate non-boundary nodes that are not 
                // inside the substruct
                // **********************************************
                feNode deletedNode;
                for ( int i = substructNodes.Count - 1 ; i >= 0 ; i-- )
                {
                    for ( int j = substructNodes[i].Count - 1 ; j >= 0 ; j-- )
                    {
                        // get a reference to the current node
                        deletedNode = substructNodes[i][j];

                        // if the node is external ...
                        if ( !deletedNode.IsInside( substruct ) )
                        {
                            // ... eliminate elements containing the node, ...
                            for ( int k = substructElements.Count - 1 ; k >= 0 ; k-- )
                            {
                                if ( substructElements[k].Nodes.Contains( deletedNode ) )
                                {
                                    substructElements.RemoveAt( k );
                                }
                            }

                            // ... and finally eliminate the node
                            substructNodes[i].RemoveAt( j );
                        }
                    }
                }   // END OF DELETION OF NODES OUTSIDE SUBSTRUCT LOOP


                // **********************************************
                // move substructure nodes to global nodes list
                // **********************************************
                substructNodes.ForEach( delegate( List<feNode> column ) { nodes.AddRange( column ); } );
                substructNodes.Clear();

                // **********************************************
                // move substructure elements to global elements list
                // **********************************************
                elements.AddRange( substructElements );
                substructElements.Clear();

            } // END OF SUBSTRUCTURES LOOP


            // **********************************************
            // eliminate multiple node numbers at the same
            // location (on substruct boundaries)
            // **********************************************
            int mergeNodeIndex;
            for ( int i = nodes.Count - 1 ; i >= 0 ; i-- )
            {
                for ( int j = i - 1 ; j >= 0 ; j-- )
                {
                    // check if the nodes are near enough to merge
                    if ( feNode.Dist( nodes[i] , nodes[j] ) < tolerDist )
                    {
                        // replace occurrences of the node to be deleted
                        // with the node it is being merged with
                        foreach ( fe4NodedQuadElement element in elements )
                        {
                            mergeNodeIndex = element.Nodes.FindIndex( delegate( feNode node ) { return node == nodes[i]; } );

                            while ( mergeNodeIndex != -1 )
                            {
                                element.Nodes[mergeNodeIndex] = nodes[j];
                                mergeNodeIndex = element.Nodes.FindIndex( delegate( feNode node ) { return node == nodes[i]; } );
                            }
                        }
                        foreach ( fe2NodedBoundElement boundElement in boundElements )
                        {
                            mergeNodeIndex = boundElement.Nodes.FindIndex( delegate( feNode node ) { return node == nodes[i]; } );
                            if ( mergeNodeIndex != -1 ) boundElement.Nodes[mergeNodeIndex] = nodes[j];
                        }

                        nodes[j].Merge( nodes[i] , false );
                        nodes.RemoveAt( i );
                        break;
                    }
                }
            }


            // **********************************************
            // eliminate repeated and overlapping elements
            // **********************************************
            List<feNode> ielementNodes = new List<feNode>() , jelementNodes = new List<feNode>();
            List<int> removeElementIndices = new List<int>();
            int sharedNodeCount;
            // body elements
            for ( int i = 0 ; i < elements.Count ; i++ )
            {
                if ( removeElementIndices.Contains( i ) ) continue;

                for ( int j = i + 1 ; j < elements.Count ; j++ )
                {
                    if ( removeElementIndices.Contains( j ) ) continue;

                    // get element node lists
                    ielementNodes = elements[i].Nodes.Distinct().ToList();
                    jelementNodes = elements[j].Nodes.Distinct().ToList();

                    sharedNodeCount = 0;
                    if ( ielementNodes.Count > jelementNodes.Count )
                    {
                        // determine how many nodes the elements share
                        for ( int m = 0 ; m < elements[j].Nodes.Count ; m++ )
                        {
                            if ( elements[i].Nodes.Contains( elements[j].Nodes[m] ) )
                                sharedNodeCount++;
                        }

                        // if there are more than 3 shared nodes, they overlap
                        if ( sharedNodeCount > 3 )
                        {
                            removeElementIndices.Add( j );
                        }
                    }
                    else
                    {
                        // determine how many nodes the elements share
                        for ( int m = 0 ; m < elements[i].Nodes.Count ; m++ )
                        {
                            if ( elements[j].Nodes.Contains( elements[i].Nodes[m] ) )
                                sharedNodeCount++;
                        }

                        // if there are more than 3 shared nodes, they overlap
                        if ( sharedNodeCount > 3 )
                        {
                            removeElementIndices.Add( i );
                            break;
                        }
                    }

                }   // END OF OVERLAPPING ELEMENT FINDER j LOOP

            }   // END OF OVERLAPPING ELEMENT FINDER i LOOP

            // remove the selected elements
            removeElementIndices.Sort(); removeElementIndices.Reverse();
            while ( removeElementIndices.Count > 0 )
            {
                elements.RemoveAt( removeElementIndices[0] );
                removeElementIndices.RemoveAt( 0 );
            }

            // boundary elements
            for ( int i = 0 ; i < boundElements.Count ; i++ )
            {
                if ( removeElementIndices.Contains( i ) ) continue;

                for ( int j = i + 1 ; j < boundElements.Count ; j++ )
                {
                    if ( removeElementIndices.Contains( j ) ) continue;

                    sharedNodeCount = 0;
                    for ( int m = 0 ; m < boundElements[i].Nodes.Count ; m++ )
                    {
                        if ( boundElements[j].Nodes.Contains( boundElements[i].Nodes[m] ) ) sharedNodeCount++;
                    }
                    if ( sharedNodeCount > 1 ) removeElementIndices.Add( j );

                }   // END OF OVERLAPPING BOUNDARY ELEMENT FINDER j LOOP

            }   // END OF OVERLAPPING BOUNDARY ELEMENT FINDER i LOOP

            // remove the selected elements
            removeElementIndices.Sort(); removeElementIndices.Reverse();
            while ( removeElementIndices.Count > 0 )
            {
                boundElements.RemoveAt( removeElementIndices[0] );
                removeElementIndices.RemoveAt( 0 );
            }

            //// *********************************************
            //// for printing total substruct area (debugging)
            //// *********************************************
            //double substructArea = 0;
            //foreach (feSubstruct substruct in substructs)
            //{
            //    substructArea += substruct.Area;
            //}
            //MessageBox.Show(string.Format("{0}", substructArea), "Substruct Area");



            // **********************************************
            // sort nodes and eliminate gaps in numbering
            // **********************************************

            /* INSERTION ORDER SORT */
            //nodes.Sort(feNode.CompareNodesByNumber);

            /* ORIGIN DISTANCE SORT */
            //nodes.Sort(feNode.CompareNodesByOriginDist);

            /* HORIZONTAL/VERTICAL DOMINANCE SORT */
            double xMaxDomain = substructs[0].XMax ,
                    xMinDomain = substructs[0].XMin ,
                    yMaxDomain = substructs[0].YMax ,
                    yMinDomain = substructs[0].YMin;
            for ( int i = 1 ; i < substructs.Count ; i++ )
            {
                if ( substructs[i].XMax > xMaxDomain ) xMaxDomain = substructs[i].XMax;
                if ( substructs[i].XMin < xMinDomain ) xMinDomain = substructs[i].XMin;
                if ( substructs[i].YMax > yMaxDomain ) yMaxDomain = substructs[i].YMax;
                if ( substructs[i].YMin < yMinDomain ) yMinDomain = substructs[i].YMin;
            }
            if ( (xMaxDomain - xMinDomain) > (yMaxDomain - yMinDomain) )
            {
                nodes.Sort( feNode.CompareNodesHorizontally );
            }
            else
            {
                nodes.Sort( feNode.CompareNodesVertically );
            }

            int printNode = 0;
            for ( int i = 0 ; i < nodes.Count ; i++ )
            {
                nodes[i].Number = i + 1;
                if ( nodes[i].IsPrintPoint ) printNode = nodes[i].Number;
            }


            // **********************************************
            // sort boundary elements by number and eliminate
            // gaps in numbering
            // **********************************************
            boundElements.Sort( fe2NodedBoundElement.CompareElementsByNumber );
            for ( int i = 0 ; i < boundElements.Count ; i++ )
            {
                boundElements[i].Number = i + 1;
            }

            // **********************************************
            // eliminate elements with zero area
            // **********************************************
            for ( int i = elements.Count - 1 ; i >= 0 ; i-- )
            {
                if ( elements[i].Area < 1e-5 ) elements.RemoveAt( i );
            }

            // **********************************************
            // sort body elements by number and eliminate
            // gaps in numbering
            // **********************************************
            elements.Sort( fe4NodedQuadElement.CompareElementsByNumber );
            for ( int i = 0 ; i < elements.Count ; i++ )
            {
                elements[i].Number = i + 1;
            }


            // **********************************************
            // create input files for FEA engine:
            //      - material types (.mtl)
            //      - analysis phases (.phs)
            //      - node numbers and locations (.nod)
            //      - elements with node connectivity and 
            //          material type (.ele)
            // **********************************************
            string[] path = canvas.FilePath.Split( '.' );
            path[path.Length - 1] = "mtl";
            using ( TextWriter tw = new StreamWriter( string.Join( "." , path ) ) )
            {
                tw.WriteLine( canvas.MaterialTypes.Count - 1 );
                for ( int i = 0 ; i < canvas.MaterialTypes.Count - 1 ; i++ )
                {
                    tw.WriteLine( "{0}\t{1}\t{2}\t{3}\t{4}\t{5}" ,
                                    canvas.MaterialTypes[i].Gamma ,
                                    canvas.MaterialTypes[i].Phi ,
                                    canvas.MaterialTypes[i].Cohesion ,
                                    canvas.MaterialTypes[i].Psi ,
                                    canvas.MaterialTypes[i].Emod ,
                                    canvas.MaterialTypes[i].Nu );
                }
            }

            path[path.Length - 1] = "phs";
            using ( TextWriter tw = new StreamWriter( string.Join( "." , path ) ) )
            {
                tw.WriteLine( /*NPHASE=*/ canvas.AnalysisPhases.Count - 1 );

                for ( int i = 1 ; i < canvas.AnalysisPhases.Count ; i++ )
                {
                    tw.WriteLine( "{0}\t{1}\t{2}\t{3}\t{4}\t{5}\t{6}" ,
                        /*iphase=*/ canvas.AnalysisPhases[i].Number ,
                        /*BEGPHS(i)=*/ canvas.AnalysisPhases[i].BeginPhase.Number ,
                        /*RESET(i)=*/ canvas.AnalysisPhases[i].ResetDisplacements ? 1 : 0 ,
                        /*NSTEP(i)=*/  canvas.AnalysisPhases[i].NSteps ,
                        /*NITER(i)=*/  canvas.AnalysisPhases[i].NIterations ,
                        /*NPRINT(i)=*/ canvas.AnalysisPhases[i].NPrintLines ,
                        /*GFACT(i)=*/  canvas.AnalysisPhases[i].GravityFactor );
                }
            }

            path[path.Length - 1] = "nod";
            using ( TextWriter tw = new StreamWriter( string.Join( "." , path ) ) )
            {
                tw.WriteLine( "{0}\t{1}\t{2}\t{3}"/*\t{4}\t{5}\t{6}\t{7}\t{8}"*/ ,
                    /*NNOD=*/ nodes.Count ,
                    /*NDIM=*/ 2 ,
                    /*NVAR=*/ 2 ,
                    /*IPRINT=*/ printNode );
                    // /*NSTEP=*/ canvas.FEAParameters.NStep ,
                    // /*NITER=*/ canvas.FEAParameters.NIter ,
                    // /*NPRINT=*/ canvas.FEAParameters.NPrint ,
                    // /*LFACT=*/ canvas.FEAParameters.LFact ,
                    // /*GFACT=*/ canvas.FEAParameters.GFact );

                int[] fix = new int[2 * nodes[0].PhaseFixedX.Count];
                double[] load = new double[2 * nodes[0].XLoad.Count];
                int ii , iii;
                foreach ( feNode node in nodes )
                {
                    for ( int i = 0 ; i < node.PhaseFixedX.Count ; i++ )
                    {
                        ii = 2 * i;
                        iii = ii + 1;
                        fix[ii] = node.PhaseFixedX[i] ? 0 : 1;
                        fix[iii] = node.PhaseFixedY[i] ? 0 : 1;
                        load[ii] = node.XLoad[i];
                        load[iii] = node.YLoad[i];
                    }

                    tw.WriteLine( "{0}\t{1}\t{2}\t{3}\t{4}" ,
                                    node.Number ,
                                    node.X , node.Y ,
                                    string.Join( "\t" , fix ) ,
                                    string.Join( "\t" , load ) );
                }
            }

            // only create the element file if this call is for final output
            // (i.e. it is not be used to subsequently create tri elements)
            if ( output )
            {
                path[path.Length - 1] = "ele";
                using ( TextWriter tw = new StreamWriter( string.Join( "." , path ) ) )
                {
                    tw.WriteLine( "{0}\t{1}" ,
                                    /*NEL=*/ elements.Count ,
                                    /*NNODEL=*/ 4 );

                    int[] mtlIndex = new int[elements[0].PhaseMaterials.Count];
                    int nullIndex = canvas.MaterialTypes.FindIndex( delegate( MaterialType m ) { return m.Name == "NULL"; } ) + 1;
                    foreach ( fe4NodedQuadElement element in elements )
                    {
                        for ( int i = 0 ; i < element.PhaseMaterials.Count ; i++ )
                        {
                            mtlIndex[i] = 
                                canvas.MaterialTypes.FindIndex( delegate( MaterialType m ) { return m == element.PhaseMaterials[i]; } )
                                + 1;
                            if ( mtlIndex[i] == nullIndex ) mtlIndex[i] = 0;
                        }
                        //mtlIndex = canvas.MaterialTypes.FindIndex( delegate( MaterialType m ) { return m == element.Material; } );

                        tw.WriteLine( "{0}\t{1}\t{2}\t{3}\t{4}\t{5}" ,
                                        element.Number ,
                                        element.Nodes[0].Number ,
                                        element.Nodes[1].Number ,
                                        element.Nodes[2].Number ,
                                        element.Nodes[3].Number ,
                                        string.Join( "\t" , mtlIndex ) );
                                        //mtlIndex + 1 );
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
                //MessageBox.Show(string.Format("{0}", elementArea), "Element Area");
            }

            path[path.Length - 1] = "bel";
            using ( TextWriter tw = new StreamWriter( string.Join( "." , path ) ) )
            {
                tw.WriteLine( "{0}\t{1}" ,
                                /*NELB=*/ boundElements.Count ,
                                /*NNODELB=*/ 2 );

                double[] nload = new double[2 * boundElements[0].NLoads[0].Count];
                double[] tload = new double[2 * boundElements[0].TLoads[0].Count];
                int i0 , i1;
                foreach ( fe2NodedBoundElement element in boundElements )
                {
                    for ( int i = 0 ; i < element.NLoads[0].Count ; i++ )
                    {
                        i0 = 2 * i;
                        i1 = i0 + 1;

                        nload[i0] = element.NLoads[0][i];
                        nload[i1] = element.NLoads[1][i];
                        tload[i0] = element.TLoads[0][i];
                        tload[i1] = element.TLoads[1][i];
                    }

                    tw.WriteLine( "{0}\t{1}\t{2}\t{3}\t{4}" ,
                                    element.Number ,
                                    element.Nodes[0].Number , element.Nodes[1].Number ,
                                    string.Join( "\t" , nload ) ,
                                    string.Join( "\t" , tload ) );
                                    //string.Join( "\t" , element.NLoads[0] ) ,
                                    //string.Join( "\t" , element.NLoads[1] ) ,
                                    //string.Join( "\t" , element.TLoads[0] ) ,
                                    //string.Join( "\t" , element.TLoads[1] ) );
                                    //element.NLoads[0] , element.NLoads[1] ,
                                    //element.TLoads[0] , element.TLoads[1] );
                }
            }


            // **********************************************
            // add graphics polygons to elements for display
            // and add the polygons to the canvas (if the call
            // is not being used to subsequently create tri
            // elements)
            // **********************************************
            if ( output )
            {
                Polygon newPolygon;
                double x , y;
                for ( int i = 0 ; i < elements.Count ; i++ )
                {
                    newPolygon = new Polygon();
                    newPolygon.StrokeThickness = 0.6;
                    newPolygon.Stroke = Brushes.Black;
                    newPolygon.Opacity = /*1.0*/ 0.8;
                    newPolygon.Fill = /*Brushes.White*/ elements[i].Material.Fill;

                    foreach ( feNode node in elements[i].Nodes )
                    {
                        x = node.X / (scale * factor) * dpiX + originX;
                        y = yHeight - (node.Y / (scale * factor) * dpiY + originY);
                        newPolygon.Points.Add( new Point( x , y ) );
                    }

                    elements[i].Boundary = newPolygon;
                    canvas.Children.Add( newPolygon );
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
        public static List<fe3NodedTriElement> MeshGenStructured3NodedTri ( SlopeCanvas canvas , double colWidth , double rowHeight )
        {
            // run the 4-noded quad element generator (this forms the basis for splitting into 3-noded triangular elements)
            List<fe4NodedQuadElement> quadElements = MeshGenStructured4NodedQuad( canvas , colWidth , rowHeight , false );

            // list for storing the 3-noded triangular elements
            List<fe3NodedTriElement> elements = new List<fe3NodedTriElement>();

            // count of 3-noded triangular elements
            int triElementCount = 0;

            // for unboxing feNode object references
            feNode node1 , node2 , node3;

            // for creating nodes
            int numPhases = canvas.AnalysisPhases.Count - 1;

            // ******************************************
            // find quad elements that have repeated
            // nodes, delete the duplicate, and create
            // a tri element
            // ******************************************
            for ( int i = quadElements.Count - 1 ; i >= 0 ; i-- )
            {
                for ( int j = 0 ; j < quadElements[i].Nodes.Count ; j++ )
                {
                    // check if the element contains a repeated node
                    if ( quadElements[i].Nodes.Count( delegate( feNode node ) { return node == quadElements[i].Nodes[j]; } ) > 1 )
                    {
                        // remove the repeated node
                        quadElements[i].Nodes.RemoveAt( j );

                        // get the three vertex nodes
                        node1 = quadElements[i].Nodes[0];
                        node2 = quadElements[i].Nodes[1];
                        node3 = quadElements[i].Nodes[2];

                        // create a new tri element
                        elements.Add( new fe3NodedTriElement( quadElements[i].Parent , ++triElementCount ,
                            node1 , node2 , node3 ,
                            quadElements[i].Material ,
                            quadElements[i].PhaseMaterials , false ) );

                        // remove the old quad element
                        quadElements.RemoveAt( i );

                        break;
                    }
                }
            }


            // ******************************************
            // split remaining quad elements along their
            // short diagonal
            // ******************************************
            while ( quadElements.Count > 0 )
            {
                if ( feNode.Dist( quadElements[0].Nodes[0] , quadElements[0].Nodes[2] )
                    < feNode.Dist( quadElements[0].Nodes[1] , quadElements[0].Nodes[3] ) )
                {
                    node1 = quadElements[0].Nodes[0];

                    // element 1
                    node2 = quadElements[0].Nodes[1];
                    node3 = quadElements[0].Nodes[2];
                    elements.Add( new fe3NodedTriElement( quadElements[0].Parent , ++triElementCount ,
                            node1 , node2 , node3 ,
                            quadElements[0].Material ,
                            quadElements[0].PhaseMaterials , false ) );

                    // element 2
                    node2 = node3;
                    node3 = quadElements[0].Nodes[3];
                    elements.Add( new fe3NodedTriElement( quadElements[0].Parent , ++triElementCount ,
                            node1 , node2 , node3 ,
                            quadElements[0].Material ,
                            quadElements[0].PhaseMaterials , false ) );
                }
                else
                {
                    node1 = quadElements[0].Nodes[1];

                    // element 1
                    node2 = quadElements[0].Nodes[3];
                    node3 = quadElements[0].Nodes[0];
                    elements.Add( new fe3NodedTriElement( quadElements[0].Parent , ++triElementCount ,
                            node1 , node2 , node3 ,
                            quadElements[0].Material ,
                            quadElements[0].PhaseMaterials , false ) );

                    // element 2
                    node3 = node2;
                    node2 = quadElements[0].Nodes[2];
                    elements.Add( new fe3NodedTriElement( quadElements[0].Parent , ++triElementCount ,
                            node1 , node2 , node3 ,
                            quadElements[0].Material ,
                            quadElements[0].PhaseMaterials , false ) );
                }

                // delete the quad element
                quadElements.RemoveAt( 0 );
            }


            // **********************************************
            // eliminate elements with centroid outside
            // their corresponding substruct
            // **********************************************
            feNode centroidNode;
            foreach ( feSubstruct substruct in canvas.FEASubstructs )
            {
                // check all elements
                for ( int i = elements.Count - 1 ; i >= 0 ; i-- )
                {
                    // if parent is not correct, skip this element
                    if ( elements[i].Parent != substruct ) continue;

                    // get node references
                    node1 = elements[i].Nodes[0];
                    node2 = elements[i].Nodes[1];
                    node3 = elements[i].Nodes[2];

                    // compute centroid
                    centroidNode = new feNode( 0 , false ,
                        (node1.X + node2.X + node3.X) / 3.0 ,
                        (node1.Y + node2.Y + node3.Y) / 3.0 ,
                        0 );

                    // if centroid not inside substruct, delete the element
                    if ( !centroidNode.IsInside( substruct ) ) elements.RemoveAt( i );
                }
            }


            // **********************************************
            // eliminate elements with zero area
            // **********************************************
            for ( int i = elements.Count - 1 ; i >= 0 ; i-- )
            {
                if ( elements[i].Area < 1e-5 ) elements.RemoveAt( i );
            }


            // **********************************************
            // renumber elements in case of elimination
            // **********************************************
            for ( int i = 0 ; i < elements.Count ; i++ )
            {
                elements[i].Number = i + 1;
            }


            // get canvas dimensions/properties
            double originX = canvas.OriginOffsetX ,
                   originY = canvas.OriginOffsetY ,
                   dpiX = canvas.DpiX ,
                   dpiY = canvas.DpiY ,
                   scale = canvas.Scale ,
                   yHeight = canvas.ActualHeight;
            Units units = canvas.Units;

            // get units dependent scaling factor
            double factor;
            switch ( units )
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
            string[] path = canvas.FilePath.Split( '.' );
            path[path.Length - 1] = "ele";
            using ( TextWriter tw = new StreamWriter( string.Join( "." , path ) ) )
            {
                tw.WriteLine( "{0}\t{1}" ,
                                /*NEL=*/ elements.Count ,
                                /*NNODEL=*/ 3 );

                int[] mtlIndex = new int[elements[0].PhaseMaterials.Count];
                int nullIndex = canvas.MaterialTypes.FindIndex( delegate( MaterialType m ) { return m.Name == "NULL"; } ) + 1;
                foreach ( fe3NodedTriElement element in elements )
                {
                    for ( int i = 0 ; i < element.PhaseMaterials.Count ; i++ )
                    {
                        mtlIndex[i] =
                            canvas.MaterialTypes.FindIndex( delegate( MaterialType m ) { return m == element.PhaseMaterials[i]; } )
                            + 1;
                        if ( mtlIndex[i] == nullIndex ) mtlIndex[i] = 0;
                    }

                    //mtlIndex = canvas.MaterialTypes.FindIndex( delegate( MaterialType m ) { return m == element.Material; } );
                    tw.WriteLine( "{0}\t{1}\t{2}\t{3}\t{4}" ,
                                    element.Number ,
                                    element.Nodes[0].Number ,
                                    element.Nodes[1].Number ,
                                    element.Nodes[2].Number ,
                                    string.Join( "\t" , mtlIndex ) );
                                    //mtlIndex + 1 );
                }
            }


            // **********************************************
            // add graphics polygons to elements for display
            // and add the polygons to the canvas
            // **********************************************
            Polygon newPolygon;
            double x , y;
            for ( int i = 0 ; i < elements.Count ; i++ )
            {
                newPolygon = new Polygon();
                newPolygon.StrokeThickness = 0.6;
                newPolygon.Stroke = Brushes.Black;
                newPolygon.Opacity = /*1.0*/ 0.8;
                newPolygon.Fill = /*Brushes.White*/ elements[i].Material.Fill;

                foreach ( feNode node in elements[i].Nodes )
                {
                    x = node.X / (scale * factor) * dpiX + originX;
                    y = yHeight - (node.Y / (scale * factor) * dpiY + originY);
                    newPolygon.Points.Add( new Point( x , y ) );
                }

                elements[i].Boundary = newPolygon;
                canvas.Children.Add( newPolygon );
            }


            //// *********************************************
            //// for printing total element area (debugging)
            //// *********************************************
            //double elementArea = 0;
            //foreach (fe3NodedTriElement element in elements)
            //{
            //    elementArea += element.Area;
            //}
            //MessageBox.Show(string.Format("{0}", elementArea), "Element Area");

            return elements;

        } // END OF MeshGenStructured3NodedTri
    }
}