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
using System.Linq;
using System.Windows;
using System.Windows.Shapes;

namespace SlopeFEA
{
    /// <summary>
    /// feNode - Class for finite element nodes (1d, 2d, 3d)
    /// </summary>
    class feNode : IEquatable<feNode>
    {
        /// <summary>
        /// Constructor for 1d FEA node.
        /// </summary>
        /// <param name="number">Node number.</param>
        /// <param name="x">Global coordinate.</param>
        public feNode (int number,
                        double x)
        {
            this.Number = number;
            this.IsBoundary = false;
            this.X = x;
            this.Y = 0.0;
            this.Z = 0.0;
            this.IsFixedX = false;
            this.IsFixedY = false;
            this.IsFixedZ = false;
        }

        /// <summary>
        /// Constructor for 2d FEA node.
        /// </summary>
        /// <param name="number">Node number.</param>
        /// <param name="isBoundary">Indicates if the node is on a substruct boundary.</param>
        /// <param name="x">Global x-coordinate.</param>
        /// <param name="y">Global y-coordinate.</param>
        public feNode (int number,
                        bool isBoundary,
                        double x,
                        double y)
        {
            this.Number = number;
            this.IsBoundary = isBoundary;
            this.X = x;
            this.Y = y;
            this.Z = 0.0;
            this.IsFixedX = false;
            this.IsFixedY = false;
            this.IsFixedZ = false;
        }

        /// <summary>
        /// Constructor for 3d FEA node.
        /// </summary>
        /// <param name="number">Node number.</param>
        /// <param name="isBoundary">Indicates if the node is on a substruct boundary.</param>
        /// <param name="x">Global x-coordinate.</param>
        /// <param name="y">Global y-coordinate.</param>
        /// <param name="z">Global z-coordinate.</param>
        public feNode (int number,
                        bool isBoundary,
                        double x,
                        double y,
                        double z)
        {
            this.Number = number;
            this.IsBoundary = isBoundary;
            this.X = x;
            this.Y = y;
            this.Z = z;
            this.IsFixedX = false;
            this.IsFixedY = false;
            this.IsFixedZ = false;
        }

        /// <summary>
        /// Node number property.
        /// </summary>
        public int Number { get; set; }

        /// <summary>
        /// Boundary node flag property.
        /// </summary>
        public bool IsBoundary { get; set; }

        /// <summary>
        /// Indicates if the node is locked to its current coordinates.
        /// (Typically checked prior to a call to Merge().)
        /// </summary>
        public bool IsLocked { get; set; }

        /// <summary>
        /// Global x-coordinate property.
        /// </summary>
        public double X { get; set; }

        /// <summary>
        /// Global x-fixity property.
        /// </summary>
        public bool IsFixedX { get; set; }

        /// <summary>
        /// Global y-coordinate property.
        /// </summary>
        public double Y { get; set; }

        /// <summary>
        /// Global y-fixity property.
        /// </summary>
        public bool IsFixedY { get; set; }

        /// <summary>
        /// Global z-coordinate property.
        /// </summary>
        public double Z { get; set; }

        /// <summary>
        /// Global z-fixity property.
        /// </summary>
        public bool IsFixedZ { get; set; }


        /// <summary>
        /// Merge nodes combining coordinates as specified by mergeCoords,
        /// combining boundary and locked flags taking true as dominant,
        /// and taking the minimum node number. Note: It is important to
        /// subsequently replace references to the duplicate node in
        /// elements once the merge has been completed.
        /// </summary>
        /// <param name="m">Node to be merged with this node.</param>
        /// <param name="mergeCoords">If true, take average coordinates; if false, take coordinates of m.</param>
        public void Merge (feNode m, bool mergeCoords)
        {
            if (mergeCoords)
            {
                this.X = 0.5 * (this.X + m.X);
                this.Y = 0.5 * (this.Y + m.Y);
                this.Z = 0.5 * (this.Y + m.Y);
            }
            else
            {
                this.X = m.X;
                this.Y = m.Y;
                this.Z = m.Z;
            }

            this.IsBoundary = this.IsBoundary || m.IsBoundary;
            this.IsLocked = this.IsLocked || m.IsLocked;
            this.Number = Math.Min(this.Number, m.Number);
            this.IsFixedX = this.IsFixedX || m.IsFixedX;
            this.IsFixedY = this.IsFixedY || m.IsFixedY;
            this.IsFixedZ = this.IsFixedZ || m.IsFixedZ;
        }


        /// <summary>
        /// Determines if this node is inside the given feSubstruct.
        /// 
        /// Adapted from the simplified winding algorithm described in:
        /// 
        ///     B. Stein. 1997. "A Point About Polygons." Linux Journal.
        ///         Article: http://www.linuxjournal.com/article/2029?page=0,0
        ///         Source: http://www.linuxjournal.com/files/linuxjournal.com/linuxjournal/articles/020/2029/2029l1.html
        /// 
        /// Note that this function should only be used for checking if a node is inside
        /// the substruct for which meshing is currently being performed since it
        /// automatically returns true if the node is flagged as on the boundary.
        /// </summary>
        /// <param name="substruct">The substructure (block of material) polygon to check.</param>
        /// <returns>true if inside, false otherwise.</returns>
        public bool IsInside (feSubstruct substruct)
        {
            // return true in special case of a boundary node
            // (for efficiency and to reduce errors due to floating point
            // representation of coordinates)
            if (this.IsBoundary) return true;

            // initialize local variables
            Point p1, p2;
            bool inside = false;

            // node cannot be inside a straight line
            if (substruct.Points.Count < 3) return inside;

            // polygon boundary iterators
            Point oldPoint = new Point(substruct.Points[substruct.Points.Count - 1].X,
                substruct.Points[substruct.Points.Count - 1].Y);
            Point newPoint;

            // loop through boundary checking if line drawn from this
            // node to infinity along the y axis is intersected by
            // each line segment of the polygon
            for (int i = 0; i < substruct.Points.Count; i++)
            {
                // update iterator
                newPoint = new Point(substruct.Points[i].X, substruct.Points[i].Y);

                // if the node lies exactly at a vertex, it is inside ...
                if (newPoint.X == this.X && newPoint.Y == this.Y) return true;

                // ... otherwise set left and right points accordingly
                else if (newPoint.X > oldPoint.X)
                {
                    p1 = oldPoint;
                    p2 = newPoint;
                }
                else
                {
                    p1 = newPoint;
                    p2 = oldPoint;
                }

                // if the line segment straddles the y-axis and
                // the intersection occurs on the positive side of
                // this node, flip the winding bit
                if ((newPoint.X < this.X) == (this.X <= oldPoint.X)
                    && (this.Y - p1.Y) * (p2.X - p1.X) < (p2.Y - p1.Y) * (this.X - p1.X))
                {
                    inside = !inside;
                }

                // step the iterator forward
                oldPoint = newPoint;

            }

            return inside;
        }

        /// <summary>
        /// Determines if this node is inside the given fe3NodedTriElement.
        /// (see documentation/commentary for feSubstruct input overload)
        /// </summary>
        public bool IsInside (fe3NodedTriElement element)
        {
            Point p1, p2;

            bool inside = false;

            if (element.Nodes.Count < 3) return inside;

            Point oldPoint = new Point(element.Nodes[element.Nodes.Count - 1].X,
                element.Nodes[element.Nodes.Count - 1].Y);

            Point newPoint;
            for (int i = 0; i < element.Nodes.Count; i++)
            {
                newPoint = new Point(element.Nodes[i].X, element.Nodes[i].Y);

                if (newPoint.X > oldPoint.X)
                {
                    p1 = oldPoint;
                    p2 = newPoint;
                }
                else
                {
                    p1 = newPoint;
                    p2 = oldPoint;
                }

                if ((newPoint.X <= this.X) == (this.X <= oldPoint.X)
                    && (this.Y - p1.Y) * (p2.X - p1.X) < (p2.Y - p1.Y) * (this.X - p1.X))
                {
                    inside = !inside;
                }

                oldPoint = newPoint;

            }

            return inside;
        }


        /// <summary>
        /// Determines if this node is inside the given fe4NodedQuadElement.
        /// (see documentation/commentary for feSubstruct input overload)
        /// </summary>
        public bool IsInside (fe4NodedQuadElement element)
        {
            Point p1, p2;

            bool inside = false;

            if (element.Nodes.Count < 3) return inside;

            Point oldPoint = new Point(element.Nodes[element.Nodes.Count - 1].X,
                element.Nodes[element.Nodes.Count - 1].Y);

            Point newPoint;
            for (int i = 0; i < element.Nodes.Count; i++)
            {
                newPoint = new Point(element.Nodes[i].X, element.Nodes[i].Y);

                if (newPoint.X == this.X && newPoint.Y == this.Y) return true;
                else if (newPoint.X > oldPoint.X)
                {
                    p1 = oldPoint;
                    p2 = newPoint;
                }
                else
                {
                    p1 = newPoint;
                    p2 = oldPoint;
                }

                if ((newPoint.X < this.X) == (this.X <= oldPoint.X)
                    && (this.Y - p1.Y) * (p2.X - p1.X) < (p2.Y - p1.Y) * (this.X - p1.X))
                {
                    inside = !inside;
                }

                oldPoint = newPoint;

            }

            return inside;
        }


        /// <summary>
        /// Determines the shortest distance between the given feNodes based on Cartesian coordinates.
        /// </summary>
        /// <param name="n1">First node.</param>
        /// <param name="n2">Second node.</param>
        /// <returns>Linear distance between the nodes.</returns>
        public static double Dist (feNode n1, feNode n2)
        {
            return Math.Sqrt(Math.Pow(n1.X - n2.X, 2) + Math.Pow(n1.Y - n2.Y, 2) + Math.Pow(n1.Z - n2.Z, 2));
        }


        /// <summary>
        /// Comparison of nodes based on node number (for use with sorting algorithms).
        /// </summary>
        /// <param name="n0">First node.</param>
        /// <param name="n1">Second node.</param>
        /// <returns>1 if n0 is greater, 0 if they are equal, -1 if n1 is greater</returns>
        public static int CompareNodesByNumber (feNode n0, feNode n1)
        {
            if (n0 == null)
            {
                if (n1 == null)
                {
                    // if both are null, they are equal
                    return 0;
                }
                else
                {
                    // if n0 is null and n1 is not null, n1 is greater
                    return -1;
                }
            }
            else
            {
                if (n1 == null)
                {
                    // if n0 is not null and n1 is null, n0 is greater
                    return 1;
                }

                // if both are not null, compare by node number
                else
                {
                    if (n0.Number > n1.Number) return 1;
                    else if (n0.Number == n1.Number) return 0;
                    else return -1;
                }
            }
        }

        /// <summary>
        /// Comparison of nodes based on coordinates (for use with sorting algorithms).
        /// The comparison is horizontally dominant, that is, coordinates are sorted
        /// first by X then by Y.
        /// </summary>
        /// <param name="n0">First node.</param>
        /// <param name="n1">Second node.</param>
        /// <returns>1 if n0 is greater, 0 if they are equal, -1 if n1 is greater</returns>
        public static int CompareNodesHorizontally (feNode n0, feNode n1)
        {
            if (n0 == null)
            {
                if (n1 == null)
                {
                    // if both are null, they are equal
                    return 0;
                }
                else
                {
                    // if n0 is null and n1 is not null, n1 is greater
                    return -1;
                }
            }
            else
            {
                if (n1 == null)
                {
                    // if n0 is not null and n1 is null, n0 is greater
                    return 1;
                }

                // if both are not null, compare by X then Y
                else
                {
                    double toler = 1e-3;

                    // check if nodes are on same approximate vertical line
                    if (Math.Abs(n0.X - n1.X) < toler)
                    {
                        // compare by y-coord
                        if (Math.Abs(n0.Y - n1.Y) < toler) return 0;
                        else if (n0.Y > n1.Y) return 1;
                        else return -1;
                    }

                    // otherwise, compare by x-coord alone
                    else if (n0.X > n1.X)
                    {
                        return 1;
                    }
                    else
                    {
                        return -1;
                    }
                }
            }
        }


        /// <summary>
        /// Comparison of nodes based on coordinates (for use with sorting algorithms).
        /// The comparison is vertically dominant, that is, coordinates are sorted
        /// first by Y then by X.
        /// </summary>
        /// <param name="n0">First node.</param>
        /// <param name="n1">Second node.</param>
        /// <returns>1 if n0 is greater, 0 if they are equal, -1 if n1 is greater</returns>
        public static int CompareNodesVertically (feNode n0, feNode n1)
        {
            if (n0 == null)
            {
                if (n1 == null)
                {
                    // if both are null, they are equal
                    return 0;
                }
                else
                {
                    // if n0 is null and n1 is not null, n1 is greater
                    return -1;
                }
            }
            else
            {
                if (n1 == null)
                {
                    // if n0 is not null and n1 is null, n0 is greater
                    return 1;
                }

                // if both are not null, compare by Y then X
                else
                {
                    double toler = 1e-3;

                    // check if nodes are on same approximate horizontal line
                    if (Math.Abs(n0.Y - n1.Y) < toler)
                    {
                        // compare by x-coord
                        if (Math.Abs(n0.X - n1.X) < toler) return 0;
                        else if (n0.X > n1.X) return 1;
                        else return -1;
                    }

                    // otherwise, compare by y-coord alone
                    else if (n0.Y > n1.Y)
                    {
                        return 1;
                    }
                    else
                    {
                        return -1;
                    }
                }
            }
        }


        /// <summary>
        /// Comparison of nodes based on coordinates (for use with sorting algorithms).
        /// The comparison is based on distance from the origin.
        /// </summary>
        /// <param name="n0">First node.</param>
        /// <param name="n1">Second node.</param>
        /// <returns>1 if n0 is greater, 0 if they are equal, -1 if n1 is greater</returns>
        public static int CompareNodesByOriginDist (feNode n0, feNode n1)
        {
            if (n0 == null)
            {
                if (n1 == null)
                {
                    // if both are null, they are equal
                    return 0;
                }
                else
                {
                    // if n0 is null and n1 is not null, n1 is greater
                    return -1;
                }
            }
            else
            {
                if (n1 == null)
                {
                    // if n0 is not null and n1 is null, n0 is greater
                    return 1;
                }

                // if both are not null, compare by node number
                else
                {

                    double origDist0 = (n0.X * n0.X) + (n0.Y * n0.Y) + (n0.Z * n0.Z);
                    double origDist1 = (n1.X * n1.X) + (n1.Y * n1.Y) + (n1.Z * n1.Z);

                    if (origDist0 > origDist1) return 1;
                    else if (origDist0 == origDist1) return 0;
                    else return -1;
                }
            }
        }


        /// <summary>
        /// Override for object comparison.
        /// </summary>
        /// <returns>Hash code based on node number.</returns>
        public override int GetHashCode ()
        {
            return this.Number.GetHashCode();
        }

        /// <summary>
        /// Override for object comparison.
        /// </summary>
        /// <returns>true if objects referenced are the same, false otherwise.</returns>
        public override bool Equals (object obj)
        {
            return base.Equals(obj);
        }

        /// <summary>
        /// Override for IEquatable interface.
        /// </summary>
        /// <returns>true if objects referenced are the same, false otherwise.</returns>
        public bool Equals (feNode other)
        {
            return System.Object.Equals(this, other);
        }
    }


    /// <summary>
    /// feLineConstraint - Class for applying horizontal and vertical fixities along lines.
    /// </summary>
    class feLineConstraint
    {
        private bool isFixedX, isFixedY;

        /// <summary>
        /// Class constructor.
        /// </summary>
        /// <param name="n1">First node.</param>
        /// <param name="n2">Second node.</param>
        /// <param name="fixX">true if x-displacement is fixed, false otherwise.</param>
        /// <param name="fixY">true if y-displacement is fixed, false otherwise.</param>
        public feLineConstraint(Point n1, Point n2,
                                bool fixX, bool fixY)
        {
            // create list of boundary nodes for the constraint
            Points = new List<Point>() { n1, n2 };

            // set visibility of constraints
            this.isFixedX = fixX;
            this.isFixedY = fixY;
        }

        public List<Point> Points { get; set; }

        public bool IsFixedX { get { return this.isFixedX; } }
        public bool IsFixedY { get { return this.isFixedY; } }
    }


    /// <summary>
    /// feSubstruct - Class for FEA substructs (material blocks)
    /// </summary>
    class feSubstruct
    {
        /// <summary>
        /// Class constructor.
        /// </summary>
        /// <param name="material">The type of material contained in the block.</param>
        public feSubstruct (MaterialType material)
        {
            this.Material = material;

            this.Points = new List<Point>();
            this.IsFixedX = new List<bool>();
            this.IsFixedY = new List<bool>();
            this.LineConstraints = new List<feLineConstraint>();
        }

        /// <summary>
        /// Material type property.
        /// </summary>
        public MaterialType Material { get; set; }

        /// <summary>
        /// Boundary point list property.
        /// </summary>
        public List<Point> Points { get; set; }

        /// <summary>
        /// Line constraint list property.
        /// </summary>
        public List<feLineConstraint> LineConstraints { get; set; }

        /// <summary>
        /// Boundary point x-fixity property.
        /// </summary>
        public List<bool> IsFixedX { get; set; }

        /// <summary>
        /// Boundary point y-fixity property.
        /// </summary>
        public List<bool> IsFixedY { get; set; }

        /// <summary>
        /// Minimum x-coordinate property.
        /// </summary>
        public double XMin
        {
            get
            {
                double xmin = Points[0].X;
                for (int i = 1; i < Points.Count; i++) xmin = Math.Min(xmin, Points[i].X);
                return xmin;
            }
        }

        /// <summary>
        /// Maximum x-coordinate property.
        /// </summary>
        public double XMax
        {
            get
            {
                double xmax = Points[0].X;
                for (int i = 1; i < Points.Count; i++) xmax = Math.Max(xmax, Points[i].X);
                return xmax;
            }
        }

        /// <summary>
        /// Minimum y-coordinate property.
        /// </summary>
        public double YMin
        {
            get
            {
                double ymin = Points[0].Y;
                for (int i = 1; i < Points.Count; i++) ymin = Math.Min(ymin, Points[i].Y);
                return ymin;
            }
        }

        /// <summary>
        /// Maximum y-coordinate property.
        /// </summary>
        public double YMax
        {
            get
            {
                double ymax = Points[0].Y;
                for (int i = 1; i < Points.Count; i++) ymax = Math.Max(ymax, Points[i].Y);
                return ymax;
            }
        }

        /// <summary>
        /// Substruct area property.
        /// 
        /// Based on computation for non-self-intersecting polygons described at:
        /// 
        ///     E.W. Weisstein. "Polygon Area." From MathWorld--A Wolfram Web Resource.
        ///         Article: http://mathworld.wolfram.com/PolygonArea.html
        /// 
        /// Note: The value will be positive if the points are ordered CCW and negative
        /// if ordered CW.
        /// </summary>
        public double Area
        {
            get
            {
                double a = Points[Points.Count - 1].X * Points[0].Y - Points[0].X * Points[Points.Count - 1].Y;
                for (int i = 1; i < Points.Count; i++)
                {
                    a += Points[i - 1].X * Points[i].Y - Points[i].X * Points[i - 1].Y;
                }
                return 0.5*a;
            }
        }

        /// <summary>
        /// Ensure point order is CCW.
        /// </summary>
        public void SortPoints ()
        {
            if (this.Area < 0)
            {
                Points.Reverse();
                IsFixedX.Reverse();
                IsFixedY.Reverse();
            }
        }

    }

    
    /// <summary>
    /// fe2NodedBoundElement - Class for boundary elements (used to apply tractions).
    /// </summary>
    class fe2NodedBoundElement
    {
        /// <summary>
        /// Class constructor.
        /// </summary>
        /// <param name="number">Element number.</param>
        /// <param name="n1">Member node 1.</param>
        /// <param name="n2">Member node 2.</param>
        public fe2NodedBoundElement (int number,
                                        feNode n1, feNode n2)
        {
            this.Number = number;

            this.Nodes = new List<feNode>(2);
            this.Nodes.Add(n1);
            this.Nodes.Add(n2);
        }

        /// <summary>
        /// Element number property.
        /// </summary>
        public int Number { get; set; }

        /// <summary>
        /// List of member nodes property.
        /// </summary>
        public List<feNode> Nodes { get; set; }

        /// <summary>
        /// Accessor property for element length.
        /// </summary>
        public double Length
        {
            get
            {
                return Math.Sqrt(Math.Pow(Nodes[0].X - Nodes[1].X, 2) + Math.Pow(Nodes[0].Y - Nodes[1].Y, 2));
            }
        }

        /// <summary>
        /// Comparison of elements by number (for sorting algorithms).
        /// </summary>
        /// <param name="e0">First element.</param>
        /// <param name="e1">Second element.</param>
        /// <returns>1 if first element is greater, 0 if they are equal, -1 if second element is greater.</returns>
        public static int CompareElementsByNumber (fe2NodedBoundElement e0, fe2NodedBoundElement e1)
        {
            if (e0 == null)
            {
                if (e1 == null)
                {
                    // if both are null, they are equal
                    return 0;
                }
                else
                {
                    // if n0 is null and n1 is not null, n1 is greater
                    return -1;
                }
            }
            else
            {
                if (e1 == null)
                {
                    // if n0 is not null and n1 is null, n0 is greater
                    return 1;
                }

                // if both are not null, compare by node number
                else
                {
                    if (e0.Number > e1.Number) return 1;
                    else if (e0.Number == e1.Number) return 0;
                    else return -1;
                }
            }
        }
    }

    /// <summary>
    /// fe3NodedTriElement - Class for 3-noded triangular FEA element.
    /// </summary>
    class fe3NodedTriElement
    {
        feSubstruct parent;

        /// <summary>
        /// Parent unaware constructor (for use ONLY in loading existing .nod and .ele files).
        /// </summary>
        /// <param name="number">Element number.</param>
        /// <param name="n1">First node.</param>
        /// <param name="n2">Second node.</param>
        /// <param name="n3">Third node.</param>
        /// <param name="material">Material type.</param>
        /// <param name="sort">Flag to initially sort the points.</param>
        public fe3NodedTriElement (int number,
                                    feNode n1, feNode n2, feNode n3,
                                    MaterialType material,
                                    bool sort)
        {
            this.Number = number;
            this.Material = material;

            this.Nodes = new List<feNode>(3);
            this.Nodes.Add(n1);
            this.Nodes.Add(n2);
            this.Nodes.Add(n3);

            if (sort) SortNodes();
        }

        /// <summary>
        /// Parent aware constructor (for use in MeshGen).
        /// </summary>
        /// <param name="parent">Parent substructure.</param>
        /// <param name="number">Element number.</param>
        /// <param name="n1">First node.</param>
        /// <param name="n2">Second node.</param>
        /// <param name="n3">Third node.</param>
        /// <param name="material">Material type.</param>
        /// <param name="sort">Flag to initially sort the points.</param>
        public fe3NodedTriElement (feSubstruct parent, int number,
                                    feNode n1, feNode n2, feNode n3,
                                    MaterialType material,
                                    bool sort)
        {
            this.parent = parent;
            this.Number = number;
            this.Material = material;

            this.Nodes = new List<feNode>(3);
            this.Nodes.Add(n1);
            this.Nodes.Add(n2);
            this.Nodes.Add(n3);

            if (sort) SortNodes();
        }


        /// <summary>
        /// Parent substructure property.
        /// </summary>
        public feSubstruct Parent { get { return this.parent; } }


        /// <summary>
        /// Element number property.
        /// </summary>
        public int Number { get; set; }

        /// <summary>
        /// Material type property.
        /// </summary>
        public MaterialType Material { get; set; }

        /// <summary>
        /// Boundary polygon property (for graphical display).
        /// </summary>
        public Polygon Boundary { get; set; }

        /// <summary>
        /// List of boundary nodes property.
        /// </summary>
        public List<feNode> Nodes { get; set; }

        /// <summary>
        /// Element area property.
        /// 
        /// Based on computation for non-self-intersecting polygons described at:
        /// 
        ///     E.W. Weisstein. "Polygon Area." From MathWorld--A Wolfram Web Resource.
        ///         Article: http://mathworld.wolfram.com/PolygonArea.html
        /// 
        /// Note: The value will be positive if the points are ordered CCW and negative
        /// if ordered CW.
        /// </summary>
        public double Area
        {
            get
            {
                return 0.5 * (Nodes[0].X * Nodes[1].Y - Nodes[1].X * Nodes[0].Y
                            + Nodes[1].X * Nodes[2].Y - Nodes[2].X * Nodes[1].Y
                            + Nodes[2].X * Nodes[0].Y - Nodes[0].X * Nodes[2].Y);
            }
        }

        /// <summary>
        /// Sort nodes in CCW order.
        /// </summary>
        public void SortNodes ()
        {
            if (this.Area < 0)
            {
                feNode tmp = Nodes[1];
                Nodes[1] = Nodes[2];
                Nodes[2] = tmp;
            }
        }

        /// <summary>
        /// Comparison of elements by number (for sorting algorithms).
        /// </summary>
        /// <param name="e0">First element.</param>
        /// <param name="e1">Second element.</param>
        /// <returns>1 if first element is greater, 0 if they are equal, -1 if second element is greater.</returns>
        public static int CompareElementsByNumber (fe3NodedTriElement e0, fe3NodedTriElement e1)
        {
            if (e0 == null)
            {
                if (e1 == null)
                {
                    // if both are null, they are equal
                    return 0;
                }
                else
                {
                    // if n0 is null and n1 is not null, n1 is greater
                    return -1;
                }
            }
            else
            {
                if (e1 == null)
                {
                    // if n0 is not null and n1 is null, n0 is greater
                    return 1;
                }

                // if both are not null, compare by node number
                else
                {
                    if (e0.Number > e1.Number) return 1;
                    else if (e0.Number == e1.Number) return 0;
                    else return -1;
                }
            }
        }

        /// <summary>
        /// Translates the graphics coordinates of the boundary polygon.
        /// </summary>
        /// <param name="delta">Translation vector.</param>
        public void Translate (Vector delta)
        {
            Point p;
            for (int i = 0; i < Boundary.Points.Count; i++)
            {
                p = Boundary.Points[i];
                p.X += delta.X;
                p.Y += delta.Y;
                Boundary.Points[i] = p;
            }
        }

        /// <summary>
        /// Zooms the graphics coordinates of the boundary polygon.
        /// </summary>
        /// <param name="factor">Scaling factor.</param>
        /// <param name="centre">Centre of graphics canvas.</param>
        public void Zoom (double factor, Point centre)
        {
            Point p;
            for (int i = 0; i < Boundary.Points.Count; i++)
            {
                p = Boundary.Points[i];
                p.X = centre.X + factor * (p.X - centre.X);
                p.Y = centre.Y + factor * (p.Y - centre.Y);
                Boundary.Points[i] = p;
            }
        }
    }

    /// <summary>
    /// fe4NodedQuadElement - Class for 4-noded quadrilateral FEA elements.
    /// </summary>
    class fe4NodedQuadElement
    {
        feSubstruct parent;

        /// <summary>
        /// Parent unaware constructor (for use ONLY in loading existing .nod and .ele files).
        /// </summary>
        /// <param name="number">Element number.</param>
        /// <param name="n1">First node.</param>
        /// <param name="n2">Second node.</param>
        /// <param name="n3">Third node.</param>
        /// <param name="n4">Fourth node.</param>
        /// <param name="material">Material type.</param>
        /// <param name="sort">Flag for intial sorting.</param>
        public fe4NodedQuadElement (int number,
                                    feNode n1, feNode n2, feNode n3, feNode n4,
                                    MaterialType material,
                                    bool sort)
        {
            this.Number = number;
            this.Material = material;

            this.Nodes = new List<feNode>(4);
            this.Nodes.Add(n1);
            this.Nodes.Add(n2);
            this.Nodes.Add(n3);
            this.Nodes.Add(n4);

            if (sort) SortNodes(false); // do not check for repeated nodes on initial creation
        }

        /// <summary>
        /// Parent aware constructor (for use in MeshGen).
        /// </summary>
        /// <param name="parent">Parent substructure</param>
        /// <param name="number">Element number.</param>
        /// <param name="n1">First node.</param>
        /// <param name="n2">Second node.</param>
        /// <param name="n3">Third node.</param>
        /// <param name="n4">Fourth node.</param>
        /// <param name="material">Material type.</param>
        /// <param name="sort">Flag for intial sorting.</param>
        public fe4NodedQuadElement (feSubstruct parent, int number,
                                    feNode n1, feNode n2, feNode n3, feNode n4,
                                    MaterialType material,
                                    bool sort)
        {
            this.parent = parent;
            this.Number = number;
            this.Material = material;

            this.Nodes = new List<feNode>(4);
            this.Nodes.Add(n1);
            this.Nodes.Add(n2);
            this.Nodes.Add(n3);
            this.Nodes.Add(n4);

            if (sort) SortNodes(false); // do not check for repeated nodes on initial creation
        }

        /// <summary>
        /// Parent substructure property.
        /// </summary>
        public feSubstruct Parent { get { return this.parent; } }

        /// <summary>
        /// Element number property.
        /// </summary>
        public int Number { get; set; }

        /// <summary>
        /// Material type property.
        /// </summary>
        public MaterialType Material { get; set; }

        /// <summary>
        /// Boundary polygon property (for graphical display).
        /// </summary>
        public Polygon Boundary { get; set; }

        /// <summary>
        /// List of boundary nodes property.
        /// </summary>
        public List<feNode> Nodes { get; set; }

        /// <summary>
        /// Element area property.
        /// 
        /// Based on computation for non-self-intersecting polygons described at:
        /// 
        ///     E.W. Weisstein. "Polygon Area." From MathWorld--A Wolfram Web Resource.
        ///         Article: http://mathworld.wolfram.com/PolygonArea.html
        /// 
        /// Note: The value will be positive if the points are ordered CCW and negative
        /// if ordered CW.
        /// </summary>
        public double Area
        {
            get
            {
                double a = Nodes[Nodes.Count - 1].X * Nodes[0].Y - Nodes[0].X * Nodes[Nodes.Count - 1].Y;
                for (int i = 1; i < Nodes.Count; i++)
                {
                    a += Nodes[i - 1].X * Nodes[i].Y - Nodes[i].X * Nodes[i - 1].Y;
                }
                return 0.5*a;
            }
        }

        /// <summary>
        /// Sort nodes in CCW order (and ensure repeated node is alpha and omega).
        /// </summary>
        /// <param name="checkRepeat">Flag for repeated node check.</param>
        public void SortNodes (bool checkRepeat)
        {
            if (checkRepeat)
            {
                // loop through nodes checking for repeated nodes
                int repeatCount = 0;
                List<int> repeatIndex = new List<int>();
                for (int i = 0; i < Nodes.Count; i++)
                {
                    if (Nodes.Count(delegate(feNode node) { return node == Nodes[i]; }) > 1)
                    {
                        repeatIndex.Add(i);
                        repeatCount++;
                    }
                }

                // if there was a repeated node, swap indices ensuring
                // references to repeated node are first and last in
                // node list
                if (repeatCount > 0)
                {
                    feNode tmp = Nodes[0];
                    Nodes[0] = Nodes[repeatIndex[0]];
                    Nodes[repeatIndex[0]] = tmp;

                    tmp = Nodes[3];
                    Nodes[3] = Nodes[repeatIndex[1]];
                    Nodes[repeatIndex[1]] = tmp;
                }
            }

            // check CCW order
            if (this.Area < 0) Nodes.Reverse();
        }


        /// <summary>
        /// Comparison of elements by number (for sorting algorithms).
        /// </summary>
        /// <param name="e0">First element.</param>
        /// <param name="e1">Second element.</param>
        /// <returns>1 if first element is greater, 0 if they are equal, -1 if second element is greater.</returns>
        public static int CompareElementsByNumber (fe4NodedQuadElement e0, fe4NodedQuadElement e1)
        {
            if (e0 == null)
            {
                if (e1 == null)
                {
                    // if both are null, they are equal
                    return 0;
                }
                else
                {
                    // if n0 is null and n1 is not null, n1 is greater
                    return -1;
                }
            }
            else
            {
                if (e1 == null)
                {
                    // if n0 is not null and n1 is null, n0 is greater
                    return 1;
                }

                // if both are not null, compare by node number
                else
                {
                    if (e0.Number > e1.Number) return 1;
                    else if (e0.Number == e1.Number) return 0;
                    else return -1;
                }
            }
        }

        /// <summary>
        /// Translates boundary polygon graphics coordinates.
        /// </summary>
        /// <param name="delta">Translation vector.</param>
        public void Translate (Vector delta)
        {
            Point p;
            for (int i = 0; i < Boundary.Points.Count; i++)
            {
                p = Boundary.Points[i];
                p.X += delta.X;
                p.Y += delta.Y;
                Boundary.Points[i] = p;
            }
        }

        /// <summary>
        /// Zooms boundary polygon graphics coordinates.
        /// </summary>
        /// <param name="factor">Scaling factor.</param>
        /// <param name="centre">Plotting canvas centre.</param>
        public void Zoom (double factor, Point centre)
        {
            Point p;
            for (int i = 0; i < Boundary.Points.Count; i++)
            {
                p = Boundary.Points[i];
                p.X = centre.X + factor * (p.X - centre.X);
                p.Y = centre.Y + factor * (p.Y - centre.Y);
                Boundary.Points[i] = p;
            }
        }
    }
}