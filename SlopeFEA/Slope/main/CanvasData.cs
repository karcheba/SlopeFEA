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

namespace SlopeFEA
{
    enum DrawModes { Select, Boundaries, Materials, Pan, ZoomArea, MovePoints, AddPoints, FixX, FixY };
    enum Units { Metres, Millimetres, Feet, Inches };
    enum Scales
    {
        sc1000, sc800, sc600, sc500, sc400, sc300, sc200, sc150,
        sc100, sc50, sc25, sc10, sc5, sc2, sc1, Custom
    };
    enum GridType { Major, Minor };
    enum MeshPointType { Entrance, Exit };
    enum SoilMovement { LtoR, RtoL, None };
    enum AnalysisType { Bishop, RFEM, FEA3NodedTri, FEA4NodedQuad };

    class GridPoint
    {
        private GridType type;
        private Ellipse location;
        
        public GridPoint(Point pt, GridType type)
        {
            location = new Ellipse();
            location.Height = 1.5;
            location.Width = 1.5;
            location.Fill = Brushes.Black;
            location.Stroke = Brushes.Black;
            location.Visibility = Visibility.Hidden;
            location.Margin = new Thickness(pt.X - 0.75, pt.Y - 0.75, 0, 0);

            this.type = type;
        }

        public Ellipse Location { get { return this.location; } }
        public GridType Type { get { return this.type; } }

        public bool IsVisible
        {
            set
            {
                Location.Visibility = value ? Visibility.Visible : Visibility.Hidden;
            }
        }

        public void Translate(Vector delta)
        {
            location.Margin = new Thickness(location.Margin.Left + delta.X, location.Margin.Top + delta.Y, 0, 0);
        }

        public void Zoom(double factor, Point centre)
        {
            location.Margin = new Thickness(centre.X + factor * (location.Margin.Left - centre.X),
                                            centre.Y + factor * (location.Margin.Top - centre.Y),
                                            0, 0);
        }
    }

    class SlopeBoundary
    {
        private SlopeCanvas canvas;
        private bool isSelected, showMesh = false;
        private List<DrawingPoint> boundaryPoints;
        private List<MeshLine> meshLines;
        private List<Point> upperSurface;
        private SoilMovement soilDir = SoilMovement.None;

        public SlopeBoundary(SlopeCanvas canvas)
        {
            this.canvas = canvas;

            meshLines = new List<MeshLine>();
            upperSurface = new List<Point>();

            Boundary = new Polygon();
            boundaryPoints = new List<DrawingPoint>();
            Boundary.Stroke = Brushes.Black;
            Boundary.StrokeThickness = 1.5;
            Boundary.Fill = Brushes.White;
            Boundary.Opacity = 0.6;
            Boundary.Visibility = Visibility.Visible;

            canvas.Children.Add(Boundary);
        }

        public SlopeBoundary(SlopeCanvas canvas, Point[] pts)
        {
            this.canvas = canvas;

            meshLines = new List<MeshLine>();
            upperSurface = new List<Point>();

            Boundary = new Polygon();
            boundaryPoints = new List<DrawingPoint>();
            Boundary.Stroke = Brushes.Black;
            Boundary.StrokeThickness = 1.5;
            Boundary.Fill = Brushes.White;
            Boundary.Opacity = 0.6;
            Boundary.MouseLeftButtonDown += new MouseButtonEventHandler(MouseLeftButtonDown);
            Boundary.Visibility = Visibility.Visible;

            canvas.Children.Add(Boundary);

            for (int i = 0; i < pts.Length - 1; i++)
            {
                Boundary.Points.Add(pts[i]);
                boundaryPoints.Add(new DrawingPoint(canvas, this, pts[i]));
            }

            SortPoints();
        }

        public Polygon Boundary { get; set; }
        public List<DrawingPoint> BoundaryPoints { get { return this.boundaryPoints; } }

        public bool IsSelected
        {
            get
            {
                return this.isSelected;
            }

            set
            {
                this.isSelected = value;

                Boundary.Stroke = value ? Brushes.Red : Brushes.Black;
            }
        }

        public SoilMovement SoilDirection { get { return this.soilDir; } }

        public bool IsMouseOver { get { return Boundary.IsMouseOver; } }

        public List<MeshLine> Mesh { get { return this.meshLines; } }

        public bool ShowMesh
        {
            get
            {
                return this.showMesh;
            }
            set
            {
                if (canvas.IsMeshed)
                {
                    this.showMesh = value;
                    foreach (MeshLine ml in meshLines) ml.IsVisible = value;
                }
                else
                {
                    this.showMesh = false;
                }
            }
        }

        public double XMin
        {
            get
            {
                if (boundaryPoints.Count == 0) return 0;

                double min = boundaryPoints[0].Point.X;
                for (int i = 1; i < boundaryPoints.Count; i++)
                {
                    min = Math.Min(min, boundaryPoints[i].Point.X);
                }
                return min;
            }
        }

        public double XMax
        {
            get
            {
                if (boundaryPoints.Count == 0) return 0;

                double max = boundaryPoints[0].Point.X;
                for (int i = 1; i < boundaryPoints.Count; i++)
                {
                    max = Math.Max(max, boundaryPoints[i].Point.X);
                }
                return max;
            }
        }

        public double YMin
        {
            get
            {
                if (boundaryPoints.Count == 0) return 0;

                double min = boundaryPoints[0].Point.Y;
                for (int i = 1; i < boundaryPoints.Count; i++)
                {
                    min = Math.Max(min, boundaryPoints[i].Point.Y);
                }
                return min;
            }
        }

        public double Area
        {
            get
            {
                if (boundaryPoints.Count == 0) return 0;

                double x1, y1, x2, y2;

                x1 = boundaryPoints[boundaryPoints.Count - 1].Point.X;
                y1 = boundaryPoints[boundaryPoints.Count - 1].Point.Y;
                x2 = boundaryPoints[0].Point.X;
                y2 = boundaryPoints[0].Point.Y;

                double sum = x1 * y2 - x2 * y1;

                for (int i = 0; i < boundaryPoints.Count - 1; i++)
                {
                    x1 = boundaryPoints[i].Point.X;
                    y1 = boundaryPoints[i].Point.Y;
                    x2 = boundaryPoints[i + 1].Point.X;
                    y2 = boundaryPoints[i + 1].Point.Y;

                    sum += x1 * y2 - x2 * y1;
                }

                return -0.5 * sum;  // negative since y-axis is inverted for graphics
            }
        }

        public void SortPoints()
        {
            if (this.Area < 0)
            {
                this.BoundaryPoints.Reverse();
                this.Boundary.Points.Reverse();
            }
        }

        public bool SaveMesh()
        {
            ClosableCanvasTabItem parentTab = (ClosableCanvasTabItem)((Grid)canvas.Parent).Parent;

            string[] split = ((string)parentTab.Tag).Split('.');

            if (split[split.Length - 1] != "slp") return false;

            split[split.Length - 1] = "msh";

            string path = string.Join(".", split);

            using (TextWriter tw = new StreamWriter(path))
            {
                double factor;
                double xOffset = canvas.OriginOffsetX, yOffset = canvas.OriginOffsetY;
                double canvasHeight = canvas.ActualHeight;
                string units;
                switch (canvas.Units)
                {
                    case Units.Metres: units = "m"; factor = 0.0254; break;
                    case Units.Millimetres: units = "mm"; factor = 25.4; break;
                    case Units.Feet: units = "ft"; factor = 1.0 / 12.0; break;
                    default: units = "in"; factor = 1; break;
                }

                double scaleX = factor * canvas.Scale / canvas.DpiX;
                double scaleY = factor * canvas.Scale / canvas.DpiY;

                tw.WriteLine(String.Format("Units = {0}", units));

                tw.WriteLine();

                if (upperSurface.Count == 0) CheckYMax(XMin, XMax, YMin);

                tw.WriteLine(String.Format("Number of upper surface points = {0}", upperSurface.Count));

                tw.WriteLine();

                tw.WriteLine("Upper Surface Coordinates");

                double x = 0, y = 0;
                foreach (Point p in upperSurface)
                {
                    x = (p.X - xOffset) * scaleX;
                    y = (canvasHeight - p.Y - yOffset) * scaleY;
                    tw.WriteLine(String.Format("{0}, {1}", x, y));
                }

                tw.WriteLine();

                tw.WriteLine(String.Format("Number of mesh lines = {0}", meshLines.Count));

                tw.WriteLine();

                if (meshLines.Count > 0)
                {
                    tw.WriteLine("ML1");
                    tw.WriteLine(String.Format("Number of points = {0}", meshLines[0].MeshPoints.Count));

                    x = (XMin - xOffset) * scaleX;

                    foreach (MeshPoint mp in meshLines[0].MeshPoints)
                    {
                        y = (canvasHeight - mp.Y - yOffset) * scaleY;
                        tw.WriteLine(String.Format("{0}, {1}, {2}, \"{3}\"", x, y, mp.Type, mp.Material));
                    }

                    tw.WriteLine();
                }

                for (int i = 1; i < meshLines.Count - 1; i++)
                {
                    tw.WriteLine(String.Format("ML{0}", i + 1));
                    tw.WriteLine(String.Format("Number of points = {0}", meshLines[i].MeshPoints.Count));

                    x = (meshLines[i].Line.X1 - xOffset) * scaleX;

                    foreach (MeshPoint mp in meshLines[i].MeshPoints)
                    {
                        y = (canvasHeight - mp.Y - yOffset) * scaleY;
                        tw.WriteLine(String.Format("{0}, {1}, {2}, \"{3}\"", x, y, mp.Type, mp.Material));
                    }

                    tw.WriteLine();
                }

                if (meshLines.Count > 1)
                {
                    tw.WriteLine(String.Format("ML{0}", meshLines.Count));
                    tw.WriteLine(String.Format("Number of points = {0}", meshLines[meshLines.Count - 1].MeshPoints.Count));

                    x = (XMax - xOffset) * scaleX;

                    foreach (MeshPoint mp in meshLines[meshLines.Count - 1].MeshPoints)
                    {
                        y = (canvasHeight - mp.Y - yOffset) * scaleY;
                        tw.WriteLine(String.Format("{0}, {1}, {2}, \"{3}\"", x, y, mp.Type, mp.Material));
                    }

                    tw.WriteLine();
                }
            }

            return true;
        }

        public double YMaxOfX(double x, double yMin)
        {
            double toler = 1e-5;
            double y = yMin;

            for (int i = 0; i < boundaryPoints.Count; i++)
            {
                if (Math.Abs(x - boundaryPoints[i].Point.X) < toler)
                {
                    y = Math.Min(y, boundaryPoints[i].Point.Y);
                }
            }

            return y;
        }

        public bool CheckXMaxMin(double value)
        {
            int count = 0;
            bool counting = false, done1 = false, wasZero = false, done2 = false;
            double toler = 1e-5;
            for (int i = 0; i < boundaryPoints.Count; i++)
            {
                if (count == 0 && Math.Abs(boundaryPoints[i].Point.X - value) < toler)
                {
                    counting = true;
                    count++;
                    if (i == 0) wasZero = true;
                }
                else if (counting)
                {
                    if (Math.Abs(boundaryPoints[i].Point.X - value) < toler) count++;
                    else
                    {
                        counting = false;
                        if (!done1) done1 = true;
                        else done2 = true;
                    }
                }
                else if (done1 && Math.Abs(boundaryPoints[i].Point.X - value) < toler)
                {
                    if (wasZero)
                    {
                        count++;
                        counting = true;
                    }
                    else return false;
                }
                else if (done2)
                {
                    return false;
                }
            }

            return count >= 2;
        }

        public bool CheckYMin(double value)
        {
            int count = 0;
            bool counting = false, done1 = false, wasZero = false, done2 = false;
            double toler = 1e-5;
            for (int i = 0; i < boundaryPoints.Count; i++)
            {
                if (count == 0 && Math.Abs(boundaryPoints[i].Point.Y - value) < toler)
                {
                    counting = true;
                    count++;
                    if (i == 0) wasZero = true;
                }
                else if (counting)
                {
                    if (Math.Abs(boundaryPoints[i].Point.Y - value) < toler) count++;
                    else
                    {
                        counting = false;
                        if (!done1) done1 = true;
                        else done2 = true;
                    }
                }
                else if (done1 && Math.Abs(boundaryPoints[i].Point.Y - value) < toler)
                {
                    if (wasZero)
                    {
                        count++;
                        counting = true;
                    }
                    else return false;
                }
                else if (done2)
                {
                    return false;
                }
            }

            return count >= 2;
        }

        public int CheckYMax(double xMin, double xMax, double yMin)
        {
            upperSurface.Clear();

            int countDir = 1;
            double x = xMin, y = yMin;
            double toler = 1e-5;
            int icurr = 0, inext = 0, maxIndex = boundaryPoints.Count - 1;

            soilDir = SoilMovement.None;

            for (int i = 0; i < boundaryPoints.Count; i++)
            {
                if (Math.Abs(x - boundaryPoints[i].Point.X) < toler)
                {
                    if (boundaryPoints[i].Point.Y <= y)
                    {
                        icurr = i;
                        y = boundaryPoints[i].Point.Y;
                    }
                }
            }

            upperSurface.Add(new Point(x, y));

            while ((xMax - x) > toler)
            {
                if (icurr == maxIndex && countDir == 1) inext = 0;
                else if (icurr == 0 && countDir == -1) inext = maxIndex;
                else inext = icurr + countDir;

                if (x == xMin)
                {
                    if (Math.Abs(x - boundaryPoints[inext].Point.X) < toler)
                    {
                        countDir = -1;
                        if (icurr == 0) inext = maxIndex;
                        else inext = icurr + countDir;

                        // minimum X boundary does not have a non-vertical exit line
                        if (Math.Abs(x - boundaryPoints[inext].Point.X) < toler) return -2;
                    }
                }

                if ((x - boundaryPoints[inext].Point.X) > toler) return -3; // upper surface direction inconsistent

                x = boundaryPoints[inext].Point.X;

                if ((y - boundaryPoints[inext].Point.Y) < -toler)
                {
                    if (soilDir == SoilMovement.None) soilDir = SoilMovement.LtoR;
                    else if (soilDir == SoilMovement.RtoL) return -4;  // soil direction inconsistent
                }
                else if ((y - boundaryPoints[inext].Point.Y) > toler)
                {
                    if (soilDir == SoilMovement.None) soilDir = SoilMovement.RtoL;
                    else if (soilDir == SoilMovement.LtoR) return -4;   // soil direction inconsistent
                }

                y = boundaryPoints[inext].Point.Y;

                upperSurface.Add(new Point(x, y));

                icurr = inext;
            }

            return soilDir == SoilMovement.LtoR ? 1 : (soilDir == SoilMovement.RtoL ? -1 : 0);
        }

        public void GenerateMesh(double xMin, double xMax)
        {
            meshLines = new List<MeshLine>();

            double toler = 1e-5;
            meshLines.Add(new MeshLine(canvas, xMin + toler));

            double factor;
            switch (canvas.Units)
            {
                case Units.Metres: factor = 0.0254; break;
                case Units.Millimetres: factor = 25.4; break;
                case Units.Feet: factor = 1.0 / 12.0; break;
                default: factor = 1; break;
            }

            double sliceWidth = canvas.GeneticAlgorithmParameters.SliceWidth / (factor * canvas.Scale) * canvas.DpiX;

            double x = xMin + sliceWidth;

            while (x < xMax)
            {
                meshLines.Add(new MeshLine(canvas, x));

                x += sliceWidth;
            }

            if (xMax - x > toler) meshLines.Add(new MeshLine(canvas, xMax - toler));

            canvas.IsMeshed = true;
            canvas.ShowMesh = true;
            canvas.IsSaved = false;
        }

        public void ClearMesh()
        {
            foreach (MeshLine ml in meshLines) ml.Delete();
            meshLines.Clear();
        }

        public void Delete()
        {
            Boundary.Points.Clear();
            for (int i = 0; i < boundaryPoints.Count; i++)
            {
                canvas.Children.Remove(boundaryPoints[i].Dot);
            }
            boundaryPoints.Clear();

            while (canvas.MaterialBlocks.Count > 0)
            {
                canvas.MaterialBlocks[0].Delete();
            }
        }

        public void AddPoint(DrawingPoint p1, DrawingPoint p2)
        {
            int index1 = 0;
            for (int i = 0; i < BoundaryPoints.Count; i++)
            {
                if (BoundaryPoints[i] == p1)
                {
                    index1 = i;
                    break;
                }
            }

            int index2 = 0;
            for (int i = 0; i < BoundaryPoints.Count; i++)
            {
                if (BoundaryPoints[i] == p2)
                {
                    index2 = i;
                    break;
                }
            }

            int maxIndex = Math.Max(index1, index2);
            int minIndex = Math.Min(index1, index2);

            if ((maxIndex - minIndex) == 1)
            {
                Point newPoint = new Point(0.5 * (p1.Point.X + p2.Point.X), 0.5 * (p1.Point.Y + p2.Point.Y));
                BoundaryPoints.Insert(maxIndex, new DrawingPoint(canvas, this, newPoint));
                Boundary.Points.Insert(maxIndex, newPoint);
                canvas.IsSaved = false;
                canvas.IsVerified = false;
            }
            else if (minIndex == 0 && maxIndex == BoundaryPoints.Count - 1)
            {
                Point newPoint = new Point(0.5 * (p1.Point.X + p2.Point.X), 0.5 * (p1.Point.Y + p2.Point.Y));
                BoundaryPoints.Add(new DrawingPoint(canvas, this, newPoint));
                Boundary.Points.Add(newPoint);
                canvas.IsSaved = false;
                canvas.IsVerified = false;
            }
            else
            {
                MessageBox.Show("Points must be different and adjacent.", "Error");
            }
        }

        public void Translate(Vector delta)
        {
            foreach (MeshLine ml in meshLines) ml.Translate(delta);

            Point p;
            for (int i = 0; i < Boundary.Points.Count; i++)
            {
                p = Boundary.Points[i];
                p.X += delta.X;
                p.Y += delta.Y;
                Boundary.Points[i] = p;
                boundaryPoints[i].Translate(delta);
            }
        }

        public void Zoom(double factor, Point centre)
        {
            foreach (MeshLine ml in meshLines) ml.Zoom(factor, centre);

            Point p;
            for (int i = 0; i < Boundary.Points.Count; i++)
            {
                p = Boundary.Points[i];
                p.X = centre.X + factor * (p.X - centre.X);
                p.Y = centre.Y + factor * (p.Y - centre.Y);
                Boundary.Points[i] = p;
                boundaryPoints[i].Zoom(factor, centre);
            }
        }

        private void MouseLeftButtonDown(object sender, MouseEventArgs e)
        {
            // Select boundary object
            if (canvas.DrawMode == DrawModes.Select)
            {
                this.IsSelected = true;
            }
        }

        public int CheckIntersecting()
        {
            double  x1, y1, x2, y2, x3, y3, x4, y4,
                    m1, m2, b1, b2,
                    x, y;
            bool    vert1, vert2;
            double  toler = 1e-5;
            int     count = 0;

            for (int i = 0; i < Boundary.Points.Count - 1; i++)
            {
                x1 = Boundary.Points[i].X;
                y1 = Boundary.Points[i].Y;

                x2 = Boundary.Points[i + 1].X;
                y2 = Boundary.Points[i + 1].Y;

                vert1 = Math.Abs(x2 - x1) < toler;

                if (vert1)
                {
                    x = x1;
                    m1 = 0;
                    b1 = 0;
                }
                else
                {
                    m1 = (y2 - y1) / (x2 - x1);
                    b1 = y1 - m1 * x1;
                    x = 0;
                }

                for (int j = i + 1; j < Boundary.Points.Count; j++)
                {
                    x3 = Boundary.Points[j].X;
                    y3 = Boundary.Points[j].Y;

                    if (j == Boundary.Points.Count - 1)
                    {
                        x4 = Boundary.Points[0].X;
                        y4 = Boundary.Points[0].Y;
                    }
                    else
                    {
                        x4 = Boundary.Points[j + 1].X;
                        y4 = Boundary.Points[j + 1].Y;
                    }

                    vert2 = Math.Abs(x4 - x3) < toler;

                    if (vert2)
                    {
                        x = x3;
                        m2 = 0;
                        b2 = 0;
                    }
                    else
                    {
                        m2 = (y4 - y3) / (x4 - x3);
                        b2 = y3 - m2 * x3;
                        x = 0;
                    }

                    if (vert1 || vert2)
                    {
                        if (!vert1)
                        {
                            y = m1 * x + b1;

                            if ((x - Math.Min(x1, x2)) < toler && (Math.Max(x1, x2) - x) < toler
                                && (y - Math.Min(y3, y4)) < toler && (Math.Max(y3, y4) - y) < toler)
                            {
                                count++;
                            }
                        }
                        else if (!vert2)
                        {
                            y = m2 * x + b2;

                            if ((x - Math.Min(x3, x4)) < toler && (Math.Max(x3, x4) - x) < toler
                                && (y - Math.Min(y1, y2)) < toler && (Math.Max(y1, y2) - y) < toler)
                            {
                                count++;
                            }
                        }
                        else
                        {
                            if (Math.Abs(x3 - x1) < toler && ((y3 < y2 && y4 > y1) || (y3 > y2 && y4 < y1))) count++;
                        }
                    }
                    else
                    {
                        if (Math.Abs(m2 - m1) < toler)
                        {
                            if (Math.Abs(b2 - b1) < toler
                                && !((x1 == x4 && y1 == y4) || (x2 == x3 && y2 == y3)))
                            {
                                count++;
                            }
                        }
                        else
                        {
                            x = (b2 - b1) / (m1 - m2);

                            if ((x - Math.Min(x1, x2)) > toler && (Math.Max(x1, x2) - x) > toler
                                && (x - Math.Min(x3, x4)) > toler && (Math.Max(x3, x4) - x) > toler)
                            {
                                count++;
                            }
                        }
                    }
                }
            }

            return count;
        }
    }

    class DrawingPoint
    {
        private SlopeCanvas canvas;
        private object parent;
        private Point point;
        private Ellipse dot;
        private bool isSelected;
        private bool isFixedX;
        private bool isFixedY;
        private List<Polyline> fixLines;

        public DrawingPoint(SlopeCanvas canvas, object parent, Point pt)
        {
            this.canvas = canvas;
            this.parent = parent;
            this.point = pt;

            fixLines = new List<Polyline>();
            Polyline newLine;
            for (int i = 0; i < 4; i++)
            {
                newLine = new Polyline();
                newLine.Visibility = Visibility.Hidden;
                newLine.Fill = Brushes.Blue;
                newLine.Opacity = 1.0;
                newLine.StrokeThickness = 1.5;
                newLine.Stroke = Brushes.Blue;
                fixLines.Add(newLine);
                canvas.Children.Add(newLine);
            }

            fixLines[0].Points.Add(new Point(pt.X - 7, pt.Y - 3.5));
            fixLines[0].Points.Add(new Point(pt.X + 7, pt.Y - 3.5));

            fixLines[1].Points.Add(new Point(pt.X - 7, pt.Y + 3.5));
            fixLines[1].Points.Add(new Point(pt.X + 7, pt.Y + 3.5));

            fixLines[2].Points.Add(new Point(pt.X - 3.5, pt.Y + 7));
            fixLines[2].Points.Add(new Point(pt.X - 3.5, pt.Y - 7));

            fixLines[3].Points.Add(new Point(pt.X + 3.5, pt.Y + 7));
            fixLines[3].Points.Add(new Point(pt.X + 3.5, pt.Y - 7));


            dot = new Ellipse();
            dot.HorizontalAlignment = HorizontalAlignment.Left;
            dot.VerticalAlignment = VerticalAlignment.Top;
            dot.Margin = new Thickness(pt.X - 3.5, pt.Y - 3.5, 0, 0);
            dot.Height = 7;
            dot.Width = 7;
            dot.Stroke = Brushes.Black;
            dot.Fill = Brushes.Black;
            dot.Opacity = 0.7;
            dot.MouseLeftButtonDown += new MouseButtonEventHandler(MouseLeftButtonDown);

            canvas.Children.Add(dot);
        }

        public bool IsSelected
        {
            get
            {
                return this.isSelected;
            }
            set
            {
                this.isSelected = value;

                if (value)
                {
                    dot.Fill = Brushes.Red;
                    dot.Stroke = Brushes.Red;
                }
                else
                {
                    dot.Fill = Brushes.Black;
                    dot.Stroke = Brushes.Black;
                }
            }
        }

        public bool IsFixedX
        {
            get
            {
                return this.isFixedX;
            }
            set
            {
                this.isFixedX = value;
                fixLines[2].Visibility = fixLines[3].Visibility = value ? Visibility.Visible : Visibility.Hidden;
            }
        }

        public bool IsFixedY
        {
            get
            {
                return this.isFixedY;
            }
            set
            {
                this.isFixedY = value;
                fixLines[0].Visibility = fixLines[1].Visibility = value ? Visibility.Visible : Visibility.Hidden;
            }
        }

        public Point Point { get { return this.point; } }
        public Ellipse Dot { get { return this.dot; } }
        public bool IsMouseOver { get { return dot.IsMouseOver; } }
        public object Parent { get { return parent; } }
        public List<Polyline> FixLines { get { return fixLines; } }

        public void Delete()
        {
            SlopeBoundary boundary = parent as SlopeBoundary;
            MaterialBlock material = parent as MaterialBlock;

            if (boundary != null)
            {
                for (int i = 0; i < boundary.Boundary.Points.Count; i++)
                {
                    if (boundary.Boundary.Points[i] == this.Point) boundary.Boundary.Points.RemoveAt(i);
                }

                canvas.Children.Remove(this.Dot);
                boundary.BoundaryPoints.Remove(this);
                if (boundary.BoundaryPoints.Count == 0) boundary.Delete();
            }

            if (material != null)
            {
                for (int i = 0; i < material.Boundary.Points.Count; i++)
                {
                    if (material.Boundary.Points[i] == this.Point) material.Boundary.Points.RemoveAt(i);
                }

                canvas.Children.Remove(this.Dot);
                material.BoundaryPoints.Remove(this);

                if (material.BoundaryPoints.Count == 0) material.Delete();
            }
        }

        public void Translate(Vector delta)
        {
            point += delta;
            dot.Margin = new Thickness(point.X - 3.5, point.Y - 3.5, 0, 0);
            Point p;
            foreach (Polyline l in fixLines)
            {
                p = l.Points[0];
                p += delta;
                l.Points[0] = p;

                p = l.Points[1];
                p += delta;
                l.Points[1] = p;
            }
        }

        public void Zoom(double factor, Point centre)
        {
            point.X = centre.X + factor * (point.X - centre.X);
            point.Y = centre.Y + factor * (point.Y - centre.Y);
            dot.Margin = new Thickness(point.X - 3.5, point.Y - 3.5, 0, 0);

            fixLines[0].Points[0] = new Point(point.X - 7, point.Y - 3.5);
            fixLines[0].Points[1] = new Point(point.X + 7, point.Y - 3.5);

            fixLines[1].Points[0] = new Point(point.X - 7, point.Y + 3.5);
            fixLines[1].Points[1] = new Point(point.X + 7, point.Y + 3.5);

            fixLines[2].Points[0] = new Point(point.X - 3.5, point.Y + 7);
            fixLines[2].Points[1] = new Point(point.X - 3.5, point.Y - 7);

            fixLines[3].Points[0] = new Point(point.X + 3.5, point.Y + 7);
            fixLines[3].Points[1] = new Point(point.X + 3.5, point.Y - 7);
        }

        /// <summary>
        /// Move point independent of other points.
        /// </summary>
        /// <param name="delta">Move vector.</param>
        public void Move(Vector delta)
        {
            // attempt to cast parent object
            SlopeBoundary boundary = parent as SlopeBoundary;
            MaterialBlock material = parent as MaterialBlock;

            // initialize index of point
            int boundPointIndex = -1;

            // obtain the index from the appropriate parent object
            if (boundary != null) boundPointIndex = boundary.Boundary.Points.IndexOf(point);
            else if (material != null) boundPointIndex = material.Boundary.Points.IndexOf(point);

            // shift the point, its display circle, and its fixity lines
            point += delta;
            dot.Margin = new Thickness(point.X - 3.5, point.Y - 3.5, 0, 0);
            Point p;
            foreach (Polyline l in fixLines)
            {
                p = l.Points[0];
                p += delta;
                l.Points[0] = p;

                p = l.Points[1];
                p += delta;
                l.Points[1] = p;
            }

            // update associated polygons and line constraints
            if (boundary != null) boundary.Boundary.Points[boundPointIndex] = point;
            else if (material != null)
            {
                material.Boundary.Points[boundPointIndex] = point;

                foreach (LineConstraint lc in material.LineConstraints)
                {
                    if (lc.Nodes.Contains(this)) lc.UpdateLocation();
                }
            }
        }

        /// <summary>
        /// Override for left-click selection
        /// </summary>
        /// <param name="sender">Reference to sending object.</param>
        /// <param name="e">Mouse event arguments.</param>
        private void MouseLeftButtonDown(object sender, MouseEventArgs e)
        {
            if (canvas.DrawMode == DrawModes.Select
                || canvas.DrawMode == DrawModes.AddPoints
                || canvas.DrawMode == DrawModes.MovePoints
                || canvas.DrawMode == DrawModes.FixX
                || canvas.DrawMode == DrawModes.FixY)
            {
                this.IsSelected = true;
            }
        }
    }

    class LineConstraint
    {
        private SlopeCanvas canvas;
        private bool isFixedX, isFixedY;
        private List<Polyline> fixLines;

        public LineConstraint(SlopeCanvas canvas,
                                DrawingPoint p1, DrawingPoint p2, 
                                bool fixX, bool fixY)
        {
            // set parent drawing canvas
            this.canvas = canvas;

            // create list of boundary nodes for the constraint
            Nodes = new List<DrawingPoint>() { p1, p2 };

            // set constraints on boundary nodes
            Nodes[0].IsFixedX = fixX || Nodes[0].IsFixedX;
            Nodes[0].IsFixedY = fixY || Nodes[0].IsFixedY;
            Nodes[1].IsFixedX = fixX || Nodes[1].IsFixedX;
            Nodes[1].IsFixedY = fixY || Nodes[1].IsFixedY;

            // compute the point at which to plot the constraint
            MidPoint = new Point(0.5 * (p1.Point.X + p2.Point.X), 0.5 * (p1.Point.Y + p2.Point.Y));

            // create plotting lines for constraints
            fixLines = new List<Polyline>();
            Polyline newLine;
            for (int i = 0; i < 4; i++)
            {
                newLine = new Polyline();
                newLine.Visibility = Visibility.Hidden;
                newLine.Fill = Brushes.Blue;
                newLine.Opacity = 1.0;
                newLine.StrokeThickness = 1.5;
                newLine.Stroke = Brushes.Blue;
                fixLines.Add(newLine);
                canvas.Children.Add(newLine);
            }

            fixLines[0].Points.Add(new Point(MidPoint.X - 7, MidPoint.Y - 3.5));
            fixLines[0].Points.Add(new Point(MidPoint.X + 7, MidPoint.Y - 3.5));

            fixLines[1].Points.Add(new Point(MidPoint.X - 7, MidPoint.Y + 3.5));
            fixLines[1].Points.Add(new Point(MidPoint.X + 7, MidPoint.Y + 3.5));

            fixLines[2].Points.Add(new Point(MidPoint.X - 3.5, MidPoint.Y + 7));
            fixLines[2].Points.Add(new Point(MidPoint.X - 3.5, MidPoint.Y - 7));

            fixLines[3].Points.Add(new Point(MidPoint.X + 3.5, MidPoint.Y + 7));
            fixLines[3].Points.Add(new Point(MidPoint.X + 3.5, MidPoint.Y - 7));

            // set visibility of constraints
            this.IsFixedX = fixX;
            this.IsFixedY = fixY;
        }

        public List<DrawingPoint> Nodes { get; set; }

        public Point MidPoint { get; set; }

        public bool IsFixedX
        {
            get
            {
                return this.isFixedX;
            }
            set
            {
                this.isFixedX = value;

                fixLines[2].Visibility = fixLines[3].Visibility = value ? Visibility.Visible : Visibility.Hidden;

                if (value)
                {
                    Nodes[0].IsFixedX = value;
                    Nodes[1].IsFixedX = value;
                }
            }
        }

        public bool IsFixedY
        {
            get
            {
                return this.isFixedY;
            }
            set
            {
                this.isFixedY = value;

                fixLines[0].Visibility = fixLines[1].Visibility = value ? Visibility.Visible : Visibility.Hidden;

                if (value)
                {
                    Nodes[0].IsFixedY = value;
                    Nodes[1].IsFixedY = value;
                }
            }
        }

        public void UpdateLocation()
        {
            DrawingPoint p1 = Nodes[0], p2 = Nodes[1];

            // compute the point at which to plot the constraint
            MidPoint = new Point(0.5 * (p1.Point.X + p2.Point.X), 0.5 * (p1.Point.Y + p2.Point.Y));

            fixLines[0].Points[0] = new Point(MidPoint.X - 7, MidPoint.Y - 3.5);
            fixLines[0].Points[1] = new Point(MidPoint.X + 7, MidPoint.Y - 3.5);

            fixLines[1].Points[0] = new Point(MidPoint.X - 7, MidPoint.Y + 3.5);
            fixLines[1].Points[1] = new Point(MidPoint.X + 7, MidPoint.Y + 3.5);

            fixLines[2].Points[0] = new Point(MidPoint.X - 3.5, MidPoint.Y + 7);
            fixLines[2].Points[1] = new Point(MidPoint.X - 3.5, MidPoint.Y - 7);

            fixLines[3].Points[0] = new Point(MidPoint.X + 3.5, MidPoint.Y + 7);
            fixLines[3].Points[1] = new Point(MidPoint.X + 3.5, MidPoint.Y - 7);
        }
    }

    class ZoomRect
    {
        public ZoomRect()
        {
            Boundary = new Polygon();
            Boundary.Stroke = Brushes.Black;
            Boundary.Fill = Brushes.LightBlue;
            Boundary.Opacity = 0.3;
            Boundary.Visibility = Visibility.Visible;
        }

        public Polygon Boundary { get; set; }
    }

    class MaterialBlock
    {
        private SlopeCanvas canvas;
        private bool isSelected;
        private MaterialType material;
        private List<DrawingPoint> boundaryPoints;
        private List<LineConstraint> lineConstraints;

        public MaterialBlock(SlopeCanvas canvas, Point[] pts)
        {
            this.canvas = canvas;

            Boundary = new Polygon();
            boundaryPoints = new List<DrawingPoint>();
            Boundary.Stroke = Brushes.Black;
            Boundary.StrokeThickness = 1.1;
            Boundary.Fill = Brushes.WhiteSmoke;
            Boundary.Opacity = 0.8;
            Boundary.MouseLeftButtonDown += new MouseButtonEventHandler(this.MouseLeftButtonDown);
            Boundary.Visibility = Visibility.Visible;

            canvas.Children.Add(Boundary);

            for (int i = 0; i < pts.Length - 1; i++)
            {
                Boundary.Points.Add(pts[i]);
                boundaryPoints.Add(new DrawingPoint(canvas, this, pts[i]));
            }

            Material = new MaterialType();

            lineConstraints = new List<LineConstraint>();

            SortPoints();
        }

        public bool IsSelected
        {
            get
            {
                return this.isSelected;
            }
            set
            {
                this.isSelected = value;

                Boundary.Stroke = value ? Brushes.Red : Brushes.Black;
            }
        }

        public bool IsMouseOver { get { return Boundary.IsMouseOver; } }

        public Polygon Boundary { get; set; }
        public List<DrawingPoint> BoundaryPoints { get { return this.boundaryPoints; } }
        public List<LineConstraint> LineConstraints { get { return this.lineConstraints; } }

        public MaterialType Material
        {
            get
            {
                return this.material;
            }
            set
            {
                this.material = value;

                this.Boundary.Fill = value.Fill != null ? value.Fill : Brushes.WhiteSmoke;
            }
        }

        public double Area
        {
            get
            {
                if (boundaryPoints.Count == 0) return 0;

                double x1, y1, x2, y2;

                x1 = boundaryPoints[boundaryPoints.Count - 1].Point.X;
                y1 = boundaryPoints[boundaryPoints.Count - 1].Point.Y;
                x2 = boundaryPoints[0].Point.X;
                y2 = boundaryPoints[0].Point.Y;

                double sum = x1 * y2 - x2 * y1;

                for (int i = 0; i < boundaryPoints.Count - 1; i++)
                {
                    x1 = boundaryPoints[i].Point.X;
                    y1 = boundaryPoints[i].Point.Y;
                    x2 = boundaryPoints[i + 1].Point.X;
                    y2 = boundaryPoints[i + 1].Point.Y;

                    sum += x1 * y2 - x2 * y1;
                }

                return -0.5 * sum;  // negative since y-axis is inverted for graphics
            }
        }

        public void SortPoints()
        {
            if (this.Area < 0)
            {
                this.Boundary.Points.Reverse();
                this.BoundaryPoints.Reverse();
            }
        }

        public void Delete()
        {
            canvas.Children.Remove(this.Boundary);
            for (int i = 0; i < boundaryPoints.Count; i++)
            {
                canvas.Children.Remove(boundaryPoints[i].Dot);
            }
            Boundary.Points.Clear();
            canvas.MaterialBlocks.Remove(this);
        }

        public void AddPoint(DrawingPoint p1, DrawingPoint p2)
        {
            int index1 = 0;
            for (int i = 0; i < BoundaryPoints.Count; i++)
            {
                if (BoundaryPoints[i] == p1)
                {
                    index1 = i;
                    break;
                }
            }

            int index2 = 0;
            for (int i = 0; i < BoundaryPoints.Count; i++)
            {
                if (BoundaryPoints[i] == p2)
                {
                    index2 = i;
                    break;
                }
            }

            int maxIndex = Math.Max(index1, index2);
            int minIndex = Math.Min(index1, index2);

            if ((maxIndex - minIndex) == 1)
            {
                Point newPoint = new Point(0.5 * (p1.Point.X + p2.Point.X), 0.5 * (p1.Point.Y + p2.Point.Y));
                BoundaryPoints.Insert(maxIndex, new DrawingPoint(canvas, this, newPoint));
                Boundary.Points.Insert(maxIndex, newPoint);
                canvas.IsSaved = false;
                canvas.IsVerified = false;
            }
            else if (minIndex == 0 && maxIndex == BoundaryPoints.Count - 1)
            {
                Point newPoint = new Point(0.5 * (p1.Point.X + p2.Point.X), 0.5 * (p1.Point.Y + p2.Point.Y));
                BoundaryPoints.Add(new DrawingPoint(canvas, this, newPoint));
                Boundary.Points.Add(newPoint);
                canvas.IsSaved = false;
                canvas.IsVerified = false;
            }
            else
            {
                MessageBox.Show("Points must be different and adjacent.", "Error");
            }
        }

        public void FixX(DrawingPoint p1, DrawingPoint p2)
        {
            // find point indices in list
            int index1 = BoundaryPoints.FindIndex(delegate(DrawingPoint p) { return p == p1; });
            int index2 = BoundaryPoints.FindIndex(delegate(DrawingPoint p) { return p == p2; });

            // if points were not successfully found
            if (index1 == -1 || index2 == -1)
            {
                MessageBox.Show("Points not found on block.", "Fix X error");
                return;
            }

            // ensure max and min as appropriate
            if (((index1 > index2) && !(index2 == 0 && index1 == BoundaryPoints.Count - 1))
                || (index1 == 0 && index2 == BoundaryPoints.Count - 1))
            {
                int tmp = index1;
                index1 = index2;
                index2 = tmp;

                DrawingPoint tmpPt = p1;
                p1 = p2;
                p2 = tmpPt;
            }

            // if points are the same, fix/unfix the point ...
            if (index1 == index2)
            {
                bool fix = !p1.IsFixedX;
                p1.IsFixedX = fix;

                if (!fix)
                {
                    foreach (LineConstraint lc in LineConstraints)
                    {
                        if (lc.Nodes.Contains(p1)) lc.IsFixedX = fix;
                    }
                }

                canvas.IsSaved = false;
                canvas.IsVerified = false;
            }

            // ... or if the points are adjacent, create a line constraint ...
            else if (((index2 - index1) == 1) || (index2 == 0 && index1 == BoundaryPoints.Count - 1))
            {
                LineConstraint existingLC = LineConstraints.Find(delegate(LineConstraint lc) { return lc.Nodes.Contains(p1) && lc.Nodes.Contains(p2); });

                if (existingLC != null)
                {
                    existingLC.IsFixedX = !existingLC.IsFixedX;
                }
                else
                {
                    LineConstraints.Add(new LineConstraint(canvas, p1, p2, true, false));
                }

                canvas.IsSaved = false;
                canvas.IsVerified = false;
            }

            // ... otherwise, indicate that a constraint cannot be applied in this manner
            else
            {
                MessageBox.Show("Points must be either the same or directly adjacent.", "Error");
            }

            // remove any line constraints that are both unfixed
            for (int i = LineConstraints.Count - 1; i >= 0; i--)
            {
                if (!(LineConstraints[i].IsFixedX) && !(LineConstraints[i].IsFixedY))
                    LineConstraints.RemoveAt(i);
            }
        }

        public void FixY(DrawingPoint p1, DrawingPoint p2)
        {
            // find point indices in list
            int index1 = BoundaryPoints.FindIndex(delegate(DrawingPoint p) { return p == p1; });
            int index2 = BoundaryPoints.FindIndex(delegate(DrawingPoint p) { return p == p2; });

            // if points were not successfully found
            if (index1 == -1 || index2 == -1)
            {
                MessageBox.Show("Points not found on block.", "Fix Y error");
                return;
            }

            // ensure max and min as appropriate
            if ((index1 > index2) || (index1 == 0 && index2 == BoundaryPoints.Count - 1))
            {
                int tmp = index1;
                index1 = index2;
                index2 = tmp;

                DrawingPoint tmpPt = p1;
                p1 = p2;
                p2 = tmpPt;
            }

            // if points are the same, fix/unfix the point ...
            if (index1 == index2)
            {
                bool fix = !p1.IsFixedY;
                p1.IsFixedY = fix;

                if (!fix)
                {
                    foreach (LineConstraint lc in LineConstraints)
                    {
                        if (lc.Nodes.Contains(p1)) lc.IsFixedY = fix;
                    }
                }

                canvas.IsSaved = false;
                canvas.IsVerified = false;
            }

            // ... or if the points are adjacent, create a line constraint ...
            else if ((index2 - index1) == 1 || (index2 == 0 && index1 == BoundaryPoints.Count - 1))
            {
                LineConstraint existingLC = LineConstraints.Find(delegate(LineConstraint lc) { return lc.Nodes.Contains(p1) && lc.Nodes.Contains(p2); });

                if (existingLC != null)
                {
                    existingLC.IsFixedY = !existingLC.IsFixedY;
                }
                else
                {
                    LineConstraints.Add(new LineConstraint(canvas, p1, p2, false, true));
                }

                canvas.IsSaved = false;
                canvas.IsVerified = false;
            }

            // ... otherwise, indicate that a constraint cannot be applied in this manner
            else
            {
                MessageBox.Show("Points must be either the same or directly adjacent.", "Error");
            }


            // remove any line constraints that are both unfixed
            for (int i = LineConstraints.Count - 1; i >= 0; i--)
            {
                if (!(LineConstraints[i].IsFixedX) && !(LineConstraints[i].IsFixedY))
                    LineConstraints.RemoveAt(i);
            }
        }

        public void Translate(Vector delta)
        {
            Point p;
            for (int i = 0; i < Boundary.Points.Count; i++)
            {
                p = Boundary.Points[i];
                p.X += delta.X;
                p.Y += delta.Y;
                Boundary.Points[i] = p;
                boundaryPoints[i].Translate(delta);
            }

            foreach (LineConstraint lc in LineConstraints)
            {
                lc.UpdateLocation();
            }
        }

        public void Zoom(double factor, Point centre)
        {
            Point p;
            for (int i = 0; i < Boundary.Points.Count; i++)
            {
                p = Boundary.Points[i];
                p.X = centre.X + factor * (p.X - centre.X);
                p.Y = centre.Y + factor * (p.Y - centre.Y);
                Boundary.Points[i] = p;
                boundaryPoints[i].Zoom(factor, centre);
            }

            foreach (LineConstraint lc in LineConstraints)
            {
                lc.UpdateLocation();
            }
        }

        private void MouseLeftButtonDown(object sender, MouseEventArgs e)
        {
            if (canvas.DrawMode == DrawModes.Select)
            {
                this.IsSelected = true;
            }
        }

        public int CheckIntersecting()
        {
            double x1, y1, x2, y2, x3, y3, x4, y4,
                    m1, m2, b1, b2,
                    x, y;
            bool vert1, vert2;
            double toler = 1e-5;
            int count = 0;

            for (int i = 0; i < Boundary.Points.Count - 1; i++)
            {
                x1 = Boundary.Points[i].X;
                y1 = Boundary.Points[i].Y;

                x2 = Boundary.Points[i + 1].X;
                y2 = Boundary.Points[i + 1].Y;

                vert1 = Math.Abs(x2 - x1) < toler;

                if (vert1)
                {
                    x = x1;
                    m1 = 0;
                    b1 = 0;
                }
                else
                {
                    m1 = (y2 - y1) / (x2 - x1);
                    b1 = y1 - m1 * x1;
                    x = 0;
                }

                for (int j = i + 1; j < Boundary.Points.Count; j++)
                {
                    x3 = Boundary.Points[j].X;
                    y3 = Boundary.Points[j].Y;

                    if (j == Boundary.Points.Count - 1)
                    {
                        x4 = Boundary.Points[0].X;
                        y4 = Boundary.Points[0].Y;
                    }
                    else
                    {
                        x4 = Boundary.Points[j + 1].X;
                        y4 = Boundary.Points[j + 1].Y;
                    }

                    vert2 = Math.Abs(x4 - x3) < toler;

                    if (vert2)
                    {
                        x = x3;
                        m2 = 0;
                        b2 = 0;
                    }
                    else
                    {
                        m2 = (y4 - y3) / (x4 - x3);
                        b2 = y3 - m2 * x3;
                        x = 0;
                    }

                    if (vert1 || vert2)
                    {
                        if (!vert1)
                        {
                            y = m1 * x + b1;

                            if ((x - Math.Min(x1, x2)) < toler && (Math.Max(x1, x2) - x) < toler
                                && (y - Math.Min(y3, y4)) < toler && (Math.Max(y3, y4) - y) < toler)
                            {
                                count++;
                            }
                        }
                        else if (!vert2)
                        {
                            y = m2 * x + b2;

                            if ((x - Math.Min(x3, x4)) < toler && (Math.Max(x3, x4) - x) < toler
                                && (y - Math.Min(y1, y2)) < toler && (Math.Max(y1, y2) - y) < toler)
                            {
                                count++;
                            }
                        }
                        else
                        {
                            if (Math.Abs(x3 - x1) < toler && ((y3 < y2 && y4 > y1) || (y3 > y2 && y4 < y1))) count++;
                        }
                    }
                    else
                    {
                        if (Math.Abs(m2 - m1) < toler)
                        {
                            if (Math.Abs(b2 - b1) < toler
                                && !((x1 == x4 && y1 == y4) || (x2 == x3 && y2 == y3)))
                            {
                                count++;
                            }
                        }
                        else
                        {
                            x = (b2 - b1) / (m1 - m2);

                            if ((x - Math.Min(x1, x2)) > toler && (Math.Max(x1, x2) - x) > toler
                                && (x - Math.Min(x3, x4)) > toler && (Math.Max(x3, x4) - x) > toler)
                            {
                                count++;
                            }
                        }
                    }
                }
            }

            return count;
        }
    }

    public class MaterialType
    {
        double kno_, knr_, Acoef_, ktrb_, ktrs_;

        public MaterialType()
        {
        }

        public double Phi { get; set; }
        public double Cohesion { get; set; }
        public double Gamma { get; set; }
        public double Emod { get; set; }
        public double Nu { get; set; }

        // Calculate RFEM soil properties
        // after Stolle & Guo (2008)
        public void ComputeRFEMProperties()
        {
            double kappa = 1.0, del = 0.0015;

            kno_ = (Emod * (1 - Nu) / ((1 + Nu) * (1 - 2 * Nu + 0.00001))) / del;
            knr_ = kno_ / 100;
            Acoef_ = kappa / (kno_ - knr_);

            ktrb_ = (Emod / (2 * (1 + Nu))) / (del * 10000);
            ktrs_ = ktrb_ / 10;
        }

        public double Kno { get { return kno_; } }
        public double Knr { get { return knr_; } }
        public double Acoef { get { return Acoef_; } }
        public double Ktrb { get { return ktrb_; } }
        public double Ktrs { get { return ktrs_; } }

        public Brush Fill { get; set; }
        public string Name { get; set; }

        public override string ToString()
        {
            return this.Name != null ? this.Name : "Null";
        }
    }

    class GAParams
    {
        public GAParams()
        {
        }

        public int Population { get; set; }
        public int Generations { get; set; }
        public double FittestProportion { get; set; }
        public double MatingPoolProportion { get; set; }
        public double CrossoverProbability { get; set; }
        public double MutationProbability { get; set; }
        public double SliceWidth { get; set; }
    }

    class FEAParams
    {
        public FEAParams ()
        {
        }

        public double ElementSize { get; set; }
        public double ColWidth { get; set; }
        public double RowHeight { get; set; }
    }

    class MeshLine
    {
        private SlopeCanvas canvas;
        private List<MeshPoint> meshPoints;
        private Line line;

        public MeshLine(SlopeCanvas canvas, double x)
        {
            this.canvas = canvas;

            // initialize and pre-allocate for 10 MeshPoints per MeshLine and 2 per MaterialBlock
            meshPoints = new List<MeshPoint>(10);
            List<MeshPoint> blockPoints = new List<MeshPoint>(2);

            // for intersection finding algorithm
            double toler = 1e-5;
            double x1, y1, x2, y2, m = 0, y = 0;
            //bool added;

            // loop through MaterialBlocks finding boundary intersection points
            foreach (MaterialBlock mb in canvas.MaterialBlocks)
            {
                // clear intersection point list (does not de-allocate memory)
                blockPoints.Clear();

                // initialize trailing point to final point in list
                x1 = mb.Boundary.Points[mb.Boundary.Points.Count - 1].X;
                y1 = mb.Boundary.Points[mb.Boundary.Points.Count - 1].Y;

                // loop through MaterialBlock points, finding intersections
                foreach (Point p in mb.Boundary.Points)
                {
                    x2 = p.X;   // update leading point
                    y2 = p.Y;

                    // MeshLine will not intersect segment
                    if (x < Math.Min(x1, x2) || x > Math.Max(x1, x2) || Math.Abs(x2 - x1) < toler)
                    {
                        x1 = x2; y1 = y2;   // skip to next point
                        continue;
                    }

                    // compute slope and y-coord of intersection
                    if (x2 > x1)
                    {
                        m = (y2 - y1) / (x2 - x1);
                        y = y1 + m * (x - x1);
                    }
                    else
                    {
                        m = (y1 - y2) / (x1 - x2);
                        y = y2 + m * (x - x2);
                    }

                    // add the intersection point if found
                    blockPoints.Add(new MeshPoint(new Point(x, y), mb.Material));

                    x1 = x2;    // update trailing point
                    y1 = y2;
                }

                // sort points for labeling
                blockPoints.Sort(MeshPoint.CompareByY);

                // label points as entering or exiting the block depending on order
                for (int i = 0; i < blockPoints.Count; i++)
                    blockPoints[i].Type = i % 2 == 0 ? MeshPointType.Entrance : MeshPointType.Exit;

                // insert block points in y-order in MeshLine
                for (int i = 0; i < meshPoints.Count; i++)
                {
                    if (meshPoints[i].Y < blockPoints[0].Y)
                    {
                        meshPoints.InsertRange(i, blockPoints.GetRange(0, 2));
                        blockPoints.RemoveRange(0, 2);
                        if (blockPoints.Count <= 1) break;
                    }
                }

                // insert remaining block points at end of MeshLine
                while (blockPoints.Count > 1)
                {
                    meshPoints.AddRange(blockPoints.GetRange(0, 2));
                    blockPoints.RemoveRange(0, 2);
                }
            }

            line = new Line();
            line.Stroke = Brushes.Black;
            line.StrokeThickness = 0.75;
            line.Opacity = 0.9;
            line.StrokeDashArray.Add(7.5);
            line.StrokeDashArray.Add(5);
            line.X1 = x;
            line.X2 = x;
            line.Y1 = meshPoints[0].Y;
            line.Y2 = meshPoints[meshPoints.Count - 1].Y;

            canvas.Children.Add(line);

            for (int i = 0; i < meshPoints.Count; i++)
                canvas.Children.Add(meshPoints[i].Location);
        }

        public Line Line { get { return this.line; } }
        public List<MeshPoint> MeshPoints { get { return this.meshPoints; } }

        public bool IsVisible
        {
            set
            {
                line.Visibility = value ? Visibility.Visible : Visibility.Hidden;

                foreach (MeshPoint mp in meshPoints) mp.IsVisible = value;
            }
        }

        public void Delete()
        {
            foreach (MeshPoint mp in meshPoints) canvas.Children.Remove(mp.Location);
            meshPoints.Clear();

            canvas.Children.Remove(line);
        }

        public void Translate(Vector delta)
        {
            foreach (MeshPoint mp in meshPoints) mp.Translate(delta);

            line.X1 += delta.X;
            line.X2 += delta.X;
            line.Y1 += delta.Y;
            line.Y2 += delta.Y;
        }

        public void Zoom(double factor, Point centre)
        {
            foreach (MeshPoint mp in meshPoints) mp.Zoom(factor, centre);

            line.X1 = centre.X + factor * (line.X1 - centre.X);
            line.X2 = centre.X + factor * (line.X2 - centre.X);
            line.Y1 = centre.Y + factor * (line.Y1 - centre.Y);
            line.Y2 = centre.Y + factor * (line.Y2 - centre.Y);
        }
    }

    class MeshPoint
    {
        private Ellipse location;
        private MaterialType material;

        public MeshPoint(Point pt, MaterialType material)
        {
            location = new Ellipse();
            location.Height = 3;
            location.Width = 3;
            location.Fill = Brushes.Black;
            location.Stroke = Brushes.Black;
            location.Visibility = Visibility.Visible;
            location.Margin = new Thickness(pt.X - 1.5, pt.Y - 1.5, 0, 0);

            this.material = material;
        }

        public double X { get { return this.location.Margin.Left + 1.5; } }
        public double Y { get { return this.location.Margin.Top + 1.5; } }
        public Ellipse Location { get { return this.location; } }
        public MaterialType Material { get { return this.material; } }
        public MeshPointType Type { get; set; }

        public bool IsVisible
        {
            set
            {
                Location.Visibility = value ? Visibility.Visible : Visibility.Hidden;
            }
        }

        public void Translate(Vector delta)
        {
            location.Margin = new Thickness(location.Margin.Left + delta.X, location.Margin.Top + delta.Y, 0, 0);
        }

        public void Zoom(double factor, Point centre)
        {
            location.Margin = new Thickness(centre.X + factor * ((location.Margin.Left + 1.5) - centre.X) - 1.5,
                                            centre.Y + factor * ((location.Margin.Top + 1.5) - centre.Y) - 1.5,
                                            0, 0);
        }

        public static int CompareByY(MeshPoint mp1, MeshPoint mp2)
        {
            // If mp1 is null...
            if (mp1 == null)
            {
                // ...and mp2 is also null...
                if (mp2 == null)
                {
                    // ...mp1 == mp2
                    return 0;
                }

                // ...and mp2 is not null...
                else
                {
                    // ...mp2 > mp1
                    return -1;
                }
            }

            // If mp1 is not null...
            else
            {
                // ...and mp2 is null...
                if (mp2 == null)
                {
                    // ...mp1 > mp2
                    return 1;
                }

                // ...and mp2 is also not null...
                else
                {
                    // ...compare y coordinates of points
                    if (mp1.Location.Margin.Top < mp2.Location.Margin.Top) return 1;
                    else if (mp1.Location.Margin.Top > mp2.Location.Margin.Top) return -1;
                    else return 0;
                }
            }
        }
    }

    class AnalysisMeshPoint
    {
        private Point location;
        private MaterialType material;
        private MeshPointType type;

        public AnalysisMeshPoint(Point pt, MaterialType material, MeshPointType type)
        {
            location.X=pt.X;
            location.Y=pt.Y;

            this.material = material;
            this.type = type;
        }

        public double X { get { return this.location.X; } }
        public double Y { get { return this.location.Y; } }
        public MaterialType Material { get { return this.material; } }
        public MeshPointType Type { get { return this.type; } }
    }
}