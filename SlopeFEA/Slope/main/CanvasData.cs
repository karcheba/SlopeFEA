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
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Shapes;

namespace SlopeFEA
{
    public enum DrawModes { Select , Boundaries , Materials , Pan , ZoomArea , MovePoints , AddPoints , PrintPoint , Fixities , PointLoad , LineLoad };
    public enum PlotModes { DeformedMesh , DisplacementVectors , PlasticPoints , SmoothedStress , UnSmoothedStress };
    public enum Units { Metres , Millimetres , Feet , Inches };
    public enum Scales
    {
        sc1000 , sc800 , sc600 , sc500 , sc400 , sc300 , sc200 , sc150 ,
        sc100 , sc50 , sc25 , sc10 , sc5 , sc2 , sc1 , Custom
    };
    public enum GridType { Major , Minor };
    public enum MeshPointType { Entrance , Exit };
    public enum SoilMovement { LtoR , RtoL , None };
    public enum AnalysisType { Bishop , RFEM , FEA3NodedTri , FEA4NodedQuad };

    public class GridPoint
    {
        private GridType type;
        private Ellipse location;

        public GridPoint ( Point pt , GridType type )
        {
            location = new Ellipse();
            location.Height = 1.5;
            location.Width = 1.5;
            location.Fill = Brushes.Black;
            location.Stroke = Brushes.Black;
            location.Visibility = Visibility.Hidden;
            location.Margin = new Thickness( pt.X - 0.75 , pt.Y - 0.75 , 0 , 0 );

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

        public void Translate ( Vector delta )
        {
            location.Margin = new Thickness( location.Margin.Left + delta.X , location.Margin.Top + delta.Y , 0 , 0 );
        }

        public void Zoom ( double factor , Point centre )
        {
            location.Margin = new Thickness( centre.X + factor * (location.Margin.Left - centre.X) ,
                                            centre.Y + factor * (location.Margin.Top - centre.Y) ,
                                            0 , 0 );
        }
    }

    public class SlopeBoundary
    {
        private SlopeCanvas canvas;
        private bool isSelected , showMesh = false;
        private List<DrawingPoint> boundaryPoints;
        private List<MeshLine> meshLines;
        private List<Point> upperSurface;
        private SoilMovement soilDir = SoilMovement.None;

        public SlopeBoundary ( SlopeCanvas canvas )
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

            canvas.Children.Add( Boundary );
        }

        public SlopeBoundary ( SlopeCanvas canvas , Point[] pts )
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
            Boundary.MouseLeftButtonDown += new MouseButtonEventHandler( MouseLeftButtonDown );
            Boundary.Visibility = Visibility.Visible;

            canvas.Children.Add( Boundary );

            for ( int i = 0 ; i < pts.Length ; i++ )
            {
                Boundary.Points.Add( pts[i] );
                boundaryPoints.Add( new DrawingPoint( canvas , this , pts[i] ) );
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
                if ( canvas.IsMeshed )
                {
                    this.showMesh = value;
                    meshLines.ForEach( delegate( MeshLine ml ) { ml.IsVisible = value; } );
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
                if ( boundaryPoints.Count == 0 ) return 0;

                double min = boundaryPoints[0].Point.X;
                for ( int i = 1 ; i < boundaryPoints.Count ; i++ )
                {
                    min = Math.Min( min , boundaryPoints[i].Point.X );
                }
                return min;
            }
        }

        public double XMax
        {
            get
            {
                if ( boundaryPoints.Count == 0 ) return 0;

                double max = boundaryPoints[0].Point.X;
                for ( int i = 1 ; i < boundaryPoints.Count ; i++ )
                {
                    max = Math.Max( max , boundaryPoints[i].Point.X );
                }
                return max;
            }
        }

        public double YMin
        {
            get
            {
                if ( boundaryPoints.Count == 0 ) return 0;

                double min = boundaryPoints[0].Point.Y;
                for ( int i = 1 ; i < boundaryPoints.Count ; i++ )
                {
                    min = Math.Max( min , boundaryPoints[i].Point.Y );
                }
                return min;
            }
        }

        public double Area
        {
            get
            {
                if ( boundaryPoints.Count == 0 ) return 0;

                double x1 , y1 , x2 , y2;

                x1 = boundaryPoints[boundaryPoints.Count - 1].Point.X;
                y1 = boundaryPoints[boundaryPoints.Count - 1].Point.Y;
                x2 = boundaryPoints[0].Point.X;
                y2 = boundaryPoints[0].Point.Y;

                double sum = x1 * y2 - x2 * y1;

                for ( int i = 0 ; i < boundaryPoints.Count - 1 ; i++ )
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

        public void SortPoints ()
        {
            if ( this.Area < 0 )
            {
                this.BoundaryPoints.Reverse();

                PointCollection pts = this.Boundary.Points;
                PointCollection revPts = new PointCollection();
                for ( int i = pts.Count - 1 ; i >= 0 ; i-- )
                {
                    revPts.Add( pts[i] );
                }
                this.Boundary.Points = revPts;
            }
        }

        public bool SaveMesh ()
        {
            ClosableCanvasTabItem parentTab = (ClosableCanvasTabItem) ((Grid) canvas.Parent).Parent;

            string[] split = ((string) parentTab.Tag).Split( '.' );

            if ( split[split.Length - 1] != "slp" ) return false;

            split[split.Length - 1] = "msh";

            string path = string.Join( "." , split );

            using ( TextWriter tw = new StreamWriter( path ) )
            {
                double factor;
                double xOffset = canvas.OriginOffsetX , yOffset = canvas.OriginOffsetY;
                double canvasHeight = canvas.ActualHeight;
                string units;
                switch ( canvas.Units )
                {
                    case Units.Metres: units = "m"; factor = 0.0254; break;
                    case Units.Millimetres: units = "mm"; factor = 25.4; break;
                    case Units.Feet: units = "ft"; factor = 1.0 / 12.0; break;
                    default: units = "in"; factor = 1; break;
                }

                double scaleX = factor * canvas.Scale / canvas.DpiX;
                double scaleY = factor * canvas.Scale / canvas.DpiY;

                tw.WriteLine( String.Format( "Units = {0}" , units ) );

                tw.WriteLine();

                if ( upperSurface.Count == 0 ) CheckYMax( XMin , XMax , YMin );

                tw.WriteLine( String.Format( "Number of upper surface points = {0}" , upperSurface.Count ) );

                tw.WriteLine();

                tw.WriteLine( "Upper Surface Coordinates" );

                double x = 0 , y = 0;
                foreach ( Point p in upperSurface )
                {
                    x = (p.X - xOffset) * scaleX;
                    y = (canvasHeight - p.Y - yOffset) * scaleY;
                    tw.WriteLine( String.Format( "{0}, {1}" , x , y ) );
                }

                tw.WriteLine();

                tw.WriteLine( String.Format( "Number of mesh lines = {0}" , meshLines.Count ) );

                tw.WriteLine();

                if ( meshLines.Count > 0 )
                {
                    tw.WriteLine( "ML1" );
                    tw.WriteLine( String.Format( "Number of points = {0}" , meshLines[0].MeshPoints.Count ) );

                    x = (XMin - xOffset) * scaleX;

                    foreach ( MeshPoint mp in meshLines[0].MeshPoints )
                    {
                        y = (canvasHeight - mp.Y - yOffset) * scaleY;
                        tw.WriteLine( String.Format( "{0}, {1}, {2}, \"{3}\"" , x , y , mp.Type , mp.Material ) );
                    }

                    tw.WriteLine();
                }

                for ( int i = 1 ; i < meshLines.Count - 1 ; i++ )
                {
                    tw.WriteLine( String.Format( "ML{0}" , i + 1 ) );
                    tw.WriteLine( String.Format( "Number of points = {0}" , meshLines[i].MeshPoints.Count ) );

                    x = (meshLines[i].Line.X1 - xOffset) * scaleX;

                    foreach ( MeshPoint mp in meshLines[i].MeshPoints )
                    {
                        y = (canvasHeight - mp.Y - yOffset) * scaleY;
                        tw.WriteLine( String.Format( "{0}, {1}, {2}, \"{3}\"" , x , y , mp.Type , mp.Material ) );
                    }

                    tw.WriteLine();
                }

                if ( meshLines.Count > 1 )
                {
                    tw.WriteLine( String.Format( "ML{0}" , meshLines.Count ) );
                    tw.WriteLine( String.Format( "Number of points = {0}" , meshLines[meshLines.Count - 1].MeshPoints.Count ) );

                    x = (XMax - xOffset) * scaleX;

                    foreach ( MeshPoint mp in meshLines[meshLines.Count - 1].MeshPoints )
                    {
                        y = (canvasHeight - mp.Y - yOffset) * scaleY;
                        tw.WriteLine( String.Format( "{0}, {1}, {2}, \"{3}\"" , x , y , mp.Type , mp.Material ) );
                    }

                    tw.WriteLine();
                }
            }

            return true;
        }

        public double YMaxOfX ( double x , double yMin )
        {
            double toler = 1e-5;
            double y = yMin;

            for ( int i = 0 ; i < boundaryPoints.Count ; i++ )
            {
                if ( Math.Abs( x - boundaryPoints[i].Point.X ) < toler )
                {
                    y = Math.Min( y , boundaryPoints[i].Point.Y );
                }
            }

            return y;
        }

        public bool CheckXMaxMin ( double value )
        {
            int count = 0;
            bool counting = false , done1 = false , wasZero = false , done2 = false;
            double toler = 1e-5;
            for ( int i = 0 ; i < boundaryPoints.Count ; i++ )
            {
                if ( count == 0 && Math.Abs( boundaryPoints[i].Point.X - value ) < toler )
                {
                    counting = true;
                    count++;
                    if ( i == 0 ) wasZero = true;
                }
                else if ( counting )
                {
                    if ( Math.Abs( boundaryPoints[i].Point.X - value ) < toler ) count++;
                    else
                    {
                        counting = false;
                        if ( !done1 ) done1 = true;
                        else done2 = true;
                    }
                }
                else if ( done1 && Math.Abs( boundaryPoints[i].Point.X - value ) < toler )
                {
                    if ( wasZero )
                    {
                        count++;
                        counting = true;
                    }
                    else return false;
                }
                else if ( done2 )
                {
                    return false;
                }
            }

            return count >= 2;
        }

        public bool CheckYMin ( double value )
        {
            int count = 0;
            bool counting = false , done1 = false , wasZero = false , done2 = false;
            double toler = 1e-5;
            for ( int i = 0 ; i < boundaryPoints.Count ; i++ )
            {
                if ( count == 0 && Math.Abs( boundaryPoints[i].Point.Y - value ) < toler )
                {
                    counting = true;
                    count++;
                    if ( i == 0 ) wasZero = true;
                }
                else if ( counting )
                {
                    if ( Math.Abs( boundaryPoints[i].Point.Y - value ) < toler ) count++;
                    else
                    {
                        counting = false;
                        if ( !done1 ) done1 = true;
                        else done2 = true;
                    }
                }
                else if ( done1 && Math.Abs( boundaryPoints[i].Point.Y - value ) < toler )
                {
                    if ( wasZero )
                    {
                        count++;
                        counting = true;
                    }
                    else return false;
                }
                else if ( done2 )
                {
                    return false;
                }
            }

            return count >= 2;
        }

        public int CheckYMax ( double xMin , double xMax , double yMin )
        {
            upperSurface.Clear();

            int countDir = 1;
            double x = xMin , y = yMin;
            double toler = 1e-5;
            int icurr = 0 , inext = 0 , maxIndex = boundaryPoints.Count - 1;

            soilDir = SoilMovement.None;

            for ( int i = 0 ; i < boundaryPoints.Count ; i++ )
            {
                if ( Math.Abs( x - boundaryPoints[i].Point.X ) < toler )
                {
                    if ( boundaryPoints[i].Point.Y <= y )
                    {
                        icurr = i;
                        y = boundaryPoints[i].Point.Y;
                    }
                }
            }

            upperSurface.Add( new Point( x , y ) );

            while ( (xMax - x) > toler )
            {
                if ( icurr == maxIndex && countDir == 1 ) inext = 0;
                else if ( icurr == 0 && countDir == -1 ) inext = maxIndex;
                else inext = icurr + countDir;

                if ( x == xMin )
                {
                    if ( Math.Abs( x - boundaryPoints[inext].Point.X ) < toler )
                    {
                        countDir = -1;
                        if ( icurr == 0 ) inext = maxIndex;
                        else inext = icurr + countDir;

                        // minimum X boundary does not have a non-vertical exit line
                        if ( Math.Abs( x - boundaryPoints[inext].Point.X ) < toler ) return -2;
                    }
                }

                if ( (x - boundaryPoints[inext].Point.X) > toler ) return -3; // upper surface direction inconsistent

                x = boundaryPoints[inext].Point.X;

                if ( (y - boundaryPoints[inext].Point.Y) < -toler )
                {
                    if ( soilDir == SoilMovement.None ) soilDir = SoilMovement.LtoR;
                    else if ( soilDir == SoilMovement.RtoL ) return -4;  // soil direction inconsistent
                }
                else if ( (y - boundaryPoints[inext].Point.Y) > toler )
                {
                    if ( soilDir == SoilMovement.None ) soilDir = SoilMovement.RtoL;
                    else if ( soilDir == SoilMovement.LtoR ) return -4;   // soil direction inconsistent
                }

                y = boundaryPoints[inext].Point.Y;

                upperSurface.Add( new Point( x , y ) );

                icurr = inext;
            }

            return soilDir == SoilMovement.LtoR ? 1 : (soilDir == SoilMovement.RtoL ? -1 : 0);
        }

        public void GenerateMesh ( double xMin , double xMax )
        {
            meshLines = new List<MeshLine>();

            double toler = 1e-5;
            meshLines.Add( new MeshLine( canvas , xMin + toler ) );

            double factor;
            switch ( canvas.Units )
            {
                case Units.Metres: factor = 0.0254; break;
                case Units.Millimetres: factor = 25.4; break;
                case Units.Feet: factor = 1.0 / 12.0; break;
                default: factor = 1; break;
            }

            double sliceWidth = canvas.GeneticAlgorithmParameters.SliceWidth / (factor * canvas.Scale) * canvas.DpiX;

            double x = xMin + sliceWidth;

            while ( x < xMax )
            {
                meshLines.Add( new MeshLine( canvas , x ) );

                x += sliceWidth;
            }

            if ( xMax - x > toler ) meshLines.Add( new MeshLine( canvas , xMax - toler ) );

            canvas.IsMeshed = true;
            canvas.ShowMesh = true;
            canvas.IsSaved = false;
        }

        public void ClearMesh ()
        {
            meshLines.ForEach( delegate( MeshLine ml ) { ml.Delete(); } );
            meshLines.Clear();
        }

        public void Delete ()
        {
            Boundary.Points.Clear();
            for ( int i = 0 ; i < boundaryPoints.Count ; i++ )
            {
                canvas.Children.Remove( boundaryPoints[i].Dot );
            }
            boundaryPoints.Clear();

            while ( canvas.MaterialBlocks.Count > 0 )
            {
                canvas.MaterialBlocks[0].Delete();
            }
        }

        public void AddPoint ( DrawingPoint p1 , DrawingPoint p2 )
        {
            int index1 = 0;
            for ( int i = 0 ; i < BoundaryPoints.Count ; i++ )
            {
                if ( BoundaryPoints[i] == p1 )
                {
                    index1 = i;
                    break;
                }
            }

            int index2 = 0;
            for ( int i = 0 ; i < BoundaryPoints.Count ; i++ )
            {
                if ( BoundaryPoints[i] == p2 )
                {
                    index2 = i;
                    break;
                }
            }

            int maxIndex = Math.Max( index1 , index2 );
            int minIndex = Math.Min( index1 , index2 );

            if ( (maxIndex - minIndex) == 1 )
            {
                Point newPoint = new Point( 0.5 * (p1.Point.X + p2.Point.X) , 0.5 * (p1.Point.Y + p2.Point.Y) );
                BoundaryPoints.Insert( maxIndex , new DrawingPoint( canvas , this , newPoint ) );
                Boundary.Points.Insert( maxIndex , newPoint );
                canvas.IsSaved = false;
                canvas.IsVerified = false;
            }
            else if ( minIndex == 0 && maxIndex == BoundaryPoints.Count - 1 )
            {
                Point newPoint = new Point( 0.5 * (p1.Point.X + p2.Point.X) , 0.5 * (p1.Point.Y + p2.Point.Y) );
                BoundaryPoints.Add( new DrawingPoint( canvas , this , newPoint ) );
                Boundary.Points.Add( newPoint );
                canvas.IsSaved = false;
                canvas.IsVerified = false;
            }
            else
            {
                MessageBox.Show( "Points must be different and adjacent." , "Error" );
            }
        }

        public void Translate ( Vector delta )
        {
            meshLines.ForEach( delegate( MeshLine ml ) { ml.Translate( delta ); } );

            Point p;
            for ( int i = 0 ; i < Boundary.Points.Count ; i++ )
            {
                p = Boundary.Points[i];
                p.X += delta.X;
                p.Y += delta.Y;
                Boundary.Points[i] = p;
                boundaryPoints[i].Translate( delta );
            }
        }

        public void Zoom ( double factor , Point centre )
        {
            meshLines.ForEach( delegate( MeshLine ml ) { ml.Zoom( factor , centre ); } );

            Point p;
            for ( int i = 0 ; i < Boundary.Points.Count ; i++ )
            {
                p = Boundary.Points[i];
                p.X = centre.X + factor * (p.X - centre.X);
                p.Y = centre.Y + factor * (p.Y - centre.Y);
                Boundary.Points[i] = p;
                boundaryPoints[i].Zoom( factor , centre );
            }
        }

        private void MouseLeftButtonDown ( object sender , MouseEventArgs e )
        {
            // Select boundary object
            if ( canvas.DrawMode == DrawModes.Select )
            {
                this.IsSelected = true;
            }
        }

        public int CheckIntersecting ()
        {
            double x1 , y1 , x2 , y2 , x3 , y3 , x4 , y4 ,
                    m1 , m2 , b1 , b2 ,
                    x , y;
            bool vert1 , vert2;
            double toler = 1e-5;
            int count = 0;

            for ( int i = 0 ; i < Boundary.Points.Count - 1 ; i++ )
            {
                x1 = Boundary.Points[i].X;
                y1 = Boundary.Points[i].Y;

                x2 = Boundary.Points[i + 1].X;
                y2 = Boundary.Points[i + 1].Y;

                vert1 = Math.Abs( x2 - x1 ) < toler;

                if ( vert1 )
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

                for ( int j = i + 1 ; j < Boundary.Points.Count ; j++ )
                {
                    x3 = Boundary.Points[j].X;
                    y3 = Boundary.Points[j].Y;

                    if ( j == Boundary.Points.Count - 1 )
                    {
                        x4 = Boundary.Points[0].X;
                        y4 = Boundary.Points[0].Y;
                    }
                    else
                    {
                        x4 = Boundary.Points[j + 1].X;
                        y4 = Boundary.Points[j + 1].Y;
                    }

                    vert2 = Math.Abs( x4 - x3 ) < toler;

                    if ( vert2 )
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

                    if ( vert1 || vert2 )
                    {
                        if ( !vert1 )
                        {
                            y = m1 * x + b1;

                            if ( (x - Math.Min( x1 , x2 )) < toler && (Math.Max( x1 , x2 ) - x) < toler
                                && (y - Math.Min( y3 , y4 )) < toler && (Math.Max( y3 , y4 ) - y) < toler )
                            {
                                count++;
                            }
                        }
                        else if ( !vert2 )
                        {
                            y = m2 * x + b2;

                            if ( (x - Math.Min( x3 , x4 )) < toler && (Math.Max( x3 , x4 ) - x) < toler
                                && (y - Math.Min( y1 , y2 )) < toler && (Math.Max( y1 , y2 ) - y) < toler )
                            {
                                count++;
                            }
                        }
                        else
                        {
                            if ( Math.Abs( x3 - x1 ) < toler && ((y3 < y2 && y4 > y1) || (y3 > y2 && y4 < y1)) ) count++;
                        }
                    }
                    else
                    {
                        if ( Math.Abs( m2 - m1 ) < toler )
                        {
                            if ( Math.Abs( b2 - b1 ) < toler
                                && !((x1 == x4 && y1 == y4) || (x2 == x3 && y2 == y3)) )
                            {
                                count++;
                            }
                        }
                        else
                        {
                            x = (b2 - b1) / (m1 - m2);

                            if ( (x - Math.Min( x1 , x2 )) > toler && (Math.Max( x1 , x2 ) - x) > toler
                                && (x - Math.Min( x3 , x4 )) > toler && (Math.Max( x3 , x4 ) - x) > toler )
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

    public class DrawingPoint
    {
        private SlopeCanvas canvas;
        private SlopeDefineCanvas defineCanvas;
        private SlopeBoundary parentBoundary;
        private List<MaterialBlock> parentBlocks;
        private Point point;
        private Ellipse dot;
        private bool isSelected;
        private bool isPrintPoint;
        private bool isFixedX , isFixActiveX , isFixedY , isFixActiveY;
        private List<Polyline> fixLines;
        private List<bool> phaseFixActiveX , phaseFixActiveY;

        public DrawingPoint ( SlopeCanvas canvas , object parent , Point pt )
        {
            this.canvas = canvas;
            if ( parent is SlopeBoundary ) this.parentBoundary = parent as SlopeBoundary;
            else if ( parent is MaterialBlock ) this.parentBlocks = new List<MaterialBlock>() { parent as MaterialBlock };
            this.point = pt;

            fixLines = new List<Polyline>();
            Polyline newLine;
            for ( int i = 0 ; i < 4 ; i++ )
            {
                newLine = new Polyline();
                newLine.Visibility = Visibility.Hidden;
                newLine.Fill = Brushes.Blue;
                newLine.Opacity = 1.0;
                newLine.StrokeThickness = 1.5;
                newLine.Stroke = Brushes.Blue;
                fixLines.Add( newLine );
                canvas.Children.Add( newLine );

                newLine.MouseLeftButtonUp += new MouseButtonEventHandler( fixLines_MouseLeftButtonUp );
            }

            fixLines[0].Points.Add( new Point( point.X - 7 , point.Y - 3.5 ) );
            fixLines[0].Points.Add( new Point( point.X + 7 , point.Y - 3.5 ) );

            fixLines[1].Points.Add( new Point( point.X - 7 , point.Y + 3.5 ) );
            fixLines[1].Points.Add( new Point( point.X + 7 , point.Y + 3.5 ) );

            fixLines[2].Points.Add( new Point( point.X - 3.5 , point.Y + 7 ) );
            fixLines[2].Points.Add( new Point( point.X - 3.5 , point.Y - 7 ) );

            fixLines[3].Points.Add( new Point( point.X + 3.5 , point.Y + 7 ) );
            fixLines[3].Points.Add( new Point( point.X + 3.5 , point.Y - 7 ) );


            dot = new Ellipse();
            dot.HorizontalAlignment = HorizontalAlignment.Left;
            dot.VerticalAlignment = VerticalAlignment.Top;
            dot.Height = 7;
            dot.Width = 7;
            dot.Margin = new Thickness( point.X - 0.5 * dot.Width , point.Y - 0.5 * dot.Height , 0 , 0 );
            dot.Stroke = Brushes.Black;
            dot.Fill = Brushes.Black;
            dot.Opacity = 0.7;
            dot.MouseLeftButtonDown += new MouseButtonEventHandler( MouseLeftButtonDown );

            phaseFixActiveX = new List<bool>();
            phaseFixActiveY = new List<bool>();

            canvas.Children.Add( dot );
        }

        public DrawingPoint ( SlopeDefineCanvas canvas , MaterialBlock parent , Point pt )
        {
            this.defineCanvas = canvas;
            this.parentBlocks = new List<MaterialBlock>() { parent };
            this.point = pt;

            fixLines = new List<Polyline>();
            Polyline newLine;
            for ( int i = 0 ; i < 4 ; i++ )
            {
                newLine = new Polyline();
                newLine.Visibility = Visibility.Hidden;
                newLine.Fill = Brushes.Blue;
                newLine.Opacity = 1.0;
                newLine.StrokeThickness = 1.5;
                newLine.Stroke = Brushes.Blue;
                fixLines.Add( newLine );
                canvas.Children.Add( newLine );

                newLine.MouseLeftButtonUp += new MouseButtonEventHandler( fixLines_MouseLeftButtonUp );
            }

            fixLines[0].Points.Add( new Point( point.X - 7 , point.Y - 3.5 ) );
            fixLines[0].Points.Add( new Point( point.X + 7 , point.Y - 3.5 ) );

            fixLines[1].Points.Add( new Point( point.X - 7 , point.Y + 3.5 ) );
            fixLines[1].Points.Add( new Point( point.X + 7 , point.Y + 3.5 ) );

            fixLines[2].Points.Add( new Point( point.X - 3.5 , point.Y + 7 ) );
            fixLines[2].Points.Add( new Point( point.X - 3.5 , point.Y - 7 ) );

            fixLines[3].Points.Add( new Point( point.X + 3.5 , point.Y + 7 ) );
            fixLines[3].Points.Add( new Point( point.X + 3.5 , point.Y - 7 ) );


            dot = new Ellipse();
            dot.HorizontalAlignment = HorizontalAlignment.Left;
            dot.VerticalAlignment = VerticalAlignment.Top;
            dot.Height = 7;
            dot.Width = 7;
            dot.Margin = new Thickness( point.X - 0.5 * dot.Width , point.Y - 0.5 * dot.Height , 0 , 0 );
            dot.Stroke = Brushes.Black;
            dot.Fill = Brushes.Black;
            dot.Opacity = 0.7;
            dot.Visibility = Visibility.Hidden;
            dot.MouseLeftButtonDown += new MouseButtonEventHandler( MouseLeftButtonDown );

            phaseFixActiveX = new List<bool>();
            phaseFixActiveY = new List<bool>();

            canvas.Children.Add( dot );
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

                if ( value )
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

        public bool IsPrintPoint
        {
            get
            {
                return this.isPrintPoint;
            }
            set
            {
                this.isPrintPoint = value;

                if ( value )
                {
                    dot.Height = 12;
                    dot.Width = 12;
                }
                else
                {
                    dot.Height = 7;
                    dot.Width = 7;
                }

                dot.Margin = new Thickness( point.X - 0.5 * dot.Width , point.Y - 0.5 * dot.Height , 0 , 0 );
            }
        }

        public bool IsFixedX
        {
            get { return this.isFixedX; }
            set
            {
                this.isFixedX = value;
                this.IsFixActiveX = value;
                fixLines[2].Visibility = fixLines[3].Visibility = value ? Visibility.Visible : Visibility.Hidden;
            }
        }

        public bool IsFixActiveX
        {
            get { return this.isFixActiveX; }
            set
            {
                this.isFixActiveX = value;
                fixLines[2].Opacity = fixLines[3].Opacity = value ? 1.0 : 0.2;
            }
        }

        public List<bool> PhaseFixActiveX { get { return this.phaseFixActiveX; } }

        public bool IsFixedY
        {
            get { return this.isFixedY; }
            set
            {
                this.isFixedY = value;
                this.IsFixActiveY = value;
                fixLines[0].Visibility = fixLines[1].Visibility = value ? Visibility.Visible : Visibility.Hidden;
            }
        }

        public bool IsFixActiveY
        {
            get { return this.isFixActiveY; }
            set
            {
                this.isFixActiveY = value;
                fixLines[0].Opacity = fixLines[1].Opacity = value ? 1.0 : 0.2;
            }
        }

        public List<bool> PhaseFixActiveY { get { return this.phaseFixActiveY; } }

        public Point Point { get { return this.point; } }
        public Ellipse Dot { get { return this.dot; } }
        public bool IsMouseOver { get { return dot.IsMouseOver; } }
        public SlopeBoundary ParentBoundary { get { return parentBoundary; } }
        public List<MaterialBlock> ParentBlocks { get { return parentBlocks; } }
        public List<Polyline> FixLines { get { return fixLines; } }

        public void Merge ( DrawingPoint other )
        {
            // merge point fixities
            this.IsFixedX = this.IsFixedX || other.IsFixedX;
            this.IsFixedY = this.IsFixedY || other.IsFixedY;

            // obtain all LineConstraints, LineLoads, and PointLoads containing this point
            List<LineConstraint> thisLCs = new List<LineConstraint>();
            List<LineLoad> thisLLs = new List<LineLoad>();
            List<PointLoad> thisPLs = new List<PointLoad>();
            this.ParentBlocks.ForEach(
                delegate( MaterialBlock mb )
                {
                    thisLCs.AddRange( mb.LineConstraints.FindAll( delegate( LineConstraint lc ) { return !thisLCs.Contains( lc ) && lc.Nodes.Contains( this ); } ) );
                    thisLLs.AddRange( mb.LineLoads.FindAll( delegate( LineLoad ll ) { return !thisLLs.Contains( ll ) && ll.Nodes.Contains( this ); } ) );
                    thisPLs.AddRange( mb.PointLoads.FindAll( delegate( PointLoad pl ) { return !thisPLs.Contains( pl ) && pl.Node == this; } ) );
                } );
            // merge multiple point loads at this point
            for ( int i = thisPLs.Count - 1 ; i >= 1 ; i-- )
            {
                if ( thisPLs[i].IsLoadedX )
                {
                    thisPLs[0].IsLoadedX = true;
                    thisPLs[0].XLoad += thisPLs[i].XLoad;
                }
                if ( thisPLs[i].IsLoadedY )
                {
                    thisPLs[0].IsLoadedY = true;
                    thisPLs[0].YLoad += thisPLs[i].YLoad;
                }

                this.ParentBlocks.ForEach(
                    delegate( MaterialBlock mb ) 
                    { mb.PointLoads.RemoveAll( delegate( PointLoad pl ) { return pl == thisPLs[i]; } ); } );
                thisPLs[i].Delete();
                thisPLs.RemoveAt( i );
            }
            PointLoad thisPL = null;
            if ( thisPLs.Count > 0 ) thisPL = thisPLs[0];
            
            // obtain all LineConstraints, LineLoads, and PointLoads containing other point
            List<LineConstraint> otherLCs = new List<LineConstraint>();
            List<LineLoad> otherLLs = new List<LineLoad>();
            List<PointLoad> otherPLs = new List<PointLoad>();
            other.ParentBlocks.ForEach(
                delegate( MaterialBlock mb )
                {
                    otherLCs.AddRange( mb.LineConstraints.FindAll( delegate( LineConstraint lc ) { return !otherLCs.Contains( lc ) && lc.Nodes.Contains( other ); } ) );
                    otherLLs.AddRange( mb.LineLoads.FindAll( delegate( LineLoad ll ) { return !otherLLs.Contains( ll ) && ll.Nodes.Contains( other ); } ) );
                    otherPLs.AddRange( mb.PointLoads.FindAll( delegate( PointLoad pl ) { return !otherPLs.Contains( pl ) && pl.Node == other; } ) );
                } );
            // merge multiple point loads at other point
            for ( int i = otherPLs.Count - 1 ; i >= 1 ; i-- )
            {
                if ( otherPLs[i].IsLoadedX )
                {
                    otherPLs[0].IsLoadedX = true;
                    otherPLs[0].XLoad += otherPLs[i].XLoad;
                }
                if ( otherPLs[i].IsLoadedY )
                {
                    otherPLs[0].IsLoadedY = true;
                    otherPLs[0].YLoad += otherPLs[i].YLoad;
                }

                other.ParentBlocks.ForEach(
                    delegate( MaterialBlock mb )
                    { mb.PointLoads.RemoveAll( delegate( PointLoad pl ) { return pl == otherPLs[i]; } ); } );
                otherPLs[i].Delete();
                otherPLs.RemoveAt( i );
            }
            PointLoad otherPL = null;
            if ( otherPLs.Count > 0 ) otherPL = otherPLs[0];

            // set nodes currently containing other node to this node
            otherLCs.ForEach(
                delegate( LineConstraint lc )
                {
                    if ( lc.Nodes[0] == other ) lc.Nodes[0] = this;
                    if ( lc.Nodes[1] == other ) lc.Nodes[1] = this;
                } );
            otherLLs.ForEach(
                delegate( LineLoad ll )
                {
                    if ( ll.Nodes[0] == other ) ll.Nodes[0] = this;
                    if ( ll.Nodes[1] == other ) ll.Nodes[1] = this;
                } );

            // merge any LineContraints on a newly shared edge
            thisLCs.ForEach(
                delegate( LineConstraint lc )
                {
                    // get point that is not this point
                    DrawingPoint otherPt = lc.Nodes.Find( delegate( DrawingPoint p ) { return p != this; } );
                    if ( otherPt != null )
                    {
                        // check for LineConstraints former of other node that contain otherPt
                        otherLCs.ForEach(
                            delegate( LineConstraint otherLC )
                            {
                                if ( otherLC.Nodes.Contains( otherPt ) )
                                {
                                    // merge fixities
                                    lc.IsFixedX = lc.IsFixedX || otherLC.IsFixedX;
                                    lc.IsFixedY = lc.IsFixedY || otherLC.IsFixedY;

                                    // replace the LineConstraint in other points parent blocks
                                    other.ParentBlocks.ForEach(
                                        delegate( MaterialBlock mb )
                                        {
                                            int index = mb.LineConstraints.FindIndex( delegate( LineConstraint lc0 ) { return lc0 == otherLC; } );
                                            if ( index >= 0 ) mb.LineConstraints[index] = lc;
                                        } );
                                    otherLC.Delete();
                                }
                            } );
                    }
                } );
            // merge any LineLoads on a newly shared edge
            thisLLs.ForEach(
                delegate( LineLoad ll )
                {
                    // get the point that is not this point
                    DrawingPoint otherPt = ll.Nodes.Find( delegate( DrawingPoint p ) { return p != this; } );
                    if ( otherPt != null )
                    {
                        otherLLs.ForEach(
                            delegate( LineLoad otherLL )
                            {
                                if ( otherLL.Nodes.Contains( otherPt ) )
                                {
                                    // merge loads (NB: adjacent edges will always have reversed node order since both have CCW ordered points
                                    if ( otherLL.IsLoadedN )
                                    {
                                        ll.IsLoadedN = true;
                                        ll.NLoad1 -= otherLL.NLoad2;
                                        ll.NLoad2 -= otherLL.NLoad1;
                                    }
                                    if ( otherLL.IsLoadedT )
                                    {
                                        ll.IsLoadedT = true;
                                        ll.TLoad1 -= otherLL.TLoad2;
                                        ll.TLoad2 -= otherLL.TLoad1;
                                    }

                                    // delete the LineLoad formerly containing other point, leaving only a single summed LineLoad
                                    other.ParentBlocks.ForEach(
                                        delegate( MaterialBlock mb )
                                        {
                                            mb.LineLoads.RemoveAll( delegate( LineLoad ll0 ) { return ll0 == otherLL; } );
                                        } );
                                    otherLL.Delete();
                                }
                            } );
                    }
                } );
            // merge PointLoads if both points contained one
            if ( thisPL != null && otherPL != null )
            {
                if ( otherPL.IsLoadedX )
                {
                    thisPL.IsLoadedX = true;
                    thisPL.XLoad += otherPL.XLoad;
                }
                if ( otherPL.IsLoadedY )
                {
                    thisPL.IsLoadedY = true;
                    thisPL.YLoad += otherPL.YLoad;
                }

                // delete PointLoad formerly of other point, leaving only a single summed load
                other.ParentBlocks.ForEach(
                    delegate( MaterialBlock mb ) { mb.PointLoads.RemoveAll( delegate( PointLoad pl ) { return pl == otherPL; } ); } );
                otherPL.Delete();
            }

            foreach ( MaterialBlock mb in other.ParentBlocks )
            {
                // merge the actual points
                int index = mb.BoundaryPoints.FindIndex( delegate( DrawingPoint p ) { return p == other; } );
                mb.BoundaryPoints[index] = this;
                mb.Boundary.Points[index] = this.Point;
                if ( !this.ParentBlocks.Contains( mb ) ) this.ParentBlocks.Add( mb );

                // eliminate oddities such as LineLoads and LineConstraints containing duplicate points
                for ( int i = mb.LineLoads.Count - 1 ; i >= 0 ; i-- )
                {
                    if ( (!mb.LineLoads[i].IsLoadedN && !mb.LineLoads[i].IsLoadedT)
                        || (mb.LineLoads[i].Nodes[0] == this && mb.LineLoads[i].Nodes[1] == this) )
                    {
                        mb.LineLoads[i].Delete();
                        mb.LineLoads.RemoveAt( i );
                    }
                }
                for ( int i = mb.LineConstraints.Count - 1 ; i >= 0 ; i-- )
                {
                    if ( (!mb.LineConstraints[i].IsFixedX && !mb.LineConstraints[i].IsFixedY)
                        || (mb.LineConstraints[i].Nodes[0] == this && mb.LineConstraints[i].Nodes[1] == this) )
                    {
                        mb.LineConstraints[i].Delete();
                        mb.LineConstraints.RemoveAt( i );
                    }
                }
            }

            other.ParentBlocks.Clear();
            other.Delete();
        }

        public void Delete ()
        {
            if ( ParentBoundary != null )
            {
                for ( int i = ParentBoundary.Boundary.Points.Count - 1 ; i >= 0 ; i-- )
                {
                    if ( ParentBoundary.Boundary.Points[i] == this.Point ) ParentBoundary.Boundary.Points.RemoveAt( i );
                }

                canvas.Children.Remove( this.Dot );
                ParentBoundary.BoundaryPoints.Remove( this );

                if ( ParentBoundary.BoundaryPoints.Count <= 2 ) ParentBoundary.Delete();
            }

            if ( ParentBlocks != null )
            {
                ParentBlocks.ForEach(
                    delegate( MaterialBlock mb )
                    {
                        for ( int i = mb.Boundary.Points.Count - 1 ; i >= 0 ; i-- )
                        {
                            if ( mb.Boundary.Points[i] == this.Point ) mb.Boundary.Points.RemoveAt( i );
                        }

                        mb.BoundaryPoints.Remove( this );

                        // check if line constraints contain the node and delete them
                        List<LineConstraint> existingLCs = mb.LineConstraints.FindAll( delegate( LineConstraint lc ) { return lc.Nodes.Contains( this ); } );
                        existingLCs.ForEach( delegate( LineConstraint lc ) { lc.Delete(); mb.LineConstraints.Remove( lc ); } );
                        existingLCs.Clear();

                        // check if line loads contain the node and delete them
                        List<LineLoad> existingLLs = mb.LineLoads.FindAll( delegate( LineLoad ll ) { return ll.Nodes.Contains( this ); } );
                        existingLLs.ForEach( delegate( LineLoad ll ) { ll.Delete(); mb.LineLoads.Remove( ll ); } );
                        existingLLs.Clear();

                        // check if point loads contain the node and delete them
                        List<PointLoad> existingPLs = mb.PointLoads.FindAll( delegate( PointLoad pl ) { return pl.Node == this; } );
                        existingPLs.ForEach( delegate( PointLoad pl ) { pl.Delete(); mb.PointLoads.Remove( pl ); } );
                        existingPLs.Clear();

                        if ( mb.BoundaryPoints.Count <= 2 ) mb.Delete();
                    } );

                canvas.Children.Remove( this.Dot );

                ClearFixLines();
            }
        }

        public void ClearFixLines ()
        {
            fixLines.ForEach( delegate( Polyline line ) { canvas.Children.Remove( line ); } );
            fixLines.Clear();
        }

        public void Translate ( Vector delta )
        {
            point += delta;
            dot.Margin = new Thickness( point.X - 0.5 * dot.Width , point.Y - 0.5 * dot.Height , 0 , 0 );
            Point p;
            foreach ( Polyline l in fixLines )
            {
                p = l.Points[0];
                p += delta;
                l.Points[0] = p;

                p = l.Points[1];
                p += delta;
                l.Points[1] = p;
            }
        }

        public void Zoom ( double factor , Point centre )
        {
            point.X = centre.X + factor * (point.X - centre.X);
            point.Y = centre.Y + factor * (point.Y - centre.Y);
            dot.Margin = new Thickness( point.X - 0.5 * dot.Width , point.Y - 0.5 * dot.Height , 0 , 0 );

            fixLines[0].Points[0] = new Point( point.X - 7 , point.Y - 3.5 );
            fixLines[0].Points[1] = new Point( point.X + 7 , point.Y - 3.5 );

            fixLines[1].Points[0] = new Point( point.X - 7 , point.Y + 3.5 );
            fixLines[1].Points[1] = new Point( point.X + 7 , point.Y + 3.5 );

            fixLines[2].Points[0] = new Point( point.X - 3.5 , point.Y + 7 );
            fixLines[2].Points[1] = new Point( point.X - 3.5 , point.Y - 7 );

            fixLines[3].Points[0] = new Point( point.X + 3.5 , point.Y + 7 );
            fixLines[3].Points[1] = new Point( point.X + 3.5 , point.Y - 7 );
        }

        /// <summary>
        /// Move point independent of other points.
        /// </summary>
        /// <param name="delta">Move vector.</param>
        public void Move ( Vector delta )
        {
            if ( ParentBoundary != null )
            {
                // initialize index of point
                int boundPointIndex = -1;

                // obtain the index from the appropriate parent object
                boundPointIndex = ParentBoundary.Boundary.Points.IndexOf( point );

                // shift the point, its display circle, and its fixity lines
                point += delta;
                dot.Margin = new Thickness( point.X - 0.5 * dot.Width , point.Y - 0.5 * dot.Height , 0 , 0 );
                Point p;
                foreach ( Polyline l in fixLines )
                {
                    p = l.Points[0];
                    p += delta;
                    l.Points[0] = p;

                    p = l.Points[1];
                    p += delta;
                    l.Points[1] = p;
                }

                // update associated polygons, line constraints, line loads, and point loads
                ParentBoundary.Boundary.Points[boundPointIndex] = point;
            }
            else if ( ParentBlocks != null )
            {
                // initialize index of point
                int boundPointIndex = -1;

                // obtain the index from the appropriate parent objects and update
                ParentBlocks.ForEach(
                    delegate( MaterialBlock mb )
                    {
                        boundPointIndex = mb.Boundary.Points.IndexOf( point );
                        mb.Boundary.Points[boundPointIndex] = point + delta;

                        mb.LineConstraints.ForEach(
                        delegate( LineConstraint lc ) { if ( lc.Nodes.Contains( this ) )lc.Update(); } );
                        mb.LineLoads.ForEach(
                            delegate( LineLoad ll ) { if ( ll.Nodes.Contains( this ) )ll.Update(); } );
                        mb.PointLoads.ForEach(
                            delegate( PointLoad pl ) { if ( pl.Node == this )pl.Update(); } );
                    } );

                // shift the point, its display circle, and its fixity lines
                point += delta;
                dot.Margin = new Thickness( point.X - 0.5 * dot.Width , point.Y - 0.5 * dot.Height , 0 , 0 );
                Point p;
                foreach ( Polyline l in fixLines )
                {
                    p = l.Points[0];
                    p += delta;
                    l.Points[0] = p;

                    p = l.Points[1];
                    p += delta;
                    l.Points[1] = p;
                }
            }
        }

        /// <summary>
        /// Override for left-click selection
        /// </summary>
        /// <param name="sender">Reference to sending object.</param>
        /// <param name="e">Mouse event arguments.</param>
        private void MouseLeftButtonDown ( object sender , MouseEventArgs e )
        {
            if ( canvas != null )
            {
                if ( canvas.DrawMode == DrawModes.Select
                    || canvas.DrawMode == DrawModes.AddPoints
                    || canvas.DrawMode == DrawModes.MovePoints
                    || canvas.DrawMode == DrawModes.Fixities
                    || canvas.DrawMode == DrawModes.PointLoad
                    || canvas.DrawMode == DrawModes.LineLoad
                    || canvas.DrawMode == DrawModes.PrintPoint )
                {
                    this.IsSelected = true;
                }
            }
        }

        /// <summary>
        /// Override for left-click selection
        /// </summary>
        /// <param name="sender">Reference to sending object.</param>
        /// <param name="e">Mouse event arguments.</param>
        private void fixLines_MouseLeftButtonUp ( object sender , MouseEventArgs e )
        {
            if ( canvas != null )
            {
                if ( canvas.DrawMode == DrawModes.Select
                    || canvas.DrawMode == DrawModes.LineLoad )
                {
                    // start dialog for user input
                    SetFixityDialog dlg = new SetFixityDialog( canvas , this );
                    dlg.ShowDialog();

                    if ( dlg.DialogResult == true )
                    {
                        canvas.IsSaved = false;
                        canvas.IsVerified = false;
                    }
                }
            }
            else if ( defineCanvas != null )
            {
                ActivateFixityDialog dlg = new ActivateFixityDialog( defineCanvas , this );
                dlg.ShowDialog();
            }
        }
    }

    public class LineConstraint
    {
        private SlopeCanvas canvas;
        private SlopeDefineCanvas defineCanvas;
        private bool isFixedX , isActiveX , isFixedY , isActiveY;
        private List<Polyline> fixLines;
        private List<bool> phaseFixedX , phaseFixedY;

        public LineConstraint ( SlopeCanvas canvas ,
                                DrawingPoint p1 , DrawingPoint p2 ,
                                bool fixX , bool fixY )
        {
            // set parent drawing canvas
            this.canvas = canvas;

            // create list of boundary nodes for the constraint
            Nodes = new List<DrawingPoint>() { p1 , p2 };

            // set constraints on boundary nodes
            Nodes[0].IsFixedX = fixX || Nodes[0].IsFixedX;
            Nodes[0].IsFixedY = fixY || Nodes[0].IsFixedY;
            Nodes[1].IsFixedX = fixX || Nodes[1].IsFixedX;
            Nodes[1].IsFixedY = fixY || Nodes[1].IsFixedY;

            // compute the point at which to plot the constraint
            MidPoint = new Point( 0.5 * (p1.Point.X + p2.Point.X) , 0.5 * (p1.Point.Y + p2.Point.Y) );

            // create plotting lines for constraints
            fixLines = new List<Polyline>();
            Polyline newLine;
            for ( int i = 0 ; i < 4 ; i++ )
            {
                newLine = new Polyline();
                newLine.Visibility = Visibility.Hidden;
                newLine.Fill = Brushes.Blue;
                newLine.Opacity = 1.0;
                newLine.StrokeThickness = 1.5;
                newLine.Stroke = Brushes.Blue;
                fixLines.Add( newLine );
                canvas.Children.Add( newLine );

                newLine.MouseLeftButtonUp += new MouseButtonEventHandler( MouseLeftButtonUp );
            }

            fixLines[0].Points.Add( new Point( MidPoint.X - 7 , MidPoint.Y - 3.5 ) );
            fixLines[0].Points.Add( new Point( MidPoint.X + 7 , MidPoint.Y - 3.5 ) );

            fixLines[1].Points.Add( new Point( MidPoint.X - 7 , MidPoint.Y + 3.5 ) );
            fixLines[1].Points.Add( new Point( MidPoint.X + 7 , MidPoint.Y + 3.5 ) );

            fixLines[2].Points.Add( new Point( MidPoint.X - 3.5 , MidPoint.Y + 7 ) );
            fixLines[2].Points.Add( new Point( MidPoint.X - 3.5 , MidPoint.Y - 7 ) );

            fixLines[3].Points.Add( new Point( MidPoint.X + 3.5 , MidPoint.Y + 7 ) );
            fixLines[3].Points.Add( new Point( MidPoint.X + 3.5 , MidPoint.Y - 7 ) );

            phaseFixedX = new List<bool>();
            phaseFixedY = new List<bool>();

            // set visibility of constraints
            this.IsFixedX = fixX;
            this.IsFixedY = fixY;
        }

        public LineConstraint ( SlopeDefineCanvas canvas ,
                                DrawingPoint p1 , DrawingPoint p2 ,
                                bool fixX , bool fixY )
        {
            // set parent drawing canvas
            this.defineCanvas = canvas;

            // create list of boundary nodes for the constraint
            Nodes = new List<DrawingPoint>() { p1 , p2 };

            // set constraints on boundary nodes
            Nodes[0].IsFixedX = fixX || Nodes[0].IsFixedX;
            Nodes[0].IsFixedY = fixY || Nodes[0].IsFixedY;
            Nodes[1].IsFixedX = fixX || Nodes[1].IsFixedX;
            Nodes[1].IsFixedY = fixY || Nodes[1].IsFixedY;

            // compute the point at which to plot the constraint
            MidPoint = new Point( 0.5 * (p1.Point.X + p2.Point.X) , 0.5 * (p1.Point.Y + p2.Point.Y) );

            // create plotting lines for constraints
            fixLines = new List<Polyline>();
            Polyline newLine;
            for ( int i = 0 ; i < 4 ; i++ )
            {
                newLine = new Polyline();
                newLine.Visibility = Visibility.Hidden;
                newLine.Fill = Brushes.Blue;
                newLine.Opacity = 1.0;
                newLine.StrokeThickness = 1.5;
                newLine.Stroke = Brushes.Blue;
                fixLines.Add( newLine );
                canvas.Children.Add( newLine );

                newLine.MouseLeftButtonUp += new MouseButtonEventHandler( MouseLeftButtonUp );
            }

            fixLines[0].Points.Add( new Point( MidPoint.X - 7 , MidPoint.Y - 3.5 ) );
            fixLines[0].Points.Add( new Point( MidPoint.X + 7 , MidPoint.Y - 3.5 ) );

            fixLines[1].Points.Add( new Point( MidPoint.X - 7 , MidPoint.Y + 3.5 ) );
            fixLines[1].Points.Add( new Point( MidPoint.X + 7 , MidPoint.Y + 3.5 ) );

            fixLines[2].Points.Add( new Point( MidPoint.X - 3.5 , MidPoint.Y + 7 ) );
            fixLines[2].Points.Add( new Point( MidPoint.X - 3.5 , MidPoint.Y - 7 ) );

            fixLines[3].Points.Add( new Point( MidPoint.X + 3.5 , MidPoint.Y + 7 ) );
            fixLines[3].Points.Add( new Point( MidPoint.X + 3.5 , MidPoint.Y - 7 ) );

            phaseFixedX = new List<bool>();
            phaseFixedY = new List<bool>();

            // set visibility of constraints
            this.IsFixedX = fixX;
            this.IsFixedY = fixY;
        }

        public List<DrawingPoint> Nodes { get; set; }
        public List<Polyline> FixLines { get { return this.fixLines; } }

        public Point MidPoint { get; set; }

        public bool IsFixedX
        {
            get { return this.isFixedX; }
            set
            {
                this.isFixedX = value;
                this.IsActiveX = value;

                fixLines[2].Visibility = fixLines[3].Visibility = value ? Visibility.Visible : Visibility.Hidden;

                if ( value )
                {
                    Nodes[0].IsFixedX = value;
                    Nodes[1].IsFixedX = value;
                }
            }
        }

        public bool IsActiveX
        {
            get { return this.isActiveX; }
            set
            {
                this.isActiveX = value;

                fixLines[2].Opacity = fixLines[3].Opacity = value ? 1.0 : 0.2;
            }
        }

        public List<bool> PhaseFixedX { get { return this.phaseFixedX; } }

        public bool IsFixedY
        {
            get
            {
                return this.isFixedY;
            }
            set
            {
                this.isFixedY = value;
                this.IsActiveY = value;

                fixLines[0].Visibility = fixLines[1].Visibility = value ? Visibility.Visible : Visibility.Hidden;

                if ( value )
                {
                    Nodes[0].IsFixedY = value;
                    Nodes[1].IsFixedY = value;
                }
            }
        }

        public bool IsActiveY
        {
            get { return this.isActiveY; }
            set
            {
                this.isActiveY = value;

                fixLines[0].Opacity = fixLines[1].Opacity = value ? 1.0 : 0.2;
            }
        }

        public List<bool> PhaseFixedY { get { return this.phaseFixedY; } }

        public void Update ()
        {
            DrawingPoint p1 = Nodes[0] , p2 = Nodes[1];

            // compute the point at which to plot the constraint
            MidPoint = new Point( 0.5 * (p1.Point.X + p2.Point.X) , 0.5 * (p1.Point.Y + p2.Point.Y) );

            fixLines[0].Points[0] = new Point( MidPoint.X - 7 , MidPoint.Y - 3.5 );
            fixLines[0].Points[1] = new Point( MidPoint.X + 7 , MidPoint.Y - 3.5 );

            fixLines[1].Points[0] = new Point( MidPoint.X - 7 , MidPoint.Y + 3.5 );
            fixLines[1].Points[1] = new Point( MidPoint.X + 7 , MidPoint.Y + 3.5 );

            fixLines[2].Points[0] = new Point( MidPoint.X - 3.5 , MidPoint.Y + 7 );
            fixLines[2].Points[1] = new Point( MidPoint.X - 3.5 , MidPoint.Y - 7 );

            fixLines[3].Points[0] = new Point( MidPoint.X + 3.5 , MidPoint.Y + 7 );
            fixLines[3].Points[1] = new Point( MidPoint.X + 3.5 , MidPoint.Y - 7 );
        }

        public void Delete ()
        {
            fixLines.ForEach( delegate( Polyline line ) { canvas.Children.Remove( line ); } );
            fixLines.Clear();
        }

        /// <summary>
        /// Override for left-click selection
        /// </summary>
        /// <param name="sender">Reference to sending object.</param>
        /// <param name="e">Mouse event arguments.</param>
        private void MouseLeftButtonUp ( object sender , MouseEventArgs e )
        {
            if ( canvas != null )
            {
                if ( canvas.DrawMode == DrawModes.Select
                    || canvas.DrawMode == DrawModes.LineLoad )
                {
                    // start dialog for user input
                    SetFixityDialog dlg = new SetFixityDialog( canvas , this );
                    dlg.ShowDialog();

                    // if there is no load in horizontal or vertical direction, delete the load ...
                    if ( !this.IsFixedX && !this.IsFixedY )
                    {
                        this.Delete();

                        foreach ( MaterialBlock mb in canvas.MaterialBlocks )
                        {
                            mb.LineConstraints.RemoveAll( delegate( LineConstraint lc ) { return lc == this; } );
                        }
                    }

                    // ... otherwise update its visibility and plotting location
                    else
                    {
                        this.Update();
                    }

                    if ( dlg.DialogResult == true )
                    {
                        canvas.IsSaved = false;
                        canvas.IsVerified = false;
                    }
                }
            }
            else if ( defineCanvas != null )
            {
                ActivateFixityDialog dlg = new ActivateFixityDialog( defineCanvas , this );
                dlg.ShowDialog();

                this.Update();
            }
        }
    }


    public class PointLoad
    {
        private SlopeCanvas canvas;
        private SlopeDefineCanvas defineCanvas;
        private bool isLoadedX , isActiveX , isLoadedY , isActiveY;
        private List<Polyline> loadLines;
        private List<bool> phaseActiveX , phaseActiveY;
        private List<double> phaseFactorX , phaseFactorY;
        private static double Cpos = Math.Cos( 0.75 * Math.PI ) ,
                                Spos = Math.Sin( 0.75 * Math.PI ) ,
                                Cneg = Cpos ,
                                Sneg = -Spos;

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="canvas">Parent drawing canvas</param>
        /// <param name="node">Parent node</param>
        /// <param name="isLoadedX">Is load applied in the horizontal direction?</param>
        /// <param name="xLoad">Value of horizontal load</param>
        /// <param name="isLoadedY">Is load applied in the vertical direction?</param>
        /// <param name="yLoad">Value of vertical load</param>
        public PointLoad ( SlopeCanvas canvas , DrawingPoint node ,
                                bool isLoadedX , double xLoad ,
                                bool isLoadedY , double yLoad )
        {
            // set parent drawing canvas
            this.canvas = canvas;

            // set parent node
            this.Node = node;

            // create plotting lines for constraints
            loadLines = new List<Polyline>();
            Polyline newLine;
            for ( int i = 0 ; i < 6 ; i++ )
            {
                newLine = new Polyline();
                newLine.Visibility = Visibility.Hidden;
                newLine.Fill = Brushes.Blue;
                newLine.Opacity = 1.0;
                newLine.StrokeThickness = 1.75;
                newLine.Stroke = Brushes.Blue;
                newLine.Points.Add( new Point() );
                newLine.Points.Add( new Point() );
                loadLines.Add( newLine );
                canvas.Children.Add( newLine );

                newLine.MouseLeftButtonUp += new MouseButtonEventHandler( MouseLeftButtonUp );
            }

            // set load state
            this.IsLoadedX = isLoadedX;
            if ( this.IsLoadedX ) this.XFactor = 1.0;
            this.XLoad = xLoad;
            this.IsLoadedY = isLoadedY;
            if ( this.IsLoadedY ) this.YFactor = 1.0;
            this.YLoad = yLoad;

            // Initialize analysis phase lists
            phaseActiveX = new List<bool>();
            phaseActiveY = new List<bool>();
            phaseFactorX = new List<double>();
            phaseFactorY = new List<double>();

            Update();
        }

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="canvas">Parent drawing canvas</param>
        /// <param name="node">Parent node</param>
        /// <param name="isLoadedX">Is load applied in the horizontal direction?</param>
        /// <param name="xLoad">Value of horizontal load</param>
        /// <param name="isLoadedY">Is load applied in the vertical direction?</param>
        /// <param name="yLoad">Value of vertical load</param>
        public PointLoad ( SlopeDefineCanvas canvas , DrawingPoint node ,
                                bool isLoadedX , double xLoad ,
                                bool isLoadedY , double yLoad )
        {
            // set parent drawing canvas
            this.defineCanvas = canvas;

            // set parent node
            this.Node = node;

            // create plotting lines for constraints
            loadLines = new List<Polyline>();
            Polyline newLine;
            for ( int i = 0 ; i < 6 ; i++ )
            {
                newLine = new Polyline();
                newLine.Visibility = Visibility.Hidden;
                newLine.Fill = Brushes.Blue;
                newLine.Opacity = 1.0;
                newLine.StrokeThickness = 1.75;
                newLine.Stroke = Brushes.Blue;
                newLine.Points.Add( new Point() );
                newLine.Points.Add( new Point() );
                loadLines.Add( newLine );
                canvas.Children.Add( newLine );

                newLine.MouseLeftButtonUp += new MouseButtonEventHandler( MouseLeftButtonUp );
            }

            // set load state
            this.IsLoadedX = isLoadedX;
            if ( this.IsLoadedX ) this.XFactor = 1.0;
            this.XLoad = xLoad;
            this.IsLoadedY = isLoadedY;
            if ( this.IsLoadedY ) this.YFactor = 1.0;
            this.YLoad = yLoad;

            // Initialize analysis phase lists
            phaseActiveX = new List<bool>();
            phaseActiveY = new List<bool>();
            phaseFactorX = new List<double>();
            phaseFactorY = new List<double>();

            Update();
        }

        /// <summary>
        /// Parent node property.
        /// </summary>
        public DrawingPoint Node { get; set; }

        /// <summary>
        /// List of plotting lines property.
        /// </summary>
        public List<Polyline> LoadLines { get { return this.loadLines; } }

        public double XFactor { get; set; }
        public double YFactor { get; set; }

        public List<double> PhaseFactorX { get { return this.phaseFactorX; } }
        public List<double> PhaseFactorY { get { return this.phaseFactorY; } }

        /// <summary>
        /// Properties indicating whether a load is applied.
        /// </summary>
        public bool IsLoadedX 
        { 
            get { return this.isLoadedX; }
            set
            {
                this.isLoadedX = value;
                this.IsActiveX = value;
                if ( !value ) this.XLoad = 0.0;
                this.Update();
            }
        }
        public bool IsActiveX
        {
            get { return this.isActiveX; }
            set
            {
                this.isActiveX = value;
                if ( !this.IsActiveX ) XFactor = 0.0;
                this.Update();
            }
        }
        public List<bool> PhaseActiveX { get { return this.phaseActiveX; } }
        public bool IsLoadedY 
        { 
            get { return this.isLoadedY; }
            set
            {
                this.isLoadedY = value;
                this.IsActiveY = value;
                if ( !value ) this.YLoad = 0.0;
                this.Update();
            }
        }
        public bool IsActiveY
        {
            get { return this.isActiveY; }
            set
            {
                this.isActiveY = value;
                if ( !this.IsActiveY ) YFactor = 0.0;
                this.Update();
            }
        }
        public List<bool> PhaseActiveY { get { return this.phaseActiveY; } }

        /// <summary>
        /// Horizontal load value.
        /// </summary>
        public double XLoad { get; set; }

        /// <summary>
        /// Vertical load value.
        /// </summary>
        public double YLoad { get; set; }


        /// <summary>
        /// Function for applying loads.
        /// </summary>
        /// <param name="isLoadedX">Is load applied in the horizontal direction?</param>
        /// <param name="xLoad">Value of horizontal load</param>
        /// <param name="isLoadedY">Is load applied in the vertical direction?</param>
        /// <param name="yLoad">Value of vertical load</param>
        public void ApplyLoad ( bool isLoadedX , double xLoad ,
                                bool isLoadedY , double yLoad )
        {
            this.IsLoadedX = isLoadedX;
            if ( IsLoadedX ) this.XLoad = xLoad;

            this.IsLoadedY = isLoadedY;
            if ( IsLoadedY ) this.YLoad = yLoad;
        }

        /// <summary>
        /// Updates load line visibility and location
        /// </summary>
        public void Update ()
        {
            // for applying rotations
            double xprime , yprime;
            bool posXLoad = XLoad >= 0 , posYLoad = YLoad >= 0;

            // horizontal load arrow shaft
            loadLines[0].Points[0] = new Point( Node.Point.X , Node.Point.Y );
            loadLines[0].Points[1] = loadLines[0].Points[0] + new Vector( 40 , 0 );
            // horizontal load arrow head 1
            loadLines[1].Points[0] = posXLoad ? loadLines[0].Points[1] : loadLines[0].Points[0];
            loadLines[1].Points[1] = posXLoad ? new Point( 12 , 0 ) : new Point( -12 , 0 );
            xprime = loadLines[1].Points[1].X * Cpos - loadLines[1].Points[1].Y * Spos + loadLines[1].Points[0].X;
            yprime = loadLines[1].Points[1].X * Spos + loadLines[1].Points[1].Y * Cpos + loadLines[1].Points[0].Y;
            loadLines[1].Points[1] = new Point( xprime , yprime );
            // horizontal load arrow head 2
            loadLines[2].Points[0] = posXLoad ? loadLines[0].Points[1] : loadLines[0].Points[0];
            loadLines[2].Points[1] = posXLoad ? new Point( 12 , 0 ) : new Point( -12 , 0 );
            xprime = loadLines[2].Points[1].X * Cneg - loadLines[2].Points[1].Y * Sneg + loadLines[2].Points[0].X;
            yprime = loadLines[2].Points[1].X * Sneg + loadLines[2].Points[1].Y * Cneg + loadLines[2].Points[0].Y;
            loadLines[2].Points[1] = new Point( xprime , yprime );

            // vertical load arrow shaft
            loadLines[3].Points[0] = new Point( Node.Point.X , Node.Point.Y );
            loadLines[3].Points[1] = loadLines[3].Points[0] + new Vector( 0 , -40 );
            // vertical load arrow head 1
            loadLines[4].Points[0] = posYLoad ? loadLines[3].Points[1] : loadLines[3].Points[0];
            loadLines[4].Points[1] = posYLoad ? new Point( 0 , -12 ) : new Point( 0 , 12 );
            xprime = loadLines[4].Points[1].X * Cpos - loadLines[4].Points[1].Y * Spos + loadLines[4].Points[0].X;
            yprime = loadLines[4].Points[1].X * Spos + loadLines[4].Points[1].Y * Cpos + loadLines[4].Points[0].Y;
            loadLines[4].Points[1] = new Point( xprime , yprime );
            // vertical load arrow head 2
            loadLines[5].Points[0] = posYLoad ? loadLines[3].Points[1] : loadLines[3].Points[0];
            loadLines[5].Points[1] = posYLoad ? new Point( 0 , -12 ) : new Point( 0 , 12 );
            xprime = loadLines[5].Points[1].X * Cneg - loadLines[5].Points[1].Y * Sneg + loadLines[5].Points[0].X;
            yprime = loadLines[5].Points[1].X * Sneg + loadLines[5].Points[1].Y * Cneg + loadLines[5].Points[0].Y;
            loadLines[5].Points[1] = new Point( xprime , yprime );

            int i = 0;
            for ( ; i < 3 ; i++ )
            {
                loadLines[i].Visibility = IsLoadedX ? Visibility.Visible : Visibility.Hidden;
                loadLines[i].Opacity = IsActiveX ? 1.0 : 0.2;
            }
            for ( ; i < 6 ; i++ )
            {
                loadLines[i].Visibility = IsLoadedY ? Visibility.Visible : Visibility.Hidden;
                loadLines[i].Opacity = IsActiveY ? 1.0 : 0.2;
            }
        }

        public void Delete ()
        {
            loadLines.ForEach( delegate( Polyline line ) { canvas.Children.Remove( line ); } );
            loadLines.Clear();
        }

        /// <summary>
        /// Override for left-click selection
        /// </summary>
        /// <param name="sender">Reference to sending object.</param>
        /// <param name="e">Mouse event arguments.</param>
        private void MouseLeftButtonUp ( object sender , MouseEventArgs e )
        {
            if ( canvas != null )
            {
                if ( canvas.DrawMode == DrawModes.Select
                    || canvas.DrawMode == DrawModes.PointLoad )
                {
                    // start dialog for user input
                    AddPointLoadDialog dlg = new AddPointLoadDialog( canvas , this );
                    dlg.ShowDialog();

                    // if there is no load in horizontal or vertical direction, delete the load ...
                    if ( !this.IsLoadedX && !this.IsLoadedY )
                    {
                        this.Delete();

                        MaterialBlock parent = null;
                        foreach ( MaterialBlock mb in canvas.MaterialBlocks )
                        {
                            if ( mb.PointLoads.Contains( this ) )
                            {
                                parent = mb;
                                break;
                            }
                        }
                        if ( parent != null ) parent.PointLoads.Remove( this );
                    }

                    // ... otherwise update its visibility and plotting location
                    else
                    {
                        this.Update();
                    }

                    if ( dlg.DialogResult == true )
                    {
                        canvas.IsSaved = false;
                        canvas.IsVerified = false;
                    }
                }
            }
            else if ( defineCanvas != null )
            {
                FactorPointLoadDialog dlg = new FactorPointLoadDialog( defineCanvas , this );
                dlg.ShowDialog();

                this.Update();
            }
        }
    }


    /// <summary>
    /// LineLoad - Class for defining linearly varying loads between two adjacent nodes.
    /// </summary>
    public class LineLoad
    {
        private SlopeCanvas canvas;
        private SlopeDefineCanvas defineCanvas;
        private bool isLoadedN , isActiveN , isLoadedT , isActiveT;
        private List<Polyline> loadLines;
        private static double Cpos = Math.Cos( 0.75 * Math.PI ) ,
                                Spos = Math.Sin( 0.75 * Math.PI ) ,
                                Cneg = Cpos ,
                                Sneg = -Spos;

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="canvas">Parent drawing canvas</param>
        /// <param name="p1">Node 1 (assumed to be sorted CCW)</param>
        /// <param name="p2">Node 2 (assumed to be sorted CCW)</param>
        /// <param name="isLoadedN">Is load applied in the normal direction?</param>
        /// <param name="nLoad1">Value of normal load at node 1.</param>
        /// <param name="nLoad2">Value of normal load at node 2.</param>
        /// <param name="isLoadedT">Is load applied in the tangential direction?</param>
        /// <param name="tLoad1">Value of tangential load at node 1.</param>
        /// <param name="tLoad2">Value of tangential load at node 2.</param>
        public LineLoad ( SlopeCanvas canvas ,
                                DrawingPoint p1 , DrawingPoint p2 ,
                                bool isLoadedN ,
                                double nLoad1 , double nLoad2 ,
                                bool isLoadedT ,
                                double tLoad1 , double tLoad2 )
        {
            // set parent drawing canvas
            this.canvas = canvas;

            // create list of boundary nodes for the load
            Nodes = new List<DrawingPoint>() { p1 , p2 };

            PlotPoints = new List<Point>() { new Point() , new Point() , new Point() };

            // create plotting lines for loads
            loadLines = new List<Polyline>();
            Polyline newLine;
            for ( int i = 0 ; i < 18 ; i++ )
            {
                newLine = new Polyline();
                newLine.Visibility = Visibility.Hidden;
                newLine.Fill = Brushes.Blue;
                newLine.Opacity = 1.0;
                newLine.StrokeThickness = 1.25;
                newLine.Stroke = Brushes.Blue;
                newLine.Points.Add( new Point() );
                newLine.Points.Add( new Point() );
                loadLines.Add( newLine );
                canvas.Children.Add( newLine );

                newLine.MouseLeftButtonUp += new MouseButtonEventHandler( MouseLeftButtonUp );
            }

            // set load state
            this.IsLoadedN = isLoadedN;
            if ( this.IsLoadedN ) this.NFactor = 1.0;
            this.NLoad1 = nLoad1;
            this.NLoad2 = nLoad2;
            this.IsLoadedT = isLoadedT;
            if ( this.IsLoadedT ) this.TFactor = 1.0;
            this.TLoad1 = tLoad1;
            this.TLoad2 = tLoad2;

            Update();
        }

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="canvas">Parent drawing canvas</param>
        /// <param name="p1">Node 1 (assumed to be sorted CCW)</param>
        /// <param name="p2">Node 2 (assumed to be sorted CCW)</param>
        /// <param name="isLoadedN">Is load applied in the normal direction?</param>
        /// <param name="nLoad1">Value of normal load at node 1.</param>
        /// <param name="nLoad2">Value of normal load at node 2.</param>
        /// <param name="isLoadedT">Is load applied in the tangential direction?</param>
        /// <param name="tLoad1">Value of tangential load at node 1.</param>
        /// <param name="tLoad2">Value of tangential load at node 2.</param>
        public LineLoad ( SlopeDefineCanvas canvas ,
                                DrawingPoint p1 , DrawingPoint p2 ,
                                bool isLoadedN ,
                                double nLoad1 , double nLoad2 ,
                                bool isLoadedT ,
                                double tLoad1 , double tLoad2 )
        {
            // set parent drawing canvas
            this.defineCanvas = canvas;

            // create list of boundary nodes for the load
            Nodes = new List<DrawingPoint>() { p1 , p2 };

            PlotPoints = new List<Point>() { new Point() , new Point() , new Point() };

            // create plotting lines for loads
            loadLines = new List<Polyline>();
            Polyline newLine;
            for ( int i = 0 ; i < 18 ; i++ )
            {
                newLine = new Polyline();
                newLine.Visibility = Visibility.Hidden;
                newLine.Fill = Brushes.Blue;
                newLine.Opacity = 1.0;
                newLine.StrokeThickness = 1.25;
                newLine.Stroke = Brushes.Blue;
                newLine.Points.Add( new Point() );
                newLine.Points.Add( new Point() );
                loadLines.Add( newLine );
                canvas.Children.Add( newLine );

                newLine.MouseLeftButtonUp += new MouseButtonEventHandler( MouseLeftButtonUp );
            }

            // set load state
            this.IsLoadedN = isLoadedN;
            if ( this.IsLoadedN ) this.NFactor = 1.0;
            this.NLoad1 = nLoad1;
            this.NLoad2 = nLoad2;
            this.TFactor = 1.0;
            this.IsLoadedT = isLoadedT;
            if ( this.IsLoadedT ) this.TFactor = 1.0;
            this.TLoad1 = tLoad1;
            this.TLoad2 = tLoad2;

            Update();
        }

        /// <summary>
        /// List of nodes property.
        /// </summary>
        public List<DrawingPoint> Nodes { get; set; }

        /// <summary>
        /// List of plotting lines property.
        /// </summary>
        public List<Polyline> LoadLines { get { return this.loadLines; } }

        /// <summary>
        /// Property for plotting location of load lines.
        /// </summary>
        public List<Point> PlotPoints { get; set; }

        /// <summary>
        /// Properties indicating whether a load is applied.
        /// </summary>
        public bool IsLoadedN 
        { 
            get { return this.isLoadedN; }
            set
            {
                this.isLoadedN = value;
                this.IsActiveN = value;
                if ( !value ) NLoad1 = NLoad2 = 0.0;
                this.Update();
            }
        }
        public bool IsActiveN
        {
            get { return this.isActiveN; }
            set
            {
                this.isActiveN = value;
                if ( !this.IsActiveN ) NFactor = 0.0;
                this.Update();
            }
        }
        public bool IsLoadedT
        {
            get { return this.isLoadedT; }
            set
            {
                this.isLoadedT = value;
                this.IsActiveT = value;
                if ( !value ) TLoad1 = TLoad2 = 0.0;
                this.Update();
            }
        }
        public bool IsActiveT
        {
            get { return this.isActiveT; }
            set
            {
                this.isActiveT = value;
                if ( !this.IsActiveT ) TFactor = 0.0;
                this.Update();
            }
        }

        public double NFactor { get; set; }
        public double TFactor { get; set; }

        /// <summary>
        /// Normal load values.
        /// </summary>
        public double NLoad1 { get; set; }
        public double NLoad2 { get; set; }

        /// <summary>
        /// Tangential load values.
        /// </summary>
        public double TLoad1 { get; set; }
        public double TLoad2 { get; set; }


        /// <summary>
        /// Function for applying loads.
        /// </summary>
        /// <param name="isLoadedN">Is a normal load applied?</param>
        /// <param name="nLoad1">Value of normal load at node 1.</param>
        /// <param name="nLoad2">Value of normal load at node 2.</param>
        /// <param name="isLoadedT">Is a tangential load applied?</param>
        /// <param name="tLoad1">Value of tangential load at node 1.</param>
        /// <param name="tLoad2">Value of tangential load at node 2.</param>
        public void ApplyLoad ( bool isLoadedN ,
                                double nLoad1 , double nLoad2 ,
                                bool isLoadedT ,
                                double tLoad1 , double tLoad2 )
        {
            this.IsLoadedN = isLoadedN;
            if ( IsLoadedN )
            {
                this.NLoad1 = nLoad1;
                this.NLoad2 = nLoad2;
            }

            this.IsLoadedT = isLoadedT;
            if ( IsLoadedT )
            {
                this.TLoad1 = tLoad1;
                this.TLoad2 = tLoad2;
            }

            Update();
        }

        /// <summary>
        /// Updates load line visibility and location
        /// </summary>
        public void Update ()
        {
            // update plotting points
            for ( int i = 0 ; i < 3 ; i++ ) PlotPoints[i] = Nodes[0].Point + (i + 1) * 0.25 * (Nodes[1].Point - Nodes[0].Point);

            // unit tangential and normal vectors
            Vector tang = Nodes[1].Point - Nodes[0].Point ,
                norm = new Vector( -tang.Y , tang.X );
            tang /= tang.Length;
            norm /= norm.Length;

            int i0 , i1 , i2 , i3 , i4 , i5;
            Point plotPoint;
            double xprime , yprime;
            bool posNLoad , posTLoad;
            for ( int i = 0 ; i < 3 ; i++ )
            {
                // load line indices
                i0 = 6 * i; i1 = i0 + 1; i2 = i0 + 2; i3 = i0 + 3; i4 = i0 + 4; i5 = i0 + 5;

                // point about which to plot the load lines
                plotPoint = PlotPoints[i] + 15 * norm;

                // arrow direction indicator
                posNLoad = (NLoad1 + (i + 1) * 0.25 * (NLoad2 - NLoad1)) >= 0;
                posTLoad = (TLoad1 + (i + 1) * 0.25 * (TLoad2 - TLoad1)) >= 0;

                // normal load arrow shaft
                loadLines[i0].Points[0] = plotPoint - 10 * norm;
                loadLines[i0].Points[1] = plotPoint + 10 * norm;
                loadLines[i0].Visibility = IsLoadedN ? Visibility.Visible : Visibility.Hidden;
                loadLines[i0].Opacity = IsActiveN ? 1.0 : 0.2;
                // normal load arrow head 1
                loadLines[i1].Points[0] = posNLoad ? loadLines[i0].Points[1] : loadLines[i0].Points[0];
                loadLines[i1].Points[1] = posNLoad ? (Point) (6 * norm) : (Point) (-6 * norm);
                xprime = loadLines[i1].Points[1].X * Cpos - loadLines[i1].Points[1].Y * Spos + loadLines[i1].Points[0].X;
                yprime = loadLines[i1].Points[1].X * Spos + loadLines[i1].Points[1].Y * Cpos + loadLines[i1].Points[0].Y;
                loadLines[i1].Points[1] = new Point( xprime , yprime );
                loadLines[i1].Visibility = IsLoadedN ? Visibility.Visible : Visibility.Hidden;
                loadLines[i1].Opacity = IsActiveN ? 1.0 : 0.2;
                // normal load arrow head 2
                loadLines[i2].Points[0] = posNLoad ? loadLines[i0].Points[1] : loadLines[i0].Points[0];
                loadLines[i2].Points[1] = posNLoad ? (Point) (6 * norm) : (Point) (-6 * norm);
                xprime = loadLines[i2].Points[1].X * Cneg - loadLines[i2].Points[1].Y * Sneg + loadLines[i2].Points[0].X;
                yprime = loadLines[i2].Points[1].X * Sneg + loadLines[i2].Points[1].Y * Cneg + loadLines[i2].Points[0].Y;
                loadLines[i2].Points[1] = new Point( xprime , yprime );
                loadLines[i2].Visibility = IsLoadedN ? Visibility.Visible : Visibility.Hidden;
                loadLines[i2].Opacity = IsActiveN ? 1.0 : 0.2;

                // tangential load arrow shaft
                loadLines[i3].Points[0] = plotPoint - 10 * tang;
                loadLines[i3].Points[1] = plotPoint + 10 * tang;
                loadLines[i3].Visibility = IsLoadedT ? Visibility.Visible : Visibility.Hidden;
                loadLines[i3].Opacity = IsActiveT ? 1.0 : 0.2;
                // tangential load arrow head 1
                loadLines[i4].Points[0] = posTLoad ? loadLines[i3].Points[1] : loadLines[i3].Points[0];
                loadLines[i4].Points[1] = posTLoad ? (Point) (6 * tang) : (Point) (-6 * tang);
                xprime = loadLines[i4].Points[1].X * Cpos - loadLines[i4].Points[1].Y * Spos + loadLines[i4].Points[0].X;
                yprime = loadLines[i4].Points[1].X * Spos + loadLines[i4].Points[1].Y * Cpos + loadLines[i4].Points[0].Y;
                loadLines[i4].Points[1] = new Point( xprime , yprime );
                loadLines[i4].Visibility = IsLoadedT ? Visibility.Visible : Visibility.Hidden;
                loadLines[i4].Opacity = IsActiveT ? 1.0 : 0.2;
                // tangential load arrow head 2
                loadLines[i5].Points[0] = posTLoad ? loadLines[i3].Points[1] : loadLines[i3].Points[0];
                loadLines[i5].Points[1] = posTLoad ? (Point) (6 * tang) : (Point) (-6 * tang);
                xprime = loadLines[i5].Points[1].X * Cneg - loadLines[i5].Points[1].Y * Sneg + loadLines[i5].Points[0].X;
                yprime = loadLines[i5].Points[1].X * Sneg + loadLines[i5].Points[1].Y * Cneg + loadLines[i5].Points[0].Y;
                loadLines[i5].Points[1] = new Point( xprime , yprime );
                loadLines[i5].Visibility = IsLoadedT ? Visibility.Visible : Visibility.Hidden;
                loadLines[i5].Opacity = IsActiveT ? 1.0 : 0.2;
            }
        }

        public void Delete ()
        {
            loadLines.ForEach( delegate( Polyline line ) { canvas.Children.Remove( line ); } );
            loadLines.Clear();
        }

        /// <summary>
        /// Override for left-click selection
        /// </summary>
        /// <param name="sender">Reference to sending object.</param>
        /// <param name="e">Mouse event arguments.</param>
        private void MouseLeftButtonUp ( object sender , MouseEventArgs e )
        {
            if ( canvas != null )
            {
                if ( canvas.DrawMode == DrawModes.Select
                    || canvas.DrawMode == DrawModes.LineLoad )
                {
                    // start dialog for user input
                    AddLineLoadDialog dlg = new AddLineLoadDialog( canvas , this );
                    dlg.ShowDialog();

                    // if there is no load in horizontal or vertical direction, delete the load ...
                    if ( !this.IsLoadedN && !this.IsLoadedT )
                    {
                        this.Delete();

                        MaterialBlock parent = null;
                        foreach ( MaterialBlock mb in canvas.MaterialBlocks )
                        {
                            if ( mb.LineLoads.Contains( this ) )
                            {
                                parent = mb;
                                break;
                            }
                        }
                        if ( parent != null ) parent.LineLoads.Remove( this );
                    }

                    // ... otherwise update its visibility and plotting location
                    else
                    {
                        this.Update();
                    }

                    if ( dlg.DialogResult == true )
                    {
                        canvas.IsSaved = false;
                        canvas.IsVerified = false;
                    }
                }
            }
            else if ( defineCanvas != null )
            {
                FactorLineLoadDialog dlg = new FactorLineLoadDialog( defineCanvas , this );
                dlg.ShowDialog();

                this.Update();
            }
        }
    }

    public class DisplacementVector
    {
        private SlopePlotCanvas canvas;
        private List<Polyline> plotLines;
        private static double Cpos = Math.Cos( 0.75 * Math.PI ) ,
                                Spos = Math.Sin( 0.75 * Math.PI ) ,
                                Cneg = Cpos ,
                                Sneg = -Spos;
        private Point plotPoint;
        private List<double> disps;
        private Vector dir;
        private double magnitude;

        /// <summary>
        /// Constructor
        /// </summary>
        /// <param name="canvas">Parent drawing canvas</param>
        public DisplacementVector ( SlopePlotCanvas canvas, Point plotPoint,List<double>disps )
        {
            // set parent drawing canvas
            this.canvas = canvas;

            // set plotting location and disp values
            this.plotPoint = plotPoint;
            this.disps = disps;

            // get magnitude and direction of displacement vector
            this.dir = new Vector( disps[0] , -disps[1] );
            this.magnitude = dir.Length;
            this.dir /= magnitude;

            // create plotting lines for constraints
            plotLines = new List<Polyline>();
            Polyline newLine;
            for ( int i = 0 ; i < 3 ; i++ )
            {
                newLine = new Polyline();
                newLine.StrokeStartLineCap = PenLineCap.Triangle;
                newLine.StrokeEndLineCap = PenLineCap.Triangle;
                newLine.Fill = Brushes.Red;
                newLine.Opacity = 1.0;
                newLine.StrokeThickness = 1.0;
                newLine.Stroke = Brushes.Red;
                newLine.Points.Add( new Point() );
                newLine.Points.Add( new Point() );
                plotLines.Add( newLine );
            }

            Update();
        }

        public List<Polyline> PlotLines { get { return this.plotLines; } }

        public void Translate ( Vector delta )
        {
            plotPoint += delta;
            Update();
        }

        public void Zoom ( double factor , Point centre )
        {
            plotPoint = centre + factor * (plotPoint - centre);
            Update();
        }

        public void Update ()
        {
            // get units dependent scaling factor
            double factor;
            switch ( canvas.Units )
            {
                case Units.Metres: factor = 0.0254; break;
                case Units.Millimetres: factor = 25.4; break;
                case Units.Feet: factor = 1.0 / 12.0; break;
                default: factor = 1.0; break;
            }

            double scaleFactor = canvas.Magnification * magnitude / (canvas.Scale * factor) * canvas.DpiX;
            double arrowSize = Math.Min( 5 , 0.5 * scaleFactor );
            Point headPoint = plotPoint + scaleFactor * dir;

            double xprime , yprime;

            // disp arrow shaft
            plotLines[0].Points[0] = plotPoint;
            plotLines[0].Points[1] = headPoint;
            // disp arrow head 1
            plotLines[1].Points[0] = headPoint;
            plotLines[1].Points[1] = (Point) (arrowSize * dir);
            xprime = plotLines[1].Points[1].X * Cpos - plotLines[1].Points[1].Y * Spos + plotLines[1].Points[0].X;
            yprime = plotLines[1].Points[1].X * Spos + plotLines[1].Points[1].Y * Cpos + plotLines[1].Points[0].Y;
            plotLines[1].Points[1] = new Point( xprime , yprime );
            // disp arrow head 2
            plotLines[2].Points[0] = headPoint;
            plotLines[2].Points[1] = (Point) (arrowSize * dir);
            xprime = plotLines[2].Points[1].X * Cneg - plotLines[2].Points[1].Y * Sneg + plotLines[2].Points[0].X;
            yprime = plotLines[2].Points[1].X * Sneg + plotLines[2].Points[1].Y * Cneg + plotLines[2].Points[0].Y;
            plotLines[2].Points[1] = new Point( xprime , yprime );
        }
    }


    public class ZoomRect
    {
        public ZoomRect ()
        {
            Boundary = new Polygon();
            Boundary.Stroke = Brushes.Black;
            Boundary.Fill = Brushes.LightBlue;
            Boundary.Opacity = 0.3;
            Boundary.Visibility = Visibility.Visible;
        }

        public Polygon Boundary { get; set; }
    }

    public class MaterialBlock
    {
        private SlopeCanvas canvas;
        private SlopePlotCanvas plotCanvas;
        private SlopeDefineCanvas defineCanvas;
        private bool isSelected;
        private MaterialType material;
        private List<DrawingPoint> boundaryPoints;
        private List<LineConstraint> lineConstraints;
        private List<LineLoad> lineLoads;
        private List<PointLoad> pointLoads;
        private static SolidColorBrush selectFill;
        private List<MaterialType> phaseMaterials;

        public MaterialBlock ( SlopeCanvas canvas , Point[] pts )
        {
            if ( selectFill == null )
            {
                Color selectColour = new Color();
                selectColour = new Color();
                selectColour.A = 20;
                selectColour.R = 200;
                selectColour.G = 0;
                selectColour.B = 0;
                selectFill = new SolidColorBrush( selectColour );
            }

            this.canvas = canvas;

            Boundary = new Polygon();
            boundaryPoints = new List<DrawingPoint>();
            Boundary.Stroke = Brushes.Black;
            Boundary.StrokeThickness = 1.1;
            Boundary.StrokeLineJoin = PenLineJoin.Round;
            Boundary.StrokeStartLineCap = PenLineCap.Round;
            Boundary.StrokeEndLineCap = PenLineCap.Round;
            Boundary.Fill = Brushes.WhiteSmoke;
            Boundary.Opacity = 0.8;
            Boundary.MouseLeftButtonDown += new MouseButtonEventHandler( this.MouseLeftButtonDown );
            Boundary.Visibility = Visibility.Visible;

            canvas.Children.Add( Boundary );

            for ( int i = 0 ; i < pts.Length ; i++ )
            {
                // check if point is same as existing point
                bool foundPoint = false;
                foreach ( MaterialBlock mb in canvas.MaterialBlocks )
                {
                    foreach ( DrawingPoint p in mb.BoundaryPoints )
                    {
                        if ( (pts[i] - p.Point).Length < p.Dot.Width / 2 )
                        {
                            Boundary.Points.Add( p.Point );
                            boundaryPoints.Add( p );
                            p.ParentBlocks.Add( this );
                            foundPoint = true;
                            break;
                        }
                    }
                    if ( foundPoint ) break;
                }

                if ( !foundPoint )
                {
                    Boundary.Points.Add( pts[i] );
                    boundaryPoints.Add( new DrawingPoint( canvas , this , pts[i] ) );
                }
            }

            Material = new MaterialType();

            lineConstraints = new List<LineConstraint>();
            lineLoads = new List<LineLoad>();
            pointLoads = new List<PointLoad>();

            foreach ( MaterialBlock mb in canvas.MaterialBlocks )
            {
                if ( mb == this ) continue;

                foreach ( LineConstraint lc in mb.LineConstraints )
                {
                    if ( this.BoundaryPoints.Contains( lc.Nodes[0] ) && this.BoundaryPoints.Contains( lc.Nodes[1] ) )
                    {
                        if ( !this.LineConstraints.Contains( lc ) ) this.LineConstraints.Add( lc );
                    }
                }
            }

            phaseMaterials = new List<MaterialType>();

            SortPoints();
        }

        public MaterialBlock ( SlopePlotCanvas canvas , MaterialType mtl, Point[] pts )
        {
            this.plotCanvas = canvas;

            Boundary = new Polygon();
            boundaryPoints = new List<DrawingPoint>();
            Boundary.Stroke = Brushes.Black;
            Boundary.StrokeThickness = 0.8;
            Boundary.StrokeLineJoin = PenLineJoin.Round;
            Boundary.StrokeStartLineCap = PenLineCap.Round;
            Boundary.StrokeEndLineCap = PenLineCap.Round;
            Boundary.Fill = mtl.Fill;
            Boundary.Opacity = 0.6;
            Boundary.Visibility = Visibility.Visible;

            for ( int i = 0 ; i < pts.Length ; i++ )
            {
                Boundary.Points.Add( pts[i] );
            }

            Material = mtl;

            phaseMaterials = new List<MaterialType>();

            lineConstraints = new List<LineConstraint>();
            lineLoads = new List<LineLoad>();
            pointLoads = new List<PointLoad>();
        }

        public MaterialBlock ( SlopeDefineCanvas canvas , MaterialType mtl , Point[] pts )
        {
            this.defineCanvas = canvas;

            Boundary = new Polygon();
            boundaryPoints = new List<DrawingPoint>();
            Boundary.Stroke = Brushes.Black;
            Boundary.StrokeThickness = 0.8;
            Boundary.StrokeLineJoin = PenLineJoin.Round;
            Boundary.StrokeStartLineCap = PenLineCap.Round;
            Boundary.StrokeEndLineCap = PenLineCap.Round;
            Boundary.Fill = mtl.Fill;
            Boundary.Opacity = 0.8;
            Boundary.Visibility = Visibility.Visible;

            for ( int i = 0 ; i < pts.Length ; i++ )
            {
                // check if point is same as existing point
                bool foundPoint = false;
                foreach ( MaterialBlock mb in canvas.Substructs )
                {
                    foreach ( DrawingPoint p in mb.BoundaryPoints )
                    {
                        if ( (pts[i] - p.Point).Length < p.Dot.Width / 2 )
                        {
                            Boundary.Points.Add( p.Point );
                            boundaryPoints.Add( p );
                            p.ParentBlocks.Add( this );
                            foundPoint = true;
                            break;
                        }
                    }
                    if ( foundPoint ) break;
                }

                if ( !foundPoint )
                {
                    Boundary.Points.Add( pts[i] );
                    boundaryPoints.Add( new DrawingPoint( canvas , this , pts[i] ) );
                }
            }

            Material = mtl;

            lineConstraints = new List<LineConstraint>();
            lineLoads = new List<LineLoad>();
            pointLoads = new List<PointLoad>();

            foreach ( MaterialBlock mb in canvas.Substructs )
            {
                if ( mb == this ) continue;

                foreach ( LineConstraint lc in mb.LineConstraints )
                {
                    if ( this.BoundaryPoints.Contains( lc.Nodes[0] ) && this.BoundaryPoints.Contains( lc.Nodes[1] ) )
                    {
                        if ( !this.LineConstraints.Contains( lc ) ) this.LineConstraints.Add( lc );
                    }
                }
            }

            phaseMaterials = new List<MaterialType>();

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
                Boundary.Fill = value ? selectFill : material.Fill;
            }
        }

        public bool IsMouseOver { get { return Boundary.IsMouseOver; } }

        public Polygon Boundary { get; set; }
        public List<DrawingPoint> BoundaryPoints { get { return this.boundaryPoints; } }
        public List<LineConstraint> LineConstraints { get { return this.lineConstraints; } }
        public List<LineLoad> LineLoads { get { return this.lineLoads; } }
        public List<PointLoad> PointLoads { get { return this.pointLoads; } }

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

        public List<MaterialType> PhaseMaterials { get { return this.phaseMaterials; } }

        public double Area
        {
            get
            {
                if ( boundaryPoints.Count == 0 ) return 0;

                double x1 , y1 , x2 , y2;

                x1 = boundaryPoints[boundaryPoints.Count - 1].Point.X;
                y1 = boundaryPoints[boundaryPoints.Count - 1].Point.Y;
                x2 = boundaryPoints[0].Point.X;
                y2 = boundaryPoints[0].Point.Y;

                double sum = x1 * y2 - x2 * y1;

                for ( int i = 0 ; i < boundaryPoints.Count - 1 ; i++ )
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

        public void SortPoints ()
        {
            if ( this.Area < 0 )
            {
                this.BoundaryPoints.Reverse();

                PointCollection pts = this.Boundary.Points;
                PointCollection revPts = new PointCollection();
                for ( int i = pts.Count - 1 ; i >= 0 ; i-- )
                {
                    revPts.Add( pts[i] );
                }
                this.Boundary.Points = revPts;
            }
        }

        public void Delete ()
        {
            canvas.Children.Remove( this.Boundary );

            boundaryPoints.ForEach(
                delegate( DrawingPoint p )
                {
                    if ( p.ParentBlocks.Count == 1 )
                    {
                        canvas.Children.Remove( p.Dot );
                        p.ClearFixLines();
                    }
                    else p.ParentBlocks.Remove( this );
                } );
            Boundary.Points.Clear();

            LineConstraints.ForEach( delegate( LineConstraint lc ) { lc.Delete(); } );
            LineLoads.ForEach( delegate( LineLoad ll ) { ll.Delete(); } );
            PointLoads.ForEach( delegate( PointLoad pl ) { pl.Delete(); } );

            canvas.MaterialBlocks.Remove( this );
        }

        public DrawingPoint AddPoint ( DrawingPoint p1 , DrawingPoint p2, DrawingPoint pNew )
        {
            // find point indices in list
            int index1 = BoundaryPoints.FindIndex( delegate( DrawingPoint p ) { return p == p1; } );
            int index2 = BoundaryPoints.FindIndex( delegate( DrawingPoint p ) { return p == p2; } );

            // if points were not successfully found
            if ( index1 == -1 || index2 == -1 )
            {
                MessageBox.Show( "Points not found on block." , "Error" );
                return null;
            }

            // ensure max and min as appropriate
            if ( ((index1 > index2) && !(index2 == 0 && index1 == BoundaryPoints.Count - 1))
                || (index1 == 0 && index2 == BoundaryPoints.Count - 1) )
            {
                int tmp = index1;
                index1 = index2;
                index2 = tmp;

                DrawingPoint tmpPt = p1;
                p1 = p2;
                p2 = tmpPt;
            }

            DrawingPoint newNode;
            Point newPoint;
            if ( (index2 - index1) == 1 )
            {
                if ( pNew == null )
                {
                    newPoint = new Point( 0.5 * (p1.Point.X + p2.Point.X) , 0.5 * (p1.Point.Y + p2.Point.Y) );
                    newNode = new DrawingPoint( canvas , this , newPoint );
                }
                else
                {
                    newNode = pNew;
                    newNode.ParentBlocks.Add( this );
                    newPoint = pNew.Point;
                }
                BoundaryPoints.Insert( index2 , newNode );
                Boundary.Points.Insert( index2 , newPoint );
            }
            else if ( index2 == 0 && index1 == BoundaryPoints.Count - 1 )
            {
                if ( pNew == null )
                {
                    newPoint = new Point( 0.5 * (p1.Point.X + p2.Point.X) , 0.5 * (p1.Point.Y + p2.Point.Y) );
                    newNode = new DrawingPoint( canvas , this , newPoint );
                }
                else
                {
                    newNode = pNew;
                    newNode.ParentBlocks.Add( this );
                    newPoint = pNew.Point;
                }
                BoundaryPoints.Add( newNode );
                Boundary.Points.Add( newPoint );
            }
            else
            {
                MessageBox.Show( "Points must be different and adjacent." , "Error" );
                return null;
            }

            // if a line constraint exists between these two points, remove it and create two in its place
            List<LineConstraint> existingLCs = new List<LineConstraint>();
            List<MaterialBlock> existingParents = new List<MaterialBlock>();
            canvas.MaterialBlocks.ForEach(
                delegate( MaterialBlock mb )
                {
                    existingLCs.AddRange( mb.LineConstraints.FindAll(
                        delegate( LineConstraint lc )
                        {
                            if ( lc.Nodes.Contains( p1 ) && lc.Nodes.Contains( p2 ) )
                            {
                                existingParents.Add( mb );
                                return true;
                            }
                            else return false;
                        } ) );
                } );
            // create the two new line constraints
            LineConstraint newLC1 = new LineConstraint( canvas , p1 , newNode , false , false );
            LineConstraint newLC2 = new LineConstraint( canvas , newNode , p2 , false , false );
            existingLCs.ForEach(
                delegate( LineConstraint lc )
                {
                    // match the new node fixity to the line constraint
                    newNode.IsFixedX = newNode.IsFixedX || lc.IsFixedX;
                    newNode.IsFixedY = newNode.IsFixedY || lc.IsFixedY;
                    newLC1.IsFixedX = newLC1.IsFixedX || lc.IsFixedX;
                    newLC1.IsFixedY = newLC1.IsFixedY || lc.IsFixedY;
                    newLC2.IsFixedX = newLC2.IsFixedX || lc.IsFixedX;
                    newLC2.IsFixedY = newLC2.IsFixedY || lc.IsFixedY;

                    // clear the existing plotting lines, remove the existing constraint, and add the new constraints
                    existingParents.ForEach( delegate( MaterialBlock mb ) { mb.LineConstraints.Remove( lc ); } );
                    lc.Delete();
                } );
            existingParents.ForEach( delegate( MaterialBlock mb ) { mb.LineConstraints.Add( newLC1 ); mb.LineConstraints.Add( newLC2 ); } );
            existingLCs.Clear(); existingParents.Clear();


            // if a line load exists between these two points, remove it and create two in its place
            List<LineLoad> existingLLs = new List<LineLoad>();
            canvas.MaterialBlocks.ForEach(
                delegate( MaterialBlock mb )
                {
                    existingLLs.AddRange( mb.LineLoads.FindAll(
                        delegate( LineLoad ll )
                        {
                            if ( ll.Nodes.Contains( p1 ) && ll.Nodes.Contains( p2 ) )
                            {
                                existingParents.Add( mb );
                                return true;
                            }
                            else return false;
                        } ) );
                } );
            LineLoad newLL1 = new LineLoad( canvas , p1 , newNode , false , 0 , 0 , false , 0 , 0 );
            LineLoad newLL2 = new LineLoad( canvas , newNode , p2 , false , 0 , 0 , false , 0 , 0 );
            existingLLs.ForEach(
                delegate( LineLoad ll )
                {
                    newLL1.IsLoadedN = newLL1.IsLoadedN || ll.IsLoadedN;
                    if ( newLL1.IsLoadedN )
                    {
                        newLL1.NLoad1 += ll.NLoad1;
                        newLL1.NLoad2 += 0.5 * (ll.NLoad1 + ll.NLoad2);
                    }
                    newLL1.IsLoadedT = newLL1.IsLoadedT || ll.IsLoadedT;
                    if ( newLL1.IsLoadedT )
                    {
                        newLL1.TLoad1 += ll.TLoad1;
                        newLL1.TLoad2 += 0.5 * (ll.TLoad1 + ll.TLoad2);
                    }

                    newLL2.IsLoadedN = newLL2.IsLoadedN || ll.IsLoadedN;
                    if ( newLL2.IsLoadedN )
                    {
                        newLL2.NLoad1 += 0.5 * (ll.NLoad1 + ll.NLoad2);
                        newLL2.NLoad2 += ll.NLoad2;
                    }
                    newLL2.IsLoadedT = newLL2.IsLoadedT || ll.IsLoadedT;
                    if ( newLL2.IsLoadedT )
                    {
                        newLL2.TLoad1 += 0.5 * (ll.TLoad1 + ll.TLoad2);
                        newLL2.TLoad2 += ll.TLoad2;
                    }

                    // clear the existing plotting lines, remove the existing line load, and add the new line loads
                    existingParents.ForEach( delegate( MaterialBlock mb ) { mb.LineLoads.Remove( ll ); } );
                    ll.Delete();
                } );
            LineLoads.Add( newLL1 ); LineLoads.Add( newLL2 );
            existingLLs.Clear(); existingParents.Clear();

            foreach ( MaterialBlock mb in canvas.MaterialBlocks )
            {
                for ( int i = mb.LineLoads.Count - 1 ; i >= 0 ; i-- )
                {
                    if ( !mb.LineLoads[i].IsLoadedN && !mb.LineLoads[i].IsLoadedT )
                    {
                        mb.LineLoads[i].Delete();
                        mb.LineLoads.RemoveAt( i );
                    }
                }

                for ( int i = mb.LineConstraints.Count - 1 ; i >= 0 ; i-- )
                {
                    if ( !mb.LineConstraints[i].IsFixedX && !mb.LineConstraints[i].IsFixedY )
                    {
                        mb.LineConstraints[i].Delete();
                        mb.LineConstraints.RemoveAt( i );
                    }
                }
            }

            canvas.IsSaved = false;
            canvas.IsVerified = false;

            return newNode;
        }

        public bool ApplyFixity ( DrawingPoint p1 , DrawingPoint p2 )
        {
            bool added = false;

            // find point indices in list
            int index1 = BoundaryPoints.FindIndex( delegate( DrawingPoint p ) { return p == p1; } );
            int index2 = BoundaryPoints.FindIndex( delegate( DrawingPoint p ) { return p == p2; } );

            // if points were not successfully found
            if ( index1 == -1 || index2 == -1 )
            {
                MessageBox.Show( "Points not found on block." , "Fix X error" );
                return added;
            }

            // ensure max and min as appropriate
            if ( ((index1 > index2) && !(index2 == 0 && index1 == BoundaryPoints.Count - 1))
                || (index1 == 0 && index2 == BoundaryPoints.Count - 1) )
            {
                int tmp = index1;
                index1 = index2;
                index2 = tmp;

                DrawingPoint tmpPt = p1;
                p1 = p2;
                p2 = tmpPt;
            }

            // if points are the same, fix/unfix the point ...
            if ( index1 == index2 )
            {
                SetFixityDialog dlg = new SetFixityDialog( canvas , p1 );
                dlg.ShowDialog();

                if ( dlg.DialogResult == true )
                {
                    canvas.IsSaved = false;
                    canvas.IsVerified = false;
                }
                added = true;
            }

            // ... or if the points are adjacent, create a line constraint ...
            else if ( ((index2 - index1) == 1) || (index2 == 0 && index1 == BoundaryPoints.Count - 1) )
            {
                LineConstraint existingLC = LineConstraints.Find( delegate( LineConstraint lc ) { return lc.Nodes.Contains( p1 ) && lc.Nodes.Contains( p2 ); } );

                if ( existingLC != null )
                {
                    SetFixityDialog dlg = new SetFixityDialog( canvas , existingLC );
                    dlg.ShowDialog();

                    if ( dlg.DialogResult == true )
                    {
                        canvas.IsSaved = false;
                        canvas.IsVerified = false;
                    }
                    added = true;
                }
                else
                {
                    LineConstraint newLC = new LineConstraint( canvas , p1 , p2 , false , false );
                    SetFixityDialog dlg = new SetFixityDialog( canvas , newLC );
                    dlg.ShowDialog();

                    if ( dlg.DialogResult == true )
                    {
                        canvas.MaterialBlocks.ForEach(
                            delegate( MaterialBlock mb )
                            {
                                if ( mb.BoundaryPoints.Contains( p1 ) && mb.BoundaryPoints.Contains( p2 ) )
                                    mb.LineConstraints.Add( newLC );
                            } );

                        canvas.IsSaved = false;
                        canvas.IsVerified = false;
                    }

                    added = true;
                }
            }

            // ... otherwise, indicate that a constraint cannot be applied in this manner
            else
            {
                MessageBox.Show( "Points must be either the same or directly adjacent." , "Error" );
                return added;
            }

            // remove any line constraints that are both unfixed
            canvas.MaterialBlocks.ForEach(
                delegate( MaterialBlock mb )
                {
                    mb.LineConstraints.RemoveAll( delegate( LineConstraint lc ) { return (!lc.IsFixedX && !lc.IsFixedY); } );
                } );

            return added;
        }

        public void ApplyPointLoad ( DrawingPoint p )
        {
            // check if a point load has already been defined at this point
            PointLoad load = pointLoads.Find( delegate( PointLoad pl ) { return pl.Node == p; } );

            // if undefined, create a new point load object
            if ( load == null )
            {
                load = new PointLoad( canvas , p , false , 0 , false , 0 );
                pointLoads.Add( load );
            }

            // start dialog for user input
            AddPointLoadDialog dlg = new AddPointLoadDialog( canvas , load );
            dlg.ShowDialog();

            // if there is no load in horizontal or vertical direction, delete the load ...
            if ( !load.IsLoadedX && !load.IsLoadedY )
            {
                load.Delete();
                pointLoads.Remove( load );
            }

            // ... otherwise update its visibility and plotting location
            else
            {
                load.Update();
            }

            if ( dlg.DialogResult == true )
            {
                canvas.IsSaved = false;
                canvas.IsVerified = false;
            }
        }

        public void ApplyLineLoad ( DrawingPoint p1 , DrawingPoint p2 )
        {
            // find point indices in list
            int index1 = BoundaryPoints.FindIndex( delegate( DrawingPoint p ) { return p == p1; } );
            int index2 = BoundaryPoints.FindIndex( delegate( DrawingPoint p ) { return p == p2; } );

            // if points were not successfully found
            if ( index1 == -1 || index2 == -1 )
            {
                MessageBox.Show( "Points not found on block." , "Line Load Error" );
                return;
            }

            // ensure max and min as appropriate
            if ( (index1 > index2) || (index1 == 0 && index2 == BoundaryPoints.Count - 1) )
            {
                int tmp = index1;
                index1 = index2;
                index2 = tmp;

                DrawingPoint tmpPt = p1;
                p1 = p2;
                p2 = tmpPt;
            }

            // points must be adjacent and different
            if ( (index2 - index1) == 1 || (index2 == 0 && index1 == BoundaryPoints.Count - 1) )
            {
                // check if a line load has already been defined between these two objects
                LineLoad load = lineLoads.Find( delegate( LineLoad l ) { return l.Nodes[0] == p1 && l.Nodes[1] == p2; } );

                // if undefined, create a new line load object
                if ( load == null )
                {
                    load = new LineLoad( canvas , p1 , p2 , false , 0 , 0 , false , 0 , 0 );
                    lineLoads.Add( load );
                }

                // start dialog for user input
                AddLineLoadDialog dlg = new AddLineLoadDialog( canvas , load );
                dlg.ShowDialog();

                // if there is no load in normal or tangential direction, delete the load ...
                if ( !load.IsLoadedN && !load.IsLoadedT )
                {
                    load.Delete();
                    lineLoads.Remove( load );
                }

                // ... otherwise update its visibility and plotting location
                else
                {
                    load.Update();
                }

                if ( dlg.DialogResult == true )
                {
                    canvas.IsSaved = false;
                    canvas.IsVerified = false;
                }
            }
            else
            {
                MessageBox.Show( "Points must be different and adjacent." , "Line Load Error" );
            }
        }

        public void Translate ( Vector delta )
        {
            Point p;
            for ( int i = 0 ; i < Boundary.Points.Count ; i++ )
            {
                p = Boundary.Points[i];
                p.X += delta.X;
                p.Y += delta.Y;
                Boundary.Points[i] = p;
            }

            if ( canvas != null || defineCanvas != null )
            {
                boundaryPoints.ForEach(
                    delegate( DrawingPoint dp )
                    {
                        if ( dp.ParentBlocks.IndexOf( this ) == 0 ) dp.Translate( delta );
                    } );
            }

            LineConstraints.ForEach( delegate( LineConstraint lc ) { lc.Update(); } );
            LineLoads.ForEach( delegate( LineLoad ll ) { ll.Update(); } );
            PointLoads.ForEach( delegate( PointLoad pl ) { pl.Update(); } );
        }

        public void Zoom ( double factor , Point centre )
        {
            Point p;
            for ( int i = 0 ; i < Boundary.Points.Count ; i++ )
            {
                p = Boundary.Points[i];
                p.X = centre.X + factor * (p.X - centre.X);
                p.Y = centre.Y + factor * (p.Y - centre.Y);
                Boundary.Points[i] = p;
            }

            if ( canvas != null || defineCanvas != null )
            {
                boundaryPoints.ForEach(
                    delegate( DrawingPoint dp )
                    {
                        if ( dp.ParentBlocks.IndexOf( this ) == 0 ) dp.Zoom( factor , centre );
                    } );
            }

            LineConstraints.ForEach( delegate( LineConstraint lc ) { lc.Update(); } );
            LineLoads.ForEach( delegate( LineLoad ll ) { ll.Update(); } );
            PointLoads.ForEach( delegate( PointLoad pl ) { pl.Update(); } );
        }

        private void MouseLeftButtonDown ( object sender , MouseEventArgs e )
        {
            if ( (canvas != null && canvas.DrawMode == DrawModes.Select)
                    || (defineCanvas != null && defineCanvas.DrawMode == DrawModes.Select) )
                this.IsSelected = true;
        }

        public int CheckIntersecting ()
        {
            double x1 , y1 , x2 , y2 , x3 , y3 , x4 , y4 ,
                    m1 , m2 , b1 , b2 ,
                    x , y;
            bool vert1 , vert2;
            double toler = 1e-5;
            int count = 0;

            for ( int i = 0 ; i < Boundary.Points.Count - 1 ; i++ )
            {
                x1 = Boundary.Points[i].X;
                y1 = Boundary.Points[i].Y;

                x2 = Boundary.Points[i + 1].X;
                y2 = Boundary.Points[i + 1].Y;

                vert1 = Math.Abs( x2 - x1 ) < toler;

                if ( vert1 )
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

                for ( int j = i + 1 ; j < Boundary.Points.Count ; j++ )
                {
                    x3 = Boundary.Points[j].X;
                    y3 = Boundary.Points[j].Y;

                    if ( j == Boundary.Points.Count - 1 )
                    {
                        x4 = Boundary.Points[0].X;
                        y4 = Boundary.Points[0].Y;
                    }
                    else
                    {
                        x4 = Boundary.Points[j + 1].X;
                        y4 = Boundary.Points[j + 1].Y;
                    }

                    vert2 = Math.Abs( x4 - x3 ) < toler;

                    if ( vert2 )
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

                    if ( vert1 || vert2 )
                    {
                        if ( !vert1 )
                        {
                            y = m1 * x + b1;

                            if ( (x - Math.Min( x1 , x2 )) < toler && (Math.Max( x1 , x2 ) - x) < toler
                                && (y - Math.Min( y3 , y4 )) < toler && (Math.Max( y3 , y4 ) - y) < toler )
                            {
                                count++;
                            }
                        }
                        else if ( !vert2 )
                        {
                            y = m2 * x + b2;

                            if ( (x - Math.Min( x3 , x4 )) < toler && (Math.Max( x3 , x4 ) - x) < toler
                                && (y - Math.Min( y1 , y2 )) < toler && (Math.Max( y1 , y2 ) - y) < toler )
                            {
                                count++;
                            }
                        }
                        else
                        {
                            if ( Math.Abs( x3 - x1 ) < toler && ((y3 < y2 && y4 > y1) || (y3 > y2 && y4 < y1)) ) count++;
                        }
                    }
                    else
                    {
                        if ( Math.Abs( m2 - m1 ) < toler )
                        {
                            if ( Math.Abs( b2 - b1 ) < toler
                                && !((x1 == x4 && y1 == y4) || (x2 == x3 && y2 == y3)) )
                            {
                                count++;
                            }
                        }
                        else
                        {
                            x = (b2 - b1) / (m1 - m2);

                            if ( (x - Math.Min( x1 , x2 )) > toler && (Math.Max( x1 , x2 ) - x) > toler
                                && (x - Math.Min( x3 , x4 )) > toler && (Math.Max( x3 , x4 ) - x) > toler )
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
        double kno_ , knr_ , Acoef_ , ktrb_ , ktrs_;

        public MaterialType ()
        {
            Phi = 0;
            Cohesion = 0;
            Psi = 0;
            Gamma = 0;
            Emod = 0;
            Nu = 0;
        }

        public double Phi { get; set; }
        public double Cohesion { get; set; }
        public double Psi { get; set; }
        public double Gamma { get; set; }
        public double Emod { get; set; }
        public double Nu { get; set; }

        // Calculate RFEM soil properties
        // after Stolle & Guo (2008)
        public void ComputeRFEMProperties ()
        {
            double kappa = 1.0 , del = 0.0015;

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

        public override string ToString ()
        {
            return this.Name != null ? this.Name : "Null";
        }
    }

    public class GAParams
    {
        public GAParams ()
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

    public class FEAParams
    {
        public FEAParams ()
        {
        }

        public double ColWidth { get; set; }
        public double RowHeight { get; set; }
        public int NStep { get; set; }
        public int NIter { get; set; }
        public int NPrint { get; set; }
        public double LFact { get; set; }
        public double GFact { get; set; }
    }

    public class MeshLine
    {
        private SlopeCanvas canvas;
        private List<MeshPoint> meshPoints;
        private Line line;

        public MeshLine ( SlopeCanvas canvas , double x )
        {
            this.canvas = canvas;

            // initialize and pre-allocate for 10 MeshPoints per MeshLine and 2 per MaterialBlock
            meshPoints = new List<MeshPoint>( 10 );
            List<MeshPoint> blockPoints = new List<MeshPoint>( 2 );

            // for intersection finding algorithm
            double toler = 1e-5;
            double x1 , y1 , x2 , y2 , m = 0 , y = 0;
            //bool added;

            // loop through MaterialBlocks finding boundary intersection points
            foreach ( MaterialBlock mb in canvas.MaterialBlocks )
            {
                // clear intersection point list (does not de-allocate memory)
                blockPoints.Clear();

                // initialize trailing point to final point in list
                x1 = mb.Boundary.Points[mb.Boundary.Points.Count - 1].X;
                y1 = mb.Boundary.Points[mb.Boundary.Points.Count - 1].Y;

                // loop through MaterialBlock points, finding intersections
                foreach ( Point p in mb.Boundary.Points )
                {
                    x2 = p.X;   // update leading point
                    y2 = p.Y;

                    // MeshLine will not intersect segment
                    if ( x < Math.Min( x1 , x2 ) || x > Math.Max( x1 , x2 ) || Math.Abs( x2 - x1 ) < toler )
                    {
                        x1 = x2; y1 = y2;   // skip to next point
                        continue;
                    }

                    // compute slope and y-coord of intersection
                    if ( x2 > x1 )
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
                    blockPoints.Add( new MeshPoint( new Point( x , y ) , mb.Material ) );

                    x1 = x2;    // update trailing point
                    y1 = y2;
                }

                // sort points for labeling
                blockPoints.Sort( MeshPoint.CompareByY );

                // label points as entering or exiting the block depending on order
                for ( int i = 0 ; i < blockPoints.Count ; i++ )
                    blockPoints[i].Type = i % 2 == 0 ? MeshPointType.Entrance : MeshPointType.Exit;

                // insert block points in y-order in MeshLine
                for ( int i = 0 ; i < meshPoints.Count ; i++ )
                {
                    if ( meshPoints[i].Y < blockPoints[0].Y )
                    {
                        meshPoints.InsertRange( i , blockPoints.GetRange( 0 , 2 ) );
                        blockPoints.RemoveRange( 0 , 2 );
                        if ( blockPoints.Count <= 1 ) break;
                    }
                }

                // insert remaining block points at end of MeshLine
                while ( blockPoints.Count > 1 )
                {
                    meshPoints.AddRange( blockPoints.GetRange( 0 , 2 ) );
                    blockPoints.RemoveRange( 0 , 2 );
                }
            }

            line = new Line();
            line.Stroke = Brushes.Black;
            line.StrokeThickness = 0.75;
            line.Opacity = 0.9;
            line.StrokeDashArray.Add( 7.5 );
            line.StrokeDashArray.Add( 5 );
            line.X1 = x;
            line.X2 = x;
            line.Y1 = meshPoints[0].Y;
            line.Y2 = meshPoints[meshPoints.Count - 1].Y;

            canvas.Children.Add( line );

            for ( int i = 0 ; i < meshPoints.Count ; i++ )
                canvas.Children.Add( meshPoints[i].Location );
        }

        public Line Line { get { return this.line; } }
        public List<MeshPoint> MeshPoints { get { return this.meshPoints; } }

        public bool IsVisible
        {
            set
            {
                line.Visibility = value ? Visibility.Visible : Visibility.Hidden;
                meshPoints.ForEach( delegate( MeshPoint mp ) { mp.IsVisible = value; } );
            }
        }

        public void Delete ()
        {
            meshPoints.ForEach( delegate( MeshPoint mp ) { canvas.Children.Remove( mp.Location ); } );
            meshPoints.Clear();

            canvas.Children.Remove( line );
        }

        public void Translate ( Vector delta )
        {
            meshPoints.ForEach( delegate( MeshPoint mp ) { mp.Translate( delta ); } );

            line.X1 += delta.X;
            line.X2 += delta.X;
            line.Y1 += delta.Y;
            line.Y2 += delta.Y;
        }

        public void Zoom ( double factor , Point centre )
        {
            meshPoints.ForEach( delegate( MeshPoint mp ) { mp.Zoom( factor , centre ); } );

            line.X1 = centre.X + factor * (line.X1 - centre.X);
            line.X2 = centre.X + factor * (line.X2 - centre.X);
            line.Y1 = centre.Y + factor * (line.Y1 - centre.Y);
            line.Y2 = centre.Y + factor * (line.Y2 - centre.Y);
        }
    }

    public class MeshPoint
    {
        private Ellipse location;
        private MaterialType material;

        public MeshPoint ( Point pt , MaterialType material )
        {
            location = new Ellipse();
            location.Height = 3;
            location.Width = 3;
            location.Fill = Brushes.Black;
            location.Stroke = Brushes.Black;
            location.Visibility = Visibility.Visible;
            location.Margin = new Thickness( pt.X - 1.5 , pt.Y - 1.5 , 0 , 0 );

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

        public void Translate ( Vector delta )
        {
            location.Margin = new Thickness( location.Margin.Left + delta.X , location.Margin.Top + delta.Y , 0 , 0 );
        }

        public void Zoom ( double factor , Point centre )
        {
            location.Margin = new Thickness( centre.X + factor * ((location.Margin.Left + 1.5) - centre.X) - 1.5 ,
                                            centre.Y + factor * ((location.Margin.Top + 1.5) - centre.Y) - 1.5 ,
                                            0 , 0 );
        }

        public static int CompareByY ( MeshPoint mp1 , MeshPoint mp2 )
        {
            // If mp1 is null...
            if ( mp1 == null )
            {
                // ...and mp2 is also null...
                if ( mp2 == null )
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
                if ( mp2 == null )
                {
                    // ...mp1 > mp2
                    return 1;
                }

                // ...and mp2 is also not null...
                else
                {
                    // ...compare y coordinates of points
                    if ( mp1.Location.Margin.Top < mp2.Location.Margin.Top ) return 1;
                    else if ( mp1.Location.Margin.Top > mp2.Location.Margin.Top ) return -1;
                    else return 0;
                }
            }
        }
    }

    public class AnalysisMeshPoint
    {
        private Point location;
        private MaterialType material;
        private MeshPointType type;

        public AnalysisMeshPoint ( Point pt , MaterialType material , MeshPointType type )
        {
            location.X = pt.X;
            location.Y = pt.Y;

            this.material = material;
            this.type = type;
        }

        public double X { get { return this.location.X; } }
        public double Y { get { return this.location.Y; } }
        public MaterialType Material { get { return this.material; } }
        public MeshPointType Type { get { return this.type; } }
    }


    public class AnalysisPhase
    {
        public AnalysisPhase ( int number,
            string name ,
            AnalysisPhase begin ,
            bool reset ,
            int nsteps ,
            int niterations ,
            int nprint ,
            double gravityFactor )
        {
            Number = number;

            Name = name;
            BeginPhase = begin;
            ResetDisplacements = reset;

            NSteps = nsteps;
            NIterations = niterations;
            NPrintLines = nprint;
            GravityFactor = gravityFactor;
        }

        public int Number { get; set; }

        public string Name { get; set; }
        public AnalysisPhase BeginPhase { get; set; }
        public bool ResetDisplacements { get; set; }

        public int NSteps { get; set; }
        public int NIterations { get; set; }
        public int NPrintLines { get; set; }
        public double GravityFactor { get; set; }

        public override string ToString ()
        {
            return Name + " (" + Number.ToString() + ")";
        }
    }
}