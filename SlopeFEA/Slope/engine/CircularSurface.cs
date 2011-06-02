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
using System.Windows;
using System.Windows.Media;

namespace SlopeFEA
{
    public class DisplayCircularSurface
    {
        private System.Windows.Shapes.Path surface;
        private SlopeCanvas canvas;
        private PathFigure surfaceFigure;
        ArcSegment surfaceArc;

        private bool isGlobalCritical = false;
        private bool isLocalCritical = false;
        private bool isFilled = false;

        public DisplayCircularSurface ( SlopeCanvas canvas, Point enter, Point exit, double radius )
        {
            this.canvas = canvas;

            surface = new System.Windows.Shapes.Path();
            surface.Stroke = Brushes.Black;
            surface.StrokeThickness = 1;
            surface.Fill = Brushes.Transparent;
            surface.Opacity = 0.8;
            surface.StrokeDashArray.Add( 5 );
            surface.StrokeDashArray.Add( 5 );

            PathGeometry surfaceGeom = new PathGeometry();
            surfaceGeom.FillRule = FillRule.Nonzero;
            surface.Data = surfaceGeom;

            surfaceFigure = new PathFigure();
            surfaceFigure.StartPoint = enter;
            surfaceGeom.Figures.Add( surfaceFigure );

            surfaceArc = new ArcSegment();
            surfaceArc.Size = new Size( radius, radius );
            surfaceArc.Point = exit;
            surfaceArc.SweepDirection = SweepDirection.Counterclockwise;
            surfaceArc.IsLargeArc = false;
            surfaceFigure.Segments.Add( surfaceArc );

            canvas.Children.Add( surface );
        }

        public System.Windows.Shapes.Path Surface { get { return this.surface; } }

        public bool IsVisible
        {
            get
            {
                return this.Surface.IsVisible;
            }
            set
            {
                this.Surface.Visibility = value ? Visibility.Visible : Visibility.Hidden;
            }
        }

        public bool IsGlobalCritical
        {
            get
            {
                return this.isGlobalCritical;
            }
            set
            {
                if ( value != this.isGlobalCritical )
                {
                    this.isGlobalCritical = value;

                    surface.StrokeDashArray = null;
                    surface.StrokeThickness = value ? 2 : 1;
                }
            }
        }

        public bool IsLocalCritical
        {
            get
            {
                return this.isLocalCritical;
            }
            set
            {
                if ( value != this.isLocalCritical )
                {
                    this.isLocalCritical = value;

                    surface.StrokeThickness = value ? 1.25 : 1;
                }
            }
        }

        public bool IsFilled
        {
            get
            {
                return this.isFilled;
            }
            set
            {
                if ( value != this.isFilled )
                {
                    this.isFilled = value;

                    surface.Fill = value ? Brushes.Red : Brushes.Transparent;
                }
            }
        }

        public void Delete ()
        {
            canvas.Children.Remove( surface );
        }

        public void Translate ( Vector delta )
        {
            Point p;

            p = surfaceFigure.StartPoint;
            p.X += delta.X;
            p.Y += delta.Y;
            surfaceFigure.StartPoint = p;

            p = surfaceArc.Point;
            p.X += delta.X;
            p.Y += delta.Y;
            surfaceArc.Point = p;
        }

        public void Zoom ( double factor, Point centre )
        {
            Point p;

            p = surfaceFigure.StartPoint;
            p.X = centre.X + factor * (p.X - centre.X);
            p.Y = centre.Y + factor * (p.Y - centre.Y);
            surfaceFigure.StartPoint = p;

            p = surfaceArc.Point;
            p.X = centre.X + factor * (p.X - centre.X);
            p.Y = centre.Y + factor * (p.Y - centre.Y);
            surfaceArc.Point = p;

            surfaceArc.Size = new Size( factor * surfaceArc.Size.Width, factor * surfaceArc.Size.Height );
        }
    }

    public class CircularSurface
    {
        private double x, y, r,
                        height, yBoundMin,
                        yMin, yMax,
                        rMin, rMax,
                        xMin, xMax;

        private SoilMovement soilDirection;

        private List<Point> surface;
        private List<double> radiusBreaks;

        private Random random;

        public CircularSurface ( double x, double y, double r, double sf,
                                double xEnter, double yEnter,
                                double xExit, double yExit )
        {
            random = new Random();

            this.x = x;
            this.y = y;
            this.r = r;
            this.SF = sf;
            this.XEnter = xEnter;
            this.YEnter = yEnter;
            this.XExit = xExit;
            this.YExit = yExit;
        }

        public CircularSurface ( List<Point> surface, double yBoundMin,
                                List<double> radiusBreaks, SoilMovement soilDirection )
        {
            random = new Random();

            this.x = 0.0;
            this.y = 0.0;
            this.r = 0.0;
            this.SF = 0.0;
            this.surface = surface;
            this.yBoundMin = yBoundMin;
            this.radiusBreaks = radiusBreaks;
            this.soilDirection = soilDirection;

            this.height = Math.Abs( surface[0].Y - surface[surface.Count - 1].Y );
        }

        public CircularSurface ( List<Point> surface, double yBoundMin,
                                List<double> radiusBreaks, SoilMovement soilDirection,
                                double x, double y, double r, List<double> limits )
        {
            random = new Random();

            this.x = x;
            this.y = y;
            this.r = r;
            this.SF = 0;
            this.surface = surface;
            this.yBoundMin = yBoundMin;
            this.radiusBreaks = radiusBreaks;
            this.soilDirection = soilDirection;
            this.height = Math.Abs( surface[0].Y - surface[surface.Count - 1].Y );

            this.xMin = limits[0];
            this.xMax = limits[1];
            this.yMin = limits[2];
            this.yMax = limits[3];
            this.rMin = limits[4];
            this.rMax = limits[5];
        }

        public CircularSurface ( List<Point> surface, double yBoundMin,
                                List<double> radiusBreaks, SoilMovement soilDirection,
                                double x, double y, double r, List<double> limits,
                                double sf, double xEnter, double yEnter,
                                double xExit, double yExit )
        {
            random = new Random();

            this.x = x;
            this.y = y;
            this.r = r;
            this.SF = sf;
            this.surface = surface;
            this.yBoundMin = yBoundMin;
            this.radiusBreaks = radiusBreaks;
            this.soilDirection = soilDirection;
            this.height = Math.Abs( surface[0].Y - surface[surface.Count - 1].Y );

            this.xMin = limits[0];
            this.xMax = limits[1];
            this.yMin = limits[2];
            this.yMax = limits[3];
            this.rMin = limits[4];
            this.rMax = limits[5];

            this.XEnter = xEnter;
            this.YEnter = yEnter;
            this.XExit = xExit;
            this.YExit = yExit;
        }

        public SoilMovement SoilDirection { get { return soilDirection; } }

        public double X
        {
            get
            {
                return x;
            }
            set
            {
                if ( value < xMin ) x = xMin;
                else if ( value > xMax ) x = xMax;
                else x = value;
            }
        }

        public double Y
        {
            get
            {
                return y;
            }
            set
            {
                if ( value < yMin ) y = yMin;
                else if ( value > yMax ) y = yMax;
                else y = value;

                SetRLimits();
                if ( r < rMin ) r = rMin;
                else if ( r > rMax ) r = rMax;

                SetXLimits();
                if ( x < xMin ) x = xMin;
                else if ( x > xMax ) x = xMax;
            }
        }

        public double R
        {
            get
            {
                return r;
            }
            set
            {
                if ( value < rMin ) r = rMin;
                else if ( value > rMax ) r = rMax;
                else r = value;

                SetXLimits();
                if ( x < xMin ) x = xMin;
                else if ( x > xMax ) x = xMax;
            }
        }

        public List<double> Limits
        {
            get
            {
                List<double> limits = new List<double>();

                limits.Add( xMin );
                limits.Add( xMax );
                limits.Add( yMin );
                limits.Add( yMax );
                limits.Add( rMin );
                limits.Add( rMax );

                return limits;
            }
        }

        public double SF { get; set; }
        public double SFWeight { get; set; }
        public double XEnter { get; set; }
        public double XExit { get; set; }
        public double YEnter { get; set; }
        public double YExit { get; set; }

        public void GenerateSurface ()
        {
            SetYLimits();

            GenerateY();
            GenerateR();
            GenerateX();
        }

        public void SetYLimits ()
        {
            yMin = Math.Max( surface[0].Y, surface[surface.Count - 1].Y );

            yMax = yMin + 1.5 * height;
        }

        public void GenerateY ()
        {
            Y = yMin + random.NextDouble() * (yMax - yMin);
        }

        public void SetRLimits ()
        {
            rMin = y - yMin;
            rMax = y - yBoundMin;
        }

        public void GenerateR ()
        {
            R = rMin + random.NextDouble() * (rMax - rMin);
        }

        public void SetXLimits ()
        {
            double yLowest, yRadius = radiusBreaks[0],
                    x1 = surface[0].X, y1 = surface[0].Y,
                    x2 = surface[0].X, y2 = surface[0].Y,
                    v = 0, w = 0, m = 0, c = 0,
                    mSq = 0, rSq,
                    A, B, C, toler = 1e-5;

            bool vert;

            // Compute y coord of lowest point in circle
            yLowest = y - r;

            // Compute square of radius (for efficiency)
            rSq = Math.Pow( r, 2 );

            // Shortcut if lowest point is below toe of slope
            if ( yLowest <= radiusBreaks[0] )
            {
                // Sub min x in boundary surface
                A = 1.0;
                B = -2 * surface[0].X;
                C = Math.Pow( B / 2, 2 ) + Math.Pow( surface[0].Y - this.y, 2 ) - rSq;

                xMin = (-B + Math.Sqrt( Math.Pow( B, 2 ) - 4 * A * C )) / (2 * A);

                // Sub max x in boundary surface
                B = -2 * surface[surface.Count - 1].X;
                C = Math.Pow( B / 2, 2 ) + Math.Pow( surface[surface.Count - 1].Y - this.y, 2 ) - rSq;

                xMax = (-B - Math.Sqrt( Math.Pow( B, 2 ) - 4 * A * C )) / (2 * A);

                return;
            }

            // Find appropriate radius breakpoint
            for ( int i = 0 ; i < radiusBreaks.Count ; i++ )
            {
                if ( yLowest <= radiusBreaks[i] )
                {
                    yRadius = radiusBreaks[i];
                    break;
                }
            }

            // If soil movement is left-to-right
            if ( soilDirection == SoilMovement.LtoR )
            {
                // Min x coord of centre is for circle passing
                // through xMin on upper surface
                A = 1.0;
                B = -2 * surface[0].X;
                C = Math.Pow( B / 2, 2 ) + Math.Pow( surface[0].Y - this.y, 2 ) - rSq;

                xMin = (-B + Math.Sqrt( Math.Pow( B, 2 ) - 4 * A * C )) / (2 * A);

                // Find start and end points of tangent line for xMax
                for ( int i = 0 ; i < surface.Count - 1 ; i++ )
                {
                    if ( surface[i].Y == yRadius )
                    {
                        x1 = surface[i].X;
                        y1 = surface[i].Y;
                        x2 = surface[i + 1].X;
                        y2 = surface[i + 1].Y;
                        break;
                    }
                }

                // Check if line is vertical
                vert = Math.Abs( x2 - x1 ) < toler;

                // Compute slope and y intercept
                if ( !vert )
                {
                    m = (y2 - y1) / (x2 - x1);
                    mSq = Math.Pow( m, 2 );
                    c = y1 - m * x1;
                }

                // For maximum x coord of centre, let circle with
                // given y coord of centre, y, and radius, r, be 
                // tangent to line defined by (x1, y1),(x2,y2)
                // having slope, m, and y-intercept, c, at point (v,w).
                // Point v is solved for using quadratic equation
                // for Av^2 + Bv + C = 0
                //		where,
                //				A = m^2 * (1 + m^2)
                //				B = 2*m*(1 + m^2)*(c - y)
                //				C = (1 + m^2)*(c - y)^2 - r^2
                if ( !vert )
                {
                    A = mSq * (1 + mSq);
                    B = 2 * m * (1 + mSq) * (c - this.y);
                    C = (1 + mSq) * Math.Pow( c - this.y, 2 ) - rSq;

                    // ADD the discriminant
                    v = (-B + Math.Sqrt( Math.Pow( B, 2 ) - 4 * A * C )) / (2 * A);

                    // Check that (v,w) lies on the line segment
                    if ( v > x1 )
                    {
                        w = m * v + c;
                        xMax = m * (w - this.y) + v;
                    }
                    // Otherwise, use (x1,y1)
                    else
                    {
                        A = 1.0;
                        B = -2 * x1;
                        C = Math.Pow( x1, 2 ) + Math.Pow( y1 - this.y, 2 ) - rSq;

                        xMax = (-B + Math.Sqrt( Math.Pow( B, 2 ) - 4 * A * C )) / (2 * A);
                    }
                }
                else
                {
                    // If line is vertical, max x coord of centre
                    // is for circle passing through (x1, y1)

                    A = 1.0;
                    B = -2 * x1;
                    C = Math.Pow( x1, 2 ) + Math.Pow( y1 - this.y, 2 ) - rSq;

                    xMax = (-B + Math.Sqrt( Math.Pow( B, 2 ) - 4 * A * C )) / (2 * A);
                }

                xMax -= 0.025 / 1.5 * height;
            }

            // If soil movement is right-to-left (or not detected)
            else
            {
                // Maximum x coord of centre is for circle passing
                // through xMax on upper surface
                A = 1.0;
                B = -2 * surface[surface.Count - 1].X;
                C = Math.Pow( B / 2, 2 ) + Math.Pow( surface[surface.Count - 1].Y - this.y, 2 ) - rSq;

                xMax = (-B - Math.Sqrt( Math.Pow( B, 2 ) - 4 * A * C )) / (2 * A);

                // Find start and end points of tangent line for xMin
                for ( int i = 0 ; i < surface.Count ; i++ )
                {
                    if ( surface[i].Y == yRadius )
                    {
                        x1 = surface[i].X;
                        y1 = surface[i].Y;
                        x2 = surface[i + 1].X;
                        y2 = surface[i + 1].Y;
                        break;
                    }
                }

                // Check if line is vertical
                vert = Math.Abs( x2 - x1 ) < toler;

                // Compute slope and y intercept
                if ( !vert )
                {
                    m = (y2 - y1) / (x2 - x1);
                    mSq = Math.Pow( m, 2 );
                    c = y1 - m * x1;
                }

                // For minimum x coord of centre, let circle with
                // given y coord of centre, y, and radius, r, be 
                // tangent to line defined by (x1, y1),(x2,y2)
                // having slope, m, and y-intercept, c, at point (v,w).
                // Point v is solved for using quadratic equation
                // for Av^2 + Bv + C = 0
                //		where,
                //				A = m^2 * (1 + m^2)
                //				B = 2*m*(1 + m^2)*(c - y)
                //				C = (1 + m^2)*(c - y)^2 - r^2
                if ( !vert )
                {
                    A = mSq * (1 + mSq);
                    B = 2 * m * (1 + mSq) * (c - this.y);
                    C = (1 + mSq) * Math.Pow( c - this.y, 2 ) - rSq;

                    // SUBTRACT the discriminant
                    v = (-B - Math.Sqrt( Math.Pow( B, 2 ) - 4 * A * C )) / (2 * A);

                    // Check that (v,w) lies on the line segment
                    if ( v < x2 )
                    {
                        w = m * v + c;
                        xMin = m * (w - this.y) + v;
                    }
                    // Otherwise, use (x2,y2)
                    else
                    {
                        A = 1.0;
                        B = -2 * x2;
                        C = Math.Pow( x2, 2 ) + Math.Pow( y2 - this.y, 2 ) - rSq;

                        xMin = (-B - Math.Sqrt( Math.Pow( B, 2 ) - 4 * A * C )) / (2 * A);
                    }
                }
                else
                {
                    // If line is vertical, min x coord of centre
                    // is for circle passing through (x2, y2)

                    A = 1.0;
                    B = -2 * x2;
                    C = Math.Pow( x2, 2 ) + Math.Pow( y2 - this.y, 2 ) - rSq;

                    xMin = (-B - Math.Sqrt( Math.Pow( B, 2 ) - 4 * A * C )) / (2 * A);
                }

                xMin += 0.025 / 1.5 * height;
            }
        }

        public void GenerateX ()
        {
            X = xMin + random.NextDouble() * (xMax - xMin);
        }
    }
}
