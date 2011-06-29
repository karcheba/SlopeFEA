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
using System.ComponentModel;
using System.IO;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Controls.Primitives;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Shapes;

namespace SlopeFEA
{
    public partial class SlopePlotCanvas : Canvas
    {
        private SlopeCanvas source;
        private bool panning , zooming;
        private AnalysisType analysisType;
        private double dpiX , dpiY;
        private int mouseDelta = 0;
        private Point transPoint;
        private ZoomRect zoomRect;
        private Grid xAxis , yAxis, infoBlock;
        private List<MaterialType> materialTypes;
        private FEAParams feaParams;
        private List<MaterialBlock> substructs;
        private List<fe3NodedTriElement> triMesh, deformedTriMesh;
        private List<feNode> nodes , deformedNodes;
        private List<List<double>> disp;
        private double maxDisp;


        /// <summary>
        /// (Constructor) Adds various drawing Polygons.
        /// </summary>
        public SlopePlotCanvas ( SlopeCanvas source )
        {
            this.source = source;
            this.analysisType = source.AnalysisType;
            this.dpiX = source.DpiX;
            this.dpiY = source.DpiY;
            this.FilePath = source.FilePath;

            this.SizeChanged += new SizeChangedEventHandler( SlopePlotCanvas_SizeChanged );

            // For zooming to a particular area
            zoomRect = new ZoomRect();

            // Initialize material type list
            materialTypes = source.MaterialTypes;

            // Initialize list of FEA parameters
            feaParams = source.FEAParameters;

            // Initialize node data
            LoadNodeData();

            // Initialize mesh
            triMesh = new List<fe3NodedTriElement>();
            deformedTriMesh = new List<fe3NodedTriElement>();
        }


        // ----------------------------------
        // PROPERTIES
        // ----------------------------------

        public string FilePath { get; set; }

        public DrawModes DrawMode { get; set; }
        public Units Units { get; set; }                        // See enum types above
        public Scales ScaleType { get; set; }

        public double OriginOffsetX { get; set; }               // Plotting origin location (in pixels)
        public double OriginOffsetY { get; set; }               // (0,0) -> BL corner

        public double Scale { get; set; }                       // Plotting scale (actual units / screen unit)

        public double XAxisMax { get; set; }
        public double XAxisMin { get; set; }                    // Plotting axis extents (in actual units)
        public double YAxisMax { get; set; }
        public double YAxisMin { get; set; }

        public double XMajorDivision { get; set; }              // Major grid spacing (in actual units)
        public double YMajorDivision { get; set; }

        public int XMinorDivisions { get; set; }                // # minor grid divisions / major grid spacer
        public int YMinorDivisions { get; set; }

        public bool IsScaled { get; set; }                      // Toggle for initial display setup


        // ----------------------------------
        // UTILITY FUNCTIONS
        // ----------------------------------

        /// <summary>
        /// Set initial (non-painting) properties
        /// </summary>
        public void InitializeCanvas ()
        {
            this.ClipToBounds = true;

            panning = false;
            zooming = false;

            DrawMode = DrawModes.Select;
            Units = source.Units;
            ScaleType = Scales.Custom;

            OriginOffsetX = 50;
            OriginOffsetY = 50;

            XAxisMax = source.XAxisMax;
            XAxisMin = source.XAxisMin;
            YAxisMax = source.YAxisMax;
            YAxisMin = source.YAxisMin;

            XMajorDivision = source.XMajorDivision;
            YMajorDivision = source.YMajorDivision;

            XMinorDivisions = source.XMinorDivisions;
            YMinorDivisions = source.YMinorDivisions;

            IsScaled = false;
        }


        /// <summary>
        /// Construction of axes and grid
        /// </summary>
        public void BuildAxes ()
        {
            // Compute plot scaling factors [(pixels/inch) / (actual units/screen unit)]
            double xFactor = dpiX / Scale;
            double yFactor = dpiY / Scale;
            switch ( Units )
            {
                // Apply unit modification factor (screen units/inch)
                case Units.Metres: xFactor /= 0.0254; yFactor /= 0.0254; break;
                case Units.Millimetres: xFactor /= 25.4; yFactor /= 25.4; break;
                case Units.Feet: xFactor *= 12.0; yFactor *= 12.0; break;
                default: break;
            } // Result --> (pixels / actual unit)

            // Compute minor grid spacing (actual units)
            double xMinor = XMajorDivision / XMinorDivisions;
            double yMinor = YMajorDivision / YMinorDivisions;

            /*
             * Refresh X axis
             */
            xAxis.Children.Clear();

            // Add surrounding border
            Border xAxisBorder = new Border();
            xAxisBorder.BorderBrush = Brushes.DimGray;
            xAxisBorder.VerticalAlignment = VerticalAlignment.Stretch;
            xAxisBorder.HorizontalAlignment = HorizontalAlignment.Stretch;
            xAxisBorder.BorderThickness = new Thickness( 1 );
            xAxisBorder.Margin = new Thickness( 0 );
            xAxis.Children.Add( xAxisBorder );

            // Add appropriate units label
            TextBlock xLabel = new TextBlock();
            string units;
            switch ( this.Units )
            {
                case Units.Metres: units = "m"; break;
                case Units.Millimetres: units = "mm"; break;
                case Units.Feet: units = "ft"; break;
                default: units = "in"; break;
            }
            xLabel.Text = String.Format( "X ({0})" , units );
            xLabel.FontSize = 10;
            xLabel.VerticalAlignment = VerticalAlignment.Bottom;
            xLabel.HorizontalAlignment = HorizontalAlignment.Left;
            xLabel.Margin = new Thickness( 10 , 0 , 0 , 7 );
            xAxis.Children.Add( xLabel );

            Line majorLine , minorLine;      // For boxing axis lines and labels
            TextBlock majorLabel;

            // Add major and minor axis lines and labels to major divisions
            for ( double xMajor = XAxisMin ; xMajor <= XAxisMax ; xMajor += XMajorDivision )
            {
                majorLine = new Line();
                majorLine.Stroke = Brushes.Black;
                majorLine.X1 = majorLine.X2 = xMajor * xFactor + OriginOffsetX;
                majorLine.Y1 = 0;
                majorLine.Y2 = 25;
                xAxis.Children.Add( majorLine );

                majorLabel = new TextBlock();
                majorLabel.Text = String.Format( "{0}" , Math.Round( xMajor , 2 ) );
                majorLabel.FontSize = 10;
                majorLabel.VerticalAlignment = VerticalAlignment.Top;
                majorLabel.HorizontalAlignment = HorizontalAlignment.Left;
                majorLabel.Margin = new Thickness( majorLine.X1 + 5 , 13 , 0 , 0 );
                xAxis.Children.Add( majorLabel );

                if ( Math.Abs( xMajor - this.XAxisMax ) > 1e-3 )
                {
                    for ( int ixMinor = 1 ; ixMinor < XMinorDivisions ; ixMinor++ )
                    {
                        minorLine = new Line();
                        minorLine.Stroke = Brushes.Black;
                        minorLine.X1 = minorLine.X2 = majorLine.X1 + ixMinor * xMinor * xFactor;
                        minorLine.Y1 = 0;
                        minorLine.Y2 = 10;
                        xAxis.Children.Add( minorLine );
                    }
                }
            }

            /*
             * Refresh Y axis
             */
            yAxis.Children.Clear();

            // Add surrounding border
            Border yAxisBorder = new Border();
            yAxisBorder.BorderBrush = Brushes.DimGray;
            yAxisBorder.VerticalAlignment = VerticalAlignment.Stretch;
            yAxisBorder.HorizontalAlignment = HorizontalAlignment.Stretch;
            yAxisBorder.BorderThickness = new Thickness( 1 );
            yAxisBorder.Margin = new Thickness( 0 );
            yAxis.Children.Add( yAxisBorder );

            // Add appropriate units label
            TextBlock yLabel = new TextBlock();
            yLabel.Text = String.Format( "Y\n({0})" , units );
            yLabel.FontSize = 10;
            yLabel.TextAlignment = TextAlignment.Right;
            yLabel.VerticalAlignment = VerticalAlignment.Bottom;
            yLabel.HorizontalAlignment = HorizontalAlignment.Right;
            yLabel.Margin = new Thickness( 0 , 0 , 35 , 10 );
            yAxis.Children.Add( yLabel );

            // Add major and minor axis lines and labels to major divisions
            // (NB: Actual units origin @ (0, ActualHeight) which is BL corner
            //      while screen pixels origin @ (0,0) which is TL corner
            for ( double yMajor = YAxisMin ; yMajor <= YAxisMax ; yMajor += YMajorDivision )
            {
                majorLine = new Line();
                majorLine.Stroke = Brushes.Black;
                majorLine.Y1 = majorLine.Y2 = ActualHeight - (yMajor * yFactor + OriginOffsetY);
                majorLine.X1 = yAxis.ActualWidth;
                majorLine.X2 = majorLine.X1 - 25;
                yAxis.Children.Add( majorLine );

                majorLabel = new TextBlock();
                majorLabel.Text = String.Format( "{0}" , Math.Round( yMajor , 2 ) );
                majorLabel.FontSize = 10;
                majorLabel.VerticalAlignment = VerticalAlignment.Bottom;
                majorLabel.HorizontalAlignment = HorizontalAlignment.Right;
                majorLabel.TextAlignment = TextAlignment.Right;
                majorLabel.Margin = new Thickness( 0 , 0 , 15 , ActualHeight - majorLine.Y1 + 3 );
                yAxis.Children.Add( majorLabel );

                if ( Math.Abs( yMajor - YAxisMax ) > 1e-3 )
                {
                    for ( int iyMinor = 1 ; iyMinor < YMinorDivisions ; iyMinor++ )
                    {
                        minorLine = new Line();
                        minorLine.Stroke = Brushes.Black;
                        minorLine.Y1 = minorLine.Y2 = majorLine.Y1 - iyMinor * yMinor * yFactor;
                        minorLine.X1 = yAxis.ActualWidth;
                        minorLine.X2 = minorLine.X1 - 10;
                        yAxis.Children.Add( minorLine );
                    }
                }
            }


            /*
             * Refresh drawing canvas
             */
            this.Children.Clear();

            // Add drawing Polygon objects back on top

            // Add reference elements on top of polygons
            triMesh.ForEach(
                delegate( fe3NodedTriElement element ) { this.Children.Add( element.Boundary ); } );

            // Add deformed elements on top of reference elements
            deformedTriMesh.ForEach(
                delegate( fe3NodedTriElement element ) { this.Children.Add( element.Boundary ); } );

            // Add temporary drawing objects on top of everything
            this.Children.Add( zoomRect.Boundary );
        }


        /// <summary>
        /// Apply translation to canvas and axes content
        /// </summary>
        public void Translate ( Vector delta )
        {
            /*
             * Update plotting origin (in pixels)
             */
            OriginOffsetX += delta.X;
            OriginOffsetY -= delta.Y;


            /*
             * Translate axis components
             */

            Line line;          // For unboxing axis lines and labels
            TextBlock tb;

            // Update coordinates of x axis objects
            for ( int i = 2 ; i < xAxis.Children.Count ; i++ )
            {
                if ( xAxis.Children[i] is Line )
                {
                    line = xAxis.Children[i] as Line;
                    line.X1 = line.X2 += delta.X;
                }
                else if ( xAxis.Children[i] is TextBlock )
                {
                    tb = xAxis.Children[i] as TextBlock;
                    tb.Margin = new Thickness( tb.Margin.Left + delta.X , tb.Margin.Top , 0 , 0 );
                }
            }

            // Update coordinates of y axis objects
            for ( int i = 2 ; i < yAxis.Children.Count ; i++ )
            {
                if ( yAxis.Children[i] is Line )
                {
                    line = yAxis.Children[i] as Line;
                    line.Y1 = line.Y2 += delta.Y;
                }
                else if ( yAxis.Children[i] is TextBlock )
                {
                    tb = yAxis.Children[i] as TextBlock;
                    tb.Margin = new Thickness( 0 , 0 , tb.Margin.Right , tb.Margin.Bottom - delta.Y );
                }
            }


            /*
             * Translate plotting canvas components
             */
            
            // Update FEA elements
            triMesh.ForEach( delegate( fe3NodedTriElement element ) { element.Translate( delta ); } );
            deformedTriMesh.ForEach( delegate( fe3NodedTriElement element ) { element.Translate( delta ); } );
        }


        /// <summary>
        /// Zooms canvas content and axes
        /// </summary>
        /// <param name="factor">scaling factor for zoom</param>
        /// <param name="centre">central reference point</param>
        public void Zoom ( double factor , Point centre )
        {
            // Update plotting scale
            Scale /= factor;

            // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
            // Modify display of scale
            // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

            // Update plotting origin (in pixels)
            OriginOffsetX = centre.X + factor * (OriginOffsetX - centre.X);
            OriginOffsetY = (ActualHeight - centre.Y) - factor * ((ActualHeight - OriginOffsetY) - centre.Y);


            /*
             * Zoom plotting axes
             */

            Line line;          // For unboxing axis lines and labels
            TextBlock tb;

            // Zoom x axis components
            for ( int i = 2 ; i < xAxis.Children.Count ; i++ )
            {
                if ( xAxis.Children[i] is Line )
                {
                    line = xAxis.Children[i] as Line;
                    line.X1 = line.X2 = centre.X + factor * (line.X2 - centre.X);
                }
                else if ( xAxis.Children[i] is TextBlock )
                {
                    tb = xAxis.Children[i] as TextBlock;
                    tb.Margin = new Thickness( centre.X + factor * (tb.Margin.Left - centre.X) , tb.Margin.Top , 0 , 0 );
                }
            }

            // Zoom y axis components
            for ( int i = 2 ; i < yAxis.Children.Count ; i++ )
            {
                if ( yAxis.Children[i] is Line )
                {
                    line = yAxis.Children[i] as Line;
                    line.Y1 = line.Y2 = centre.Y + factor * (line.Y2 - centre.Y);
                }
                else if ( yAxis.Children[i] is TextBlock )
                {
                    tb = yAxis.Children[i] as TextBlock;
                    tb.Margin = new Thickness( 0 , 0 , tb.Margin.Right , (yAxis.ActualHeight - centre.Y) - factor * (yAxis.ActualHeight - tb.Margin.Bottom - centre.Y) );
                }
            }


            /*
             * Zoom canvas plotting components
             */

            // Zoom FEA elements
            triMesh.ForEach( delegate( fe3NodedTriElement element ) { element.Zoom( factor , centre ); } );
            deformedTriMesh.ForEach( delegate( fe3NodedTriElement element ) { element.Zoom( factor , centre ); } );
        }


        /// <summary>
        /// Fits boundaries and axes extents to view window
        /// (Essentially a wrapper for Translate and Zoom with computation of appropriate
        /// translation and scaling factors)
        /// </summary>
        /// <param name="zoom">true = centre view AND fit extents, false = just centre view</param>
        public void CentreAndFitExtents ( bool zoom )
        {
            // Get initial values for extents from plotting axes
            // (NB: Drawing is allowed outside this range and this
            //      will also be checked)
            double xMax = XAxisMax;
            double xMin = XAxisMin;
            double yMax = YAxisMax;
            double yMin = YAxisMin;

            // Get units dependent scaling factor
            double factor;
            switch ( Units )
            {
                case Units.Metres: factor = 0.0254; break;
                case Units.Millimetres: factor = 25.4; break;
                case Units.Feet: factor = 1.0 / 12.0; break;
                default: factor = 1.0; break;
            }

            // Convert maximum extents to pixels
            double xMaxPix = (xMax / Scale) * (dpiX / factor) + OriginOffsetX;
            double xMinPix = (xMin / Scale) * (dpiX / factor) + OriginOffsetX;
            double yMaxPix = ActualHeight - ((yMax / Scale) * (dpiY / factor) + OriginOffsetY);
            double yMinPix = ActualHeight - ((yMin / Scale) * (dpiY / factor) + OriginOffsetY);

            // Compute centre of desired window and centre of current window
            Point fitCentre = new Point( 0.5 * (xMaxPix + xMinPix) , 0.5 * (yMaxPix + yMinPix) );
            Point canvCentre = new Point( 0.5 * ActualWidth , 0.5 * ActualHeight );

            // Shift points to desired centre
            Translate( canvCentre - fitCentre );

            if ( zoom )
            {
                // Compute desired scale (fit all content with minimum of 75 pixels of padding)
                double canvasWidth = (ActualWidth - 75) / dpiX * factor;
                double canvasHeight = (ActualHeight - 75) / dpiY * factor;
                double reqScale = Math.Max( (xMax - xMin) / canvasWidth , (yMax - yMin) / canvasHeight );

                // Zoom by factor to achieve required scale with centre of window as focus point
                Zoom( Scale / reqScale , new Point( 0.5 * ActualWidth , 0.5 * ActualHeight ) );
            }
        }


        /// <summary>
        /// Zooms the window to fit the rectangle specified by zoomBounds
        /// </summary>
        public void ZoomArea ( Polygon zoomBounds )
        {
            // Compute centre of zoom region and centre of canvas
            Point fitCentre =
                new Point( 0.5 * (zoomBounds.Points[0].X + zoomBounds.Points[2].X) ,
                            0.5 * (zoomBounds.Points[0].Y + zoomBounds.Points[2].Y) );

            Point canvCentre = new Point( 0.5 * ActualWidth , 0.5 * ActualHeight );

            // Shift points so centres are coincident
            Translate( canvCentre - fitCentre );

            // Get units dependent scaling factor
            double factor;
            switch ( Units )
            {
                case Units.Metres: factor = 0.0254; break;
                case Units.Millimetres: factor = 25.4; break;
                case Units.Feet: factor = 1.0 / 12.0; break;
                default: factor = 1.0; break;
            }

            // Compute zoom region dimensions (in actual units)
            double realWidth = Math.Abs( zoomBounds.Points[0].X - zoomBounds.Points[2].X ) * (factor / dpiX) * Scale;
            double realHeight = Math.Abs( zoomBounds.Points[0].Y - zoomBounds.Points[2].Y ) * (factor / dpiY) * Scale;

            // Compute canvas size (in screen units)
            double canvasWidth = ActualWidth / dpiX * factor;
            double canvasHeight = ActualHeight / dpiY * factor;

            // Compute required scale (actual units/screen units)
            double reqScale = Math.Max( realWidth / canvasWidth , realHeight / canvasHeight );

            // Zoom by appropriate factor centred on canvas centre
            Zoom( Scale / reqScale , new Point( 0.5 * ActualWidth , 0.5 * ActualHeight ) );
        }


        /// <summary>
        /// Cancels current drawing operation
        /// </summary>
        public void CancelDrawing ()
        {
            panning = false;
            zooming = false;
        }


        /// <summary>
        /// Gets node data from output file
        /// </summary>
        public void LoadNodeData ()
        {
            string[] pathSplit = FilePath.Split( '.' );

            // Find node file
            pathSplit[1] = "nod";
            string nodePath = String.Join( "." , pathSplit );
            if ( !File.Exists( nodePath ) )
            {
                MessageBox.Show( "Could not find node data file." , "Error" );
                return;
            }

            // Load node data from file
            // N.B.: nodes list is initialized with a null to 
            //          account for 1 based indexing in Fortran
            nodes = new List<feNode>() { null };
            disp = new List<List<double>>() { null };
            maxDisp = 0.0;
            double currDisp;
            using ( TextReader tr = new StreamReader( nodePath ) )
            {
                int nnod = int.Parse( tr.ReadLine().Split( '\t' )[0] );

                // advance to line containing "TRACTION LOADING" header
                while ( !tr.ReadLine().Contains( "TRACTION LOADING" ) ) ;

                tr.ReadLine();
                tr.ReadLine();
                tr.ReadLine();
                tr.ReadLine();
                tr.ReadLine();

                string[] lineSplit;
                for ( int i = 1 ; i <= nnod ; i++ )
                {
                    lineSplit = tr.ReadLine().Split( new char[] { ' ' } , StringSplitOptions.RemoveEmptyEntries );

                    nodes.Add( new feNode( int.Parse( lineSplit[0] ) , false , 
                                            double.Parse( lineSplit[1] ) , 
                                            double.Parse( lineSplit[2] ) ) );

                    disp.Add( new List<double>(){   double.Parse(lineSplit[3]),
                                                    double.Parse(lineSplit[4])} );

                    currDisp = Math.Sqrt( Math.Pow( disp[i][0] , 2 ) + Math.Pow( disp[i][1] , 2 ) );
                    if ( currDisp > maxDisp ) maxDisp = currDisp;
                }
            }
        }

        /// <summary>
        /// Plots deformed mesh output
        /// </summary>
        public void PlotDeformedMesh ()
        {
            // get canvas dimensions/properties
            double originX = OriginOffsetX ,
                   originY = OriginOffsetY ,
                   scale = Scale ,
                   yHeight = ActualHeight;
            Units units = Units;

            // get units dependent scaling factor
            double factor;
            switch ( units )
            {
                case Units.Metres: factor = 0.0254; break;
                case Units.Millimetres: factor = 25.4; break;
                case Units.Feet: factor = 1.0 / 12.0; break;
                default: factor = 1.0; break;
            }

            Polygon newPolygon;
            double x , y;

            // on first call (from constructor) read from file
            if ( triMesh.Count == 0 )
            {
                string[] pathSplit = FilePath.Split( '.' );

                // Find element file
                pathSplit[1] = "ele";
                string elementPath = String.Join( "." , pathSplit );
                if ( !File.Exists( elementPath ) )
                {
                    MessageBox.Show( "Could not find element data file." , "Error" );
                    return;
                }

                string[] lineSplit;
                using ( TextReader tr = new StreamReader( elementPath ) )
                {
                    int nel = int.Parse( tr.ReadLine().Split( '\t' )[0] );

                    for ( int i = 0 ; i < nel ; i++ )
                    {
                        lineSplit = tr.ReadLine().Split( '\t' );

                        triMesh.Add( new fe3NodedTriElement( int.Parse( lineSplit[0] ) ,
                            nodes[int.Parse( lineSplit[1] )] ,
                            nodes[int.Parse( lineSplit[2] )] ,
                            nodes[int.Parse( lineSplit[3] )] ,
                            materialTypes[int.Parse( lineSplit[4] ) - 1] ,
                            false ) );

                        deformedTriMesh.Add( new fe3NodedTriElement( int.Parse( lineSplit[0] ) ,
                            nodes[int.Parse( lineSplit[1] )] ,
                            nodes[int.Parse( lineSplit[2] )] ,
                            nodes[int.Parse( lineSplit[3] )] ,
                            materialTypes[int.Parse( lineSplit[4] ) - 1] ,
                            false ) );
                    }
                }

                triMesh.ForEach( delegate( fe3NodedTriElement element ) {

                    newPolygon = new Polygon();
                    newPolygon.StrokeThickness = 0.6;
                    newPolygon.Stroke = Brushes.Black;
                    newPolygon.Opacity = 1.0 /*0.8*/;
                    newPolygon.Fill = Brushes.Transparent /* element.Material.Fill*/;
                    
                    foreach ( feNode node in element.Nodes )
                    {
                        x = node.X / (scale * factor) * dpiX + originX;
                        y = yHeight - (node.Y / (scale * factor) * dpiY + originY);
                        newPolygon.Points.Add( new Point( x , y ) );
                    }

                    element.Boundary = newPolygon;
                } );
            }

            // set reference mesh bounds to visible
            triMesh.ForEach( delegate( fe3NodedTriElement element ) { element.Boundary.Stroke = Brushes.Black; } );

            // compute deformed coordinates
            double mag = 0.25 * 0.5 * (feaParams.RowHeight + feaParams.ColWidth) / maxDisp;
            deformedNodes = new List<feNode>() { null };
            for ( int i = 1 ; i < nodes.Count ; i++ )
            {
                deformedNodes.Add( new feNode( i , false ,
                    nodes[i].X + mag * disp[i][0] ,
                    nodes[i].Y + mag * disp[i][1] ) );
            }

            // set mesh to deformed coords
            deformedTriMesh.ForEach( delegate( fe3NodedTriElement element )
            {
                element.Nodes[0] = deformedNodes[element.Nodes[0].Number];
                element.Nodes[1] = deformedNodes[element.Nodes[1].Number];
                element.Nodes[2] = deformedNodes[element.Nodes[2].Number];

                newPolygon = new Polygon();
                newPolygon.StrokeThickness = 0.8;
                newPolygon.Stroke = Brushes.Red;
                newPolygon.Opacity = /*1.0*/ 0.8;
                newPolygon.Fill = /*Brushes.White*/ element.Material.Fill;

                foreach ( feNode node in element.Nodes )
                {
                    x = node.X / (scale * factor) * dpiX + originX;
                    y = yHeight - (node.Y / (scale * factor) * dpiY + originY);
                    newPolygon.Points.Add( new Point( x , y ) );
                }

                element.Boundary = newPolygon;
            } );

            // refresh plotting axes
            BuildAxes();
        }


        // ----------------------------------
        // OVERRIDES
        // ----------------------------------

        protected override void OnMouseLeftButtonUp ( MouseButtonEventArgs e )
        {
            base.OnMouseLeftButtonUp( e );

            Point p = Mouse.GetPosition( this );

            switch ( DrawMode )
            {
                case (DrawModes.Pan):
                    {
                        // End panning operations
                        panning = false;
                        this.Cursor = ((TextBlock) (((MainWindow) ((Grid) ((TabControl) ((TabItem) ((Grid) source.Parent).Parent).Parent).Parent).Parent).Resources["handCursor"])).Cursor;
                    }
                    break;


                case (DrawModes.ZoomArea):
                    {
                        // End zooming operations
                        zooming = false;

                        // Zoom to highlighted region
                        if ( zoomRect.Boundary.Points.Count == 4 )
                        {
                            ZoomArea( zoomRect.Boundary );
                        }

                        // Clean up resources
                        zoomRect.Boundary.Points.Clear();
                        Mouse.Capture( this , CaptureMode.None );
                    }
                    break;


                case (DrawModes.Select):
                    {
                        
                    }
                    break;
            }
        }


        protected override void OnMouseMove ( MouseEventArgs e )
        {
            base.OnMouseMove( e );

            Point p = Mouse.GetPosition( this );

            /*
             * Pan Mode
             * (NB: This is also activated during any mode with the middle mouse button)
             */
            if ( (DrawMode == DrawModes.Pan && Mouse.LeftButton == MouseButtonState.Pressed)
                || Mouse.MiddleButton == MouseButtonState.Pressed )
            {
                // Initialize panning operations
                if ( !panning )
                {
                    panning = true;
                    this.Cursor = ((TextBlock) (((MainWindow) ((Grid) ((TabControl) ((TabItem) ((Grid) source.Parent).Parent).Parent).Parent).Parent).Resources["grabCursor"])).Cursor;

                    // Set reference point for panning translation
                    transPoint = p;
                }
                else
                {
                    // Translate canvas and axes
                    Translate( p - transPoint );

                    // Update reference point
                    transPoint = p;
                }
            }


            /*
             * Zoom Area Mode
             */
            if ( this.DrawMode == DrawModes.ZoomArea && Mouse.LeftButton == MouseButtonState.Pressed )
            {
                // Initialize zooming operation
                if ( !zooming )
                {
                    zooming = true;
                    Mouse.Capture( this , CaptureMode.SubTree );

                    // Add corner points to zoom rectangle
                    // (NB: Polygon was opted for over Rectangle since
                    //      coordinate updates require fewer operations)
                    zoomRect.Boundary.Points.Add( p );
                    zoomRect.Boundary.Points.Add( p );
                    zoomRect.Boundary.Points.Add( p );
                    zoomRect.Boundary.Points.Add( p );
                }
                else
                {
                    Point zoomRectPt;   // For unboxing and updating zoom rectangle coords

                    // Point 0 remains at (Original X, Original Y)

                    // Point 1 moves to (Current X, Original Y)
                    zoomRectPt = zoomRect.Boundary.Points[1];
                    zoomRectPt.X = p.X;
                    zoomRectPt.Y = zoomRect.Boundary.Points[0].Y;
                    zoomRect.Boundary.Points[1] = zoomRectPt;

                    // Point 2 moves to (Current X, Current Y)
                    zoomRect.Boundary.Points[2] = p;

                    // Point 3 moves to (Original X, Current Y)
                    zoomRectPt = zoomRect.Boundary.Points[3];
                    zoomRectPt.X = zoomRect.Boundary.Points[0].X;
                    zoomRectPt.Y = p.Y;
                    zoomRect.Boundary.Points[3] = zoomRectPt;
                }
            }
        }


        protected override void OnMouseUp ( MouseButtonEventArgs e )
        {
            base.OnMouseUp( e );

            /*
             * Pan Mode (drawing mode independent middle mouse button version)
             */
            if ( e.ChangedButton == MouseButton.Middle )
            {
                // End panning operations
                panning = false;

                switch ( DrawMode )
                {
                    case DrawModes.Pan:
                        this.Cursor = ((TextBlock) (((MainWindow) ((Grid) ((TabControl) ((TabItem) ((Grid) source.Parent).Parent).Parent).Parent).Parent).Resources["handCursor"])).Cursor;
                        break;
                    case DrawModes.ZoomArea:
                        this.Cursor = ((TextBlock) (((MainWindow) ((Grid) ((TabControl) ((TabItem) ((Grid) source.Parent).Parent).Parent).Parent).Parent).Resources["zoomAreaCursor"])).Cursor;
                        break;
                    default:
                        this.Cursor = Cursors.Arrow;
                        break;
                }
            }
        }


        protected override void OnMouseRightButtonUp ( MouseButtonEventArgs e )
        {
            base.OnMouseRightButtonUp( e );

            // End drawing
            CancelDrawing();

            // Reset cursor and drawing mode
            this.Cursor = Cursors.Arrow;
            DrawMode = DrawModes.Select;
        }


        protected override void OnMouseWheel ( MouseWheelEventArgs e )
        {
            base.OnMouseWheel( e );

            // Update mouse scroll sentinel
            mouseDelta += e.Delta;

            // Zoom in (centred on mouse position)
            while ( mouseDelta >= 120 )
            {
                Zoom( 1.1 , e.GetPosition( this ) );
                mouseDelta -= 120;
            }

            // Zoom out (centred on mouse position)
            while ( mouseDelta <= -120 )
            {
                Zoom( 1 / 1.1 , e.GetPosition( this ) );
                mouseDelta += 120;
            }
        }


        // ----------------------------------
        // CANVAS EVENTS
        // ----------------------------------


        private void SlopePlotCanvas_SizeChanged ( object sender , SizeChangedEventArgs e )
        {
            // This only occurs once, immediately after canvas intialization
            if ( !IsScaled )
            {
                // Get units dependent scaling factor
                double factor;
                switch ( Units )
                {
                    case Units.Metres: factor = 0.0254; break;
                    case Units.Millimetres: factor = 25.4; break;
                    case Units.Feet: factor = 1.0 / 12.0; break;
                    default: factor = 1.0; break;
                }

                // Compute required initial scale
                double canvasWidth = (ActualWidth - 50) / dpiX * factor;
                double canvasHeight = (ActualHeight - 50) / dpiY * factor;
                Scale = Math.Max( (XAxisMax - XAxisMin) / canvasWidth , (YAxisMax - YAxisMin) / canvasHeight );

                // Get axis references
                xAxis = (Grid) ((Grid) this.Parent).Children[0];
                yAxis = (Grid) ((Grid) this.Parent).Children[1];
                infoBlock = (Grid) ((Grid) this.Parent).Children[2];

                // Construct axes and grid
                PlotDeformedMesh();

                // Centre and fit view
                CentreAndFitExtents( true );

                IsScaled = true;
            }
            else
            {
                // Compute translation factor for y coordinates
                double deltaY = e.NewSize.Height - e.PreviousSize.Height;

                Line line;      // For unboxing axis lines

                // Update coordinates of y axis lines
                for ( int i = 2 ; i < yAxis.Children.Count ; i++ )
                {
                    if ( yAxis.Children[i] is Line )
                    {
                        line = yAxis.Children[i] as Line;
                        line.Y1 = line.Y2 += deltaY;
                    }
                }

                Vector delta = new Vector( 0 , deltaY );

                // Update FEA elements
                triMesh.ForEach( delegate( fe3NodedTriElement element ) { element.Translate( delta ); } );
                deformedTriMesh.ForEach( delegate( fe3NodedTriElement element ) { element.Translate( delta ); } );
            }
        }
    }
}
