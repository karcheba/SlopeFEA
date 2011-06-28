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
using System.Text;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Data;
using System.Windows.Documents;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Media.Imaging;
using System.Windows.Shapes;

namespace SlopeFEA
{
    /// <summary>
    /// Interaction logic for PlotResultsWindow.xaml
    /// </summary>
    public partial class PlotResultsWindow : Window
    {
        private SlopeCanvas canvas;

        public PlotResultsWindow ( Window owner , SlopeCanvas canvas )
        {
            InitializeComponent();

            this.Owner = owner;
            this.canvas = canvas;

            // Create Grid to contain two axes, info block, and plotting canvas
            Grid plottingGrid = new Grid();
            plottingGrid.HorizontalAlignment = HorizontalAlignment.Stretch;
            plottingGrid.VerticalAlignment = VerticalAlignment.Stretch;
            plottingGrid.Background = Brushes.WhiteSmoke;
            plottingGrid.Margin = new Thickness( 0 , menuPanel.Height , 0 , 0 );
            contentGrid.Children.Add( plottingGrid );

            double axisWidth = 55;
            double infoBlockWidth = 200;
            double progressBarWidth = 10;

            // Create Grid object for x axis
            Grid xAxis = new Grid();
            xAxis.ClipToBounds = true;
            xAxis.Background = Brushes.WhiteSmoke;
            xAxis.VerticalAlignment = VerticalAlignment.Bottom;
            xAxis.HorizontalAlignment = HorizontalAlignment.Stretch;
            xAxis.Margin = new Thickness( axisWidth , 0 , infoBlockWidth , progressBarWidth );
            xAxis.Height = axisWidth;
            plottingGrid.Children.Add( xAxis );

            // Create Grid object for y axis
            Grid yAxis = new Grid();
            yAxis.ClipToBounds = true;
            yAxis.Background = Brushes.WhiteSmoke;
            yAxis.VerticalAlignment = VerticalAlignment.Stretch;
            yAxis.HorizontalAlignment = HorizontalAlignment.Left;
            yAxis.Margin = new Thickness( 0 , 0 , 0 , axisWidth + progressBarWidth );
            yAxis.Width = axisWidth;
            plottingGrid.Children.Add( yAxis );

            // Create Grid object for plotting info
            Grid infoBlock = new Grid();
            infoBlock.ClipToBounds = true;
            infoBlock.Background = Brushes.WhiteSmoke;
            infoBlock.VerticalAlignment = VerticalAlignment.Stretch;
            infoBlock.HorizontalAlignment = HorizontalAlignment.Right;
            infoBlock.Margin = new Thickness( 0 , 0 , 0 , progressBarWidth );
            infoBlock.Width = infoBlockWidth;
            plottingGrid.Children.Add( infoBlock );

            // Create SlopePlotCanvas object for drawing surface
            SlopePlotCanvas drawingCanvas = new SlopePlotCanvas( canvas );
            drawingCanvas.Background = Brushes.White;
            drawingCanvas.VerticalAlignment = VerticalAlignment.Stretch;
            drawingCanvas.HorizontalAlignment = HorizontalAlignment.Stretch;
            drawingCanvas.Margin = new Thickness( axisWidth , 0 , infoBlockWidth , axisWidth + progressBarWidth );
            drawingCanvas.InitializeCanvas();
            plottingGrid.Children.Add( drawingCanvas );
        }

        private void pan_Click ( object sender , RoutedEventArgs e )
        {
            Grid plottingGrid = contentGrid.Children[1] as Grid;

            SlopePlotCanvas currCanvas = null;
            if ( plottingGrid != null )
            {
                currCanvas = plottingGrid.Children[3] as SlopePlotCanvas;
            }

            if ( currCanvas != null )
            {
                currCanvas.CancelDrawing();
                currCanvas.Cursor = ((TextBlock) (((MainWindow) ((Grid) ((TabControl) ((TabItem) ((Grid) canvas.Parent).Parent).Parent).Parent).Parent).Resources["handCursor"])).Cursor;
                currCanvas.DrawMode = DrawModes.Pan;
            }
        }

        private void zoomIn_Click ( object sender , RoutedEventArgs e )
        {
            Grid plottingGrid = contentGrid.Children[1] as Grid;

            SlopePlotCanvas currCanvas = null;
            if ( plottingGrid != null )
            {
                currCanvas = plottingGrid.Children[3] as SlopePlotCanvas;
            }

            if ( currCanvas != null )
            {
                currCanvas.Zoom( 1.1 , new Point( 0.5 * currCanvas.ActualWidth , 0.5 * currCanvas.ActualHeight ) );
            }
        }

        private void zoomOut_Click ( object sender , RoutedEventArgs e )
        {
            Grid plottingGrid = contentGrid.Children[1] as Grid;

            SlopePlotCanvas currCanvas = null;
            if ( plottingGrid != null )
            {
                currCanvas = plottingGrid.Children[3] as SlopePlotCanvas;
            }

            if ( currCanvas != null )
            {
                currCanvas.Zoom( 1/1.1 , new Point( 0.5 * currCanvas.ActualWidth , 0.5 * currCanvas.ActualHeight ) );
            }
        }

        private void zoomArea_Click ( object sender , RoutedEventArgs e )
        {
            Grid plottingGrid = contentGrid.Children[1] as Grid;

            SlopePlotCanvas currCanvas = null;
            if ( plottingGrid != null )
            {
                currCanvas = plottingGrid.Children[3] as SlopePlotCanvas;
            }

            if ( currCanvas != null )
            {
                currCanvas.CancelDrawing();
                currCanvas.Cursor = ((TextBlock) (((MainWindow) ((Grid) ((TabControl) ((TabItem) ((Grid) canvas.Parent).Parent).Parent).Parent).Parent).Resources["zoomAreaCursor"])).Cursor;
                currCanvas.DrawMode = DrawModes.ZoomArea;
            }
        }

        private void zoomAll_Click ( object sender , RoutedEventArgs e )
        {
            Grid plottingGrid = contentGrid.Children[1] as Grid;

            SlopePlotCanvas currCanvas = null;
            if ( plottingGrid != null )
            {
                currCanvas = plottingGrid.Children[3] as SlopePlotCanvas;
            }

            if ( currCanvas != null )
            {
                currCanvas.CentreAndFitExtents( true );
            }
        }

        private void scale_Click ( object sender , RoutedEventArgs e )
        {
            Grid plottingGrid = contentGrid.Children[1] as Grid;

            SlopePlotCanvas currCanvas = null;
            if ( plottingGrid != null )
            {
                currCanvas = plottingGrid.Children[3] as SlopePlotCanvas;
            }
            if ( currCanvas == null ) return;

            // Obtain currently selected scale MenuItem (if there is one)
            // and uncheck all scale MenuItems
            MenuItem oldScale = null;
            foreach ( MenuItem scale in scaleList.Items )
            {
                if ( scale.IsChecked )
                {
                    scale.IsChecked = false;
                    oldScale = scale;
                }
            }

            // Cast sending scale MenuItem and set it to checked
            MenuItem newScale = sender as MenuItem;
            if ( newScale != null )
            {
                newScale.IsChecked = true;
            }

            // If the new value is different from the previous,
            // or the user would like to specify a custom scale
            if ( newScale != oldScale || newScale == scCustom )
            {
                // Select from list of default scales, or set Custom
                double desiredScale;
                switch ( newScale.Name )
                {
                    // 1000:1
                    case "sc1000":
                        currCanvas.ScaleType = Scales.sc1000;
                        desiredScale = 1000;
                        break;
                    // 800:1
                    case "sc800":
                        currCanvas.ScaleType = Scales.sc800;
                        desiredScale = 800;
                        break;

                    // 600:1
                    case "sc600":
                        currCanvas.ScaleType = Scales.sc600;
                        desiredScale = 600;
                        break;

                    // 500:1
                    case "sc500":
                        currCanvas.ScaleType = Scales.sc500;
                        desiredScale = 500;
                        break;

                    // 400:1
                    case "sc400":
                        currCanvas.ScaleType = Scales.sc400;
                        desiredScale = 400;
                        break;

                    // 300:1
                    case "sc300":
                        currCanvas.ScaleType = Scales.sc300;
                        desiredScale = 300;
                        break;

                    // 200:1
                    case "sc200":
                        currCanvas.ScaleType = Scales.sc200;
                        desiredScale = 200;
                        break;

                    // 150:1
                    case "sc150":
                        currCanvas.ScaleType = Scales.sc150;
                        desiredScale = 150;
                        break;

                    // 100:1
                    case "sc100":
                        currCanvas.ScaleType = Scales.sc100;
                        desiredScale = 100;
                        break;

                    // 50:1
                    case "sc50":
                        currCanvas.ScaleType = Scales.sc50;
                        desiredScale = 50;
                        break;

                    // 25:1
                    case "sc25":
                        currCanvas.ScaleType = Scales.sc25;
                        desiredScale = 25;
                        break;

                    // 10:1
                    case "sc10":
                        currCanvas.ScaleType = Scales.sc10;
                        desiredScale = 10;
                        break;

                    // 5:1
                    case "sc5":
                        currCanvas.ScaleType = Scales.sc5;
                        desiredScale = 5;
                        break;

                    // 2:1
                    case "sc2":
                        currCanvas.ScaleType = Scales.sc2;
                        desiredScale = 2;
                        break;

                    // 1:1
                    case "sc1":
                        currCanvas.ScaleType = Scales.sc1;
                        desiredScale = 1;
                        break;

                    // Custom
                    default:
                        // Create modal dialog box to prompt user for new scale
                        CustomScaleDialog dlg = new CustomScaleDialog( this );
                        dlg.ShowDialog();

                        // If the dialog returned OK, set new scale
                        if ( dlg.DialogResult == true )
                        {
                            desiredScale = Double.Parse( dlg.scale.Text );
                            currCanvas.ScaleType = Scales.Custom;
                        }
                        // If the dialog returned Cancel, reset to previous scale
                        else
                        {
                            desiredScale = currCanvas.Scale;
                            newScale.IsChecked = false;
                            oldScale.IsChecked = true;
                        }

                        break;
                }
                // Zoom to desired scale and centre content WITHOUT zoom
                currCanvas.Zoom( currCanvas.Scale / desiredScale , new Point( 0.5 * currCanvas.ActualHeight , 0.5 * currCanvas.ActualWidth ) );
                currCanvas.CentreAndFitExtents( false );
            }
        }

        private void axisOptions_Click ( object sender , RoutedEventArgs e )
        {
            Grid plottingGrid = contentGrid.Children[1] as Grid;

            SlopePlotCanvas currCanvas = null;
            if ( plottingGrid != null )
            {
                currCanvas = plottingGrid.Children[3] as SlopePlotCanvas;
            }
            if ( currCanvas == null ) return;

            AxisOptionsDialog dlg = new AxisOptionsDialog( this , currCanvas );
            dlg.ShowDialog();
        }

        private void plotDeformedMesh_Click ( object sender , RoutedEventArgs e )
        {
            Grid plottingGrid = contentGrid.Children[1] as Grid;

            SlopePlotCanvas currCanvas = null;
            if ( plottingGrid != null )
            {
                currCanvas = plottingGrid.Children[3] as SlopePlotCanvas;
            }
            if ( currCanvas == null ) return;

            currCanvas.PlotDeformedMesh();
        }

        private void plotDisplacementVectors_Click ( object sender , RoutedEventArgs e )
        {

        }

        private void plotPlasticPoints_Click ( object sender , RoutedEventArgs e )
        {

        }

        private void plotSmoothStress_Click ( object sender , RoutedEventArgs e )
        {

        }

        private void plotUnSmoothStress_Click ( object sender , RoutedEventArgs e )
        {

        }
    }
}
