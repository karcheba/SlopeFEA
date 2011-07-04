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
    /// Interaction logic for DefineAnalysisWindow.xaml
    /// </summary>
    public partial class DefineAnalysisWindow : Window
    {
        private SlopeCanvas canvas;

        public DefineAnalysisWindow ( Window owner , SlopeCanvas canvas )
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

            double axisWidth = 55 ,
                    inputBlockWidth = 250 ,
                    progressBarWidth = 10;

            // Create Grid object for x axis
            Grid xAxis = new Grid();
            xAxis.ClipToBounds = true;
            xAxis.Background = Brushes.WhiteSmoke;
            xAxis.VerticalAlignment = VerticalAlignment.Bottom;
            xAxis.HorizontalAlignment = HorizontalAlignment.Stretch;
            xAxis.Margin = new Thickness( axisWidth , 0 , inputBlockWidth , progressBarWidth );
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
            Grid inputBlock = new Grid();
            inputBlock.ClipToBounds = true;
            inputBlock.Background = Brushes.WhiteSmoke;
            inputBlock.VerticalAlignment = VerticalAlignment.Stretch;
            inputBlock.HorizontalAlignment = HorizontalAlignment.Right;
            inputBlock.Margin = new Thickness( 0 , 0 , 0 , progressBarWidth );
            inputBlock.Width = inputBlockWidth;
            plottingGrid.Children.Add( inputBlock );

            // Add plotting elements to infoBlock           // CHILD 0 = Border
            Border inputBlockBorder = new Border();
            inputBlockBorder.BorderBrush = Brushes.DimGray;
            inputBlockBorder.VerticalAlignment = VerticalAlignment.Stretch;
            inputBlockBorder.HorizontalAlignment = HorizontalAlignment.Stretch;
            inputBlockBorder.BorderThickness = new Thickness( 1 );
            inputBlockBorder.Margin = new Thickness( 0 );
            inputBlock.Children.Add( inputBlockBorder );

            // Create SlopePlotCanvas object for drawing surface
            SlopeDefineCanvas inputCanvas = new SlopeDefineCanvas( canvas );
            inputCanvas.Background = Brushes.White;
            inputCanvas.VerticalAlignment = VerticalAlignment.Stretch;
            inputCanvas.HorizontalAlignment = HorizontalAlignment.Stretch;
            inputCanvas.Margin = new Thickness( axisWidth , 0 , inputBlockWidth , axisWidth + progressBarWidth );
            inputCanvas.InitializeCanvas();
            plottingGrid.Children.Add( inputCanvas );
        }


        private void pan_Click ( object sender , RoutedEventArgs e )
        {
            Grid plottingGrid = contentGrid.Children[1] as Grid;

            SlopeDefineCanvas currCanvas = null;
            if ( plottingGrid != null )
            {
                currCanvas = plottingGrid.Children[3] as SlopeDefineCanvas;
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

            SlopeDefineCanvas currCanvas = null;
            if ( plottingGrid != null )
            {
                currCanvas = plottingGrid.Children[3] as SlopeDefineCanvas;
            }

            if ( currCanvas != null )
            {
                currCanvas.Zoom( 1.1 , new Point( 0.5 * currCanvas.ActualWidth , 0.5 * currCanvas.ActualHeight ) );
            }
        }

        private void zoomOut_Click ( object sender , RoutedEventArgs e )
        {
            Grid plottingGrid = contentGrid.Children[1] as Grid;

            SlopeDefineCanvas currCanvas = null;
            if ( plottingGrid != null )
            {
                currCanvas = plottingGrid.Children[3] as SlopeDefineCanvas;
            }

            if ( currCanvas != null )
            {
                currCanvas.Zoom( 1 / 1.1 , new Point( 0.5 * currCanvas.ActualWidth , 0.5 * currCanvas.ActualHeight ) );
            }
        }

        private void zoomArea_Click ( object sender , RoutedEventArgs e )
        {
            Grid plottingGrid = contentGrid.Children[1] as Grid;

            SlopeDefineCanvas currCanvas = null;
            if ( plottingGrid != null )
            {
                currCanvas = plottingGrid.Children[3] as SlopeDefineCanvas;
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

            SlopeDefineCanvas currCanvas = null;
            if ( plottingGrid != null )
            {
                currCanvas = plottingGrid.Children[3] as SlopeDefineCanvas;
            }

            if ( currCanvas != null )
            {
                currCanvas.CentreAndFitExtents( true );
            }
        }
    }
}
