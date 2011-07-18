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
using System.Windows;

namespace SlopeFEA
{
    /// <summary>
    /// Interaction logic for FactorPointLoadDialog.xaml
    /// </summary>
    public partial class FactorPointLoadDialog : Window
    {
        private SlopeDefineCanvas canvas;
        private PointLoad load;

        public FactorPointLoadDialog ( SlopeDefineCanvas canvas , PointLoad load )
        {
            InitializeComponent();

            this.canvas = canvas;
            this.load = load;

            // get units dependent scaling factor and strings
            double factor;
            string coordUnits , loadUnits;
            switch ( canvas.Units )
            {
                case Units.Metres: factor = 0.0254; coordUnits = "m"; loadUnits = "kN"; break;
                case Units.Millimetres: factor = 25.4; coordUnits = "mm"; loadUnits = "kN"; break;
                case Units.Feet: factor = 1.0 / 12.0; coordUnits = "ft"; loadUnits = "lbf"; break;
                default: factor = 1.0; coordUnits = "in"; loadUnits = "lbf"; break;
            }

            // set units labels
            nodeUnits.Content = coordUnits;
            xLoadUnits.Content = loadUnits;
            yLoadUnits.Content = loadUnits;

            // set node coordinates
            double xCoord , yCoord;
            xCoord = (load.Node.Point.X - canvas.OriginOffsetX) / canvas.DpiX * factor * canvas.Scale;
            yCoord = (canvas.ActualHeight - load.Node.Point.Y - canvas.OriginOffsetY) / canvas.DpiY * factor * canvas.Scale;
            coords.Content = string.Format( "({0}, {1})" , Math.Round( xCoord , 2 ) , Math.Round( yCoord , 2 ) );

            // set existing load values (if present)
            MaterialBlock parent = canvas.Substructs.Find( delegate( MaterialBlock mb ) { return mb.Material.Name != "NULL" && mb.PointLoads.Contains( load ); } );
            isLoadedX.IsEnabled = parent != null && load.IsLoadedX;
            isLoadedX.IsChecked = xFactor.IsEnabled = parent != null && load.IsActiveX;
            xFactor.Text = string.Format( "{0}" , Math.Round( load.XFactor , 3 ) );
            xLoad.Text = string.Format( "{0}" , Math.Round( load.XLoad , 2 ) );
            isLoadedY.IsEnabled = parent != null && load.IsLoadedY;
            isLoadedY.IsChecked = yFactor.IsEnabled = parent != null && load.IsActiveY;
            yFactor.Text = string.Format( "{0}" , Math.Round( load.YFactor , 3 ) );
            yLoad.Text = string.Format( "{0}" , Math.Round( load.YLoad , 2 ) );
        }

        private void ok_Click ( object sender , RoutedEventArgs e )
        {
            double xFactorVal = 0 , yFactorVal = 0;
            bool isLoadedXVal = (bool) isLoadedX.IsChecked;
            bool isLoadedYVal = (bool) isLoadedY.IsChecked;

            if ( isLoadedXVal )
            {
                if ( !double.TryParse( xFactor.Text , out xFactorVal ) )
                {
                    MessageBox.Show( "Horizontal load factor must have a numeric value." , "Data Error" );
                    return;
                }
            }

            if ( isLoadedYVal )
            {
                if ( !double.TryParse( yFactor.Text , out yFactorVal ) )
                {
                    MessageBox.Show( "Vertical load factor must have a numeric value." , "Data Error" );
                    return;
                }
            }

            load.IsActiveX = isLoadedXVal;
            load.XFactor = xFactorVal;
            load.IsActiveY = isLoadedYVal;
            load.YFactor = yFactorVal;

            this.DialogResult = true;
        }


        private void isLoadedX_Checked ( object sender , RoutedEventArgs e )
        {
            xFactor.IsEnabled = true;
        }

        private void isLoadedX_Unchecked ( object sender , RoutedEventArgs e )
        {
            xFactor.IsEnabled = false;
        }

        private void isLoadedY_Checked ( object sender , RoutedEventArgs e )
        {
            yFactor.IsEnabled = true;
        }

        private void isLoadedY_Unchecked ( object sender , RoutedEventArgs e )
        {
            yFactor.IsEnabled = false;
        }
    }
}
