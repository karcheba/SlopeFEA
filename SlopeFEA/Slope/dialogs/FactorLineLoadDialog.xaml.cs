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
    /// Interaction logic for FactorLineLoadDialog.xaml
    /// </summary>
    public partial class FactorLineLoadDialog : Window
    {
        private SlopeDefineCanvas canvas;
        private LineLoad load;

        public FactorLineLoadDialog ( SlopeDefineCanvas canvas , LineLoad load )
        {
            InitializeComponent();

            this.canvas = canvas;
            this.load = load;

            // get units dependent scaling factor and strings
            double factor;
            string coordUnits , loadUnits;
            switch ( canvas.Units )
            {
                case Units.Metres: factor = 0.0254; coordUnits = "m"; loadUnits = "kN/m"; break;
                case Units.Millimetres: factor = 25.4; coordUnits = "mm"; loadUnits = "kN/m"; break;
                case Units.Feet: factor = 1.0 / 12.0; coordUnits = "ft"; loadUnits = "lbf/ft"; break;
                default: factor = 1.0; coordUnits = "in"; loadUnits = "lbf/ft"; break;
            }

            // set units labels
            node1Units.Content = coordUnits;
            node2Units.Content = coordUnits;
            nLoad1Units.Content = loadUnits;
            nLoad2Units.Content = loadUnits;
            tLoad1Units.Content = loadUnits;
            tLoad2Units.Content = loadUnits;

            // set node coordinates
            double xCoord , yCoord;
            xCoord = (load.Nodes[0].Point.X - canvas.OriginOffsetX) / canvas.DpiX * factor * canvas.Scale;
            yCoord = (canvas.ActualHeight - load.Nodes[0].Point.Y - canvas.OriginOffsetY) / canvas.DpiY * factor * canvas.Scale;
            coords1.Content = string.Format( "({0}, {1})" , Math.Round( xCoord , 2 ) , Math.Round( yCoord , 2 ) );
            xCoord = (load.Nodes[1].Point.X - canvas.OriginOffsetX) / canvas.DpiX * factor * canvas.Scale;
            yCoord = (canvas.ActualHeight - load.Nodes[1].Point.Y - canvas.OriginOffsetY) / canvas.DpiY * factor * canvas.Scale;
            coords2.Content = string.Format( "({0}, {1})" , Math.Round( xCoord , 2 ) , Math.Round( yCoord , 2 ) );

            // set existing load values (if present)
            MaterialBlock parent = canvas.Substructs.Find( delegate( MaterialBlock mb ) { return mb.Material.Name != "NULL" && mb.LineLoads.Contains( load ); } );
            isLoadedN.IsEnabled = parent != null && load.IsLoadedN;
            isLoadedN.IsChecked = nFactor.IsEnabled = parent != null && load.IsActiveN;
            nFactor.Text = string.Format( "{0}" , Math.Round( load.NFactor , 3 ) );
            nLoad1.Text = string.Format( "{0}" , Math.Round( load.NLoad1 , 2 ) );
            nLoad2.Text = string.Format( "{0}" , Math.Round( load.NLoad2 , 2 ) );
            isLoadedT.IsEnabled = parent != null && load.IsLoadedT;
            isLoadedT.IsChecked = tFactor.IsEnabled = parent != null && load.IsActiveT;
            tFactor.Text = string.Format( "{0}" , Math.Round( load.TFactor , 3 ) );
            tLoad1.Text = string.Format( "{0}" , Math.Round( load.TLoad1 , 2 ) );
            tLoad2.Text = string.Format( "{0}" , Math.Round( load.TLoad2 , 2 ) );
        }

        private void ok_Click ( object sender , RoutedEventArgs e )
        {
            double nFactorVal = 0 , tFactorVal = 0;
            bool isLoadedNVal = (bool) isLoadedN.IsChecked;
            bool isLoadedTVal = (bool) isLoadedT.IsChecked;

            if ( isLoadedNVal )
            {
                if ( !double.TryParse( nFactor.Text , out nFactorVal ) )
                {
                    MessageBox.Show( "Normal load factor must have a numeric value." , "Data Error" );
                    return;
                }
            }

            if ( isLoadedTVal )
            {
                if ( !double.TryParse( tFactor.Text , out tFactorVal ) )
                {
                    MessageBox.Show( "Tangential load factor must have a numeric value." , "Data Error" );
                    return;
                }
            }

            load.IsActiveN = isLoadedNVal;
            load.NFactor = nFactorVal;
            load.IsActiveT = isLoadedTVal;
            load.TFactor = tFactorVal;

            this.DialogResult = true;
        }


        private void isLoadedN_Checked ( object sender , RoutedEventArgs e )
        {
            nFactor.IsEnabled = true;
        }

        private void isLoadedN_Unchecked ( object sender , RoutedEventArgs e )
        {
            nFactor.IsEnabled = false;
        }

        private void isLoadedT_Checked ( object sender , RoutedEventArgs e )
        {
            tFactor.IsEnabled = true;
        }

        private void isLoadedT_Unchecked ( object sender , RoutedEventArgs e )
        {
            tFactor.IsEnabled = false;
        }
    }
}
