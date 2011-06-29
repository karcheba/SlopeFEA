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
    /// Interaction logic for SetMagnificationDialog.xaml
    /// </summary>
    public partial class SetMagnificationDialog : Window
    {
        private SlopePlotCanvas plotCanvas;

        public SetMagnificationDialog ( Window owner )
        {
            InitializeComponent();

            this.Owner = owner;

            plotCanvas = (SlopePlotCanvas) ((Grid) ((PlotResultsWindow) this.Owner).contentGrid.Children[1]).Children[3];
            mag.Text = String.Format( "{0}" , Math.Round( plotCanvas.Magnification , 2 ) );
        }

        private void ok_Click ( object sender , RoutedEventArgs e )
        {
            double output;
            if ( !double.TryParse( mag.Text , out output ) || output < 0 )
            {
                MessageBox.Show( "Must enter a positive numeric value." , "Error" );
                return;
                
            }

            plotCanvas.Magnification = output;

            // Close dialog
            this.DialogResult = true;
        }
    }
}
