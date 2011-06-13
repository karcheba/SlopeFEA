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

using System.Windows;
using System.Windows.Controls;

namespace SlopeFEA
{
    /// <summary>
    /// Interaction logic for FEAParamsDialog.xaml
    /// </summary>
    public partial class FEAParamsDialog : Window
    {
        private SlopeCanvas canvas;

        public FEAParamsDialog ( Window owner )
        {
            InitializeComponent();

            this.Owner = owner;

            canvas = (SlopeCanvas) ((Grid) ((TabControl) ((Grid) this.Owner.Content).Children[2]).SelectedContent).Children[2];

            colWidth.Text = canvas.FEAParameters.ColWidth.ToString();
            rowHeight.Text = canvas.FEAParameters.RowHeight.ToString();
            nStep.Text = canvas.FEAParameters.NStep.ToString();
            nIter.Text = canvas.FEAParameters.NIter.ToString();
            nPrint.Text = canvas.FEAParameters.NPrint.ToString();
            LFact.Text = canvas.FEAParameters.LFact.ToString();
            GFact.Text = canvas.FEAParameters.GFact.ToString();

            string units;
            switch ( canvas.Units )
            {
                case Units.Metres: units = "m"; break;
                case Units.Millimetres: units = "mm"; break;
                case Units.Feet: units = "ft"; break;
                default: units = "in"; break;
            }
            colWidthUnits.Content = units;
            rowHeightUnits.Content = units;
        }

        private void ok_Click ( object sender , RoutedEventArgs e )
        {
            double newColWidth;
            if ( !double.TryParse( colWidth.Text , out newColWidth ) || newColWidth <= 0 )
            {
                MessageBox.Show( "Column width must be a positive number" , "Error" );
                return;
            }

            double newRowHeight;
            if ( !double.TryParse( rowHeight.Text , out newRowHeight ) || newRowHeight <= 0 )
            {
                MessageBox.Show( "Row height must be a positive number" , "Error" );
                return;
            }

            int newNStep;
            if ( !int.TryParse( nStep.Text , out newNStep ) || newNStep < 1 )
            {
                MessageBox.Show( "Number of load steps must be an integer >= 1" , "Error" );
                return;
            }

            int newNIter;
            if ( !int.TryParse( nIter.Text , out newNIter ) || newNIter < 1 )
            {
                MessageBox.Show( "Number of iterations must be an integer >= 1" , "Error" );
                return;
            }

            int newNPrint;
            if ( !int.TryParse( nPrint.Text , out newNPrint ) || newNPrint < 1 )
            {
                MessageBox.Show( "Number of print lines must be an integer >= 1" , "Error" );
                return;
            }

            double newLFact;
            if ( !double.TryParse( LFact.Text , out newLFact ) || newLFact < 0 )
            {
                MessageBox.Show( "Load factor must be a number >= 0." , "Error" );
                return;
            }

            double newGFact;
            if ( !double.TryParse( GFact.Text , out newGFact ) || newGFact < 0 )
            {
                MessageBox.Show( "Gravity factor must be a number >= 0." , "Error" );
                return;
            }

            canvas.FEAParameters.ColWidth = newColWidth;
            canvas.FEAParameters.RowHeight = newRowHeight;
            canvas.FEAParameters.NStep = newNStep;
            canvas.FEAParameters.NIter = newNIter;
            canvas.FEAParameters.NPrint = newNPrint;
            canvas.FEAParameters.LFact = newLFact;
            canvas.FEAParameters.GFact = newGFact;

            canvas.IsSaved = false;
            canvas.IsVerified = false;

            this.DialogResult = true;
        }
    }
}
