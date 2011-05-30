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

        public FEAParamsDialog (Window owner)
        {
            InitializeComponent();

            this.Owner = owner;

            canvas = (SlopeCanvas)((Grid)((TabControl)((Grid)this.Owner.Content).Children[2]).SelectedContent).Children[2];

            colWidth.Text = canvas.FEAParameters.ColWidth.ToString();
            rowHeight.Text = canvas.FEAParameters.RowHeight.ToString();

            string units;
            switch (canvas.Units)
            {
                case Units.Metres: units = "m"; break;
                case Units.Millimetres: units = "mm"; break;
                case Units.Feet: units = "ft"; break;
                default: units = "in"; break;
            }
            colWidthUnits.Content = units;
            rowHeightUnits.Content = units;
        }

        private void ok_Click(object sender, RoutedEventArgs e)
        {
            double newColWidth;
            if (!double.TryParse(colWidth.Text, out newColWidth) || newColWidth <= 0)
            {
                MessageBox.Show("Column width must be a positive number", "Error");
                return;
            }

            double newRowHeight;
            if (!double.TryParse(rowHeight.Text, out newRowHeight) || newRowHeight <= 0)
            {
                MessageBox.Show("Row height must be a positive number", "Error");
                return;
            }

            canvas.FEAParameters.ColWidth = newColWidth;
            canvas.FEAParameters.RowHeight = newRowHeight;

            canvas.IsSaved = false;
            canvas.IsVerified = false;

            this.DialogResult = true;
        }
    }
}
