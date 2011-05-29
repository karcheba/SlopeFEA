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
