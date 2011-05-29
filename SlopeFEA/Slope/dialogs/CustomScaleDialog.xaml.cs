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
    /// Interaction logic for CustomScaleDialog.xaml
    /// </summary>
    public partial class CustomScaleDialog : Window
    {
        private SlopeCanvas canvas;


        /// <summary>
        /// Initializes the dialog box.
        /// </summary>
        public CustomScaleDialog(Window owner)
        {
            InitializeComponent();

            this.Owner = owner;

            canvas = (SlopeCanvas)((Grid)((TabControl)((Grid)this.Owner.Content).Children[2]).SelectedContent).Children[2];

            scale.Text = String.Format("{0}", Math.Round(canvas.Scale, 2));
        }

        /// <summary>
        /// Accept input and return to main window.
        /// </summary>
        private void ok_Click(object sender, RoutedEventArgs e)
        {
            double output;
            if (double.TryParse(scale.Text, out output) && output >= 0)
            {
                // Close dialog
                this.DialogResult = true;
            }
            else
            {
                MessageBox.Show("Must enter a positive numeric value.", "Error");
            }
        }
    }
}
