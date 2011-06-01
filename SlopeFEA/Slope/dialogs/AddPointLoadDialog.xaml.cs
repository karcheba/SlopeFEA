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
    /// Interaction logic for AddPointLoadDialog.xaml
    /// </summary>
    public partial class AddPointLoadDialog : Window
    {
        private SlopeCanvas canvas;
        private PointLoad load;

        public AddPointLoadDialog(SlopeCanvas canvas, PointLoad load)
        {
            InitializeComponent();

            this.canvas = canvas;
            this.load = load;

            // get units dependent scaling factor and strings
            double factor;
            string coordUnits, loadUnits;
            switch (canvas.Units)
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
            double xCoord, yCoord;
            xCoord = (load.Node.Point.X - canvas.OriginOffsetX) / canvas.DpiX * factor * canvas.Scale;
            yCoord = (canvas.ActualHeight - load.Node.Point.Y - canvas.OriginOffsetY) / canvas.DpiY * factor * canvas.Scale;
            coords.Content = String.Format("({0}, {1})", Math.Round(xCoord, 2), Math.Round(yCoord, 2));

            // set existing load values (if present)
            isLoadedX.IsChecked = xLoad.IsEnabled = load.IsLoadedX;
            xLoad.Text = String.Format("{0}", Math.Round(load.XLoad, 2));
            isLoadedY.IsChecked = yLoad.IsEnabled = load.IsLoadedY;
            yLoad.Text = String.Format("{0}", Math.Round(load.YLoad, 2));
        }

        private void ok_Click(object sender, RoutedEventArgs e)
        {
            double xLoadVal = 0, yLoadVal = 0;
            bool isLoadedXVal = (bool)isLoadedX.IsChecked;
            bool isLoadedYVal = (bool)isLoadedY.IsChecked;

            if (isLoadedXVal)
            {
                if (!double.TryParse(xLoad.Text, out xLoadVal))
                {
                    MessageBox.Show("Horizontal load must have a numeric value.", "Data Error");
                    return;
                }
            }

            if (isLoadedYVal)
            {
                if (!double.TryParse(yLoad.Text, out yLoadVal))
                {
                    MessageBox.Show("Vertical load must have a numeric value.", "Data Error");
                    return;
                }
            }

            load.ApplyLoad(isLoadedXVal, xLoadVal,
                            isLoadedYVal, yLoadVal);

            canvas.IsSaved = false;

            this.DialogResult = true;
        }


        private void isLoadedX_Checked(object sender, RoutedEventArgs e)
        {
            xLoad.IsEnabled = true;
        }

        private void isLoadedX_Unchecked(object sender, RoutedEventArgs e)
        {
            xLoad.IsEnabled = false;
        }

        private void isLoadedY_Checked(object sender, RoutedEventArgs e)
        {
            yLoad.IsEnabled = true;
        }

        private void isLoadedY_Unchecked(object sender, RoutedEventArgs e)
        {
            yLoad.IsEnabled = false;
        }
    }
}
