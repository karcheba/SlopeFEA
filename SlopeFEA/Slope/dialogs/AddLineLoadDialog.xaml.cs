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
    /// Interaction logic for AddLoadDialog.xaml
    /// </summary>
    public partial class AddLineLoadDialog : Window
    {
        private SlopeCanvas canvas;
        private LineLoad load;

        public AddLineLoadDialog (SlopeCanvas canvas, LineLoad load)
        {
            InitializeComponent();

            this.canvas = canvas;
            this.load = load;
            //this.Owner = (Window)((Grid)((TabControl)((Grid)canvas.Parent).Parent).Parent).Parent;

            //canvas = (SlopeCanvas)((Grid)((TabControl)((Grid)this.Owner.Content).Children[2]).SelectedContent).Children[2];

            // get units dependent scaling factor and strings
            double factor;
            string coordUnits, loadUnits;
            switch (canvas.Units)
            {
                case Units.Metres: factor = 0.0254; coordUnits = "m"; loadUnits = "kN/m"; break;
                case Units.Millimetres: factor = 25.4; coordUnits = "mm"; loadUnits = "kN/mm"; break;
                case Units.Feet: factor = 1.0 / 12.0; coordUnits = "ft"; loadUnits = "lbf/ft"; break;
                default: factor = 1.0; coordUnits = "in"; loadUnits = "lbf/in"; break;
            }

            // set units labels
            node1Units.Content = coordUnits;
            node2Units.Content = coordUnits;
            nLoad1Units.Content = loadUnits;
            nLoad2Units.Content = loadUnits;
            tLoad1Units.Content = loadUnits;
            tLoad2Units.Content = loadUnits;

            // set node coordinates
            double xCoord, yCoord;
            xCoord = (load.Nodes[0].Point.X - canvas.OriginOffsetX) / canvas.DpiX * factor * canvas.Scale;
            yCoord = (canvas.ActualHeight - load.Nodes[0].Point.Y - canvas.OriginOffsetY) / canvas.DpiY * factor * canvas.Scale;
            coords1.Content = String.Format("({0}, {1})", Math.Round(xCoord, 2), Math.Round(yCoord, 2));
            xCoord = (load.Nodes[1].Point.X - canvas.OriginOffsetX) / canvas.DpiX * factor * canvas.Scale;
            yCoord = (canvas.ActualHeight - load.Nodes[1].Point.Y - canvas.OriginOffsetY) / canvas.DpiY * factor * canvas.Scale;
            coords2.Content = String.Format("({0}, {1})", Math.Round(xCoord, 2), Math.Round(yCoord, 2));

            // set existing load values (if present)
            isLoadedN.IsChecked = nLoad1.IsEnabled = nLoad2.IsEnabled = load.IsLoadedN;
            nLoad1.Text = String.Format("{0}", Math.Round(load.NLoad1, 2));
            nLoad2.Text = String.Format("{0}", Math.Round(load.NLoad2, 2));
            isLoadedT.IsChecked = tLoad1.IsEnabled = tLoad2.IsEnabled = load.IsLoadedT;
            tLoad1.Text = String.Format("{0}", Math.Round(load.TLoad1, 2));
            tLoad2.Text = String.Format("{0}", Math.Round(load.TLoad2, 2));
        }

        private void ok_Click (object sender, RoutedEventArgs e)
        {
            double nLoad1Val = 0, nLoad2Val = 0, tLoad1Val = 0, tLoad2Val = 0;
            bool isLoadedNVal = (bool)isLoadedN.IsChecked;
            bool isLoadedTVal = (bool)isLoadedT.IsChecked;

            if (isLoadedNVal)
            {
                if (!double.TryParse(nLoad1.Text, out nLoad1Val))
                {
                    MessageBox.Show("Normal load at node 1 must have a numeric value.", "Data Error");
                    return;
                }

                if (!double.TryParse(nLoad2.Text, out nLoad2Val))
                {
                    MessageBox.Show("Normal load at node 2 must have a numeric value.", "Data Error");
                    return;
                }
            }

            if (isLoadedTVal)
            {
                if (!double.TryParse(tLoad1.Text, out tLoad1Val))
                {
                    MessageBox.Show("Tangential load at node 1 must have a numeric value.", "Data Error");
                    return;
                }

                if (!double.TryParse(tLoad2.Text, out tLoad2Val))
                {
                    MessageBox.Show("Tangential load at node 2 must have a numeric value.", "Data Error");
                    return;
                }
            }

            load.ApplyLoad(isLoadedNVal, nLoad1Val, nLoad2Val,
                            isLoadedTVal, tLoad1Val, tLoad2Val);

            canvas.IsSaved = false;

            this.DialogResult = true;
        }


        private void isLoadedN_Checked (object sender, RoutedEventArgs e)
        {
            nLoad1.IsEnabled = true;
            nLoad2.IsEnabled = true;
        }

        private void isLoadedN_Unchecked (object sender, RoutedEventArgs e)
        {
            nLoad1.IsEnabled = false;
            nLoad2.IsEnabled = false;
        }

        private void isLoadedT_Checked (object sender, RoutedEventArgs e)
        {
            tLoad1.IsEnabled = true;
            tLoad2.IsEnabled = true;
        }

        private void isLoadedT_Unchecked (object sender, RoutedEventArgs e)
        {
            tLoad1.IsEnabled = false;
            tLoad2.IsEnabled = false;
        }
    }
}
