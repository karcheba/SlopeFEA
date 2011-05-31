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

namespace SlopeFEA.dialogs
{
    /// <summary>
    /// Interaction logic for AddLoadDialog.xaml
    /// </summary>
    public partial class AddLoadDialog : Window
    {
        private SlopeCanvas canvas;

        public AddLoadDialog (Window owner, LineLoad load)
        {
            InitializeComponent();

            this.Owner = owner;

            canvas = (SlopeCanvas)((Grid)((TabControl)((Grid)this.Owner.Content).Children[2]).SelectedContent).Children[2];

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
            xLoad1Units.Content = loadUnits;
            xLoad2Units.Content = loadUnits;
            yLoad1Units.Content = loadUnits;
            yLoad2Units.Content = loadUnits;

            // set node coordinates
            double xCoord, yCoord;
            xCoord = (load.Nodes[0].Point.X - canvas.OriginOffsetX) / canvas.DpiX * factor * canvas.Scale;
            yCoord = (canvas.ActualHeight - load.Nodes[0].Point.Y - canvas.OriginOffsetY) / canvas.DpiY * factor * canvas.Scale;
            coords1.Content = String.Format("({0}, {1})", Math.Round(xCoord, 2), Math.Round(yCoord, 2));
            xCoord = (load.Nodes[1].Point.X - canvas.OriginOffsetX) / canvas.DpiX * factor * canvas.Scale;
            yCoord = (canvas.ActualHeight - load.Nodes[1].Point.Y - canvas.OriginOffsetY) / canvas.DpiY * factor * canvas.Scale;
            coords2.Content = String.Format("({0}, {1})", Math.Round(xCoord, 2), Math.Round(yCoord, 2));

            // set existing load values (if present)
            isLoadedX.IsChecked = xLoad1.IsEnabled = xLoad2.IsEnabled = load.IsLoadedX;
            xLoad1.Text = String.Format("{0}", Math.Round(load.XLoad1, 2));
            xLoad2.Text = String.Format("{0}", Math.Round(load.XLoad2, 2));
            isLoadedY.IsChecked = yLoad1.IsEnabled = yLoad2.IsEnabled = load.IsLoadedY;
            yLoad1.Text = String.Format("{0}", Math.Round(load.YLoad1, 2));
            yLoad2.Text = String.Format("{0}", Math.Round(load.YLoad2, 2));
        }

        private void ok_Click (object sender, RoutedEventArgs e)
        {
            

            canvas.IsSaved = false;

            this.DialogResult = true;
        }
    }
}
