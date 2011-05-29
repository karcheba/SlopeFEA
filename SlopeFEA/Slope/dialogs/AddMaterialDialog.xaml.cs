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
    /// Interaction logic for AddMaterialDialog.xaml
    /// </summary>
    public partial class AddMaterialDialog : Window
    {
        private SlopeCanvas canvas;

        public AddMaterialDialog(Window owner)
        {
            InitializeComponent();

            this.Owner = owner;

            canvas = (SlopeCanvas)((Grid)((TabControl)((Grid)this.Owner.Content).Children[2]).SelectedContent).Children[2];

            for (int i = 0; i < canvas.MaterialTypes.Count; i++)
            {
                materialList.Items.Add(canvas.MaterialTypes[i]);
            }

            switch (canvas.Units)
            {
                case Units.Metres:
                    cohUnits.Content = emodUnits.Content = "kPa";
                    gammaUnits.Content = "kN/m^3";
                    break;
                case Units.Millimetres:
                    cohUnits.Content = emodUnits.Content = "kPa";
                    gammaUnits.Content = "kN/m^3";
                    break;
                case Units.Feet:
                    cohUnits.Content = emodUnits.Content = "psi";
                    gammaUnits.Content = "pcf";
                    break;
                default:
                    cohUnits.Content = emodUnits.Content = "psi";
                    gammaUnits.Content = "pcf";
                    break;
            }

            colour.Fill = Brushes.Transparent;
        }

        private void materialList_SelectionChanged(object sender, SelectionChangedEventArgs e)
        {
            if (materialList.SelectedItem is MaterialType)
            {
                add.IsEnabled = false;
                modify.IsEnabled = true;
                remove.IsEnabled = true;

                MaterialType currMaterial = materialList.SelectedItem as MaterialType;

                colour.Fill = currMaterial.Fill;
                phi.Text = String.Format("{0}", Math.Round(currMaterial.Phi, 2));
                coh.Text = String.Format("{0}", Math.Round(currMaterial.Cohesion, 2));
                gamma.Text = String.Format("{0}", Math.Round(currMaterial.Gamma, 2));
                emod.Text = String.Format("{0}", Math.Round(currMaterial.Emod, 2));
                nu.Text = String.Format("{0}", Math.Round(currMaterial.Nu, 2));
            }
            else
            {
                if (this.IsInitialized)
                {
                    add.IsEnabled = true;
                    modify.IsEnabled = false;
                    remove.IsEnabled = false;

                    colour.Fill = Brushes.Transparent;
                    phi.Text = "";
                    coh.Text = "";
                    gamma.Text = "";
                    emod.Text = "";
                    nu.Text = "";
                }
            }
        }

        private void chooseColour_Click(object sender, RoutedEventArgs e)
        {
            System.Windows.Forms.ColorDialog colourDlg = new System.Windows.Forms.ColorDialog();
            colourDlg.AllowFullOpen = true;
            colourDlg.FullOpen = true;
            colourDlg.ShowDialog();

            Color newColour = new Color();
            newColour.A = colourDlg.Color.A;
            newColour.R = colourDlg.Color.R;
            newColour.G = colourDlg.Color.G;
            newColour.B = colourDlg.Color.B;

            colour.Fill = new SolidColorBrush(newColour);
        }

        private void add_Click(object sender, RoutedEventArgs e)
        {
            MaterialType newMaterial = new MaterialType();

            if (materialList.Text == "Add new material..." || materialList.Text == "")
            {
                MessageBox.Show("Must give the material a name.", "Error");
                return;
            }

            if (colour.Fill == Brushes.Transparent)
            {
                MessageBox.Show("Must define a colour.", "Error");
                return;
            }

            double newPhi;
            if (!double.TryParse(phi.Text, out newPhi) || newPhi < 0)
            {
                MessageBox.Show("Phi must be a positive number.", "Error");
                return;
            }

            double newCoh;
            if (!double.TryParse(coh.Text, out newCoh) || newCoh < 0)
            {
                MessageBox.Show("Cohesion must be a positive number.", "Error");
                return;
            }

            double newGamma;
            if (!double.TryParse(gamma.Text, out newGamma) || newGamma < 0)
            {
                MessageBox.Show("Gamma must be a positive number.", "Error");
                return;
            }

            double newEmod;
            if (!double.TryParse(emod.Text, out newEmod) || newEmod < 0)
            {
                MessageBox.Show("Elastic modulus must be a positive number.", "Error");
                return;
            }

            double newNu;
            if (!double.TryParse(nu.Text, out newNu) || newNu < 0 || newNu >= 0.5)
            {
                MessageBox.Show("Poisson's ratio must be a number in the range: 0 <= nu < 0.5", "Error");
                return;
            }

            newMaterial.Name = materialList.Text;
            newMaterial.Fill = colour.Fill;
            newMaterial.Phi = newPhi;
            newMaterial.Cohesion = newCoh;
            newMaterial.Gamma = newGamma;
            newMaterial.Emod = newEmod;
            newMaterial.Nu = newNu;

            canvas.MaterialTypes.Add(newMaterial);

            materialList.Items.Clear();

            materialList.Items.Add("Add new material...");

            for (int i = 0; i < canvas.MaterialTypes.Count; i++)
            {
                materialList.Items.Add(canvas.MaterialTypes[i]);
            }

            materialList.SelectedIndex = 0;
            materialList.Focus();

            canvas.IsSaved = false;
        }

        private void modify_Click(object sender, RoutedEventArgs e)
        {
            MaterialType currMaterial = materialList.SelectedItem as MaterialType;

            if (colour.Fill == Brushes.Transparent)
            {
                MessageBox.Show("Must define a colour.", "Error");
                return;
            }

            double newPhi;
            if (!double.TryParse(phi.Text, out newPhi) || newPhi < 0)
            {
                MessageBox.Show("Phi must be a positive number.", "Error");
                return;
            }

            double newCoh;
            if (!double.TryParse(coh.Text, out newCoh) || newCoh < 0)
            {
                MessageBox.Show("Cohesion must be a positive number.", "Error");
                return;
            }

            double newGamma;
            if (!double.TryParse(gamma.Text, out newGamma) || newGamma < 0)
            {
                MessageBox.Show("Gamma must be a positive number.", "Error");
                return;
            }

            double newEmod;
            if (!double.TryParse(emod.Text, out newEmod) || newEmod < 0)
            {
                MessageBox.Show("Elastic modulus must be a positive number.", "Error");
                return;
            }

            double newNu;
            if (!double.TryParse(nu.Text, out newNu) || newNu < 0 || newNu >= 0.5)
            {
                MessageBox.Show("Poisson's ratio must be a number in the range: 0 <= nu < 0.5", "Error");
                return;
            }

            currMaterial.Fill = colour.Fill;
            currMaterial.Phi = newPhi;
            currMaterial.Cohesion = newCoh;
            currMaterial.Gamma = newGamma;
            currMaterial.Emod = newEmod;
            currMaterial.Nu = newNu;

            materialList.SelectedIndex = 0;

            canvas.IsSaved = false;
        }

        private void remove_Click(object sender, RoutedEventArgs e)
        {
            MaterialType currMaterial = materialList.SelectedItem as MaterialType;

            if (currMaterial != null)
            {
                canvas.MaterialTypes.Remove(currMaterial);

                for (int i = 0; i < canvas.MaterialBlocks.Count; i++)
                {
                    if (canvas.MaterialBlocks[i].Material == currMaterial) canvas.MaterialBlocks[i].Material = null;
                }

                materialList.Items.Clear();

                materialList.Items.Add("Add new material...");

                for (int i = 0; i < canvas.MaterialTypes.Count; i++)
                {
                    materialList.Items.Add(canvas.MaterialTypes[i]);
                }

                materialList.SelectedIndex = 0;

                canvas.IsSaved = false;
                canvas.IsVerified = false;
            }
        }
    }
}
