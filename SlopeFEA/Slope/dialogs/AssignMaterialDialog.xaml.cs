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
    /// Interaction logic for AssignMaterialDialog.xaml
    /// </summary>
    public partial class AssignMaterialDialog : Window
    {
        private SlopeCanvas canvas;
        private MaterialType selectedMaterial;

        public AssignMaterialDialog(Window owner)
        {
            InitializeComponent();

            this.Owner = owner;

            canvas = (SlopeCanvas)((Grid)((TabControl)((Grid)this.Owner.Content).Children[2]).SelectedContent).Children[2];

            for (int i = 0; i < canvas.MaterialTypes.Count; i++)
            {
                materialList.Items.Add(canvas.MaterialTypes[i]);
            }

            materialList.SelectedIndex = 0;

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
        }

        public MaterialType SelectedMaterial { get { return selectedMaterial; } }

        private void materialList_SelectionChanged(object sender, SelectionChangedEventArgs e)
        {
            selectedMaterial = materialList.SelectedItem as MaterialType;

            if (selectedMaterial != null)
            {
                colour.Fill = selectedMaterial.Fill;
                phi.Text = String.Format("{0}", Math.Round(selectedMaterial.Phi, 2));
                coh.Text = String.Format("{0}", Math.Round(selectedMaterial.Cohesion, 2));
                gamma.Text = String.Format("{0}", Math.Round(selectedMaterial.Gamma, 2));
                emod.Text = String.Format("{0}", Math.Round(selectedMaterial.Emod, 2));
                nu.Text = String.Format("{0}", Math.Round(selectedMaterial.Nu, 2));
            }
        }

        private void ok_Click(object sender, RoutedEventArgs e)
        {
            this.DialogResult = true;
        }
    }
}
