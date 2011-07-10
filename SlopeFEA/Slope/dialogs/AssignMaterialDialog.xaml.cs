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
using System.Windows.Controls;

namespace SlopeFEA
{
    /// <summary>
    /// Interaction logic for AssignMaterialDialog.xaml
    /// </summary>
    public partial class AssignMaterialDialog : Window
    {
        private SlopeCanvas canvas;
        private MaterialType selectedMaterial;

        public AssignMaterialDialog ( Window owner )
        {
            InitializeComponent();

            this.Owner = owner;

            canvas = (SlopeCanvas) ((Grid) ((TabControl) ((Grid) this.Owner.Content).Children[2]).SelectedContent).Children[2];

            canvas.MaterialTypes.ForEach( delegate( MaterialType mt ) { materialList.Items.Add( mt ); } );

            materialList.SelectedIndex = 0;

            switch ( canvas.Units )
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

        private void materialList_SelectionChanged ( object sender , SelectionChangedEventArgs e )
        {
            selectedMaterial = materialList.SelectedItem as MaterialType;

            if ( selectedMaterial != null )
            {
                colour.Fill = selectedMaterial.Fill;
                phi.Text = string.Format( "{0}" , Math.Round( selectedMaterial.Phi , 2 ) );
                coh.Text = string.Format( "{0}" , Math.Round( selectedMaterial.Cohesion , 2 ) );
                psi.Text = string.Format( "{0}" , Math.Round( selectedMaterial.Psi , 2 ) );
                gamma.Text = string.Format( "{0}" , Math.Round( selectedMaterial.Gamma , 2 ) );
                emod.Text = string.Format( "{0}" , Math.Round( selectedMaterial.Emod , 2 ) );
                nu.Text = string.Format( "{0}" , Math.Round( selectedMaterial.Nu , 2 ) );
            }
        }

        private void ok_Click ( object sender , RoutedEventArgs e )
        {
            this.DialogResult = true;
        }
    }
}
