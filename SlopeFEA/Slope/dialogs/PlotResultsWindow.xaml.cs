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
    /// Interaction logic for PlotResultsWindow.xaml
    /// </summary>
    public partial class PlotResultsWindow : Window
    {
        private SlopeCanvas canvas;

        public PlotResultsWindow ( Window owner , SlopeCanvas canvas )
        {
            InitializeComponent();

            this.Owner = owner;
            this.canvas = canvas;
        }

        private void pan_Click ( object sender , RoutedEventArgs e )
        {

        }

        private void zoomIn_Click ( object sender , RoutedEventArgs e )
        {

        }

        private void zoomOut_Click ( object sender , RoutedEventArgs e )
        {

        }

        private void zoomArea_Click ( object sender , RoutedEventArgs e )
        {

        }

        private void zoomAll_Click ( object sender , RoutedEventArgs e )
        {

        }

        private void scale_Click ( object sender , RoutedEventArgs e )
        {

        }

        private void axisOptions_Click ( object sender , RoutedEventArgs e )
        {

        }

        private void plotDeformedMesh_Click ( object sender , RoutedEventArgs e )
        {

        }

        private void plotDisplacementVectors_Click ( object sender , RoutedEventArgs e )
        {

        }

        private void plotPlasticPoints_Click ( object sender , RoutedEventArgs e )
        {

        }

        private void plotSmoothStress_Click ( object sender , RoutedEventArgs e )
        {

        }

        private void plotUnSmoothStress_Click ( object sender , RoutedEventArgs e )
        {

        }
    }
}
