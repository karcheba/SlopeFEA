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
    /// Interaction logic for AxisOptionsDialog.xaml
    /// </summary>
    public partial class AxisOptionsDialog : Window
    {
        private SlopeCanvas canvas;

        public AxisOptionsDialog ( Window owner )
        {
            InitializeComponent();

            this.Owner = owner;

            canvas = (SlopeCanvas) ((Grid) ((TabControl) ((Grid) this.Owner.Content).Children[2]).SelectedContent).Children[2];

            xMax.Text = String.Format( "{0}", Math.Round( canvas.XAxisMax, 2 ) );
            xMin.Text = String.Format( "{0}", Math.Round( canvas.XAxisMin, 2 ) );
            xMajor.Text = String.Format( "{0}", Math.Round( canvas.XMajorDivision, 2 ) );
            xMinor.Text = String.Format( "{0}", canvas.XMinorDivisions );

            yMax.Text = String.Format( "{0}", Math.Round( canvas.YAxisMax, 2 ) );
            yMin.Text = String.Format( "{0}", Math.Round( canvas.YAxisMin, 2 ) );
            yMajor.Text = String.Format( "{0}", Math.Round( canvas.YMajorDivision, 2 ) );
            yMinor.Text = String.Format( "{0}", canvas.YMinorDivisions );

            string units;
            switch ( canvas.Units )
            {
                case Units.Metres: units = "m"; break;
                case Units.Millimetres: units = "mm"; break;
                case Units.Feet: units = "ft"; break;
                default: units = "in"; break;
            }

            xMaxUnits.Content = units;
            xMinUnits.Content = units;
            xMajorUnits.Content = units;

            yMaxUnits.Content = units;
            yMinUnits.Content = units;
            yMajorUnits.Content = units;
        }

        private void ok_Click ( object sender, RoutedEventArgs e )
        {
            double xmax, xmin, xmajor;
            double ymax, ymin, ymajor;
            int xminor, yminor;

            if ( !double.TryParse( xMax.Text, out xmax ) )
            {
                MessageBox.Show( "Max X must be a numeric value.", "Error" );
                return;
            }

            if ( !double.TryParse( xMin.Text, out xmin ) )
            {
                MessageBox.Show( "Min X must be a numeric value.", "Error" );
                return;
            }

            if ( !double.TryParse( xMajor.Text, out xmajor ) )
            {
                MessageBox.Show( "Major X division must be a numeric value.", "Error" );
                return;
            }

            if ( xmajor <= 0 )
            {
                MessageBox.Show( "Major X division must be greater than zero.", "Error" );
                return;
            }

            if ( !int.TryParse( xMinor.Text, out xminor ) )
            {
                MessageBox.Show( "Minor X division count must be an integer.", "Error" );
                return;
            }

            if ( xminor <= 0 )
            {
                MessageBox.Show( "Minor X division count must be greater than zero.", "Error" );
                return;
            }

            if ( !double.TryParse( yMax.Text, out ymax ) )
            {
                MessageBox.Show( "Max Y must be a numeric value.", "Error" );
                return;
            }

            if ( !double.TryParse( yMin.Text, out ymin ) )
            {
                MessageBox.Show( "Min Y must be a numeric value.", "Error" );
                return;
            }

            if ( !double.TryParse( yMajor.Text, out ymajor ) )
            {
                MessageBox.Show( "Major Y division must be a numeric value.", "Error" );
                return;
            }

            if ( ymajor <= 0 )
            {
                MessageBox.Show( "Major Y division must be greater than zero.", "Error" );
                return;
            }

            if ( !int.TryParse( yMinor.Text, out yminor ) )
            {
                MessageBox.Show( "Minor Y division count must be an integer.", "Error" );
                return;
            }

            if ( yminor <= 0 )
            {
                MessageBox.Show( "Minor Y division count must be greater than zero.", "Error" );
                return;
            }

            if ( xmax < xmin )
            {
                double t = xmax;
                xmax = xmin;
                xmin = t;
            }

            if ( ymax < ymin )
            {
                double t = ymax;
                ymax = ymin;
                ymin = t;
            }

            if ( xmax < 0 ) xmax = 0;
            if ( xmin > 0 ) xmin = 0;
            if ( ymax < 0 ) ymax = 0;
            if ( ymin > 0 ) ymin = 0;

            if ( xmax == 0 && xmin == 0 )
            {
                MessageBox.Show( "Difference between minimum X and maximum X must be non-zero.", "Error" );
                return;
            }

            if ( ymax == 0 && ymin == 0 )
            {
                MessageBox.Show( "Difference between minimum Y and maximum Y must be non-zero.", "Error" );
                return;
            }

            double ratio, diff;

            ratio = xmax / xmajor;
            diff = ratio - Math.Truncate( ratio );
            xmax += Math.Abs( diff ) < 0.5 ? -diff * xmajor : Math.Sign( diff ) * (1 - Math.Abs( diff )) * xmajor;

            ratio = xmin / xmajor;
            diff = ratio - Math.Truncate( ratio );
            xmin += Math.Abs( diff ) < 0.5 ? -diff * xmajor : Math.Sign( diff ) * (1 - Math.Abs( diff )) * xmajor;

            ratio = ymax / xmajor;
            diff = ratio - Math.Truncate( ratio );
            ymax += Math.Abs( diff ) < 0.5 ? -diff * ymajor : Math.Sign( diff ) * (1 - Math.Abs( diff )) * ymajor;

            ratio = ymin / xmajor;
            diff = ratio - Math.Truncate( ratio );
            ymin += Math.Abs( diff ) < 0.5 ? -diff * ymajor : Math.Sign( diff ) * (1 - Math.Abs( diff )) * ymajor;

            canvas.XAxisMax = xmax;
            canvas.XAxisMin = xmin;
            canvas.XMajorDivision = xmajor;
            canvas.XMinorDivisions = xminor;

            canvas.YAxisMax = ymax;
            canvas.YAxisMin = ymin;
            canvas.YMajorDivision = ymajor;
            canvas.YMinorDivisions = yminor;

            canvas.BuildAxes();
            canvas.CentreAndFitExtents( true );

            canvas.IsSaved = false;

            this.DialogResult = true;
        }
    }
}
