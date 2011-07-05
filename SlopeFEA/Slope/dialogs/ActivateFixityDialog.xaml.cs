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

using System.Windows;

namespace SlopeFEA
{
    /// <summary>
    /// Interaction logic for ActivateFixityDialog.xaml
    /// </summary>
    public partial class ActivateFixityDialog : Window
    {
        private SlopeDefineCanvas canvas;
        private LineConstraint lc;
        private DrawingPoint p;

        public ActivateFixityDialog ( SlopeDefineCanvas canvas , LineConstraint lc )
        {
            InitializeComponent();

            this.canvas = canvas;

            this.lc = lc;
            isFixedX.IsEnabled = lc.IsFixedX;
            isFixedY.IsEnabled = lc.IsFixedY;
            isFixedX.IsChecked = lc.IsActiveX;
            isFixedY.IsChecked = lc.IsActiveY;
        }

        public ActivateFixityDialog ( SlopeDefineCanvas canvas , DrawingPoint p )
        {
            InitializeComponent();

            this.canvas = canvas;

            this.p = p;
            isFixedX.IsEnabled = p.IsFixedX;
            isFixedY.IsEnabled = p.IsFixedY;
            isFixedX.IsChecked = p.IsFixActiveX;
            isFixedY.IsChecked = p.IsFixActiveY;
        }

        private void ok_Click ( object sender , RoutedEventArgs e )
        {
            if ( lc != null )
            {
                lc.IsActiveX = (bool) isFixedX.IsChecked;
                lc.IsActiveY = (bool) isFixedY.IsChecked;
            }
            else if ( p != null )
            {
                p.IsFixActiveX = (bool) isFixedX.IsChecked;
                p.IsFixActiveY = (bool) isFixedY.IsChecked;
            }

            this.DialogResult = true;
        }
    }
}
