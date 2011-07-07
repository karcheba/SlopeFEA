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
using System.Collections.Generic;

namespace SlopeFEA
{
    /// <summary>
    /// Interaction logic for SetFixityDialog.xaml
    /// </summary>
    public partial class SetFixityDialog : Window
    {
        private SlopeCanvas canvas;
        private LineConstraint lc;
        private DrawingPoint p;

        public SetFixityDialog ( SlopeCanvas canvas , LineConstraint lc )
        {
            InitializeComponent();

            this.canvas = canvas;

            this.lc = lc;
            isFixedX.IsChecked = lc.IsFixedX;
            isFixedY.IsChecked = lc.IsFixedY;
        }

        public SetFixityDialog ( SlopeCanvas canvas , DrawingPoint p )
        {
            InitializeComponent();

            this.canvas = canvas;

            this.p = p;
            isFixedX.IsChecked = p.IsFixedX;
            isFixedY.IsChecked = p.IsFixedY;

            List<LineConstraint> attachedLCs = new List<LineConstraint>();
            canvas.MaterialBlocks.ForEach(
                delegate( MaterialBlock mb )
                {
                    attachedLCs.AddRange( mb.LineConstraints.FindAll( delegate( LineConstraint lc ) { return lc.Nodes.Contains( p ); } ) );
                } );
            attachedLCs.ForEach(
                delegate( LineConstraint lc )
                {
                    if ( lc.IsFixedX ) isFixedX.IsEnabled = false;
                    if ( lc.IsFixedY ) isFixedY.IsEnabled = false;
                } );
            attachedLCs.Clear();
        }

        private void ok_Click ( object sender , RoutedEventArgs e )
        {
            if ( lc != null )
            {
                lc.IsFixedX = (bool) isFixedX.IsChecked;
                lc.IsFixedY = (bool) isFixedY.IsChecked;

                List<LineConstraint> existingLCs = new List<LineConstraint>();
                existingLCs.Add( lc );
                DrawingPoint endNode = lc.Nodes[0];
                foreach ( MaterialBlock mb in canvas.MaterialBlocks )
                {
                    existingLCs.AddRange( mb.LineConstraints.FindAll( delegate( LineConstraint lc0 ) { return !existingLCs.Contains( lc0 ) && lc0.Nodes.Contains( endNode ); } ) );
                }
                endNode.IsFixedX = endNode.IsFixedY = false;
                existingLCs.ForEach( delegate( LineConstraint lc0 ) { endNode.IsFixedX = endNode.IsFixedX || lc0.IsFixedX; endNode.IsFixedY = endNode.IsFixedY || lc0.IsFixedY; } );
                existingLCs.Clear();

                existingLCs.Add( lc );
                endNode = lc.Nodes[1];
                foreach ( MaterialBlock mb in canvas.MaterialBlocks )
                {
                    existingLCs.AddRange( mb.LineConstraints.FindAll( delegate( LineConstraint lc0 ) { return !existingLCs.Contains( lc0 ) && lc0.Nodes.Contains( endNode ); } ) );
                }
                endNode.IsFixedX = endNode.IsFixedY = false;
                existingLCs.ForEach( delegate( LineConstraint lc0 ) { endNode.IsFixedX = endNode.IsFixedX || lc0.IsFixedX; endNode.IsFixedY = endNode.IsFixedY || lc0.IsFixedY; } );
                existingLCs.Clear();
            }
            else if ( p != null )
            {
                p.IsFixedX = (bool) isFixedX.IsChecked;
                p.IsFixedY = (bool) isFixedY.IsChecked;
            }

            this.DialogResult = true;
        }
    }
}
