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
    /// Interaction logic for VerifyDialog.xaml
    /// </summary>
    public partial class VerifyDialog : Window
    {
        private int errorCount = 0 , warningCount = 0;
        private SlopeCanvas canvas;
        string errNote , msg;

        public VerifyDialog ( Window owner )
        {
            InitializeComponent();

            this.Owner = owner;

            canvas = (SlopeCanvas) ((Grid) ((TabControl) ((Grid) this.Owner.Content).Children[2]).SelectedContent).Children[2];
        }

        public int ErrorCount { get { return this.errorCount; } }

        private void verify_Click ( object sender , RoutedEventArgs e )
        {
            double factor;
            string units;
            switch ( canvas.Units )
            {
                case Units.Metres: units = "m"; factor = 0.0254; break;
                case Units.Millimetres: units = "mm"; factor = 25.4; break;
                case Units.Feet: units = "ft"; factor = 1.0 / 12.0; break;
                default: units = "in"; factor = 1; break;
            }

            verifyMessages.Items.Add( "-------------------------------------------------------------" );
            verifyMessages.Items.Add( "Verifying problem input data..." );
            verifyMessages.Items.Add( "-------------------------------------------------------------" );

            verifyMessages.Items.Add( "" );

            errNote = canvas.HasBoundary ? "" : "***Error: ";
            msg = canvas.HasBoundary ? "has" : "does not have";
            if ( !canvas.HasBoundary ) errorCount++;
            verifyMessages.Items.Add( String.Format( "{0}Problem definition {1} analysis boundaries." , errNote , msg ) );

            int numIntersecting;
            double boundArea = 0;
            if ( canvas.HasBoundary )
            {
                errNote = canvas.Boundary.BoundaryPoints.Count > 2 ? "" : "***Error: ";
                msg = canvas.Boundary.BoundaryPoints.Count == 1 ? "" : "s";
                if ( canvas.Boundary.BoundaryPoints.Count <= 2 ) errorCount++;
                verifyMessages.Items.Add( String.Format( "{0}Analysis boundary has {1} point{2}." , errNote , canvas.Boundary.BoundaryPoints.Count , msg ) );

                numIntersecting = canvas.Boundary.CheckIntersecting();
                errNote = numIntersecting == 0 ? "" : "***Error: ";
                msg = numIntersecting == 1 ? "" : "s";
                if ( numIntersecting > 0 ) errorCount++;
                verifyMessages.Items.Add( String.Format( "{0}Analysis boundary has {1} intersecting line{2}." , errNote , numIntersecting , msg ) );

                double xMin = canvas.Boundary.XMin; bool minVert = canvas.Boundary.CheckXMaxMin( xMin );
                double xMax = canvas.Boundary.XMax; bool maxVert = canvas.Boundary.CheckXMaxMin( xMax );
                double yMin = canvas.Boundary.YMin; bool minHoriz = canvas.Boundary.CheckYMin( yMin );

                double yMaxXMin = canvas.Boundary.YMaxOfX( xMin , yMin );
                double yMaxXMax = canvas.Boundary.YMaxOfX( xMax , yMin );
                int upperResult = canvas.Boundary.CheckYMax( xMin , xMax , yMin );

                xMin = (xMin - canvas.OriginOffsetX) / canvas.DpiX * factor * canvas.Scale;
                verifyMessages.Items.Add( String.Format( "Minimum x value lies at:\tx = {0} {1}" , Math.Round( xMin , 2 ) , units ) );
                errNote = minVert ? "" : "***Error: ";
                msg = minVert ? "is" : "is not";
                if ( !minVert ) errorCount++;
                verifyMessages.Items.Add( String.Format( "{0}Minimum x boundary {1} vertical." , errNote , msg ) );

                xMax = (xMax - canvas.OriginOffsetX) / canvas.DpiX * factor * canvas.Scale;
                verifyMessages.Items.Add( String.Format( "Maximum x value lies at:\tx = {0} {1}" , Math.Round( xMax , 2 ) , units ) );
                errNote = maxVert ? "" : "***Error: ";
                msg = maxVert ? "is" : "is not";
                if ( !maxVert ) errorCount++;
                verifyMessages.Items.Add( String.Format( "{0}Maximum x boundary {1} vertical." , errNote , msg ) );

                yMin = ((canvas.ActualHeight - yMin) - canvas.OriginOffsetY) / canvas.DpiY * factor * canvas.Scale;
                verifyMessages.Items.Add( String.Format( "Minimum y value lies at:\ty = {0} {1}" , Math.Round( yMin , 2 ) , units ) );
                errNote = minHoriz ? "" : "***Error: ";
                msg = minHoriz ? "is" : "is not";
                if ( !minHoriz ) errorCount++;
                verifyMessages.Items.Add( String.Format( "{0}Minimum y boundary {1} horizontal." , errNote , msg ) );

                errNote = (upperResult == -2 || upperResult == -3 || upperResult == -4) ? "***Error: " : (upperResult == 0 ? "+++Warning: " : "");
                switch ( upperResult )
                {
                    case 1: msg = "Soil movement direction detected as left-to-right."; break;
                    case -1: msg = "Soil movement direction detected as right-to-left."; break;
                    case -2: msg = "Minimum x boundary has zero non-vertical exit lines"; errorCount++; break;
                    case -3: msg = "Upper surface direction inconsistent."; errorCount++; break;
                    case -4: msg = "Soil movement direction inconsistent"; errorCount++; break;
                    default: msg = "Soil movement direction not detected (upper surface is flat)."; warningCount++; break;
                }
                verifyMessages.Items.Add( String.Format( "{0}{1}" , errNote , msg ) );

                boundArea = canvas.Boundary.Area;
                boundArea *= Math.Pow( factor * canvas.Scale , 2 ) / (canvas.DpiX * canvas.DpiY);
                errNote = boundArea == 0 ? "***Error: " : "";
                if ( boundArea == 0 ) errorCount++;
                verifyMessages.Items.Add( String.Format( "{0}Analysis boundary area is {1} {2}^2" , errNote , Math.Round( boundArea , 2 ) , units ) );

                if ( errorCount == 0
                    && (canvas.AnalysisType == AnalysisType.Bishop || canvas.AnalysisType == AnalysisType.RFEM) )
                {
                    yMaxXMin = ((canvas.ActualHeight - yMaxXMin) - canvas.OriginOffsetY) / canvas.DpiY * factor * canvas.Scale;
                    yMaxXMax = ((canvas.ActualHeight - yMaxXMax) - canvas.OriginOffsetY) / canvas.DpiY * factor * canvas.Scale;

                    double yMaxUpper = Math.Max( yMaxXMin , yMaxXMax );
                    double yMinUpper = Math.Min( yMaxXMin , yMaxXMax );

                    double height = yMaxUpper - yMinUpper;

                    double yMaxCentre = yMaxUpper + 1.5 * height;

                    double maxRadius = yMaxCentre - yMin;

                    double xCentre = xMin + Math.Sqrt( Math.Pow( maxRadius , 2 ) - Math.Pow( yMaxXMin - yMaxCentre , 2 ) );

                    double xMaxCalc = xCentre + Math.Sqrt( Math.Pow( maxRadius , 2 ) - Math.Pow( yMaxXMax - yMaxCentre , 2 ) );

                    if ( xMaxCalc > xMax )
                    {
                        errorCount++;
                        verifyMessages.Items.Add( String.Format( "***Error: Analysis region not wide enough. Widen it by at least {0} {1}." , Math.Round( xMaxCalc - xMax , 2 ) , units ) );
                    }
                }
            }

            verifyMessages.Items.Add( "" );

            int numMaterials = canvas.MaterialTypes.Count;
            errNote = numMaterials == 0 ? "***Error: " : "";
            msg = numMaterials == 1 ? "" : "s";
            if ( numMaterials == 0 ) errorCount++;
            verifyMessages.Items.Add( String.Format( "{0}Problem definition has {1} material type{2}." , errNote , numMaterials , msg ) );

            verifyMessages.Items.Add( "" );

            int numMaterialBlocks = canvas.MaterialBlocks.Count;
            errNote = numMaterialBlocks == 0 ? "***Error: " : "";
            msg = numMaterialBlocks == 1 ? "" : "s";
            if ( numMaterialBlocks == 0 ) errorCount++;
            verifyMessages.Items.Add( String.Format( "{0}Problem definition has {1} material block{2}." , errNote , numMaterialBlocks , msg ) );

            verifyMessages.Items.Add( "" );

            double areaSum = 0;
            double materialArea = 0;
            for ( int i = 0 ; i < canvas.MaterialBlocks.Count ; i++ )
            {
                if ( canvas.MaterialBlocks[i].Material == null )
                {
                    errorCount++;
                    verifyMessages.Items.Add( String.Format( "***Error: Material block {0} does not have a material type assigned to it." , i + 1 ) );
                }
                else
                {
                    verifyMessages.Items.Add( String.Format( "Material block {0} contains {1}." , i + 1 , canvas.MaterialBlocks[i].Material ) );
                }

                errNote = canvas.MaterialBlocks[i].BoundaryPoints.Count > 2 ? "" : "***Error: ";
                msg = canvas.MaterialBlocks[i].BoundaryPoints.Count == 1 ? "" : "s";
                if ( canvas.MaterialBlocks[i].BoundaryPoints.Count <= 2 ) errorCount++;
                verifyMessages.Items.Add( String.Format( "{0}Material block {1} has {2} point{3}." , errNote , i + 1 , canvas.MaterialBlocks[i].BoundaryPoints.Count , msg ) );

                numIntersecting = canvas.MaterialBlocks[i].CheckIntersecting();
                errNote = numIntersecting == 0 ? "" : "***Error: ";
                msg = numIntersecting == 1 ? "" : "s";
                if ( numIntersecting > 0 ) errorCount++;
                verifyMessages.Items.Add( String.Format( "{0}Material block {1} has {2} intersecting line{3}." , errNote , i + 1 , numIntersecting , msg ) );

                materialArea = canvas.MaterialBlocks[i].Area;
                materialArea *= Math.Pow( factor * canvas.Scale , 2 ) / (canvas.DpiX * canvas.DpiY);
                areaSum += materialArea;
                errNote = materialArea == 0 ? "***Error: " : "";
                if ( materialArea == 0 ) errorCount++;
                verifyMessages.Items.Add( String.Format( "{0}Area of material block {1} is {2} {3}^2" , errNote , i + 1 , Math.Round( materialArea , 2 ) , units ) );

                verifyMessages.Items.Add( "" );
            }

            verifyMessages.Items.Add( String.Format( "Total material block area is {0} {1}^2" , Math.Round( areaSum , 2 ) , units ) );

            double areaDiff = areaSum - boundArea;
            errNote = Math.Abs( areaDiff ) > 1e-5 ? "***Error: " : "";
            if ( Math.Abs( areaDiff ) > 1e-5 ) errorCount++;
            verifyMessages.Items.Add( String.Format( "{0}Material block area - Analysis boundary area = {1} {2}^2" , errNote , Math.Round( areaDiff , 2 ) , units ) );

            verifyMessages.Items.Add( "" );

            verifyMessages.Items.Add( "-------------------------------------------------------------" );
            verifyMessages.Items.Add( "Problem verification complete:" );
            msg = errorCount == 1 ? "" : "s";
            verifyMessages.Items.Add( String.Format( "{0} error{1}" , errorCount , msg ) );
            msg = warningCount == 1 ? "" : "s";
            verifyMessages.Items.Add( String.Format( "{0} warning{1}" , warningCount , msg ) );
            verifyMessages.Items.Add( "-------------------------------------------------------------" );
        }
    }
}
