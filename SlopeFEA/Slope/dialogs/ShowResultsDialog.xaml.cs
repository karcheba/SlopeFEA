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
using System.IO;

namespace SlopeFEA
{
    /// <summary>
    /// Interaction logic for ShowResultsDialog.xaml
    /// </summary>
    public partial class ShowResultsDialog : Window
    {
        private SlopeCanvas canvas;
        private List<DisplayCircularSurface> runSurfaces;
        private AnalysisType analysisType;
        private string resultsPath;
        private string[] resultsPathSplit;
        private List<string> results;
        private double scale, dpiX, dpiY, originOffsetX, originOffsetY, factor, actualHeight;

        public ShowResultsDialog(Window owner)
        {
            InitializeComponent();

            // intialize canvas information
            this.Owner = owner;
            canvas = (SlopeCanvas)((Grid)((TabControl)((Grid)this.Owner.Content).Children[2]).SelectedContent).Children[2];
            runSurfaces = canvas.RunSurfaces;
            analysisType = canvas.AnalysisType;
            scale = canvas.Scale;
            dpiX = canvas.DpiX;
            dpiY = canvas.DpiY;
            originOffsetX = canvas.OriginOffsetX;
            originOffsetY = canvas.OriginOffsetY;
            actualHeight = canvas.ActualHeight;
            switch (canvas.Units)
            {
                case Units.Metres: factor = 0.0254; break;
                case Units.Millimetres: factor = 25.4; break;
                case Units.Feet: factor = 1.0 / 12.0; break;
                default: factor = 1.0; break;
            }

            // get path to results file
            resultsPath = canvas.FilePath;
            resultsPathSplit = resultsPath.Split('.');

            // check type of analysis and set appropriate file extension
            switch (analysisType)
            {
                case AnalysisType.Bishop: resultsPathSplit[resultsPathSplit.Length - 1] = "bish"; break;
                default: resultsPathSplit[resultsPathSplit.Length - 1] = "rfem"; break;
            }
            resultsPath = string.Join(".", resultsPathSplit);

            // error catch to make sure results file exists
            if (File.Exists(resultsPath))
            {
                // BUILD RESULTS SELECTION LIST

                // add critical run item to list
                selectRun.Items.Add("Critical Run");

                // read in all existing output file contents
                results = new List<string>(File.ReadAllLines(resultsPath));

                // find first run in results file
                int irun = results.FindIndex(delegate(string s) { return s.Contains("Run #"); });
                int run = 1;

                // loop through results file, adding an item for each run
                while (irun != -1)
                {
                    selectRun.Items.Add(String.Format("Run #{0}", run++));
                    irun = results.FindIndex(irun + 1, delegate(string s) { return s.Contains("Run #"); });
                }

                // set selected item to critical run
                selectRun.SelectedIndex = 0;
            }
            else
            {
                runResults.Text += "Error! No results file found.\n";
            }
        }

        private void selectRun_SelectionChanged(object sender, SelectionChangedEventArgs e)
        {
            // clear results listbox
            runResults.Text = "";

            // error catch in case results file has been moved/deleted
            if (!File.Exists(resultsPath))
            {
                runResults.Text += "Error! No results file found.\n";
                return;
            }

            // clear currently displayed results from canvas
            for (int i = 0; i < runSurfaces.Count; i++) runSurfaces[i].Delete();
            runSurfaces.Clear();

            int run = 0;
            if (selectRun.SelectedIndex == 0)           // critical run
            {
                // find critical surface geometry
                int icritical = results.FindIndex(
                    delegate(string s) { return s.Contains("MOST CRITICAL SURFACE"); });

                // read run number
                run = int.Parse(results[icritical + 10].Split(new char[] { '\t' }, StringSplitOptions.RemoveEmptyEntries)[1]);
            }
            else                                        // numbered runs
            {
                // compute run number
                run = selectRun.SelectedIndex;
            }

            // find index of run number
            int irun = results.FindIndex(
                    delegate(string s) { return s.Contains(String.Format("Run #{0}", run)); });

            runResults.Text += results[irun] + "\n";        // add run number to results listbox
            runResults.Text += results[irun + 1] + "\n";    // add time stamp
            runResults.Text += results[irun + 2] + "\n";    // add headers

            // Read in local minimum surface data
            runResults.Text += results[irun + 3] + "\n";
            string[] surfacedata = results[irun + 3].Split(new char[] { '\t' }, StringSplitOptions.RemoveEmptyEntries);

            // Read in surface geometry
            double radius = double.Parse(surfacedata[2]);
            double xEnter = double.Parse(surfacedata[3]);
            double yEnter = double.Parse(surfacedata[4]);
            double xExit = double.Parse(surfacedata[5]);
            double yExit = double.Parse(surfacedata[6]);

            // Convert to screen pixel units
            radius = radius / (factor * scale) * dpiX;
            xEnter = xEnter / (factor * scale) * dpiX + originOffsetX;
            yEnter = actualHeight - (yEnter / (factor * scale) * dpiY + originOffsetY);
            xExit = xExit / (factor * scale) * dpiX + originOffsetX;
            yExit = actualHeight - (yExit / (factor * scale) * dpiY + originOffsetY);

            // Add to list of current run surfaces
            runSurfaces.Add(new DisplayCircularSurface(canvas,
                new Point(xEnter, yEnter), new Point(xExit, yExit), radius));

            // Indicate that it is a local critical surface with thicker line
            runSurfaces[0].IsLocalCritical = true;

            // Repeat for the rest of the current run surfaces
            for (int i = 4; i < 13; i++)
            {
                runResults.Text += results[irun + i] + "\n";

                surfacedata = results[irun + i].Split(new char[] { '\t' }, StringSplitOptions.RemoveEmptyEntries);

                radius = double.Parse(surfacedata[2]);
                xEnter = double.Parse(surfacedata[3]);
                yEnter = double.Parse(surfacedata[4]);
                xExit = double.Parse(surfacedata[5]);
                yExit = double.Parse(surfacedata[6]);

                radius = radius / (factor * scale) * dpiX;
                xEnter = xEnter / (factor * scale) * dpiX + originOffsetX;
                yEnter = actualHeight - (yEnter / (factor * scale) * dpiY + originOffsetY);
                xExit = xExit / (factor * scale) * dpiX + originOffsetX;
                yExit = actualHeight - (yExit / (factor * scale) * dpiY + originOffsetY);

                runSurfaces.Add(new DisplayCircularSurface(canvas,
                new Point(xEnter, yEnter), new Point(xExit, yExit), radius));
            }
        }
    }
}
