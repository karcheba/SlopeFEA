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
using System.IO;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Input;
using Microsoft.Win32;

namespace SlopeFEA
{
    /// <summary>
    /// Interaction logic for MainWindow.xaml
    /// </summary>
    public partial class MainWindow : Window
    {
        // -------------------------------
        // Command shortcuts
        // -------------------------------
        public static RoutedCommand commandNew = new RoutedCommand();
        public static RoutedCommand commandOpen = new RoutedCommand();
        public static RoutedCommand commandSave = new RoutedCommand();
        public static RoutedCommand commandSaveAll = new RoutedCommand();
        public static RoutedCommand commandExit = new RoutedCommand();
        public static RoutedCommand commandDelete = new RoutedCommand();
        public static RoutedCommand commandGenerateMesh = new RoutedCommand();
        public static RoutedCommand commandRunAnalysis = new RoutedCommand();
        
        private int untitledCount = 0;

        // -------------------------------
        // Window operations
        // -------------------------------

        /// <summary>
        /// (Constructor) Initializes main window and sets up shortcut keys.
        /// </summary>
        public MainWindow()
        {
            InitializeComponent();
            
            // Bind "New" command
            CommandBinding cb = new CommandBinding(commandNew, new_Executed);
            KeyGesture kg = new KeyGesture(Key.N, ModifierKeys.Control);
            InputBinding ib = new InputBinding(commandNew, kg);
            this.CommandBindings.Add(cb);
            this.InputBindings.Add(ib);

            // Bind "Open" command
            cb = new CommandBinding(commandOpen, open_Executed);
            kg = new KeyGesture(Key.O, ModifierKeys.Control);
            ib = new InputBinding(commandOpen, kg);
            this.CommandBindings.Add(cb);
            this.InputBindings.Add(ib);

            // Bind "Save" command
            cb = new CommandBinding(commandSave, save_Executed);
            kg = new KeyGesture(Key.S, ModifierKeys.Control);
            ib = new InputBinding(commandSave, kg);
            this.CommandBindings.Add(cb);
            this.InputBindings.Add(ib);

            // Bind "SaveAll" command
            cb = new CommandBinding(commandSaveAll, saveAll_Executed);
            kg = new KeyGesture(Key.S, ModifierKeys.Alt);
            ib = new InputBinding(commandSaveAll, kg);
            this.CommandBindings.Add(cb);
            this.InputBindings.Add(ib);

            // Bind "Exit" command
            cb = new CommandBinding(commandExit, exit_Executed);
            kg = new KeyGesture(Key.X, ModifierKeys.Control);
            ib = new InputBinding(commandExit, kg);
            this.CommandBindings.Add(cb);
            this.InputBindings.Add(ib);

            // Bind "Delete" command
            cb = new CommandBinding(commandDelete, deleteBlock_Executed);
            kg = new KeyGesture(Key.Delete);
            ib = new InputBinding(commandDelete, kg);
            this.CommandBindings.Add(cb);
            this.InputBindings.Add(ib);

            // Bind "Generate Mesh" command
            cb = new CommandBinding(commandGenerateMesh, generateMesh_Executed);
            kg = new KeyGesture(Key.F7);
            ib = new InputBinding(commandGenerateMesh, kg);
            this.CommandBindings.Add(cb);
            this.InputBindings.Add(ib);

            // Bind "Run Analysis" command
            cb = new CommandBinding(commandRunAnalysis, run_Executed);
            kg = new KeyGesture(Key.F5);
            ib = new InputBinding(commandRunAnalysis, kg);
            this.CommandBindings.Add(cb);
            this.InputBindings.Add(ib);
        }

        /// <summary>
        /// Update size of content window if toolbar items are dragged to change the toolbar size.
        /// </summary>
        private void toolbarTray_SizeChanged(object sender, SizeChangedEventArgs e)
        {
            windowManager.Margin = new Thickness(0, menuDock.ActualHeight + toolbarTray.ActualHeight, 0, windowManager.Margin.Bottom);
        }



        // -------------------------------
        // File Menu operations
        // -------------------------------

        /// <summary>
        /// Exit the program (Click for menu, Executed for key shortcut)
        /// </summary>
        private void exit_Click(object sender, RoutedEventArgs e)
        {
            this.Close();
        }
        private void exit_Executed(object sender, ExecutedRoutedEventArgs e)
        {
            exit_Click(sender, (RoutedEventArgs)e);
        }


        /// <summary>
        /// Show some information about the program
        /// </summary>
        private void about_Click(object sender, RoutedEventArgs e)
        {
            MessageBox.Show("Slope 2011 (c) Brandon Karchewski, McMaster University", "About Slope 2011");
        }


        /// <summary>
        /// Create a new tabbed document (Click for menu, Executed for key shortcut)
        /// </summary>
        private void new_Click(object sender, RoutedEventArgs e)
        {
            // Create new tab, increment document count, and set new tab as current
            ClosableCanvasTabItem newTab = new ClosableCanvasTabItem();
            newTab.Title = "Untitled " + (++untitledCount);
            windowManager.Items.Add(newTab);
            windowManager.SelectedItem = newTab;

            // Set default scale menu check
            foreach (MenuItem scale in scaleList.Items)
            {
                scale.IsChecked = false;
            }
            scCustom.IsChecked = true;

            // Set default units menu check
            foreach (MenuItem units in unitsList.Items)
            {
                units.IsChecked = false;
            }
            unitsM.IsChecked = true;

            // Default mesh menu state
            showMesh.IsChecked = false;
            showMesh.IsEnabled = false;

            // Default grid state
            foreach (MenuItem grid in gridList.Items)
            {
                grid.IsChecked = false;
            }
        }
        private void new_Executed(object sender, ExecutedRoutedEventArgs e)
        {
            new_Click(sender, (RoutedEventArgs)e);
        }


        /// <summary>
        /// Load a saved .slp file (Click = menu, Executed = shortcut)
        /// </summary>
        private void open_Click(object sender, RoutedEventArgs e)
        {
            // Create default SaveFileDialog
            OpenFileDialog openDialog = new OpenFileDialog();
            openDialog.DefaultExt = "slp";
            openDialog.AddExtension = true;
            openDialog.Filter = "Slope Documents (.slp)|*.slp|Text Documents (.txt)|*.txt|All Files|*.*";
            openDialog.Title = "Open Slope File";
            openDialog.ValidateNames = true;

            // Check if SaveFileDialog returned successfully
            if (openDialog.ShowDialog().Value)
            {
                string[] path = openDialog.FileName.Split('.');
                if (!(path[path.Length - 1] == "slp"))
                {
                    MessageBox.Show("Incorrect file format. Input files _ must be of type \"*.slp\"", "Error");
                    return;
                }

                // Create new tab, increment document count, and set new tab as current
                ClosableCanvasTabItem newTab = new ClosableCanvasTabItem();
                newTab.Tag = openDialog.FileName;
                windowManager.Items.Add(newTab);
                windowManager.SelectedItem = newTab;

                // Set default scale menu check
                foreach (MenuItem scale in scaleList.Items)
                {
                    scale.IsChecked = false;
                }
                scCustom.IsChecked = true;

                // Set default units menu check
                foreach (MenuItem units in unitsList.Items)
                {
                    units.IsChecked = false;
                }

                // Default mesh menu state
                showMesh.IsChecked = false;
                showMesh.IsEnabled = false;

                // Default grid state
                foreach (MenuItem grid in gridList.Items)
                {
                    grid.IsChecked = false;
                }

                windowManager.UpdateLayout();

                SlopeCanvas newCanvas = (SlopeCanvas)((Grid)newTab.Content).Children[2];

                newCanvas.OpenInputFile(newTab.Tag as string);

                switch (newCanvas.Units)
                {
                    case Units.Metres: unitsM.IsChecked = true; break;
                    case Units.Millimetres: unitsMM.IsChecked = true; break;
                    case Units.Feet: unitsFT.IsChecked = true; break;
                    default: unitsIN.IsChecked = true; break;
                }
            }
        }
        private void open_Executed(object sender, ExecutedRoutedEventArgs e)
        {
            open_Click(sender, (RoutedEventArgs)e);
        }


        /// <summary>
        /// Save current tab if it has unsaved changes (Click = menu, Executed = shortcut)
        /// </summary>
        private void save_Click(object sender, RoutedEventArgs e)
        {
            // Get currently selected tab
            ClosableCanvasTabItem currTab = windowManager.SelectedItem as ClosableCanvasTabItem;

            SlopeCanvas currCanvas = null;
            if (currTab != null)
            {
                currCanvas = ((Grid)currTab.Content).Children[2] as SlopeCanvas;
            }
            else return;

            // Check if document has never been saved
            if (currTab.Tag == null) //(header.Contains("Untitled") && !header.Contains("."))
            {
                saveAs_Click(sender, e);
            }

            // Check if document has unsaved changes
            else if (!currCanvas.IsSaved)
            {
                // Save changes to existing file
                currCanvas.SaveInputFile(currTab.Tag as string);
            }
        }
        private void save_Executed(object sender, ExecutedRoutedEventArgs e)
        {
            save_Click(sender, (RoutedEventArgs)e);
        }


        /// <summary>
        /// Save current tab under a (possibly) new file name
        /// </summary>
        private void saveAs_Click(object sender, RoutedEventArgs e)
        {
            // Get currently selected tab
            ClosableCanvasTabItem currTab = windowManager.SelectedItem as ClosableCanvasTabItem;

            if (currTab != null)
            {
                // Create default SaveFileDialog
                SaveFileDialog saveDialog = new SaveFileDialog();
                saveDialog.DefaultExt = "slp";
                saveDialog.AddExtension = true;
                saveDialog.Filter = "Slope Documents (.slp)|*.slp|Text Documents (.txt)|*.txt|All Files|*.*";
                saveDialog.FileName = (currTab.Title.Split('.'))[0];
                saveDialog.OverwritePrompt = true;
                saveDialog.Title = "Save Slope File";
                saveDialog.ValidateNames = true;

                // Check if SaveFileDialog returned successfully
                if (saveDialog.ShowDialog().Value)
                {
                    SlopeCanvas currCanvas = (SlopeCanvas)((Grid)currTab.Content).Children[2];

                    // Set current tab's name and file path
                    currTab.Tag = saveDialog.FileName;
                    currCanvas.SaveInputFile(currTab.Tag as string);
                }
            }
        }


        /// <summary>
        /// Save all open tabs with unsaved changes (Click = menu, Executed = shortcut)
        /// </summary>
        private void saveAll_Click(object sender, RoutedEventArgs e)
        {
            // Get currently selected tab
            ClosableCanvasTabItem currTab = windowManager.SelectedItem as ClosableCanvasTabItem;

            // Iterate over open documents, applying Save command appropriately
            foreach (ClosableCanvasTabItem item in windowManager.Items)
            {
                windowManager.SelectedItem = item;
                save_Click(item, e);
            }

            // Reset selected tab to original
            windowManager.SelectedItem = currTab;
        }
        private void saveAll_Executed(object sender, ExecutedRoutedEventArgs e)
        {
            saveAll_Click(sender, (RoutedEventArgs)e);
        }

        /// <summary>
        /// Closes currently selected tab.
        /// </summary>
        private void close_Click(object sender, RoutedEventArgs e)
        {
            ClosableCanvasTabItem currTab = windowManager.SelectedItem as ClosableCanvasTabItem;
            if (currTab != null)
            {
                windowManager.Items.Remove(currTab);
            }

            if (windowManager.Items.Count == 0)
            {
                xStatus.Content = "X";
                yStatus.Content = "Y";
                scaleStatus.Content = "Scale";

                MenuItem fileMenu = (MenuItem)mainMenu.Items[0];
                    MenuItem save = (MenuItem)fileMenu.Items[3];
                    MenuItem saveAs = (MenuItem)fileMenu.Items[4];
                    MenuItem saveAll = (MenuItem)fileMenu.Items[5];
                    MenuItem close = (MenuItem)fileMenu.Items[7];
                    MenuItem closeAll = (MenuItem)fileMenu.Items[8];

                    save.IsEnabled = false;
                    saveAs.IsEnabled = false;
                    saveAll.IsEnabled = false;
                    close.IsEnabled = false;
                    closeAll.IsEnabled = false;

                MenuItem viewMenu = (MenuItem)mainMenu.Items[1];
                    MenuItem showMesh = (MenuItem)viewMenu.Items[11];

                    showMesh.IsEnabled = false;
                    showMesh.IsChecked = false;
                viewMenu.IsEnabled = false;

                MenuItem defineMenu = (MenuItem)mainMenu.Items[2];

                defineMenu.IsEnabled = false;

                MenuItem analyzeMenu = (MenuItem)mainMenu.Items[3];
                    MenuItem runAnalysis = (MenuItem)analyzeMenu.Items[0];
                    MenuItem stopAnalysis = (MenuItem)analyzeMenu.Items[1];
                    MenuItem generateMesh = (MenuItem)analyzeMenu.Items[3];
                    MenuItem clearMesh = (MenuItem)analyzeMenu.Items[4];

                    runAnalysis.IsEnabled = false;
                    stopAnalysis.IsEnabled = false;
                    generateMesh.IsEnabled = false;
                    clearMesh.IsEnabled = false;
                analyzeMenu.IsEnabled = false;

                MenuItem outputMenu = (MenuItem)mainMenu.Items[4];
                outputMenu.IsEnabled = false;
            }
        }

        /// <summary>
        /// Closes all open tabs and creates a new untitled document.
        /// </summary>
        private void closeAll_Click(object sender, RoutedEventArgs e)
        {
            while (windowManager.Items.Count > 0)
            {
                ClosableCanvasTabItem currTab = windowManager.SelectedItem as ClosableCanvasTabItem;
                if (currTab != null)
                {
                    windowManager.Items.Remove(currTab);
                }
            }

            xStatus.Content = "X";
            yStatus.Content = "Y";
            scaleStatus.Content = "Scale";

            MenuItem fileMenu = (MenuItem)mainMenu.Items[0];
                MenuItem save = (MenuItem)fileMenu.Items[3];
                MenuItem saveAs = (MenuItem)fileMenu.Items[4];
                MenuItem saveAll = (MenuItem)fileMenu.Items[5];
                MenuItem close = (MenuItem)fileMenu.Items[7];
                MenuItem closeAll = (MenuItem)fileMenu.Items[8];

                save.IsEnabled = false;
                saveAs.IsEnabled = false;
                saveAll.IsEnabled = false;
                close.IsEnabled = false;
                closeAll.IsEnabled = false;

            MenuItem viewMenu = (MenuItem)mainMenu.Items[1];
                MenuItem showMesh = (MenuItem)viewMenu.Items[11];

                showMesh.IsEnabled = false;
                showMesh.IsChecked = false;
            viewMenu.IsEnabled = false;

            MenuItem defineMenu = (MenuItem)mainMenu.Items[2];

            defineMenu.IsEnabled = false;

            MenuItem analyzeMenu = (MenuItem)mainMenu.Items[3];
                MenuItem runAnalysis = (MenuItem)analyzeMenu.Items[0];
                MenuItem stopAnalysis = (MenuItem)analyzeMenu.Items[1];
                MenuItem generateMesh = (MenuItem)analyzeMenu.Items[3];
                MenuItem clearMesh = (MenuItem)analyzeMenu.Items[4];

                runAnalysis.IsEnabled = false;
                stopAnalysis.IsEnabled = false;
                generateMesh.IsEnabled = false;
                clearMesh.IsEnabled = false;
            analyzeMenu.IsEnabled = false;

            MenuItem outputMenu = (MenuItem)mainMenu.Items[4];
            outputMenu.IsEnabled = false;
        }

        /// <summary>
        /// Zooms in on canvas content by 10%, centred on current canvas content centre.
        /// </summary>
        private void zoomIn_Click(object sender, RoutedEventArgs e)
        {
            Grid currGrid = windowManager.SelectedContent as Grid;

            SlopeCanvas currCanvas = null;
            if (currGrid != null)
            {
                currCanvas = currGrid.Children[2] as SlopeCanvas;
            }

            if (currCanvas != null)
            {
                currCanvas.Zoom(1.1, new Point(0.5 * currCanvas.ActualWidth, 0.5 * currCanvas.ActualHeight));
            }
        }

        /// <summary>
        /// Zooms out from canvas content by 10%, centred on current canvas content centre.
        /// </summary>
        private void zoomOut_Click(object sender, RoutedEventArgs e)
        {
            Grid currGrid = windowManager.SelectedContent as Grid;

            SlopeCanvas currCanvas = null;
            if (currGrid != null)
            {
                currCanvas = currGrid.Children[2] as SlopeCanvas;
            }

            if (currCanvas != null)
            {
                currCanvas.Zoom(1/1.1, new Point(0.5 * currCanvas.ActualWidth, 0.5 * currCanvas.ActualHeight));
            }
        }

        /// <summary>
        /// Begins ZoomArea mode, which allows the user to zoom to a selected rectangle.
        /// </summary>
        private void zoomArea_Click(object sender, RoutedEventArgs e)
        {
            Grid currGrid = windowManager.SelectedContent as Grid;
            
            SlopeCanvas currCanvas = null;
            if (currGrid != null)
            {
                currCanvas = currGrid.Children[2] as SlopeCanvas;
            }

            if (currCanvas != null)
            {
                currCanvas.ClearSelections();
                currCanvas.CancelDrawing();
                currCanvas.Cursor = ((TextBlock)this.Resources["zoomAreaCursor"]).Cursor;
                currCanvas.DrawMode = DrawModes.ZoomArea;
            }
        }

        /// <summary>
        /// Zooms canvas content to fit the greate of all existing content or specified axis extents.
        /// </summary>
        private void zoomAll_Click(object sender, RoutedEventArgs e)
        {
            Grid currGrid = windowManager.SelectedContent as Grid;

            SlopeCanvas currCanvas = null;
            if (currGrid != null)
            {
                currCanvas = currGrid.Children[2] as SlopeCanvas;
            }

            if (currCanvas != null)
            {
                // Centre WITH zoom
                currCanvas.CentreAndFitExtents(true);
            }
        }

        /// <summary>
        /// Begins pan mode, which allows the user to drag canvas content into view.
        /// </summary>
        private void pan_Click(object sender, RoutedEventArgs e)
        {
            Grid currGrid = windowManager.SelectedContent as Grid;

            SlopeCanvas currCanvas = null;
            if (currGrid != null)
            {
                currCanvas = currGrid.Children[2] as SlopeCanvas;
            }

            if (currCanvas != null)
            {
                currCanvas.ClearSelections();
                currCanvas.CancelDrawing();
                currCanvas.Cursor = ((TextBlock)this.Resources["handCursor"]).Cursor;
                currCanvas.DrawMode = DrawModes.Pan;
            }
        }

        /// <summary>
        /// Begins draw boundaries mode, which allows the user to specify the
        /// boundaries of the slope stability analysis problem graphically
        /// </summary>
        private void drawBounds_Click(object sender, RoutedEventArgs e)
        {
            Grid currGrid = windowManager.SelectedContent as Grid;

            SlopeCanvas currCanvas = null;
            if (currGrid != null)
            {
                currCanvas = currGrid.Children[2] as SlopeCanvas;
            }

            if (currCanvas != null)
            {
                // If boundaries already exist, prompt user to confirm drawing new
                // boundaries (this will delete existing bounds).
                MessageBoxResult newBoundary = MessageBoxResult.Yes;
                if (currCanvas.HasBoundary)
                {
                    newBoundary = MessageBox.Show("This will erase existing boundary. Continue?", "Erase Boundaries", MessageBoxButton.YesNo);
                }

                // Set drawing mode and set cursor to boxed cross
                if (newBoundary == MessageBoxResult.Yes)
                {
                    currCanvas.ClearSelections();
                    currCanvas.RemoveBoundary();
                    currCanvas.CancelDrawing();
                    currCanvas.DrawMode = DrawModes.Boundaries;
                    currCanvas.Cursor = ((TextBlock)this.Resources["drawCursor"]).Cursor;
                }
            }
        }

        /// <summary>
        /// Begins draw materials mode, which allows the user to specify
        /// blocks of material within the pre-specified analysis boundaries
        /// </summary>
        private void drawMaterials_Click(object sender, RoutedEventArgs e)
        {
            Grid currGrid = windowManager.SelectedContent as Grid;

            SlopeCanvas currCanvas = null;
            if (currGrid != null)
            {
                currCanvas = currGrid.Children[2] as SlopeCanvas;
            }

            if (currCanvas != null)
            {
                currCanvas.ClearSelections();
                currCanvas.CancelDrawing();
                currCanvas.DrawMode = DrawModes.Materials;
                currCanvas.Cursor = ((TextBlock)this.Resources["drawCursor"]).Cursor;
            }
        }

        /// <summary>
        /// Allows user to specify plotting extents and major/minor grid resolution
        /// </summary>
        private void axisOptions_Click(object sender, RoutedEventArgs e)
        {
            Grid currGrid = windowManager.SelectedContent as Grid;

            SlopeCanvas currCanvas = null;
            if (currGrid != null)
            {
                currCanvas = currGrid.Children[2] as SlopeCanvas;
            }
            if (currCanvas == null) return;

            AxisOptionsDialog dlg = new AxisOptionsDialog(this);
            dlg.ShowDialog();
        }

        /// <summary>
        /// Responds to clicks on scale related MenuItems
        /// </summary>
        private void scale_Click(object sender, RoutedEventArgs e)
        {
            Grid currGrid = windowManager.SelectedContent as Grid;

            SlopeCanvas currCanvas = null;
            if (currGrid != null)
            {
                currCanvas = currGrid.Children[2] as SlopeCanvas;
            }
            if (currCanvas == null) return;

            // Obtain currently selected scale MenuItem (if there is one)
            // and uncheck all scale MenuItems
            MenuItem oldScale = null;
            foreach (MenuItem scale in scaleList.Items)
            {
                if (scale.IsChecked)
                {
                    scale.IsChecked = false;
                    oldScale = scale;
                }
            }

            // Cast sending scale MenuItem and set it to checked
            MenuItem newScale = sender as MenuItem;
            if (newScale != null)
            {
                newScale.IsChecked = true;
            }

            // If the new value is different from the previous,
            // or the user would like to specify a custom scale
            if (newScale != oldScale || newScale == scCustom)
            {
                // Select from list of default scales, or set Custom
                double desiredScale;
                switch (newScale.Name)
                {
                    // 1000:1
                    case "sc1000":
                        currCanvas.ScaleType = Scales.sc1000;
                        desiredScale = 1000;
                        break;
                    // 800:1
                    case "sc800":
                        currCanvas.ScaleType = Scales.sc800;
                        desiredScale = 800;
                        break;

                    // 600:1
                    case "sc600":
                        currCanvas.ScaleType = Scales.sc600;
                        desiredScale = 600;
                        break;

                    // 500:1
                    case "sc500":
                        currCanvas.ScaleType = Scales.sc500;
                        desiredScale = 500;
                        break;

                    // 400:1
                    case "sc400":
                        currCanvas.ScaleType = Scales.sc400;
                        desiredScale = 400;
                        break;

                    // 300:1
                    case "sc300":
                        currCanvas.ScaleType = Scales.sc300;
                        desiredScale = 300;
                        break;

                    // 200:1
                    case "sc200":
                        currCanvas.ScaleType = Scales.sc200;
                        desiredScale = 200;
                        break;

                    // 150:1
                    case "sc150":
                        currCanvas.ScaleType = Scales.sc150;
                        desiredScale = 150;
                        break;

                    // 100:1
                    case "sc100":
                        currCanvas.ScaleType = Scales.sc100;
                        desiredScale = 100;
                        break;

                    // 50:1
                    case "sc50":
                        currCanvas.ScaleType = Scales.sc50;
                        desiredScale = 50;
                        break;

                    // 25:1
                    case "sc25":
                        currCanvas.ScaleType = Scales.sc25;
                        desiredScale = 25;
                        break;

                    // 10:1
                    case "sc10":
                        currCanvas.ScaleType = Scales.sc10;
                        desiredScale = 10;
                        break;

                    // 5:1
                    case "sc5":
                        currCanvas.ScaleType = Scales.sc5;
                        desiredScale = 5;
                        break;

                    // 2:1
                    case "sc2":
                        currCanvas.ScaleType = Scales.sc2;
                        desiredScale = 2;
                        break;

                    // 1:1
                    case "sc1":
                        currCanvas.ScaleType = Scales.sc1;
                        desiredScale = 1;
                        break;

                    // Custom
                    default:
                        // Create modal dialog box to prompt user for new scale
                        CustomScaleDialog dlg = new CustomScaleDialog(this);
                        dlg.ShowDialog();

                        // If the dialog returned OK, set new scale
                        if (dlg.DialogResult == true)
                        {
                            desiredScale = Double.Parse(dlg.scale.Text);
                            currCanvas.ScaleType = Scales.Custom;
                        }
                        // If the dialog returned Cancel, reset to previous scale
                        else
                        {
                            desiredScale = currCanvas.Scale;
                            newScale.IsChecked = false;
                            oldScale.IsChecked = true;
                        }

                        break;
                }
                // Zoom to desired scale and centre content WITHOUT zoom
                currCanvas.Zoom(currCanvas.Scale / desiredScale, new Point(0.5 * currCanvas.ActualHeight, 0.5 * currCanvas.ActualWidth));
                currCanvas.CentreAndFitExtents(false);
            }
        }

        /// <summary>
        /// Responds to units related MenuItem clicks
        /// </summary>
        private void units_Click(object sender, RoutedEventArgs e)
        {
            Grid currGrid = windowManager.SelectedContent as Grid;

            SlopeCanvas currCanvas = null;
            if (currGrid != null)
            {
                currCanvas = currGrid.Children[2] as SlopeCanvas;
            }
            if (currCanvas == null) return;

            // Get currently selected units (if exists)
            // and uncheck all units MenuItems
            MenuItem oldUnits = null;
            foreach (MenuItem units in unitsList.Items)
            {
                if (units.IsChecked)
                {
                    units.IsChecked = false;
                    oldUnits = units;
                }
            }

            // Cast sending object to MenuItem and set it to checked
            MenuItem newUnits = sender as MenuItem;
            if (newUnits != null)
            {
                newUnits.IsChecked = true;
            }

            // Respond only if new units are different from previous
            if (newUnits != oldUnits)
            {
                // Check if the user wants to convert values, or just change unit label
                MessageBoxResult convert =
                    MessageBox.Show("Do you wish to convert existing values to new units?\n\n" +
                                    "Yes: Converts numerical values to their equivalent in new units.\n" +
                                    "No: Changes unit label and modifies scale appropriately.\n" +
                                    "Cancel: Maintains existing units and scale.", "Unit Conversion", MessageBoxButton.YesNoCancel);

                // If the user changed their mind, reset menu state and do nothing
                if (convert == MessageBoxResult.Cancel)
                {
                    newUnits.IsChecked = false;
                    oldUnits.IsChecked = true;
                }
                // Otherwise pass processing on to SlopeCanvas object
                else
                {
                    currCanvas.UpdateUnits(newUnits, convert);
                }

            }
        }

        /// <summary>
        /// Responds to grid display related MenuItem clicks
        /// </summary>
        private void grid_Click(object sender, RoutedEventArgs e)
        {
            // Get currently shown grid (if exists)
            MenuItem oldGrid = null;
            foreach (MenuItem grid in gridList.Items)
            {
                if (grid.IsChecked)
                {
                    grid.IsChecked = false;
                    oldGrid = grid;
                }
            }

            // Get desired grid toggle
            MenuItem newGrid = sender as MenuItem;
            if (newGrid != null)
            {
                Grid currGrid = windowManager.SelectedContent as Grid;

                SlopeCanvas currCanvas = null;
                if (currGrid != null)
                {
                    currCanvas = currGrid.Children[2] as SlopeCanvas;
                }
                if (currCanvas == null) return;

                // If sender is same as previous and grid was on,
                // turn off the grid
                if (newGrid == oldGrid)
                {
                    switch (newGrid.Name)
                    {
                        case "gridMajor":
                            currCanvas.ShowMajorGrid = false;
                            break;

                        default:
                            currCanvas.ShowMinorGrid = false;
                            break;
                    }
                }
                // Otherwise, turn on the selected grid and turn off the other
                else
                {
                    newGrid.IsChecked = true;
                    switch (newGrid.Name)
                    {
                        case "gridMajor":
                            currCanvas.ShowMajorGrid = true;
                            currCanvas.ShowMinorGrid = false;
                            break;

                        default:
                            currCanvas.ShowMajorGrid = false;
                            currCanvas.ShowMinorGrid = true;
                            break;
                    }
                }
                
                // Call display update from canvas
                currCanvas.UpdateGridDisplay();
            }
        }

        /// <summary>
        /// Allows the user to toggle display of analysis mesh lines.
        /// </summary>
        private void showMesh_Click(object sender, RoutedEventArgs e)
        {
            MenuItem mesh = sender as MenuItem;

            Grid currGrid = windowManager.SelectedContent as Grid;

            SlopeCanvas currCanvas = null;
            if (currGrid != null)
            {
                currCanvas = currGrid.Children[2] as SlopeCanvas;
            }

            if (currCanvas != null)
            {
                currCanvas.ShowMesh = mesh.IsChecked;
            }
        }

        private void defineMaterials_Click(object sender, RoutedEventArgs e)
        {
            Grid currGrid = windowManager.SelectedContent as Grid;

            SlopeCanvas currCanvas = null;
            if (currGrid != null)
            {
                currCanvas = currGrid.Children[2] as SlopeCanvas;
            }

            if (currCanvas != null)
            {
                AddMaterialDialog dlg = new AddMaterialDialog(this);
                dlg.ShowDialog();
            }
        }

        private void run_Click(object sender, RoutedEventArgs e)
        {
            Grid currGrid = windowManager.SelectedContent as Grid;

            SlopeCanvas currCanvas = null;
            if (currGrid != null)
            {
                currCanvas = currGrid.Children[2] as SlopeCanvas;
            }

            if (currCanvas != null)
            {
                if (currCanvas.IsVerified && currCanvas.IsMeshed && !currCanvas.IsAnalyzing)
                {
                    currCanvas.RunAnalysis();
                }
            }
        }
        private void run_Executed(object sender, ExecutedRoutedEventArgs e)
        {
            run_Click(sender, (RoutedEventArgs)e);
        }

        private void generateMesh_Click(object sender, RoutedEventArgs e)
        {
            Grid currGrid = windowManager.SelectedContent as Grid;

            SlopeCanvas currCanvas = null;
            if (currGrid != null)
            {
                currCanvas = currGrid.Children[2] as SlopeCanvas;
            }

            if (currCanvas != null)
            {
                if (currCanvas.IsVerified)
                {
                    if (currCanvas.IsMeshed) currCanvas.IsMeshed = false;

                    switch (currCanvas.AnalysisType)
                    {
                        case AnalysisType.Bishop:
                            currCanvas.Boundary.GenerateMesh(currCanvas.Boundary.XMin, currCanvas.Boundary.XMax);
                            break;

                        case AnalysisType.RFEM:
                            currCanvas.Boundary.GenerateMesh(currCanvas.Boundary.XMin, currCanvas.Boundary.XMax);
                            break;

                        case AnalysisType.FEA4NodedQuad:
                            currCanvas.FEAQuadElements =
                                SlopeCanvas.MeshGenStructured4NodedQuad(currCanvas,
                                    currCanvas.FEAParameters.ColWidth, currCanvas.FEAParameters.RowHeight, true);

                            currCanvas.IsMeshed = true;
                            currCanvas.ShowMesh = true;
                            currCanvas.IsSaved = false;

                            break;

                        default:
                            currCanvas.FEATriElements =
                                SlopeCanvas.MeshGenStructured3NodedTri(currCanvas,
                                    currCanvas.FEAParameters.ColWidth, currCanvas.FEAParameters.RowHeight);

                            currCanvas.IsMeshed = true;
                            currCanvas.ShowMesh = true;
                            currCanvas.IsSaved = false;

                            break;
                    }

                    currCanvas.IsSaved = false;

                    ClosableCanvasTabItem currTab = windowManager.SelectedItem as ClosableCanvasTabItem;
                    currCanvas.SaveInputFile((string)currTab.Tag);

                    currCanvas.BuildAxes();
                }
            }
        }
        private void generateMesh_Executed(object sender, ExecutedRoutedEventArgs e)
        {
            generateMesh_Click(sender, (RoutedEventArgs)e);
        }

        private void analysisParameters_Click(object sender, RoutedEventArgs e)
        {
            Grid currGrid = windowManager.SelectedContent as Grid;

            SlopeCanvas currCanvas = null;
            if (currGrid != null)
            {
                currCanvas = currGrid.Children[2] as SlopeCanvas;
            }

            if (currCanvas != null)
            {
                switch(currCanvas.AnalysisType)
                {
                    case AnalysisType.Bishop: (new GenAlgParamsDialog(this)).ShowDialog(); break;
                    case AnalysisType.RFEM: (new GenAlgParamsDialog(this)).ShowDialog(); break;
                    default: (new FEAParamsDialog(this)).ShowDialog(); break;
                }
            }
        }

        private void addMaterials_Click(object sender, RoutedEventArgs e)
        {
            Grid currGrid = windowManager.SelectedContent as Grid;

            SlopeCanvas currCanvas = null;
            if (currGrid != null)
            {
                currCanvas = currGrid.Children[2] as SlopeCanvas;
            }

            if (currCanvas != null)
            {
                AssignMaterialDialog dlg = new AssignMaterialDialog(this);
                dlg.ShowDialog();

                if (dlg.DialogResult == true)
                {
                    if (dlg.SelectedMaterial != null)
                    {
                        for (int i = 0; i < currCanvas.MaterialBlocks.Count; i++)
                        {
                            if (currCanvas.MaterialBlocks[i].IsSelected)
                            {
                                currCanvas.MaterialBlocks[i].Material = dlg.SelectedMaterial;
                                currCanvas.IsSaved = false;
                            }
                        }
                    }
                }
            }
        }

        private void deleteBlock_Click(object sender, RoutedEventArgs e)
        {
            Grid currGrid = windowManager.SelectedContent as Grid;

            SlopeCanvas currCanvas = null;
            if (currGrid != null)
            {
                currCanvas = currGrid.Children[2] as SlopeCanvas;
            }

            if (currCanvas != null)
            {
                currCanvas.DeleteSelectedItems();
            }
        }
        private void deleteBlock_Executed(object sender, ExecutedRoutedEventArgs e)
        {
            deleteBlock_Click(sender, (RoutedEventArgs)e);
        }

        private void addPoint_Click(object sender, RoutedEventArgs e)
        {
            Grid currGrid = windowManager.SelectedContent as Grid;

            SlopeCanvas currCanvas = null;
            if (currGrid != null)
            {
                currCanvas = currGrid.Children[2] as SlopeCanvas;
            }

            if (currCanvas != null)
            {
                currCanvas.ClearSelections();
                currCanvas.CancelDrawing();
                currCanvas.DrawMode = DrawModes.AddPoints;
                currCanvas.Cursor = ((TextBlock)this.Resources["addPointsCursor"]).Cursor;
            }
        }

        private void movePoint_Click(object sender, RoutedEventArgs e)
        {
            Grid currGrid = windowManager.SelectedContent as Grid;

            SlopeCanvas currCanvas = null;
            if (currGrid != null)
            {
                currCanvas = currGrid.Children[2] as SlopeCanvas;
            }

            if (currCanvas != null)
            {
                currCanvas.ClearSelections();
                currCanvas.CancelDrawing();
                currCanvas.DrawMode = DrawModes.MovePoints;
                currCanvas.Cursor = ((TextBlock)this.Resources["movePointsCursor"]).Cursor;
            }
        }

        private void verify_Click(object sender, RoutedEventArgs e)
        {
            Grid currGrid = windowManager.SelectedContent as Grid;

            SlopeCanvas currCanvas = null;
            if (currGrid != null)
            {
                currCanvas = currGrid.Children[2] as SlopeCanvas;
            }
            if (currCanvas == null) return;

            VerifyDialog dlg = new VerifyDialog(this);
            dlg.ShowDialog();

            if (dlg.ErrorCount == 0)
            {
                currCanvas.IsVerified = true;
                currCanvas.IsSaved = false;

                ClosableCanvasTabItem currTab = windowManager.SelectedItem as ClosableCanvasTabItem;
                currCanvas.SaveInputFile((string)currTab.Tag);
            }
        }

        private void clearMesh_Click(object sender, RoutedEventArgs e)
        {
            Grid currGrid = windowManager.SelectedContent as Grid;

            SlopeCanvas currCanvas = null;
            if (currGrid != null)
            {
                currCanvas = currGrid.Children[2] as SlopeCanvas;
            }

            if (currCanvas != null)
            {
                currCanvas.IsMeshed = false;
                currCanvas.IsSaved = false;
            }
        }

        private void stop_Click(object sender, RoutedEventArgs e)
        {
            Grid currGrid = windowManager.SelectedContent as Grid;

            SlopeCanvas currCanvas = null;
            if (currGrid != null)
            {
                currCanvas = currGrid.Children[2] as SlopeCanvas;
            }

            if (currCanvas != null)
            {
                if (currCanvas.IsVerified && currCanvas.IsMeshed && currCanvas.IsAnalyzing)
                {
                    currCanvas.StopAnalysis();
                }
            }
        }

        private void showCritical_Click(object sender, RoutedEventArgs e)
        {
            MenuItem showCritical = sender as MenuItem;

            Grid currGrid = windowManager.SelectedContent as Grid;

            SlopeCanvas currCanvas = null;
            if (currGrid != null)
            {
                currCanvas = currGrid.Children[2] as SlopeCanvas;
            }

            if (currCanvas != null)
            {
                currCanvas.ShowCritical = showCritical.IsChecked;
            }
        }

        private void viewResults_Click(object sender, RoutedEventArgs e)
        {
            //MessageBox.Show("Select a run to show the top 10 surfaces", "View Run Results");
            Grid currGrid = windowManager.SelectedContent as Grid;

            SlopeCanvas currCanvas = null;
            if (currGrid != null)
            {
                currCanvas = currGrid.Children[2] as SlopeCanvas;
            }
            if (currCanvas == null) return;

            if (currCanvas.IsAnalyzed)
            {
                ShowResultsDialog dlg = new ShowResultsDialog(this);
                dlg.ShowDialog();
            }
        }

        private void outputFile_Click(object sender, RoutedEventArgs e)
        {
            Grid currGrid = windowManager.SelectedContent as Grid;

            SlopeCanvas currCanvas = null;
            if (currGrid != null)
            {
                currCanvas = currGrid.Children[2] as SlopeCanvas;
            }

            if (currCanvas != null)
            {
                if (currCanvas.IsAnalyzed)
                {
                    string[] pathsplit = currCanvas.FilePath.Split('.');

                    if (pathsplit.Length > 1 && pathsplit[pathsplit.Length - 1] == "slp")
                    {
                        switch (currCanvas.AnalysisType)
                        {
                            case AnalysisType.Bishop: pathsplit[pathsplit.Length - 1] = "bish"; break;
                            default: pathsplit[pathsplit.Length - 1] = "rfem"; break;
                        }
                        
                        string path = string.Join(".", pathsplit);

                        if (File.Exists(path)) System.Diagnostics.Process.Start(@path);
                    }
                }
            }
        }

        private void analysisMethod_Click(object sender, RoutedEventArgs e)
        {
            Grid currGrid = windowManager.SelectedContent as Grid;

            SlopeCanvas currCanvas = null;
            if (currGrid != null)
            {
                currCanvas = currGrid.Children[2] as SlopeCanvas;
            }
            if (currCanvas == null) return;

            // Obtain currently selected analysis MenuItem (if there is one)
            // and uncheck all analysis MenuItems
            MenuItem oldAnalysis = null;
            for (int i = 0; i < analysisList.Items.Count; i++)
            {
                if (((MenuItem)analysisList.Items[i]).IsChecked)
                {
                    ((MenuItem)analysisList.Items[i]).IsChecked = false;
                }
            }
            for (int i = 0; i < stabilityList.Items.Count; i++)
            {
                if (((MenuItem)stabilityList.Items[i]).IsChecked)
                {
                    ((MenuItem)stabilityList.Items[i]).IsChecked = false;
                    oldAnalysis = stabilityList.Items[i] as MenuItem;
                }
            }
            for (int i = 0; i < feaList.Items.Count; i++)
            {
                if (((MenuItem)feaList.Items[i]).IsChecked)
                {
                    ((MenuItem)feaList.Items[i]).IsChecked = false;
                    oldAnalysis = feaList.Items[i] as MenuItem;
                }
            }


            // Cast sending analysis MenuItem and set it to checked
            MenuItem newAnalysis = sender as MenuItem;
            if (newAnalysis != null)
            {
                newAnalysis.IsChecked = true;

                if (newAnalysis == amBishop || newAnalysis == amRFEM)
                {
                    stabilityList.IsChecked = true;
                }
                else
                {
                    feaList.IsChecked = true;
                }
            }

            // If the new value is different from the previous
            if (newAnalysis != oldAnalysis)
            {
                // Select from list of analysis types
                switch (newAnalysis.Name)
                {
                    case "amBishop": currCanvas.AnalysisType = AnalysisType.Bishop; break;
                    case "amRFEM": currCanvas.AnalysisType = AnalysisType.RFEM; break;
                    case "amFEA3NodedTri": currCanvas.AnalysisType = AnalysisType.FEA3NodedTri; break;
                    default: currCanvas.AnalysisType = AnalysisType.FEA4NodedQuad; break;
                }

                currCanvas.IsSaved = false;
                currCanvas.IsVerified = false;
            }
        }

        private void fixX_Click (object sender, RoutedEventArgs e)
        {
            Grid currGrid = windowManager.SelectedContent as Grid;

            SlopeCanvas currCanvas = null;
            if (currGrid != null)
            {
                currCanvas = currGrid.Children[2] as SlopeCanvas;
            }

            if (currCanvas != null)
            {
                currCanvas.ClearSelections();
                currCanvas.CancelDrawing();
                currCanvas.DrawMode = DrawModes.FixX;
                currCanvas.Cursor = ((TextBlock)this.Resources["rollerYCursor"]).Cursor;
            }
        }

        private void fixY_Click (object sender, RoutedEventArgs e)
        {
            Grid currGrid = windowManager.SelectedContent as Grid;

            SlopeCanvas currCanvas = null;
            if (currGrid != null)
            {
                currCanvas = currGrid.Children[2] as SlopeCanvas;
            }

            if (currCanvas != null)
            {
                currCanvas.ClearSelections();
                currCanvas.CancelDrawing();
                currCanvas.DrawMode = DrawModes.FixY;
                currCanvas.Cursor = ((TextBlock)this.Resources["rollerXCursor"]).Cursor;
            }
        }

        private void pointLoad_Click (object sender, RoutedEventArgs e)
        {
            Grid currGrid = windowManager.SelectedContent as Grid;

            SlopeCanvas currCanvas = null;
            if (currGrid != null)
            {
                currCanvas = currGrid.Children[2] as SlopeCanvas;
            }

            if (currCanvas != null)
            {
                currCanvas.ClearSelections();
                currCanvas.CancelDrawing();
                currCanvas.DrawMode = DrawModes.PointLoad;
                currCanvas.Cursor = ((TextBlock)this.Resources["pointLoadCursor"]).Cursor;
            }
        }

        private void lineLoad_Click (object sender, RoutedEventArgs e)
        {
            Grid currGrid = windowManager.SelectedContent as Grid;

            SlopeCanvas currCanvas = null;
            if (currGrid != null)
            {
                currCanvas = currGrid.Children[2] as SlopeCanvas;
            }

            if (currCanvas != null)
            {
                currCanvas.ClearSelections();
                currCanvas.CancelDrawing();
                currCanvas.DrawMode = DrawModes.LineLoad;
                currCanvas.Cursor = ((TextBlock)this.Resources["lineLoadCursor"]).Cursor;
            }
        }
    }
}
