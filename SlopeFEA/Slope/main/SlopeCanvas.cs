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
using System.Collections.Generic;
using System.ComponentModel;
using System.IO;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Controls.Primitives;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Shapes;

namespace SlopeFEA
{
    public partial class SlopeCanvas : Canvas
    {
        private bool drawing , panning , zooming , moving , caughtCtrl;
        private bool isSaved = false , isVerified = false , isMeshed = false ,
            showMesh = false , showCritical = false , isAnalyzing = false , isAnalyzed = false;
        private AnalysisType analysisType;
        private double dpiX , dpiY;
        private int mouseDelta = 0;
        private SlopeBoundary boundary;
        private DrawingPoint movingBoundPoint;
        private List<MaterialBlock> materialBlocks;
        private List<DrawingPoint> addPoints , fixPoints , loadPoints;
        private Polyline drawLine;
        private Point transPoint , movePoint;
        private ZoomRect zoomRect;
        private Grid xAxis , yAxis;
        private ProgressBar analysisProgress;
        private Rectangle hideCanvasRect;
        private List<GridPoint> gridPoints;
        private List<MaterialType> materialTypes;
        private GAParams genAlgParams;
        private FEAParams feaParams;
        private BackgroundWorker analysisWorker;
        private DisplayCircularSurface criticalSurface;
        private List<DisplayCircularSurface> runSurfaces;
        private List<AnalysisPhase> feaPhases;

        /// <summary>
        /// (Constructor) Adds various drawing Polygons.
        /// </summary>
        public SlopeCanvas ()
        {
            analysisWorker = new BackgroundWorker();
            InitializeBackgroundWorker();

            this.SizeChanged += new SizeChangedEventHandler( SlopeCanvas_SizeChanged );

            // For drawing boundaries and material blocks
            drawLine = new Polyline();
            drawLine.Stroke = Brushes.Black;
            drawLine.Opacity = 0.9;

            // For displaying analysis boundaries
            boundary = new SlopeBoundary( this );

            // Initialize material block list
            materialBlocks = new List<MaterialBlock>();

            // For zooming to a particular area
            zoomRect = new ZoomRect();

            // Initialize grid point list
            gridPoints = new List<GridPoint>();

            // Initialize material type list
            MaterialType nullMaterial = new MaterialType();
            nullMaterial.Fill = Brushes.GhostWhite;
            nullMaterial.Name = "NULL";
            materialTypes = new List<MaterialType>() { nullMaterial };

            // Initialize list for adding a drawing point
            addPoints = new List<DrawingPoint>();

            // Initialize list for fixing points
            fixPoints = new List<DrawingPoint>();

            // Initialize list for load points
            loadPoints = new List<DrawingPoint>();

            // Initialize list of GA parameters
            genAlgParams = new GAParams();

            // Initialize list of FEA parameters
            feaParams = new FEAParams();

            // Initialize list of FEA elements
            FEATriElements = new List<fe3NodedTriElement>();
            FEAQuadElements = new List<fe4NodedQuadElement>();
            FEASubstructs = new List<feSubstruct>();

            // Initialize list of surfaces
            runSurfaces = new List<DisplayCircularSurface>();

            // Initialize list of analysis phases
            feaPhases = new List<AnalysisPhase>() { new AnalysisPhase( 0 , "NULL" , null , true , 0 , 0 , 0 , 0 ) };
        }

        private void InitializeBackgroundWorker ()
        {
            analysisWorker.WorkerSupportsCancellation = true;
            analysisWorker.WorkerReportsProgress = true;

            analysisWorker.DoWork += new DoWorkEventHandler( analysisWorker_DoWork );
            analysisWorker.RunWorkerCompleted += new RunWorkerCompletedEventHandler( analysisWorker_RunWorkerCompleted );
            analysisWorker.ProgressChanged += new ProgressChangedEventHandler( analysisWorker_ProgressChanged );
        }



        // ----------------------------------
        // PROPERTIES
        // ----------------------------------

        public string FilePath { get; set; }

        public DrawModes DrawMode { get; set; }
        public Units Units { get; set; }                        // See enum types above
        public Scales ScaleType { get; set; }

        public double OriginOffsetX { get; set; }               // Plotting origin location (in pixels)
        public double OriginOffsetY { get; set; }               // (0,0) -> BL corner

        public double Scale { get; set; }                       // Plotting scale (actual units / screen unit)

        public double XAxisMax { get; set; }
        public double XAxisMin { get; set; }                    // Plotting axis extents (in actual units)
        public double YAxisMax { get; set; }
        public double YAxisMin { get; set; }

        public double XMajorDivision { get; set; }              // Major grid spacing (in actual units)
        public double YMajorDivision { get; set; }

        public int XMinorDivisions { get; set; }                // # minor grid divisions / major grid spacer
        public int YMinorDivisions { get; set; }

        public bool ShowMajorGrid { get; set; }
        public bool ShowMinorGrid { get; set; }                 // Toggles for various plotting objects

        public List<fe3NodedTriElement> FEATriElements { get; set; }
        public List<fe4NodedQuadElement> FEAQuadElements { get; set; }
        public List<feSubstruct> FEASubstructs { get; set; }

        public bool ShowMesh
        {
            get
            {
                return this.showMesh;
            }
            set
            {
                if ( value != this.showMesh )
                {
                    this.showMesh = value;

                    if ( this.IsMeshed )
                    {
                        boundary.ShowMesh = value;

                        FEATriElements.ForEach( delegate( fe3NodedTriElement element ) { element.Boundary.Visibility = value ? Visibility.Visible : Visibility.Hidden; } );

                        FEAQuadElements.ForEach( delegate( fe4NodedQuadElement element ) { element.Boundary.Visibility = value ? Visibility.Visible : Visibility.Hidden; } );
                    }
                    else
                    {
                        boundary.ShowMesh = false;
                    }
                }
            }
        }

        public bool ShowCritical
        {
            get
            {
                return this.showCritical;
            }
            set
            {
                if ( value != this.showCritical )
                {
                    this.showCritical = value;

                    if ( this.IsAnalyzed && criticalSurface != null )
                    {
                        criticalSurface.IsVisible = value;
                    }
                }
            }
        }

        public bool IsScaled { get; set; }                      // Toggle for initial display setup

        public bool IsVerified                                  // Toggle for geometry verification
        {
            get
            {
                return this.isVerified;
            }
            set
            {
                if ( this.isVerified != value )
                {
                    this.isVerified = value;

                    ClosableCanvasTabItem parentTab = (ClosableCanvasTabItem) ((Grid) this.Parent).Parent;
                    Menu mainMenu = (Menu) ((DockPanel) ((Grid) ((TabControl) parentTab.Parent).Parent).Children[0]).Children[0];
                    MenuItem analyzeMenu = (MenuItem) mainMenu.Items[3];
                    MenuItem generateMesh = (MenuItem) analyzeMenu.Items[3];

                    generateMesh.IsEnabled = value;

                    if ( !value )
                    {
                        this.IsMeshed = value;
                        this.IsAnalyzed = value;

                        // Check if mesh or analysis output files exist and move them
                        // to a new file name (so they are not overwritten or confused
                        // with new analysis on the updated input)
                        if ( FilePath != null )
                        {
                            string[] split = FilePath.Split( '.' );
                            if ( split.Length > 1 && split[split.Length - 1] == "slp" )
                            {
                                string[] meshsplit = new string[split.Length];
                                split.CopyTo( meshsplit , 0 );
                                meshsplit[meshsplit.Length - 1] = "msh";
                                string oldmeshpath = string.Join( "." , meshsplit );

                                string[] bishsplit = new string[split.Length];
                                split.CopyTo( bishsplit , 0 );
                                bishsplit[bishsplit.Length - 1] = "bish";
                                string oldbishpath = string.Join( "." , bishsplit );

                                string[] rfemsplit = new string[split.Length];
                                split.CopyTo( rfemsplit , 0 );
                                rfemsplit[rfemsplit.Length - 1] = "rfem";
                                string oldrfempath = string.Join( "." , rfemsplit );

                                int oldcount = 0;

                                // If a mesh or analysis file exists...
                                if ( File.Exists( oldmeshpath ) || File.Exists( oldbishpath ) || File.Exists( oldrfempath ) )
                                {
                                    // ...affix a counter to the filenames...
                                    meshsplit[0] = String.Format( "{0}_old{1}" , meshsplit[0] , oldcount );
                                    string newmeshpath = string.Join( "." , meshsplit );

                                    bishsplit[0] = String.Format( "{0}_old{1}" , bishsplit[0] , oldcount );
                                    string newbishpath = string.Join( "." , bishsplit );

                                    rfemsplit[0] = String.Format( "{0}_old{1}" , rfemsplit[0] , oldcount );
                                    string newrfempath = string.Join( "." , rfemsplit );

                                    // ...and check if other previous analysis files exist...
                                    while ( File.Exists( newmeshpath ) || File.Exists( newbishpath ) || File.Exists( newrfempath ) )
                                    {
                                        oldcount++;

                                        meshsplit[0] = String.Format( "{0}{1}" , meshsplit[0].Substring( 0 , meshsplit[0].Length - 1 ) , oldcount );
                                        newmeshpath = string.Join( "." , meshsplit );

                                        bishsplit[0] = String.Format( "{0}{1}" , bishsplit[0].Substring( 0 , bishsplit[0].Length - 1 ) , oldcount );
                                        newbishpath = string.Join( "." , bishsplit );

                                        rfemsplit[0] = String.Format( "{0}{1}" , rfemsplit[0].Substring( 0 , rfemsplit.Length - 1 ) , oldcount );
                                        newrfempath = string.Join( "." , rfemsplit );
                                    }

                                    // ...finally move previous analysis file to new filename
                                    if ( File.Exists( oldmeshpath ) ) File.Move( oldmeshpath , newmeshpath );
                                    if ( File.Exists( oldbishpath ) ) File.Move( oldbishpath , newbishpath );
                                    if ( File.Exists( oldrfempath ) ) File.Move( oldrfempath , newrfempath );
                                }
                            }
                        }
                    }
                }
            }
        }

        public bool IsMeshed                                  // Toggle for mesh generation
        {
            get
            {
                return this.isMeshed;
            }
            set
            {
                if ( this.isMeshed != value )
                {
                    this.isMeshed = value;

                    ClosableCanvasTabItem parentTab = (ClosableCanvasTabItem) ((Grid) this.Parent).Parent;
                    Menu mainMenu = (Menu) ((DockPanel) ((Grid) ((TabControl) parentTab.Parent).Parent).Children[0]).Children[0];
                    MenuItem analyzeMenu = (MenuItem) mainMenu.Items[3];
                    MenuItem runAnalysis = (MenuItem) analyzeMenu.Items[0];

                    runAnalysis.IsEnabled = value;

                    MenuItem clearMesh = (MenuItem) analyzeMenu.Items[4];

                    clearMesh.IsEnabled = value;

                    MenuItem viewMenu = (MenuItem) mainMenu.Items[1];
                    MenuItem showMesh = (MenuItem) viewMenu.Items[11];

                    showMesh.IsChecked = value;
                    showMesh.IsEnabled = value;

                    if ( !value )
                    {
                        this.Boundary.ClearMesh();

                        for ( int i = 0 ; i < this.FEAQuadElements.Count ; i++ )
                        {
                            this.Children.Remove( this.FEAQuadElements[i].Boundary );
                        }
                        this.FEAQuadElements.Clear();

                        for ( int i = 0 ; i < this.FEATriElements.Count ; i++ )
                        {
                            this.Children.Remove( this.FEATriElements[i].Boundary );
                        }
                        this.FEATriElements.Clear();

                        string[] path = FilePath.Split( '.' );
                        path[path.Length - 1] = "nod";
                        File.Delete( string.Join( "." , path ) );
                        path[path.Length - 1] = "ele";
                        File.Delete( string.Join( "." , path ) );
                        path[path.Length - 1] = "mtl";
                        File.Delete( string.Join( "." , path ) );
                        path[path.Length - 1] = "bel";
                        File.Delete( string.Join( "." , path ) );

                        this.IsAnalyzed = false;

                        if ( criticalSurface != null ) criticalSurface.Delete();

                        while ( runSurfaces.Count > 0 )
                        {
                            runSurfaces[0].Delete();
                            runSurfaces.RemoveAt( 0 );
                        }
                    }
                }
            }
        }

        public bool IsAnalyzing
        {
            get
            {
                return this.isAnalyzing;
            }
            set
            {
                if ( this.isAnalyzing != value )
                {
                    this.isAnalyzing = value;

                    ClosableCanvasTabItem parentTab = (ClosableCanvasTabItem) ((Grid) this.Parent).Parent;
                    Menu mainMenu = (Menu) ((DockPanel) ((Grid) ((TabControl) parentTab.Parent).Parent).Children[0]).Children[0];
                    MenuItem analyzeMenu = (MenuItem) mainMenu.Items[3];
                    MenuItem runAnalysis = (MenuItem) analyzeMenu.Items[0];
                    MenuItem stopAnalysis = (MenuItem) analyzeMenu.Items[1];

                    runAnalysis.IsEnabled = !value;
                    stopAnalysis.IsEnabled = value;

                    MenuItem generateMesh = (MenuItem) analyzeMenu.Items[3];
                    MenuItem clearMesh = (MenuItem) analyzeMenu.Items[4];

                    generateMesh.IsEnabled = !value;
                    clearMesh.IsEnabled = !value;

                    if ( value ) CentreAndFitExtents( true );

                    analysisProgress.Visibility = value ? Visibility.Visible : Visibility.Hidden;
                    hideCanvasRect.Visibility = value ? Visibility.Visible : Visibility.Hidden;
                }
            }
        }

        public bool IsAnalyzed
        {
            get
            {
                return this.isAnalyzed;
            }
            set
            {
                if ( this.isAnalyzed != value )
                {
                    this.isAnalyzed = value;

                    ClosableCanvasTabItem parentTab = (ClosableCanvasTabItem) ((Grid) this.Parent).Parent;
                    Menu mainMenu = (Menu) ((DockPanel) ((Grid) ((TabControl) parentTab.Parent).Parent).Children[0]).Children[0];
                    MenuItem outputMenu = (MenuItem) mainMenu.Items[4];

                    outputMenu.IsEnabled = value;
                }
            }
        }

        public AnalysisType AnalysisType
        {
            get
            {
                return this.analysisType;
            }
            set
            {
                if ( AnalysisType != value )
                {
                    this.analysisType = value;
                    this.IsSaved = false;
                }
            }
        }

        public List<DisplayCircularSurface> RunSurfaces { get { return this.runSurfaces; } }

        public double DpiX { get { return this.dpiX; } }
        public double DpiY { get { return this.dpiY; } }

        public bool HasBoundary
        {
            get
            {
                return boundary != null ? boundary.Boundary.Points.Count > 0 : false;
            }
        }

        public bool IsSaved
        {
            get
            {
                return this.isSaved;
            }
            set
            {
                if ( this.isSaved != value )
                {
                    this.isSaved = value;

                    ClosableCanvasTabItem parentTab = (ClosableCanvasTabItem) ((Grid) this.Parent).Parent;

                    string path = parentTab.Tag as string;
                    if ( path != null )
                    {
                        string[] pathSplit = path.Split( '\\' );
                        string header = value ? pathSplit[pathSplit.Length - 1] : pathSplit[pathSplit.Length - 1] + "*";
                        parentTab.Title = header;
                    }
                }
            }
        }

        public List<MaterialBlock> MaterialBlocks { get { return this.materialBlocks; } }
        public List<MaterialType> MaterialTypes { get { return this.materialTypes; } }

        public SlopeBoundary Boundary { get { return this.boundary; } }

        public GAParams GeneticAlgorithmParameters { get { return this.genAlgParams; } }
        public FEAParams FEAParameters { get { return this.feaParams; } }

        public List<AnalysisPhase> FEAPhases { get { return this.feaPhases; } }


        // ----------------------------------
        // UTILITY FUNCTIONS
        // ----------------------------------

        /// <summary>
        /// Set initial (non-painting) properties
        /// </summary>
        public void InitializeCanvas ()
        {
            this.ClipToBounds = true;

            drawing = false;
            panning = false;
            zooming = false;
            moving = false;
            caughtCtrl = false;

            DrawMode = DrawModes.Select;
            Units = Units.Metres;
            ScaleType = Scales.Custom;

            OriginOffsetX = 50;
            OriginOffsetY = 50;

            XAxisMax = 100;
            XAxisMin = 0;
            YAxisMax = 50;
            YAxisMin = 0;

            XMajorDivision = 10;
            YMajorDivision = 10;

            XMinorDivisions = 5;
            YMinorDivisions = 5;

            ShowMajorGrid = false;
            ShowMinorGrid = false;
            ShowMesh = false;

            IsScaled = false;
            IsVerified = false;
            IsMeshed = false;

            genAlgParams.Population = 15;
            genAlgParams.Generations = 100;
            genAlgParams.FittestProportion = 0.1;
            genAlgParams.MatingPoolProportion = 0.5;
            genAlgParams.CrossoverProbability = 0.7;
            genAlgParams.MutationProbability = 0.25;
            genAlgParams.SliceWidth = 10;

            feaParams.ColWidth = 1.0;
            feaParams.RowHeight = 1.0;
            feaParams.NStep = 5;
            feaParams.NIter = 200;
            feaParams.NPrint = 10;
            feaParams.LFact = 1.0;
            feaParams.GFact = 1.0;

            AnalysisType = AnalysisType.Bishop;
        }


        public void RunAnalysis ()
        {
            string[] path;

            switch ( AnalysisType )
            {
                case AnalysisType.Bishop:
                    this.IsAnalyzing = true;

                    if ( boundary.SaveMesh() )
                    {
                        analysisWorker.RunWorkerAsync();
                    }
                    else
                    {
                        MessageBox.Show( "Error generating .msh file." , "Error" );
                    }
                    break;

                case AnalysisType.RFEM:
                    this.IsAnalyzing = true;

                    if ( boundary.SaveMesh() )
                    {
                        analysisWorker.RunWorkerAsync();
                    }
                    else
                    {
                        MessageBox.Show( "Error generating .msh file." , "Error" );
                    }
                    break;

                case AnalysisType.FEA3NodedTri:
                    path = FilePath.Split( '.' );
                    FortranWrappers.slopefea3node_( path[0] , path[0].Length + 1 );
                    MessageBox.Show( "Run FEA 3 Node code." , "FEA Analysis" );
                    this.IsAnalyzed = true;
                    SaveInputFile( FilePath );
                    break;

                default:
                    path = FilePath.Split( '.' );
                    FortranWrappers.slopefea4node_( path[0] , path[0].Length + 1 );
                    MessageBox.Show( "Run FEA 4 Node code." , "FEA Analysis" );
                    this.IsAnalyzed = true;
                    SaveInputFile( FilePath );
                    break;
            }
        }


        public void StopAnalysis ()
        {
            analysisWorker.CancelAsync();

            analysisWorker = new BackgroundWorker();
            InitializeBackgroundWorker();

            this.IsAnalyzing = false;
        }


        public void SaveInputFile ( string path )
        {
            if ( path == null ) return;

            FilePath = path;

            using ( TextWriter tw = new StreamWriter( path ) )
            {
                tw.WriteLine( "*****************************************" );
                tw.WriteLine();
                tw.WriteLine( "        Slope 2011 Input File" );
                tw.WriteLine();
                string[] projectnamesplit = path.Split( new char[] { '\\' , '.' } , StringSplitOptions.RemoveEmptyEntries );
                string projectname = projectnamesplit[projectnamesplit.Length - 2];
                tw.WriteLine( "        Project Name: {0}" , projectname );
                tw.WriteLine();
                tw.WriteLine( "*****************************************" );
                tw.WriteLine();

                tw.WriteLine( "--------------------------------" );
                tw.WriteLine( "PLOTTING DATA" );
                tw.WriteLine( "--------------------------------" );
                tw.WriteLine();

                // Get units dependent scaling factor and label
                double factor;
                string units;
                switch ( Units )
                {
                    case Units.Metres: units = "m, kPa, kN/m^3"; factor = 0.0254; break;
                    case Units.Millimetres: units = "mm, kPa, kN/m^3"; factor = 25.4; break;
                    case Units.Feet: units = "ft, psi, pcf"; factor = 1.0 / 12.0; break;
                    default: units = "in, psi, pcf"; factor = 1; break;
                }

                tw.WriteLine( "Units = {0}" , units );

                tw.WriteLine();

                tw.WriteLine( "XAxisMax = {0}" , XAxisMax );
                tw.WriteLine( "XAxisMin = {0}" , XAxisMin );
                tw.WriteLine( "YAxisMax = {0}" , YAxisMax );
                tw.WriteLine( "YAxisMin = {0}" , YAxisMin );

                tw.WriteLine();

                tw.WriteLine( "XMajorDivision = {0}" , XMajorDivision );
                tw.WriteLine( "YMajorDivision = {0}" , YMajorDivision );

                tw.WriteLine();

                tw.WriteLine( "XMinorDivisions = {0}" , XMinorDivisions );
                tw.WriteLine( "YMinorDivisions = {0}" , YMinorDivisions );

                tw.WriteLine();
                tw.WriteLine();

                tw.WriteLine( "--------------------------------" );
                tw.WriteLine( "BOUNDARY GEOMETRY DATA" );
                tw.WriteLine( "--------------------------------" );
                tw.WriteLine();
                tw.WriteLine( "Number of Boundary Points = {0}" , boundary.BoundaryPoints.Count );

                tw.WriteLine();

                double xCoord , yCoord;
                Point p;

                for ( int i = 0 ; i < boundary.BoundaryPoints.Count ; i++ )
                {
                    p = boundary.BoundaryPoints[i].Point;

                    xCoord = (p.X - OriginOffsetX) / dpiX * factor * Scale;
                    yCoord = (ActualHeight - p.Y - OriginOffsetY) / dpiY * factor * Scale;

                    tw.WriteLine( "{0}, {1}" , xCoord , yCoord );
                }

                tw.WriteLine();
                if ( boundary.BoundaryPoints.Count > 0 ) tw.WriteLine();

                tw.WriteLine( "--------------------------------" );
                tw.WriteLine( "MATERIAL TYPE DATA" );
                tw.WriteLine( "--------------------------------" );
                tw.WriteLine();
                tw.WriteLine( "Number of Material Types = {0}" , materialTypes.Count - 1 );

                tw.WriteLine();

                for ( int i = 0 ; i < materialTypes.Count - 1 ; i++ )
                {
                    tw.WriteLine( "Material #{0}" , i + 1 );
                    tw.WriteLine( "Name = \"{0}\"" , materialTypes[i].Name );
                    tw.WriteLine( "Colour = {0}" , materialTypes[i].Fill );
                    tw.WriteLine( "Phi = {0}" , materialTypes[i].Phi );
                    tw.WriteLine( "Coh = {0}" , materialTypes[i].Cohesion );
                    tw.WriteLine( "Psi = {0}" , materialTypes[i].Psi );
                    tw.WriteLine( "Gamma = {0}" , materialTypes[i].Gamma );
                    tw.WriteLine( "Emod = {0}" , materialTypes[i].Emod );
                    tw.WriteLine( "Nu = {0}" , materialTypes[i].Nu );

                    tw.WriteLine();
                }

                tw.WriteLine();
                tw.WriteLine( "--------------------------------" );
                tw.WriteLine( "MATERIAL BLOCK DATA" );
                tw.WriteLine( "--------------------------------" );
                tw.WriteLine();
                tw.WriteLine( "Number of Material Blocks = {0}" , materialBlocks.Count );

                tw.WriteLine();

                for ( int i = 0 ; i < materialBlocks.Count ; i++ )
                {
                    tw.WriteLine( "Material Block #{0}" , i + 1 );
                    tw.WriteLine( "Material Type = \"{0}\"" , materialBlocks[i].Material );
                    tw.WriteLine( "Number of Boundary Points = {0}" , materialBlocks[i].BoundaryPoints.Count );

                    for ( int j = 0 ; j < materialBlocks[i].BoundaryPoints.Count ; j++ )
                    {
                        p = materialBlocks[i].BoundaryPoints[j].Point;

                        xCoord = (p.X - OriginOffsetX) / dpiX * factor * Scale;
                        yCoord = (ActualHeight - p.Y - OriginOffsetY) / dpiY * factor * Scale;

                        tw.WriteLine( "{0}, {1}, {2}, {3}, {4}" , xCoord , yCoord ,
                            materialBlocks[i].BoundaryPoints[j].IsFixedX ,
                            materialBlocks[i].BoundaryPoints[j].IsFixedY ,
                            materialBlocks[i].BoundaryPoints[j].IsPrintPoint );
                    }

                    tw.WriteLine( "Number of Line Constraints = {0}" , materialBlocks[i].LineConstraints.Count );
                    int index1 , index2;
                    foreach ( LineConstraint lc in materialBlocks[i].LineConstraints )
                    {
                        index1 = materialBlocks[i].BoundaryPoints.FindIndex( delegate( DrawingPoint pt ) { return pt == lc.Nodes[0]; } );
                        index2 = materialBlocks[i].BoundaryPoints.FindIndex( delegate( DrawingPoint pt ) { return pt == lc.Nodes[1]; } );
                        tw.WriteLine( "{0}, {1}, {2}, {3}" ,
                            index1 , index2 ,
                            lc.IsFixedX , lc.IsFixedY );
                    }

                    tw.WriteLine( "Number of Line Loads = {0}" , materialBlocks[i].LineLoads.Count );
                    foreach ( LineLoad ll in materialBlocks[i].LineLoads )
                    {
                        index1 = materialBlocks[i].BoundaryPoints.FindIndex( delegate( DrawingPoint pt ) { return pt == ll.Nodes[0]; } );
                        index2 = materialBlocks[i].BoundaryPoints.FindIndex( delegate( DrawingPoint pt ) { return pt == ll.Nodes[1]; } );
                        tw.WriteLine( "{0}, {1}, {2}, {3}, {4}, {5}, {6}, {7}" ,
                            index1 , index2 ,
                            ll.IsLoadedN , ll.NLoad1 , ll.NLoad2 ,
                            ll.IsLoadedT , ll.TLoad1 , ll.TLoad2 );
                    }

                    tw.WriteLine( "Number of Point Loads = {0}" , materialBlocks[i].PointLoads.Count );
                    foreach ( PointLoad pl in materialBlocks[i].PointLoads )
                    {
                        index1 = materialBlocks[i].BoundaryPoints.FindIndex( delegate( DrawingPoint pt ) { return pt == pl.Node; } );
                        tw.WriteLine( "{0}, {1}, {2}, {3}, {4}" ,
                            index1 ,
                            pl.IsLoadedX , pl.XLoad ,
                            pl.IsLoadedY , pl.YLoad );
                    }

                    tw.WriteLine();
                }

                tw.WriteLine();
                tw.WriteLine( "--------------------------------" );
                tw.WriteLine( "GENETIC ALGORITHM PARAMETERS" );
                tw.WriteLine( "--------------------------------" );
                tw.WriteLine();

                tw.WriteLine( "Population = {0}" , genAlgParams.Population );
                tw.WriteLine( "Generations = {0}" , genAlgParams.Generations );
                tw.WriteLine( "Fittest Proportion = {0}" , genAlgParams.FittestProportion );
                tw.WriteLine( "Mating Pool Proportion = {0}" , genAlgParams.MatingPoolProportion );
                tw.WriteLine( "Crossover Probability = {0}" , genAlgParams.CrossoverProbability );
                tw.WriteLine( "Mutation Probability = {0}" , genAlgParams.MutationProbability );
                tw.WriteLine( "Slice Width = {0}" , genAlgParams.SliceWidth );

                tw.WriteLine();
                tw.WriteLine();
                tw.WriteLine( "--------------------------------" );
                tw.WriteLine( "FEA PARAMETERS" );
                tw.WriteLine( "--------------------------------" );
                tw.WriteLine();

                tw.WriteLine( "Column Width = {0}" , feaParams.ColWidth );
                tw.WriteLine( "Row Height = {0}" , feaParams.RowHeight );
                tw.WriteLine( "Number of Load Steps = {0}" , feaParams.NStep );
                tw.WriteLine( "Number of Iterations = {0}" , feaParams.NIter );
                tw.WriteLine( "Number of Print Lines = {0}" , feaParams.NPrint );
                tw.WriteLine( "Load Factor = {0}" , feaParams.LFact );
                tw.WriteLine( "Gravity Factor = {0}" , feaParams.GFact );

                tw.WriteLine();
                tw.WriteLine();

                tw.WriteLine( "--------------------------------" );
                tw.WriteLine( "ANALYSIS STATUS" );
                tw.WriteLine( "--------------------------------" );
                tw.WriteLine();

                tw.WriteLine( "Analysis Type = {0}" , AnalysisType );
                tw.WriteLine( "Verified = {0}" , this.IsVerified );
                tw.WriteLine( "Meshed = {0}" , this.IsMeshed );
                tw.WriteLine( "Analyzed = {0}" , this.IsAnalyzed );

                tw.WriteLine();
                tw.WriteLine();

                tw.WriteLine( "--------------------------------" );
                tw.WriteLine( "ANALYSIS PHASES" );
                tw.WriteLine( "--------------------------------" );
                tw.WriteLine();

                tw.WriteLine( "Number of Analysis Phases = {0}" , FEAPhases.Count - 1 );

                tw.WriteLine();

                for ( int i = 1 ; i < FEAPhases.Count ; i++ )
                {
                    tw.WriteLine( "Phase #{0}" , FEAPhases[i].Number );

                    tw.WriteLine( "Name = \"{0}\"" , FEAPhases[i].Name );
                    tw.WriteLine( "Begin Phase = \"{0}\"" , FEAPhases[i].BeginPhase.Name );
                    tw.WriteLine( "Reset Displacements=\"{0}\"" , FEAPhases[i].ResetDisplacements );

                    tw.WriteLine( "Number of Load Steps = {0}" , FEAPhases[i].NSteps );
                    tw.WriteLine( "Number of Iterations = {0}" , FEAPhases[i].NIterations );
                    tw.WriteLine( "Number of Load Steps / Print Line = {0}" , FEAPhases[i].NPrintLines );
                    tw.WriteLine( "Gravity Factor = {0}" , FEAPhases[i].GravityFactor );

                    for ( int j = 0 ; j < MaterialBlocks.Count ; j++ )
                    {
                        tw.WriteLine( "Material Block #{0}" , j + 1 );
                        tw.WriteLine( MaterialBlocks[j].PhaseMaterials[i - 1].Name );
                    }

                    tw.WriteLine();
                }
            }

            this.IsSaved = true;
        }

        public void OpenInputFile ( string path )
        {
            if ( path == null ) return;

            FilePath = path;

            using ( TextReader tr = new StreamReader( path ) )
            {
                tr.ReadLine();      // *****************************************
                tr.ReadLine();      //
                tr.ReadLine();      //         Slope 2011 Input File
                tr.ReadLine();      //
                tr.ReadLine();      //         Project Name:
                tr.ReadLine();      //
                tr.ReadLine();      // *****************************************
                tr.ReadLine();      //
                tr.ReadLine();      // --------------------------------
                tr.ReadLine();      // PLOTTING DATA
                tr.ReadLine();      // --------------------------------
                tr.ReadLine();      //

                string units = tr.ReadLine().Split( new char[] { '=' , ',' , ' ' } , StringSplitOptions.RemoveEmptyEntries )[1].ToLower();
                double factor;
                switch ( units )
                {
                    case "m":
                        Units = Units.Metres;
                        factor = 0.0254;
                        break;
                    case "mm":
                        Units = Units.Millimetres;
                        factor = 25.4;
                        break;
                    case "ft":
                        Units = Units.Feet;
                        factor = 1.0 / 12.0;
                        break;
                    default:
                        Units = Units.Inches;
                        factor = 1.0;
                        break;
                }

                tr.ReadLine();

                XAxisMax = double.Parse( tr.ReadLine().Split( '=' )[1] );
                XAxisMin = double.Parse( tr.ReadLine().Split( '=' )[1] );
                YAxisMax = double.Parse( tr.ReadLine().Split( '=' )[1] );
                YAxisMin = double.Parse( tr.ReadLine().Split( '=' )[1] );

                tr.ReadLine();

                XMajorDivision = double.Parse( tr.ReadLine().Split( '=' )[1] );
                YMajorDivision = double.Parse( tr.ReadLine().Split( '=' )[1] );

                tr.ReadLine();

                XMinorDivisions = int.Parse( tr.ReadLine().Split( '=' )[1] );
                YMinorDivisions = int.Parse( tr.ReadLine().Split( '=' )[1] );

                BuildAxes();

                tr.ReadLine();      //
                tr.ReadLine();      //
                tr.ReadLine();      // --------------------------------
                tr.ReadLine();      // BOUNDARY GEOMETRY DATA
                tr.ReadLine();      // --------------------------------
                tr.ReadLine();      //

                int numBoundPoints = int.Parse( tr.ReadLine().Split( '=' )[1] );

                tr.ReadLine();

                if ( numBoundPoints > 0 )
                {
                    Point[] boundPoints = new Point[numBoundPoints];
                    double xCoord , yCoord;
                    string[] coords;
                    for ( int i = 0 ; i < numBoundPoints ; i++ )
                    {
                        coords = tr.ReadLine().Split( ',' );
                        xCoord = double.Parse( coords[0] );
                        yCoord = double.Parse( coords[1] );
                        boundPoints[i].X = xCoord / (factor * Scale) * dpiX + OriginOffsetX;
                        boundPoints[i].Y = ActualHeight - (yCoord / (factor * Scale) * dpiY + OriginOffsetY);
                    }
                    boundary = new SlopeBoundary( this , boundPoints );

                    tr.ReadLine();
                }

                tr.ReadLine();      //
                tr.ReadLine();      // --------------------------------
                tr.ReadLine();      // MATERIAL TYPE DATA
                tr.ReadLine();      // --------------------------------
                tr.ReadLine();      //

                int numMaterials = int.Parse( tr.ReadLine().Split( '=' )[1] );

                tr.ReadLine();

                MaterialType newMaterial;
                for ( int i = 0 ; i < numMaterials ; i++ )
                {
                    tr.ReadLine();

                    newMaterial = new MaterialType();
                    newMaterial.Name = tr.ReadLine().Split( new char[] { '\"' } , StringSplitOptions.RemoveEmptyEntries )[1];
                    newMaterial.Fill = new SolidColorBrush( (Color) ColorConverter.ConvertFromString(
                        tr.ReadLine().Split( new char[] { '=' , ' ' } , StringSplitOptions.RemoveEmptyEntries )[1] ) );
                    newMaterial.Phi = double.Parse( tr.ReadLine().Split( '=' )[1] );
                    newMaterial.Cohesion = double.Parse( tr.ReadLine().Split( '=' )[1] );
                    newMaterial.Psi = double.Parse( tr.ReadLine().Split( '=' )[1] );
                    newMaterial.Gamma = double.Parse( tr.ReadLine().Split( '=' )[1] );
                    newMaterial.Emod = double.Parse( tr.ReadLine().Split( '=' )[1] );
                    newMaterial.Nu = double.Parse( tr.ReadLine().Split( '=' )[1] );

                    tr.ReadLine();

                    materialTypes.Insert( this.materialTypes.Count - 1 , newMaterial );
                }

                tr.ReadLine();      //
                tr.ReadLine();      // --------------------------------
                tr.ReadLine();      // MATERIAL BLOCK DATA
                tr.ReadLine();      // --------------------------------
                tr.ReadLine();      //

                int numMaterialBlocks = int.Parse( tr.ReadLine().Split( '=' )[1] );

                tr.ReadLine();

                if ( numMaterialBlocks > 0 )
                {
                    MaterialBlock newMaterialBlock;
                    MaterialType newMaterialType;
                    LineConstraint newLineConstraint, existingLineConstraint;
                    Point[] materialBoundPoints;
                    bool[] isFixedX;
                    bool[] isFixedY;
                    bool[] isPrintPoint;
                    string materialName;
                    int numMaterialBoundPoints , numLineConstraints , numLineLoads , numPointLoads;
                    double xCoord , yCoord;
                    string[] coords , lineConstraint , lineLoad , pointLoad;

                    for ( int i = 0 ; i < numMaterialBlocks ; i++ )
                    {
                        tr.ReadLine();

                        materialName = tr.ReadLine().Split( new char[] { '\"' } , StringSplitOptions.RemoveEmptyEntries )[1];

                        numMaterialBoundPoints = int.Parse( tr.ReadLine().Split( '=' )[1] );

                        materialBoundPoints = new Point[numMaterialBoundPoints];
                        isFixedX = new bool[numMaterialBoundPoints];
                        isFixedY = new bool[numMaterialBoundPoints];
                        isPrintPoint = new bool[numMaterialBoundPoints];

                        for ( int j = 0 ; j < numMaterialBoundPoints ; j++ )
                        {
                            coords = tr.ReadLine().Split( new char[] { ',' , ' ' } , StringSplitOptions.RemoveEmptyEntries );
                            xCoord = double.Parse( coords[0] );
                            yCoord = double.Parse( coords[1] );
                            materialBoundPoints[j].X = xCoord / (factor * Scale) * dpiX + OriginOffsetX;
                            materialBoundPoints[j].Y = ActualHeight - (yCoord / (factor * Scale) * dpiY + OriginOffsetY);
                            isFixedX[j] = coords[2] == Boolean.TrueString;
                            isFixedY[j] = coords[3] == Boolean.TrueString;
                            isPrintPoint[j] = coords[4] == Boolean.TrueString;
                        }

                        newMaterialBlock = new MaterialBlock( this , materialBoundPoints );
                        for ( int j = 0 ; j < numMaterialBoundPoints ; j++ )
                        {
                            newMaterialBlock.BoundaryPoints[j].IsFixedX = isFixedX[j];
                            newMaterialBlock.BoundaryPoints[j].IsFixedY = isFixedY[j];
                            newMaterialBlock.BoundaryPoints[j].IsPrintPoint = isPrintPoint[j];
                        }

                        numLineConstraints = int.Parse( tr.ReadLine().Split( '=' )[1] );
                        for ( int j = 0 ; j < numLineConstraints ; j++ )
                        {
                            lineConstraint = tr.ReadLine().Split( new char[] { ',' , ' ' } , StringSplitOptions.RemoveEmptyEntries );
                            newLineConstraint = new LineConstraint( this ,
                                newMaterialBlock.BoundaryPoints[int.Parse( lineConstraint[0] )] ,
                                newMaterialBlock.BoundaryPoints[int.Parse( lineConstraint[1] )] ,
                                lineConstraint[2] == Boolean.TrueString ,
                                lineConstraint[3] == Boolean.TrueString );
                            existingLineConstraint = null;
                            foreach ( MaterialBlock mb in materialBlocks )
                            {
                                existingLineConstraint = mb.LineConstraints.Find(
                                    delegate( LineConstraint lc )
                                    {
                                        return lc.Nodes.Contains( newLineConstraint.Nodes[0] )
                                            && lc.Nodes.Contains( newLineConstraint.Nodes[1] );
                                    } );
                                if ( existingLineConstraint != null ) break;
                            }
                            if ( existingLineConstraint != null )
                            {
                                if ( !newMaterialBlock.LineConstraints.Contains( existingLineConstraint ) )
                                    newMaterialBlock.LineConstraints.Add( existingLineConstraint );
                            }
                            else newMaterialBlock.LineConstraints.Add( newLineConstraint );
                        }

                        numLineLoads = int.Parse( tr.ReadLine().Split( '=' )[1] );
                        for ( int j = 0 ; j < numLineLoads ; j++ )
                        {
                            lineLoad = tr.ReadLine().Split( new char[] { ',' , ' ' } , StringSplitOptions.RemoveEmptyEntries );
                            newMaterialBlock.LineLoads.Add( new LineLoad( this ,
                                newMaterialBlock.BoundaryPoints[int.Parse( lineLoad[0] )] ,
                                newMaterialBlock.BoundaryPoints[int.Parse( lineLoad[1] )] ,
                                lineLoad[2] == Boolean.TrueString , double.Parse( lineLoad[3] ) , double.Parse( lineLoad[4] ) ,
                                lineLoad[5] == Boolean.TrueString , double.Parse( lineLoad[6] ) , double.Parse( lineLoad[7] ) ) );
                        }

                        numPointLoads = int.Parse( tr.ReadLine().Split( '=' )[1] );
                        for ( int j = 0 ; j < numPointLoads ; j++ )
                        {
                            pointLoad = tr.ReadLine().Split( new char[] { ',' , ' ' } , StringSplitOptions.RemoveEmptyEntries );
                            newMaterialBlock.PointLoads.Add( new PointLoad( this ,
                                newMaterialBlock.BoundaryPoints[int.Parse( pointLoad[0] )] ,
                                pointLoad[1] == Boolean.TrueString , double.Parse( pointLoad[2] ) ,
                                pointLoad[3] == Boolean.TrueString , double.Parse( pointLoad[4] ) ) );
                        }

                        newMaterialType = materialTypes.Find( delegate( MaterialType mt ) { return mt.Name == materialName; } );

                        if ( newMaterialType != null ) newMaterialBlock.Material = newMaterialType;

                        materialBlocks.Add( newMaterialBlock );

                        tr.ReadLine();
                    }
                }

                tr.ReadLine();      //
                tr.ReadLine();      // --------------------------------
                tr.ReadLine();      // GENETIC ALGORITHM PARAMETERS
                tr.ReadLine();      // --------------------------------
                tr.ReadLine();      //

                genAlgParams.Population = int.Parse( tr.ReadLine().Split( '=' )[1] );
                genAlgParams.Generations = int.Parse( tr.ReadLine().Split( '=' )[1] );
                genAlgParams.FittestProportion = double.Parse( tr.ReadLine().Split( '=' )[1] );
                genAlgParams.MatingPoolProportion = double.Parse( tr.ReadLine().Split( '=' )[1] );
                genAlgParams.CrossoverProbability = double.Parse( tr.ReadLine().Split( '=' )[1] );
                genAlgParams.MutationProbability = double.Parse( tr.ReadLine().Split( '=' )[1] );
                genAlgParams.SliceWidth = double.Parse( tr.ReadLine().Split( '=' )[1] );

                tr.ReadLine();      //
                tr.ReadLine();      //
                tr.ReadLine();      // --------------------------------
                tr.ReadLine();      // FEA PARAMETERS
                tr.ReadLine();      // --------------------------------
                tr.ReadLine();      //

                feaParams.ColWidth = double.Parse( tr.ReadLine().Split( '=' )[1] );
                feaParams.RowHeight = double.Parse( tr.ReadLine().Split( '=' )[1] );
                feaParams.NStep = int.Parse( tr.ReadLine().Split( '=' )[1] );
                feaParams.NIter = int.Parse( tr.ReadLine().Split( '=' )[1] );
                feaParams.NPrint = int.Parse( tr.ReadLine().Split( '=' )[1] );
                feaParams.LFact = double.Parse( tr.ReadLine().Split( '=' )[1] );
                feaParams.GFact = double.Parse( tr.ReadLine().Split( '=' )[1] );

                tr.ReadLine();      //
                tr.ReadLine();      //
                tr.ReadLine();      // --------------------------------
                tr.ReadLine();      // ANALYSIS STATUS
                tr.ReadLine();      // --------------------------------
                tr.ReadLine();      //

                string analysistype = tr.ReadLine().Split( new char[] { '=' , ' ' } , StringSplitOptions.RemoveEmptyEntries )[2];

                switch ( analysistype )
                {
                    case "Bishop": AnalysisType = AnalysisType.Bishop; break;
                    case "RFEM": AnalysisType = AnalysisType.RFEM; break;
                    case "FEA4NodedQuad": AnalysisType = AnalysisType.FEA4NodedQuad; break;
                    default: AnalysisType = AnalysisType.FEA3NodedTri; break;
                }

                this.IsVerified = tr.ReadLine().Split( new char[] { '=' , ' ' } , StringSplitOptions.RemoveEmptyEntries )[1]
                                        == Boolean.TrueString;

                this.IsMeshed = tr.ReadLine().Split( new char[] { '=' , ' ' } , StringSplitOptions.RemoveEmptyEntries )[1]
                                        == Boolean.TrueString;



                this.IsAnalyzed = tr.ReadLine().Split( new char[] { '=' , ' ' } , StringSplitOptions.RemoveEmptyEntries )[1]
                                        == Boolean.TrueString;

                this.ShowCritical = this.IsAnalyzed;


                tr.ReadLine();      //
                tr.ReadLine();      //
                tr.ReadLine();      // --------------------------------
                tr.ReadLine();      // ANALYSIS PHASES
                tr.ReadLine();      // --------------------------------
                tr.ReadLine();      //

                int numAnalysisPhases = int.Parse( tr.ReadLine().Split( '=' )[1] );

                tr.ReadLine();

                if ( numAnalysisPhases > 0 )
                {
                    AnalysisPhase beginPhase;
                    int number , nstep , niter , nprint;
                    string name , beginName, mtlType;
                    bool reset;
                    double gfact;

                    for ( int i = 0 ; i < numAnalysisPhases ; i++ )
                    {
                        number = int.Parse( tr.ReadLine().Split( '#' )[1] );
                        name = tr.ReadLine().Split( new char[] { '\"' } , StringSplitOptions.RemoveEmptyEntries )[1];
                        beginName = tr.ReadLine().Split( new char[] { '\"' } , StringSplitOptions.RemoveEmptyEntries )[1];
                        beginPhase = FEAPhases.Find( delegate( AnalysisPhase ap ) { return ap.Name == beginName; } );
                        reset = tr.ReadLine().Split( new char[] { '\"' } , StringSplitOptions.RemoveEmptyEntries )[1] == Boolean.TrueString;
                        nstep = int.Parse( tr.ReadLine().Split( '=' )[1] );
                        niter = int.Parse( tr.ReadLine().Split( '=' )[1] );
                        nprint = int.Parse( tr.ReadLine().Split( '=' )[1] );
                        gfact = double.Parse( tr.ReadLine().Split( '=' )[1] );

                        for ( int j = 0 ; j < MaterialBlocks.Count ; j++ )
                        {
                            tr.ReadLine();
                            mtlType = tr.ReadLine();
                            MaterialBlocks[j].PhaseMaterials.Add( MaterialTypes.Find( delegate( MaterialType mt ) { return mt.Name == mtlType; } ) );
                        }

                        tr.ReadLine();

                        FEAPhases.Add( new AnalysisPhase( number , name , beginPhase , reset , nstep , niter , nprint , gfact ) );
                    }
                }
            }

            ClosableCanvasTabItem parentTab = (ClosableCanvasTabItem) ((Grid) this.Parent).Parent;
            TabControl windowManager = (TabControl) parentTab.Parent;

            if ( this.IsMeshed )
            {
                switch ( AnalysisType )
                {
                    case AnalysisType.Bishop: boundary.GenerateMesh( boundary.XMin , boundary.XMax ); break;
                    case AnalysisType.RFEM: boundary.GenerateMesh( boundary.XMin , boundary.XMax ); break;
                    case AnalysisType.FEA3NodedTri: LoadFEA3NodedTriMesh(); break;
                    default: LoadFEA4NodedQuadMesh(); break;
                }

            }

            if ( this.IsAnalyzed )
            {
                string[] pathsplit = FilePath.Split( '.' );
                pathsplit[pathsplit.Length - 1] = "bish";
                string bishpath = string.Join( "." , pathsplit );

                if ( File.Exists( bishpath ) )
                {
                    Menu mainMenu = (Menu) ((DockPanel) ((Grid) windowManager.Parent).Children[0]).Children[0];
                    MenuItem outputMenu = (MenuItem) mainMenu.Items[4];
                    MenuItem showCritical = (MenuItem) outputMenu.Items[0];

                    List<string> contents = new List<string>( File.ReadAllLines( bishpath ) );

                    int icritical = contents.FindIndex( delegate( string s ) { return s.Contains( "MOST CRITICAL SURFACE" ); } );

                    double factor;
                    switch ( Units )
                    {
                        case Units.Metres: factor = 0.0254; break;
                        case Units.Millimetres: factor = 25.4; break;
                        case Units.Feet: factor = 1.0 / 12.0; break;
                        default: factor = 1.0; break;
                    }

                    // Read in global critical surface geometry
                    double radius = double.Parse( contents[icritical + 4].Split( '=' )[1] );
                    double xEnter = double.Parse( contents[icritical + 5].Split( '=' )[1] );
                    double yEnter = double.Parse( contents[icritical + 6].Split( '=' )[1] );
                    double xExit = double.Parse( contents[icritical + 7].Split( '=' )[1] );
                    double yExit = double.Parse( contents[icritical + 8].Split( '=' )[1] );

                    // Convert to screen pixel units
                    radius = radius / (factor * Scale) * dpiX;
                    xEnter = xEnter / (factor * Scale) * dpiX + OriginOffsetX;
                    yEnter = ActualHeight - (yEnter / (factor * Scale) * dpiY + OriginOffsetY);
                    xExit = xExit / (factor * Scale) * dpiX + OriginOffsetX;
                    yExit = ActualHeight - (yExit / (factor * Scale) * dpiY + OriginOffsetY);

                    // Create new surface
                    criticalSurface = new DisplayCircularSurface( this ,
                        new Point( xEnter , yEnter ) , new Point( xExit , yExit ) , radius );

                    // Indicate that it is the global critical surface with thicker line
                    criticalSurface.IsGlobalCritical = true;
                    showCritical.IsChecked = true;
                }
            }

            windowManager.SelectedItem = null;
            windowManager.SelectedItem = parentTab;

            this.IsSaved = true;

            BuildAxes();
            CentreAndFitExtents( true );
        }


        /// <summary>
        /// Construction of axes and grid
        /// </summary>
        public void BuildAxes ()
        {
            // Compute plot scaling factors [(pixels/inch) / (actual units/screen unit)]
            double xFactor = dpiX / Scale;
            double yFactor = dpiY / Scale;
            switch ( Units )
            {
                // Apply unit modification factor (screen units/inch)
                case Units.Metres: xFactor /= 0.0254; yFactor /= 0.0254; break;
                case Units.Millimetres: xFactor /= 25.4; yFactor /= 25.4; break;
                case Units.Feet: xFactor *= 12.0; yFactor *= 12.0; break;
                default: break;
            } // Result --> (pixels / actual unit)

            // Compute minor grid spacing (actual units)
            double xMinor = XMajorDivision / XMinorDivisions;
            double yMinor = YMajorDivision / YMinorDivisions;

            /*
             * Refresh X axis
             */
            xAxis.Children.Clear();

            // Add surrounding border
            Border xAxisBorder = new Border();
            xAxisBorder.BorderBrush = Brushes.DimGray;
            xAxisBorder.VerticalAlignment = VerticalAlignment.Stretch;
            xAxisBorder.HorizontalAlignment = HorizontalAlignment.Stretch;
            xAxisBorder.BorderThickness = new Thickness( 1 );
            xAxisBorder.Margin = new Thickness( 0 );
            xAxis.Children.Add( xAxisBorder );

            // Add appropriate units label
            TextBlock xLabel = new TextBlock();
            string units;
            switch ( this.Units )
            {
                case Units.Metres: units = "m"; break;
                case Units.Millimetres: units = "mm"; break;
                case Units.Feet: units = "ft"; break;
                default: units = "in"; break;
            }
            xLabel.Text = String.Format( "X ({0})" , units );
            xLabel.FontSize = 10;
            xLabel.VerticalAlignment = VerticalAlignment.Bottom;
            xLabel.HorizontalAlignment = HorizontalAlignment.Left;
            xLabel.Margin = new Thickness( 10 , 0 , 0 , 7 );
            xAxis.Children.Add( xLabel );

            Line majorLine , minorLine;      // For boxing axis lines and labels
            TextBlock majorLabel;

            // Add major and minor axis lines and labels to major divisions
            for ( double xMajor = XAxisMin ; xMajor <= XAxisMax ; xMajor += XMajorDivision )
            {
                majorLine = new Line();
                majorLine.Stroke = Brushes.Black;
                majorLine.X1 = majorLine.X2 = xMajor * xFactor + OriginOffsetX;
                majorLine.Y1 = 0;
                majorLine.Y2 = 25;
                xAxis.Children.Add( majorLine );

                majorLabel = new TextBlock();
                majorLabel.Text = String.Format( "{0}" , Math.Round( xMajor , 2 ) );
                majorLabel.FontSize = 10;
                majorLabel.VerticalAlignment = VerticalAlignment.Top;
                majorLabel.HorizontalAlignment = HorizontalAlignment.Left;
                majorLabel.Margin = new Thickness( majorLine.X1 + 5 , 13 , 0 , 0 );
                xAxis.Children.Add( majorLabel );

                if ( Math.Abs( xMajor - this.XAxisMax ) > 1e-3 )
                {
                    for ( int ixMinor = 1 ; ixMinor < XMinorDivisions ; ixMinor++ )
                    {
                        minorLine = new Line();
                        minorLine.Stroke = Brushes.Black;
                        minorLine.X1 = minorLine.X2 = majorLine.X1 + ixMinor * xMinor * xFactor;
                        minorLine.Y1 = 0;
                        minorLine.Y2 = 10;
                        xAxis.Children.Add( minorLine );
                    }
                }
            }

            /*
             * Refresh Y axis
             */
            yAxis.Children.Clear();

            // Add surrounding border
            Border yAxisBorder = new Border();
            yAxisBorder.BorderBrush = Brushes.DimGray;
            yAxisBorder.VerticalAlignment = VerticalAlignment.Stretch;
            yAxisBorder.HorizontalAlignment = HorizontalAlignment.Stretch;
            yAxisBorder.BorderThickness = new Thickness( 1 );
            yAxisBorder.Margin = new Thickness( 0 );
            yAxis.Children.Add( yAxisBorder );

            // Add appropriate units label
            TextBlock yLabel = new TextBlock();
            yLabel.Text = String.Format( "Y\n({0})" , units );
            yLabel.FontSize = 10;
            yLabel.TextAlignment = TextAlignment.Right;
            yLabel.VerticalAlignment = VerticalAlignment.Bottom;
            yLabel.HorizontalAlignment = HorizontalAlignment.Right;
            yLabel.Margin = new Thickness( 0 , 0 , 35 , 10 );
            yAxis.Children.Add( yLabel );

            // Add major and minor axis lines and labels to major divisions
            // (NB: Actual units origin @ (0, ActualHeight) which is BL corner
            //      while screen pixels origin @ (0,0) which is TL corner
            for ( double yMajor = YAxisMin ; yMajor <= YAxisMax ; yMajor += YMajorDivision )
            {
                majorLine = new Line();
                majorLine.Stroke = Brushes.Black;
                majorLine.Y1 = majorLine.Y2 = ActualHeight - (yMajor * yFactor + OriginOffsetY);
                majorLine.X1 = 65;
                majorLine.X2 = 40;
                yAxis.Children.Add( majorLine );

                majorLabel = new TextBlock();
                majorLabel.Text = String.Format( "{0}" , Math.Round( yMajor , 2 ) );
                majorLabel.FontSize = 10;
                majorLabel.VerticalAlignment = VerticalAlignment.Bottom;
                majorLabel.HorizontalAlignment = HorizontalAlignment.Right;
                majorLabel.TextAlignment = TextAlignment.Right;
                majorLabel.Margin = new Thickness( 0 , 0 , 15 , ActualHeight - majorLine.Y1 + 3 );
                yAxis.Children.Add( majorLabel );

                if ( Math.Abs( yMajor - YAxisMax ) > 1e-3 )
                {
                    for ( int iyMinor = 1 ; iyMinor < YMinorDivisions ; iyMinor++ )
                    {
                        minorLine = new Line();
                        minorLine.Stroke = Brushes.Black;
                        minorLine.Y1 = minorLine.Y2 = majorLine.Y1 - iyMinor * yMinor * yFactor;
                        minorLine.X1 = 65;
                        minorLine.X2 = 55;
                        yAxis.Children.Add( minorLine );
                    }
                }
            }

            /*
             * Refresh drawing canvas
             */
            this.Children.Clear();
            gridPoints.Clear();

            GridPoint gp;
            Point p;

            bool createMajor = ((XAxisMax - XAxisMin) / XMajorDivision + 1) * ((YAxisMax - YAxisMin) / YMajorDivision + 1) < 15000
                                        ? true : false;

            bool createMinor = ((XAxisMax - XAxisMin) / xMinor + 1) * ((YAxisMax - YAxisMin) / yMinor + 1) < 15000
                                        ? true : false;

            // Create major and minor grid points
            if ( createMajor )
            {
                for ( double x = XAxisMin ; x <= XAxisMax ; x += xMinor )
                {
                    for ( double y = YAxisMin ; y <= YAxisMax ; y += yMinor )
                    {
                        // Create and add the grid point
                        p = new Point( x * xFactor + OriginOffsetX , ActualHeight - (y * yFactor + OriginOffsetY) );

                        // Determine if it is a major or minor grid point
                        // and tag it appropriately
                        double xRatio = x / XMajorDivision;
                        double yRatio = y / YMajorDivision;

                        gp = null;
                        // If x and y major ratios are both whole numbers, it is a major grid point
                        if ( Math.Abs( xRatio - Math.Truncate( xRatio ) ) < 1e-3
                            && Math.Abs( yRatio - Math.Truncate( yRatio ) ) < 1e-3 )
                        {
                            gp = new GridPoint( p , GridType.Major );
                        }
                        else if ( createMinor )
                        {
                            gp = new GridPoint( p , GridType.Minor );
                        }

                        if ( gp != null )
                        {
                            // Set appropriate visibility
                            if ( ShowMinorGrid || (ShowMajorGrid && gp.Type == GridType.Major) )
                            {
                                gp.IsVisible = true;
                            }
                            else
                            {
                                gp.IsVisible = false;
                            }


                            gridPoints.Add( gp );

                            // Add grid point to canvas
                            this.Children.Add( gp.Location );
                        }
                    }
                }
            }

            // Add drawing Polygon objects back on top
            this.Children.Add( boundary.Boundary );
            materialBlocks.ForEach( delegate( MaterialBlock mb ) { this.Children.Add( mb.Boundary ); } );

            // Add elements on top of polygons
            FEATriElements.ForEach(
                delegate( fe3NodedTriElement element ) { this.Children.Add( element.Boundary ); } );
            FEAQuadElements.ForEach(
                delegate( fe4NodedQuadElement element ) { this.Children.Add( element.Boundary ); } );

            // Add constraints and loads on top of polygons, elements
            materialBlocks.ForEach(
                delegate( MaterialBlock mb )
                {
                    mb.BoundaryPoints.ForEach(
                        delegate( DrawingPoint dp )
                        {
                            dp.FixLines.ForEach( delegate( Polyline l ) { if ( !this.Children.Contains( l ) ) this.Children.Add( l ); } );
                        } );

                    mb.LineConstraints.ForEach(
                        delegate( LineConstraint lc )
                        {
                            lc.FixLines.ForEach(
                                delegate( Polyline l ) { if ( !this.Children.Contains( l ) ) this.Children.Add( l ); } );
                        } );

                    mb.LineLoads.ForEach(
                        delegate( LineLoad ll )
                        {
                            ll.LoadLines.ForEach(
                                delegate( Polyline l ) { if ( !this.Children.Contains( l ) ) this.Children.Add( l ); } );
                        } );

                    mb.PointLoads.ForEach(
                        delegate( PointLoad pl )
                        {
                            pl.LoadLines.ForEach(
                                delegate( Polyline l ) { if ( !this.Children.Contains( l ) )this.Children.Add( l ); } );
                        } );
                } );

            // Add vertex points on top of polygons, elements, constraints/loads
            boundary.BoundaryPoints.ForEach( delegate( DrawingPoint dp ) { this.Children.Add( dp.Dot ); } );
            materialBlocks.ForEach( delegate( MaterialBlock mb )
            {
                mb.BoundaryPoints.ForEach(
                    delegate( DrawingPoint dp ) { if ( !this.Children.Contains( dp.Dot ) ) this.Children.Add( dp.Dot ); } );
            } );

            // Add analysis results on top of problem definition
            if ( criticalSurface != null ) this.Children.Add( criticalSurface.Surface );
            runSurfaces.ForEach( delegate( DisplayCircularSurface rs ) { this.Children.Add( rs.Surface ); } );

            // Add temporary drawing objects on top of everything
            this.Children.Add( drawLine );
            this.Children.Add( zoomRect.Boundary );

            if ( !createMajor )
            {
                MessageBox.Show( "Too many grid points to display.\nSnapping still active." , "Display Optimization" );
            }
            else if ( !createMinor )
            {
                MessageBox.Show( "Too many minor grid points to display.\nMajor grid points still shown.\nSnapping still active." , "Display Optimization" );
            }
        }


        /// <summary>
        /// Apply translation to canvas and axes content
        /// </summary>
        public void Translate ( Vector delta )
        {
            /*
             * Update plotting origin (in pixels)
             */
            OriginOffsetX += delta.X;
            OriginOffsetY -= delta.Y;

            /*
             * Translate axis components
             */

            Line line;          // For unboxing axis lines and labels
            TextBlock tb;

            // Update coordinates of x axis objects
            for ( int i = 2 ; i < xAxis.Children.Count ; i++ )
            {
                if ( xAxis.Children[i] is Line )
                {
                    line = xAxis.Children[i] as Line;
                    line.X1 = line.X2 += delta.X;
                }
                else if ( xAxis.Children[i] is TextBlock )
                {
                    tb = xAxis.Children[i] as TextBlock;
                    tb.Margin = new Thickness( tb.Margin.Left + delta.X , tb.Margin.Top , 0 , 0 );
                }
            }

            // Update coordinates of y axis objects
            for ( int i = 2 ; i < yAxis.Children.Count ; i++ )
            {
                if ( yAxis.Children[i] is Line )
                {
                    line = yAxis.Children[i] as Line;
                    line.Y1 = line.Y2 += delta.Y;
                }
                else if ( yAxis.Children[i] is TextBlock )
                {
                    tb = yAxis.Children[i] as TextBlock;
                    tb.Margin = new Thickness( 0 , 0 , tb.Margin.Right , tb.Margin.Bottom - delta.Y );
                }
            }

            /*
             * Translate plotting canvas components (grid points and drawing polygons)
             */

            // Update grid point locations
            gridPoints.ForEach( delegate( GridPoint gp ) { gp.Translate( delta ); } );

            // Update boundary shape
            boundary.Translate( delta );

            // Update material blocks
            materialBlocks.ForEach( delegate( MaterialBlock mb ) { mb.Translate( delta ); } );

            // Update FEA elements
            FEATriElements.ForEach( delegate( fe3NodedTriElement element ) { element.Translate( delta ); } );
            FEAQuadElements.ForEach( delegate( fe4NodedQuadElement element ) { element.Translate( delta ); } );

            // Update drawing line
            Point p;
            for ( int i = 0 ; i < drawLine.Points.Count ; i++ )
            {
                p = drawLine.Points[i];
                p.X += delta.X;
                p.Y += delta.Y;
                drawLine.Points[i] = p;
            }

            // Update critical surface
            if ( criticalSurface != null ) criticalSurface.Translate( delta );

            // Update run surfaces
            runSurfaces.ForEach( delegate( DisplayCircularSurface rs ) { rs.Translate( delta ); } );
        }


        /// <summary>
        /// Shows or unshows appropriate grid points
        /// </summary>
        public void UpdateGridDisplay ()
        {
            for ( int i = 0 ; i < gridPoints.Count ; i++ )
            {
                if ( ShowMinorGrid || (ShowMajorGrid && gridPoints[i].Type == GridType.Major) )
                {
                    gridPoints[i].IsVisible = true;
                }
                else
                {
                    gridPoints[i].IsVisible = false;
                }
            }
        }


        /// <summary>
        /// Zooms canvas content and axes
        /// </summary>
        /// <param name="factor">scaling factor for zoom</param>
        /// <param name="centre">central reference point</param>
        public void Zoom ( double factor , Point centre )
        {
            // Update plotting scale
            Scale /= factor;

            // Modify status bar display of scale
            StatusBar status =
                (StatusBar) ((DockPanel) ((Grid) ((TabControl) ((TabItem) ((Grid) this.Parent).Parent).Parent).Parent).Children[1]).Children[0];
            StatusBarItem statusLabel = (StatusBarItem) status.Items[2];
            statusLabel.Content = String.Format( "Scale = {0} : 1" , Math.Round( Scale , 2 ) );

            // Update plotting origin (in pixels)
            OriginOffsetX = centre.X + factor * (OriginOffsetX - centre.X);
            OriginOffsetY = (ActualHeight - centre.Y) - factor * ((ActualHeight - OriginOffsetY) - centre.Y);

            /*
             * Zoom plotting axes
             */

            Line line;          // For unboxing axis lines and labels
            TextBlock tb;

            // Zoom x axis components
            for ( int i = 2 ; i < xAxis.Children.Count ; i++ )
            {
                if ( xAxis.Children[i] is Line )
                {
                    line = xAxis.Children[i] as Line;
                    line.X1 = line.X2 = centre.X + factor * (line.X2 - centre.X);
                }
                else if ( xAxis.Children[i] is TextBlock )
                {
                    tb = xAxis.Children[i] as TextBlock;
                    tb.Margin = new Thickness( centre.X + factor * (tb.Margin.Left - centre.X) , tb.Margin.Top , 0 , 0 );
                }
            }

            // Zoom y axis components
            for ( int i = 2 ; i < yAxis.Children.Count ; i++ )
            {
                if ( yAxis.Children[i] is Line )
                {
                    line = yAxis.Children[i] as Line;
                    line.Y1 = line.Y2 = centre.Y + factor * (line.Y2 - centre.Y);
                }
                else if ( yAxis.Children[i] is TextBlock )
                {
                    tb = yAxis.Children[i] as TextBlock;
                    tb.Margin = new Thickness( 0 , 0 , tb.Margin.Right , (yAxis.ActualHeight - centre.Y) - factor * (yAxis.ActualHeight - tb.Margin.Bottom - centre.Y) );
                }
            }

            /*
             * Zoom canvas plotting components (grid and drawing objects)
             */

            // Zoom grid point locations
            gridPoints.ForEach( delegate( GridPoint gp ) { gp.Zoom( factor , centre ); } );

            // Zoom boundary shape
            boundary.Zoom( factor , centre );

            // Zoom material blocks
            materialBlocks.ForEach( delegate( MaterialBlock mb ) { mb.Zoom( factor , centre ); } );

            // Zoom FEA elements
            FEATriElements.ForEach( delegate( fe3NodedTriElement element ) { element.Zoom( factor , centre ); } );
            FEAQuadElements.ForEach( delegate( fe4NodedQuadElement element ) { element.Zoom( factor , centre ); } );

            // Zoom drawing line
            Point p;
            for ( int i = 0 ; i < drawLine.Points.Count ; i++ )
            {
                p = drawLine.Points[i];
                p.X = centre.X + factor * (p.X - centre.X);
                p.Y = centre.Y + factor * (p.Y - centre.Y);
                drawLine.Points[i] = p;
            }

            // Zoom critical surface
            if ( criticalSurface != null ) criticalSurface.Zoom( factor , centre );

            // Zoom run surfaces
            runSurfaces.ForEach( delegate( DisplayCircularSurface rs ) { rs.Zoom( factor , centre ); } );
        }


        /// <summary>
        /// Fits boundaries and axes extents to view window
        /// (Essentially a wrapper for Translate and Zoom with computation of appropriate
        /// translation and scaling factors)
        /// </summary>
        /// <param name="zoom">true = centre view AND fit extents, false = just centre view</param>
        public void CentreAndFitExtents ( bool zoom )
        {
            // Get initial values for extents from plotting axes
            // (NB: Drawing is allowed outside this range and this
            //      will also be checked)
            double xMax = XAxisMax;
            double xMin = XAxisMin;
            double yMax = YAxisMax;
            double yMin = YAxisMin;

            // Get units dependent scaling factor
            double factor;
            switch ( Units )
            {
                case Units.Metres: factor = 0.0254; break;
                case Units.Millimetres: factor = 25.4; break;
                case Units.Feet: factor = 1.0 / 12.0; break;
                default: factor = 1.0; break;
            }

            // Convert maximum extents to pixels
            double xMaxPix = (xMax / Scale) * (dpiX / factor) + OriginOffsetX;
            double xMinPix = (xMin / Scale) * (dpiX / factor) + OriginOffsetX;
            double yMaxPix = ActualHeight - ((yMax / Scale) * (dpiY / factor) + OriginOffsetY);
            double yMinPix = ActualHeight - ((yMin / Scale) * (dpiY / factor) + OriginOffsetY);

            // Check boundary drawing object for points outside
            // specified axis extents
            bool foundExtents = false;
            foreach ( Point p in boundary.Boundary.Points )
            {
                if ( p.X > xMaxPix ) { xMaxPix = p.X; foundExtents = true; }
                else if ( p.X < xMinPix ) { xMinPix = p.X; foundExtents = true; }

                if ( p.Y < yMaxPix ) { yMaxPix = p.Y; foundExtents = true; }
                else if ( p.Y > yMinPix ) { yMinPix = p.Y; foundExtents = true; }
            }
            foreach ( MaterialBlock mb in materialBlocks )
            {
                foreach ( Point p in mb.Boundary.Points )
                {
                    if ( p.X > xMaxPix ) { xMaxPix = p.X; foundExtents = true; }
                    else if ( p.X < xMinPix ) { xMinPix = p.X; foundExtents = true; }

                    if ( p.Y < yMaxPix ) { yMaxPix = p.Y; foundExtents = true; }
                    else if ( p.Y > yMinPix ) { yMinPix = p.Y; foundExtents = true; }
                }
            }

            // Convert extents back to pixels if they are outside specified extents
            if ( foundExtents )
            {
                xMax = (xMaxPix - OriginOffsetX) * (factor / dpiX) * Scale;
                xMin = (xMinPix - OriginOffsetX) * (factor / dpiX) * Scale;
                yMax = (ActualHeight - yMaxPix - OriginOffsetY) * (factor / dpiY) * Scale;
                yMin = (ActualHeight - yMinPix - OriginOffsetY) * (factor / dpiY) * Scale;
            }

            // Compute centre of desired window and centre of current window
            Point fitCentre = new Point( 0.5 * (xMaxPix + xMinPix) , 0.5 * (yMaxPix + yMinPix) );
            Point canvCentre = new Point( 0.5 * ActualWidth , 0.5 * ActualHeight );

            // Shift points to desired centre
            Translate( canvCentre - fitCentre );

            if ( zoom )
            {
                // Compute desired scale (fit all content with minimum of 100 pixels of padding)
                double canvasWidth = (ActualWidth - 100) / dpiX * factor;
                double canvasHeight = (ActualHeight - 100) / dpiY * factor;
                double reqScale = Math.Max( (xMax - xMin) / canvasWidth , (yMax - yMin) / canvasHeight );

                // Zoom by factor to achieve required scale with centre of window as focus point
                Zoom( Scale / reqScale , new Point( 0.5 * ActualWidth , 0.5 * ActualHeight ) );
            }
        }


        /// <summary>
        /// Zooms the window to fit the rectangle specified by zoomBounds
        /// </summary>
        public void ZoomArea ( Polygon zoomBounds )
        {
            // Compute centre of zoom region and centre of canvas
            Point fitCentre =
                new Point( 0.5 * (zoomBounds.Points[0].X + zoomBounds.Points[2].X) ,
                            0.5 * (zoomBounds.Points[0].Y + zoomBounds.Points[2].Y) );

            Point canvCentre = new Point( 0.5 * ActualWidth , 0.5 * ActualHeight );

            // Shift points so centres are coincident
            Translate( canvCentre - fitCentre );

            // Get units dependent scaling factor
            double factor;
            switch ( Units )
            {
                case Units.Metres: factor = 0.0254; break;
                case Units.Millimetres: factor = 25.4; break;
                case Units.Feet: factor = 1.0 / 12.0; break;
                default: factor = 1.0; break;
            }

            // Compute zoom region dimensions (in actual units)
            double realWidth = Math.Abs( zoomBounds.Points[0].X - zoomBounds.Points[2].X ) * (factor / dpiX) * Scale;
            double realHeight = Math.Abs( zoomBounds.Points[0].Y - zoomBounds.Points[2].Y ) * (factor / dpiY) * Scale;

            // Compute canvas size (in screen units)
            double canvasWidth = ActualWidth / dpiX * factor;
            double canvasHeight = ActualHeight / dpiY * factor;

            // Compute required scale (actual units/screen units)
            double reqScale = Math.Max( realWidth / canvasWidth , realHeight / canvasHeight );

            // Zoom by appropriate factor centred on canvas centre
            Zoom( Scale / reqScale , new Point( 0.5 * ActualWidth , 0.5 * ActualHeight ) );
        }


        /// <summary>
        /// Delete existing geometric boundary (and contained material blocks)
        /// </summary>
        public void RemoveBoundary ()
        {
            boundary.Delete();
            boundary.IsSelected = false;
            this.IsSaved = false;
            this.IsVerified = false;
        }

        public void DeleteSelectedItems ()
        {

            if ( boundary.IsSelected )
            {
                MessageBoxResult confirmed;
                confirmed = MessageBox.Show( "Are you sure you want to delete analysis boundaries and all material blocks?" ,
                                            "Delete Analysis Boundaries" , MessageBoxButton.YesNo );

                if ( confirmed == MessageBoxResult.Yes )
                {
                    RemoveBoundary();
                }

                return;
            }

            List<MaterialBlock> deletedBlocks = materialBlocks.FindAll( delegate( MaterialBlock mb ) { return mb.IsSelected; } );
            if ( deletedBlocks.Count > 0 )
            {
                string countMsg = deletedBlocks.Count == 1 ? " this material block" : " these " + deletedBlocks.Count + " material blocks";

                MessageBoxResult confirmed;
                confirmed = MessageBox.Show( String.Format( "Are you sure you want to delete{0}?" , countMsg ) ,
                                            "Delete Material Blocks" , MessageBoxButton.YesNo );

                if ( confirmed == MessageBoxResult.Yes )
                {
                    for ( int i = 0 ; i < deletedBlocks.Count ; i++ )
                    {
                        deletedBlocks[i].Delete();
                    }

                    this.IsSaved = false;
                    this.IsVerified = false;
                }

                return;
            }

            List<DrawingPoint> deletedBoundPoints = boundary.BoundaryPoints.FindAll( delegate( DrawingPoint bp ) { return bp.IsSelected; } );
            if ( deletedBoundPoints.Count > 0 )
            {
                string countMsg = deletedBoundPoints.Count == 1 ? " this boundary point" : " these " + deletedBoundPoints.Count + " boundary points";

                MessageBoxResult confirmed;
                confirmed = MessageBox.Show( String.Format( "Are you sure you want to delete{0}?" , countMsg ) ,
                                            "Delete Boundary Points" , MessageBoxButton.YesNo );

                if ( confirmed == MessageBoxResult.Yes )
                {
                    for ( int i = 0 ; i < deletedBoundPoints.Count ; i++ )
                    {
                        deletedBoundPoints[i].Delete();
                    }

                    this.IsSaved = false;
                    this.IsVerified = false;
                }

                return;
            }

            materialBlocks.ForEach(
                delegate( MaterialBlock mb )
                {
                    deletedBoundPoints.AddRange( mb.BoundaryPoints.FindAll(
                        delegate( DrawingPoint p ) { return !deletedBoundPoints.Contains( p ) && p.IsSelected; } ) );
                } );
            //for ( int i = 0 ; i < materialBlocks.Count ; i++ )
            //{
            //    deletedBoundPoints.InsertRange( 0 , materialBlocks[i].BoundaryPoints.FindAll( delegate( DrawingPoint bp ) { return bp.IsSelected; } ) );
            //}
            if ( deletedBoundPoints.Count > 0 )
            {
                string countMsg = deletedBoundPoints.Count == 1 ? " this material boundary point" : " these " + deletedBoundPoints.Count + " material boundary points";

                MessageBoxResult confirmed;
                confirmed = MessageBox.Show( String.Format( "Are you sure you want to delete{0}?" , countMsg ) ,
                                            "Delete Material Boundary Points" , MessageBoxButton.YesNo );

                if ( confirmed == MessageBoxResult.Yes )
                {
                    for ( int i = 0 ; i < deletedBoundPoints.Count ; i++ )
                    {
                        deletedBoundPoints[i].Delete();
                    }

                    this.IsSaved = false;
                    this.IsVerified = false;
                }

                return;
            }
        }


        /// <summary>
        /// Cancels current material block drawing operation
        /// </summary>
        public void CancelDrawing ()
        {
            drawing = false;
            panning = false;
            zooming = false;
            moving = false;
            caughtCtrl = false;

            drawLine.Points.Clear();
            this.Children.Remove( drawLine );
            this.Children.Add( drawLine );
        }

        public void ClearMaterialSelections ()
        {
            materialBlocks.ForEach( delegate( MaterialBlock mb ) { mb.IsSelected = false; } );
        }

        public void ClearBoundaryPointSelections ()
        {
            boundary.BoundaryPoints.ForEach( delegate( DrawingPoint p ) { p.IsSelected = false; } );
        }

        public void ClearMaterialBoundaryPointSelections ()
        {
            materialBlocks.ForEach( delegate( MaterialBlock mb )
            { mb.BoundaryPoints.ForEach( delegate( DrawingPoint p ) { p.IsSelected = false; } ); } );
        }


        /// <summary>
        /// Unhighlight any selected items
        /// </summary>
        public void ClearSelections ()
        {
            boundary.IsSelected = false;

            ClearMaterialSelections();
            ClearBoundaryPointSelections();
            ClearMaterialBoundaryPointSelections();
        }


        /// <summary>
        /// Respond to change of plotting units
        /// </summary>
        /// <param name="convert">
        /// MessageBoxResult.Yes - convert numerical unit values
        /// MessageBoxResult.No - leave numerical values, adjust scale
        /// </param>
        public void UpdateUnits ( MenuItem sender , MessageBoxResult convert )
        {
            // Set conversion factor (current units / in)
            double distConv , pressConv , weightConv , lineLoadConv , pointLoadConv;
            switch ( Units )
            {
                case Units.Metres:
                    distConv = 0.0254;
                    pressConv = 0.145037738;            // psi / kPa
                    weightConv = 6.36588034748143;      // (lbf / ft^3) / (kN / m^3)
                    pointLoadConv = 224.808943;         // lbf / kN
                    lineLoadConv = 68.5217657222469;    // (lbf / ft) / (kN / m)
                    break;
                case Units.Millimetres:
                    distConv = 25.4;
                    pressConv = 0.145037738;
                    weightConv = 6.36588034748143;
                    pointLoadConv = 224.808943;
                    lineLoadConv = 68.5217657222469;
                    break;
                case Units.Feet:
                    distConv = 1.0 / 12.0;
                    pressConv = 1.0;
                    weightConv = 1.0;
                    pointLoadConv = 1.0;
                    lineLoadConv = 1.0;
                    break;
                default:
                    distConv = 1.0;
                    pressConv = 1.0;
                    weightConv = 1.0;
                    pointLoadConv = 1.0;
                    lineLoadConv = 1.0;
                    break;
            }

            // Set new units and update conversion factor
            // convFact -> [(current units / in) / (new units / in)]
            string unitsLabel;
            switch ( sender.Name )
            {
                case "unitsM":
                    Units = Units.Metres;
                    unitsLabel = "m";
                    distConv /= 0.0254;
                    pressConv /= 0.145037738;
                    weightConv /= 6.36588034748143;
                    pointLoadConv /= 224.808943;
                    lineLoadConv /= 68.5217657222469;
                    break;

                case "unitsMM":
                    Units = Units.Millimetres;
                    unitsLabel = "mm";
                    distConv /= 25.4;
                    pressConv /= 0.145037738;
                    weightConv /= 6.36588034748143;
                    pointLoadConv /= 224.808943;
                    lineLoadConv /= 68.5217657222469;
                    break;

                case "unitsFT":
                    Units = Units.Feet;
                    unitsLabel = "ft";
                    distConv *= 12.0;
                    break;

                default:
                    Units = Units.Inches;
                    unitsLabel = "in";
                    break;
            }

            // Update axis units labels
            ((TextBlock) xAxis.Children[1]).Text = String.Format( "X ({0})" , unitsLabel );
            ((TextBlock) yAxis.Children[1]).Text = String.Format( "Y\n({0})" , unitsLabel );

            // If conversion is desired, convert appropriate values
            if ( convert == MessageBoxResult.Yes )
            {
                XAxisMax /= distConv;
                XAxisMin /= distConv;
                YAxisMax /= distConv;
                YAxisMin /= distConv;
                XMajorDivision /= distConv;
                YMajorDivision /= distConv;

                materialTypes.ForEach(
                    delegate( MaterialType mt )
                    {
                        mt.Cohesion *= pressConv;
                        mt.Gamma *= weightConv;
                        mt.Emod *= pressConv;
                    } );

                genAlgParams.SliceWidth /= distConv;
                feaParams.ColWidth /= distConv;
                feaParams.RowHeight /= distConv;

                FEATriElements.ForEach(
                    delegate( fe3NodedTriElement e )
                    {
                        e.Nodes.ForEach(
                            delegate( feNode node ) { node.X /= distConv; node.Y /= distConv; } );
                    } );
                FEAQuadElements.ForEach(
                    delegate( fe4NodedQuadElement e )
                    {
                        e.Nodes.ForEach(
                            delegate( feNode node ) { node.X /= distConv; node.Y /= distConv; } );
                    } );

                foreach ( MaterialBlock mb in MaterialBlocks )
                {
                    foreach ( LineLoad ll in mb.LineLoads )
                    {
                        ll.ApplyLoad( ll.IsLoadedN ,
                            ll.NLoad1 * lineLoadConv , ll.NLoad2 * lineLoadConv ,
                            ll.IsLoadedT ,
                            ll.TLoad1 * lineLoadConv , ll.TLoad2 * lineLoadConv );
                    }

                    foreach ( PointLoad pl in mb.PointLoads )
                    {
                        pl.ApplyLoad( pl.IsLoadedX , pl.XLoad * pointLoadConv ,
                            pl.IsLoadedY , pl.YLoad * pointLoadConv );
                    }
                }


                // Rebuild plotting axes and grid
                BuildAxes();
            }
            // If conversion is not desired, modify the scale
            else
            {
                Scale *= distConv;

                StatusBar status =
                (StatusBar) ((DockPanel) ((Grid) ((TabControl) ((TabItem) ((Grid) this.Parent).Parent).Parent).Parent).Children[1]).Children[0];
                StatusBarItem statusLabel = (StatusBarItem) status.Items[2];

                statusLabel.Content = String.Format( "Scale = {0}:1" , Math.Round( Scale , 2 ) );
            }

            this.IsSaved = false;
            this.IsVerified = false;
        }

        /// <summary>
        /// Loads the existing 3 noded tri mesh
        /// </summary>
        public void LoadFEA3NodedTriMesh ()
        {
            // load file path
            string[] path = FilePath.Split( '.' );

            // get node file name
            path[path.Length - 1] = "nod";
            string nodeFile = string.Join( "." , path );

            // get element file name
            path[path.Length - 1] = "ele";
            string elementFile = string.Join( "." , path );

            // ensure that these files exist
            if ( !File.Exists( nodeFile ) || !File.Exists( elementFile ) )
            {
                if ( !File.Exists( nodeFile ) ) MessageBox.Show( "Error: .nod file not found" );
                if ( !File.Exists( elementFile ) ) MessageBox.Show( "Error: .ele file not found" );
                return;
            }

            List<feNode> nodes = new List<feNode>();
            string[] split;
            using ( TextReader tr = new StreamReader( nodeFile ) )
            {
                int numNodes = int.Parse( tr.ReadLine().Split( '\t' )[0] );
                feNode newNode;
                for ( int i = 0 ; i < numNodes ; i++ )
                {
                    split = tr.ReadLine().Split( '\t' );
                    newNode = new feNode( int.Parse( split[0] ) , false ,
                        double.Parse( split[1] ) , double.Parse( split[2] ) );
                    newNode.XLoad = double.Parse( split[3] );
                    newNode.YLoad = double.Parse( split[4] );
                    nodes.Add( newNode );
                }
            }

            List<fe3NodedTriElement> elements = new List<fe3NodedTriElement>();
            using ( TextReader tr = new StreamReader( elementFile ) )
            {
                int numElements = int.Parse( tr.ReadLine().Split( '\t' )[0] );
                for ( int i = 0 ; i < numElements ; i++ )
                {
                    split = tr.ReadLine().Split( '\t' );
                    elements.Add( new fe3NodedTriElement( int.Parse( split[0] ) ,
                        nodes[int.Parse( split[1] ) - 1] ,
                        nodes[int.Parse( split[2] ) - 1] ,
                        nodes[int.Parse( split[3] ) - 1] ,
                        MaterialTypes[int.Parse( split[4] ) - 1] , false ) );
                }
            }

            FEATriElements = elements;

            // get canvas dimensions/properties
            double originX = OriginOffsetX ,
                   originY = OriginOffsetY ,
                   dpiX = DpiX ,
                   dpiY = DpiY ,
                   scale = Scale ,
                   yHeight = ActualHeight;
            Units units = Units;

            // get units dependent scaling factor
            double factor;
            switch ( units )
            {
                case Units.Metres: factor = 0.0254; break;
                case Units.Millimetres: factor = 25.4; break;
                case Units.Feet: factor = 1.0 / 12.0; break;
                default: factor = 1.0; break;
            }

            Polygon newPolygon;
            double x , y;
            for ( int i = 0 ; i < elements.Count ; i++ )
            {
                newPolygon = new Polygon();
                newPolygon.StrokeThickness = 0.6;
                newPolygon.Stroke = Brushes.Black;
                newPolygon.Opacity = /*1.0*/ 0.8;
                newPolygon.Fill = /*Brushes.White*/ elements[i].Material.Fill;

                foreach ( feNode node in elements[i].Nodes )
                {
                    x = node.X / (scale * factor) * dpiX + originX;
                    y = yHeight - (node.Y / (scale * factor) * dpiY + originY);
                    newPolygon.Points.Add( new Point( x , y ) );
                }

                elements[i].Boundary = newPolygon;
                this.Children.Add( newPolygon );
            }

            ShowMesh = true;
        }


        /// <summary>
        /// Loads the existing 4 noded quad mesh
        /// </summary>
        public void LoadFEA4NodedQuadMesh ()
        {
            // load file path
            string[] path = FilePath.Split( '.' );

            // get node file name
            path[path.Length - 1] = "nod";
            string nodeFile = string.Join( "." , path );

            // get element file name
            path[path.Length - 1] = "ele";
            string elementFile = string.Join( "." , path );

            // ensure that these files exist
            if ( !File.Exists( nodeFile ) || !File.Exists( elementFile ) )
            {
                if ( !File.Exists( nodeFile ) ) MessageBox.Show( "Error: .nod file not found" );
                if ( !File.Exists( elementFile ) ) MessageBox.Show( "Error: .ele file not found" );
                return;
            }

            List<feNode> nodes = new List<feNode>();
            string[] split;
            using ( TextReader tr = new StreamReader( nodeFile ) )
            {
                int numNodes = int.Parse( tr.ReadLine().Split( '\t' )[0] );
                feNode newNode;
                for ( int i = 0 ; i < numNodes ; i++ )
                {
                    split = tr.ReadLine().Split( '\t' );
                    newNode = new feNode( int.Parse( split[0] ) , false ,
                        double.Parse( split[1] ) , double.Parse( split[2] ) );
                    newNode.XLoad = double.Parse( split[3] );
                    newNode.YLoad = double.Parse( split[4] );
                    nodes.Add( newNode );
                }
            }

            List<fe4NodedQuadElement> elements = new List<fe4NodedQuadElement>();
            using ( TextReader tr = new StreamReader( elementFile ) )
            {
                int numElements = int.Parse( tr.ReadLine().Split( '\t' )[0] );
                for ( int i = 0 ; i < numElements ; i++ )
                {
                    split = tr.ReadLine().Split( '\t' );
                    elements.Add( new fe4NodedQuadElement( int.Parse( split[0] ) ,
                        nodes[int.Parse( split[1] ) - 1] ,
                        nodes[int.Parse( split[2] ) - 1] ,
                        nodes[int.Parse( split[3] ) - 1] ,
                        nodes[int.Parse( split[4] ) - 1] ,
                        MaterialTypes[int.Parse( split[5] ) - 1] , false ) );
                }
            }

            FEAQuadElements = elements;

            // get canvas dimensions/properties
            double originX = OriginOffsetX ,
                   originY = OriginOffsetY ,
                   dpiX = DpiX ,
                   dpiY = DpiY ,
                   scale = Scale ,
                   yHeight = ActualHeight;
            Units units = Units;

            // get units dependent scaling factor
            double factor;
            switch ( units )
            {
                case Units.Metres: factor = 0.0254; break;
                case Units.Millimetres: factor = 25.4; break;
                case Units.Feet: factor = 1.0 / 12.0; break;
                default: factor = 1.0; break;
            }

            Polygon newPolygon;
            double x , y;
            for ( int i = 0 ; i < elements.Count ; i++ )
            {
                newPolygon = new Polygon();
                newPolygon.StrokeThickness = 0.6;
                newPolygon.Stroke = Brushes.Black;
                newPolygon.Opacity = /*1.0*/ 0.8;
                newPolygon.Fill = /*Brushes.White*/ elements[i].Material.Fill;

                foreach ( feNode node in elements[i].Nodes )
                {
                    x = node.X / (scale * factor) * dpiX + originX;
                    y = yHeight - (node.Y / (scale * factor) * dpiY + originY);
                    newPolygon.Points.Add( new Point( x , y ) );
                }

                elements[i].Boundary = newPolygon;
                this.Children.Add( newPolygon );
            }

            ShowMesh = true;
        }



        // ----------------------------------
        // OVERRIDES
        // ----------------------------------

        protected override void OnMouseLeftButtonUp ( MouseButtonEventArgs e )
        {
            base.OnMouseLeftButtonUp( e );

            Point p = Mouse.GetPosition( this );

            switch ( DrawMode )
            {
                case (DrawModes.Boundaries):
                    {
                        // Initiate drawing
                        if ( !drawing ) drawing = true;

                        // Snap to nearest grid point
                        if ( Keyboard.IsKeyDown( Key.LeftCtrl ) || Keyboard.IsKeyDown( Key.RightCtrl ) )
                        {
                            if ( ShowMinorGrid || ShowMajorGrid )
                            {
                                // Convert mouse position to inches with origin @ BL
                                double xCoord = (p.X - OriginOffsetX) / dpiX;
                                double yCoord = ((ActualHeight - p.Y) - OriginOffsetY) / dpiY;

                                // Get units dependent scaling factor
                                double factor;
                                switch ( this.Units )
                                {
                                    case Units.Metres: factor = 0.0254; break;
                                    case Units.Millimetres: factor = 25.4; break;
                                    case Units.Feet: factor = 1.0 / 12.0; break;
                                    default: factor = 1.0; break;
                                }

                                // Convert mouse position to actual units
                                xCoord *= factor * Scale;
                                yCoord *= factor * Scale;

                                // Get grid spacing (major or minor)
                                double xDiv = XMajorDivision;
                                double yDiv = YMajorDivision;
                                if ( ShowMinorGrid )
                                {
                                    xDiv /= XMinorDivisions;
                                    yDiv /= YMinorDivisions;
                                }

                                // Snap x coordinate
                                double xRatio = xCoord / xDiv;
                                double xDiff = xRatio - Math.Truncate( xRatio );
                                xCoord += Math.Abs( xDiff ) < 0.5 ? -xDiff * xDiv : Math.Sign( xDiff ) * (1 - Math.Abs( xDiff )) * xDiv;
                                p.X = xCoord / (factor * Scale) * dpiX + OriginOffsetX;

                                // Snap y coordinate
                                double yRatio = yCoord / yDiv;
                                double yDiff = yRatio - Math.Truncate( yRatio );
                                yCoord += Math.Abs( yDiff ) < 0.5 ? -yDiff * yDiv : Math.Sign( yDiff ) * (1 - Math.Abs( yDiff )) * yDiv;
                                p.Y = ActualHeight - (yCoord / (factor * Scale) * dpiY + OriginOffsetY);
                            }
                        }

                        // Draw straight lines
                        if ( Keyboard.IsKeyDown( Key.LeftShift ) || Keyboard.IsKeyDown( Key.RightShift ) )
                        {
                            if ( drawLine.Points.Count >= 2 )
                            {
                                Point lastPt = drawLine.Points[drawLine.Points.Count - 2];

                                if ( Math.Abs( lastPt.X - p.X ) < Math.Abs( lastPt.Y - p.Y ) )
                                    p.X = lastPt.X;
                                else
                                    p.Y = lastPt.Y;
                            }
                        }

                        // If it is the first point in the drawing sequence, add it to the boundary.
                        // Otherwise, fix the last point
                        if ( drawLine.Points.Count == 0 ) drawLine.Points.Add( p );
                        else drawLine.Points[drawLine.Points.Count - 1] = p;

                        // Add another copy of the current point (for "rubber banding")
                        drawLine.Points.Add( p );

                        if ( this.IsSaved ) this.IsSaved = false;
                        if ( this.IsVerified ) this.IsVerified = false;
                    }
                    break;

                case (DrawModes.Materials):
                    {
                        // Initiate drawing
                        if ( !drawing ) drawing = true;

                        // Snap to nearest grid point
                        if ( Keyboard.IsKeyDown( Key.LeftCtrl ) || Keyboard.IsKeyDown( Key.RightCtrl ) )
                        {
                            if ( ShowMinorGrid || ShowMajorGrid )
                            {
                                // Convert mouse position to inches with origin @ BL
                                double xCoord = (p.X - OriginOffsetX) / dpiX;
                                double yCoord = ((ActualHeight - p.Y) - OriginOffsetY) / dpiY;

                                // Get units dependent scaling factor
                                double factor;
                                switch ( this.Units )
                                {
                                    case Units.Metres: factor = 0.0254; break;
                                    case Units.Millimetres: factor = 25.4; break;
                                    case Units.Feet: factor = 1.0 / 12.0; break;
                                    default: factor = 1.0; break;
                                }

                                // Convert mouse position to actual units
                                xCoord *= factor * Scale;
                                yCoord *= factor * Scale;

                                // Get grid spacing (major or minor)
                                double xDiv = XMajorDivision;
                                double yDiv = YMajorDivision;
                                if ( ShowMinorGrid )
                                {
                                    xDiv /= XMinorDivisions;
                                    yDiv /= YMinorDivisions;
                                }

                                // Snap x coordinate
                                double xRatio = xCoord / xDiv;
                                double xDiff = xRatio - Math.Truncate( xRatio );
                                xCoord += Math.Abs( xDiff ) < 0.5 ? -xDiff * xDiv : Math.Sign( xDiff ) * (1 - Math.Abs( xDiff )) * xDiv;
                                p.X = xCoord / (factor * Scale) * dpiX + OriginOffsetX;

                                // Snap y coordinate
                                double yRatio = yCoord / yDiv;
                                double yDiff = yRatio - Math.Truncate( yRatio );
                                yCoord += Math.Abs( yDiff ) < 0.5 ? -yDiff * yDiv : Math.Sign( yDiff ) * (1 - Math.Abs( yDiff )) * yDiv;
                                p.Y = ActualHeight - (yCoord / (factor * Scale) * dpiY + OriginOffsetY);
                            }
                        }

                        // Draw straight lines
                        if ( Keyboard.IsKeyDown( Key.LeftShift ) || Keyboard.IsKeyDown( Key.RightShift ) )
                        {
                            if ( drawLine.Points.Count >= 2 )
                            {
                                Point lastPt = drawLine.Points[drawLine.Points.Count - 2];

                                if ( Math.Abs( lastPt.X - p.X ) < Math.Abs( lastPt.Y - p.Y ) )
                                    p.X = lastPt.X;
                                else
                                    p.Y = lastPt.Y;
                            }
                        }

                        // If it is the first point in the drawing sequence, add it to the boundary.
                        // Otherwise, fix the last point
                        if ( drawLine.Points.Count == 0 ) drawLine.Points.Add( p );
                        else drawLine.Points[drawLine.Points.Count - 1] = p;

                        // Add another copy of the current point (for "rubber banding")
                        drawLine.Points.Add( p );

                        if ( this.IsSaved ) this.IsSaved = false;
                        if ( this.IsVerified ) this.IsVerified = false;
                    }
                    break;


                case (DrawModes.Pan):
                    {
                        // End panning operations
                        panning = false;
                        this.Cursor = ((TextBlock) (((MainWindow) ((Grid) ((TabControl) ((TabItem) ((Grid) this.Parent).Parent).Parent).Parent).Parent).Resources["handCursor"])).Cursor;
                    }
                    break;


                case (DrawModes.ZoomArea):
                    {
                        // End zooming operations
                        zooming = false;

                        // Zoom to highlighted region
                        if ( zoomRect.Boundary.Points.Count == 4 )
                        {
                            ZoomArea( zoomRect.Boundary );
                        }

                        // Clean up resources
                        zoomRect.Boundary.Points.Clear();
                        Mouse.Capture( this , CaptureMode.None );
                    }
                    break;


                case (DrawModes.AddPoints):
                    {
                        DrawingPoint addPoint = boundary.BoundaryPoints.Find( delegate( DrawingPoint bp ) { return bp.IsMouseOver; } );

                        if ( addPoint == null )
                        {
                            for ( int i = 0 ; i < materialBlocks.Count ; i++ )
                            {
                                addPoint = materialBlocks[i].BoundaryPoints.Find( delegate( DrawingPoint bp ) { return bp.IsMouseOver; } );
                                if ( addPoint != null ) break;
                            }
                        }

                        if ( addPoint != null ) addPoints.Add( addPoint );
                        else
                        {
                            addPoints.Clear();
                            ClearSelections();
                        }

                        if ( addPoints.Count == 2 )
                        {
                            bool added = false;
                            DrawingPoint newPoint = null;
                            if ( addPoints[0].ParentBoundary != null && addPoints[1].ParentBoundary != null )
                            {
                                boundary.AddPoint( addPoints[0] , addPoints[1] );
                                added = true;
                            }
                            else if ( addPoints[0].ParentBlocks != null && addPoints[1].ParentBlocks != null )
                            {
                                addPoints[0].ParentBlocks.ForEach(
                                    delegate( MaterialBlock mb )
                                    {
                                        if ( addPoints[1].ParentBlocks.Contains( mb ) )
                                        {
                                            newPoint = mb.AddPoint( addPoints[0] , addPoints[1] , newPoint );
                                            added = true;
                                        }
                                    } );
                            }

                            if ( !added )
                            {
                                MessageBox.Show( "Points must be on the same block." , "Error" );
                            }

                            addPoints.Clear();
                            ClearSelections();
                        }
                    }
                    break;


                case (DrawModes.Fixities):
                    {
                        DrawingPoint fixPoint = null;
                        for ( int i = 0 ; i < materialBlocks.Count ; i++ )
                        {
                            fixPoint = materialBlocks[i].BoundaryPoints.Find( delegate( DrawingPoint bp ) { return bp.IsMouseOver; } );
                            if ( fixPoint != null ) break;
                        }

                        if ( fixPoint != null ) fixPoints.Add( fixPoint );
                        else
                        {
                            fixPoints.Clear();
                            ClearSelections();
                        }

                        if ( fixPoints.Count == 2 )
                        {
                            bool added = false;
                            if ( fixPoints[0].ParentBlocks != null && fixPoints[1].ParentBlocks != null )
                            {
                                foreach ( MaterialBlock mb in fixPoints[0].ParentBlocks )
                                {
                                    if ( fixPoints[1].ParentBlocks.Contains( mb ) )
                                    {
                                        added = mb.ApplyFixity( fixPoints[0] , fixPoints[1] );
                                        if ( added ) break;
                                    }
                                }
                            }

                            if ( !added ) MessageBox.Show( "Points must be on the same material block." , "Error" );

                            fixPoints.Clear();
                            ClearSelections();
                        }
                    }
                    break;


                case (DrawModes.PointLoad):
                    {
                        DrawingPoint loadPoint = null;
                        for ( int i = 0 ; i < materialBlocks.Count ; i++ )
                        {
                            loadPoint = materialBlocks[i].BoundaryPoints.Find( delegate( DrawingPoint bp ) { return bp.IsMouseOver; } );
                            if ( loadPoint != null ) break;
                        }

                        if ( loadPoint != null && loadPoint.ParentBlocks != null )
                        {
                            MaterialBlock mb = loadPoint.ParentBlocks[0];

                            if ( mb != null ) mb.ApplyPointLoad( loadPoint );

                            ClearSelections();
                        }
                    }
                    break;


                case (DrawModes.LineLoad):
                    {
                        DrawingPoint loadPoint = null;
                        for ( int i = 0 ; i < materialBlocks.Count ; i++ )
                        {
                            loadPoint = materialBlocks[i].BoundaryPoints.Find( delegate( DrawingPoint bp ) { return bp.IsMouseOver; } );
                            if ( loadPoint != null ) break;
                        }

                        if ( loadPoint != null ) loadPoints.Add( loadPoint );
                        else
                        {
                            loadPoints.Clear();
                            ClearSelections();
                        }

                        if ( loadPoints.Count == 2 )
                        {
                            bool added = false;
                            if ( loadPoints[0].ParentBlocks != null && loadPoints[1].ParentBlocks != null )
                            {
                                foreach ( MaterialBlock mb in loadPoints[0].ParentBlocks )
                                {
                                    if ( loadPoints[1].ParentBlocks.Contains( mb ) )
                                    {
                                        mb.ApplyLineLoad( loadPoints[0] , loadPoints[1] );
                                        added = true;
                                        break;
                                    }
                                }
                            }

                            if ( !added ) MessageBox.Show( "Points must be on the same block." , "Error" );

                            MaterialBlocks.ForEach(
                                delegate( MaterialBlock mb )
                                {
                                    for ( int i = mb.LineLoads.Count - 1 ; i >= 0 ; i-- )
                                    {
                                        if ( !mb.LineLoads[i].IsLoadedN && !mb.LineLoads[i].IsLoadedT )
                                        {
                                            mb.LineLoads[i].Delete();
                                            mb.LineLoads.RemoveAt( i );
                                        }
                                    }
                                } );

                            loadPoints.Clear();
                            ClearSelections();
                        }
                    }
                    break;


                case (DrawModes.MovePoints):
                    {
                        if ( moving )
                        {
                            if ( movingBoundPoint.ParentBlocks != null )
                            {
                                bool merged = false;
                                foreach ( MaterialBlock mb in materialBlocks )
                                {
                                    foreach ( DrawingPoint dp in mb.BoundaryPoints )
                                    {
                                        if ( dp != movingBoundPoint && (dp.Point - movingBoundPoint.Point).Length < dp.Dot.Width / 2 )
                                        {
                                            dp.Merge( movingBoundPoint );
                                            merged = true;
                                            break;
                                        }
                                    }
                                    if ( merged ) break;
                                }
                            }

                            moving = false;
                            caughtCtrl = false;
                            movingBoundPoint = null;
                            ClearSelections();
                        }
                    }
                    break;

                case (DrawModes.PrintPoint):
                    {
                        ClearSelections();

                        for ( int i = 0 ; i < materialBlocks.Count ; i++ )
                        {
                            DrawingPoint pp = materialBlocks[i].BoundaryPoints.Find( delegate( DrawingPoint bp ) { return bp.IsMouseOver; } );

                            if ( pp != null )
                            {
                                materialBlocks.ForEach(
                                    delegate( MaterialBlock mb )
                                    {
                                        mb.BoundaryPoints.ForEach( delegate( DrawingPoint bp ) { bp.IsPrintPoint = false; } );
                                    } );

                                pp.IsPrintPoint = true;

                                if ( this.IsSaved ) this.IsSaved = false;
                                if ( this.IsVerified ) this.IsVerified = false;

                                break;
                            }
                        }
                    }
                    break;


                case (DrawModes.Select):
                    {
                        // De-select objects if mouse is over blank canvas area
                        if ( !boundary.IsMouseOver )
                        {
                            if ( Keyboard.IsKeyDown( Key.LeftShift ) || Keyboard.IsKeyDown( Key.RightShift ) )
                            {
                                boundary.IsSelected = false;

                                if ( materialBlocks.Find( delegate( MaterialBlock mb ) { return mb.IsMouseOver; } ) == null )
                                {
                                    ClearMaterialSelections();
                                }

                                if ( boundary.BoundaryPoints.Find( delegate( DrawingPoint bp ) { return bp.IsMouseOver; } ) == null )
                                {
                                    ClearBoundaryPointSelections();
                                }

                                bool foundMaterialBoundPoint = false;
                                for ( int i = 0 ; i < materialBlocks.Count ; i++ )
                                {
                                    if ( materialBlocks[i].BoundaryPoints.Find( delegate( DrawingPoint bp ) { return bp.IsMouseOver; } ) != null )
                                    {
                                        foundMaterialBoundPoint = true;
                                        break;
                                    }
                                }
                                if ( !foundMaterialBoundPoint ) ClearMaterialBoundaryPointSelections();
                            }
                            else
                            {
                                ClearSelections();
                            }

                            for ( int i = 0 ; i < materialBlocks.Count ; i++ )
                            {
                                if ( materialBlocks[i].IsMouseOver )
                                {
                                    materialBlocks[i].IsSelected = true;
                                    break;
                                }
                            }

                            for ( int i = 0 ; i < boundary.BoundaryPoints.Count ; i++ )
                            {
                                if ( boundary.BoundaryPoints[i].IsMouseOver )
                                {
                                    boundary.BoundaryPoints[i].IsSelected = true;
                                    break;
                                }
                            }

                            for ( int i = 0 ; i < materialBlocks.Count ; i++ )
                            {
                                if ( materialBlocks[i].BoundaryPoints.Find(
                                    delegate( DrawingPoint bp )
                                    {
                                        if ( bp.IsMouseOver )
                                        {
                                            bp.IsSelected = true;
                                            return true;
                                        }
                                        return false;
                                    } ) != null )
                                {
                                    break;
                                }
                            }
                        }
                        else
                        {
                            ClearMaterialSelections();
                            ClearMaterialBoundaryPointSelections();
                            ClearBoundaryPointSelections();
                        }
                    }
                    break;
            }
        }

        protected override void OnMouseMove ( MouseEventArgs e )
        {
            base.OnMouseMove( e );

            Point p = Mouse.GetPosition( this );

            /*
             * Draw Boundaries / Material Blocks Mode
             */
            if ( drawing )
            {
                if ( Keyboard.IsKeyDown( Key.LeftShift ) || Keyboard.IsKeyDown( Key.RightShift ) )
                {
                    if ( drawLine.Points.Count >= 2 )
                    {
                        Point lastPt = drawLine.Points[drawLine.Points.Count - 2];

                        if ( Math.Abs( lastPt.X - p.X ) < Math.Abs( lastPt.Y - p.Y ) )
                            p.X = lastPt.X;
                        else
                            p.Y = lastPt.Y;
                    }
                }

                // Update "rubber banding" point
                drawLine.Points[drawLine.Points.Count - 1] = p;
            }


            /*
             * Pan Mode
             * (NB: This is also activated during any mode with the middle mouse button)
             */
            if ( (DrawMode == DrawModes.Pan && Mouse.LeftButton == MouseButtonState.Pressed)
                || Mouse.MiddleButton == MouseButtonState.Pressed )
            {
                // Initialize panning operations
                if ( !panning )
                {
                    panning = true;
                    this.Cursor = ((TextBlock) (((MainWindow) ((Grid) ((TabControl) ((TabItem) ((Grid) this.Parent).Parent).Parent).Parent).Parent).Resources["grabCursor"])).Cursor;

                    // Set reference point for panning translation
                    transPoint = p;
                }
                else
                {
                    // Translate canvas and axes
                    Translate( p - transPoint );

                    // Update reference point
                    transPoint = p;
                }
            }


            /*
             * Zoom Area Mode
             */
            if ( this.DrawMode == DrawModes.ZoomArea && Mouse.LeftButton == MouseButtonState.Pressed )
            {
                // Initialize zooming operation
                if ( !zooming )
                {
                    zooming = true;
                    Mouse.Capture( this , CaptureMode.SubTree );

                    // Add corner points to zoom rectangle
                    // (NB: Polygon was opted for over Rectangle since
                    //      coordinate updates require fewer operations)
                    zoomRect.Boundary.Points.Add( p );
                    zoomRect.Boundary.Points.Add( p );
                    zoomRect.Boundary.Points.Add( p );
                    zoomRect.Boundary.Points.Add( p );
                }
                else
                {
                    Point zoomRectPt;   // For unboxing and updating zoom rectangle coords

                    // Point 0 remains at (Original X, Original Y)

                    // Point 1 moves to (Current X, Original Y)
                    zoomRectPt = zoomRect.Boundary.Points[1];
                    zoomRectPt.X = p.X;
                    zoomRectPt.Y = zoomRect.Boundary.Points[0].Y;
                    zoomRect.Boundary.Points[1] = zoomRectPt;

                    // Point 2 moves to (Current X, Current Y)
                    zoomRect.Boundary.Points[2] = p;

                    // Point 3 moves to (Original X, Current Y)
                    zoomRectPt = zoomRect.Boundary.Points[3];
                    zoomRectPt.X = zoomRect.Boundary.Points[0].X;
                    zoomRectPt.Y = p.Y;
                    zoomRect.Boundary.Points[3] = zoomRectPt;
                }
            }

            // Get status bar label references for updating coordinates
            StatusBar status =
                (StatusBar) ((DockPanel) ((Grid) ((TabControl) ((TabItem) ((Grid) this.Parent).Parent).Parent).Parent).Children[1]).Children[0];
            StatusBarItem xCoordLabel = (StatusBarItem) status.Items[0];
            StatusBarItem yCoordLabel = (StatusBarItem) status.Items[1];

            // Convert mouse coords to inches with origin @ BL corner
            double xCoord = (p.X - OriginOffsetX) / dpiX;
            double yCoord = (ActualHeight - p.Y - OriginOffsetY) / dpiY;

            // Get units dependent scaling factor and label
            double factor;
            string units;
            switch ( Units )
            {
                case Units.Metres: units = "m"; factor = 0.0254; break;
                case Units.Millimetres: units = "mm"; factor = 25.4; break;
                case Units.Feet: units = "ft"; factor = 1.0 / 12.0; break;
                default: units = "in"; factor = 1; break;
            }

            // Convert coordinates to actual units
            xCoord *= factor * Scale;
            yCoord *= factor * Scale;

            // Snap to nearest grid point
            if ( Keyboard.IsKeyDown( Key.LeftCtrl ) || Keyboard.IsKeyDown( Key.RightCtrl ) )
            {
                if ( ShowMinorGrid || ShowMajorGrid )
                {
                    // Get grid spacing (major or minor)
                    double xDiv = XMajorDivision;
                    double yDiv = YMajorDivision;
                    if ( ShowMinorGrid )
                    {
                        xDiv /= XMinorDivisions;
                        yDiv /= YMinorDivisions;
                    }

                    // Snap x coordinate
                    double xRatio = xCoord / xDiv;
                    double xDiff = xRatio - Math.Truncate( xRatio );
                    xCoord += Math.Abs( xDiff ) < 0.5 ? -xDiff * xDiv : Math.Sign( xDiff ) * (1 - Math.Abs( xDiff )) * xDiv;
                    p.X = xCoord / (factor * Scale) * dpiX + OriginOffsetX;

                    // Snap y coordinate
                    double yRatio = yCoord / yDiv;
                    double yDiff = yRatio - Math.Truncate( yRatio );
                    yCoord += Math.Abs( yDiff ) < 0.5 ? -yDiff * yDiv : Math.Sign( yDiff ) * (1 - Math.Abs( yDiff )) * yDiv;
                    p.Y = ActualHeight - (yCoord / (factor * Scale) * dpiY + OriginOffsetY);
                }
            }

            if ( DrawMode == DrawModes.MovePoints && Mouse.LeftButton == MouseButtonState.Pressed )
            {
                if ( movingBoundPoint == null )
                {
                    movingBoundPoint = boundary.BoundaryPoints.Find( delegate( DrawingPoint bp ) { return bp.IsMouseOver && bp.IsSelected; } );
                }

                if ( movingBoundPoint == null )
                {
                    for ( int i = 0 ; i < materialBlocks.Count ; i++ )
                    {
                        movingBoundPoint = materialBlocks[i].BoundaryPoints.Find( delegate( DrawingPoint bp ) { return bp.IsMouseOver && bp.IsSelected; } );
                        if ( movingBoundPoint != null ) break;
                    }
                }

                if ( movingBoundPoint != null )
                {
                    if ( !moving )
                    {
                        moving = true;
                        movePoint = movingBoundPoint.Point;
                    }
                    else
                    {
                        movingBoundPoint.Move( p - movePoint );
                        movePoint = p;

                        if ( this.IsSaved ) this.IsSaved = false;
                        if ( this.IsVerified ) this.IsVerified = false;

                        if ( (Keyboard.IsKeyDown( Key.LeftCtrl ) || Keyboard.IsKeyDown( Key.RightCtrl ))
                            && (ShowMinorGrid || ShowMajorGrid) )
                        {
                            if ( !caughtCtrl )
                            {
                                p = this.PointToScreen( p );
                                NativeMethods.SetCursorPos( (int) p.X , (int) p.Y );
                                caughtCtrl = true;
                            }
                        }
                        else caughtCtrl = false;
                    }
                }
            }

            // Update status bar labels
            string label = String.Format( "X = {0} {1}" , Math.Round( xCoord , 2 ) , units );
            xCoordLabel.Content = label;
            label = String.Format( "Y = {0} {1}" , Math.Round( yCoord , 2 ) , units );
            yCoordLabel.Content = label;
        }

        protected override void OnMouseUp ( MouseButtonEventArgs e )
        {
            base.OnMouseUp( e );

            /*
             * Pan Mode (drawing mode independent middle mouse button version)
             */
            if ( e.ChangedButton == MouseButton.Middle )
            {
                // End panning operations
                panning = false;

                switch ( DrawMode )
                {
                    case DrawModes.Boundaries:
                        this.Cursor = ((TextBlock) (((MainWindow) ((Grid) ((TabControl) ((TabItem) ((Grid) this.Parent).Parent).Parent).Parent).Parent).Resources["drawCursor"])).Cursor;
                        break;
                    case DrawModes.Materials:
                        this.Cursor = ((TextBlock) (((MainWindow) ((Grid) ((TabControl) ((TabItem) ((Grid) this.Parent).Parent).Parent).Parent).Parent).Resources["drawCursor"])).Cursor;
                        break;
                    case DrawModes.Pan:
                        this.Cursor = ((TextBlock) (((MainWindow) ((Grid) ((TabControl) ((TabItem) ((Grid) this.Parent).Parent).Parent).Parent).Parent).Resources["handCursor"])).Cursor;
                        break;
                    case DrawModes.ZoomArea:
                        this.Cursor = ((TextBlock) (((MainWindow) ((Grid) ((TabControl) ((TabItem) ((Grid) this.Parent).Parent).Parent).Parent).Parent).Resources["zoomAreaCursor"])).Cursor;
                        break;
                    case DrawModes.MovePoints:
                        this.Cursor = ((TextBlock) (((MainWindow) ((Grid) ((TabControl) ((TabItem) ((Grid) this.Parent).Parent).Parent).Parent).Parent).Resources["movePointsCursor"])).Cursor;
                        break;
                    case DrawModes.AddPoints:
                        this.Cursor = ((TextBlock) (((MainWindow) ((Grid) ((TabControl) ((TabItem) ((Grid) this.Parent).Parent).Parent).Parent).Parent).Resources["addPointsCursor"])).Cursor;
                        break;
                    case DrawModes.Fixities:
                        this.Cursor = ((TextBlock) (((MainWindow) ((Grid) ((TabControl) ((TabItem) ((Grid) this.Parent).Parent).Parent).Parent).Parent).Resources["fixityCursor"])).Cursor;
                        break;
                    case DrawModes.PointLoad:
                        this.Cursor = ((TextBlock) (((MainWindow) ((Grid) ((TabControl) ((TabItem) ((Grid) this.Parent).Parent).Parent).Parent).Parent).Resources["pointLoadCursor"])).Cursor;
                        break;
                    case DrawModes.LineLoad:
                        this.Cursor = ((TextBlock) (((MainWindow) ((Grid) ((TabControl) ((TabItem) ((Grid) this.Parent).Parent).Parent).Parent).Parent).Resources["lineLoadCursor"])).Cursor;
                        break;
                    case DrawModes.PrintPoint:
                        this.Cursor = ((TextBlock) (((MainWindow) ((Grid) ((TabControl) ((TabItem) ((Grid) this.Parent).Parent).Parent).Parent).Parent).Resources["printPointCursor"])).Cursor;
                        break;
                    default:
                        this.Cursor = Cursors.Arrow;
                        break;
                }
            }
        }

        protected override void OnMouseRightButtonUp ( MouseButtonEventArgs e )
        {
            base.OnMouseRightButtonUp( e );

            /*
             * Draw Boundaries Mode
             */

            if ( drawing )
            {
                // Remove rubber banding point
                drawLine.Points.RemoveAt( drawLine.Points.Count - 1 );

                // Get array of drawing points
                Point[] points = new Point[drawLine.Points.Count];
                drawLine.Points.CopyTo( points , 0 );

                if ( DrawMode == DrawModes.Boundaries )
                {
                    if ( points.Length > 2 )
                    {
                        // Remove existing boundary
                        this.Children.Remove( boundary.Boundary );

                        // Add new boundary
                        boundary = new SlopeBoundary( this , points );
                    }
                }
                else if ( DrawMode == DrawModes.Materials )
                {
                    if ( points.Length > 2 )
                    {
                        // Add new material block
                        MaterialBlock newMaterialBlock = new MaterialBlock( this , points );
                        newMaterialBlock.Material = materialTypes.Find( delegate( MaterialType mt ) { return mt.Name == "NULL"; } );
                        materialBlocks.Add( newMaterialBlock );
                    }
                }

                BuildAxes();
            }

            // De-select any selected objects
            ClearSelections();

            // End drawing
            CancelDrawing();

            // Reset cursor and drawing mode
            this.Cursor = Cursors.Arrow;
            DrawMode = DrawModes.Select;

            TabItem tabParent = (TabItem) ((Grid) this.Parent).Parent;
            TabControl windowManager = (TabControl) tabParent.Parent;
            windowManager.SelectedItem = null;
            windowManager.SelectedItem = tabParent;
        }

        protected override void OnMouseWheel ( MouseWheelEventArgs e )
        {
            base.OnMouseWheel( e );

            // Update mouse scroll sentinel
            mouseDelta += e.Delta;

            // Zoom in (centred on mouse position)
            while ( mouseDelta >= 120 )
            {
                Zoom( 1.1 , e.GetPosition( this ) );
                mouseDelta -= 120;
            }

            // Zoom out (centred on mouse position)
            while ( mouseDelta <= -120 )
            {
                Zoom( 1 / 1.1 , e.GetPosition( this ) );
                mouseDelta += 120;
            }
        }



        // ----------------------------------
        // CANVAS EVENTS
        // ----------------------------------


        private void SlopeCanvas_SizeChanged ( object sender , SizeChangedEventArgs e )
        {
            // This only occurs once, immediately after canvas intialization
            if ( !IsScaled )
            {
                // Obtain DPI values for scaling
                System.Windows.Forms.Button dummy = new System.Windows.Forms.Button();
                System.Drawing.Graphics g = dummy.CreateGraphics();
                dpiX = g.DpiX;
                dpiY = g.DpiY;
                g.Dispose();
                dummy.Dispose();

                // Get units dependent scaling factor
                double factor;
                switch ( Units )
                {
                    case Units.Metres: factor = 0.0254; break;
                    case Units.Millimetres: factor = 25.4; break;
                    case Units.Feet: factor = 1.0 / 12.0; break;
                    default: factor = 1.0; break;
                }

                // Compute required initial scale
                double canvasWidth = (ActualWidth - 100) / dpiX * factor;
                double canvasHeight = (ActualHeight - 100) / dpiY * factor;
                Scale = Math.Max( (XAxisMax - XAxisMin) / canvasWidth , (YAxisMax - YAxisMin) / canvasHeight );

                // Get axis references
                xAxis = (Grid) ((Grid) this.Parent).Children[0];
                yAxis = (Grid) ((Grid) this.Parent).Children[1];

                // Get progress bar reference
                analysisProgress = (ProgressBar) ((Grid) this.Parent).Children[3];

                // Get hide canvas rectangle reference
                hideCanvasRect = (Rectangle) ((Grid) this.Parent).Children[4];

                // Construct axes and grid
                BuildAxes();

                // Centre and fit view
                CentreAndFitExtents( true );

                IsScaled = true;
            }
            else
            {
                // Compute translation factor for y coordinates
                double deltaY = e.NewSize.Height - e.PreviousSize.Height;

                Line line;      // For unboxing axis lines

                // Update coordinates of y axis lines
                for ( int i = 2 ; i < yAxis.Children.Count ; i++ )
                {
                    if ( yAxis.Children[i] is Line )
                    {
                        line = yAxis.Children[i] as Line;
                        line.Y1 = line.Y2 += deltaY;
                    }
                }

                Vector delta = new Vector( 0 , deltaY );

                // Update grid point locations
                gridPoints.ForEach( delegate( GridPoint gp ) { gp.Translate( delta ); } );

                // Update boundary shape
                boundary.Translate( delta );

                // Update material blocks
                materialBlocks.ForEach( delegate( MaterialBlock mb ) { mb.Translate( delta ); } );

                // Update FEA elements
                FEATriElements.ForEach( delegate( fe3NodedTriElement element ) { element.Translate( delta ); } );
                FEAQuadElements.ForEach( delegate( fe4NodedQuadElement element ) { element.Translate( delta ); } );
                
                // Update drawing line
                Point p;
                for ( int i = 0 ; i < drawLine.Points.Count ; i++ )
                {
                    p = drawLine.Points[i];
                    p.Y += deltaY;
                    drawLine.Points[i] = p;
                }

                // Update critical surface
                if ( criticalSurface != null ) criticalSurface.Translate( delta );

                // Update run surfaces
                runSurfaces.ForEach( delegate( DisplayCircularSurface rs ) { rs.Translate( delta ); } );
            }
        }


        // ----------------------------------
        // ANALYSIS WORKER EVENTS
        // ----------------------------------

        private void analysisWorker_DoWork ( object sender , DoWorkEventArgs e )
        {
            BackgroundWorker worker = sender as BackgroundWorker;

            e.Result = GenAlg( this , worker , e );
        }

        private void analysisWorker_RunWorkerCompleted ( object sender , RunWorkerCompletedEventArgs e )
        {
            if ( e.Error != null ) MessageBox.Show( e.Error.Message );
            else if ( e.Cancelled ) MessageBox.Show( "Cancelled" );
            else
            {
                ClosableCanvasTabItem parentTab = (ClosableCanvasTabItem) ((Grid) this.Parent).Parent;
                TabControl windowManager = (TabControl) parentTab.Parent;
                Menu mainMenu = (Menu) ((DockPanel) ((Grid) windowManager.Parent).Children[0]).Children[0];
                MenuItem outputMenu = (MenuItem) mainMenu.Items[4];
                MenuItem showCritical = (MenuItem) outputMenu.Items[0];

                windowManager.SelectedItem = parentTab;

                this.IsAnalyzed = true;

                SaveInputFile( FilePath );

                string[] pathsplit = FilePath.Split( '.' );

                double globalMinSF = 1000 , localMinSF = 1000;
                if ( pathsplit.Length > 1 )
                {
                    switch ( AnalysisType )
                    {
                        case AnalysisType.Bishop: pathsplit[pathsplit.Length - 1] = "bish"; break;
                        default: pathsplit[pathsplit.Length - 1] = "rfem"; break;
                    }

                    string path = string.Join( "." , pathsplit );

                    if ( File.Exists( path ) )
                    {
                        // Get units dependent scaling factor
                        double factor;
                        switch ( Units )
                        {
                            case Units.Metres: factor = 0.0254; break;
                            case Units.Millimetres: factor = 25.4; break;
                            case Units.Feet: factor = 1.0 / 12.0; break;
                            default: factor = 1.0; break;
                        }

                        // Read in all existing output file contents
                        List<string> contents = new List<string>( File.ReadAllLines( path ) );

                        // Delete previous surfaces
                        if ( criticalSurface != null ) criticalSurface.Delete();
                        for ( int i = 0 ; i < runSurfaces.Count ; i++ ) runSurfaces[i].Delete();
                        runSurfaces.Clear();

                        // Find line with global critical surface
                        int icritical = contents.FindIndex( delegate( string s ) { return s.Contains( "MOST CRITICAL SURFACE" ); } );

                        // Read in global critical surface geometry
                        double radius = double.Parse( contents[icritical + 4].Split( '=' )[1] );
                        double xEnter = double.Parse( contents[icritical + 5].Split( '=' )[1] );
                        double yEnter = double.Parse( contents[icritical + 6].Split( '=' )[1] );
                        double xExit = double.Parse( contents[icritical + 7].Split( '=' )[1] );
                        double yExit = double.Parse( contents[icritical + 8].Split( '=' )[1] );
                        globalMinSF = double.Parse( contents[icritical + 9].Split( '=' )[1] );

                        // Convert to screen pixel units
                        radius = radius / (factor * Scale) * dpiX;
                        xEnter = xEnter / (factor * Scale) * dpiX + OriginOffsetX;
                        yEnter = ActualHeight - (yEnter / (factor * Scale) * dpiY + OriginOffsetY);
                        xExit = xExit / (factor * Scale) * dpiX + OriginOffsetX;
                        yExit = ActualHeight - (yExit / (factor * Scale) * dpiY + OriginOffsetY);

                        // Create new surface
                        criticalSurface = new DisplayCircularSurface( this ,
                            new Point( xEnter , yEnter ) , new Point( xExit , yExit ) , radius );

                        // Indicate that it is the global critical surface with thicker line
                        criticalSurface.IsGlobalCritical = true;
                        showCritical.IsChecked = true;
                        this.ShowCritical = true;

                        // Find line with number of runs and read this line
                        int iruncount = contents.FindIndex( delegate( string s ) { return s.Contains( "Number of Runs = " ); } );
                        int runs = int.Parse( contents[iruncount].Split( '=' )[1] );

                        // Get current run data
                        string runheader = String.Format( "Run #{0}" , runs );
                        int irunline = contents.FindIndex( delegate( string s ) { return s.Contains( runheader ); } );

                        // Read in local minimum surface data
                        string[] surfacedata = contents[irunline + 3].Split( new char[] { '\t' } , StringSplitOptions.RemoveEmptyEntries );

                        // Read in surface geometry
                        radius = double.Parse( surfacedata[2] );
                        xEnter = double.Parse( surfacedata[3] );
                        yEnter = double.Parse( surfacedata[4] );
                        xExit = double.Parse( surfacedata[5] );
                        yExit = double.Parse( surfacedata[6] );
                        localMinSF = double.Parse( surfacedata[7] );

                        // Convert to screen pixel units
                        radius = radius / (factor * Scale) * dpiX;
                        xEnter = xEnter / (factor * Scale) * dpiX + OriginOffsetX;
                        yEnter = ActualHeight - (yEnter / (factor * Scale) * dpiY + OriginOffsetY);
                        xExit = xExit / (factor * Scale) * dpiX + OriginOffsetX;
                        yExit = ActualHeight - (yExit / (factor * Scale) * dpiY + OriginOffsetY);

                        // Add to list of current run surfaces
                        runSurfaces.Add( new DisplayCircularSurface( this ,
                            new Point( xEnter , yEnter ) , new Point( xExit , yExit ) , radius ) );

                        // Indicate that it is a local critical surface with thicker line
                        runSurfaces[0].IsLocalCritical = true;

                        // Repeat for the rest of the current run surfaces
                        for ( int i = 4 ; i < 13 ; i++ )
                        {
                            surfacedata = contents[irunline + i].Split( new char[] { '\t' } , StringSplitOptions.RemoveEmptyEntries );

                            radius = double.Parse( surfacedata[2] );
                            xEnter = double.Parse( surfacedata[3] );
                            yEnter = double.Parse( surfacedata[4] );
                            xExit = double.Parse( surfacedata[5] );
                            yExit = double.Parse( surfacedata[6] );

                            radius = radius / (factor * Scale) * dpiX;
                            xEnter = xEnter / (factor * Scale) * dpiX + OriginOffsetX;
                            yEnter = ActualHeight - (yEnter / (factor * Scale) * dpiY + OriginOffsetY);
                            xExit = xExit / (factor * Scale) * dpiX + OriginOffsetX;
                            yExit = ActualHeight - (yExit / (factor * Scale) * dpiY + OriginOffsetY);

                            runSurfaces.Add( new DisplayCircularSurface( this ,
                            new Point( xEnter , yEnter ) , new Point( xExit , yExit ) , radius ) );
                        }
                    }
                }

                MessageBox.Show( String.Format( "Global Minimum Fs = {0}\n\nLocal Minimum Fs = {1}" ,
                    globalMinSF , localMinSF ) , "Analysis Complete" );
            }

            analysisProgress.Value = 0;

            this.IsAnalyzing = false;
        }

        private void analysisWorker_ProgressChanged ( object sender , ProgressChangedEventArgs e )
        {
            analysisProgress.Value = e.ProgressPercentage;
        }


        // -----------------------------------------------------------------------------
        // MATRIX CLASS TESTER
        // -----------------------------------------------------------------------------
        //private void matrixTest()
        //{
        //    string[] split = FilePath.Split('.');

        //    split[split.Length - 1] = "dense";
        //    string densepath = string.Join(".", split);

        //    using (TextWriter tw = new StreamWriter(densepath))
        //    {
        //        DenseMatrix I = new DenseMatrix(5, 5, true);
        //        tw.WriteLine("I = 5x5 identity matrix");
        //        I.Print(tw);

        //        tw.WriteLine();

        //        DenseMatrix A = new DenseMatrix(5, 5);
        //        tw.WriteLine("A = empty 5x5 matrix");
        //        A.Print(tw);

        //        tw.WriteLine();

        //        A.FillRandom();
        //        tw.WriteLine("A = random 5x5 matrix");
        //        A.Print(tw);

        //        tw.WriteLine();

        //        tw.WriteLine("A^T");
        //        A.Transpose.Print(tw);

        //        tw.WriteLine();

        //        tw.WriteLine("A[2,3] = {0:e6}", A[2, 3]);

        //        tw.WriteLine();

        //        tw.WriteLine("A[2,:]");
        //        A[2].Print(tw);

        //        tw.WriteLine();

        //        tw.WriteLine("A[:,3]");
        //        A[3, true].Print(tw);

        //        tw.WriteLine();

        //        DenseMatrix B = A.Copy();
        //        tw.WriteLine("B = A.Copy()");
        //        B.Print(tw);

        //        tw.WriteLine();

        //        DenseMatrix[] luA = A.LU;
        //        tw.WriteLine("A = LU");
        //        tw.WriteLine("L");
        //        luA[0].Print(tw);
        //        tw.WriteLine();
        //        tw.WriteLine("U");
        //        luA[1].Print(tw);

        //        tw.WriteLine();

        //        A.FillRandom();
        //        tw.WriteLine("A = random 5x5 matrix");
        //        A.Print(tw);
        //        tw.WriteLine("det(A) = {0}", A.Determinant);

        //        tw.WriteLine();

        //        tw.WriteLine("B");
        //        B.Print(tw);
        //        tw.WriteLine("det(B) = {0}", B.Determinant);

        //        tw.WriteLine();

        //        tw.WriteLine("A==B = {0}", A == B);
        //        tw.WriteLine("A!=B = {0}", A != B);

        //        tw.WriteLine();

        //        tw.WriteLine("+A");
        //        (+A).Print(tw);

        //        tw.WriteLine();

        //        tw.WriteLine("-A");
        //        (-A).Print(tw);

        //        tw.WriteLine();

        //        tw.WriteLine("A + B");
        //        (A + B).Print(tw);

        //        tw.WriteLine();

        //        tw.WriteLine("A + 5");
        //        (A + 5).Print(tw);

        //        tw.WriteLine();

        //        tw.WriteLine("A - B");
        //        (A - B).Print(tw);

        //        tw.WriteLine();

        //        tw.WriteLine("A * B");
        //        (A * B).Print(tw);

        //        tw.WriteLine();

        //        tw.WriteLine("A * 5");
        //        (A * 5).Print(tw);

        //        tw.WriteLine();

        //        tw.WriteLine("A .* B");
        //        DenseMatrix.ElementMult(A, B).Print(tw);

        //        tw.WriteLine();

        //        tw.WriteLine("A / 5");
        //        (A / 5).Print(tw);

        //        tw.WriteLine();

        //        tw.WriteLine("Abs(-A)");
        //        DenseMatrix.Abs(-A).Print(tw);

        //        tw.WriteLine();

        //        tw.WriteLine("A^3");
        //        DenseMatrix.Pow(A, 3).Print(tw);

        //        tw.WriteLine();

        //        tw.WriteLine("A^(-1)");
        //        A.Inverse.Print(tw);

        //        tw.WriteLine();

        //        tw.WriteLine("A = random 7x7 symmetric positive definite");
        //        A = new DenseMatrix(7, 7);
        //        A.FillRandSymPosDef();
        //        A.Print(tw);

        //        tw.WriteLine();

        //        tw.WriteLine("f = random 7x1 matrix");
        //        DenseMatrix f = new DenseMatrix(7);
        //        f.FillRandom();
        //        f.Print(tw);

        //        tw.WriteLine();

        //        tw.WriteLine("A^(-1)");
        //        DenseMatrix Ainv = A.Inverse;
        //        Ainv.Print(tw);

        //        tw.WriteLine();

        //        tw.WriteLine("x = Ainv * f");
        //        DenseMatrix x = Ainv * f;
        //        x.Print(tw);

        //        tw.WriteLine();

        //        DateTime starttime = DateTime.Now;
        //        A = new DenseMatrix(100, 100);
        //        A.FillRandSymPosDef();
        //        Ainv = A.Inverse;
        //        TimeSpan runtime = DateTime.Now - starttime;
        //        tw.WriteLine("Time to invert 100x100 symmetric positive definite matrix = {0}", runtime);

        //        tw.WriteLine();
        //    }

        //    split[split.Length - 1] = "band";
        //    string bandpath = string.Join(".", split);

        //    using (TextWriter tw = new StreamWriter(bandpath))
        //    {
        //        BandSymMatrix I = new BandSymMatrix(5, 3, true);
        //        tw.WriteLine("I = 5x5 identity matrix");
        //        I.Print(tw);

        //        tw.WriteLine();

        //        BandSymMatrix A = new BandSymMatrix(5, 3);
        //        tw.WriteLine("A = empty 5x5 matrix");
        //        A.Print(tw);

        //        tw.WriteLine();

        //        A.FillRandom();
        //        tw.WriteLine("A = random 5x5 matrix");
        //        A.Print(tw);

        //        tw.WriteLine();

        //        tw.WriteLine("A^T");
        //        A.Transpose.Print(tw);

        //        tw.WriteLine();

        //        tw.WriteLine("A[2,3] = {0:e6}", A[2, 3]);

        //        tw.WriteLine();

        //        tw.WriteLine("A[2,:]");
        //        A[2].Print(tw);

        //        tw.WriteLine();

        //        tw.WriteLine("A[:,3]");
        //        A[3, true].Print(tw);

        //        tw.WriteLine();

        //        A.FillRandPosDef();
        //        BandSymMatrix B = A.Copy();
        //        tw.WriteLine("B = A.Copy()");
        //        B.Print(tw);

        //        tw.WriteLine();

        //        DenseMatrix L = A.Cholesky;
        //        tw.WriteLine("A = L * L^T");
        //        tw.WriteLine("L");
        //        L.Print(tw);

        //        tw.WriteLine();

        //        A.FillRandPosDef();
        //        tw.WriteLine("A = random 5x5 matrix");
        //        A.Print(tw);
        //        tw.WriteLine("det(A) = {0}", A.Determinant);

        //        tw.WriteLine();

        //        tw.WriteLine("B");
        //        B.Print(tw);
        //        tw.WriteLine("det(B) = {0}", B.Determinant);

        //        tw.WriteLine();

        //        tw.WriteLine("A==B = {0}", A == B);
        //        tw.WriteLine("A!=B = {0}", A != B);

        //        tw.WriteLine();

        //        tw.WriteLine("+A");
        //        (+A).Print(tw);

        //        tw.WriteLine();

        //        tw.WriteLine("-A");
        //        (-A).Print(tw);

        //        tw.WriteLine();

        //        tw.WriteLine("A + B");
        //        (A + B).Print(tw);

        //        tw.WriteLine();

        //        tw.WriteLine("A + 5");
        //        (A + 5).Print(tw);

        //        tw.WriteLine();

        //        tw.WriteLine("A - B");
        //        (A - B).Print(tw);

        //        tw.WriteLine();

        //        tw.WriteLine("A * B");
        //        (A * B).Print(tw);

        //        tw.WriteLine();

        //        tw.WriteLine("A * 5");
        //        (A * 5).Print(tw);

        //        tw.WriteLine();

        //        tw.WriteLine("A .* B");
        //        BandSymMatrix.ElementMult(A, B).Print(tw);

        //        tw.WriteLine();

        //        tw.WriteLine("A / 5");
        //        (A / 5).Print(tw);

        //        tw.WriteLine();

        //        tw.WriteLine("Abs(-A)");
        //        BandSymMatrix.Abs(-A).Print(tw);

        //        tw.WriteLine();

        //        tw.WriteLine("A^3");
        //        BandSymMatrix.Pow(A, 3).Print(tw);

        //        tw.WriteLine();

        //        tw.WriteLine("A^(-1)");
        //        A.Inverse.Print(tw);

        //        tw.WriteLine();

        //        tw.WriteLine("A = random 7x7 symmetric positive definite");
        //        A = new BandSymMatrix(7, 3);
        //        A.FillRandPosDef();
        //        A.Print(tw);

        //        tw.WriteLine();

        //        tw.WriteLine("f = random 7x1 matrix");
        //        DenseMatrix f = new DenseMatrix(7);
        //        f.FillRandom();
        //        f.Print(tw);

        //        tw.WriteLine();

        //        tw.WriteLine("A^(-1)");
        //        DenseMatrix Ainv = A.Inverse;
        //        Ainv.Print(tw);

        //        tw.WriteLine();

        //        tw.WriteLine("x = Ainv * f");
        //        DenseMatrix x = Ainv * f;
        //        x.Print(tw);

        //        tw.WriteLine();

        //        DateTime starttime = DateTime.Now;
        //        A = new BandSymMatrix(100, 3);
        //        A.FillRandPosDef();
        //        Ainv = A.Inverse;
        //        TimeSpan runtime = DateTime.Now - starttime;
        //        tw.WriteLine("Time to invert 100x100 symmetric positive definite matrix = {0}", runtime);

        //        tw.WriteLine();
        //    }

        //    split[split.Length - 1] = "combo";
        //    string combopath = string.Join(".", split);

        //    using (TextWriter tw = new StreamWriter(combopath))
        //    {
        //        BandSymMatrix A = new BandSymMatrix(5, 3);
        //        A.FillRandPosDef();
        //        tw.WriteLine("A = random 5x5 matrix");
        //        A.Print(tw);
        //        tw.WriteLine("det(A) = {0}", A.Determinant);

        //        tw.WriteLine();

        //        DenseMatrix B = A.Copy();
        //        tw.WriteLine("B = A.Copy()");
        //        B.Print(tw);

        //        tw.WriteLine();

        //        B.FillRandom();
        //        tw.WriteLine("B");
        //        B.Print(tw);
        //        tw.WriteLine("det(B) = {0}", B.Determinant);

        //        tw.WriteLine();

        //        tw.WriteLine("A==B = {0}", A == B);
        //        tw.WriteLine("A!=B = {0}", A != B);

        //        tw.WriteLine();

        //        tw.WriteLine("A + B");
        //        (A + B).Print(tw);

        //        tw.WriteLine();

        //        tw.WriteLine("A - B");
        //        (A - B).Print(tw);

        //        tw.WriteLine();

        //        tw.WriteLine("A * B");
        //        (A * B).Print(tw);

        //        tw.WriteLine();

        //        tw.WriteLine("A .* B");
        //        DenseMatrix.ElementMult(A, B).Print(tw);

        //        tw.WriteLine();
        //    }
        //}
    }
}
