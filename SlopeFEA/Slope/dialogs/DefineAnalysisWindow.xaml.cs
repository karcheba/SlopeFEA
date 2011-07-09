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
    /// Interaction logic for DefineAnalysisWindow.xaml
    /// </summary>
    public partial class DefineAnalysisWindow : Window
    {
        private SlopeCanvas canvas;
        private SlopeDefineCanvas inputCanvas;
        private List<MaterialType> materialTypes;
        private MaterialType selectedMaterial;
        private Rectangle materialFill;
        private ComboBox materialList, phaseList, beginPhaseList;
        private CheckBox resetDisplacements;
        private TextBox loadSteps , iterations , printLines , gravityFactor;
        private TextBlock phiValue , cohValue , psiValue , gammaValue , emodValue , nuValue;
        private Label phiUnits , cohUnits , psiUnits , gammaUnits , emodUnits;
        private Button add , modify , delete;

        public DefineAnalysisWindow ( Window owner , SlopeCanvas canvas )
        {
            InitializeComponent();

            this.Owner = owner;
            this.canvas = canvas;
            materialTypes = canvas.MaterialTypes;

            // Create Grid to contain two axes, info block, and plotting canvas
            Grid inputGrid = new Grid();
            inputGrid.HorizontalAlignment = HorizontalAlignment.Stretch;
            inputGrid.VerticalAlignment = VerticalAlignment.Stretch;
            inputGrid.Background = Brushes.WhiteSmoke;
            inputGrid.Margin = new Thickness( 0 , menuPanel.Height , 0 , 0 );
            contentGrid.Children.Add( inputGrid );

            // Some formatting constants
            double axisWidth = 55 ,
                    inputAreaWidth = 325 ,
                    progressBarWidth = 10 ,
                    buttonWidth = 60 ,
                    buttonOffset = 2.5 * buttonWidth;

            /*
             * Create coordinate axes
             */

            // X-axis
            Grid xAxis = new Grid();
            xAxis.ClipToBounds = true;
            xAxis.Background = Brushes.WhiteSmoke;
            xAxis.VerticalAlignment = VerticalAlignment.Bottom;
            xAxis.HorizontalAlignment = HorizontalAlignment.Stretch;
            xAxis.Margin = new Thickness( axisWidth , 0 , inputAreaWidth , progressBarWidth );
            xAxis.Height = axisWidth;
            inputGrid.Children.Add( xAxis );
            // Y-axis
            Grid yAxis = new Grid();
            yAxis.ClipToBounds = true;
            yAxis.Background = Brushes.WhiteSmoke;
            yAxis.VerticalAlignment = VerticalAlignment.Stretch;
            yAxis.HorizontalAlignment = HorizontalAlignment.Left;
            yAxis.Margin = new Thickness( 0 , 0 , 0 , axisWidth + progressBarWidth );
            yAxis.Width = axisWidth;
            inputGrid.Children.Add( yAxis );

            /*
             * Create input area for analysis phase data
             */

            // Parent = ScrollViewer
            ScrollViewer inputArea = new ScrollViewer();
            inputArea.ClipToBounds = true;
            inputArea.Background = Brushes.WhiteSmoke;
            inputArea.VerticalAlignment = VerticalAlignment.Stretch;
            inputArea.HorizontalAlignment = HorizontalAlignment.Right;
            inputArea.Margin = new Thickness( 0 , 0 , 0 , progressBarWidth );
            inputArea.Width = inputAreaWidth;
            inputGrid.Children.Add( inputArea );

            // Parent.Content = Grid object for input
            Grid inputBlock = new Grid();
            inputBlock.ClipToBounds = true;
            inputBlock.Background = Brushes.WhiteSmoke;
            inputBlock.VerticalAlignment = VerticalAlignment.Stretch;
            inputBlock.HorizontalAlignment = HorizontalAlignment.Stretch;
            inputBlock.Margin = new Thickness( 0 );
            inputArea.Content = inputBlock;

            // Parent.Content.Children[0] = Border
            Border inputBlockBorder = new Border();
            inputBlockBorder.BorderBrush = Brushes.DimGray;
            inputBlockBorder.VerticalAlignment = VerticalAlignment.Stretch;
            inputBlockBorder.HorizontalAlignment = HorizontalAlignment.Stretch;
            inputBlockBorder.BorderThickness = new Thickness( 1 );
            inputBlockBorder.Margin = new Thickness( 0 );
            inputBlock.Children.Add( inputBlockBorder );


            // Parent.Content.Children[1] = Phase data GroupBox
            GroupBox phaseBox = new GroupBox();
            phaseBox.Header = "Analysis Phase Data";
            phaseBox.FontWeight = FontWeights.Bold;
            phaseBox.VerticalAlignment = VerticalAlignment.Top;
            phaseBox.HorizontalAlignment = HorizontalAlignment.Stretch;
            phaseBox.Margin = new Thickness( 15 , 30 , 15 , 0 );
            phaseBox.Height = 250;
            inputBlock.Children.Add( phaseBox );

            // Parent.Content.Children[1].Content = Grid for phase data GroupBox elements
            Grid phaseGrid = new Grid();
            phaseGrid.VerticalAlignment = VerticalAlignment.Stretch;
            phaseGrid.HorizontalAlignment = HorizontalAlignment.Stretch;
            phaseGrid.Margin = new Thickness( 0 );
            phaseBox.Content = phaseGrid;

            // Label for AnalysisPhase ComboBox
            Label phaseListLabel = new Label();
            phaseListLabel.Content = "Phase:";
            phaseListLabel.Height = 28;
            phaseListLabel.Width = 80;
            phaseListLabel.HorizontalContentAlignment = HorizontalAlignment.Right;
            phaseListLabel.HorizontalAlignment = HorizontalAlignment.Center;
            phaseListLabel.VerticalAlignment = VerticalAlignment.Center;
            phaseListLabel.Margin = new Thickness( -180 , -170 , 0 , 0 );
            phaseListLabel.FontWeight = FontWeights.Normal;
            phaseGrid.Children.Add( phaseListLabel );

            // Name of selected AnalysisPhase
            phaseList = new ComboBox();
            phaseList.Text = "Add new analysis phase...";
            phaseList.Height = 23;
            phaseList.Width = 160;
            phaseList.HorizontalAlignment = HorizontalAlignment.Center;
            phaseList.VerticalAlignment = VerticalAlignment.Center;
            phaseList.Margin = new Thickness( 70 , -170 , 0 , 0 );
            phaseList.FontWeight = FontWeights.Normal;
            phaseList.IsEditable = true;
            phaseList.SelectionChanged += new SelectionChangedEventHandler( phaseList_SelectionChanged );
            phaseGrid.Children.Add( phaseList );

            // Label for AnalysisPhase ComboBox
            Label beginPhaseLabel = new Label();
            beginPhaseLabel.Content = "Begin From:";
            beginPhaseLabel.Height = 28;
            beginPhaseLabel.Width = 80;
            beginPhaseLabel.HorizontalContentAlignment = HorizontalAlignment.Right;
            beginPhaseLabel.HorizontalAlignment = HorizontalAlignment.Center;
            beginPhaseLabel.VerticalAlignment = VerticalAlignment.Center;
            beginPhaseLabel.Margin = new Thickness( -180 , -110 , 0 , 0 );
            beginPhaseLabel.FontWeight = FontWeights.Normal;
            phaseGrid.Children.Add( beginPhaseLabel );

            // Name of AnalysisPhase to begin from
            beginPhaseList = new ComboBox();
            beginPhaseList.Height = 23;
            beginPhaseList.Width = 160;
            beginPhaseList.HorizontalAlignment = HorizontalAlignment.Center;
            beginPhaseList.VerticalAlignment = VerticalAlignment.Center;
            beginPhaseList.Margin = new Thickness( 70 , -110 , 0 , 0 );
            beginPhaseList.FontWeight = FontWeights.Normal;
            beginPhaseList.IsEditable = false;
            beginPhaseList.SelectionChanged += new SelectionChangedEventHandler( beginPhaseList_SelectionChanged );
            phaseGrid.Children.Add( beginPhaseList );

            // CheckBox for reset displacements option
            resetDisplacements = new CheckBox();
            resetDisplacements.Content = "Reset displacements to zero?";
            resetDisplacements.Height = 23;
            resetDisplacements.Width = 180;
            resetDisplacements.HorizontalAlignment = HorizontalAlignment.Center;
            resetDisplacements.VerticalAlignment = VerticalAlignment.Center;
            resetDisplacements.HorizontalContentAlignment = HorizontalAlignment.Center;
            resetDisplacements.Margin = new Thickness( 0 , -50 , 0 , 0 );
            resetDisplacements.FontWeight = FontWeights.Normal;
            phaseGrid.Children.Add( resetDisplacements );

            // Label for number of load steps
            Label loadStepsLabel = new Label();
            loadStepsLabel.Content = "# Load Steps";
            loadStepsLabel.Height = 28;
            loadStepsLabel.Width = 150;
            loadStepsLabel.HorizontalAlignment = HorizontalAlignment.Center;
            loadStepsLabel.VerticalAlignment = VerticalAlignment.Center;
            loadStepsLabel.HorizontalContentAlignment = HorizontalAlignment.Right;
            loadStepsLabel.Margin = new Thickness( -120 , 26 , 0 , 0 );
            loadStepsLabel.FontWeight = FontWeights.Normal;
            phaseGrid.Children.Add( loadStepsLabel );

            // TextBox for number of load steps
            loadSteps = new TextBox();
            loadSteps.Height = 23;
            loadSteps.Width = 90;
            loadSteps.HorizontalAlignment = HorizontalAlignment.Center;
            loadSteps.VerticalAlignment = VerticalAlignment.Center;
            loadSteps.Margin = new Thickness( 140 , 26 , 0 , 0 );
            loadSteps.FontWeight = FontWeights.Normal;
            phaseGrid.Children.Add( loadSteps );

            // Label for number of iterations
            Label iterationsLabel = new Label();
            iterationsLabel.Content = "# Iterations";
            iterationsLabel.Height = 28;
            iterationsLabel.Width = 150;
            iterationsLabel.HorizontalAlignment = HorizontalAlignment.Center;
            iterationsLabel.VerticalAlignment = VerticalAlignment.Center;
            iterationsLabel.HorizontalContentAlignment = HorizontalAlignment.Right;
            iterationsLabel.Margin = new Thickness( -120 , 78 , 0 , 0 );
            iterationsLabel.FontWeight = FontWeights.Normal;
            phaseGrid.Children.Add( iterationsLabel );

            // TextBox for number of iterations
            iterations = new TextBox();
            iterations.Height = 23;
            iterations.Width = 90;
            iterations.HorizontalAlignment = HorizontalAlignment.Center;
            iterations.VerticalAlignment = VerticalAlignment.Center;
            iterations.Margin = new Thickness( 140 , 78 , 0 , 0 );
            iterations.FontWeight = FontWeights.Normal;
            phaseGrid.Children.Add( iterations );

            // Label for number of load steps/print line
            Label printLinesLabel = new Label();
            printLinesLabel.Content = "# Load Steps / Print Line";
            printLinesLabel.Height = 28;
            printLinesLabel.Width = 150;
            printLinesLabel.HorizontalAlignment = HorizontalAlignment.Center;
            printLinesLabel.VerticalAlignment = VerticalAlignment.Center;
            printLinesLabel.HorizontalContentAlignment = HorizontalAlignment.Right;
            printLinesLabel.Margin = new Thickness( -120 , 132 , 0 , 0 );
            printLinesLabel.FontWeight = FontWeights.Normal;
            phaseGrid.Children.Add( printLinesLabel );

            // TextBox for number of load steps/print line
            printLines = new TextBox();
            printLines.Height = 23;
            printLines.Width = 90;
            printLines.HorizontalAlignment = HorizontalAlignment.Center;
            printLines.VerticalAlignment = VerticalAlignment.Center;
            printLines.Margin = new Thickness( 140 , 132 , 0 , 0 );
            printLines.FontWeight = FontWeights.Normal;
            phaseGrid.Children.Add( printLines );

            // Label for gravity factor
            Label gravityLabel = new Label();
            gravityLabel.Content = "Gravity Factor";
            gravityLabel.Height = 28;
            gravityLabel.Width = 150;
            gravityLabel.HorizontalAlignment = HorizontalAlignment.Center;
            gravityLabel.VerticalAlignment = VerticalAlignment.Center;
            gravityLabel.HorizontalContentAlignment = HorizontalAlignment.Right;
            gravityLabel.Margin = new Thickness( -120 , 184 , 0 , 0 );
            gravityLabel.FontWeight = FontWeights.Normal;
            phaseGrid.Children.Add( gravityLabel );

            // TextBox for gravity factor
            gravityFactor = new TextBox();
            gravityFactor.Height = 23;
            gravityFactor.Width = 90;
            gravityFactor.HorizontalAlignment = HorizontalAlignment.Center;
            gravityFactor.VerticalAlignment = VerticalAlignment.Center;
            gravityFactor.Margin = new Thickness( 140 , 184 , 0 , 0 );
            gravityFactor.FontWeight = FontWeights.Normal;
            phaseGrid.Children.Add( gravityFactor );


            /*
             * Initialize analysis phase parameters
             */

            for ( int i = 1 ; i < canvas.FEAPhases.Count ; i++ )
            {
                phaseList.Items.Add( canvas.FEAPhases[i] );
            }

            for ( int i = 0 ; i < canvas.FEAPhases.Count ; i++ )
            {
                beginPhaseList.Items.Add( canvas.FEAPhases[i] );
            }

            //phaseList.SelectedIndex = 0;


            /*
             * MaterialType selection data
             */

            // Parent.Content.Children[2] = MaterialType GroupBox
            GroupBox materialBox = new GroupBox();
            materialBox.Header = "Assign Materials";
            materialBox.FontWeight = FontWeights.Bold;
            materialBox.VerticalAlignment = VerticalAlignment.Top;
            materialBox.HorizontalAlignment = HorizontalAlignment.Stretch;
            materialBox.Margin = new Thickness( 15 , phaseBox.Margin.Top + phaseBox.Height + 30 , 15 , 0 );
            materialBox.Height = 300;
            inputBlock.Children.Add( materialBox );

            // Parent.Content.Children[2].Content = Grid for MaterialType GroupBox elements
            Grid materialGrid = new Grid();
            materialGrid.VerticalAlignment = VerticalAlignment.Stretch;
            materialGrid.HorizontalAlignment = HorizontalAlignment.Stretch;
            materialGrid.Margin = new Thickness( 0 );
            materialBox.Content = materialGrid;

            // Fill colour for selected MaterialType
            materialFill = new Rectangle();
            materialFill.Height = 20;
            materialFill.Width = 30;
            materialFill.HorizontalAlignment = HorizontalAlignment.Center;
            materialFill.VerticalAlignment = VerticalAlignment.Center;
            materialFill.Margin = new Thickness( -180 , -220 , 0 , 0 );
            materialFill.Stroke = Brushes.Black;
            materialFill.StrokeThickness = 1.5;
            materialFill.Fill = Brushes.Transparent;
            materialGrid.Children.Add( materialFill );

            // Name of selected MaterialType
            materialList = new ComboBox();
            materialList.Height = 23;
            materialList.Width = 150;
            materialList.HorizontalAlignment = HorizontalAlignment.Center;
            materialList.VerticalAlignment = VerticalAlignment.Center;
            materialList.Margin = new Thickness( 50 , -220 , 0 , 0 );
            materialList.FontWeight = FontWeights.Normal;
            materialList.IsEditable = true;
            materialList.SelectionChanged += new SelectionChangedEventHandler( materialList_SelectionChanged );
            materialGrid.Children.Add( materialList );

            // Angle of friction
            // Property Name
            Label phiLabel = new Label();
            phiLabel.Content = "Angle of Friction";
            phiLabel.Height = 28;
            phiLabel.Width = 100;
            phiLabel.HorizontalContentAlignment = HorizontalAlignment.Right;
            phiLabel.HorizontalAlignment = HorizontalAlignment.Center;
            phiLabel.VerticalAlignment = VerticalAlignment.Center;
            phiLabel.Margin = new Thickness( -160 , -125 , 0 , 0 );
            phiLabel.FontWeight = FontWeights.Normal;
            materialGrid.Children.Add( phiLabel );
            // Property Value
            phiValue = new TextBlock();
            phiValue.Height = 23;
            phiValue.Width = 90;
            phiValue.Text = "test........................................................";
            phiValue.HorizontalAlignment = HorizontalAlignment.Center;
            phiValue.VerticalAlignment = VerticalAlignment.Center;
            phiValue.Margin = new Thickness( 45 , -125 , 0 , 0 );
            phiValue.FontWeight = FontWeights.Normal;
            materialGrid.Children.Add( phiValue );
            // Property Units
            phiUnits = new Label();
            phiUnits.Content = "deg";
            phiUnits.Height = 28;
            phiUnits.Width = 55;
            phiUnits.HorizontalContentAlignment = HorizontalAlignment.Left;
            phiUnits.HorizontalAlignment = HorizontalAlignment.Center;
            phiUnits.VerticalAlignment = VerticalAlignment.Center;
            phiUnits.Margin = new Thickness( 220 , -125 , 0 , 0 );
            phiUnits.FontWeight = FontWeights.Normal;
            materialGrid.Children.Add( phiUnits );

            // Cohesion
            // Property Name
            Label cohLabel = new Label();
            cohLabel.Content = "Cohesion";
            cohLabel.Height = 28;
            cohLabel.Width = 100;
            cohLabel.HorizontalContentAlignment = HorizontalAlignment.Right;
            cohLabel.HorizontalAlignment = HorizontalAlignment.Center;
            cohLabel.VerticalAlignment = VerticalAlignment.Center;
            cohLabel.Margin = new Thickness( -160 , -75 , 0 , 0 );
            cohLabel.FontWeight = FontWeights.Normal;
            materialGrid.Children.Add( cohLabel );
            // Property Value
            cohValue = new TextBlock();
            cohValue.Height = 23;
            cohValue.Width = 90;
            cohValue.Text = "test...............................................................................";
            cohValue.HorizontalAlignment = HorizontalAlignment.Center;
            cohValue.VerticalAlignment = VerticalAlignment.Center;
            cohValue.Margin = new Thickness( 45 , -75 , 0 , 0 );
            cohValue.FontWeight = FontWeights.Normal;
            materialGrid.Children.Add( cohValue );
            // Property Units
            cohUnits = new Label();
            cohUnits.Content = "kPa";
            cohUnits.Height = 28;
            cohUnits.Width = 55;
            cohUnits.HorizontalContentAlignment = HorizontalAlignment.Left;
            cohUnits.HorizontalAlignment = HorizontalAlignment.Center;
            cohUnits.VerticalAlignment = VerticalAlignment.Center;
            cohUnits.Margin = new Thickness( 220 , -75 , 0 , 0 );
            cohUnits.FontWeight = FontWeights.Normal;
            materialGrid.Children.Add( cohUnits );

            // Dilatancy angle
            // Property Name
            Label psiLabel = new Label();
            psiLabel.Content = "Dilatancy Angle";
            psiLabel.Height = 28;
            psiLabel.Width = 100;
            psiLabel.HorizontalContentAlignment = HorizontalAlignment.Right;
            psiLabel.HorizontalAlignment = HorizontalAlignment.Center;
            psiLabel.VerticalAlignment = VerticalAlignment.Center;
            psiLabel.Margin = new Thickness( -160 , -25 , 0 , 0 );
            psiLabel.FontWeight = FontWeights.Normal;
            materialGrid.Children.Add( psiLabel );
            // Property Value
            psiValue = new TextBlock();
            psiValue.Height = 23;
            psiValue.Width = 90;
            psiValue.Text = "test...............................................................................";
            psiValue.HorizontalAlignment = HorizontalAlignment.Center;
            psiValue.VerticalAlignment = VerticalAlignment.Center;
            psiValue.Margin = new Thickness( 45 , -25 , 0 , 0 );
            psiValue.FontWeight = FontWeights.Normal;
            materialGrid.Children.Add( psiValue );
            // Property Units
            psiUnits = new Label();
            psiUnits.Content = "deg";
            psiUnits.Height = 28;
            psiUnits.Width = 55;
            psiUnits.HorizontalContentAlignment = HorizontalAlignment.Left;
            psiUnits.HorizontalAlignment = HorizontalAlignment.Center;
            psiUnits.VerticalAlignment = VerticalAlignment.Center;
            psiUnits.Margin = new Thickness( 220 , -25 , 0 , 0 );
            psiUnits.FontWeight = FontWeights.Normal;
            materialGrid.Children.Add( psiUnits );

            // Unit weight
            // Property Name
            Label gammaLabel = new Label();
            gammaLabel.Content = "Unit Weight";
            gammaLabel.Height = 28;
            gammaLabel.Width = 100;
            gammaLabel.HorizontalContentAlignment = HorizontalAlignment.Right;
            gammaLabel.HorizontalAlignment = HorizontalAlignment.Center;
            gammaLabel.VerticalAlignment = VerticalAlignment.Center;
            gammaLabel.Margin = new Thickness( -160 , 25 , 0 , 0 );
            gammaLabel.FontWeight = FontWeights.Normal;
            materialGrid.Children.Add( gammaLabel );
            // Property Value
            gammaValue = new TextBlock();
            gammaValue.Height = 23;
            gammaValue.Width = 90;
            gammaValue.Text = "test...............................................................................";
            gammaValue.HorizontalAlignment = HorizontalAlignment.Center;
            gammaValue.VerticalAlignment = VerticalAlignment.Center;
            gammaValue.Margin = new Thickness( 45 , 25 , 0 , 0 );
            gammaValue.FontWeight = FontWeights.Normal;
            materialGrid.Children.Add( gammaValue );
            // Property Units
            gammaUnits = new Label();
            gammaUnits.Content = "kN/m^3";
            gammaUnits.Height = 28;
            gammaUnits.Width = 55;
            gammaUnits.HorizontalContentAlignment = HorizontalAlignment.Left;
            gammaUnits.HorizontalAlignment = HorizontalAlignment.Center;
            gammaUnits.VerticalAlignment = VerticalAlignment.Center;
            gammaUnits.Margin = new Thickness( 220 , 25 , 0 , 0 );
            gammaUnits.FontWeight = FontWeights.Normal;
            materialGrid.Children.Add( gammaUnits );

            // Elastic modulus
            // Property Name
            Label emodLabel = new Label();
            emodLabel.Content = "Elastic Modulus";
            emodLabel.Height = 28;
            emodLabel.Width = 100;
            emodLabel.HorizontalContentAlignment = HorizontalAlignment.Right;
            emodLabel.HorizontalAlignment = HorizontalAlignment.Center;
            emodLabel.VerticalAlignment = VerticalAlignment.Center;
            emodLabel.Margin = new Thickness( -160 , 75 , 0 , 0 );
            emodLabel.FontWeight = FontWeights.Normal;
            materialGrid.Children.Add( emodLabel );
            // Property Value
            emodValue = new TextBlock();
            emodValue.Height = 23;
            emodValue.Width = 90;
            emodValue.Text = "test...............................................................................";
            emodValue.HorizontalAlignment = HorizontalAlignment.Center;
            emodValue.VerticalAlignment = VerticalAlignment.Center;
            emodValue.Margin = new Thickness( 45 , 75 , 0 , 0 );
            emodValue.FontWeight = FontWeights.Normal;
            materialGrid.Children.Add( emodValue );
            // Property Units
            emodUnits = new Label();
            emodUnits.Content = "kPa";
            emodUnits.Height = 28;
            emodUnits.Width = 55;
            emodUnits.HorizontalContentAlignment = HorizontalAlignment.Left;
            emodUnits.HorizontalAlignment = HorizontalAlignment.Center;
            emodUnits.VerticalAlignment = VerticalAlignment.Center;
            emodUnits.Margin = new Thickness( 220 , 75 , 0 , 0 );
            emodUnits.FontWeight = FontWeights.Normal;
            materialGrid.Children.Add( emodUnits );

            // Poisson's ratio
            // Property Name
            Label nuLabel = new Label();
            nuLabel.Content = "Poisson's Ratio";
            nuLabel.Height = 28;
            nuLabel.Width = 100;
            nuLabel.HorizontalContentAlignment = HorizontalAlignment.Right;
            nuLabel.HorizontalAlignment = HorizontalAlignment.Center;
            nuLabel.VerticalAlignment = VerticalAlignment.Center;
            nuLabel.Margin = new Thickness( -160 , 125 , 0 , 0 );
            nuLabel.FontWeight = FontWeights.Normal;
            materialGrid.Children.Add( nuLabel );
            // Property Value
            nuValue = new TextBlock();
            nuValue.Height = 23;
            nuValue.Width = 90;
            nuValue.Text = "test...............................................................................";
            nuValue.HorizontalAlignment = HorizontalAlignment.Center;
            nuValue.VerticalAlignment = VerticalAlignment.Center;
            nuValue.Margin = new Thickness( 45 , 125 , 0 , 0 );
            nuValue.FontWeight = FontWeights.Normal;
            materialGrid.Children.Add( nuValue );

            // Button for setting selected blocks to selected MaterialType
            Button setSelectedButton = new Button();
            setSelectedButton.Content = "Set Selected";
            setSelectedButton.FontWeight = FontWeights.Normal;
            setSelectedButton.Height = 23;
            setSelectedButton.Width = 150;
            setSelectedButton.VerticalAlignment = VerticalAlignment.Center;
            setSelectedButton.HorizontalAlignment = HorizontalAlignment.Center;
            setSelectedButton.Margin = new Thickness( 0 , 220 , 0 , 0 );
            setSelectedButton.Click += new RoutedEventHandler( setSelectedButton_Click );
            materialGrid.Children.Add( setSelectedButton );

            /*
             * Initialize list of MaterialTypes
             */

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

            /*
             * Analysis Phase buttons
             */

            // Parent.Content.Children[3] = Add button
            add = new Button();
            add.Content = "Add";
            add.Height = 23;
            add.Width = buttonWidth;
            add.VerticalAlignment = VerticalAlignment.Top;
            add.HorizontalAlignment = HorizontalAlignment.Center;
            add.Margin = new Thickness( -buttonOffset , materialBox.Margin.Top + materialBox.Height + 30 , 0 , 15 );
            add.Click += new RoutedEventHandler( add_Click );
            inputBlock.Children.Add( add );

            // Parent.Content.Children[4] = Modify button
            modify = new Button();
            modify.Content = "Modify";
            modify.Height = 23;
            modify.Width = buttonWidth;
            modify.VerticalAlignment = VerticalAlignment.Top;
            modify.HorizontalAlignment = HorizontalAlignment.Center;
            modify.Margin = new Thickness( 0 , add.Margin.Top , 0 , 15 );
            modify.Click += new RoutedEventHandler( modify_Click );
            modify.IsEnabled = false;
            inputBlock.Children.Add( modify );

            // Parent.Content.Children[5] = Delete button
            delete = new Button();
            delete.Content = "Delete";
            delete.Height = 23;
            delete.Width = buttonWidth;
            delete.VerticalAlignment = VerticalAlignment.Top;
            delete.HorizontalAlignment = HorizontalAlignment.Center;
            delete.Margin = new Thickness( buttonOffset , add.Margin.Top , 0 , 15 );
            delete.Click += new RoutedEventHandler( delete_Click );
            delete.IsEnabled = false;
            inputBlock.Children.Add( delete );



            /*
             * Create SlopeDefineCanvas object for drawing surface
             */

            inputCanvas = new SlopeDefineCanvas( canvas );
            inputCanvas.Background = Brushes.White;
            inputCanvas.VerticalAlignment = VerticalAlignment.Stretch;
            inputCanvas.HorizontalAlignment = HorizontalAlignment.Stretch;
            inputCanvas.Margin = new Thickness( axisWidth , 0 , inputAreaWidth , axisWidth + progressBarWidth );
            inputCanvas.InitializeCanvas();
            inputGrid.Children.Add( inputCanvas );
        }


        private void pan_Click ( object sender , RoutedEventArgs e )
        {
            Grid plottingGrid = contentGrid.Children[1] as Grid;

            SlopeDefineCanvas currCanvas = null;
            if ( plottingGrid != null )
            {
                currCanvas = plottingGrid.Children[3] as SlopeDefineCanvas;
            }

            if ( currCanvas != null )
            {
                currCanvas.CancelDrawing();
                currCanvas.Cursor = ((TextBlock) (((MainWindow) ((Grid) ((TabControl) ((TabItem) ((Grid) canvas.Parent).Parent).Parent).Parent).Parent).Resources["handCursor"])).Cursor;
                currCanvas.DrawMode = DrawModes.Pan;
            }
        }


        private void zoomIn_Click ( object sender , RoutedEventArgs e )
        {
            Grid plottingGrid = contentGrid.Children[1] as Grid;

            SlopeDefineCanvas currCanvas = null;
            if ( plottingGrid != null )
            {
                currCanvas = plottingGrid.Children[3] as SlopeDefineCanvas;
            }

            if ( currCanvas != null )
            {
                currCanvas.Zoom( 1.1 , new Point( 0.5 * currCanvas.ActualWidth , 0.5 * currCanvas.ActualHeight ) );
            }
        }

        private void zoomOut_Click ( object sender , RoutedEventArgs e )
        {
            Grid plottingGrid = contentGrid.Children[1] as Grid;

            SlopeDefineCanvas currCanvas = null;
            if ( plottingGrid != null )
            {
                currCanvas = plottingGrid.Children[3] as SlopeDefineCanvas;
            }

            if ( currCanvas != null )
            {
                currCanvas.Zoom( 1 / 1.1 , new Point( 0.5 * currCanvas.ActualWidth , 0.5 * currCanvas.ActualHeight ) );
            }
        }

        private void zoomArea_Click ( object sender , RoutedEventArgs e )
        {
            Grid plottingGrid = contentGrid.Children[1] as Grid;

            SlopeDefineCanvas currCanvas = null;
            if ( plottingGrid != null )
            {
                currCanvas = plottingGrid.Children[3] as SlopeDefineCanvas;
            }

            if ( currCanvas != null )
            {
                currCanvas.CancelDrawing();
                currCanvas.Cursor = ((TextBlock) (((MainWindow) ((Grid) ((TabControl) ((TabItem) ((Grid) canvas.Parent).Parent).Parent).Parent).Parent).Resources["zoomAreaCursor"])).Cursor;
                currCanvas.DrawMode = DrawModes.ZoomArea;
            }
        }

        private void zoomAll_Click ( object sender , RoutedEventArgs e )
        {
            Grid plottingGrid = contentGrid.Children[1] as Grid;

            SlopeDefineCanvas currCanvas = null;
            if ( plottingGrid != null )
            {
                currCanvas = plottingGrid.Children[3] as SlopeDefineCanvas;
            }

            if ( currCanvas != null )
            {
                currCanvas.CentreAndFitExtents( true );
            }
        }


        /*
         * INPUT AREA EVENT HANDLERS
         */

        private void phaseList_SelectionChanged ( object sender , SelectionChangedEventArgs e )
        {
            if ( phaseList.SelectedItem is AnalysisPhase )
            {
                add.IsEnabled = false;
                modify.IsEnabled = true;
                delete.IsEnabled = true;

                AnalysisPhase currPhase = phaseList.SelectedItem as AnalysisPhase;

                beginPhaseList.SelectedItem = currPhase.BeginPhase;
                resetDisplacements.IsChecked = currPhase.ResetDisplacements;

                loadSteps.Text = currPhase.NSteps.ToString();
                iterations.Text = currPhase.NIterations.ToString();
                printLines.Text = currPhase.NPrintLines.ToString();
                gravityFactor.Text = Math.Round( currPhase.GravityFactor , 2 ).ToString();

                int phase = currPhase.Number - 1;
                MaterialBlock currBlock , currSub;
                DrawingPoint currNode;
                LineConstraint currLC;
                for ( int i = 0 ; i < inputCanvas.Substructs.Count ; i++ )
                {
                    currBlock = canvas.MaterialBlocks[i];
                    currSub = inputCanvas.Substructs[i];

                    currSub.Material = currBlock.PhaseMaterials[phase];

                    for ( int j = 0 ; j < currSub.BoundaryPoints.Count ; j++ )
                    {
                        currNode = currSub.BoundaryPoints[j];
                        if ( currNode.IsFixedX ) currNode.IsFixActiveX = currBlock.BoundaryPoints[j].PhaseFixActiveX[phase];
                        if ( currNode.IsFixedY ) currNode.IsFixActiveY = currBlock.BoundaryPoints[j].PhaseFixActiveY[phase];
                    }

                    for ( int j = 0 ; j < currSub.LineConstraints.Count ; j++ )
                    {
                        currLC = currSub.LineConstraints[j];
                        if ( currLC.IsFixedX ) currLC.IsActiveX = currBlock.LineConstraints[j].PhaseFixedX[phase];
                        if ( currLC.IsFixedY ) currLC.IsActiveY = currBlock.LineConstraints[j].PhaseFixedY[phase];
                    }

                    for(int j=0;j
                }

                inputCanvas.ClearSelections();
            }
            else
            {
                if ( this.IsInitialized )
                {
                    add.IsEnabled = true;
                    modify.IsEnabled = false;
                    delete.IsEnabled = false;

                    beginPhaseList.SelectedItem = canvas.FEAPhases[0];
                    resetDisplacements.IsChecked = false;

                    loadSteps.Text = "";
                    iterations.Text = "";
                    printLines.Text = "";
                    gravityFactor.Text = "";

                    MaterialType nullMaterial = canvas.MaterialTypes[canvas.MaterialTypes.Count - 1];
                    inputCanvas.Substructs.ForEach(
                        delegate( MaterialBlock mb )
                        {
                            mb.Material = nullMaterial;
                            mb.BoundaryPoints.ForEach(
                                delegate( DrawingPoint p )
                                {
                                    p.IsFixActiveX = p.IsFixedX;
                                    p.IsFixActiveY = p.IsFixedY;
                                } );
                            mb.LineConstraints.ForEach(
                                delegate( LineConstraint lc )
                                {
                                    lc.IsActiveX = lc.IsFixedX;
                                    lc.IsActiveY = lc.IsFixedY;
                                } );
                        } );

                    inputCanvas.ClearSelections();
                }
            }
        }


        private void beginPhaseList_SelectionChanged ( object sender , SelectionChangedEventArgs e )
        {
            if ( beginPhaseList.SelectedItem is AnalysisPhase )
            {
                AnalysisPhase currPhase = beginPhaseList.SelectedItem as AnalysisPhase;

                int phase = currPhase.Number - 1;
                MaterialBlock currBlock , currSub;
                DrawingPoint currNode;
                LineConstraint currLC;
                if ( phase >= 0 )
                {
                    for ( int i = 0 ; i < inputCanvas.Substructs.Count ; i++ )
                    {
                        currBlock = canvas.MaterialBlocks[i];
                        currSub = inputCanvas.Substructs[i];

                        currSub.Material = currBlock.PhaseMaterials[phase];

                        for ( int j = 0 ; j < currSub.BoundaryPoints.Count ; j++ )
                        {
                            currNode = currSub.BoundaryPoints[j];
                            if ( currNode.IsFixedX ) currNode.IsFixActiveX = currBlock.BoundaryPoints[j].PhaseFixActiveX[phase];
                            if ( currNode.IsFixedY ) currNode.IsFixActiveY = currBlock.BoundaryPoints[j].PhaseFixActiveY[phase];
                        }

                        for ( int j = 0 ; j < currSub.LineConstraints.Count ; j++ )
                        {
                            currLC = currSub.LineConstraints[j];
                            if ( currLC.IsFixedX ) currLC.IsActiveX = currBlock.LineConstraints[j].PhaseFixedX[phase];
                            if ( currLC.IsFixedY ) currLC.IsActiveY = currBlock.LineConstraints[j].PhaseFixedY[phase];
                        }
                    }
                }
                else
                {
                    MaterialType nullMaterial = canvas.MaterialTypes[canvas.MaterialTypes.Count - 1];
                    inputCanvas.Substructs.ForEach(
                        delegate( MaterialBlock mb )
                        {
                            mb.Material = nullMaterial;
                            mb.BoundaryPoints.ForEach(
                                delegate( DrawingPoint p )
                                {
                                    p.IsFixActiveX = p.IsFixedX;
                                    p.IsFixActiveY = p.IsFixedY;
                                } );
                            mb.LineConstraints.ForEach(
                                delegate( LineConstraint lc )
                                {
                                    lc.IsActiveX = lc.IsFixedX;
                                    lc.IsActiveY = lc.IsFixedY;
                                } );
                        } );
                }

                inputCanvas.ClearSelections();
            }
        }

        private void materialList_SelectionChanged ( object sender , SelectionChangedEventArgs e )
        {
            selectedMaterial = materialList.SelectedItem as MaterialType;

            if ( selectedMaterial != null )
            {
                materialFill.Fill = selectedMaterial.Fill;
                phiValue.Text = String.Format( "{0}" , Math.Round( selectedMaterial.Phi , 2 ) );
                cohValue.Text = String.Format( "{0}" , Math.Round( selectedMaterial.Cohesion , 2 ) );
                psiValue.Text = String.Format( "{0}" , Math.Round( selectedMaterial.Psi , 2 ) );
                gammaValue.Text = String.Format( "{0}" , Math.Round( selectedMaterial.Gamma , 2 ) );
                emodValue.Text = String.Format( "{0}" , Math.Round( selectedMaterial.Emod , 2 ) );
                nuValue.Text = String.Format( "{0}" , Math.Round( selectedMaterial.Nu , 2 ) );
            }
        }

        private void setSelectedButton_Click ( object sender , RoutedEventArgs e )
        {
            if ( selectedMaterial == null )
            {
                MessageBox.Show( "Must select a material type." , "Error" );
                return;
            }

            if ( inputCanvas.Substructs.Find( delegate( MaterialBlock mb ) { return mb.IsSelected; } ) == null )
            {
                MessageBox.Show( "Must select at least one material block." , "Error" );
                return;
            }

            inputCanvas.Substructs.ForEach( delegate( MaterialBlock mb ) { if ( mb.IsSelected ) mb.Material = selectedMaterial; } );
        }

        private void add_Click ( object sender , RoutedEventArgs e )
        {
            if ( phaseList.Text == "Add new analysis phase..." || phaseList.Text == "" )
            {
                MessageBox.Show( "Must give the phase a name." , "Error" );
                return;
            }

            if ( phaseList.Text == "NULL" )
            {
                MessageBox.Show( "NULL phase name is reserved." , "Error" );
                return;
            }

            if ( !(beginPhaseList.SelectedItem is AnalysisPhase) )
            {
                MessageBox.Show( "Must select an analysis phase to begin from.\nSelect NULL phase for \"stress-free\" state." , "Error" );
                return;
            }

            int nsteps;
            if ( !int.TryParse( loadSteps.Text , out nsteps ) || nsteps < 1 )
            {
                MessageBox.Show( "Number of load steps must be an integer >= 1." , "Error" );
                return;
            }

            int niter;
            if ( !int.TryParse( iterations.Text , out niter ) || niter < 1 )
            {
                MessageBox.Show( "Number of iterations must be an integer >= 1." , "Error" );
                return;
            }

            int nprint;
            if ( !int.TryParse( printLines.Text , out nprint ) || nprint < 1 )
            {
                MessageBox.Show( "Number of iterations / print line must be an integer >= 1." , "Error" );
                return;
            }

            double gfact;
            if ( !double.TryParse( gravityFactor.Text , out gfact ) || gfact < 0 )
            {
                MessageBox.Show( "Gravity factor must be a positive value." , "Error" );
                return;
            }

            AnalysisPhase newPhase = new AnalysisPhase( canvas.FEAPhases.Count ,
                phaseList.Text ,
                beginPhaseList.SelectedItem as AnalysisPhase ,
                (bool) resetDisplacements.IsChecked ,
                nsteps , niter , nprint , gfact );

            canvas.FEAPhases.Add( newPhase );
            int numPhases = canvas.FEAPhases.Count - 1;

            MaterialType currMaterial;
            MaterialBlock currBlock , currSub;
            DrawingPoint currNode;
            LineConstraint currLC;
            PointLoad currPL;
            for ( int i = 0 ; i < inputCanvas.Substructs.Count ; i++ )
            {
                currSub = inputCanvas.Substructs[i];
                currBlock = canvas.MaterialBlocks[i];

                currMaterial = currSub.Material;
                currBlock.PhaseMaterials.Add( currMaterial );

                for ( int j = 0 ; j < currSub.BoundaryPoints.Count ; j++ )
                {
                    currNode = currBlock.BoundaryPoints[j];
                    if ( currNode.PhaseFixActiveX.Count < numPhases )
                    {
                        currNode.PhaseFixActiveX.Add( currSub.BoundaryPoints[j].IsFixActiveX );
                        currNode.PhaseFixActiveY.Add( currSub.BoundaryPoints[j].IsFixActiveY );
                    }
                }

                for ( int j = 0 ; j < currSub.LineConstraints.Count ; j++ )
                {
                    currLC = currBlock.LineConstraints[j];
                    if ( currLC.PhaseFixedX.Count < numPhases )
                    {
                        currLC.PhaseFixedX.Add( currSub.LineConstraints[j].IsActiveX );
                        currLC.PhaseFixedY.Add( currSub.LineConstraints[j].IsActiveY );
                    }
                }

                for ( int j = 0 ; j < currSub.PointLoads.Count ; j++ )
                {
                    currPL = currBlock.PointLoads[j];
                    if ( currPL.PhaseActiveX.Count < numPhases )
                    {
                        currPL.PhaseActiveX.Add( currSub.PointLoads[j].IsActiveX );
                        currPL.PhaseFactorX.Add( currSub.PointLoads[j].XFactor );
                        currPL.PhaseActiveY.Add( currSub.PointLoads[j].IsActiveY );
                        currPL.PhaseFactorY.Add( currSub.PointLoads[j].YFactor );
                    }
                }
            }

            beginPhaseList.Items.Clear();
            canvas.FEAPhases.ForEach( delegate( AnalysisPhase ap ) { beginPhaseList.Items.Add( ap ); } );

            phaseList.Items.Clear();
            phaseList.Items.Add( "Add new analysis phase..." );
            for ( int i = 1 ; i < canvas.FEAPhases.Count ; i++ )
            {
                phaseList.Items.Add( canvas.FEAPhases[i] );
            }
            phaseList.SelectedIndex = 0;
            phaseList.Focus();

            canvas.IsSaved = false;
        }

        private void modify_Click ( object sender , RoutedEventArgs e )
        {
        }

        private void delete_Click ( object sender , RoutedEventArgs e )
        {
        }
    }
}
