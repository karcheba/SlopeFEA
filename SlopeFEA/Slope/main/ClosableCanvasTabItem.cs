using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Windows.Controls;
using System.Windows.Controls.Primitives;
using System.Windows;
using System.Windows.Input;
using System.Windows.Media;
using SlopeFEA;
using System.Windows.Shapes;


class ClosableCanvasTabItem : TabItem
{
    /// <summary>
    /// Constructor
    /// </summary>
    public ClosableCanvasTabItem()
    {
        // Create the header UserControl
        ClosableHeader closableTabHeader = new ClosableHeader();

        // Assign user control to tab header
        this.Header = closableTabHeader;

        // Attach CloseableHeader button events
        closableTabHeader.tabClose.MouseEnter += new MouseEventHandler(tabClose_MouseEnter);
        closableTabHeader.tabClose.MouseLeave += new MouseEventHandler(tabClose_MouseLeave);
        closableTabHeader.tabClose.Click += new RoutedEventHandler(tabClose_Click);
        closableTabHeader.tabTitle.SizeChanged += new SizeChangedEventHandler(tabTitle_SizeChanged);

        // Create Grid to contain two axes and drawing canvas
        Grid contentGrid = new Grid();
        contentGrid.Background = Brushes.WhiteSmoke;
        contentGrid.Margin = new Thickness(0);
        this.Content = contentGrid;

        double axisWidth = 65;

        // Create Grid object for x axis
        Grid xAxis = new Grid();
        xAxis.ClipToBounds = true;
        xAxis.Background = Brushes.WhiteSmoke;
        xAxis.VerticalAlignment = VerticalAlignment.Bottom;
        xAxis.HorizontalAlignment = HorizontalAlignment.Stretch;
        xAxis.Margin = new Thickness(axisWidth, 0, 0, 10);
        xAxis.Height = axisWidth;
        contentGrid.Children.Add(xAxis);

        // Create Grid object for y axis
        Grid yAxis = new Grid();
        yAxis.ClipToBounds = true;
        yAxis.Background = Brushes.WhiteSmoke;
        yAxis.VerticalAlignment = VerticalAlignment.Stretch;
        yAxis.HorizontalAlignment = HorizontalAlignment.Left;
        yAxis.Margin = new Thickness(0, 0, 0, axisWidth + 10);
        yAxis.Width = axisWidth;
        contentGrid.Children.Add(yAxis);

        // Create SlopeCanvas object for drawing surface
        SlopeCanvas drawingCanvas = new SlopeCanvas();
        drawingCanvas.Background = Brushes.White;
        drawingCanvas.VerticalAlignment = VerticalAlignment.Stretch;
        drawingCanvas.HorizontalAlignment = HorizontalAlignment.Stretch;
        drawingCanvas.Margin = new Thickness(axisWidth, 0, 0, axisWidth + 10);
        drawingCanvas.InitializeCanvas();
        contentGrid.Children.Add(drawingCanvas);

        ProgressBar analysisProgress = new ProgressBar();
        analysisProgress.Height = 10;
        analysisProgress.VerticalAlignment = VerticalAlignment.Bottom;
        analysisProgress.HorizontalAlignment = HorizontalAlignment.Stretch;
        analysisProgress.Margin = new Thickness(0, 0, 0, 0);
        analysisProgress.Visibility = Visibility.Hidden;
        contentGrid.Children.Add(analysisProgress);

        Rectangle hideCanvasRect = new Rectangle();
        hideCanvasRect.Fill = Brushes.White;
        hideCanvasRect.Opacity = 0.5;
        hideCanvasRect.Stroke = Brushes.White;
        hideCanvasRect.VerticalAlignment = VerticalAlignment.Stretch;
        hideCanvasRect.HorizontalAlignment = HorizontalAlignment.Stretch;
        hideCanvasRect.Margin = new Thickness(axisWidth, 0, 0, axisWidth + 10);
        hideCanvasRect.Visibility = Visibility.Hidden;
        contentGrid.Children.Add(hideCanvasRect);
    }

    /// <summary>
    /// Property - Set header title
    /// </summary>
    public string Title
    {
        get
        {
            return ((ClosableHeader)this.Header).tabTitle.Content.ToString();
        }
        set
        {
            ((ClosableHeader)this.Header).tabTitle.Content = value;
        }
    }


    // ----------------------------------
    // OVERRIDES
    // ----------------------------------

    /// <summary>
    /// OnSelected - Make close button visible when tab is selected
    /// </summary>
    /// <param name="e"></param>
    protected override void OnSelected(RoutedEventArgs e)
    {
        base.OnSelected(e);
        ((ClosableHeader)this.Header).tabClose.Visibility = Visibility.Visible;

        SlopeCanvas canvas = (SlopeCanvas)((Grid)this.Content).Children[2];

        StatusBar status = (StatusBar)((DockPanel)((Grid)((TabControl)this.Parent).Parent).Children[1]).Children[0];
        StatusBarItem xStatus = (StatusBarItem)status.Items[0];
        StatusBarItem yStatus = (StatusBarItem)status.Items[1];
        StatusBarItem scaleStatus = (StatusBarItem)status.Items[2];

        xStatus.Content = "X";
        yStatus.Content = "Y";
        scaleStatus.Content = String.Format("Scale = {0} : 1", Math.Round(canvas.Scale, 2));

        Menu mainMenu = (Menu)((DockPanel)((Grid)((TabControl)this.Parent).Parent).Children[0]).Children[0];
            MenuItem fileMenu = (MenuItem)mainMenu.Items[0];
                MenuItem save = (MenuItem)fileMenu.Items[3];
                MenuItem saveAs = (MenuItem)fileMenu.Items[4];
                MenuItem saveAll = (MenuItem)fileMenu.Items[5];
                MenuItem close = (MenuItem)fileMenu.Items[7];
                MenuItem closeAll = (MenuItem)fileMenu.Items[8];

                save.IsEnabled = true;
                saveAs.IsEnabled = true;
                saveAll.IsEnabled = true;
                close.IsEnabled = true;
                closeAll.IsEnabled = true;

            MenuItem viewMenu = (MenuItem)mainMenu.Items[1];
            viewMenu.IsEnabled = true;
                MenuItem scaleMenu = (MenuItem)viewMenu.Items[7];
                MenuItem unitsMenu = (MenuItem)viewMenu.Items[8];
                MenuItem gridMenu = (MenuItem)viewMenu.Items[10];
            MenuItem defineMenu = (MenuItem)mainMenu.Items[2];
            defineMenu.IsEnabled = true;
            MenuItem analyzeMenu = (MenuItem)mainMenu.Items[3];
            analyzeMenu.IsEnabled = true;
                MenuItem runAnalysis = (MenuItem)analyzeMenu.Items[0];
                MenuItem stopAnalysis = (MenuItem)analyzeMenu.Items[1];
                MenuItem generateMesh = (MenuItem)analyzeMenu.Items[3];
                MenuItem clearMesh = (MenuItem)analyzeMenu.Items[4];
                MenuItem analysisType = (MenuItem)analyzeMenu.Items[8];
                    MenuItem stabilityAnalysis = (MenuItem)analysisType.Items[0];
                    MenuItem feaAnalysis = (MenuItem)analysisType.Items[1];

                runAnalysis.IsEnabled = canvas.IsMeshed && !canvas.IsAnalyzing;
                stopAnalysis.IsEnabled = canvas.IsAnalyzing;
                generateMesh.IsEnabled = canvas.IsVerified;
                clearMesh.IsEnabled = canvas.IsMeshed;

            MenuItem outputMenu = (MenuItem)mainMenu.Items[4];
            outputMenu.IsEnabled = canvas.IsAnalyzed;

        foreach(MenuItem scale in scaleMenu.Items)
        {
            if (scale.IsChecked)
                scale.IsChecked = false;
        }
        switch (canvas.ScaleType)
        {
            // 1000:1
            case Scales.sc1000:
                ((MenuItem)scaleMenu.Items[0]).IsChecked = true;
                break;

            // 800:1
            case Scales.sc800:
                ((MenuItem)scaleMenu.Items[1]).IsChecked = true;
                break;

            // 600:1
            case Scales.sc600:
                ((MenuItem)scaleMenu.Items[2]).IsChecked = true;
                break;

            // 500:1
            case Scales.sc500:
                ((MenuItem)scaleMenu.Items[3]).IsChecked = true;
                break;

            // 400:1
            case Scales.sc400:
                ((MenuItem)scaleMenu.Items[4]).IsChecked = true;
                break;

            // 300:1
            case Scales.sc300:
                ((MenuItem)scaleMenu.Items[5]).IsChecked = true;
                break;

            // 200:1
            case Scales.sc200:
                ((MenuItem)scaleMenu.Items[6]).IsChecked = true;
                break;

            // 150:1
            case Scales.sc150:
                ((MenuItem)scaleMenu.Items[7]).IsChecked = true;
                break;

            // 100:1
            case Scales.sc100:
                ((MenuItem)scaleMenu.Items[8]).IsChecked = true;
                break;

            // 50:1
            case Scales.sc50:
                ((MenuItem)scaleMenu.Items[9]).IsChecked = true;
                break;

            // 25:1
            case Scales.sc25:
                ((MenuItem)scaleMenu.Items[10]).IsChecked = true;
                break;

            // 10:1
            case Scales.sc10:
                ((MenuItem)scaleMenu.Items[11]).IsChecked = true;
                break;

            // 5:1
            case Scales.sc5:
                ((MenuItem)scaleMenu.Items[12]).IsChecked = true;
                break;

            // 2:1
            case Scales.sc2:
                ((MenuItem)scaleMenu.Items[13]).IsChecked = true;
                break;

            // 1:1
            case Scales.sc1:
                ((MenuItem)scaleMenu.Items[14]).IsChecked = true;
                break;

            // Custom
            default:
                ((MenuItem)scaleMenu.Items[15]).IsChecked = true;
                break;
        }

        for (int i = 0; i < analysisType.Items.Count; i++)
        {
            if (((MenuItem)analysisType.Items[i]).IsChecked)
                ((MenuItem)analysisType.Items[i]).IsChecked = false;
        }
        for (int i = 0; i < stabilityAnalysis.Items.Count; i++)
        {
            if (((MenuItem)stabilityAnalysis.Items[i]).IsChecked)
                ((MenuItem)stabilityAnalysis.Items[i]).IsChecked = false;
        }
        for (int i = 0; i < feaAnalysis.Items.Count; i++)
        {
            if (((MenuItem)feaAnalysis.Items[i]).IsChecked)
                ((MenuItem)feaAnalysis.Items[i]).IsChecked = false;
        }
        switch (canvas.AnalysisType)
        {
            case AnalysisType.Bishop:
                stabilityAnalysis.IsChecked = true;
                ((MenuItem)stabilityAnalysis.Items[0]).IsChecked = true;
                break;
            case AnalysisType.RFEM:
                stabilityAnalysis.IsChecked = true;
                ((MenuItem)stabilityAnalysis.Items[1]).IsChecked = true;
                break;
            case AnalysisType.FEA4NodedQuad:
                feaAnalysis.IsChecked = true;
                ((MenuItem)feaAnalysis.Items[0]).IsChecked = true;
                break;
            default:
                feaAnalysis.IsChecked = true;
                ((MenuItem)feaAnalysis.Items[1]).IsChecked = true;
                break;
        }

        foreach (MenuItem units in unitsMenu.Items)
        {
            if (units.IsChecked)
                units.IsChecked = false;
        }
        switch (canvas.Units)
        {
            case Units.Millimetres:
                ((MenuItem)unitsMenu.Items[1]).IsChecked = true;
                break;

            case Units.Feet:
                ((MenuItem)unitsMenu.Items[2]).IsChecked = true;
                break;

            case Units.Inches:
                ((MenuItem)unitsMenu.Items[3]).IsChecked = true;
                break;

            default:
                ((MenuItem)unitsMenu.Items[0]).IsChecked = true;
                break;
        }

        ((MenuItem)gridMenu.Items[0]).IsChecked = canvas.ShowMajorGrid;
        ((MenuItem)gridMenu.Items[1]).IsChecked = canvas.ShowMinorGrid;
        ((MenuItem)viewMenu.Items[11]).IsChecked = canvas.ShowMesh;
        ((MenuItem)viewMenu.Items[11]).IsEnabled = canvas.IsMeshed;
    }

    /// <summary>
    /// OnUnselected - Make close button hidden when tab is unselected
    /// </summary>
    /// <param name="e"></param>
    protected override void OnUnselected(RoutedEventArgs e)
    {
        base.OnUnselected(e);
        ((ClosableHeader)this.Header).tabClose.Visibility = Visibility.Hidden;
    }

    /// <summary>
    /// OnMouseEnter - Make close button visible when mouse enters tab
    /// </summary>
    /// <param name="e"></param>
    protected override void OnMouseEnter(MouseEventArgs e)
    {
        base.OnMouseEnter(e);
        ((ClosableHeader)this.Header).tabClose.Visibility = Visibility.Visible;
    }

    /// <summary>
    /// OnMouseLeave - Make close button hidden when mouse leaves tab (if tab is not selected)
    /// </summary>
    /// <param name="e"></param>
    protected override void OnMouseLeave(MouseEventArgs e)
    {
        base.OnMouseLeave(e);
        if (!this.IsSelected)
        {
            ((ClosableHeader)this.Header).tabClose.Visibility = Visibility.Hidden; 
        }
    }


    // ----------------------------------
    // EVENT HANDLERS
    // ----------------------------------

    /// <summary>
    /// Button MouseEnter - Change colour of X to red
    /// </summary>
    /// <param name="sender"></param>
    /// <param name="e"></param>
    private void tabClose_MouseEnter(object sender, MouseEventArgs e)
    {
        ((ClosableHeader)this.Header).tabClose.Foreground = Brushes.Red;
    }

    /// <summary>
    /// Button MouseLeave - Change colour of X to black
    /// </summary>
    /// <param name="sender"></param>
    /// <param name="e"></param>
    private void tabClose_MouseLeave(object sender, MouseEventArgs e)
    {
        ((ClosableHeader)this.Header).tabClose.Foreground = Brushes.Black;
    }

    /// <summary>
    /// Button Click - Raise an event indicating to the parent TabControl to remove the tab
    /// </summary>
    /// <param name="sender"></param>
    /// <param name="e"></param>
    private void tabClose_Click(object sender, RoutedEventArgs e)
    {
        StatusBar status = (StatusBar)((DockPanel)((Grid)((TabControl)this.Parent).Parent).Children[1]).Children[0];
        StatusBarItem xStatus = (StatusBarItem)status.Items[0];
        StatusBarItem yStatus = (StatusBarItem)status.Items[1];
        StatusBarItem scaleStatus = (StatusBarItem)status.Items[2];

        xStatus.Content = "X";
        yStatus.Content = "Y";
        scaleStatus.Content = "Scale";

        Menu mainMenu = (Menu)((DockPanel)((Grid)((TabControl)this.Parent).Parent).Children[0]).Children[0];
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
            viewMenu.IsEnabled = false;
                MenuItem showMesh = (MenuItem)viewMenu.Items[11];

                showMesh.IsEnabled = false;
                showMesh.IsChecked = false;

            MenuItem defineMenu = (MenuItem)mainMenu.Items[2];
            defineMenu.IsEnabled = false;

            MenuItem analyzeMenu = (MenuItem)mainMenu.Items[3];
            analyzeMenu.IsEnabled = false;
                MenuItem runAnalysis = (MenuItem)analyzeMenu.Items[0];
                MenuItem stopAnalysis = (MenuItem)analyzeMenu.Items[1];
                MenuItem generateMesh = (MenuItem)analyzeMenu.Items[3];
                MenuItem clearMesh = (MenuItem)analyzeMenu.Items[4];

                runAnalysis.IsEnabled = false;
                stopAnalysis.IsEnabled = false;
                generateMesh.IsEnabled = false;
                clearMesh.IsEnabled = false;

            MenuItem outputMenu = (MenuItem)mainMenu.Items[4];
            outputMenu.IsEnabled = false;

        ((TabControl)this.Parent).Items.Remove(this);
    }

    /// <summary>
    /// Label SizeChanged - Modify Margin property of Button
    /// </summary>
    /// <param name="sender"></param>
    /// <param name="?"></param>
    private void tabTitle_SizeChanged(object sender, SizeChangedEventArgs e)
    {
        ((ClosableHeader)this.Header).tabClose.Margin = new Thickness(((ClosableHeader)this.Header).tabTitle.ActualWidth + 5, 3, 4, 0);
    }
}
