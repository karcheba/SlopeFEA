using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Input;
using System.Windows.Media;
using System.Windows.Shapes;
using System.Windows.Controls.Primitives;
using System.IO;
using System.Threading;
using System.ComponentModel;
using Matrix;

namespace SlopeFEA
{
    partial class SlopeCanvas : Canvas
    {
        private static Random random = new Random();
        public delegate double EvaluateSafetyFactor(CircularSurface slipcircle, List<Point> surface, List<List<AnalysisMeshPoint>> mesh);

        private static double GenAlg(SlopeCanvas canvas, BackgroundWorker worker, DoWorkEventArgs e)
        {
            // Set the delegate function for evaluating the factor of safety
            EvaluateSafetyFactor evalSF = null;
            switch (canvas.AnalysisType)
            {
                case AnalysisType.Bishop: evalSF += Bishop; break;
                default: evalSF += RFEM; break;
            }

            // Get material data
            List<MaterialType> materialTypes = new List<MaterialType>();
            MaterialType copyMaterial;
            for (int i = 0; i < canvas.MaterialTypes.Count; i++)
            {
                copyMaterial = new MaterialType();
                copyMaterial.Phi = canvas.MaterialTypes[i].Phi;
                copyMaterial.Cohesion = canvas.MaterialTypes[i].Cohesion;
                copyMaterial.Gamma = canvas.MaterialTypes[i].Gamma;
                copyMaterial.Emod = canvas.MaterialTypes[i].Emod;
                copyMaterial.Nu = canvas.MaterialTypes[i].Nu;
                copyMaterial.Name = canvas.MaterialTypes[i].Name;

                if (canvas.AnalysisType == AnalysisType.RFEM) copyMaterial.ComputeRFEMProperties();

                materialTypes.Add(copyMaterial);
            }

            double angleFactor = Math.PI / 180.0;
            double lengthFactor;
            double cohFactor = 1.0, gammaFactor = 1.0;
            switch (canvas.Units)
            {
                case Units.Metres:
                    lengthFactor = 0.0254;
                    break;

                case Units.Millimetres:
                    lengthFactor = 25.4;
                    cohFactor /= Math.Pow(1000, 2);
                    gammaFactor /= Math.Pow(1000, 3);
                    break;

                case Units.Feet:
                    lengthFactor = 1.0 / 12.0;
                    cohFactor *= Math.Pow(12, 2);
                    break;

                default:
                    lengthFactor = 1.0;
                    gammaFactor /= Math.Pow(12, 3);
                    break;
            }

            for (int i = 0; i < materialTypes.Count; i++)
            {
                materialTypes[i].Phi *= angleFactor;
                materialTypes[i].Cohesion *= cohFactor;
                materialTypes[i].Gamma *= gammaFactor;
            }

            // Load upper surface and mesh
            string[] split = canvas.FilePath.Split('.');

            if (split[split.Length - 1] != "slp")
            {
                MessageBox.Show("Input file format incorrect. Input data must be in a *.slp file.", "Error");
                return 0;
            }
            
            split[split.Length - 1] = "msh";

            string path = string.Join(".", split);

            if (!File.Exists(path))
            {
                MessageBox.Show("Could not find mesh data file.", "Error");
                return 0;
            }

            Units meshUnits;
            List<Point> surface = new List<Point>();
            List<List<AnalysisMeshPoint>> mesh = new List<List<AnalysisMeshPoint>>();
            List<AnalysisMeshPoint> line;
            try
            {
                using (TextReader tr = new StreamReader(path))
                {
                    // Read in actual units
                    string units = tr.ReadLine().Split(new char[] { '=', ' ' }, StringSplitOptions.RemoveEmptyEntries)[1];
                    switch (units)
                    {
                        case "m":
                            meshUnits = Units.Metres;
                            break;
                        case "mm":
                            meshUnits = Units.Millimetres;
                            break;
                        case "ft":
                            meshUnits = Units.Feet;
                            break;
                        default:
                            meshUnits = Units.Inches;
                            break;
                    }

                    // Check that canvas and mesh files have same units
                    if (canvas.Units != meshUnits)
                    {
                        MessageBox.Show("Units mismatch. Mesh file must have same units as input file.", "Error");
                        return 0;
                    }

                    tr.ReadLine();

                    // Get number of upper surface points
                    int count = int.Parse(tr.ReadLine().Split('=')[1]);

                    tr.ReadLine();
                    tr.ReadLine();

                    // Read in units and add to surface point list
                    for (int i = 0; i < count; i++)
                    {
                        split = tr.ReadLine().Split(',');
                        surface.Add(new Point(double.Parse(split[0]), double.Parse(split[1])));
                    }

                    tr.ReadLine();

                    // Get number of mesh lines
                    count = int.Parse(tr.ReadLine().Split('=')[1]);

                    tr.ReadLine();

                    int pointCount;
                    string type, material;
                    MeshPointType mpType;
                    MaterialType matType = null;
                    for (int i = 0; i < count; i++)
                    {
                        // Create new list for points in current mesh line
                        line = new List<AnalysisMeshPoint>();

                        tr.ReadLine();

                        // Get number of points in current mesh line
                        pointCount = int.Parse(tr.ReadLine().Split('=')[1]);

                        for (int j = 0; j < pointCount; j++)
                        {
                            // Read data for current point
                            split = tr.ReadLine().Split(',');

                            // Get point type (entrance or exit from material block)
                            type = split[2].Split(new char[] { ' ' }, StringSplitOptions.RemoveEmptyEntries)[0];
                            switch (type)
                            {
                                case "Entrance": mpType = MeshPointType.Entrance; break;
                                default: mpType = MeshPointType.Exit; break;
                            }

                            // Get material type name and find the corresponding material from the list
                            material = split[3].Split('\"')[1];
                            matType = materialTypes.Find(delegate(MaterialType mt) { return mt.Name == material; });

                            // Add this point to the list of points for the current mesh line
                            line.Add(new AnalysisMeshPoint(new Point(double.Parse(split[0]), double.Parse(split[1])),
                                                            matType, mpType));
                        }

                        mesh.Add(line);

                        tr.ReadLine();
                    }
                }
            }
            catch
            {
                MessageBox.Show("Error in reading mesh file. Make sure it is formatted correctly.", "Error");
                return 0;
            }

            // Get genetic algorithm parameters
            int population = canvas.GeneticAlgorithmParameters.Population;
            int generations = canvas.GeneticAlgorithmParameters.Generations;
            double fittestProp = canvas.GeneticAlgorithmParameters.FittestProportion;
            double matingProp = canvas.GeneticAlgorithmParameters.MatingPoolProportion;
            double crossProb = canvas.GeneticAlgorithmParameters.CrossoverProbability;
            double mutProb = canvas.GeneticAlgorithmParameters.MutationProbability;

            // For reporting progress
            int numberOfEvaluations = population * generations;

            // Compute additional genetic algorithm parameters
            int fittest = (int)Math.Ceiling(fittestProp * population);
            int parents = population - fittest;
            fittest += parents % 2;
            parents = population - fittest;
            int mating = (int)Math.Ceiling(matingProp * population);
            int dof = 3;
            int mutations = (int)Math.Ceiling(mutProb * (population - fittest) * dof);

            List<double> radiusBreaks = FindRadiusBreaks(surface);

            double yBoundMin = canvas.Boundary.YMin;
            yBoundMin = ((canvas.ActualHeight - yBoundMin) - canvas.OriginOffsetY) / canvas.DpiY * lengthFactor * canvas.Scale;
            
            SoilMovement soilDirection = canvas.Boundary.SoilDirection;

            List<CircularSurface> currentSolutions = new List<CircularSurface>();
            List<CircularSurface> masterSolutions = new List<CircularSurface>();
            List<CircularSurface> matingPool = new List<CircularSurface>();
            List<double> parentSelectors = new List<double>();

            // Generate initial random population of failure surfaces
            while (currentSolutions.Count < population)
            {
                currentSolutions.Insert(0, new CircularSurface(surface, yBoundMin, radiusBreaks, soilDirection));
                currentSolutions[0].GenerateSurface();
            }

            bool foundTwin;
            double xdiff, ydiff, rdiff;
            double toler = 1e-5;
            int progressCount = 1;
            int percentComplete;
            double upperSF, totalWeight;
            for (int igen = 0; igen < generations; igen++)
            {
                for (int isoln = 0; isoln < currentSolutions.Count; isoln++)
                {
                    // Check if analysis has been cancelled
                    if (worker.CancellationPending)
                    {
                        e.Cancel = true;
                        return 0;
                    }

                    foundTwin = false;

                    // Check master solution list for existing solution
                    for (int imaster = 0; imaster < masterSolutions.Count; imaster++)
                    {
                        // Determine if (x,y,r) parameters are all within
                        // tolerance of a solution from the master list
                        xdiff = Math.Abs(currentSolutions[isoln].X - masterSolutions[imaster].X);
                        if (xdiff > toler) continue;

                        ydiff = Math.Abs(currentSolutions[isoln].Y - masterSolutions[imaster].Y);
                        if (ydiff > toler) continue;

                        rdiff = Math.Abs(currentSolutions[isoln].R - masterSolutions[imaster].R);
                        if (rdiff > toler) continue;

                        currentSolutions[isoln].SF = masterSolutions[imaster].SF;
                        currentSolutions[isoln].XEnter = masterSolutions[imaster].XEnter;
                        currentSolutions[isoln].XExit = masterSolutions[imaster].XExit;
                        currentSolutions[isoln].YEnter = masterSolutions[imaster].YEnter;
                        currentSolutions[isoln].YExit = masterSolutions[imaster].YExit;

                        foundTwin = true;
                    }

                    // If solution found in master list, do not evaluate
                    if (foundTwin)
                    {
                        // Update progress bar
                        percentComplete = (int)((float)progressCount++ / (float)numberOfEvaluations * (float)100);
                        worker.ReportProgress(percentComplete);
                        continue;
                    }

                    // Evaluate safety factor using the appropriate method
                    currentSolutions[isoln].SF = evalSF(currentSolutions[isoln], surface, mesh);

                    // Add new surface to master list
                    masterSolutions.Add(new CircularSurface(surface, yBoundMin, radiusBreaks, soilDirection,
                        currentSolutions[isoln].X, currentSolutions[isoln].Y, currentSolutions[isoln].R,
                        currentSolutions[isoln].Limits, currentSolutions[isoln].SF,
                        currentSolutions[isoln].XEnter, currentSolutions[isoln].YEnter,
                        currentSolutions[isoln].XExit, currentSolutions[isoln].YExit));

                    percentComplete = (int)((float)progressCount++ / (float)numberOfEvaluations * (float)100);
                    worker.ReportProgress(percentComplete);
                }

                // Sort the master solution list
                masterSolutions.Sort(SortSolutions);

                // Only keep top 20 master solutions
                while (masterSolutions.Count > 20) masterSolutions.RemoveAt(masterSolutions.Count - 1);

                // Begin mating cycle on all but the last generation
                if (igen < generations)
                {
                    // Sort the current solution list
                    currentSolutions.Sort(SortSolutions);

                    // Clear previous mating pool
                    matingPool.Clear();

                    // Take specified proportion into mating pool
                    while (matingPool.Count < mating)
                    {
                        matingPool.Add(currentSolutions[0]);
                        currentSolutions.RemoveAt(0);
                    }

                    // --------------------------------------
                    // Reproduction
                    // --------------------------------------

                    // Get value of SF @ mating+1
                    // (for producing weighting scheme)
                    upperSF = currentSolutions[0].SF;
                    
                    // Clear current solution list
                    currentSolutions.Clear();

                    // Reverse SF weighting (minimization)
                    // and sum up SF weights
                    totalWeight = 0;
                    for (int imate = 0; imate < mating; imate++)
                    {
                        totalWeight += matingPool[imate].SFWeight = matingPool[imate].SF - upperSF;
                    }

                    // Divide each SF by sum of SFs to get individual weight,
                    // and add sum of previous weights to obtain
                    // cumulative weighting scheme
                    matingPool[0].SFWeight /= totalWeight;
                    for (int imate = 1; imate < mating - 1; imate++)
                    {
                        matingPool[imate].SFWeight /= totalWeight;
                        matingPool[imate].SFWeight += matingPool[imate - 1].SFWeight;
                    }
                    matingPool[mating - 1].SFWeight = 1.0;

                    // Generate random numbers for selecting parents
                    parentSelectors.Clear();
                    while (parentSelectors.Count < parents) parentSelectors.Add(random.NextDouble());

                    // Add "fittest" solutions to front of solution list
                    for (int ifit = 0; ifit < fittest; ifit++)
                    {
                        currentSolutions.Add(new CircularSurface(surface, yBoundMin, radiusBreaks, soilDirection,
                            matingPool[ifit].X, matingPool[ifit].Y, matingPool[ifit].R,
                            matingPool[ifit].Limits));
                    }

                    // Select random parents based on weighting scheme
                    int iparent = 0;
                    while (currentSolutions.Count < population)
                    {
                        for (int imate = 0; imate < mating; imate++)
                        {
                            if (parentSelectors[iparent] <= matingPool[imate].SFWeight)
                            {
                                currentSolutions.Add(new CircularSurface(surface, yBoundMin, radiusBreaks, soilDirection,
                                    matingPool[imate].X, matingPool[imate].Y, matingPool[imate].R,
                                    matingPool[imate].Limits));

                                break;
                            }
                        }

                        iparent++;
                    }

                    int interpt, crosspt;
                    double crosscoeff;
                    double mother, father;
                    // Loop through parents performing crossover
                    // (beginning from one past the fittest)
                    for (int ichild = fittest; ichild < population; ichild += 2)
                    {
                        // Check if crossover is to occur
                        if (random.NextDouble() <= crossProb)
                        {
                            // Randomly select X, Y, or R for interpolation
                            interpt = (int)(random.NextDouble() * dof);

                            // Randomly select another point for crossover
                            crosspt = (int)(random.NextDouble() * (dof - 1));

                            // Randomly generate crossover coefficient
                            crosscoeff = random.NextDouble();

                            switch(interpt)
                            {
                                // Interpolation on X
                                case 0:
                                    mother = currentSolutions[ichild].X;
                                    father = currentSolutions[ichild + 1].X;

                                    currentSolutions[ichild].X = mother - crosscoeff * (mother - father);
                                    currentSolutions[ichild + 1].X = father + crosscoeff * (mother - father);

                                    switch (crosspt)
                                    {
                                        // Crossover on Y
                                        case 0:
                                            mother = currentSolutions[ichild].Y;
                                            father = currentSolutions[ichild + 1].Y;

                                            currentSolutions[ichild].Y = father;
                                            currentSolutions[ichild + 1].Y = mother;
                                            break;

                                        // Crossover on R
                                        default:
                                            mother = currentSolutions[ichild].R;
                                            father = currentSolutions[ichild + 1].R;

                                            currentSolutions[ichild].R = father;
                                            currentSolutions[ichild + 1].R = mother;
                                            break;
                                    }
                                    break;

                                // Interpolation on Y
                                case 1:
                                    mother = currentSolutions[ichild].Y;
                                    father = currentSolutions[ichild + 1].Y;

                                    currentSolutions[ichild].Y = mother - crosscoeff * (mother - father);
                                    currentSolutions[ichild + 1].Y = father + crosscoeff * (mother - father);

                                    switch (crosspt)
                                    {
                                        // Crossover on X
                                        case 0:
                                            mother = currentSolutions[ichild].X;
                                            father = currentSolutions[ichild + 1].X;

                                            currentSolutions[ichild].X = father;
                                            currentSolutions[ichild + 1].X = mother;
                                            break;

                                        // Crossover on R
                                        default:
                                            mother = currentSolutions[ichild].R;
                                            father = currentSolutions[ichild + 1].R;

                                            currentSolutions[ichild].R = father;
                                            currentSolutions[ichild + 1].R = mother;
                                            break;
                                    }
                                    break;

                                // Interpolation on R
                                default:
                                    mother = currentSolutions[ichild].R;
                                    father = currentSolutions[ichild + 1].R;

                                    currentSolutions[ichild].R = mother - crosscoeff * (mother - father);
                                    currentSolutions[ichild + 1].R = father + crosscoeff * (mother - father);

                                    switch (crosspt)
                                    {
                                        // Crossover on X
                                        case 0:
                                            mother = currentSolutions[ichild].X;
                                            father = currentSolutions[ichild + 1].X;

                                            currentSolutions[ichild].X = father;
                                            currentSolutions[ichild + 1].X = mother;
                                            break;

                                        // Crossover on Y
                                        default:
                                            mother = currentSolutions[ichild].Y;
                                            father = currentSolutions[ichild + 1].Y;

                                            currentSolutions[ichild].Y = father;
                                            currentSolutions[ichild + 1].Y = mother;
                                            break;
                                    }
                                    break;
                            }
                        }
                    }

                    // Loop through population performing mutations
                    int mutchild, mutpt;
                    for (int imut = 0; imut < mutations; imut++)
                    {
                        // Select child and parameter randomly
                        mutchild = (int)(random.NextDouble() * parents) + fittest;
                        mutpt = (int)(random.NextDouble() * dof);

                        switch (mutpt)
                        {
                            case 0: currentSolutions[mutchild].GenerateX(); break;
                            case 1: currentSolutions[mutchild].GenerateY(); break;
                            default: currentSolutions[mutchild].GenerateR(); break;
                        }
                    }
                }
            }

            // Maintain top 10 solutions
            masterSolutions.Sort(SortSolutions);
            while (masterSolutions.Count > 10) masterSolutions.RemoveAt(masterSolutions.Count - 1);
            masterSolutions.Sort(SortSolutions);

            // ----------------------------
            // OUTPUT FILE GENERATION
            // ----------------------------
            // Read back existing results (if any), print them to output and add this run's output to end.
            // Save the minimum case thus far immediately after geometry data.

            // Update the file path
            split = canvas.FilePath.Split('.');
            switch (canvas.AnalysisType)
            {
                case AnalysisType.Bishop: split[split.Length - 1] = "bish"; break;
                default: split[split.Length - 1] = "rfem"; break;
            }
            path = string.Join(".", split);

            // MODIFY THE EXISTING RESULTS FILE
            if (File.Exists(path))
            {
                // Read in all existing output file contents
                List<string> contents = new List<string>(File.ReadAllLines(path));

                // Find line with number of runs, increment run count, and update this line
                int iruncount = contents.FindIndex(delegate(string s) { return s.Contains("Number of Runs = "); });
                int runs = int.Parse(contents[iruncount].Split('=')[1]);
                runs++;
                contents[iruncount] = String.Format("Number of Runs = {0}", runs);

                // Find region with critical surface information and obtain its Fs
                int icrit = contents.FindIndex(delegate(string s) { return s.Contains("MOST CRITICAL SURFACE"); });
                double prevSF = double.Parse(contents[icrit + 9].Split('=')[1]);

                // If min Fs from current run is less than all previous runs, update this section
                if (masterSolutions[0].SF < prevSF)
                {
                    contents[icrit + 2] = String.Format("X_centre =\t{0}", Math.Round(masterSolutions[0].X, 2));
                    contents[icrit + 3] = String.Format("Y_centre =\t{0}", Math.Round(masterSolutions[0].Y, 2));
                    contents[icrit + 4] = String.Format("R =\t\t{0}", Math.Round(masterSolutions[0].R, 2));
                    contents[icrit + 5] = String.Format("X_enter =\t{0}", Math.Round(masterSolutions[0].XEnter, 2));
                    contents[icrit + 6] = String.Format("Y_enter =\t{0}", Math.Round(masterSolutions[0].YEnter, 2));
                    contents[icrit + 7] = String.Format("X_exit =\t{0}", Math.Round(masterSolutions[0].XExit, 2));
                    contents[icrit + 8] = String.Format("Y_exit =\t{0}", Math.Round(masterSolutions[0].YExit, 2));
                    contents[icrit + 9] = String.Format("Fs =\t\t{0}", Math.Round(masterSolutions[0].SF, 3));
                    contents[icrit + 10] = String.Format("Run No. =\t{0}", runs);
                }

                // Write all contents back to file
                File.WriteAllLines(path, contents);

                // Append current run results to end of file
                using (TextWriter tw = File.AppendText(path))
                {
                    tw.WriteLine();
                    tw.WriteLine("Run #{0}", runs);
                    tw.WriteLine("{0}", DateTime.Now);
                    tw.WriteLine("X_centre\tY_centre\tR\t\tX_enter\t\tY_enter\t\tX_exit\t\tY_exit\t\tFs");

                    for (int i = 0; i < masterSolutions.Count; i++)
                    {
                        tw.WriteLine("{0}\t\t{1}\t\t{2}\t\t{3}\t\t{4}\t\t{5}\t\t{6}\t\t{7}\t\t",
                            Math.Round(masterSolutions[i].X, 2), Math.Round(masterSolutions[i].Y, 2), Math.Round(masterSolutions[i].R, 2),
                            Math.Round(masterSolutions[i].XEnter, 2), Math.Round(masterSolutions[i].YEnter, 2),
                            Math.Round(masterSolutions[i].XExit, 2), Math.Round(masterSolutions[i].YExit, 2),
                            Math.Round(masterSolutions[i].SF, 3));
                    }
                }
            }
            
            // CREATE A NEW RESULTS FILE
            else
            {
                using (TextWriter tw = new StreamWriter(path))
                {
                    tw.WriteLine("*****************************************");
                    tw.WriteLine();
                    tw.WriteLine("        GENETIC ALGORITHM OUTPUT");
                    tw.WriteLine();
                    switch (canvas.AnalysisType)
                    {
                        case AnalysisType.Bishop: tw.WriteLine("    ANALYSIS METHOD: Bishop's Method"); break;
                        default: tw.WriteLine("         ANALYSIS METHOD: RFEM"); break;
                    }
                    tw.WriteLine();
                    tw.WriteLine("*****************************************");
                    tw.WriteLine();

                    string units;
                    switch (canvas.Units)
                    {
                        case Units.Metres: units = "m, kPa, kN/m^3"; break;
                        case Units.Millimetres: units = "mm, kPa, kN/m^3"; break;
                        case Units.Feet: units = "ft, psi, pcf"; break;
                        default: units = "in, psi, pcf"; break;
                    }

                    tw.WriteLine("Units = {0}", units);
                    tw.WriteLine();
                    tw.WriteLine();
                    tw.WriteLine("UPPER SURFACE GEOMETRY");
                    tw.WriteLine("--------------------------");
                    tw.WriteLine("Number of Points = {0}", surface.Count);
                    tw.WriteLine();
                    tw.WriteLine("(X,Y)");

                    for (int i = 0; i < surface.Count; i++)
                        tw.WriteLine("{0}, {1}", Math.Round(surface[i].X, 2), Math.Round(surface[i].Y, 2));

                    tw.WriteLine();
                    tw.WriteLine();
                    tw.WriteLine("MATERIAL DATA");
                    tw.WriteLine("--------------------------");
                    tw.WriteLine("Number of Material Types = {0}", canvas.MaterialTypes.Count);
                    tw.WriteLine();

                    for (int i = 0; i < canvas.MaterialTypes.Count; i++)
                    {
                        tw.WriteLine("Material #{0}", i + 1);
                        tw.WriteLine("Name = \"{0}\"", canvas.MaterialTypes[i].Name);
                        tw.WriteLine("Phi = {0}", Math.Round(canvas.MaterialTypes[i].Phi, 2));
                        tw.WriteLine("Cohesion = {0}", Math.Round(canvas.MaterialTypes[i].Cohesion, 2));
                        tw.WriteLine("Unit Weight = {0}", Math.Round(canvas.MaterialTypes[i].Gamma, 2));
                        tw.WriteLine();
                    }

                    tw.WriteLine();
                    tw.WriteLine("MATERIAL BLOCK GEOMETRY");
                    tw.WriteLine("--------------------------");
                    tw.WriteLine("Number of Material Blocks = {0}", canvas.MaterialBlocks.Count);
                    tw.WriteLine();

                    Point p;
                    double xCoord, yCoord;

                    for (int i = 0; i < canvas.MaterialBlocks.Count; i++)
                    {
                        tw.WriteLine("MB{0}", i + 1);
                        tw.WriteLine("Material Type = \"{0}\"", canvas.MaterialBlocks[i].Material);
                        tw.WriteLine("Number of Points = {0}", canvas.MaterialBlocks[i].BoundaryPoints.Count);
                        tw.WriteLine("(X,Y)");

                        for (int j = 0; j < canvas.MaterialBlocks[i].BoundaryPoints.Count; j++)
                        {
                            p = canvas.MaterialBlocks[i].BoundaryPoints[j].Point;

                            xCoord = (p.X - canvas.OriginOffsetX) / canvas.DpiX * lengthFactor * canvas.Scale;
                            yCoord = (canvas.ActualHeight - p.Y - canvas.OriginOffsetY) / canvas.DpiY * lengthFactor * canvas.Scale;

                            tw.WriteLine("{0}, {1}", Math.Round(xCoord, 2), Math.Round(yCoord, 2));
                        }

                        tw.WriteLine();
                    }

                    tw.WriteLine();
                    tw.WriteLine("MOST CRITICAL SURFACE");
                    tw.WriteLine("--------------------------");
                    tw.WriteLine("X_centre =\t{0}", Math.Round(masterSolutions[0].X, 2));
                    tw.WriteLine("Y_centre =\t{0}", Math.Round(masterSolutions[0].Y, 2));
                    tw.WriteLine("R =\t\t{0}", Math.Round(masterSolutions[0].R, 2));
                    tw.WriteLine("X_enter =\t{0}", Math.Round(masterSolutions[0].XEnter, 2));
                    tw.WriteLine("Y_enter =\t{0}", Math.Round(masterSolutions[0].YEnter, 2));
                    tw.WriteLine("X_exit =\t{0}", Math.Round(masterSolutions[0].XExit, 2));
                    tw.WriteLine("Y_exit =\t{0}", Math.Round(masterSolutions[0].YExit, 2));
                    tw.WriteLine("Fs =\t\t{0}", Math.Round(masterSolutions[0].SF, 3));
                    tw.WriteLine("Run No. =\t{0}", 1);       // this is the first run

                    tw.WriteLine();
                    tw.WriteLine();
                    tw.WriteLine("ADDITIONAL RUN OUTPUT");
                    tw.WriteLine("--------------------------");
                    tw.WriteLine("Number of Runs = {0}", 1); // this is the first run
                    tw.WriteLine();
                    tw.WriteLine("Run #{0}", 1);             // this is the first run
                    tw.WriteLine("{0}", DateTime.Now);

                    tw.WriteLine("X_centre\tY_centre\tR\t\tX_enter\t\tY_enter\t\tX_exit\t\tY_exit\t\tFs");

                    for (int i = 0; i < masterSolutions.Count; i++)
                    {
                        tw.WriteLine("{0}\t\t{1}\t\t{2}\t\t{3}\t\t{4}\t\t{5}\t\t{6}\t\t{7}\t\t",
                            Math.Round(masterSolutions[i].X, 2), Math.Round(masterSolutions[i].Y, 2), Math.Round(masterSolutions[i].R, 2),
                            Math.Round(masterSolutions[i].XEnter, 2), Math.Round(masterSolutions[i].YEnter, 2),
                            Math.Round(masterSolutions[i].XExit, 2), Math.Round(masterSolutions[i].YExit, 2),
                            Math.Round(masterSolutions[i].SF, 3));
                    }
                }
            }

            return masterSolutions[0].SF;
        }

        private static List<double> FindRadiusBreaks(List<Point> surface)
        {
            double  xMax, x1, y1, x2, y2,
                    currSlope = 0, prevSlope,
                    toler = 1e-5;
            Point   currPoint, prevPoint;

            List<double> result = new List<double>();

            // Get maximum x coord
            xMax = surface[surface.Count - 1].X;

            // Initialize surface point and iterator
            currPoint = surface[0];
            int iPoint = 0;

            // Add y coord of first point to break list
            result.Add(currPoint.Y);

            // Initialize x counting variable
            // (for stepping through surface)
            x2 = currPoint.X;

            while (x2 < xMax)
            {
                prevPoint = currPoint;
                currPoint = surface[++iPoint];

                x1 = prevPoint.X;
                y1 = prevPoint.Y;
                x2 = currPoint.X;
                y2 = currPoint.Y;

                prevSlope = currSlope;

                // Check if line is vertical
                if (Math.Abs(x2 - x1) < toler)
                {
                    // If soil movement is right-to-left
                    if (y2 > y1)
                    {
                        // Use max double value
                        currSlope = double.PositiveInfinity;
                    }
                    // Otherwise, if soil movement is left-to-right
                    else
                    {
                        currSlope = double.NegativeInfinity;
                    }
                }
                // Otherwise, compute slope as rise/run
                else currSlope = (y2 - y1) / (x2 - x1);

                // If surface is convex at vertex of curr and prev lines
                if (currSlope < prevSlope) result.Add(y1);
                else if (x2 == xMax) result.Add(y2);
            }

            result.Sort();
            return result.Distinct().ToList();
        }

        private static int SortSolutions(CircularSurface cs1,CircularSurface cs2)
        {
            // If cs1 is null...
            if (cs1 == null)
            {
                // ...and cs2 is also null...
                if (cs2 == null)
                {
                    // ...cs1 == cs2
                    return 0;
                }

                // ...and cs2 is not null...
                else
                {
                    // ...cs2 > cs1
                    return -1;
                }
            }

            // If cs1 is not null...
            else
            {
                // ...and cs2 is null...
                if (cs2 == null)
                {
                    // ...cs1 > cs2
                    return 1;
                }

                // ...and cs2 is also not null...
                else
                {
                    // ...compare SF of surfaces
                    if (cs1.SF > cs2.SF) return 1;
                    else if (cs1.SF < cs2.SF) return -1;
                    else return 0;
                }
            }
        }


        // ------------------------------------------------------------------
        // Bishop's Method
        // ------------------------------------------------------------------

        private static double Bishop(CircularSurface slipcircle, List<Point> surface, List<List<AnalysisMeshPoint>> mesh)
        {
            double  toler = 1e-5,
                    x0, y0, x1, y1, m, c,
                    xc, xcSq, yc, ycSq, r, rSq,
                    A, B, C, disc, sqrtDisc,
                    v, w,
                    yTop0, yBot0, yTop1, yBot1,
                    xEnter, yEnter, xExit, yExit,
                    meanWeight0, meanWeight1,
                    bishFactor, resist, applied, prevSF, currSF, errSF;

            List<double> xEnterExit = new List<double>();
            List<double> yEnterExit = new List<double>();

            List<double> alpha = new List<double>();
            List<double> width = new List<double>();
            List<double> weight = new List<double>();
            List<double> phi = new List<double>();
            List<double> coh = new List<double>();

            // Initialize SF
            double result = 1000.0;
            int iter = 0;

            // Get circular surface parameters
            xc = slipcircle.X;
            yc = slipcircle.Y;
            r = slipcircle.R;

            xcSq = Math.Pow(xc, 2);
            ycSq = Math.Pow(yc, 2);
            rSq = Math.Pow(r, 2);

            double direction = slipcircle.SoilDirection == SoilMovement.LtoR ? -1.0 : 1.0;

            // Get entry and exit points for failure surface
            for (int i = 0; i < surface.Count - 1; i++)
            {
                // Get coords of current surface line segment
                x0 = surface[i].X;
                y0 = surface[i].Y;
                x1 = surface[i + 1].X;
                y1 = surface[i + 1].Y;

                // If line is vertical
                if (Math.Abs(x0 - x1) < toler)
                {
                    v = x0;

                    A = 1.0;
                    B = -2 * yc;
                    C = Math.Pow(v - xc, 2) + ycSq - rSq;

                    disc = Math.Pow(B, 2) - 4 * A * C;
                    if (disc < 0) continue;
                    sqrtDisc = Math.Sqrt(disc);

                    w = (-B - sqrtDisc) / (2 * A);

                    if (w >= Math.Min(y0, y1) && w <= Math.Max(y0, y1))
                    {
                        xEnterExit.Add(v);
                        yEnterExit.Add(w);
                    }

                    w = (-B + sqrtDisc) / (2 * A);

                    if (w >= Math.Min(y0, y1) && w <= Math.Max(y0, y1))
                    {
                        xEnterExit.Add(v);
                        yEnterExit.Add(w);
                    }
                }
                // If line is horizontal
                else if (Math.Abs(y0 - y1) < toler)
                {
                    w = y0;

                    A = 1.0;
                    B = -2 * xc;
                    C = Math.Pow(w - yc, 2) + xcSq - rSq;

                    disc = Math.Pow(B, 2) - 4 * A * C;
                    if (disc < 0) continue;
                    sqrtDisc = Math.Sqrt(disc);

                    v = (-B - sqrtDisc) / (2 * A);

                    if (v >= Math.Min(x0, x1) && v <= Math.Max(x0, x1))
                    {
                        xEnterExit.Add(v);
                        yEnterExit.Add(w);
                    }

                    v = (-B + sqrtDisc) / (2 * A);

                    if (v >= Math.Min(x0, x1) && v <= Math.Max(x0, x1))
                    {
                        xEnterExit.Add(v);
                        yEnterExit.Add(w);
                    }
                }
                // Otherwise, compute slope and y-intercept
                else
                {
                    m = (y1 - y0) / (x1 - x0);
                    c = y0 - m * x0;

                    A = 1 + Math.Pow(m, 2);
                    B = 2 * (m * (c - yc) - xc);
                    C = Math.Pow(c - yc, 2) + xcSq - rSq;

                    disc = Math.Pow(B, 2) - 4 * A * C;
                    if (disc < 0) continue;
                    sqrtDisc = Math.Sqrt(disc);

                    v = (-B - sqrtDisc) / (2 * A);

                    if (v >= Math.Min(x0, x1) && v <= Math.Max(x0, x1))
                    {
                        w = m * v + c;

                        xEnterExit.Add(v);
                        yEnterExit.Add(w);
                    }

                    v = (-B + sqrtDisc) / (2 * A);

                    if (v >= Math.Min(x0, x1) && v <= Math.Max(x0, x1))
                    {
                        w = m * v + c;

                        xEnterExit.Add(v);
                        yEnterExit.Add(w);
                    }
                }
            }

            while (xEnterExit.Count >= 2)
            {
                // Clear entries for previous potential surface
                alpha.Clear();
                width.Clear();
                weight.Clear();
                phi.Clear();
                coh.Clear();

                // Get entry and exit coords of slip surface
                xEnter = xEnterExit[0];
                yEnter = yEnterExit[0];
                xEnterExit.RemoveAt(0);
                yEnterExit.RemoveAt(0);

                xExit = xEnterExit[0];
                yExit = yEnterExit[0];
                xEnterExit.RemoveAt(0);
                yEnterExit.RemoveAt(0);

                if (Math.Abs(xExit - xEnter) < mesh[1][0].X - mesh[0][0].X) continue;

                // Advance to mesh in region of interest
                int imesh = 0;
                while (mesh[imesh][0].X < xEnter) imesh++;

                // Initialize x and y coords and moments
                x0 = xEnter;
                yTop0 = yEnter;
                yBot0 = yEnter;
                meanWeight0 = 0;
                applied = 0;

                // Step through mesh, getting geometry and applied moments
                double yUpper, yLower, yHeight;
                int ibase;
                while (mesh[imesh][0].X < xExit)
                {
                    x1 = mesh[imesh][0].X;
                    yTop1 = mesh[imesh][mesh[imesh].Count - 1].Y;

                    // Compute y coord of bottom of right side of slice
                    A = 1.0;
                    B = -2 * yc;
                    C = Math.Pow(x1 - xc, 2) + ycSq - rSq;

                    sqrtDisc = Math.Sqrt(Math.Pow(B, 2) - 4 * A * C);

                    yBot1 = (-B - sqrtDisc) / (2 * A);

                    // Compute slice properties
                    alpha.Add(Math.Atan((yBot1 - yBot0) / (x1 - x0)));
                    width.Add(x1 - x0);

                    // Initialize base parameters to zero
                    phi.Add(0); coh.Add(0);

                    ibase = phi.Count - 1;

                    meanWeight1 = 0;
                    bool begin = false;
                    for (int imeshpt = 0; imeshpt < mesh[imesh].Count; imeshpt += 2)
                    {
                        if (mesh[imesh][imeshpt + 1].Y < yBot1) continue;

                        if (!begin)
                        {
                            /* NOTE:
                             * The angle of friction and cohesion are taken as a simple average between
                             * the current and previous slice. If the previous slice had the same as the current
                             * then the material has not changed and the result will be the same. If the previous
                             * slice had a different material, the values are taken as midway between the two
                             * slices' values. This does not account for proportion of the base of the slice
                             * occupied by each material type. For relatively thin slices, this should be 
                             * accurate enough; for very wide slices it is not very accurate, but if the slices
                             * are too wide then other errors will dominate regardless.
                             */
                            if (ibase == 0)
                            {
                                phi[ibase] = mesh[imesh][imeshpt].Material.Phi;
                                coh[ibase] = mesh[imesh][imeshpt].Material.Cohesion;
                            }
                            else
                            {
                                phi[ibase] = 0.5 * (mesh[imesh][imeshpt].Material.Phi + phi[ibase - 1]);
                                coh[ibase] = 0.5 * (mesh[imesh][imeshpt].Material.Cohesion + coh[ibase - 1]);
                            }

                            yLower = yBot1;

                            begin = true;
                        }
                        else
                        {
                            yLower = mesh[imesh][imeshpt].Y;
                        }

                        yUpper = mesh[imesh][imeshpt + 1].Y;

                        yHeight = yUpper - yLower;

                        meanWeight1 += mesh[imesh][imeshpt].Material.Gamma * yHeight;
                    }
                    meanWeight1 /= (yTop1 - yBot1);

                    weight.Add(0.5 * width[ibase] * (meanWeight0 * (yTop0 - yBot0) + meanWeight1 * (yTop1 - yBot1)));

                    applied += weight[ibase] * Math.Sin(alpha[ibase]);

                    // Update right side info to new left side
                    x0 = x1;
                    yTop0 = yTop1;
                    yBot0 = yBot1;
                    meanWeight0 = meanWeight1;
                    imesh++;
                }

                // Set right side of slice to exit coords
                x1 = xExit;
                yTop1 = yBot1 = yExit;

                // Compute slice properties
                alpha.Add(Math.Atan((yBot1 - yBot0) / (x1 - x0)));
                width.Add(x1 - x0);

                // error catch for surfaces with "zero slices"
                if (phi.Count == 0) continue;

                ibase = phi.Count - 1;

                phi.Add(phi[ibase]);
                coh.Add(coh[ibase]);

                weight.Add(0.5 * width[ibase + 1] * meanWeight0 * (yTop0 - yBot0));

                applied += weight[ibase] * Math.Sin(alpha[ibase]);

                // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                // TESTING PROPERTY LIST COUNTS
                // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

                //MessageBox.Show(String.Format(
                //    "alpha count = {0}\n" +
                //    "width count = {1}\n" +
                //    "phi count = {2}\n" +
                //    "coh count = {3}\n" +
                //    "weight count = {4}\n",
                //    alpha.Count,
                //    width.Count,
                //    phi.Count,
                //    coh.Count,
                //    weight.Count), "Count Tester");

                if (Math.Abs(applied) > toler)
                {
                    // Compute resisting moments and safety factor
                    errSF = 1.0;
                    currSF = 1.0;
                    iter = 0;

                    while (errSF > toler && iter++ < 100)
                    {
                        prevSF = currSF;
                        resist = 0;

                        for (int islice = 0; islice < alpha.Count; islice++)
                        {
                            bishFactor = Math.Cos(alpha[islice]) + Math.Sin(alpha[islice]) * Math.Tan(phi[islice]) / prevSF;

                            resist += direction * (Math.Tan(phi[islice]) * weight[islice] + coh[islice] * width[islice]) / bishFactor;
                        }

                        currSF = resist / applied;
                        errSF = Math.Abs(currSF - prevSF) / prevSF;
                    }
                }
                else currSF = 1000.0;

                if (iter == 100) currSF = 1000.0;

                // If safety factor is lowest computed for this surface
                if (currSF > 0 && currSF < result)
                {
                    result = currSF;
                    slipcircle.XEnter = xEnter;
                    slipcircle.YEnter = yEnter;
                    slipcircle.XExit = xExit;
                    slipcircle.YExit = yExit;
                }
            }

            return result;
        }

        // ------------------------------------------------------------------
        // RFEM Analysis
        // ------------------------------------------------------------------

        private static double RFEM(CircularSurface slipcircle, List<Point> surface, List<List<AnalysisMeshPoint>> mesh)
        {
            int nnodel = 2,             // nodes / element
                nvar = 2,               // dofs / node
                nvel = nnodel * nvar,   // dofs / element
                n,                      // number of slices
                nnet,                   // total system dofs
                nel,                    // total system elements
                npts;                   // number of mapping nodes

            double soildir = slipcircle.SoilDirection == SoilMovement.RtoL ? 1.0 : -1.0;

            /*List<List<int>> ico;                            // for connectivity
            List<int> index = new List<int>(nvel);          // for mapping element to global
            List<int> nd = new List<int>(nnodel);  */         // for element nodes

            double result, currSF, toler = 1e-5,
                xc, yc, r, xcSq, ycSq, rSq,
                xEnter, yEnter, xExit, yExit,
                direction,
                x0, y0, h0, x1, y1, h1,
                A, B, C, disc, sqrtDisc,
                v, w, m, c;

            BandSymMatrix gstif /*, estif = new BandSymMatrix(nvel, 5)*/;    // global and element stiffness matrices
            BandSymMatrix[] gstif_chol;      // global stiffness matrix Cholesky decomposition
            DenseMatrix gload,/* eload = new DenseMatrix(nvel, 1),*/        // global and element load vectors
                        iload, dload, dload0,                           // incremental and delta load vectors
                        gdisp, gdisp0, idisp, ddisp;                    // global, incremental, delta displacement

            /*List<double> x = new List<double>(nnodel),      // slice coordinates
                         y = new List<double>(nnodel),
                         h = new List<double>(nnodel);*/

            int listcap = 200;

            List<double> xg = new List<double>(listcap),           // mesh coordinates
                         yg = new List<double>(listcap),
                         hg = new List<double>(listcap);

            List<double> alpha = new List<double>(listcap),        // slice geometry and weight
                         width = new List<double>(listcap),
                         blength = new List<double>(listcap),
                         weight = new List<double>(listcap);

            List<double> K_trb = new List<double>(listcap),        // wt avg base properties
                         K_nob = new List<double>(listcap),
                         K_nrb = new List<double>(listcap),
                         A_coefb = new List<double>(listcap),
                         Phi_b = new List<double>(listcap),
                         Coh_b = new List<double>(listcap);

            List<double> K_trs = new List<double>(listcap),        // wt avg interslice properties
                         K_nos = new List<double>(listcap),
                         K_nrs = new List<double>(listcap),
                         A_coefs = new List<double>(listcap),
                         Phi_s = new List<double>(listcap),
                         Coh_s = new List<double>(listcap),
                         Gamma_s = new List<double>(listcap);

            double acoef = 1e-5;    // RFEM constant

            List<double> xEnterExit = new List<double>(6);
            List<double> yEnterExit = new List<double>(6);

            // Initialize SF
            result = currSF = 1000.0;

            // Get circular surface parameters
            xc = slipcircle.X;
            yc = slipcircle.Y;
            r = slipcircle.R;

            xcSq = Math.Pow(xc, 2);
            ycSq = Math.Pow(yc, 2);
            rSq = Math.Pow(r, 2);

            direction = slipcircle.SoilDirection == SoilMovement.LtoR ? -1.0 : 1.0;

            // Get entry and exit points for failure surface
            for (int i = 0; i < surface.Count - 1; i++)
            {
                // Get coords of current surface line segment
                x0 = surface[i].X;
                y0 = surface[i].Y;
                x1 = surface[i + 1].X;
                y1 = surface[i + 1].Y;

                // If line is vertical
                if (Math.Abs(x0 - x1) < toler)
                {
                    v = x0;

                    A = 1.0;
                    B = -2 * yc;
                    C = Math.Pow(v - xc, 2) + ycSq - rSq;

                    disc = Math.Pow(B, 2) - 4 * A * C;
                    if (disc < 0) continue;
                    sqrtDisc = Math.Sqrt(disc);

                    w = (-B - sqrtDisc) / (2 * A);

                    if (w >= Math.Min(y0, y1) && w <= Math.Max(y0, y1))
                    {
                        xEnterExit.Add(v);
                        yEnterExit.Add(w);
                    }

                    w = (-B + sqrtDisc) / (2 * A);

                    if (w >= Math.Min(y0, y1) && w <= Math.Max(y0, y1))
                    {
                        xEnterExit.Add(v);
                        yEnterExit.Add(w);
                    }
                }
                // If line is horizontal
                else if (Math.Abs(y0 - y1) < toler)
                {
                    w = y0;

                    A = 1.0;
                    B = -2 * xc;
                    C = Math.Pow(w - yc, 2) + xcSq - rSq;

                    disc = Math.Pow(B, 2) - 4 * A * C;
                    if (disc < 0) continue;
                    sqrtDisc = Math.Sqrt(disc);

                    v = (-B - sqrtDisc) / (2 * A);

                    if (v >= Math.Min(x0, x1) && v <= Math.Max(x0, x1))
                    {
                        xEnterExit.Add(v);
                        yEnterExit.Add(w);
                    }

                    v = (-B + sqrtDisc) / (2 * A);

                    if (v >= Math.Min(x0, x1) && v <= Math.Max(x0, x1))
                    {
                        xEnterExit.Add(v);
                        yEnterExit.Add(w);
                    }
                }
                // Otherwise, compute slope and y-intercept
                else
                {
                    m = (y1 - y0) / (x1 - x0);
                    c = y0 - m * x0;

                    A = 1 + Math.Pow(m, 2);
                    B = 2 * (m * (c - yc) - xc);
                    C = Math.Pow(c - yc, 2) + xcSq - rSq;

                    disc = Math.Pow(B, 2) - 4 * A * C;
                    if (disc < 0) continue;
                    sqrtDisc = Math.Sqrt(disc);

                    v = (-B - sqrtDisc) / (2 * A);

                    if (v >= Math.Min(x0, x1) && v <= Math.Max(x0, x1))
                    {
                        w = m * v + c;

                        xEnterExit.Add(v);
                        yEnterExit.Add(w);
                    }

                    v = (-B + sqrtDisc) / (2 * A);

                    if (v >= Math.Min(x0, x1) && v <= Math.Max(x0, x1))
                    {
                        w = m * v + c;

                        xEnterExit.Add(v);
                        yEnterExit.Add(w);
                    }
                }
            }

            while (xEnterExit.Count >= 2)
            {
                xg.Clear(); yg.Clear(); hg.Clear();

                alpha.Clear(); width.Clear(); blength.Clear(); weight.Clear();

                K_trb.Clear(); K_nob.Clear(); K_nrb.Clear(); A_coefb.Clear();
                Phi_b.Clear(); Coh_b.Clear();

                K_trs.Clear(); K_nos.Clear(); K_nrs.Clear(); A_coefs.Clear();
                Phi_s.Clear(); Coh_s.Clear(); Gamma_s.Clear();

                // Get entry and exit coords of slip surface
                xEnter = xEnterExit[0];
                yEnter = yEnterExit[0];
                xEnterExit.RemoveAt(0);
                yEnterExit.RemoveAt(0);

                xExit = xEnterExit[0];
                yExit = yEnterExit[0];
                xEnterExit.RemoveAt(0);
                yEnterExit.RemoveAt(0);

                if (Math.Abs(xExit - xEnter) < mesh[1][0].X - mesh[0][0].X) continue;

                // Advance to mesh in region of interest
                int imesh = 0;
                while (mesh[imesh][0].X < xEnter) imesh++;

                // Initialize x and y coords of LHS
                x0 = xEnter; xg.Add(x0);
                y0 = yEnter; yg.Add(y0);
                h0 = yEnter; hg.Add(h0);

                // Set first interslice parameters to zero (y0 == h0)
                K_trs.Add(0); K_nos.Add(0); K_nrs.Add(0); A_coefs.Add(0);
                Phi_s.Add(0); Coh_s.Add(0); Gamma_s.Add(0);

                // Step through mesh, getting geometry
                int ibase, iface;
                while (mesh[imesh][0].X < xExit)
                {
                    x1 = mesh[imesh][0].X;
                    h1 = mesh[imesh][mesh[imesh].Count - 1].Y;

                    // Compute y coord of bottom of right side of slice
                    A = 1.0;
                    B = -2 * yc;
                    C = Math.Pow(x1 - xc, 2) + ycSq - rSq;

                    sqrtDisc = Math.Sqrt(Math.Pow(B, 2) - 4 * A * C);

                    y1 = (-B - sqrtDisc) / (2 * A);

                    // Add slice coords to mesh coords
                    xg.Add(x1); yg.Add(y1); hg.Add(h1);

                    // Compute slice properties
                    alpha.Add(Math.Atan((y1 - y0) / (x1 - x0)));
                    width.Add(x1 - x0);
                    blength.Add(Math.Sqrt(Math.Pow(x1 - x0, 2) + Math.Pow(y1 - y0, 2)));

                    // Initialize base parameters to zero
                    K_trb.Add(0); K_nob.Add(0); K_nrb.Add(0); A_coefb.Add(0);
                    Phi_b.Add(0); Coh_b.Add(0);

                    // Initialize interslice parameters to zero
                    K_trs.Add(0); K_nos.Add(0); K_nrs.Add(0); A_coefs.Add(0);
                    Phi_s.Add(0); Coh_s.Add(0); Gamma_s.Add(0);

                    ibase = K_trb.Count - 1; iface = K_trs.Count - 1;

                    bool begin = false; double yUpper, yLower, yHeight;
                    for (int imeshpt = 0; imeshpt < mesh[imesh].Count; imeshpt += 2)
                    {
                        if (mesh[imesh][imeshpt + 1].Y < y1) continue;

                        if (!begin)
                        {
                            /* NOTE:
                             * The angle of friction and cohesion are taken as a simple average between
                             * the current and previous slice. If the previous slice had the same as the current
                             * then the material has not changed and the result will be the same. If the previous
                             * slice had a different material, the values are taken as midway between the two
                             * slices' values. This does not account for proportion of the base of the slice
                             * occupied by each material type. For relatively thin slices, this should be 
                             * accurate enough; for very wide slices it is not very accurate, but if the slices
                             * are too wide then other errors will dominate regardless.
                             */
                            if (ibase == 0)
                            {
                                K_trb[ibase] = mesh[imesh][imeshpt].Material.Ktrb;
                                K_nob[ibase] = mesh[imesh][imeshpt].Material.Kno;
                                K_nrb[ibase] = mesh[imesh][imeshpt].Material.Knr;
                                A_coefb[ibase] = mesh[imesh][imeshpt].Material.Acoef;

                                Phi_b[ibase] = mesh[imesh][imeshpt].Material.Phi;
                                Coh_b[ibase] = mesh[imesh][imeshpt].Material.Cohesion;
                            }
                            else
                            {
                                K_trb[ibase] = 0.5 * (mesh[imesh][imeshpt].Material.Ktrb + K_trb[ibase - 1]);
                                K_nob[ibase] = 0.5 * (mesh[imesh][imeshpt].Material.Kno + K_nob[ibase - 1]);
                                K_nrb[ibase] = 0.5 * (mesh[imesh][imeshpt].Material.Knr + K_nrb[ibase - 1]);
                                A_coefb[ibase] = 0.5 * (mesh[imesh][imeshpt].Material.Acoef + A_coefb[ibase - 1]);

                                Phi_b[ibase] = 0.5 * (mesh[imesh][imeshpt].Material.Phi + Phi_b[ibase - 1]);
                                Coh_b[ibase] = 0.5 * (mesh[imesh][imeshpt].Material.Cohesion + Coh_b[ibase - 1]);
                            }

                            yLower = y1;

                            begin = true;
                        }
                        else
                        {
                            yLower = mesh[imesh][imeshpt].Y;
                        }

                        yUpper = mesh[imesh][imeshpt + 1].Y;

                        yHeight = yUpper - yLower;

                        K_trs[iface] += mesh[imesh][imeshpt].Material.Ktrs * yHeight;
                        K_nos[iface] += mesh[imesh][imeshpt].Material.Kno * yHeight;
                        K_nrs[iface] += mesh[imesh][imeshpt].Material.Knr * yHeight;
                        A_coefs[iface] += mesh[imesh][imeshpt].Material.Acoef * yHeight;

                        Phi_s[iface] += mesh[imesh][imeshpt].Material.Phi * yHeight;
                        Coh_s[iface] += mesh[imesh][imeshpt].Material.Cohesion * yHeight;
                        Gamma_s[iface] += mesh[imesh][imeshpt].Material.Gamma * yHeight;
                    }
                    yHeight = h1 - y1;

                    K_trs[iface] /= yHeight;
                    K_nos[iface] /= yHeight;
                    K_nrs[iface] /= yHeight;
                    A_coefs[iface] /= yHeight;

                    Phi_s[iface] /= yHeight;
                    Coh_s[iface] /= yHeight;
                    Gamma_s[iface] /= yHeight;

                    weight.Add(0.5 * width[ibase]
                        * (Gamma_s[iface] * (hg[iface] - yg[iface])
                        + Gamma_s[iface - 1] * (hg[iface - 1] - yg[iface - 1])));

                    // Update right side info to new left side
                    x0 = x1;
                    y0 = y1;
                    h0 = h1;
                    imesh++;
                }

                // Add exit coords to mesh coords
                x1 = xExit; xg.Add(x1);
                y1 = yExit; yg.Add(y1);
                h1 = yExit; hg.Add(h1);

                // Compute slice properties
                alpha.Add(Math.Atan((y1 - y0) / (x1 - x0)));
                width.Add(x1 - x0);
                blength.Add(Math.Sqrt(Math.Pow(x1 - x0, 2) + Math.Pow(y1 - y0, 2)));

                // error catch for surfaces with "zero slices"
                if (K_trb.Count == 0) continue;

                ibase = K_trb.Count - 1;

                // Set base parameters to same as previous slice
                K_trb.Add(K_trb[ibase]);
                K_nob.Add(K_nob[ibase]);
                K_nrb.Add(K_nrb[ibase]);
                A_coefb.Add(A_coefb[ibase]);
                Phi_b.Add(Phi_b[ibase]);
                Coh_b.Add(Coh_b[ibase]);

                // Set final interslice parameters to zero (y0 == h0)
                K_trs.Add(0); K_nos.Add(0); K_nrs.Add(0); A_coefs.Add(0);
                Phi_s.Add(0); Coh_s.Add(0); Gamma_s.Add(0);

                // Add weight of final slice
                weight.Add(0.5 * width[ibase + 1] * Gamma_s[ibase] * (hg[ibase] - yg[ibase]));

                // Item counts for RFEM
                n = K_trb.Count;    // # of slices
                nel = n - 1;        // # of elements
                nnet = n * nvar;    // # of system dofs
                npts = n + 1;       // # of mapping nodes

                /*
                // Create connectivity list
                ico = new List<List<int>>(nel);
                for (int i = 0; i < nel; i++)
                {
                    ico.Add(new List<int>(2));
                    ico[i].Add(i);
                    ico[i].Add(i + 1);
                }
                */

                // Initialize global stiffness matrix
                gstif = new BandSymMatrix(nnet, 5);

                // Initialize global load vectors
                gload = new DenseMatrix(nnet);
                iload = new DenseMatrix(nnet);
                dload = new DenseMatrix(nnet);
                dload0 = new DenseMatrix(nnet);

                // Initialize global displacement vectors
                gdisp = new DenseMatrix(nnet);
                gdisp0 = new DenseMatrix(nnet);
                ddisp = new DenseMatrix(nnet);
                idisp = new DenseMatrix(nnet);

                // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                // TESTING PROPERTY LIST COUNTS
                // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

                //MessageBox.Show(String.Format(
                //    "Number of slices, n = {0}\n" +
                //    "K_trb count = {1}\n" +
                //    "K_nob count = {2}\n" +
                //    "K_nrb_count = {3}\n" +
                //    "A_coefb count = {4}\n" +
                //    "Phi_b count = {5}\n" +
                //    "Coh_b count = {6}\n" +
                //    "alpha count = {7}\n" +
                //    "width count = {8}\n" +
                //    "blength count = {9}\n" +
                //    "weight count = {10}\n\n" +
                //    "Number of mesh points, npts = {11}\n" +
                //    "K_trs count = {12}\n" +
                //    "K_nos count = {13}\n" +
                //    "K_nrs count = {14}\n" +
                //    "A_coefs count = {15}\n" +
                //    "Phi_s count = {16}\n" +
                //    "Coh_s count = {17}\n" +
                //    "xg count = {18}\n" +
                //    "yg count = {19}\n" +
                //    "hg count = {20}\n\n" +
                //    "Number of elements, nel = {21}\n" +
                //    "Number of system dofs, nnet = {22}",
                //    n,
                //    K_trb.Count,
                //    K_nob.Count,
                //    K_nrb.Count,
                //    A_coefb.Count,
                //    Phi_b.Count,
                //    Coh_b.Count,
                //    alpha.Count,
                //    width.Count,
                //    blength.Count,
                //    weight.Count,
                //    npts,
                //    K_trs.Count,
                //    K_nos.Count,
                //    K_nrs.Count,
                //    A_coefs.Count,
                //    Phi_s.Count,
                //    Coh_s.Count,
                //    xg.Count,
                //    yg.Count,
                //    hg.Count,
                //    nel,
                //    nnet), "Count Tester");

                /*eload[3, 0] = 0;    // Clear values from any previous analysis
                estif[2, 3] = 0;*/

                // build up initial stiffness matrix and initial load vector
                double cos, cos2, sin, sin2, A1, Ab, Kx, Ky, Kn, Kt, F;
                int gel0, gel1, gel2, gel3;
                for (int iel = 0; iel < nel; iel++)
                {
                    gel0 = iel * nvar;
                    gel1 = gel0 + 1;
                    gel2 = gel0 + 2;
                    gel3 = gel0 + 3;

                    // Compute geometry parameters (for efficiency)
                    cos = Math.Cos(alpha[iel]);
                    cos2 = Math.Pow(cos, 2);
                    sin = Math.Sin(alpha[iel]);
                    sin2 = Math.Pow(sin, 2);
                    A1 = hg[iel + 1] - yg[iel + 1];
                    Ab = blength[iel];

                    // Create element load vector
                    /*eload[1, 0] = -weight[iel];*/
                    gload[gel1, 0] += -weight[iel];

                    // Compute stiffness parameters
                    Kx = A1 * K_nos[iel + 1];
                    Ky = A1 * (K_trs[iel + 1] + Coh_s[iel + 1] / acoef);

                    Kn = Ab * K_nob[iel];
                    F = Math.Max(Coh_b[iel] - (weight[iel] * cos / Ab) * Math.Tan(Phi_b[iel]), 0);
                    Kt = Ab * (K_trb[iel] + F / acoef);

                    // Create element stiffness matrix
                    // (Note:   null and symmetric entries can be omitted for efficiency
                    //          due to the nature of the BandSymMatrix object)
                    //estif[0, 0] = Kx + (Kt * cos2 + Kn * sin2);
                    //estif[0, 1] = (Kt - Kn) * cos * sin;
                    //estif[0, 2] = -Kx;
                    ///* estif[0, 3] = 0; */
                    ///* estif[1, 0] = (Kt - Kn) * cos * sin; */
                    //estif[1, 1] = Ky + (Kn * cos2 + Kt * sin2);
                    ///* estif[1, 2] = 0; */
                    //estif[1, 3] = -Ky;
                    ///* estif[2, 0] = -Kx; */
                    ///* estif[2, 1] = 0; */
                    //estif[2, 2] = Kx;
                    ///* estif[2, 3] = 0; */
                    ///* estif[3, 0] = 0; */
                    ///* estif[3, 1] = -Ky; */
                    ///* estif[3, 2] = 0; */
                    //estif[3, 3] = Ky;

                    gstif[gel0, gel0] += Kx + (Kt * cos2 + Kn * sin2);
                    gstif[gel0, gel1] += (Kt - Kn) * cos * sin;
                    gstif[gel0, gel2] += -Kx;
                    /* gstif[gel0, gel3] += 0; */
                    /* gstif[gel1, gel0] += (Kt - Kn) * cos * sin; */
                    gstif[gel1, gel1] += Ky + (Kn * cos2 + Kt * sin2);
                    /* gstif[gel1, gel2] += 0; */
                    gstif[gel1, gel3] += -Ky;
                    /* gstif[gel2, gel0] += -Kx; */
                    /* gstif[gel2, gel1] += 0; */
                    gstif[gel2, gel2] += Kx;
                    /* gstif[gel2, gel3] += 0; */
                    /* gstif[gel3, gel0] += 0; */
                    /* gstif[gel3, gel1] += -Ky; */
                    /* gstif[gel3, gel2] += 0; */
                    gstif[gel3, gel3] += Ky;
                }

                // Compute geometry for last slice
                cos = Math.Cos(alpha[nel]);
                cos2 = Math.Pow(cos, 2);
                sin = Math.Sin(alpha[nel]);
                sin2 = Math.Pow(sin, 2);
                Ab = blength[nel];

                // Add to global load vector
                gload[nnet - 1, 0] = -weight[nel];

                // Compute stiffness properties for last slice
                Kn = Ab * K_nob[nel];
                F = Math.Max(Coh_b[nel] - (weight[nel] * cos / Ab) * Math.Tan(Phi_b[nel]), 0);
                Kt = Ab * (K_trb[nel] + F / acoef);

                // Add to global stiffness matrix
                gstif[nnet - 2, nnet - 2] += Kt * cos2 + Kn * sin2;
                gstif[nnet - 2, nnet - 1] += (Kt - Kn) * cos * sin;
                /* gstif[nnet - 1, nnet - 2] += (Kt - Kn) * cos * sin; */
                gstif[nnet - 1, nnet - 1] += Kn * cos2 + Kt * sin2;

                // obtain Cholesky decomposition of the (initial) global stiffness matrix
                gstif_chol = gstif.Cholesky;

                if (gstif_chol == null) continue;

                // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                /* TESTING MATRIX SOLVER */

                //BandSymMatrix.SolveInPlaceCholesky(gstif_chol, gload, ref gdisp);

                //gstif.PrintFullToFile("C:\\Users\\Brandon\\Documents\\School\\Slope 2011\\gstif.txt");
                //gstif_chol[0].PrintFullToFile("C:\\Users\\Brandon\\Documents\\School\\Slope 2011\\gstif_chol[0].txt");
                //gstif_chol[1].PrintFullToFile("C:\\Users\\Brandon\\Documents\\School\\Slope 2011\\gstif_chol[1].txt");
                //gload.PrintToFile("C:\\Users\\Brandon\\Documents\\School\\Slope 2011\\gload.txt");
                //gdisp.PrintToFile("C:\\Users\\Brandon\\Documents\\School\\Slope 2011\\gdisp.txt");

                /* MATRIX SOLVER TESTING COMPLETE */
                // !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

                // NON-LINEAR SOLVER FOR RFEM (MODIFIED NEWTON-RAPHSON)
                double eps_s = 1e-5, eps_a, iresid, gresid;
                int niter = 5000, iter;
                int nstep = (int)Math.Ceiling(Math.Log(1 / eps_s) / Math.Log(2) + 1);
                double relax = 0.25;
                double dfact = 1.0, fact0 = 0.0, factor = 0.0;
                /*bool converge;*/

                for (int istep = 0; istep < nstep; istep++)
                {
                    factor = fact0 + dfact;             // Set load scaling factor for current step
                    gdisp0.CopyInPlace(ref gdisp);      // Put current displacements and loads into
                    dload0.CopyInPlace(ref dload);      //      solver matrices

                    eps_a = 1; iter = 0; /* converge = false;*/

                    while (eps_a > eps_s && iter < niter && factor <= 1)
                    {
                        iter++;

                        // Determine global load vector for this iteration
                        DenseMatrix.MultiplyInPlace(factor, gload, ref iload);
                        DenseMatrix.AddInPlace(iload, dload, ref iload);

                        // --------------------------------------
                        // SOLVE THE STIFFNESS EQUATION
                        // --------------------------------------

                        // solve gstif*idisp = iload
                        BandSymMatrix.SolveInPlaceCholesky(gstif_chol, iload, ref idisp);

                        // compute update global displacements, ddisp = gdisp + idisp
                        DenseMatrix.AddInPlace(gdisp, idisp, ref ddisp);

                        // apply relaxation factor, gdisp = (1-relax)*gdisp + relax*ddisp
                        DenseMatrix.MultiplyInPlace(1 - relax, gdisp, ref gdisp);
                        DenseMatrix.MultiplyInPlace(relax, ddisp, ref ddisp);
                        DenseMatrix.AddInPlace(gdisp, ddisp, ref gdisp);

                        // compute actual load (non-linear load-displacement curve)
                        ComputeDLoadRFEM(   ref dload, gdisp,
                                            n, /* kappa = */ 1.0, /*acoef = */ 1e-5,
                                            yg, hg,
                                            alpha, blength,
                                            K_nob, K_nos,
                                            K_nrb, K_nrs,
                                            K_trb, K_trs,
                                            A_coefb, A_coefs,
                                            Phi_b, Phi_s,
                                            Coh_b, Coh_s);

                        // compute approximation error
                        iresid = 0; gresid = 0;
                        for (int i = 0; i < nnet; i++)
                        {
                            iresid += idisp[i, 0] * idisp[i, 0];
                            gresid += gdisp[i, 0] * gdisp[i, 0];
                        }
                        eps_a = Math.Sqrt(iresid / gresid);
                    }

                    /*converge = iter < niter;*/

                    // If NewtonRaphson solver converged, step forward
                    if (iter < niter)
                    {
                        fact0 = factor;
                        gdisp.CopyInPlace(ref gdisp0);
                        dload.CopyInPlace(ref dload0);
                        dfact /= 2;
                        niter /= 2;
                    }
                    else        // otherwise, proceed with load reduction
                    {
                        dfact /= 2;
                    }

                    // ensure lower bound for maximum iterations
                    niter = Math.Max(niter, 200);

                    // if converged on first run (i.e. Fs > 1), exit load stepping loop
                    if (Math.Abs(1 - factor) < toler) break;
                }

                currSF = ComputeSafetyFactorRFEM(   gdisp,
                                                    n, /* kappa = */ 1.0, /*acoef = */ 1e-5,
                                                    alpha, blength,
                                                    K_nob, K_nrb,
                                                    K_trb, A_coefb,
                                                    Phi_b, Coh_b);
                currSF *= soildir * factor;

                // If safety factor is lowest computed for this surface
                if (currSF > 0 && currSF < result)
                {
                    result = currSF;
                    slipcircle.XEnter = xEnter;
                    slipcircle.YEnter = yEnter;
                    slipcircle.XExit = xExit;
                    slipcircle.YExit = yExit;
                }
            }

            //Random rand = new Random();
            //result = rand.NextDouble();

            return result;
        }

        private static void ComputeDLoadRFEM(ref DenseMatrix dload, DenseMatrix gdisp,
                                                int n, double kappa, double acoef,
                                                List<double> yg, List<double> hg,
                                                List<double> alpha, List<double> blength,
                                                List<double> K_nob, List<double> K_nos,
                                                List<double> K_nrb, List<double> K_nrs,
                                                List<double> K_trb, List<double> K_trs,
                                                List<double> A_coefb, List<double> A_coefs,
                                                List<double> Phi_b, List<double> Phi_s,
                                                List<double> Coh_b, List<double> Coh_s)
        {
            dload.FillZeros();

            int iii, ii;
            double cos, sin,
                A0, A1, Ab,
                du, dv,
                ubar, vbar,
                Kx, Ky, Kn, Kt, F,
                tau_n, sigma_n, denom,
                Ns, Ts, Nb, Tb;

            // loop through nodes to compute ACTUAL loads
            for (int i = 0; i < n; i++)
            {
                ii = 2 * i; iii = ii + 1;  // compute (x,y) dof indices

                // ----------------------------------------------------
                // COMPUTE LOAD COMPONENTS DUE TO INTERSLICE STIFFNESS
                // ----------------------------------------------------

                if (i == 0)    // FIRST slice only has interslice stiffness on LEADING side
                {
                    /*
                     * 
                     * LEADING SLICE INTERFACE
                     * (note: for FIRST slice, there is zero stiffness on the trailing side)
                     * 
                     */

                    A1 = hg[1] - yg[1];         // area of slice interface

                    du = gdisp[ii + 2, 0] - gdisp[ii, 0];       // relative displacements
                    dv = gdisp[iii + 2, 0] - gdisp[iii, 0];

                    // normal interslice stiffness
                    Kx = A1 * (du <= 0 ? K_nos[i + 1] : K_nrs[i + 1] + kappa / (du + A_coefs[i + 1]));

                    Ns = du * Kx;       // normal force between slices
                    dload[ii, 0] = Ns;

                    // shear interslice stiffness
                    //F = Math.Max(Coh_s[i + 1] - (Ns / A1) * Math.Tan(Phi_s[i + 1]), 0);
                    //Ky = A1 * (K_trs[i + 1] + F / (Math.Abs(dv) + acoef));

                    sigma_n = Ns / A1;
                    denom = Math.Abs(dv);
                    F = sigma_n * Math.Tan(Phi_s[i + 1]) + Coh_s[i + 1];
                    tau_n = denom > 0.0 ? 0.5 * (K_trs[i + 1] * denom + F) : 0.0;
                    F = Math.Max(F - tau_n, 0);
                    Ky = A1 * (K_trs[i + 1] + F / (denom + acoef));

                    Ts = dv * Ky;       // shear force between slices
                    dload[iii, 0] = Ts;
                }
                else if (i == n - 1) // LAST slice only has interslice stiffness on TRAILING side
                {
                    /*
                     * 
                     * TRAILING SLICE INTERFACE
                     * (note: for LAST slice, there is zero stiffness on the leading side)
                     * 
                     */

                    A0 = hg[n - 1] - yg[n - 1];     // area of slice interface

                    du = gdisp[ii, 0] - gdisp[ii - 2, 0];       // relative displacements
                    dv = gdisp[iii, 0] - gdisp[iii - 2, 0];

                    // normal interslice stiffness
                    Kx = A0 * (du <= 0 ? K_nos[i] : K_nrs[i] + kappa / (du + A_coefs[i]));

                    Ns = -du * Kx;      // normal force between slices
                    dload[ii, 0] = Ns;

                    // shear interslice stiffness
                    //F = Math.Max(Coh_s[i] - (-Ns / A0) * Math.Tan(Phi_s[i]), 0);
                    //Ky = A0 * (K_trs[i] + F / (Math.Abs(dv) + acoef));

                    sigma_n = -Ns / A0;
                    denom = Math.Abs(dv);
                    F = sigma_n * Math.Tan(Phi_s[i]) + Coh_s[i];
                    tau_n = denom > 0.0 ? 0.5 * (K_trs[i] * denom + F) : 0.0;
                    F = Math.Max(F - tau_n, 0);
                    Ky = A0 * (K_trs[i] + F / (denom + acoef));

                    Ts = -dv * Ky;      // shear force between slices
                    dload[iii, 0] = Ts;
                }
                else
                {
                    /*
                     * 
                     * TRAILING SLICE INTERFACE
                     * 
                     */

                    A0 = hg[i] - yg[i];     // areas of slice interface

                    du = gdisp[ii, 0] - gdisp[ii - 2, 0];       // relative displacements
                    dv = gdisp[iii, 0] - gdisp[iii - 2, 0];

                    // normal interslice stiffness
                    Kx = A0 * (du <= 0 ? K_nos[i] : K_nrs[i] + kappa / (du + A_coefs[i]));

                    Ns = -du * Kx;      // normal force between slices
                    dload[ii, 0] = Ns;

                    // shear interslice stiffness
                    //F = Math.Max(Coh_s[i] - (-Ns / A0) * Math.Tan(Phi_s[i]), 0);
                    //Ky = A0 * (K_trs[i] + F / (Math.Abs(dv) + acoef));

                    sigma_n = -Ns / A0;
                    denom = Math.Abs(dv);
                    F = sigma_n * Math.Tan(Phi_s[i]) + Coh_s[i];
                    tau_n = denom > 0.0 ? 0.5 * (K_trs[i] * denom + F) : 0.0;
                    F = Math.Max(F - tau_n, 0);
                    Ky = A0 * (K_trs[i] + F / (denom + acoef));

                    Ts = -dv * Ky;      // shear force between slices
                    dload[iii, 0] = Ts;



                    /*
                     * 
                     * LEADING SLICE INTERFACE
                     * 
                     */

                    A1 = hg[i + 1] - yg[i + 1];     // areas of slice interface

                    du = gdisp[ii + 2, 0] - gdisp[ii, 0];       // relative displacements
                    dv = gdisp[iii + 2, 0] - gdisp[iii, 0];

                    // normal interslice stiffness
                    Kx = A1 * (du <= 0 ? K_nos[i + 1] : K_nrs[i + 1] + kappa / (du + A_coefs[i + 1]));

                    Ns = du * Kx;       // normal force between slices
                    dload[ii, 0] += Ns;

                    // shear interslice stiffness
                    //F = Math.Max(Coh_s[i + 1] - (Ns / A1) * Math.Tan(Phi_s[i + 1]), 0);
                    //Ky = A1 * (K_trs[i + 1] + F / (Math.Abs(dv) + acoef));

                    sigma_n = Ns / A1;
                    denom = Math.Abs(dv);
                    F = sigma_n * Math.Tan(Phi_s[i + 1]) + Coh_s[i + 1];
                    tau_n = denom > 0.0 ? 0.5 * (K_trs[i + 1] * denom + F) : 0.0;
                    F = Math.Max(F - tau_n, 0);
                    Ky = A1 * (K_trs[i + 1] + F / (denom + acoef));

                    Ts = dv * Ky;       // shear force between slices
                    dload[iii, 0] += Ts;
                }


                // ----------------------------------------------------
                // COMPUTE LOAD COMPONENTS DUE TO BASE STIFFNESS
                // ----------------------------------------------------

                cos = Math.Cos(alpha[i]);
                sin = Math.Sin(alpha[i]);       // slice geometry
                Ab = blength[i];

                // convert global displacements to local coordinates
                ubar = gdisp[ii, 0] * cos + gdisp[iii, 0] * sin;
                vbar = -gdisp[ii, 0] * sin + gdisp[iii, 0] * cos;

                // normal stiffness at base
                Kn = Ab * (vbar <= 0 ? K_nob[i] : K_nrb[i] + kappa / (vbar + A_coefb[i]));

                Nb = -vbar * Kn;        // normal force at base

                // shear stiffness at base
                //F = Math.Max(Coh_b[i] - (-Nb / Ab) * Math.Tan(Phi_b[i]), 0);
                //Kt = Ab * (K_trb[i] + F / (Math.Abs(ubar) + acoef));

                sigma_n = -Nb / Ab;
                denom = Math.Abs(ubar);
                F = sigma_n * Math.Tan(Phi_b[i]) + Coh_b[i];
                tau_n = denom > 0.0 ? 0.5 * (K_trb[i] * denom + F) : 0.0;
                F = Math.Max(F - tau_n, 0);
                Kt = Ab * (K_trb[i] + F / (denom + acoef));

                Tb = -ubar * Kt;        // shear force at base

                // convert loads from local to global coords
                dload[ii, 0] += Tb * cos - Nb * sin;
                dload[iii, 0] += Tb * sin + Nb * cos;
            }
        }

        private static double ComputeSafetyFactorRFEM(DenseMatrix gdisp,
                                                        int n, double kappa, double acoef,
                                                        List<double> alpha, List<double> blength,
                                                        List<double> K_nob, List<double> K_nrb,
                                                        List<double> K_trb, List<double> A_coefb,
                                                        List<double> Phi_b, List<double> Coh_b)
        {
            // moments of mobilized and available shear resistance
            double Ma = 0.0, Mr = 0.0;

            int iii, ii;
            double cos, sin,
                Ab,
                ubar, vbar,
                Kn, Kt, F,
                tau_n, sigma_n, denom,
                Nb, Tb;

            // loop through slices computing available and mobilized shear resistance at base
            for (int i = 0; i < n; i++)
            {
                ii = 2 * i; iii = ii + 1;   // dof indices

                // slice geometry
                cos = Math.Cos(alpha[i]);
                sin = Math.Sin(alpha[i]);
                Ab = blength[i];

                // convert global displacements into local coords
                ubar = gdisp[ii, 0] * cos + gdisp[iii, 0] * sin;
                vbar = -gdisp[ii, 0] * sin + gdisp[iii, 0] * cos;

                // normal stiffness at base
                Kn = Ab * (vbar <= 0 ? K_nob[i] : K_nrb[i] + kappa / (vbar + A_coefb[i]));

                Nb = -vbar * Kn;        // normal force at base

                // shear stiffness at base
                //F = Math.Max(Coh_b[i] - (-Nb / Ab) * Math.Tan(Phi_b[i]), 0);
                //Kt = Ab * (K_trb[i] + F / (Math.Abs(ubar) + acoef));

                sigma_n = -Nb / Ab;
                denom = Math.Abs(ubar);
                F = sigma_n * Math.Tan(Phi_b[i]) + Coh_b[i];
                tau_n = denom > 0.0 ? 0.5 * (K_trb[i] * denom + F) : 0.0;
                F = Math.Max(F - tau_n, 0);
                Kt = Ab * (K_trb[i] + F / (denom + acoef));

                Tb = -ubar * Kt;        // shear force at base

                // increment moments
                Mr += Nb * Math.Tan(Phi_b[i]) + Ab * Coh_b[i];
                Ma += Tb;
            }

            return Mr / Ma;     // compute factor of safety
        }
    }
}
