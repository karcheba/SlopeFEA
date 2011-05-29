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
    /// Interaction logic for GenAlgParamsDialog.xaml
    /// </summary>
    public partial class GenAlgParamsDialog : Window
    {
        private SlopeCanvas canvas;

        public GenAlgParamsDialog(Window owner)
        {
            InitializeComponent();

            this.Owner = owner;

            canvas = (SlopeCanvas)((Grid)((TabControl)((Grid)this.Owner.Content).Children[2]).SelectedContent).Children[2];

            population.Text = canvas.GeneticAlgorithmParameters.Population.ToString();
            generations.Text = canvas.GeneticAlgorithmParameters.Generations.ToString();
            fittest.Text = String.Format("{0}", Math.Round(canvas.GeneticAlgorithmParameters.FittestProportion, 3));
            mating.Text = String.Format("{0}", Math.Round(canvas.GeneticAlgorithmParameters.MatingPoolProportion, 3));
            crossover.Text = String.Format("{0}", Math.Round(canvas.GeneticAlgorithmParameters.CrossoverProbability, 3));
            mutation.Text = String.Format("{0}", Math.Round(canvas.GeneticAlgorithmParameters.MutationProbability, 3));
            slicewidth.Text = String.Format("{0}", Math.Round(canvas.GeneticAlgorithmParameters.SliceWidth, 2));

            string units;
            switch (canvas.Units)
            {
                case Units.Metres: units = "m"; break;
                case Units.Millimetres: units = "mm"; break;
                case Units.Feet: units = "ft"; break;
                default: units = "in"; break;
            }
            sliceUnits.Content = units;
        }

        private void ok_Click(object sender, RoutedEventArgs e)
        {
            int newPop;
            if (!int.TryParse(population.Text, out newPop) || newPop <= 0)
            {
                MessageBox.Show("Population must be a positive integer.", "Error");
                return;
            }

            int newGens;
            if (!int.TryParse(generations.Text, out newGens) || newGens <= 0)
            {
                MessageBox.Show("Number of generations must be a positive integer.", "Error");
                return;
            }

            double newFittest;
            if (!double.TryParse(fittest.Text, out newFittest) || newFittest <= 0 || newFittest > 1)
            {
                MessageBox.Show("Fittest proportion must be a number in the range: 0 < x <= 1", "Error");
                return;
            }

            double newMating;
            if (!double.TryParse(mating.Text, out newMating) || newMating <= 0 || newMating > 1)
            {
                MessageBox.Show("Mating pool proportion must be a number in the range: 0 < x <= 1", "Error");
                return;
            }

            double newCrossover;
            if (!double.TryParse(crossover.Text, out newCrossover) || newCrossover <= 0 || newCrossover > 1)
            {
                MessageBox.Show("Crossover probability must be a number in the range: 0 < x <= 1", "Error");
                return;
            }

            double newMutation;
            if (!double.TryParse(mutation.Text, out newMutation) || newMutation <= 0 || newMutation > 1)
            {
                MessageBox.Show("Mutation probability must be a number in the range: 0 < x <= 1", "Error");
                return;
            }

            double newSlice;
            if (!double.TryParse(slicewidth.Text, out newSlice) || newSlice <= 0)
            {
                MessageBox.Show("Slice width must be a positive number", "Error");
                return;
            }

            canvas.GeneticAlgorithmParameters.Population = newPop;
            canvas.GeneticAlgorithmParameters.Generations = newGens;
            canvas.GeneticAlgorithmParameters.FittestProportion = newFittest;
            canvas.GeneticAlgorithmParameters.MatingPoolProportion = newMating;
            canvas.GeneticAlgorithmParameters.CrossoverProbability = newCrossover;
            canvas.GeneticAlgorithmParameters.MutationProbability = newMutation;
            canvas.GeneticAlgorithmParameters.SliceWidth = newSlice;

            canvas.IsSaved = false;
            canvas.IsVerified = false;

            this.DialogResult = true;
        }
    }
}
