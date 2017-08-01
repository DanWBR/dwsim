/// ------------------------------------------------------
/// SwarmOps - Numeric and heuristic optimization for C#
/// Copyright (C) 2003-2011 Magnus Erik Hvass Pedersen.
/// Please see the file license.txt for license details.
/// SwarmOps on the internet: http://www.Hvass-Labs.org/
/// ------------------------------------------------------

using System.Diagnostics;

namespace SwarmOps.Optimizers
{
    /// <summary>
    /// Compute the standard Meta-Fitness measure, that is,
    /// perform a number of optimization runs on different
    /// problems and sum their results.
    /// </summary>
    /// <remarks>
    /// Preemptive Fitness Evaluation is used in that optimizations
    /// will be attempted aborted once the meta-fitness becomes
    /// worse than the Preemptive Fitness Limit (aka. fitnessLimit).
    /// In addition, the array of problems are being sorted at the
    /// end of each meta-fitness computation, so as to allow the worst
    /// performing problems to be optimized first in the next meta-fitness
    /// computation, so as to decrease the computation time further still.
    /// </remarks>
    public class MetaFitness : Problem
    {
        #region Constructors.
        /// <summary>
        /// Construct the object, un-weighted problems.
        /// </summary>
        /// <param name="optimizer">Optimizer to be used.</param>
        /// <param name="problems">Array of problems to be optimized.</param>
        /// <param name="numRuns">Number of optimization runs per problem.</param>
        /// <param name="maxIterations">Max number of optimization iterations.</param>
        public MetaFitness(Optimizer optimizer, Problem[] problems, int numRuns, int maxIterations)
            : base(maxIterations)
        {
            Optimizer = optimizer;
            NumRuns = numRuns;
            ProblemIndex = new ProblemIndex(problems);
        }

        /// <summary>
        /// Construct the object, weighted problems.
        /// </summary>
        /// <param name="optimizer">Optimize to be used.</param>
        /// <param name="weightedProblems">Array of weighted problems to be optimized.</param>
        /// <param name="numRuns">Number of optimization runs per problem.</param>
        /// <param name="maxIterations">Max number of optimization iterations.</param>
        public MetaFitness(Optimizer optimizer, WeightedProblem[] weightedProblems, int numRuns, int maxIterations)
            : base(maxIterations)
        {
            Optimizer = optimizer;
            NumRuns = numRuns;
            ProblemIndex = new ProblemIndex(weightedProblems);
        }
        #endregion

        #region Public fields.
        /// <summary>
        /// Number of optimization runs to be performed for each problem.
        /// </summary>
        public int NumRuns
        {
            get;
            private set;
        }

        /// <summary>
        /// Optimizer to use in optimizations.
        /// </summary>
        public Optimizer Optimizer
        {
            get;
            protected set;
        }
        #endregion

        #region Base-class overrides.
        /// <summary>
        /// Name of the optimization problem.
        /// </summary>
        public override string Name
        {
            get { return "MetaFitness (" + Optimizer.Name + ")"; }
        }

        /// <summary>
        /// Return LowerBound for Optimizer.
        /// </summary>
        public override double[] LowerBound
        {
            get { return Optimizer.LowerBound; }
        }

        /// <summary>
        /// Return UpperBound for Optimizer.
        /// </summary>
        public override double[] UpperBound
        {
            get { return Optimizer.UpperBound; }
        }

        /// <summary>
        /// Return LowerInit for Optimizer.
        /// </summary>
        public override double[] LowerInit
        {
            get { return Optimizer.LowerInit; }
        }

        /// <summary>
        /// Return UpperInit for Optimizer.
        /// </summary>
        public override double[] UpperInit
        {
            get { return Optimizer.UpperInit; }
        }

        /// <summary>
        /// Return Dimensionality for Optimizer.
        /// </summary>
        public override int Dimensionality
        {
            get { return Optimizer.Dimensionality; }
        }

        /// <summary>
        /// Return Zero which is the minimum fitness possible for a
        /// meta-fitness measure.
        /// </summary>
        public override double MinFitness
        {
            get { return 0; }
        }

        /// <summary>
        /// Return ParameterName for Optimizer.
        /// </summary>
        public override string[] ParameterName
        {
            get { return Optimizer.ParameterName; }
        }

        /// <summary>
        /// Compute the meta-fitness measure by passing the
        /// given parameters to the Optimizer, and perform
        /// optimization runs on the array of problems
        /// until the fitness compute exceeds the fitnessLimit.
        /// </summary>
        /// <param name="parameters">Parameters to use for the Optimizer.</param>
        /// <param name="fitnessLimit">Preemptive Fitness Limit</param>
        /// <returns>Fitness value.</returns>
        public override double Fitness(double[] parameters, double fitnessLimit)
        {
            // Initialize the fitness-sum.
            double fitnessSum = 0;

            // Iterate over the problems.
            for (int i = 0; i < ProblemIndex.Count && fitnessSum<fitnessLimit; i++)
            {
                // Assign the problem to the optimizer.
                Optimizer.Problem = ProblemIndex.GetProblem(i);

                // Get the weight associated with this problem.
                double weight = ProblemIndex.GetWeight(i);

                // Use another fitness summation because we need to keep
                // track of the performance on each problem.
                double fitnessSumInner = 0;

                // Perform a number of optimization runs.
                for (int j = 0; j < NumRuns && fitnessSum < fitnessLimit; j++)
                {
                    // Perform one optimization run on the problem.
                    Result result = Optimizer.Optimize(parameters, fitnessLimit - fitnessSum);

                    // Get the best fitness result from optimization and adjust it
                    // by subtracting its minimum possible value.
                    double fitness = result.Fitness;
                    double fitnessAdjusted = fitness - Optimizer.MinFitness;

                    // Ensure adjusted fitness is non-negative, otherwise Preemptive
                    // Fitness Evaluation does not work.
                    Debug.Assert(fitnessAdjusted >= 0);

                    // Apply weight to the adjusted fitness.
                    fitnessAdjusted *= weight;

                    // Accumulate both fitness sums.
                    fitnessSumInner += fitnessAdjusted;
                    fitnessSum += fitnessAdjusted;
                }

                // Set the fitness result achieved on the problem.
                // This was why we needed an extra summation variable.
                ProblemIndex.SetFitness(i, fitnessSumInner);
            }

            // Sort the optimization problems so that the worst
            // performing will be attempted optimized first, when
            // this method is called again.
            ProblemIndex.Sort();

            return fitnessSum;
        }

        /// <summary>
        /// Enforce constraints and evaluate feasiblity.
        /// </summary>
        /// <param name="parameters">Parameters to use for the Optimizer.</param>
        public override bool EnforceConstraints(ref double[] parameters)
        {
            return Optimizer.EnforceConstraints(ref parameters);
        }

        /// <summary>
        /// Evaluate feasibility (constraint satisfaction).
        /// </summary>
        /// <param name="parameters">Parameters to use for the Optimizer.</param>
        public override bool Feasible(double[] parameters)
        {
            return Optimizer.Feasible(parameters);
        }

        /// <summary>
        /// At beginning of new meta-optimization run print a newline.
        /// </summary>
        public override void BeginOptimizationRun()
        {
            Tools.PrintNewline();
        }
        #endregion

        #region Protected members.
        /// <summary>
        /// Sorted index of optimization problems.
        /// </summary>
        protected ProblemIndex ProblemIndex;
        #endregion
    }
}
