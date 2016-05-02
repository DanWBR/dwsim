/// ------------------------------------------------------
/// SwarmOps - Numeric and heuristic optimization for C#
/// Copyright (C) 2003-2011 Magnus Erik Hvass Pedersen.
/// Please see the file license.txt for license details.
/// SwarmOps on the internet: http://www.Hvass-Labs.org/
/// ------------------------------------------------------

namespace SwarmOps.Optimizers
{
    /// <summary>
    /// Samples the search-space completely at random. Used
    /// for comparing other optimizers to 'worst-case' performance.
    /// </summary>
    public class RND : Optimizer
    {
        #region Constructors.
        /// <summary>
        /// Construct the object.
        /// </summary>
        public RND()
            : base()
        {
        }

        /// <summary>
        /// Construct the object.
        /// </summary>
        /// <param name="problem">Problem to optimize.</param>
        public RND(Problem problem)
            : base(problem)
        {
        }
        #endregion

        #region Base-class overrides, Problem.
        /// <summary>
        /// Name of the optimizer.
        /// </summary>
        public override string Name
        {
            get { return "RND"; }
        }

        /// <summary>
        /// Number of control parameters for optimizer.
        /// </summary>
        public override int Dimensionality
        {
            get { return 0; }
        }

        /// <summary>
        /// Control parameter names.
        /// </summary>
        public override string[] ParameterName
        {
            get { return null; }
        }

        /// <summary>
        /// Default control parameters.
        /// </summary>
        public override double[] DefaultParameters
        {
            get { return null; }
        }

        /// <summary>
        /// Lower search-space boundary for control parameters.
        /// </summary>
        public override double[] LowerBound
        {
            get { return null; }
        }

        /// <summary>
        /// Upper search-space boundary for control parameters.
        /// </summary>
        public override double[] UpperBound
        {
            get { return null; }
        }
        #endregion

        #region Base-class overrides, Optimizer.
        /// <summary>
        /// Perform one optimization run and return the best found solution.
        /// </summary>
        /// <param name="parameters">Control parameters for the optimizer.</param>
        public override Result Optimize(double[] parameters)
        {
            // Signal beginning of optimization run.
            Problem.BeginOptimizationRun();

            // Get problem-context.
            double[] lowerBound = Problem.LowerBound;
            double[] upperBound = Problem.UpperBound;
            double[] lowerInit = Problem.LowerInit;
            double[] upperInit = Problem.UpperInit;
            int n = Problem.Dimensionality;

            // Allocate agent position and search-range vectors.
            double[] x = new double[n];     // Current position.
            double[] g = new double[n];		// Best-found position.

            // Initialize agent-position in search-space.
            Tools.InitializeUniform(ref x, lowerBound, upperBound);

            // Initialize fitness to worst possible.
            double fitness = Problem.MaxFitness;

            // Initialize feasibility.
            bool feasible = false;

            int i;
            for (i = 0; Problem.Continue(i, fitness, feasible); i++)
            {
                // Sample from entire search-space.
                Tools.InitializeUniform(ref x, lowerBound, upperBound);

                // Enforce constraints and evaluate feasibility.
                bool newFeasible = Problem.EnforceConstraints(ref x);

                // Compute fitness if feasibility is same or better.
                if (Tools.BetterFeasible(feasible, newFeasible))
                {
                    // Compute new fitness.
                    double newFitness = Problem.Fitness(x, fitness, feasible, newFeasible);

                    // Update best-known position and fitness.
                    if (Tools.BetterFeasibleFitness(feasible, newFeasible, fitness, newFitness))
                    {
                        // Update best-known position.
                        x.CopyTo(g, 0);

                        // Update best-known fitness.
                        fitness = newFitness;

                        // Update feasibility.
                        feasible = newFeasible;
                    }
                }

                // Trace fitness of best found solution.
                Trace(i, fitness, feasible);
            }

            // Signal end of optimization run.
            Problem.EndOptimizationRun();

            // Return best-found solution and fitness.
            return new Result(g, fitness, feasible, i);
        }
        #endregion
    }
}
