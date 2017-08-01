/// ------------------------------------------------------
/// SwarmOps - Numeric and heuristic optimization for C#
/// Copyright (C) 2003-2011 Magnus Erik Hvass Pedersen.
/// Please see the file license.txt for license details.
/// SwarmOps on the internet: http://www.Hvass-Labs.org/
/// ------------------------------------------------------

namespace SwarmOps.Optimizers
{
    /// <summary>
    /// Pattern Search (PS), an early variant was originally
    /// due to Fermi and Metropolis at the Los Alamos nuclear
    /// laboratory, as described by Davidon (1). It is also
    /// sometimes called compass search. This is a slightly
    /// different variant by Pedersen (2). It works for a
    /// wide variety of optimization problems, especially
    /// when only few iterations are allowed. It does, however,
    /// stagnate rather quickly.
    /// </summary>
    /// <remarks>
    /// References:
    /// (1) W.C. Davidon. Variable metric method for minimization.
    ///     SIAM Journal on Optimization, 1(1):1{17, 1991
    /// (2) M.E.H. Pedersen. Tuning & Simplifying Heuristical
    ///     Optimization. PhD Thesis, University of Southampton,
    ///     2010.
    /// </remarks>
    public class PS : Optimizer
    {
        #region Constructors.
        /// <summary>
        /// Construct the object.
        /// </summary>
        public PS()
            : base()
        {
        }

        /// <summary>
        /// Construct the object.
        /// </summary>
        /// <param name="problem">Problem to optimize.</param>
        public PS(Problem problem)
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
            get { return "PS"; }
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

        #region Base-class override, Optimizer.
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
            double[] x = new double[n];      // Current position.
            double[] y = new double[n];      // Potentially new position.
            double[] d = new double[n];      // Search-range.

            // Initialize agent-position in search-space.
            Tools.InitializeUniform(ref x, lowerInit, upperInit);

            // Initialize search-range to full search-space.
            Tools.InitializeRange(ref d, lowerBound, upperBound);

            // Enforce constraints and evaluate feasibility.
            bool feasible = Problem.EnforceConstraints(ref x);

            // Compute fitness of initial position.
            // This counts as an iteration below.
            double fitness = Problem.Fitness(x, feasible);

            // Trace fitness of best found solution.
            Trace(0, fitness, feasible);

            int i;
            for (i = 1; Problem.Continue(i, fitness, feasible); i++)
            {
                // Pick random dimension.
                int R = Globals.Random.Index(n);

                // Copy current position to new position
                x.CopyTo(y, 0);

                // Compute new value for randomly chosen dimension.
                y[R] += d[R];

                // Enforce constraints and evaluate feasibility.
                bool newFeasible = Problem.EnforceConstraints(ref y);

                // Compute fitness if feasibility (constraint satisfaction) is same or better.
                if (Tools.BetterFeasible(feasible, newFeasible))
                {
                    // Compute fitness of new position.
                    double newFitness = Problem.Fitness(y, fitness, feasible, newFeasible);

                    // Update best known position, if improvement.
                    if (Tools.BetterFeasibleFitness(feasible, newFeasible, fitness, newFitness))
                    {
                        // Update fitness.
                        fitness = newFitness;

                        // Update feasibility.
                        feasible = newFeasible;

                        // Update position by swapping array x and y.
                        // This is necessary because the constraint-handler
                        // may alter the position y to something different
                        // from what PS computed it to be. Otherwise we
                        // could just have updated the R'th dimension of x.
                        double[] temp = x;
                        x = y;
                        y = temp;
                    }
                    else // Worse fitness.
                    {
                        // Reduce and invert search-range.
                        d[R] *= -0.5;
                    }
                }
                else // Worse feasibility.
                {
                    // Reduce and invert search-range.
                    d[R] *= -0.5;
                }

                // Trace fitness of best found solution.
                Trace(i, fitness, feasible);
            }

            // Signal end of optimization run.
            Problem.EndOptimizationRun();

            // Return best-found solution and fitness.
            return new Result(x, fitness, feasible, i);
        }
        #endregion
    }
}
