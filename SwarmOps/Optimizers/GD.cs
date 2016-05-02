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
    /// Gradient Descent (GD), follows the gradient of
    /// the problem in small steps.
    /// </summary>
    public class GD : Optimizer
    {
        #region Constructors.
        /// <summary>
        /// Construct the object.
        /// </summary>
        public GD()
            : base()
        {
        }

        /// <summary>
        /// Construct the object.
        /// </summary>
        /// <param name="problem">Problem to optimize.</param>
        public GD(Problem problem)
            : base(problem)
        {
        }
        #endregion

        #region Get control parameters.
        /// <summary>
        /// Get parameter, Stepsize.
        /// </summary>
        /// <param name="parameters">Optimizer parameters.</param>
        public double GetStepsize(double[] parameters)
        {
            return parameters[0];
        }
        #endregion

        #region Base-class overrides, Problem.
        /// <summary>
        /// Name of the optimizer.
        /// </summary>
        public override string Name
        {
            get { return "GD"; }
        }

        /// <summary>
        /// Number of control parameters for optimizer.
        /// </summary>
        public override int Dimensionality
        {
            get { return 1; }
        }

        string[] _parameterName = { "Stepsize" };

        /// <summary>
        /// Control parameter names.
        /// </summary>
        public override string[] ParameterName
        {
            get { return _parameterName ; }
        }

        static readonly double[] _defaultParameters = { 0.05 };

        /// <summary>
        /// Default control parameters.
        /// </summary>
        public override double[] DefaultParameters
        {
            get { return _defaultParameters; }
        }

        static readonly double[] _lowerBound = { 0 };

        /// <summary>
        /// Lower search-space boundary for control parameters.
        /// </summary>
        public override double[] LowerBound
        {
            get { return _lowerBound; }
        }

        static readonly double[] _upperBound = { 2.0 };

        /// <summary>
        /// Upper search-space boundary for control parameters.
        /// </summary>
        public override double[] UpperBound
        {
            get { return _upperBound; }
        }
        #endregion

        #region Base-class overrides, Optimizer.
        /// <summary>
        /// Perform one optimization run and return the best found solution.
        /// </summary>
        /// <param name="parameters">Control parameters for the optimizer.</param>
        public override Result Optimize(double[] parameters)
        {
            Debug.Assert(parameters != null && parameters.Length == Dimensionality);

            // Signal beginning of optimization run.
            Problem.BeginOptimizationRun();

            // Retrieve parameter specific to GD method.
            double stepsize = GetStepsize(parameters);

            // Get problem-context.
            double[] lowerBound = Problem.LowerBound;
            double[] upperBound = Problem.UpperBound;
            double[] lowerInit = Problem.LowerInit;
            double[] upperInit = Problem.UpperInit;
            int n = Problem.Dimensionality;

            // Allocate agent position and search-range.
            double[] x = new double[n];					// Current position.
            double[] v = new double[n];					// Gradient/velocity.
            double[] g = new double[n];					// Best-found position.

            // Initialize agent-position in search-space.
            Tools.InitializeUniform(ref x, lowerInit, upperInit);

            // Enforce constraints and evaluate feasibility.
            bool feasible = Problem.EnforceConstraints(ref x);

            // Compute fitness of initial position.
            // This counts as an iteration below.
            double fitness = Problem.Fitness(x, feasible);

            // This is the best-found position.
            x.CopyTo(g, 0);

            // Trace fitness of best found solution.
            Trace(0, fitness, feasible);

            int i;
            for (i = 1; Problem.Continue(i, fitness, feasible); i++)
            {
                // Compute gradient.
                int gradientIterations = Problem.Gradient(x, ref v);

                // Compute norm of gradient-vector.
                double gradientNorm = Tools.Norm(v);

                // Compute current stepsize.
                double normalizedStepsize = stepsize / gradientNorm;

                // Move in direction of steepest descent.
                for (int j = 0; j < n; j++)
                {
                    x[j] -= normalizedStepsize * v[j];
                }

                // Enforce constraints and evaluate feasibility.
                bool newFeasible = Problem.EnforceConstraints(ref x);

                // Compute fitness if feasibility (constraint satisfaction) is same or better.
                if (Tools.BetterFeasible(feasible, newFeasible))
                {
                    // Compute new fitness.
                    double newFitness = Problem.Fitness(x, fitness, feasible, newFeasible);

                    // Update best position and fitness found in this run.
                    if (Tools.BetterFeasibleFitness(feasible, newFeasible, fitness, newFitness))
                    {
                        // Update this run's best known position.
                        x.CopyTo(g, 0);

                        // Update this run's best know fitness.
                        fitness = newFitness;
                    }
                }

                // Trace fitness of best found solution.
                Trace(i, fitness, feasible);

                // Add iterations for gradient computation.
                // This is incompatible with FitnessTrace.
                //i += gradientIterations;
            }

            // Signal end of optimization run.
            Problem.EndOptimizationRun();

            // Return best-found solution and fitness.
            return new Result(g, fitness, feasible, i);
        }
        #endregion
    }
}