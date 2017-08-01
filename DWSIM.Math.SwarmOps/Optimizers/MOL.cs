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
    /// Many Optimizing Liaisons (MOL) optimization method devised
    /// as a simplification to the PSO method originally due to
    /// Eberhart et al. (1, 2). The MOL method does not have any
    /// attraction to the particle's own best known position, and
    /// the algorithm also makes use of random selection of which
    /// particle to update instead of iterating over the entire swarm.
    /// It is similar to the "Social Only" PSO suggested by Kennedy (3),
    /// and was studied more thoroguhly by Pedersen et al. (4) who
    /// found it to sometimes outperform PSO, and have more easily
    /// tunable control parameters.
    /// </summary>
    /// <remarks>
    /// References:
    /// (1) J. Kennedy and R. Eberhart. Particle swarm optimization.
    ///     In Proceedings of IEEE International Conference on Neural
    ///     Networks, volume IV, pages 1942-1948, Perth, Australia, 1995
    /// (2) Y. Shi and R.C. Eberhart. A modified particle swarm optimizer.
    ///     In Proceedings of the IEEE International Conference on
    ///     Evolutionary Computation, pages 69-73, Anchorage, AK, USA, 1998.
    /// (3) J. Kennedy. The particle swarm: social adaptation of knowledge,
    ///     In: Proceedings of the IEEE International Conference on
    ///     Evolutionary Computation, Indianapolis, USA, 1997.
    /// (4) M.E.H. Pedersen and A.J. Chipperfield. Simplifying particle
    ///     swarm optimization. Applied Soft Computing, 10, p. 618-628, 2010. 
    /// </remarks>
    public class MOL : Optimizer
    {
        #region Constructors.
        /// <summary>
        /// Construct the object.
        /// </summary>
        public MOL()
            : base()
        {
        }

        /// <summary>
        /// Construct the object.
        /// </summary>
        /// <param name="problem">Problem to optimize.</param>
        public MOL(Problem problem)
            : base(problem)
        {
        }
        #endregion

        #region Sets of control parameters.
        /// <summary>
        /// Control parameters.
        /// </summary>
        public struct Parameters
        {
            /// <summary>
            /// Control parameters tuned for all benchmark problems in
            /// 2 dimensions and 400 fitness evaluations in one optimization run.
            /// </summary>
            public static readonly double[] AllBenchmarks2Dim400IterA = { 23.0, -0.3328, 2.8446 };

            /// <summary>
            /// Control parameters tuned for all benchmark problems in
            /// 2 dimensions and 400 fitness evaluations in one optimization run.
            /// </summary>
            public static readonly double[] AllBenchmarks2Dim400IterB = { 50.0, 0.284, 1.9466 };

            /// <summary>
            /// Control parameters tuned for all benchmark problems in
            /// 2 dimensions and 4000 fitness evaluations in one optimization run.
            /// </summary>
            public static readonly double[] AllBenchmarks2Dim4000IterA = { 183.0, -0.2797, 3.0539 };

            /// <summary>
            /// Control parameters tuned for all benchmark problems in
            /// 2 dimensions and 4000 fitness evaluations in one optimization run.
            /// </summary>
            public static readonly double[] AllBenchmarks2Dim4000IterB = { 139.0, 0.6372, 1.0949 };

            /// <summary>
            /// Control parameters tuned for all benchmark problems in
            /// 5 dimensions and 1000 fitness evaluations in one optimization run.
            /// </summary>
            public static readonly double[] AllBenchmarks5Dim1000Iter = { 50.0, -0.3085, 2.0273 };

            /// <summary>
            /// Control parameters tuned for all benchmark problems in
            /// 5 dimensions and 10000 fitness evaluations in one optimization run.
            /// </summary>
            public static readonly double[] AllBenchmarks5Dim10000Iter = { 96.0, -0.3675, 4.171 };

            /// <summary>
            /// Control parameters tuned for all benchmark problems in
            /// 10 dimensions and 2000 fitness evaluations in one optimization run.
            /// </summary>
            public static readonly double[] AllBenchmarks10Dim2000Iter = { 60.0, -0.27, 2.9708 };

            /// <summary>
            /// Control parameters tuned for all benchmark problems in
            /// 10 dimensions and 20000 fitness evaluations in one optimization run.
            /// </summary>
            public static readonly double[] AllBenchmarks10Dim20000Iter = { 116.0, -0.3518, 3.8304 };

            /// <summary>
            /// Control parameters tuned for all benchmark problems in
            /// 20 dimensions and 40000 fitness evaluations in one optimization run.
            /// </summary>
            public static readonly double[] AllBenchmarks20Dim40000Iter = { 228.0, -0.3747, 4.2373 };

            /// <summary>
            /// Control parameters tuned for Ackley, Rastrigin, Rosenbrock, and Schwefel1-2 in
            /// 20 dimensions and 400000 fitness evaluations in one optimization run.
            /// </summary>
            public static readonly double[] FourBenchmarks20Dim400000IterA = { 125.0, -0.2575, 4.6713 };

            /// <summary>
            /// Control parameters tuned for Ackley, Rastrigin, Rosenbrock, and Schwefel1-2 in
            /// 20 dimensions and 400000 fitness evaluations in one optimization run.
            /// </summary>
            public static readonly double[] FourBenchmarks20Dim400000IterB = { 67.0, -0.4882, 2.7923 };

            /// <summary>
            /// Control parameters tuned for all benchmark problems in
            /// 50 dimensions and 100000 fitness evaluations in one optimization run.
            /// </summary>
            public static readonly double[] AllBenchmarks50Dim100000Iter = { 290.0, -0.3067, 3.6223 };

            /// <summary>
            /// Control parameters tuned for Ackley, Rastrigin, Rosenbrock, and Schwefel1-2 in
            /// 100 dimensions and 200000 fitness evaluations in one optimization run.
            /// </summary>
            public static readonly double[] FourBenchmarks100Dim200000Iter = { 219.0, -0.1685, 3.9162 };

            /// <summary>
            /// Control parameters tuned for all benchmark problems in
            /// 30 dimensions and 60000 fitness evaluations in one optimization run.
            /// </summary>
            public static readonly double[] AllBenchmarks30Dim60000Iter = { 198.0, -0.2723, 3.8283 };

            /// <summary>
            /// Control parameters tuned for all benchmark problems in
            /// 30 dimensions and 600000 fitness evaluations in one optimization run.
            /// </summary>
            public static readonly double[] AllBenchmarks30Dim600000Iter = { 134.0, -0.43, 3.0469 };

            /// <summary>
            /// Control parameters tuned for Rastrigin in 30 dimensions and 60000
            /// fitness evaluations in one optimization run.
            /// </summary>
            public static readonly double[] Rastrigin_30Dim60000Iter = { 114.0, -0.3606, 3.822 };

            /// <summary>
            /// Control parameters tuned for Schwefel1-2 in 30 dimensions and 60000
            /// fitness evaluations in one optimization run.
            /// </summary>
            public static readonly double[] Schwefel12_30Dim60000Iter1 = { 130.0, -0.2765, 3.8011 };

            /// <summary>
            /// Control parameters tuned for Schwefel1-2 in 30 dimensions and 60000
            /// fitness evaluations in one optimization run.
            /// </summary>
            public static readonly double[] Schwefel12_30Dim60000Iter2 = { 138.0, -0.4774, 2.3943 };

            /// <summary>
            /// Control parameters tuned for Sphere and Rosenbrock problems in 30
            /// dimensions each and 60000 fitness evaluations in one optimization run.
            /// </summary>
            public static readonly double[] SphereRosenbrock_30Dim60000Iter = { 42.0, -0.4055, 3.1722 };

            /// <summary>
            /// Control parameters tuned for Rastrigin and Schwefel1-2 problems in 30
            /// dimensions each and 60000 fitness evaluations in one optimization run.
            /// </summary>
            public static readonly double[] RastriginSchwefel12_30Dim60000Iter = { 47.0, -0.3, 3.5582 };

            /// <summary>
            /// Control parameters tuned for Rastrigin and Schwefel1-2 problems in 30
            /// dimensions each and 600000 fitness evaluations in one optimization run.
            /// </summary>
            public static readonly double[] RastriginSchwefel12_30Dim600000Iter1 = { 130, -0.4135, 3.1937 };

            /// <summary>
            /// Control parameters tuned for Rastrigin and Schwefel1-2 problems in 30
            /// dimensions each and 600000 fitness evaluations in one optimization run.
            /// </summary>
            public static readonly double[] RastriginSchwefel12_30Dim600000Iter2 = { 72.0, -0.6076, 1.9609 };

            /// <summary>
            /// Control parameters tuned for QuarticNoise, Sphere, Step problems in 30
            /// dimensions each and 60000 fitness evaluations in one optimization run.
            /// </summary>
            public static readonly double[] QuarticNoiseSphereStep_30Dim60000Iter = { 83.0, -0.3461, 3.2535 };
        }
        #endregion

        #region Get control parameters.
        /// <summary>
        /// Get parameter, Number of agents, aka. swarm-size.
        /// </summary>
        /// <param name="parameters">Optimizer parameters.</param>
        public int GetNumAgents(double[] parameters)
        {
            return (int)System.Math.Round(parameters[0], System.MidpointRounding.AwayFromZero);
        }

        /// <summary>
        /// Get parameter, Omega.
        /// </summary>
        /// <param name="parameters">Optimizer parameters.</param>
        public double GetOmega(double[] parameters)
        {
            return parameters[1];
        }

        /// <summary>
        /// Get parameter, Phi.
        /// </summary>
        /// <param name="parameters">Optimizer parameters.</param>
        public double GetPhi(double[] parameters)
        {
            return parameters[2];
        }
        #endregion

        #region Base-class overrides, Problem.
        /// <summary>
        /// Name of the optimizer.
        /// </summary>
        public override string Name
        {
            get { return "MOL"; }
        }

        /// <summary>
        /// Number of control parameters for optimizer.
        /// </summary>
        public override int Dimensionality
        {
            get { return 3; }
        }

        string[] _parameterName = { "S", "omega", "phi" };

        /// <summary>
        /// Control parameter names.
        /// </summary>
        public override string[] ParameterName
        {
            get { return _parameterName; }
        }

        /// <summary>
        /// Default control parameters.
        /// </summary>
        public override double[] DefaultParameters
        {
            get { return Parameters.AllBenchmarks30Dim60000Iter; }
        }

        static readonly double[] _lowerBound = { 1.0, -2.0, -4.0 };

        /// <summary>
        /// Lower search-space boundary for control parameters.
        /// </summary>
        public override double[] LowerBound
        {
            get { return _lowerBound; }
        }

        static readonly double[] _upperBound = { 300.0, 2.0, 6.0 };

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

            // Retrieve parameter specific to this optimizer.
            int numAgents = GetNumAgents(parameters);
            double omega = GetOmega(parameters);
            double phi = GetPhi(parameters);

            // Get problem-context.
            double[] lowerBound = Problem.LowerBound;
            double[] upperBound = Problem.UpperBound;
            double[] lowerInit = Problem.LowerInit;
            double[] upperInit = Problem.UpperInit;
            int n = Problem.Dimensionality;

            // Allocate agent positions and velocities.
            double[][] agents = Tools.NewMatrix(numAgents, n);
            double[][] velocities = Tools.NewMatrix(numAgents, n);

            // Allocate velocity boundaries.
            double[] velocityLowerBound = new double[n];
            double[] velocityUpperBound = new double[n];

            // Best-found position, fitness and constraint feasibility.
            double[] g = new double[n];
            double gFitness = Problem.MaxFitness;
            bool gFeasible = false;

            // Iteration variables.
            int i, j, k;

            // Initialize velocity boundaries.
            for (k = 0; k < n; k++)
            {
                double range = System.Math.Abs(upperBound[k] - lowerBound[k]);

                velocityLowerBound[k] = -range;
                velocityUpperBound[k] = range;
            }

            // Initialize all agents.
            // This counts as iterations below.
            for (j = 0; j < numAgents && Problem.Continue(j, gFitness, gFeasible); j++)
            {
                // Refer to the j'th agent as x and v.
                double[] x = agents[j];
                double[] v = velocities[j];

                // Initialize agent-position in search-space.
                Tools.InitializeUniform(ref x, lowerInit, upperInit);

                // Initialize velocity.
                Tools.InitializeUniform(ref v, velocityLowerBound, velocityUpperBound);

                // Enforce constraints and evaluate feasibility.
                bool newFeasible = Problem.EnforceConstraints(ref x);

                // Compute fitness if feasibility (constraint satisfaction) is same or better.
                if (Tools.BetterFeasible(gFeasible, newFeasible))
                {
                    // Compute fitness of initial position.
                    double newFitness = Problem.Fitness(x, gFitness, gFeasible, newFeasible);

                    // Update swarm's best known position, if improvement.
                    if (Tools.BetterFeasibleFitness(gFeasible, newFeasible, gFitness, newFitness))
                    {
                        x.CopyTo(g, 0);
                        gFitness = newFitness;
                        gFeasible = newFeasible;
                    }
                }

                // Trace fitness of best found solution.
                Trace(j, gFitness, gFeasible);
            }

            // Perform actual optimization iterations.
            for (i = numAgents; Problem.Continue(i, gFitness, gFeasible); i++)
            {
                Debug.Assert(numAgents > 0);

                // Pick random agent.
                j = Globals.Random.Index(numAgents);

                // Refer to the j'th agent as x and v.
                double[] x = agents[j];
                double[] v = velocities[j];

                // Pick random weight.
                double r = Globals.Random.Uniform();

                // Update velocity.
                for (k = 0; k < n; k++)
                {
                    v[k] = omega * v[k] + phi * r * (g[k] - x[k]);
                }

                // Fix denormalized floating-point values in velocity.
                Tools.Denormalize(ref v);

                // Enforce velocity bounds before updating position.
                Tools.Bound(ref v, velocityLowerBound, velocityUpperBound);

                // Update position.
                for (k = 0; k < n; k++)
                {
                    x[k] = x[k] + v[k];
                }

                // Enforce constraints and evaluate feasibility.
                bool newFeasible = Problem.EnforceConstraints(ref x);

                // Compute fitness if feasibility (constraint satisfaction) is same or better.
                if (Tools.BetterFeasible(gFeasible, newFeasible))
                {
                    // Compute new fitness.
                    double newFitness = Problem.Fitness(x, gFitness, gFeasible, newFeasible);

                    // Update swarm's best known position, if improvement.
                    if (Tools.BetterFeasibleFitness(gFeasible, newFeasible, gFitness, newFitness))
                    {
                        x.CopyTo(g, 0);
                        gFitness = newFitness;
                        gFeasible = newFeasible;
                    }
                }

                // Trace fitness of best found solution.
                Trace(i, gFitness, gFeasible);
            }

            // Signal end of optimization run.
            Problem.EndOptimizationRun();

            // Return best-found solution and fitness.
            return new Result(g, gFitness, gFeasible, i);
        }
        #endregion

        /// <summary>
        /// Enforce constraints and evaluate feasiblity of the wrapped problem.
        /// </summary>
        /// <param name="parameters">Candidate solution.</param>
        public override bool EnforceConstraints(ref double[] parameters)
        {
            Tools.Bound(ref parameters, LowerBound, UpperBound);

            return Feasible(parameters);
        }

        /// <summary>
        /// Evaluate feasibility (constraint satisfaction) of the wrapped problem.
        /// </summary>
        /// <param name="parameters">Candidate solution.</param>
        public override bool Feasible(double[] parameters)
        {
#if false
            int numAgents = GetNumAgents(parameters);
            double omega = GetOmega(parameters);
            double phi = GetPhi(parameters);

            // Example of constraints on an optimizer's control optimizers.
            // These particular constraints are only for demonstration purposes.
            return (numAgents >= 1) && (omega > 0 || omega < -0.5) && (omega * phi < 0) && (omega + phi < 2);
#else
            return base.Feasible(parameters);
#endif
        }
    }
}