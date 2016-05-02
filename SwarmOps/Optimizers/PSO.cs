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
    /// Particle Swarm Optimization (PSO) originally due to
    /// Eberhart et al. (1, 2). This is a 'plain vanilla'
    /// variant which can have its parameters tuned (or
    /// meta-optimized) to work well on a range of optimization
    /// problems. Generally, however, the DE optimizer has
    /// been found to work better.
    /// </summary>
    /// <remarks>
    /// References:
    /// (1) J. Kennedy and R. Eberhart. Particle swarm optimization.
    ///     In Proceedings of IEEE International Conference on Neural
    ///     Networks, volume IV, pages 1942-1948, Perth, Australia, 1995
    /// (2) Y. Shi and R.C. Eberhart. A modified particle swarm optimizer.
    ///     In Proceedings of the IEEE International Conference on
    ///     Evolutionary Computation, pages 69-73, Anchorage, AK, USA, 1998.
    /// </remarks>
    public class PSO : Optimizer
    {
        #region Constructors.
        /// <summary>
        /// Construct the object.
        /// </summary>
        public PSO()
            : base()
        {
        }

        /// <summary>
        /// Construct the object.
        /// </summary>
        /// <param name="problem">Problem to optimize.</param>
        public PSO(Problem problem)
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
            /// Hand-tuned control parameters.
            /// </summary>
            public static readonly double[] HandTuned = { 50.0, 0.729, 1.49445, 1.49445 };

            /// <summary>
            /// Control parameters tuned for all benchmark problems in
            /// 2 dimensions and 400 fitness evaluations in one optimization run.
            /// </summary>
            public static readonly double[] AllBenchmarks2Dim400IterA = { 25.0, 0.3925, 2.5586, 1.3358 };

            /// <summary>
            /// Control parameters tuned for all benchmark problems in
            /// 2 dimensions and 400 fitness evaluations in one optimization run.
            /// </summary>
            public static readonly double[] AllBenchmarks2Dim400IterB = { 29.0, -0.4349, -0.6504, 2.2073 };

            /// <summary>
            /// Control parameters tuned for all benchmark problems in
            /// 2 dimensions and 4000 fitness evaluations in one optimization run.
            /// </summary>
            public static readonly double[] AllBenchmarks2Dim4000IterA = { 156.0, 0.4091, 2.1304, 1.0575 };

            /// <summary>
            /// Control parameters tuned for all benchmark problems in
            /// 2 dimensions and 4000 fitness evaluations in one optimization run.
            /// </summary>
            public static readonly double[] AllBenchmarks2Dim4000IterB = { 237.0, -0.2887, 0.4862, 2.5067 };

            /// <summary>
            /// Control parameters tuned for all benchmark problems in
            /// 5 dimensions and 1000 fitness evaluations in one optimization run.
            /// </summary>
            public static readonly double[] AllBenchmarks5Dim1000IterA = { 63.0, -0.3593, -0.7238, 2.0289 };

            /// <summary>
            /// Control parameters tuned for all benchmark problems in
            /// 5 dimensions and 1000 fitness evaluations in one optimization run.
            /// </summary>
            public static readonly double[] AllBenchmarks5Dim1000IterB = { 47.0, -0.1832, 0.5287, 3.1913 };

            /// <summary>
            /// Control parameters tuned for all benchmark problems in
            /// 5 dimensions and 10000 fitness evaluations in one optimization run.
            /// </summary>
            public static readonly double[] AllBenchmarks5Dim10000IterA = { 223.0, -0.3699, -0.1207, 3.3657 };

            /// <summary>
            /// Control parameters tuned for all benchmark problems in
            /// 5 dimensions and 10000 fitness evaluations in one optimization run.
            /// </summary>
            public static readonly double[] AllBenchmarks5Dim10000IterB = { 203.0, 0.5069, 2.5524, 1.0056 };

            /// <summary>
            /// Control parameters tuned for all benchmark problems in
            /// 10 dimensions and 2000 fitness evaluations in one optimization run.
            /// </summary>
            public static readonly double[] AllBenchmarks10Dim2000IterA = { 63.0, 0.6571, 1.6319, 0.6239 };

            /// <summary>
            /// Control parameters tuned for all benchmark problems in
            /// 10 dimensions and 2000 fitness evaluations in one optimization run.
            /// </summary>
            public static readonly double[] AllBenchmarks10Dim2000IterB = { 204.0, -0.2134, -0.3344, 2.3259 };

            /// <summary>
            /// Control parameters tuned for all benchmark problems in
            /// 10 dimensions and 20000 fitness evaluations in one optimization run.
            /// </summary>
            public static readonly double[] AllBenchmarks10Dim20000Iter = { 53.0, -0.3488, -0.2746, 4.8976 };

            /// <summary>
            /// Control parameters tuned for all benchmark problems in
            /// 20 dimensions and 40000 fitness evaluations in one optimization run.
            /// </summary>
            public static readonly double[] AllBenchmarks20Dim40000Iter = { 69, -0.4438, -0.2699, 3.395 };

            /// <summary>
            /// Control parameters tuned for Ackley, Rastrigin, Rosenbrock, and Schwefel1-2 in
            /// 20 dimensions and 400000 fitness evaluations in one optimization run.
            /// </summary>
            public static readonly double[] FourBenchmarks20Dim400000IterA = { 149.0, -0.3236, -0.1136, 3.9789 };

            /// <summary>
            /// Control parameters tuned for Ackley, Rastrigin, Rosenbrock, and Schwefel1-2 in
            /// 20 dimensions and 400000 fitness evaluations in one optimization run.
            /// </summary>
            public static readonly double[] FourBenchmarks20Dim400000IterB = { 60.0, -0.4736, -0.97, 3.7904 };

            /// <summary>
            /// Control parameters tuned for Ackley, Rastrigin, Rosenbrock, and Schwefel1-2 in
            /// 20 dimensions and 400000 fitness evaluations in one optimization run.
            /// </summary>
            public static readonly double[] FourBenchmarks20Dim400000IterC = { 256.0, -0.3499, -0.0513, 4.9087 };

            /// <summary>
            /// Control parameters tuned for all benchmark problems in
            /// 50 dimensions and 100000 fitness evaluations in one optimization run.
            /// </summary>
            public static readonly double[] AllBenchmarks50Dim100000Iter = { 106, -0.2256, -0.1564, 3.8876 };

            /// <summary>
            /// Control parameters tuned for Ackley, Rastrigin, Rosenbrock, and Schwefel1-2 in
            /// 100 dimensions and 200000 fitness evaluations in one optimization run.
            /// </summary>
            public static readonly double[] FourBenchmarks100Dim200000Iter = { 161.0, -0.2089, -0.0787, 3.7637 };

            /// <summary>
            /// Control parameters tuned for all benchmark problems in
            /// 30 dimensions and 60000 fitness evaluations in one optimization run.
            /// </summary>
            public static readonly double[] AllBenchmarks30Dim60000Iter = { 134.0, -0.1618, 1.8903, 2.1225 };

            /// <summary>
            /// Control parameters tuned for all benchmark problems in
            /// 30 dimensions and 600000 fitness evaluations in one optimization run.
            /// </summary>
            public static readonly double[] AllBenchmarks30Dim600000Iter = { 95.0, -0.6031, -0.6485, 2.6475 };

            /// <summary>
            /// Control parameters tuned for Ackley in 30 dimensions and 60000
            /// fitness evaluations in one optimization run.
            /// </summary>
            public static readonly double[] Ackley_30Dim60000Iter = { 24.0, -0.6421, -3.9845, 0.2583 };

            /// <summary>
            /// Control parameters tuned for Rastrigin in 30 dimensions and 60000
            /// fitness evaluations in one optimization run.
            /// </summary>
            public static readonly double[] Rastrigin_30Dim60000Iter = { 53.0, -1.3131, -0.709, -0.5648 };

            /// <summary>
            /// Control parameters tuned for Rosenbrock in 30 dimensions and 60000
            /// fitness evaluations in one optimization run.
            /// </summary>
            public static readonly double[] Rosenbrock_30Dim60000Iter = { 2.0, 0.7622, 1.3619, 3.4249 };

            /// <summary>
            /// Control parameters tuned for Schwefel1-2 in 30 dimensions and 60000
            /// fitness evaluations in one optimization run.
            /// </summary>
            public static readonly double[] Schwefel12_30Dim60000Iter = { 119.0, -0.3718, -0.2031, 3.2785 };

            /// <summary>
            /// Control parameters tuned for Sphere and Rosenbrock problems in 30
            /// dimensions each and 60000 fitness evaluations in one optimization run.
            /// </summary>
            public static readonly double[] SphereRosenbrock_30Dim60000Iter = { 84.0, -0.3036, -0.0075, 3.973 };

            /// <summary>
            /// Control parameters tuned for Rastrigin and Schwefel1-2 problems in 30
            /// dimensions each and 60000 fitness evaluations in one optimization run.
            /// </summary>
            public static readonly double[] RastriginSchwefel12_30Dim60000Iter = { 82.0, -0.3794, -0.2389, 3.5481 };

            /// <summary>
            /// Control parameters tuned for Rastrigin and Schwefel1-2 problems in 30
            /// dimensions each and 600000 fitness evaluations in one optimization run.
            /// </summary>
            public static readonly double[] RastriginSchwefel12_30Dim600000Iter = { 104.0, -0.4565, -0.1244, 3.0364 };

            /// <summary>
            /// Control parameters tuned for QuarticNoise, Sphere, Step problems in 30
            /// dimensions each and 60000 fitness evaluations in one optimization run.
            /// </summary>
            public static readonly double[] QuarticNoiseSphereStep_30Dim60000Iter = { 50.0, -0.3610, 0.7590, 2.2897 };
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
        /// Get parameter, PhiP.
        /// </summary>
        /// <param name="parameters">Optimizer parameters.</param>
        public double GetPhiP(double[] parameters)
        {
            return parameters[2];
        }

        /// <summary>
        /// Get parameter, PhiG.
        /// </summary>
        /// <param name="parameters">Optimizer parameters.</param>
        public double GetPhiG(double[] parameters)
        {
            return parameters[3];
        }
        #endregion

        #region Base-class overrides, Problem.
        /// <summary>
        /// Name of the optimizer.
        /// </summary>
        public override string Name
        {
            get { return "PSO"; }
        }

        /// <summary>
        /// Number of control parameters for optimizer.
        /// </summary>
        public override int Dimensionality
        {
            get { return 4; }
        }

        string[] _parameterName = { "S", "omega", "phi_p", "phi_g" };

        /// <summary>
        /// Control parameter names.
        /// </summary>
        public override string[] ParameterName
        {
            get { return _parameterName; }
        }

        static readonly double[] _defaultParameters = Parameters.AllBenchmarks30Dim60000Iter;

        /// <summary>
        /// Default control parameters.
        /// </summary>
        public override double[] DefaultParameters
        {
            get { return _defaultParameters; }
        }

        static readonly double[] _lowerBound = { 1.0, -2.0, -4.0, -4.0 };

        /// <summary>
        /// Lower search-space boundary for control parameters.
        /// </summary>
        public override double[] LowerBound
        {
            get { return _lowerBound; }
        }

        static readonly double[] _upperBound = { 300.0, 2.0, 4.0, 6.0 };

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

            // Retrieve parameter specific to PSO method.
            int numAgents = GetNumAgents(parameters);
            double omega = GetOmega(parameters);
            double phiP = GetPhiP(parameters); // phi1
            double phiG = GetPhiG(parameters); // phi2

            // Get problem-context.
            double[] lowerBound = Problem.LowerBound;
            double[] upperBound = Problem.UpperBound;
            double[] lowerInit = Problem.LowerInit;
            double[] upperInit = Problem.UpperInit;
            int n = Problem.Dimensionality;

            // Allocate agent positions and associated fitnesses.
            double[][] agents = Tools.NewMatrix(numAgents, n);
            double[][] velocities = Tools.NewMatrix(numAgents, n);
            double[][] bestAgentPosition = Tools.NewMatrix(numAgents, n);
            double[] bestAgentFitness = new double[numAgents];
            bool[] bestAgentFeasible = new bool[numAgents];

            // Allocate velocity boundaries.
            double[] velocityLowerBound = new double[n];
            double[] velocityUpperBound = new double[n];

            // Iteration variables.
            int i, j, k;

            // Best-found position and fitness.
            double[] g = null;
            double gFitness = Problem.MaxFitness;
            bool gFeasible = false;

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

                // Initialize velocity.
                Tools.InitializeUniform(ref v, velocityLowerBound, velocityUpperBound);

                // Initialize agent-position in search-space.
                Tools.InitializeUniform(ref x, lowerInit, upperInit);

                // Enforce constraints and evaluate feasibility.
                bestAgentFeasible[j] = Problem.EnforceConstraints(ref x);

                // Compute fitness of initial position.
                bestAgentFitness[j] = Problem.Fitness(x, bestAgentFeasible[j]);

                // Initialize best known position.
                // Contents must be copied because the agent
                // will likely move to worse positions.
                x.CopyTo(bestAgentPosition[j], 0);

                // Update swarm's best known position.
                // This must reference the agent's best-known
                // position because the current position changes.
                if (Tools.BetterFeasibleFitness(gFeasible, bestAgentFeasible[j], gFitness, bestAgentFitness[j]))
                {
                    g = bestAgentPosition[j];
                    gFitness = bestAgentFitness[j];
                    gFeasible = bestAgentFeasible[j];
                }

                // Trace fitness of best found solution.
                Trace(j, gFitness, gFeasible);
            }

            // Perform actual optimization iterations.
            for (i = numAgents; Problem.Continue(i, gFitness, gFeasible); )
            {
                Debug.Assert(numAgents > 0);

                for (j = 0; j < numAgents && Problem.Continue(i, gFitness, gFeasible); j++, i++)
                {
                    // Refer to the j'th agent as x and v.
                    double[] x = agents[j];
                    double[] v = velocities[j];
                    double[] p = bestAgentPosition[j];

                    // Pick random weights.
                    double rP = Globals.Random.Uniform();
                    double rG = Globals.Random.Uniform();

                    // Update velocity.
                    for (k = 0; k < n; k++)
                    {
                        v[k] = omega * v[k] + phiP * rP * (p[k] - x[k]) + phiG * rG * (g[k] - x[k]);
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

                    // Compute fitness if feasibility is same or better.
                    if (Tools.BetterFeasible(bestAgentFeasible[j], newFeasible))
                    {
                        // Compute new fitness.
                        double newFitness = Problem.Fitness(x, bestAgentFitness[j], bestAgentFeasible[j], newFeasible);

                        // Update best-known position in case of fitness improvement.
                        if (Tools.BetterFeasibleFitness(bestAgentFeasible[j], newFeasible, bestAgentFitness[j], newFitness))
                        {
                            // Update best-known position.
                            // Contents must be copied because the agent
                            // will likely move to worse positions.
                            x.CopyTo(bestAgentPosition[j], 0);
                            bestAgentFitness[j] = newFitness;

                            // Update feasibility.
                            bestAgentFeasible[j] = newFeasible;

                            // Update swarm's best known position,
                            // if feasibility is same or better and fitness is an improvement.
                            // This must reference the agent's best-known
                            // position because the current position changes.
                            if (Tools.BetterFeasibleFitness(gFeasible, bestAgentFeasible[j], gFitness, bestAgentFitness[j]))
                            {
                                g = bestAgentPosition[j];
                                gFitness = bestAgentFitness[j];
                                gFeasible = bestAgentFeasible[j];
                            }
                        }
                    }

                    // Trace fitness of best found solution.
                    Trace(i, gFitness, gFeasible);
                }
            }

            // Signal end of optimization run.
            Problem.EndOptimizationRun();

            // Return best-found solution and fitness.
            return new Result(g, gFitness, gFeasible, i);
        }
        #endregion
    }
}