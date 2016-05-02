/// ------------------------------------------------------
/// SwarmOps - Numeric and heuristic optimization for C#
/// Copyright (C) 2003-2011 Magnus Erik Hvass Pedersen.
/// Please see the file license.txt for license details.
/// SwarmOps on the internet: http://www.Hvass-Labs.org/
/// ------------------------------------------------------

using System.Diagnostics;

namespace SwarmOps.Optimizers.Parallel
{
    /// <summary>
    /// Parallel version of MOL which computes the fitness of its agents
    /// in parallel. Assumes the fitness function is thread-safe. Should
    /// only be used with very time-consuming optimization problems otherwise
    /// basic MOL will execute faster because of less overhead.
    /// </summary>
    public class MOL : Optimizer
    {
        #region Constructors.
        /// <summary>
        /// Construct the object.
        /// </summary>
        public MOL()
            : this(1)
        {
        }

        /// <summary>
        /// Construct the object.
        /// </summary>
        /// <param name="problem">Problem to optimize.</param>
        public MOL(Problem problem)
            : this(1, problem)
        {
        }

        /// <summary>
        /// Construct the object.
        /// </summary>
        /// <param name="numAgentsMultiple">Population size multiple, e.g. 4 ensures populations are sized 4, 8, 12, 16, ...</param>
        public MOL(int numAgentsMultiple)
            : base()
        {
            NumAgentsMultiple = numAgentsMultiple;
        }

        /// <summary>
        /// Construct the object.
        /// </summary>
        /// <param name="numAgentsMultiple">Population size multiple, e.g. 4 ensures populations are sized 4, 8, 12, 16, etc.</param>
        /// <param name="problem">Problem to optimize.</param>
        public MOL(int numAgentsMultiple, Problem problem)
            : base(problem)
        {
            NumAgentsMultiple = numAgentsMultiple;
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
            /// 5 dimensions and 10000 fitness evaluations in one optimization run.
            /// </summary>
            public static readonly double[] AllBenchmarks5Dim10000Iter = { 32.0, -0.3319, 5.91 };

            /// <summary>
            /// Control parameters tuned for all benchmark problems in
            /// 30 dimensions and 60000 fitness evaluations in one optimization run.
            /// </summary>
            public static readonly double[] AllBenchmarks30Dim60000Iter = { 164.0, -0.4241, 2.7729 };
        }
        #endregion

        #region Get control parameters.
        /// <summary>
        /// Population size multiple, e.g. 4 ensures populations are sized 4, 8, 12, 16, etc.
        /// </summary>
        public int NumAgentsMultiple
        {
            get;
            protected set;
        }

        /// <summary>
        /// Get parameter, Number of agents, aka. swarm-size.
        /// </summary>
        /// <param name="parameters">Optimizer parameters.</param>
        public int GetNumAgents(double[] parameters)
        {
            int numAgents = (int)System.Math.Round(parameters[0], System.MidpointRounding.AwayFromZero);

            // Ensure numAgents falls on desired multiple.
            numAgents--;
            int mod = numAgents % NumAgentsMultiple;
            numAgents += NumAgentsMultiple - mod;

            return numAgents;
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
            get { return "MOL-Par" + NumAgentsMultiple; }
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

            Debug.Assert(numAgents > 0);

            // Get problem-context.
            double[] lowerBound = Problem.LowerBound;
            double[] upperBound = Problem.UpperBound;
            double[] lowerInit = Problem.LowerInit;
            double[] upperInit = Problem.UpperInit;
            int n = Problem.Dimensionality;

            // Allocate agent positions, velocities, etc.
            double[][] agents = Tools.NewMatrix(numAgents, n);
            double[][] velocities = Tools.NewMatrix(numAgents, n);
            double[] fitness = new double[numAgents];
            bool[] feasible = new bool[numAgents];

            // Allocate velocity boundaries.
            double[] velocityLowerBound = new double[n];
            double[] velocityUpperBound = new double[n];

            // Best-found position and fitness.
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

            // Initialize all agents. (Non-parallel)
            for (j = 0; j < numAgents; j++)
            {
                // Refer to the j'th agent as x and v.
                double[] x = agents[j];
                double[] v = velocities[j];

                // Initialize velocity.
                Tools.InitializeUniform(ref v, velocityLowerBound, velocityUpperBound);

                // Initialize agent-position in search-space.
                Tools.InitializeUniform(ref x, lowerInit, upperInit);

                // Enforce constraints and evaluate feasibility.
                feasible[j] = Problem.EnforceConstraints(ref x);
            }

            // Compute fitness of initial position. (Parallel)
            // This counts as iterations below.
            System.Threading.Tasks.Parallel.For(0, numAgents, Globals.ParallelOptions, (jPar) =>
            {
                fitness[jPar] = Problem.Fitness(agents[jPar], feasible[jPar]);
            });

            // Update swarm's best known position. (Non-parallel)
            for (j = 0; j < numAgents; j++)
            {
                if (Tools.BetterFeasibleFitness(gFeasible, feasible[j], gFitness, fitness[j]))
                {
                    agents[j].CopyTo(g, 0);
                    gFitness = fitness[j];
                    gFeasible = feasible[j];
                }

                // Trace fitness of best found solution.
                Trace(j, gFitness, gFeasible);
            }

            // Perform actual optimization iterations.
            for (i = numAgents; Problem.Continue(i, gFitness, gFeasible); )
            {
                // Update agent positions. (Non-parallel)
                for (j = 0; j < numAgents; j++)
                {
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
                }

                // Compute new fitness. (Parallel)
                System.Threading.Tasks.Parallel.For(0, numAgents, Globals.ParallelOptions, (jPar) =>
                {
                    // Enforce constraints and evaluate feasibility.
                    feasible[jPar] = Problem.EnforceConstraints(ref agents[jPar]);

                    // Compute fitness if feasibility is same or better.
                    if (Tools.BetterFeasible(gFeasible, feasible[jPar]))
                    {
                        fitness[jPar] = Problem.Fitness(agents[jPar], gFitness, gFeasible, feasible[jPar]);
                    }
                });

                // Update swarm's best known position. (Non-parallel)
                for (j = 0; j < numAgents; j++, i++)
                {
                    if (Tools.BetterFeasibleFitness(gFeasible, feasible[j], gFitness, fitness[j]))
                    {
                        agents[j].CopyTo(g, 0);
                        gFitness = fitness[j];
                        gFeasible = feasible[j];
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