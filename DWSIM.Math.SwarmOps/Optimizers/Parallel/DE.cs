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
    /// Parallel version of DE which computes the fitness of its agents
    /// in parallel. Assumes the fitness function is thread-safe. Should
    /// only be used with very time-consuming optimization problems otherwise
    /// basic DE will execute faster because of less overhead.
    /// </summary>
    public class DE : Optimizer
    {
        #region Constructors.
        /// <summary>
        /// Construct the object.
        /// </summary>
        public DE()
            : this(1)
        {
        }

        /// <summary>
        /// Construct the object.
        /// </summary>
        /// <param name="problem">Problem to optimize.</param>
        public DE(Problem problem)
            : this(1, problem)
        {
        }

        /// <summary>
        /// Construct the object.
        /// </summary>
        /// <param name="numAgentsMultiple">Population size multiple, e.g. 4 ensures populations are sized 4, 8, 12, 16, ...</param>
        public DE(int numAgentsMultiple)
            : base()
        {
            NumAgentsMultiple = numAgentsMultiple;
        }

        /// <summary>
        /// Construct the object.
        /// </summary>
        /// <param name="numAgentsMultiple">Population size multiple, e.g. 4 ensures populations are sized 4, 8, 12, 16, etc.</param>
        /// <param name="problem">Problem to optimize.</param>
        public DE(int numAgentsMultiple, Problem problem)
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
            public static readonly double[] AllBenchmarks5Dim10000Iter = { 32.0, 0.4845, 0.9833 };

            /// <summary>
            /// Control parameters tuned for all benchmark problems in
            /// 30 dimensions and 60000 fitness evaluations in one optimization run.
            /// </summary>
            public static readonly double[] AllBenchmarks30Dim60000Iter = { 32.0, 0.3176, 0.5543 };
        }
        #endregion

        #region Get individual control parameters.
        /// <summary>
        /// Population size multiple, e.g. 4 ensures populations are sized 4, 8, 12, 16, etc.
        /// </summary>
        public int NumAgentsMultiple
        {
            get;
            protected set;
        }

        /// <summary>
        /// Get parameter, Number of agents, aka. population size.
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
        /// Get parameter, CR, aka. crossover probability.
        /// </summary>
        /// <param name="parameters">Optimizer parameters.</param>
        public double GetCR(double[] parameters)
        {

            return parameters[1];
        }

        /// <summary>
        /// Get parameter, F, aka. differential weight.
        /// </summary>
        /// <param name="parameters">Optimizer parameters.</param>
        public double GetF(double[] parameters)
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
            get { return "DE-Simple-Par" + NumAgentsMultiple; }
        }

        /// <summary>
        /// Number of control parameters for optimizer.
        /// </summary>
        public override int Dimensionality
        {
            get { return 3; }
        }

        string[] _parameterName = { "NP", "CR", "F" };

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

        static readonly double[] _lowerBound = { 3, 0, 0 };

        /// <summary>
        /// Lower search-space boundary for control parameters.
        /// </summary>
        public override double[] LowerBound
        {
            get { return _lowerBound; }
        }

        static readonly double[] _upperBound = { 200, 1, 2.0 };

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

            // Retrieve parameters specific to DE method.
            int numAgents = GetNumAgents(parameters);
            double CR = GetCR(parameters);
            double F = GetF(parameters);

            Debug.Assert(numAgents > 0);

            // Get problem-context.
            double[] lowerBound = Problem.LowerBound;
            double[] upperBound = Problem.UpperBound;
            double[] lowerInit = Problem.LowerInit;
            double[] upperInit = Problem.UpperInit;
            int n = Problem.Dimensionality;

            // Allocate agent positions, fitness and feasibility arrays.
            double[][] agentsX = Tools.NewMatrix(numAgents, n);
            double[][] agentsY = Tools.NewMatrix(numAgents, n);
            double[] fitnessX = new double[numAgents];
            double[] fitnessY = new double[numAgents];
            bool[] feasibleX = new bool[numAgents];
            bool[] feasibleY = new bool[numAgents];

            // Iteration variables.
            int i, j;

            // Fitness variables.
            double[] g = null;
            double gFitness = Problem.MaxFitness;
            bool gFeasible = false;

            // Initialize agent-position in search-space. (Non-parallel)
            for (j = 0; j < numAgents; j++)
            {
                // Initialize position.
                Tools.InitializeUniform(ref agentsX[j], lowerInit, upperInit);

                // Enforce constraints and evaluate feasibility.
                feasibleX[j] = Problem.EnforceConstraints(ref agentsX[j]);
            }

            // Compute fitness of initial positions. (Parallel)
            // This counts as iterations below.
            System.Threading.Tasks.Parallel.For(0, numAgents, Globals.ParallelOptions, (jPar) =>
            {
                fitnessX[jPar] = Problem.Fitness(agentsX[jPar], feasibleX[jPar]);
            });

            // Update best-found position. (Non-parallel)
            for (j = 0; j < numAgents; j++)
            {
                if (Tools.BetterFeasibleFitness(gFeasible, feasibleX[j], gFitness, fitnessX[j]))
                {
                    g = agentsX[j];
                    gFitness = fitnessX[j];
                    gFeasible = feasibleX[j];
                }

                // Trace fitness of best found solution.
                Trace(j, gFitness, gFeasible);
            }

            // Perform optimization.
            for (i = numAgents; Problem.Continue(i, gFitness, gFeasible); )
            {
                // Compute potential new position. (Non-parallel)
                for (j=0; j<numAgents; j++)
                {
                    // Refer to the j'th agent as x.
                    double[] x = agentsX[j];

                    // Refer to its potentially new position as y.
                    double[] y = agentsY[j];

                    // Pick a random dimension.
                    int R = Globals.Random.Index(n);

                    // Pick random and distinct agent-indices.
                    // Not necessarily distinct from x though.
                    int R1, R2;
                    Globals.Random.Index2(numAgents, out R1, out R2);

                    // Refer to the randomly picked agents as a and b.
                    double[] a = agentsX[R1];
                    double[] b = agentsX[R2];

                    // Compute potentially new position.
                    for (int k = 0; k < n; k++)
                    {
                        if (k == R || Globals.Random.Uniform() < CR)
                        {
                            y[k] = g[k] + F * (a[k] - b[k]);
                        }
                        else
                        {
                            y[k] = x[k];
                        }
                    }
                }

                // Compute fitness of y-position. (Parallel)
                System.Threading.Tasks.Parallel.For(0, numAgents, Globals.ParallelOptions, (jPar) =>
                {
                    // Enforce constraints and evaluate feasibility.
                    feasibleY[jPar] = Problem.EnforceConstraints(ref agentsY[jPar]);

                    // Compute fitness if feasibility (constraint satisfaction) is same or better.
                    if (Tools.BetterFeasible(feasibleX[jPar], feasibleY[jPar]))
                    {
                        fitnessY[jPar] = Problem.Fitness(agentsY[jPar], fitnessX[jPar], feasibleX[jPar], feasibleY[jPar]);
                    }
                });

                // Update agent-positions if improved fitness. (Non-parallel)
                for (j = 0; j < numAgents; j++, i++)
                {
                    // Update agent in case feasibility is same or better and fitness is improvement.
                    if (Tools.BetterFeasibleFitness(feasibleX[j], feasibleY[j], fitnessX[j], fitnessY[j]))
                    {
                        // Update agent's position.
                        agentsY[j].CopyTo(agentsX[j], 0);

                        // Update agent's fitness.
                        fitnessX[j] = fitnessY[j];

                        // Update agent's feasibility.
                        feasibleX[j] = feasibleY[j];

                        // Update swarm's best known position.
                        if (Tools.BetterFeasibleFitness(gFeasible, feasibleX[j], gFitness, fitnessX[j]))
                        {
                            g = agentsX[j];
                            gFitness = fitnessX[j];
                            gFeasible = feasibleX[j];
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