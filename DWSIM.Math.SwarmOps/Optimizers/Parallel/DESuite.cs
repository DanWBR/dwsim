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
    /// Parallel version of DESuite which computes the fitness of its agents
    /// in parallel. Assumes the fitness function is thread-safe. Should
    /// only be used with very time-consuming optimization problems otherwise
    /// basic DESuite will execute faster because of less overhead.
    /// </summary>
    public class DESuite : SwarmOps.Optimizers.DESuite
    {
        #region Constructors.
        /// <summary>
        /// Construct the object.
        /// </summary>
        /// <param name="crossover">Crossover variant to be used.</param>
        /// <param name="dither">Dither variant to be used.</param>
        public DESuite(DECrossover.Variant crossover, DitherVariant dither)
            : this(1, crossover, dither)
        {
        }

        /// <summary>
        /// Construct the object.
        /// </summary>
        /// <param name="problem">Problem to optimize.</param>
        /// <param name="crossover">Crossover variant to be used.</param>
        /// <param name="dither">Dither variant to be used.</param>
        public DESuite(Problem problem, DECrossover.Variant crossover, DitherVariant dither)
            : this(1, problem, crossover, dither)
        {
        }

        /// <summary>
        /// Construct the object.
        /// </summary>
        /// <param name="numAgentsMultiple">Population size multiple, e.g. 4 ensures populations are sized 4, 8, 12, 16, ...</param>
        /// <param name="crossover">Crossover variant to be used.</param>
        /// <param name="dither">Dither variant to be used.</param>
        public DESuite(int numAgentsMultiple, DECrossover.Variant crossover, DitherVariant dither)
            : base(crossover, dither)
        {
            NumAgentsMultiple = numAgentsMultiple;
        }

        /// <summary>
        /// Construct the object.
        /// </summary>
        /// <param name="numAgentsMultiple">Population size multiple, e.g. 4 ensures populations are sized 4, 8, 12, 16, ...</param>
        /// <param name="problem">Problem to optimize.</param>
        /// <param name="crossover">Crossover variant to be used.</param>
        /// <param name="dither">Dither variant to be used.</param>
        public DESuite(int numAgentsMultiple, Problem problem, DECrossover.Variant crossover, DitherVariant dither)
            : base(problem, crossover, dither)
        {
            NumAgentsMultiple = numAgentsMultiple;
        }
        #endregion

        #region Sets of control parameters.
        /// <summary>
        /// Control parameters.
        /// </summary>
        public new struct Parameters
        {
            /// <summary>
            /// Control parameters for use with Rand1Bin crossover.
            /// </summary>
            public struct Rand1Bin
            {
                /// <summary>
                /// Control parameters for use with Rand1Bin crossover, No Dither.
                /// </summary>
                public struct NoDither
                {
                    /// <summary>
                    /// Hand-tuned control parameters for use with Rand1Bin crossover, No Dither.
                    /// </summary>
                    public static readonly double[] HandTuned = { 300.0, 0.9, 0.5 };

                    /// <summary>
                    /// Control parameters for use with Rand1Bin crossover, No Dither,
                    /// tuned for all benchmark problems in 5 dimensions and 10000
                    /// fitness evaluations in one optimization run.
                    /// </summary>
                    public static readonly double[] AllBenchmarks5Dim10000Iter = { 32.0, 0.7653, 0.998 };

                    /// <summary>
                    /// Control parameters for use with Rand1Bin crossover, No Dither,
                    /// tuned for all benchmark problems in 30 dimensions and 60000
                    /// fitness evaluations in one optimization run.
                    /// </summary>
                    public static readonly double[] AllBenchmarks30Dim60000Iter = { 128.0, 0.9489, 0.455 };
                }
            }
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
        public new int GetNumAgents(double[] parameters)
        {
            int numAgents = (int)System.Math.Round(parameters[0], System.MidpointRounding.AwayFromZero);

            // Ensure numAgents falls on desired multiple.
            numAgents--;
            int mod = numAgents % NumAgentsMultiple;
            numAgents += NumAgentsMultiple - mod;

            return numAgents;
        }
        #endregion

        #region Base-class overrides, Problem.
        /// <summary>
        /// Name of the optimizer.
        /// </summary>
        public override string Name
        {
            get { return "DE-" + CrossoverName + DitherName + "-Par" + NumAgentsMultiple; }
        }

        static readonly double[] _defaultParametersDither = { 9.0, 0.5749, 1.1862, 2.1832 };

        /// <summary>
        /// Default control parameters.
        /// </summary>
        public override double[] DefaultParameters
        {
            get { return (_dither == DitherVariant.None) ? (Parameters.Rand1Bin.NoDither.AllBenchmarks30Dim60000Iter) : (_defaultParametersDither); }
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
            double FMid = GetFMid(parameters);
            double FRange = GetFRange(parameters);

            Debug.Assert(numAgents > 0);

            // Get problem-context.
            double[] lowerBound = Problem.LowerBound;
            double[] upperBound = Problem.UpperBound;
            double[] lowerInit = Problem.LowerInit;
            double[] upperInit = Problem.UpperInit;
            int n = Problem.Dimensionality;

            // Allocate agent positions and associated fitnesses.
            double[][] agentsX = Tools.NewMatrix(numAgents, n);
            double[][] agentsY = Tools.NewMatrix(numAgents, n);
            double[] fitnessX = new double[numAgents];
            double[] fitnessY = new double[numAgents];
            bool[] feasibleX = new bool[numAgents];
            bool[] feasibleY = new bool[numAgents];

            // Allocate differential weight vector.
            double[] w = new double[n];

            // Initialize differential weight vector, if no dithering is wanted.
            if (_dither == DitherVariant.None)
            {
                // Initialize crossover-weight vector.
                // Same value for all elements, vectors, and generations.
                Tools.Initialize(ref w, F);
            }

            // Random set for picking distinct agents.
            RandomOps.Set randomSet = new RandomOps.Set(Globals.Random, numAgents);

            // Iteration variables.
            int i, j;

            // Fitness variables.
            double[] g = null;
            double gFitness = Problem.MaxFitness;
            bool gFeasible = false;

            // Initialize all agents. (Non-parallel)
            for (j = 0; j < numAgents; j++)
            {
                // Initialize agent-position in search-space.
                Tools.InitializeUniform(ref agentsX[j], lowerInit, upperInit);

                // Enforce constraints and evaluate feasibility.
                feasibleX[j] = Problem.EnforceConstraints(ref agentsX[j]);
            }

            // This counts as iterations below.
            // Compute fitness of initial position.
            System.Threading.Tasks.Parallel.For(0, numAgents, Globals.ParallelOptions, (jPar) =>
            {
                fitnessX[jPar] = Problem.Fitness(agentsX[jPar], feasibleX[jPar]);
            });

            // Update best found position. (Non-parallel)
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

            for (i = numAgents; Problem.Continue(i, gFitness, gFeasible); )
            {
                // Perform dithering of differential weight, depending on dither variant wanted.
                if (_dither == DitherVariant.Generation)
                {
                    // Initialize differential-weight vector. Generation-based.
                    Tools.Initialize(ref w, Globals.Random.Uniform(FMid - FRange, FMid + FRange));
                }

                // Update agent positions. (Non-parallel)
                for (j = 0; j < numAgents; j++)
                {
                    // Perform dithering of differential weight, depending on dither variant wanted.
                    if (_dither == DitherVariant.Vector)
                    {
                        // Initialize differential-weight vector. Vector-based.
                        Tools.Initialize(ref w, Globals.Random.Uniform(FMid - FRange, FMid + FRange));
                    }
                    else if (_dither == DitherVariant.Element)
                    {
                        // Initialize differential-weight vector. Element-based.
                        Tools.InitializeUniform(ref w, FMid - FRange, FMid + FRange);
                    }

                    // Reset the random-set used for picking distinct agents.
                    // Exclude the j'th agent (also referred to as x).
                    randomSet.ResetExclude(j);

                    // Perform crossover.
                    DECrossover.DoCrossover(_crossover, CR, n, w, agentsX[j], ref agentsY[j], g, agentsX, randomSet);
                }

                // Compute new fitness. (Parallel)
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

                // Update agent positions. (Non-parallel)
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