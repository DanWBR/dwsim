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
    /// Differential Evolution (DE) optimizer originally
    /// due to Storner and Price (1). This suite offers combinations
    /// of DE variants and various perturbation schemes for its
    /// behavioural parameters. Note that this has complicated the
    /// implementation somewhat.
    /// </summary>
    /// <remarks>
    /// References:
    /// (1) R. Storn and K. Price. Differential evolution - a simple
    ///     and efficient heuristic for global optimization over
    ///     continuous spaces. Journal of Global Optimization,
    ///     11:341-359, 1997.
    /// </remarks>
    public class DESuite : Optimizer
    {
        #region Constructors.
        /// <summary>
        /// Construct the object.
        /// </summary>
        /// <param name="crossover">Crossover variant to be used.</param>
        /// <param name="dither">Dither variant to be used.</param>
        public DESuite(DECrossover.Variant crossover, DitherVariant dither)
            : base()
        {
            // Set DE operator variants to be used.
            SetVariant(crossover, dither);
        }

        /// <summary>
        /// Construct the object.
        /// </summary>
        /// <param name="problem">Problem to optimize.</param>
        /// <param name="crossover">Crossover variant to be used.</param>
        /// <param name="dither">Dither variant to be used.</param>
        public DESuite(Problem problem, DECrossover.Variant crossover, DitherVariant dither)
            : base(problem)
        {
            // Set DE operator variants to be used.
            SetVariant(crossover, dither);
        }
        #endregion

        #region Optimizer variant configuration.
        /// <summary>
        /// Enumerate the variants of dithering for the differential weight.
        /// </summary>
        public enum DitherVariant
        {
            /// <summary>
            /// No dither.
            /// </summary>
            None,

            /// <summary>
            /// Use same dithering for entire generation.
            /// </summary>
            Generation,

            /// <summary>
            /// Use same dithering for entire vector. (Aka. Dither.)
            /// </summary>
            Vector,

            /// <summary>
            /// Use new dithering for each element of vector. (Aka. Jitter.)
            /// </summary>
            Element,

            /// <summary>
            /// Use new dithering for each element of vector. (Aka. Jitter.)
            /// </summary>
            Jitter = Element
        }

        /// <summary>
        /// Set DE operator variants to be used, and determine the number
        /// of behavioural parameters associated with that variant.
        /// </summary>
        /// <param name="crossover">Crossover variant to be used.</param>
        /// <param name="dither">Dither variant to be used.</param>
        void SetVariant(DECrossover.Variant crossover, DitherVariant dither)
        {
            _dither = dither;
            _crossover = crossover;

            if (_dither == DitherVariant.None)
            {
                _dimensionality = 3;
            }
            else
            {
                _dimensionality = 4;
            }
        }

        protected DitherVariant _dither;
        protected DECrossover.Variant _crossover;

        /// <summary>
        /// Name of crossover operator.
        /// </summary>
        protected string CrossoverName
        {
            get
            {
                return DECrossover.Name(_crossover);
            }
        }

        /// <summary>
        /// Name of dither variant.
        /// </summary>
        protected string DitherName
        {
            get
            {
                string s;

                switch (_dither)
                {
                    case DitherVariant.None:
                        s = "";
                        break;

                    case DitherVariant.Generation:
                        s = "-GenDither";
                        break;

                    case DitherVariant.Vector:
                        s = "-VecDither";
                        break;

                    case DitherVariant.Element:
                        s = "-Jitter";
                        break;

                    default:
                        s = "-UnknownDither";
                        break;
                }

                return s;
            }
        }
        #endregion

        #region Sets of control parameters.
        /// <summary>
        /// Control parameters.
        /// </summary>
        public struct Parameters
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
                    /// tuned for all benchmark problems in
                    /// 2 dimensions and 400 fitness evaluations in one optimization run.
                    /// </summary>
                    public static readonly double[] AllBenchmarks2Dim400IterA = { 13.0, 0.745, 0.9096 };

                    /// <summary>
                    /// Control parameters for use with Rand1Bin crossover, No Dither,
                    /// tuned for all benchmark problems in
                    /// 2 dimensions and 400 fitness evaluations in one optimization run.
                    /// </summary>
                    public static readonly double[] AllBenchmarks2Dim400IterB = { 10.0, 0.4862, 1.1922 };

                    /// <summary>
                    /// Control parameters for use with Rand1Bin crossover, No Dither,
                    /// tuned for all benchmark problems in
                    /// 2 dimensions and 4000 fitness evaluations in one optimization run.
                    /// </summary>
                    public static readonly double[] AllBenchmarks2Dim4000IterA = { 24, 0.2515, 0.8905 };

                    /// <summary>
                    /// Control parameters for use with Rand1Bin crossover, No Dither,
                    /// tuned for all benchmark problems in
                    /// 2 dimensions and 4000 fitness evaluations in one optimization run.
                    /// </summary>
                    public static readonly double[] AllBenchmarks2Dim4000IterB = { 20.0, 0.7455, 0.9362 };

                    /// <summary>
                    /// Control parameters for use with Rand1Bin crossover, No Dither,
                    /// tuned for all benchmark problems in
                    /// 5 dimensions and 1000 fitness evaluations in one optimization run.
                    /// </summary>
                    public static readonly double[] AllBenchmarks5Dim1000Iter = { 17.0, 0.7122, 0.6301 };

                    /// <summary>
                    /// Control parameters for use with Rand1Bin crossover, No Dither,
                    /// tuned for all benchmark problems in
                    /// 5 dimensions and 10000 fitness evaluations in one optimization run.
                    /// </summary>
                    public static readonly double[] AllBenchmarks5Dim10000Iter = { 20.0, 0.6938, 0.9314 };

                    /// <summary>
                    /// Control parameters for use with Rand1Bin crossover, No Dither,
                    /// tuned for all benchmark problems in
                    /// 10 dimensions and 2000 fitness evaluations in one optimization run.
                    /// </summary>
                    public static readonly double[] AllBenchmarks10Dim2000IterA = { 28.0, 0.9426, 0.6607 };

                    /// <summary>
                    /// Control parameters for use with Rand1Bin crossover, No Dither,
                    /// tuned for all benchmark problems in
                    /// 10 dimensions and 2000 fitness evaluations in one optimization run.
                    /// </summary>
                    public static readonly double[] AllBenchmarks10Dim2000IterB = { 12.0, 0.2368, 0.6702 };

                    /// <summary>
                    /// Control parameters for use with Rand1Bin crossover, No Dither,
                    /// tuned for all benchmark problems in
                    /// 10 dimensions and 20000 fitness evaluations in one optimization run.
                    /// </summary>
                    public static readonly double[] AllBenchmarks10Dim20000Iter = { 18.0, 0.5026, 0.6714 };

                    /// <summary>
                    /// Control parameters for use with Rand1Bin crossover, No Dither,
                    /// tuned for all benchmark problems in
                    /// 20 dimensions and 40000 fitness evaluations in one optimization run.
                    /// </summary>
                    public static readonly double[] AllBenchmarks20Dim40000Iter = { 37.0, 0.9455, 0.6497 };

                    /// <summary>
                    /// Control parameters for use with Rand1Bin crossover, No Dither,
                    /// tuned for Ackley, Rastrigin, Rosenbrock, and Schwefel1-2 in
                    /// 20 dimensions and 400000 fitness evaluations in one optimization run.
                    /// </summary>
                    public static readonly double[] FourBenchmarks20Dim400000Iter = { 35.0, 0.4147, 0.5983 };

                    /// <summary>
                    /// Control parameters for use with Rand1Bin crossover, No Dither,
                    /// tuned for all benchmark problems in
                    /// 50 dimensions and 100000 fitness evaluations in one optimization run.
                    /// </summary>
                    public static readonly double[] AllBenchmarks50Dim100000Iter = { 48.0, 0.9784, 0.6876 };

                    /// <summary>
                    /// Control parameters for use with Rand1Bin crossover, No Dither,
                    /// tuned for Ackley, Rastrigin, Rosenbrock, and Schwefel1-2 in
                    /// 100 dimensions and 200000 fitness evaluations in one optimization run.
                    /// </summary>
                    public static readonly double[] FourBenchmarks100Dim200000Iter = { 46.0, 0.9565, 0.5824 };

                    /// <summary>
                    /// Mesh-tuned parameters, from mesh search when fixing CR=0.9 with various
                    /// combinations of benchmark problems and optimization run-lengths.
                    /// </summary>
                    public static readonly double[] ParametersMeshTuned = { 50.0, 0.9, 0.6 };

                    /// <summary>
                    /// Control parameters tuned for four benchmark problems (Ackley,
                    /// Rastrigin, Rosenbrock, Schwefel1-2) in 30 dimensions each
                    /// and 600000 fitness evaluations in one optimization run.
                    /// </summary>
                    public static readonly double[] FourBenchmarks30Dim600000Iter = { 75.0, 0.8803, 0.4717 };

                    /// <summary>
                    /// Control parameters for use with Rand1Bin crossover, No Dither,
                    /// tuned for all benchmark problems in 30 dimensions and 6000
                    /// fitness evaluations in one optimization run.
                    /// </summary>
                    public static readonly double[] AllBenchmarks30Dim6000Iter = { 11.0, 0.0877, 0.6419 };

                    /// <summary>
                    /// Control parameters for use with Rand1Bin crossover, No Dither,
                    /// tuned for all benchmark problems in 30 dimensions and 60000
                    /// fitness evaluations in one optimization run.
                    /// </summary>
                    public static readonly double[] AllBenchmarks30Dim60000Iter = { 19.0, 0.122, 0.4983 };

                    /// <summary>
                    /// Control parameters for use with Rand1Bin crossover, No Dither,
                    /// tuned for all benchmark problems except Schwefel1-2 in 30
                    /// dimensions and 60000 fitness evaluations in one optimization run.
                    /// </summary>
                    public static readonly double[] AllBenchmarks_NoSchwefel12_30Dim60000Iter = { 19.0, 0.4616, 0.5314 };
                }

                /// <summary>
                /// Control parameters for use with Rand1Bin crossover, Jitter.
                /// </summary>
                public struct Jitter
                {
                    /// <summary>
                    /// Hand-tuned control parameters for use with Rand1Bin crossover, Jitter.
                    /// </summary>
                    public static readonly double[] HandTuned = { 50.0, 0.9, 0.5, 0.0005 };

                    /// <summary>
                    /// Control parameters for use with Rand1Bin crossover, Jitter,
                    /// tuned for all benchmark problems in 30 dimensions and 6000
                    /// fitness evaluations in one optimization run.
                    /// </summary>
                    public static readonly double[] AllBenchmarks30Dim6000Iter = { 14.0, 0.0509, 0.0134, 1.0599 };

                    /// <summary>
                    /// Control parameters for use with Rand1Bin crossover, Jitter,
                    /// tuned for all benchmark problems in 30 dimensions and 60000
                    /// fitness evaluations in one optimization run.
                    /// </summary>
                    public static readonly double[] AllBenchmarks30Dim60000Iter = { 58.0, 0.9048, 0.3989, 0.3426 };
                }

                /// <summary>
                /// Control parameters for use with Rand1Bin crossover, VecDither.
                /// </summary>
                public struct VecDither
                {
                    /// <summary>
                    /// Hand-tuned control parameters for use with Rand1Bin crossover, VecDither.
                    /// </summary>
                    public static readonly double[] HandTuned = { 50.0, 0.9, 0.75, 0.25 };

                    /// <summary>
                    /// Control parameters for use with Rand1Bin crossover, VecDither,
                    /// tuned for all benchmark problems in 30 dimensions and 6000
                    /// fitness evaluations in one optimization run.
                    /// </summary>
                    public static readonly double[] AllBenchmarks30Dim6000Iter = { 14.0, 0.0835, 0.5536, 0.1395 };

                    /// <summary>
                    /// Control parameters for use with Rand1Bin crossover, VecDither,
                    /// tuned for all benchmark problems in 30 dimensions and 60000
                    /// fitness evaluations in one optimization run.
                    /// </summary>
                    public static readonly double[] AllBenchmarks30Dim60000Iter = { 102.0, 0.9637, 0.7876, 0.7292 };
                }
            }
        }
        #endregion

        #region Get individual control parameters.
        /// <summary>
        /// Get parameter, Number of agents, aka. population size.
        /// </summary>
        /// <param name="parameters">Optimizer parameters.</param>
        public int GetNumAgents(double[] parameters)
        {
            return (int)System.Math.Round(parameters[0], System.MidpointRounding.AwayFromZero);
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
            return (_dither == DitherVariant.None) ? (parameters[2]) : (0);
        }

        /// <summary>
        /// Get parameter, FMid, aka. differential weight dithering midpoint.
        /// </summary>
        /// <param name="parameters">Optimizer parameters.</param>
        public double GetFMid(double[] parameters)
        {
            return (_dither != DitherVariant.None) ? (parameters[2]) : (0);
        }

        /// <summary>
        /// Get parameter, FRange, aka. differential weight dithering range.
        /// </summary>
        /// <param name="parameters">Optimizer parameters.</param>
        public double GetFRange(double[] parameters)
        {
            return (_dither != DitherVariant.None) ? (parameters[3]) : (0);
        }
        #endregion

        #region Base-class overrides, Problem.
        /// <summary>
        /// Name of the optimizer.
        /// </summary>
        public override string Name
        {
            get { return "DE-" + CrossoverName + DitherName; }
        }

        int _dimensionality;

        /// <summary>
        /// Number of control parameters for optimizer.
        /// </summary>
        public override int Dimensionality
        {
            get { return _dimensionality; }
        }

        string[] _parameterName = { "NP", "CR", "F" };
        string[] _parameterNameDither = { "NP", "CR", "FMid", "FRange" };

        /// <summary>
        /// Control parameter names.
        /// </summary>
        public override string[] ParameterName
        {
            get { return (_dither == DitherVariant.None) ? (_parameterName) : (_parameterNameDither); }
        }

        static readonly double[] _defaultParameters = { 37.0, 0.496, 0.5313 };
        static readonly double[] _defaultParametersDither = { 9.0, 0.5749, 1.1862, 2.1832 };

        /// <summary>
        /// Default control parameters.
        /// </summary>
        public override double[] DefaultParameters
        {
            get { return (_dither == DitherVariant.None) ? (_defaultParameters) : (_defaultParametersDither); }
        }

        static readonly double[] _lowerBound = { 4, 0, 0 };
        static readonly double[] _lowerBoundDither = { 4, 0, 0, 0 };

        /// <summary>
        /// Lower search-space boundary for control parameters.
        /// </summary>
        public override double[] LowerBound
        {
            get { return (_dither == DitherVariant.None) ? (_lowerBound) : (_lowerBoundDither); }
        }

        static readonly double[] _upperBound = { 200, 1, 2.0 };
        static readonly double[] _upperBoundDither = { 200, 1, 2.0, 3.0 };

        /// <summary>
        /// Upper search-space boundary for control parameters.
        /// </summary>
        public override double[] UpperBound
        {
            get { return (_dither == DitherVariant.None) ? (_upperBound) : (_upperBoundDither); }
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

            // Get problem-context.
            double[] lowerInit = Problem.LowerInit;
            double[] upperInit = Problem.UpperInit;
            int n = Problem.Dimensionality;

            // Allocate agent positions and associated fitnesses and feasibility.
            double[][] agents = Tools.NewMatrix(numAgents, n);
            double[] fitness = new double[numAgents];
            bool[] feasible = new bool[numAgents];
            double[] y = new double[n];

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

            // Initialize all agents.
            // This counts as iterations below.
            for (j = 0; j < numAgents && Problem.Continue(j, gFitness, gFeasible); j++)
            {
                // Refer to the j'th agent as x.
                double[] x = agents[j];

                // Initialize agent-position in search-space.
                Tools.InitializeUniform(ref x, lowerInit, upperInit);

                // Enforce constraints and evaluate feasibility.
                feasible[j] = Problem.EnforceConstraints(ref x);

                // Compute fitness of initial position.
                fitness[j] = Problem.Fitness(x, feasible[j]);

                // Update population's best known position if it either does not exist or,
                // if feasibility is same or better and fitness is an improvement.
                if (Tools.BetterFeasibleFitness(gFeasible, feasible[j], gFitness, fitness[j]))
                {
                    g = agents[j];
                    gFitness = fitness[j];
                    gFeasible = feasible[j];
                }

                // Trace fitness of best found solution.
                Trace(j, gFitness, gFeasible);
            }

            for (i = numAgents; Problem.Continue(i, gFitness, gFeasible); )
            {
                Debug.Assert(numAgents > 0);

                // Perform dithering of differential weight, depending on dither variant wanted.
                if (_dither == DitherVariant.Generation)
                {
                    // Initialize differential-weight vector. Generation-based.
                    Tools.Initialize(ref w, Globals.Random.Uniform(FMid - FRange, FMid + FRange));
                }

                for (j = 0; j < numAgents && Problem.Continue(i, gFitness, gFeasible); j++, i++)
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

                    // Refer to the j'th agent as x.
                    double[] x = agents[j];

                    // Perform crossover.
                    DECrossover.DoCrossover(_crossover, CR, n, w, x, ref y, g, agents, randomSet);

                    // Enforce constraints and evaluate feasibility.
                    bool newFeasible = Problem.EnforceConstraints(ref y);

                    // Compute fitness if feasibility (constraint satisfaction) is same or better.
                    if (Tools.BetterFeasible(feasible[j], newFeasible))
                    {
                        // Compute new fitness.
                        double newFitness = Problem.Fitness(y, fitness[j], feasible[j], newFeasible);

                        // Update agent in case of fitness improvement.
                        if (Tools.BetterFeasibleFitness(feasible[j], newFeasible, fitness[j], newFitness))
                        {
                            // Update agent's position.
                            y.CopyTo(agents[j], 0);

                            // Update agent's fitness.
                            fitness[j] = newFitness;

                            // Update agent's feasibility.
                            feasible[j] = newFeasible;

                            // Update population's best known position,
                            // if feasibility is same or better and fitness is an improvement.
                            if (Tools.BetterFeasibleFitness(gFeasible, newFeasible, gFitness, newFitness))
                            {
                                g = agents[j];
                                gFitness = newFitness;
                                gFeasible = newFeasible;
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