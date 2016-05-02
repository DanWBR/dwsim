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
    /// due to Storner and Price (1). jDE variant due to Brest
    /// et al. (2). This variant claims to be 'self-adaptive' in
    /// that it claims to eliminate the need to choose two
    /// parameters of the original DE, but in reality it
    /// introduces an additional 6 parameters, so the jDE variant
    /// now has 9 parameters instead of just 3 of the original DE.
    /// </summary>
    /// <remarks>
    /// References:
    /// (1) R. Storn and K. Price. Differential evolution - a simple
    ///     and efficient heuristic for global optimization over
    ///     continuous spaces. Journal of Global Optimization,
    ///     11:341-359, 1997.
    /// (2) J. Brest, S. Greiner, B. Boskovic, M. Mernik, and V. Zumer.
    ///     Self-adapting control parameters in differential evolution:
    ///     a comparative study on numerical benchmark functions. IEEE
    ///     Transactions on Evolutionary Computation, 10(6):646-657, 2006.
    /// </remarks>
    public class JDE : Optimizer
    {
        #region Constructors.
        /// <summary>
        /// Construct the object.
        /// </summary>
        /// <param name="crossover">Crossover variant.</param>
        public JDE(DECrossover.Variant crossover)
            : base()
        {
            SetVariant(crossover);
        }

        /// <summary>
        /// Construct the object.
        /// </summary>
        /// <param name="problem">Problem to optimize.</param>
        /// <param name="crossover">Crossover variant.</param>
        public JDE(Problem problem, DECrossover.Variant crossover)
            : base(problem)
        {
            SetVariant(crossover);
        }
        #endregion

        #region Optimizer variant configuration.
        /// <summary>
        /// Set the optimizer variant to be used.
        /// </summary>
        /// <param name="crossover">Crossover variant.</param>
        void SetVariant(DECrossover.Variant crossover)
        {
            _crossover = crossover;
        }

        /// <summary>
        /// DE crossover variant to be used.
        /// </summary>
        DECrossover.Variant _crossover;
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
                /// Default and presumably hand-tuned parameters from jDE paper and source-code.
                /// </summary>
                public static readonly double[] HandTuned = { 100.0, 0.5, 0.1, 0.9, 0.1, 0.9, 0, 1, 0.1 };

                /// <summary>
                /// Good choice of control parameters for use with all 12 benchmark
                /// problems in 30 dimensions each, when using 6000 fitness evaluations
                /// per optimization run.
                /// </summary>
                public static readonly double[] AllBenchmarks30Dim6000Iter = { 28.0, 0.4304, 0.3009, 0.6392, 0.6449, 0.5, 0.4826, 0.9356, 0.1805 };

                /// <summary>
                /// Good choice of control parameters for use with all 12 benchmark
                /// problems in 30 dimensions each, when using 60000 fitness evaluations
                /// per optimization run.
                /// </summary>
                public static readonly double[] AllBenchmarks30Dim60000Iter = { 32.0, 0.6068, 0.3462, 1.1388, 0.0561, 0.0947, 0.9166, 0.7104, 0.018 };

                /// <summary>
                /// Control parameters tuned for four benchmark problems (Rastrigin,
                /// Schwefel1-2, Schwefel2-21, Schwefel2-22) in 30 dimensions each
                /// and 6000 fitness evaluations in one optimization run.
                /// </summary>
                public static readonly double[] FourBenchmarks30Dim6000Iter = { 77.0, 1.8074, 0.2417, 1.0375, 0.5336, 0.3911, 0.9893, 0.4157, 0.8041 };

                /// <summary>
                /// Control parameters tuned for four benchmark problems (Ackley,
                /// Rastrigin, Rosenbrock, Schwefel1-2) in 30 dimensions each
                /// and 600000 fitness evaluations in one optimization run.
                /// </summary>
                public static readonly double[] FourBenchmarks30Dim600000Iter = { 82.0, 1.5127, 0.4136, 0.1835, 0.8375, 0.0478, 0.4655, 0.6714, 0.0729 };

                /// <summary>
                /// Control parameters tuned for Sphere and Rosenbrock problems in 30
                /// dimensions each and 60000 fitness evaluations in one optimization run.
                /// </summary>
                public static readonly double[] SphereRosenbrock30Dim60000Iter = { 14.0, 0.2091, 0.6697, 0.0962, 0.2369, 0.1943, 0.0676, 0.6439, 0.6275 };

                /// <summary>
                /// Control parameters tuned for Rastrigin and Schwefel1-2 problems in 30
                /// dimensions each and 60000 fitness evaluations in one optimization run.
                /// </summary>
                public static readonly double[] RastriginSchwefel12_30Dim60000Iter = { 40.0, 1.8855, 0.103, 0.9366, 0.2843, 0.7029, 0.3101, 0.9137, 0.0909 };

                /// <summary>
                /// Control parameters tuned for QuarticNoise, Sphere, Step problems in 30
                /// dimensions each and 60000 fitness evaluations in one optimization run.
                /// </summary>
                public static readonly double[] QuarticNoiseSphereStep_30Dim60000Iter = { 97.0, 1.8255, 0.0437, 1.1422, 0.2514, 0.8331, 0.7161, 0.1705, 0.9808 };

                /// <summary>
                /// Control parameters tuned for the Rosenbrock problem in 30 dimensions and
                /// 60000 fitness evaluations in one optimization run.
                /// </summary>
                public static readonly double[] Rosenbrock_30Dim60000Iter = { 19.0, 1.1531, 0.4339, 1.3545, 0.1696, 0.3607, 0.311, 0.8671, 0.0618 };

                /// <summary>
                /// Control parameters tuned for the Schwefel1-2 problem in 30 dimensions and
                /// 60000 fitness evaluations in one optimization run.
                /// </summary>
                public static readonly double[] Schwefel12_30Dim60000Iter = { 28.0, 0.9733, 0.2937, 0.7191, 0.9093, 0.2309, 0.6364, 0.5477, 0.0835 };
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
        /// Get parameter, FInit.
        /// </summary>
        /// <param name="parameters">Optimizer parameters.</param>
        public double GetFInit(double[] parameters)
        {
            return parameters[1];
        }

        /// <summary>
        /// Get parameter, Fl.
        /// </summary>
        /// <param name="parameters">Optimizer parameters.</param>
        public double GetFl(double[] parameters)
        {
            return parameters[2];
        }

        /// <summary>
        /// Get parameter, Fu.
        /// </summary>
        /// <param name="parameters">Optimizer parameters.</param>
        public double GetFu(double[] parameters)
        {
            return parameters[3];
        }

        /// <summary>
        /// Get parameter, TauF.
        /// </summary>
        /// <param name="parameters">Optimizer parameters.</param>
        public double GetTauF(double[] parameters)
        {
            return parameters[4];
        }

        /// <summary>
        /// Get parameter, CRInit.
        /// </summary>
        /// <param name="parameters">Optimizer parameters.</param>
        public double GetCRInit(double[] parameters)
        {
            return parameters[5];
        }

        /// <summary>
        /// Get parameter, CRl.
        /// </summary>
        /// <param name="parameters">Optimizer parameters.</param>
        public double GetCRl(double[] parameters)
        {
            return parameters[6];
        }

        /// <summary>
        /// Get parameter, CRu.
        /// </summary>
        /// <param name="parameters">Optimizer parameters.</param>
        public double GetCRu(double[] parameters)
        {
            return parameters[7];
        }

        /// <summary>
        /// Get parameter, TauCR.
        /// </summary>
        /// <param name="parameters">Optimizer parameters.</param>
        public double GetTauCR(double[] parameters)
        {
            return parameters[8];
        }
        #endregion

        #region Base-class overrides, Problem.
        /// <summary>
        /// Name of the optimizer.
        /// </summary>
        public override string Name
        {
            get { return "JDE-" + DECrossover.Name(_crossover); }
        }

        /// <summary>
        /// Number of control parameters for optimizer.
        /// </summary>
        public override int Dimensionality
        {
            get { return 9; }
        }

        string[] _parameterName = {"NP", "F_{init}", "F_l", "F_u", "tau_{F}", "CR_{init}", "CR_l", "CR_u", "tau_{CR}"};

        /// <summary>
        /// Control parameter names.
        /// </summary>
        public override string[] ParameterName
        {
            get { return _parameterName; }
        }

        static readonly double[] _defaultParameters = { 8.0, 0.453133, 0.247631, 1.548331, 0.659707, 0.847650, 0.104456, 0.122205, 0.875351 };

        /// <summary>
        /// Default control parameters.
        /// </summary>
        public override double[] DefaultParameters
        {
            get { return _defaultParameters; }
        }

        static readonly double[] _lowerBound = { 4, 0, 0, 0, 0, 0, 0, 0, 0 };

        /// <summary>
        /// Lower search-space boundary for control parameters.
        /// </summary>
        public override double[] LowerBound
        {
            get { return _lowerBound; }
        }

        static readonly double[] _upperBound = { 200, 2.0, 2.0, 2.0, 1, 1, 1, 1, 1 };

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

            // Retrieve parameters specific to JDE method.
            int numAgents = GetNumAgents(parameters);

            double FInit = GetFInit(parameters);
            double Fl = GetFl(parameters);
            double Fu = GetFu(parameters);
            double tauF = GetTauF(parameters);

            double CRInit = GetCRInit(parameters);
            double CRl = GetCRl(parameters);
            double CRu = GetCRu(parameters);
            double tauCR = GetTauCR(parameters);

            // Adjust CR parameters to remain within [0,1]
            if (CRl + CRu > 1)
            {
                CRu = 1 - CRl;
            }

            // Get problem-context.
            double[] lowerInit = Problem.LowerInit;
            double[] upperInit = Problem.UpperInit;
            int n = Problem.Dimensionality;

            // Allocate agent positions and associated fitnesses.
            double[][] agents = Tools.NewMatrix(numAgents, n);
            double[] fitness = new double[numAgents];
            bool[] feasible = new bool[numAgents];
            double[] y = new double[n];
            double[] w = new double[n];
            double[] F = new double[numAgents];
            double[] CR = new double[numAgents];

            // Initialize 'self-adaptive' parameters.
            Tools.Initialize(ref F, FInit);
            Tools.Initialize(ref CR, CRInit);

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

                for (j = 0; j < numAgents && Problem.Continue(i, gFitness, gFeasible); j++, i++)
                {
                    // Reset the random-set used for picking distinct agents.
                    // Exclude the j'th agent (also referred to as x).
                    randomSet.ResetExclude(j);

                    // Refer to the j'th agent as x.
                    double[] x = agents[j];

                    // JDE 'Self-adaptive' parameters.
                    double newF = (Globals.Random.Bool(tauF)) ? (Globals.Random.Uniform(Fl, Fl + Fu)) : (F[j]);
                    double newCR = (Globals.Random.Bool(tauCR)) ? (Globals.Random.Uniform(CRl, CRl + CRu)) : (CR[j]);

                    // Initialize crossover-weight vector.
                    Tools.Initialize(ref w, newF);

                    // Perform crossover.
                    DECrossover.DoCrossover(_crossover, newCR, n, w, x, ref y, g, agents, randomSet);

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

                            // JDE 'Self-adaptive' parameters.
                            // Keep the new parameters because they led to fitness improvement.
                            F[j] = newF;
                            CR[j] = newCR;
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