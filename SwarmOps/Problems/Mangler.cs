/// ------------------------------------------------------
/// SwarmOps - Numeric and heuristic optimization for C#
/// Copyright (C) 2003-2011 Magnus Erik Hvass Pedersen.
/// Please see the file license.txt for license details.
/// SwarmOps on the internet: http://www.Hvass-Labs.org/
/// ------------------------------------------------------

using System;
using System.Diagnostics;

namespace SwarmOps.Problems
{
    /// <summary>
    /// Search-space mangler, used to increase the difficulty of
    /// optimizing benchmark problems and avoid correlation with
    /// global optima such as zero. Note that this works with
    /// parallel optimizers but not with parallel meta-optimization
    /// because of the way MetaFitness is implemented.
    /// </summary>
    public class Mangler : ProblemWrapper
    {
        #region Constructors.
        /// <summary>
        /// Constructs a new object.
        /// </summary>
        /// <param name="problem">The problem being wrapped.</param>
        /// <param name="diffusion">Diffusion factor, larger than 0, e.g. 0.01</param>
        /// <param name="displacement">Displacement factor, larger than 0, e.g. 0.1</param>
        /// <param name="fitnessNoise">FitnessNoise factor, lager than 0, e.g. 0.01</param>
        /// <param name="spillover">Spillover factor, larger than 0, e.g. 0.05</param>
        public Mangler(Problem problem,
            double diffusion,
            double displacement,
            double spillover,
            double fitnessNoise)
            : base(problem)
        {
            // Assign to fields.
            Diffusion = diffusion;
            Displacement = displacement;
            Spillover = spillover;
            FitnessNoise = fitnessNoise;

            // Allocate transformation matrix and array.
            int n = problem.Dimensionality;
            A = Tools.NewMatrix(n, n);
            b = new double[n];
        }
        #endregion

        #region Public fields.
        /// <summary>
        /// Diffusion factor.
        /// </summary>
        public double Diffusion
        {
            get;
            protected set;
        }

        /// <summary>
        /// Displacement factor.
        /// </summary>
        public double Displacement
        {
            get;
            protected set;
        }

        /// <summary>
        /// Spillover factor.
        /// </summary>
        public double Spillover
        {
            get;
            protected set;
        }

        /// <summary>
        /// FitnessNoise factor.
        /// </summary>
        public double FitnessNoise
        {
            get;
            protected set;
        }
        #endregion

        #region Private fields.
        /// <summary>
        /// Matrix used for transforming input position: y = A * x + b
        /// </summary>
        double[][] A;

        /// <summary>
        /// Array used for transforming input position: y = A * x + b
        /// </summary>
        double[] b;
        #endregion

        #region Base-class overrides.
        /// <summary>
        /// Compute fitness of wrapped problem and print the result.
        /// </summary>
        public override double Fitness(double[] parameters, double fitnessLimit, bool oldFeasible, bool newFeasible)
        {
            // Notational convenience.
            int n = Problem.Dimensionality;
            double[] x = parameters;

            // Iterator variables.
            int i, j;

            // Allocate output array. This is thread-safe and fast enough.
            double[] y = new double[n];

            // Matrix transformation y = A * x + b
            for (i = 0; i < n; i++)
            {
                y[i] = b[i];

                for (j = 0; j < n; j++)
                {
                    y[i] += A[i][j] * x[j];
                }
            }

            // Diffusion.
            for (i = 0; i < n; i++)
            {
                y[i] += Globals.Random.Gauss(0, Diffusion);
            }

            // Compute fitness.
            double fitness = Problem.Fitness(y, fitnessLimit, oldFeasible, newFeasible);

            // Fitness noise.
            double noise = System.Math.Abs(Globals.Random.Gauss()) * FitnessNoise + 1;
            fitness *= noise;

            // Compute and return fitness of displaced parameters.
            return fitness;
        }

        /// <summary>
        /// Gradient is not defined for mangled search-spaces.
        /// </summary>
        public override bool HasGradient
        {
            get { return false; }
        }

        /// <summary>
        /// Compute the gradient of the fitness-function.
        /// This is not defined for mangled search-spaces.
        /// </summary>
        /// <param name="x">Candidate solution.</param>
        /// <param name="v">Array for holding the gradient.</param>
        public override int Gradient(double[] x, ref double[] v)
        {
            throw new NotImplementedException();
        }

        /// <summary>
        /// At beginning of new optimization run create a random mangling.
        /// </summary>
        public override void BeginOptimizationRun()
        {
            // Notational convenience.
            int n = Problem.Dimensionality;

            // Iterator variables.
            int i, j;

            // Initialize transformation matrix.
            for (i = 0; i < n; i++)
            {
                for (j = 0; j < n; j++)
                {
                    // The diagonal of the transformation matrix is one, the remainder
                    // is chosen randomly.
                    A[i][j] = (i == j) ? (1) : (Globals.Random.Gauss(0, Spillover));
                }
            }

            // Initialize displacement array.
            for (i = 0; i < n; i++)
            {
                double range = Problem.UpperBound[i] - Problem.LowerBound[i];
                double d = Displacement * Globals.Random.Uniform(-range, range);
                b[i] = d;
            }

            // Call through problem-chain.
            Problem.BeginOptimizationRun();
        }
        #endregion
    }
}
