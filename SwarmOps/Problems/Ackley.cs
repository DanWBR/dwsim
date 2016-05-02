/// ------------------------------------------------------
/// SwarmOps - Numeric and heuristic optimization for C#
/// Copyright (C) 2003-2011 Magnus Erik Hvass Pedersen.
/// Please see the file license.txt for license details.
/// SwarmOps on the internet: http://www.Hvass-Labs.org/
/// ------------------------------------------------------

using System.Diagnostics;

namespace SwarmOps.Problems
{
    /// <summary>
    /// Ackley benchmark problem.
    /// </summary>
    public class Ackley : Benchmark
    {
        #region Constructors.
        /// <summary>
        /// Construct the object.
        /// </summary>
        /// <param name="dimensionality">Dimensionality of the problem (e.g. 20)</param>
        /// <param name="maxIterations">Max optimization iterations to perform.</param>
        public Ackley(int dimensionality, int maxIterations)
            : base(dimensionality, -30, 30, 15, 30, maxIterations)
        {
        }
        #endregion

        #region Base-class overrides.
        /// <summary>
        /// Name of the optimization problem.
        /// </summary>
        public override string Name
        {
            get { return "Ackley"; }
        }

        /// <summary>
        /// Minimum possible fitness.
        /// </summary>
        public override double MinFitness
        {
            get { return 0; }
        }

        /// <summary>
        /// Compute and return fitness for the given parameters.
        /// </summary>
        /// <param name="x">Candidate solution.</param>
        public override double Fitness(double[] x)
        {
            Debug.Assert(x != null && x.Length == Dimensionality);

            double fitness
                = System.Math.E
                + 20
                - 20 * System.Math.Exp(-0.2 * SqrtSum(x))
                - CosSum(x);

            // Rounding errors may cause negative fitnesses to occur even
            // though the mathematical global minimum has fitness zero.
            // Ensure this still works with meta-optimization which
            // requires non-negative fitnesses.
            if (fitness < 0)
            {
                fitness = 0;
            }

            return fitness;
        }

        /// <summary>
        /// Return whether the gradient has been implemented.
        /// </summary>
        public override bool HasGradient
        {
            get { return true; }
        }

        /// <summary>
        /// Compute the gradient of the fitness-function.
        /// </summary>
        /// <param name="x">Candidate solution.</param>
        /// <param name="v">Array for holding the gradient.</param>
        public override int Gradient(double[] x, ref double[] v)
        {
            Debug.Assert(x != null && x.Length == Dimensionality);
            Debug.Assert(v != null && v.Length == Dimensionality);

            double sqrtSum = SqrtSum(x);
            double cosSum = CosSum(x);

            double DimRec = 1.0 / Dimensionality;

            for (int i = 0; i < Dimensionality; i++)
            {
                double elm = x[i];

                v[i] = 4 * DimRec * System.Math.Exp(-0.2 * sqrtSum) * elm / sqrtSum
                       + cosSum * System.Math.Sin(System.Math.PI * 2 * elm) * System.Math.PI * 2 * DimRec;
            }

            return 0;
        }
        #endregion

        #region Protected methods.
        /// <summary>
        /// Helper-method used in both the Fitness- and Gradient-methods.
        /// </summary>
        protected double SqrtSum(double[] x)
        {
            double sum = 0;
            int n = x.Length;

            for (int i = 0; i < n; i++)
            {
                double elm = x[i];

                sum += elm * elm;
            }

            return System.Math.Sqrt(sum / n);
        }

        /// <summary>
        /// Helper-method used in both the Fitness- and Gradient-methods.
        /// </summary>
        protected double CosSum(double[] x)
        {
            double sum = 0;
            int n = x.Length;

            for (int i = 0; i < n; i++)
            {
                double elm = x[i];
                sum += System.Math.Cos(System.Math.PI * 2 * elm);
            }

            return System.Math.Exp(sum / n);
        }
        #endregion
    }
}
