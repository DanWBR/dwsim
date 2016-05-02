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
    /// Rastrigin benchmark problem.
    /// </summary>
    public class Rastrigin : Benchmark
    {
        #region Constructors.
        /// <summary>
        /// Construct the object.
        /// </summary>
        /// <param name="dimensionality">Dimensionality of the problem (e.g. 20)</param>
        /// <param name="maxIterations">Max optimization iterations to perform.</param>
        public Rastrigin(int dimensionality, int maxIterations)
            : base(dimensionality, -5.12, 5.12, 2.56, 5.12, maxIterations)
        {
        }
        #endregion

        #region Base-class overrides.
        /// <summary>
        /// Name of the optimization problem.
        /// </summary>
        public override string Name
        {
            get { return "Rastrigin"; }
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

            double value = 0;

            for (int i = 0; i < Dimensionality; i++)
            {
                double elm = x[i];
                value += elm * elm + 10 - 10 * System.Math.Cos(System.Math.PI * 2 * elm);
            }

            return value;
        }

        /// <summary>
        /// Has the gradient has been implemented?
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

            for (int i = 0; i < Dimensionality; i++)
            {
                double elm = x[i];
                v[i] = 2 * elm + System.Math.PI * 2 * 10 * System.Math.Sin(System.Math.PI * 2 * elm);
            }

            return 0;
        }
        #endregion
    }
}
