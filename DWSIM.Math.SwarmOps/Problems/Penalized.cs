/// ------------------------------------------------------
/// SwarmOps - Numeric and heuristic optimization for C#
/// Copyright (C) 2003-2011 Magnus Erik Hvass Pedersen.
/// Please see the file license.txt for license details.
/// SwarmOps on the internet: http://www.Hvass-Labs.org/
/// ------------------------------------------------------

namespace SwarmOps.Problems
{
    /// <summary>
    /// Helper-methods for Penalized benchmark problems.
    /// </summary>
    public abstract class Penalized : Benchmark
    {
        #region Constructors.
        /// <summary>
        /// Construct the object.
        /// </summary>
        /// <param name="dimensionality">Dimensionality of the problem.</param>
        /// <param name="lowerBound">Lower boundary for entire search-space.</param>
        /// <param name="upperBound">Upper boundary for entire search-space.</param>
        /// <param name="lowerInit">Lower boundary for initialization.</param>
        /// <param name="upperInit">Upper boundary for initialization.</param>
        /// <param name="maxIterations">Max optimization iterations to perform.</param>
        public Penalized(
            int dimensionality,
            double lowerBound,
            double upperBound,
            double lowerInit,
            double upperInit,
            int maxIterations)
            : base(dimensionality, lowerBound, upperBound, lowerInit, upperInit, maxIterations)
        {
        }
        #endregion

        #region Penalize helper methods.
        /// <summary>
        /// Helper-method for Penalized benchmark problems.
        /// </summary>
        protected double U(double x, double a, double k, double m)
        {
            double value;

            if (x < -a)
            {
                value = k * System.Math.Pow(-x - a, m);
            }
            else if (x > a)
            {
                value = k * System.Math.Pow(x - a, m);
            }
            else
            {
                value = 0;
            }

            return value;
        }
        #endregion
    }
}
