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
    /// Penalized1 benchmark problem.
    /// </summary>
    public class Penalized1 : Penalized
    {
        #region Constructors.
        /// <summary>
        /// Construct the object.
        /// </summary>
        /// <param name="dimensionality">Dimensionality of the problem (e.g. 20)</param>
        /// <param name="maxIterations">Max optimization iterations to perform.</param>
        public Penalized1(int dimensionality, int maxIterations)
            : base(dimensionality, -50, 50, 5, 50, maxIterations)
        {
        }
        #endregion

        #region Base-class overrides.
        /// <summary>
        /// Name of the optimization problem.
        /// </summary>
        public override string Name
        {
            get { return "Penalized1"; }
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

            // Compute main fitness value ...
            double value = GetSinY(x[0]);

            int i;
            for (i = 0; i < Dimensionality - 1; i++)
            {
                double elmY = GetY(x[i]);
                double elmYMinusOne = elmY - 1;

                value += (elmYMinusOne * elmYMinusOne) * (1 + GetSinY(x[i + 1]));
            }

            // Add last y-term.
            {
                double elmY = GetY(x[Dimensionality - 1]);
                double elmYMinusOne = elmY - 1;

                value += elmYMinusOne * elmYMinusOne;
            }

            // Compute penalty.
            double penalty = 0;

            for (i = 0; i < Dimensionality; i++)
            {
                double elm = x[i];

                penalty += U(elm, 10.0, 100.0, 4.0);
            }

            return System.Math.PI * value / Dimensionality + penalty;
        }
        #endregion

        #region Helper methods.
        /// <summary>
        /// Helper-method used in Fitness method.
        /// </summary>
        protected double GetY(double x)
        {
            return 1 + 0.25 * (x + 1);
        }

        /// <summary>
        /// Helper-method used in Fitness method.
        /// </summary>
        protected double GetSinY(double x)
        {
            double elmY = GetY(x);
            double elmSinY = System.Math.Sin(System.Math.PI * elmY);

            return 10 * elmSinY * elmSinY;
        }

        #endregion
    }
}
