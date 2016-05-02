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
    /// Penalized2 benchmark problem.
    /// </summary>
    public class Penalized2 : Penalized
    {
        #region Constructors.
        /// <summary>
        /// Construct the object.
        /// </summary>
        /// <param name="dimensionality">Dimensionality of the problem (e.g. 20)</param>
        /// <param name="maxIterations">Max optimization iterations to perform.</param>
        public Penalized2(int dimensionality, int maxIterations)
            : base(dimensionality, -50, 50, -5, 50, maxIterations)
        {
        }
        #endregion

        #region Base-class overrides.
        /// <summary>
        /// Name of the optimization problem.
        /// </summary>
        public override string Name
        {
            get { return "Penalized2"; }
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
            double value = GetSinX(x[0], 3.0);

            int i;
            for (i = 0; i < Dimensionality - 1; i++)
            {
                double elm = x[i];
                double elmMinusOne = elm - 1;
                double elmSin = GetSinX(x[i + 1], 3.0);

                value += (elmMinusOne * elmMinusOne) * (1 + elmSin);
            }

            // Add last term.
            {
                double elm = x[Dimensionality - 1];
                double elmMinusOne = elm - 1;
                double elmSin = GetSinX(elm, 2.0);

                value += elmMinusOne * elmMinusOne * (1 + elmSin);
            }

            // Compute penalty.
            double penalty = 0;

            for (i = 0; i < Dimensionality; i++)
            {
                double elm = x[i];

                penalty += U(elm, 5.0, 100.0, 4.0);
            }

            return 0.1 * value + penalty;
        }
        #endregion

        #region Helper methods.
        /// <summary>
        /// Helper-method used in Fitness method.
        /// </summary>
        protected double GetSinX(double x, double factor)
        {
            double elmSin = System.Math.Sin(factor * System.Math.PI * x);

            return elmSin * elmSin;
        }
        #endregion
    }
}
