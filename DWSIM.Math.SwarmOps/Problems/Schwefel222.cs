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
    /// Schwefel 2-22 benchmark problem.
    /// </summary>
    public class Schwefel222 : Benchmark
    {
        /// <summary>
        /// Construct the object.
        /// </summary>
        /// <param name="dimensionality">Dimensionality of the problem (e.g. 20)</param>
        /// <param name="maxIterations">Max optimization iterations to perform.</param>
        public Schwefel222(int dimensionality, int maxIterations)
            : base(dimensionality, -10, 10, 5, 10, maxIterations)
        {
        }

        #region Base-class overrides.
        /// <summary>
        /// Name of the optimization problem.
        /// </summary>
        public override string Name
        {
            get { return "Schwefel2-22"; }
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

            double sum = 0;
            double product = 1;

            for (int i = 0; i < Dimensionality; i++)
            {
                double absElm = System.Math.Abs(x[i]);

                sum += absElm;
                product *= absElm;
            }

            return sum + product;
        }
        #endregion
    }
}
