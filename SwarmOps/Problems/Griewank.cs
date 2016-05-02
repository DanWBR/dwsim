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
    /// Griewank benchmark problem.
    /// </summary>
    public class Griewank : Benchmark
    {
        #region Constructors.
        /// <summary>
        /// Construct the object.
        /// </summary>
        /// <param name="dimensionality">Dimensionality of the problem (e.g. 20)</param>
        /// <param name="maxIterations">Max optimization iterations to perform.</param>
        public Griewank(int dimensionality, int maxIterations)
            : base(dimensionality, -600, 600, 300, 600, maxIterations)
        {
        }
        #endregion

        #region Base-class overrides.
        /// <summary>
        /// Name of the optimization problem.
        /// </summary>
        public override string Name
        {
            get { return "Griewank"; }
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

            double sum = 0, prod = 1;

            for (int i = 0; i < Dimensionality; i++)
            {
                double elm = x[i];

                sum += elm * elm;
                prod *= System.Math.Cos(elm / System.Math.Sqrt((double)(i + 1)));
            }

            return sum / 4000 - prod + 1;
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

                double rec = 1.0 / System.Math.Sqrt((double)(i + 1));
                double val2 = System.Math.Sin(elm * rec) * rec;

                for (int j = 0; j < Dimensionality; j++)
                {
                    if (i != j)
                    {
                        val2 *= System.Math.Cos(x[j] / System.Math.Sqrt((double)(j + 1)));
                    }
                }

                v[i] = elm * 1.0 / 2000 + val2;
            }

            return Dimensionality;
        }
        #endregion
    }
}
