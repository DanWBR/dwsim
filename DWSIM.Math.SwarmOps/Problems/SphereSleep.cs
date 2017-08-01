/// ------------------------------------------------------
/// SwarmOps - Numeric and heuristic optimization for C#
/// Copyright (C) 2003-2011 Magnus Erik Hvass Pedersen.
/// Please see the file license.txt for license details.
/// SwarmOps on the internet: http://www.Hvass-Labs.org/
/// ------------------------------------------------------

using System.Diagnostics;
using System.Threading;

namespace SwarmOps.Problems
{
    /// <summary>
    /// Sphere benchmark problem with thread-sleeping to simulate a time-consuming
    /// problem.
    /// </summary>
    public class SphereSleep : Sphere
    {
        #region Constructors.
        /// <summary>
        /// Construct the object.
        /// </summary>
        /// <param name="dimensionality">Dimensionality of the problem (e.g. 20)</param>
        /// <param name="maxIterations">Max optimization iterations to perform.</param>
        public SphereSleep(int sleepMilliSeconds, int dimensionality, int maxIterations)
            : base(dimensionality, maxIterations)
        {
            SleepMilliSeconds = sleepMilliSeconds;
        }
        #endregion

        #region Public properties.
        /// <summary>
        /// Sleep-time in milli-seconds after each fitness evaluation.
        /// </summary>
        public int SleepMilliSeconds
        {
            get;
            protected set;
        }
        #endregion

        #region Base-class overrides.
        /// <summary>
        /// Name of the optimization problem.
        /// </summary>
        public override string Name
        {
            get { return "SphereSleep"; }
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
                value += elm * elm;
            }

            Thread.Sleep(SleepMilliSeconds);

            return value;
        }
        #endregion
    }
}
