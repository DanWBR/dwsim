/// ------------------------------------------------------
/// SwarmOps - Numeric and heuristic optimization for C#
/// Copyright (C) 2003-2011 Magnus Erik Hvass Pedersen.
/// Please see the file license.txt for license details.
/// SwarmOps on the internet: http://www.Hvass-Labs.org/
/// ------------------------------------------------------

using System.Diagnostics;

namespace SwarmOps
{
    public static partial class Tools
    {
        /// <summary>
        /// Initialize array with value.
        /// </summary>
        /// <param name="x">Array to be initialized.</param>
        /// <param name="value">Value.</param>
        public static void Initialize(ref double[] x, double value)
        {
            for (int i = 0; i < x.Length; i++)
            {
                x[i] = value;
            }
        }

        /// <summary>
        /// Initialize array with uniform random values between given boundaries.
        /// </summary>
        /// <param name="x">Array to be initialized.</param>
        /// <param name="lower">Lower boundary.</param>
        /// <param name="upper">Upper boundary.</param>
        public static void InitializeUniform(ref double[] x, double lower, double upper)
        {
            for (int i = 0; i < x.Length; i++)
            {
                x[i] = Globals.Random.Uniform(lower, upper);
            }
        }

        /// <summary>
        /// Initialize array with uniform random values between given boundaries.
        /// </summary>
        /// <param name="x">Array to be initialized.</param>
        /// <param name="lower">Array of lower boundary.</param>
        /// <param name="upper">Array of upper boundary.</param>
        public static void InitializeUniform(ref double[] x, double[] lower, double[] upper)
        {
            for (int i = 0; i < x.Length; i++)
            {
                x[i] = Globals.Random.Uniform(lower[i], upper[i]);
            }
        }

        /// <summary>
        /// Initialize array with the range between the boundaries.
        /// That is, x[i] = upper[i]-lower[i].
        /// </summary>
        /// <param name="x">Array to be initialized.</param>
        /// <param name="lower">Lower boundary.</param>
        /// <param name="upper">Upper boundary.</param>
        public static void InitializeRange(ref double[] x, double[] lower, double[] upper)
        {
            for (int i = 0; i < x.Length; i++)
            {
                x[i] = upper[i] - lower[i];

                Debug.Assert(x[i] >= 0);
            }
        }
    }
}
