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
        /// Pick a uniform random sample from [x-range, x+range].
        /// </summary>
        public static double Sample(double x, double range)
        {
            Debug.Assert(range >= 0);

            // Pick a sample from within the bounded range.
            double y = Globals.Random.Uniform(x - range, x + range);

            return y;
        }

        /// <summary>
        /// First bound the sampling range [x-range, x+range] using lowerBound
        /// and upperBound respectively, and then pick a uniform random sample
        /// from the bounded range. This avoids samples being boundary points.
        /// </summary>
        public static double SampleBounded(double x, double range, double lowerBound, double upperBound)
        {
            Debug.Assert(lowerBound < upperBound);
            Debug.Assert(range >= 0);

            double l, u;	// Sampling range.
            double y;		// Actual sample.

            // Adjust sampling range so it does not exceed bounds.
            l = System.Math.Max(x - range, lowerBound);
            u = System.Math.Min(x + range, upperBound);

            // Pick a sample from within the bounded range.
            y = Globals.Random.Uniform(l, u);

            return y;
        }
    }
}
