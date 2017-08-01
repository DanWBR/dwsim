/// ------------------------------------------------------
/// SwarmOps - Numeric and heuristic optimization for C#
/// Copyright (C) 2003-2011 Magnus Erik Hvass Pedersen.
/// Please see the file license.txt for license details.
/// SwarmOps on the internet: http://www.Hvass-Labs.org/
/// ------------------------------------------------------

using System.Linq;

namespace SwarmOps
{
    public static partial class Tools
    {
        /// <summary>
        /// Euclidian norm (or length) of a numeric vector.
        /// </summary>
        public static double Norm(double[] x)
        {
            return System.Math.Sqrt(x.Aggregate((double)0.0, (sum, elm) => sum += elm * elm));
        }
    }
}
