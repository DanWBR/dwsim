/// ------------------------------------------------------
/// SwarmOps - Numeric and heuristic optimization for C#
/// Copyright (C) 2003-2011 Magnus Erik Hvass Pedersen.
/// Please see the file license.txt for license details.
/// SwarmOps on the internet: http://www.Hvass-Labs.org/
/// ------------------------------------------------------

namespace SwarmOps
{
    public static partial class Tools
    {
        /// <summary>
        /// The limit below which the number x is considered denormalized.
        /// </summary>
        static readonly double DenormalizeLimit = 1e-30;

        /// <summary>
        /// If Abs(x) is below a limit then it is set to zero.
        /// </summary>
        public static double DenormalizeTruncate(double x)
        {
            return (System.Math.Abs(x) > DenormalizeLimit) ? (x) : (0);
        }

        /// <summary>
        /// Return denormalized version of x. This is useful for
        /// avoiding expensive CPU usage when x underflows.
        /// </summary>
        public static double Denormalize(double x)
        {
            return DenormalizeTruncate(x);
        }

        /// <summary>
        /// Denormalize an array.
        /// </summary>
        public static void Denormalize(ref double[] x)
        {
            for (int i = 0; i < x.Length; i++)
            {
                x[i] = Tools.Denormalize(x[i]);
            }
        }
    }
}
