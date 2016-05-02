/// ------------------------------------------------------
/// SwarmOps - Numeric and heuristic optimization for C#
/// Copyright (C) 2003-2011 Magnus Erik Hvass Pedersen.
/// Please see the file license.txt for license details.
/// SwarmOps on the internet: http://www.Hvass-Labs.org/
/// ------------------------------------------------------

using System.Threading.Tasks;

namespace SwarmOps
{
    /// <summary>
    /// Collection of globally used static variables.
    /// </summary>
    public static partial class Globals
    {
        /// <summary>
        /// Random Number Generator.
        /// </summary>
        public static RandomOps.Random Random;

        /// <summary>
        /// Options used in parallel processing, e.g. the max number of threads.
        /// </summary>
        public static ParallelOptions ParallelOptions = new ParallelOptions();
    }
}
