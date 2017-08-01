/// ------------------------------------------------------
/// RandomOps - (Pseudo) Random Number Generator For C#
/// Copyright (C) 2003-2010 Magnus Erik Hvass Pedersen.
/// Please see the file license.txt for license details.
/// RandomOps on the internet: http://www.Hvass-Labs.org/
/// ------------------------------------------------------

using System.Threading;

namespace RandomOps.ThreadSafe
{
    /// <summary>
    /// Base-class for making thread-safe PRNGs by creating one PRNG object
    /// for each thread. The PRNGs should have very long periods to avoid
    /// overlapping. See ThreadSafeMWC256 for an example. This is a simple
    /// and reliable way of making an efficient and parallel PRNG, see the
    /// paper: P.D. Coddington, Random Number Generators for Parallel Computers,
    /// NPAC Technical Report, Version 1.1, 1997.
    /// </summary>
    public abstract class Independent : Random
    {
        #region Constructors.
        /// <summary>
        /// Constructs the PRNG-object.
        /// </summary>
        public Independent()
            : base()
        {
        }
        #endregion

        #region Thread-local PRNG
        /// <summary>
        /// Thread-local PRNG.
        /// </summary>
        public abstract Random ThreadRNG
        {
            get;
        }
        #endregion

        #region Base-class overrides.
        /// <summary>
        /// Draw a uniform random number in the exclusive range (0,1). Thread-safe.
        /// </summary>
        public override double Uniform()
        {
            return ThreadRNG.Uniform();
        }

        /// <summary>
        /// Draw a Gaussian (or normally) distributed random number, with mean 0 and
        /// deviation 1. Thread-safe.
        /// </summary>
        public override double Gauss()
        {
            return ThreadRNG.Gauss();
        }
        #endregion
    }
}
