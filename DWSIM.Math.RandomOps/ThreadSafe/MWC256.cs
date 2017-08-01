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
    /// Thread-safe version of MWC256 which generates one PRNG object for each
    /// thread and seeds it using a MersenneTwister. The MWC256 is used because
    /// it has a very long period and hence decreases the risk of overlap and
    /// correlation of the individual threads.
    /// </summary>
    public class MWC256 : Independent
    {
        #region Constructors.
        /// <summary>
        /// Constructs the PRNG-object.
        /// </summary>
        public MWC256()
            : base()
        {
        }
        #endregion

        #region Thread-local PRNG
        /// <summary>
        /// Seeder PRNG used for seeding the thread-local PRNGs.
        /// </summary>
        /// <remarks>
        /// This is wrapped in a ThreadSafeWrapper because it may be accessed by multiple
        /// threads simultaneously. As this is just used for seeding the actual thread-local
        /// PRNGs the slow ThreadSafeWrapper will not cause a performance problem.
        /// </remarks>
        private static Random SeederPRNG = new Wrapper(new MersenneTwister());

        /// <summary>
        /// Thread-local PRNG.
        /// </summary>
        private ThreadLocal<Random> _threadRNG = new ThreadLocal<Random>(() => new RandomOps.MWC256(SeederPRNG));
        #endregion

        #region Base-class overrides.
        /// <summary>
        /// Name of the RNG.
        /// </summary>
        public override string Name
        {
            get { return "ThreadSafe.MWC256"; }
        }

        /// <summary>
        /// Thread-local PRNG.
        /// </summary>
        public override Random ThreadRNG
        {
            get { return _threadRNG.Value; }
        }
        #endregion
    }
}
