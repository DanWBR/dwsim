/// ------------------------------------------------------
/// RandomOps - (Pseudo) Random Number Generator For C#
/// Copyright (C) 2003-2010 Magnus Erik Hvass Pedersen.
/// Please see the file license.txt for license details.
/// RandomOps on the internet: http://www.Hvass-Labs.org/
/// ------------------------------------------------------

using System.Collections.Generic;

namespace RandomOps
{
    /// <summary>
    /// Similar to the RandomDotOrg-class, only this uses asynchronous retrieval
    /// of the random bytes, which is preferrable in time-critical applications.
    /// If bytes are not available in the downloaded buffer, then a thread is
    /// woken up to fill the buffer. In the meantime a Fallback RNG is used instead.
    /// Not thread-safe.
    /// </summary>
    /// <remarks>
    /// Be sure to call Dispose() after you are done using this RNG,
    /// so the worker-thread can be shut down properly. Also note
    /// that this class is not thread-safe with regard to multiple
    /// threads calling e.g. the Uniform() method simultaneously.
    /// To make it thread-safe in this manner, wrap it in a
    /// ThreadSafe-object.
    /// </remarks>
    public class RandomDotOrgAsync : ByteStreamAsync
    {
        #region Constructor.
        /// <summary>
        /// Constructs the RNG-object.
        /// </summary>
        /// <param name="bufferSize">Number of random bytes the buffer holds.</param>
        /// <param name="retrieveTrigger">Refill buffer asynchronously when its size falls below this.</param>
        /// <param name="randFallback">Fallback RNG to be used when buffer is empty.</param>
        /// <param name="numFallback">Use fallback RNG for this many bytes before trying to fill buffer again.</param>
        public RandomDotOrgAsync(int bufferSize, int retrieveTrigger, Random randFallback, int numFallback)
            : base(bufferSize, retrieveTrigger, randFallback, numFallback)
        {
        }
        #endregion

        #region Base-class overrides.
        /// <summary>
        /// Name of the RNG.
        /// </summary>
        public override string Name
        {
            get { return "Random.Org Async"; }
        }

        /// <summary>
        /// The www.random.org website only allows 10000 elements
        /// to be retrieved at once.
        /// </summary>
        protected override int MaxRetrieveLength
        {
            get { return 10000; }
        }

        /// <summary>
        /// Called from ByteStream to request the retrieval of random bytes.
        /// </summary>
        /// <param name="length">The number of bytes requested.</param>
        protected sealed override void DoFillBuffer(int length)
        {
            // Re-use the RandomDotOrg.RetrieveBytes() method.

            IEnumerable<byte> bytes = RandomDotOrg.RetrieveBytes(length);

            AddBuffer(bytes);
        }
        #endregion
    }
}
