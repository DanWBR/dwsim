/// ------------------------------------------------------
/// RandomOps - (Pseudo) Random Number Generator For C#
/// Copyright (C) 2003-2010 Magnus Erik Hvass Pedersen.
/// Please see the file license.txt for license details.
/// RandomOps on the internet: http://www.Hvass-Labs.org/
/// ------------------------------------------------------

using System;
using System.Threading;

namespace RandomOps.ThreadSafe
{
    /// <summary>
    /// Wrapper for an RNG that makes the calls to Uniform(), Bool(), Byte(), and
    /// Bytes() thread-safe by locking the object. Note that it is the ThreadSafe
    /// object that is being locked and not the RNG object it wraps around, so calls
    /// to methods should always be made to the ThreadSafe-object and not to the
    /// RNG object directly. This works well for infrequent access to the RNG but
    /// for frequent access you should use e.g. ThreadSafe.MWC256 instead for
    /// performance reasons.
    /// </summary>
    /// <remarks>
    /// Derived calls are also thread-safe because they in turn call Uniform()
    /// and the other base-methods. If you use a custom RNG that have additional
    /// methods available which do not indirectly call one of the base-methods,
    /// then you must implement similar thread-safe wrappers. The RNG-object can
    /// potentially be locked several times by one thread because of how e.g.
    /// Bool() may call Uniform(), and such nested locking is supported in C#.
    /// It may be possible to implement more efficient locks for certain RNG's,
    /// e.g. for Ran2 we could instead implement a threadsafe version of the
    /// internal Rand() method.
    /// </remarks>
    public partial class Wrapper : Random
    {
        #region Constructor.
        /// <summary>
        /// Construct the thread-safe RNG wrapper.
        /// </summary>
        /// <param name="rand">The RNG to be made thread-safe.</param>
        public Wrapper(Random rand)
        {
            _rand = rand;
        }
        #endregion

        #region Public methods.
        /// <summary>
        /// Same as a call to Monitor.Enter() on the RNG-object (not the
        /// ThreadSafe-object). Useful if you need to generate several random
        /// numbers in a batch without having to reacquire the lock for
        /// each call to the RNG.
        /// </summary>
        public void Enter()
        {
            Monitor.Enter(this);
        }

        /// <summary>
        /// Same as a call to Monitor.Exit() on the RNG-object (not the
        /// ThreadSafe-object). Must be called once for each call to Enter()
        /// to release the lock again.
        /// </summary>
        public void Exit()
        {
            Monitor.Exit(this);
        }
        #endregion

        #region Internal variables.
        /// <summary>
        /// The non-thread-safe RNG used for generating the numbers.
        /// </summary>
        Random _rand;
        #endregion

        #region Base-class Overrides.
        /// <summary>
        /// Name of the RNG.
        /// </summary>
        public override string Name
        {
            get { return "ThreadSafe(" + _rand.Name + ")"; }
        }

        /// <summary>
        /// Thread-safe wrapper for Uniform(). Note that derived methods
        /// that rely on Uniform() to create their random numbers, e.g.
        /// Gauss(), will automatically also be thread-safe.
        /// </summary>
        public sealed override double Uniform()
        {
            double value;

            lock (this)
            {
                value = _rand.Uniform();
            }

            return value;
        }

        /// <summary>
        /// Thread-safe wrapper for Bool().
        /// </summary>
        public sealed override bool Bool()
        {
            bool b;

            lock (this)
            {
                b = _rand.Bool();
            }

            return b;
        }

        /// <summary>
        /// Thread-safe wrapper for Byte().
        /// </summary>
        public sealed override byte Byte()
        {
            byte b;

            lock (this)
            {
                b = _rand.Byte();
            }

            return b;
        }

        /// <summary>
        /// Thread-safe wrapper for Bytes().
        /// </summary>
        /// <param name="length">Number of random bytes to return.</param>
        /// <returns>Array of random bytes.</returns>
        public sealed override byte[] Bytes(int length)
        {
            byte[] arr;

            lock (this)
            {
                arr = _rand.Bytes(length);
            }

            return arr;
        }

        /// <summary>
        /// Thread-safe wrapper for Gauss().
        /// </summary>
        /// <remarks>
        /// The default implementation calls Uniform() a number of times.
        /// This wrapper therefore locks the RNG-object for the entirety
        /// of these multiple calls, instead of locking for each separate
        /// call to Uniform().
        /// </remarks>
        public sealed override double Gauss()
        {
            double value;

            lock (this)
            {
                value = _rand.Gauss();
            }

            return value;
        }
        #endregion
    }
}
