/// ------------------------------------------------------
/// RandomOps - (Pseudo) Random Number Generator For C#
/// Copyright (C) 2003-2010 Magnus Erik Hvass Pedersen.
/// Please see the file license.txt for license details.
/// RandomOps on the internet: http://www.Hvass-Labs.org/
/// ------------------------------------------------------

using System;
using System.Collections.Generic;
using System.Threading;

namespace RandomOps
{
    /// <summary>
    /// A version of the ByteStream RNG base-class which works
    /// asynchronously by creating its own thread on which the
    /// buffer refilling is executed. This allows for immediate use
    /// of the fallback RNG instead of waiting for the buffer to
    /// be refilled, and is preferred for time-critical uses.
    /// </summary>
    /// <remarks>
    /// Be sure to call Dispose() after you are done using this RNG,
    /// so the worker-thread can be shut down properly. Also note
    /// that this class is not thread-safe with regard to multiple
    /// threads calling e.g. the Uniform() method simultaneously.
    /// To make it thread-safe in this manner, wrap it in a
    /// ThreadSafe-object.
    /// </remarks>
    public abstract class ByteStreamAsync : ByteStream, IDisposable
    {
        #region Constructor.
        /// <summary>
        /// Construct the RNG-object.
        /// </summary>
        /// <param name="size">Buffer-size (in bytes).</param>
        /// <param name="retrieveTrigger">Refill buffer asynchronously when its size falls below this.</param>
        /// <param name="randFallback">Fallback RNG to be used when buffer is empty.</param>
        /// <param name="numFallback">Use fallback RNG for this many bytes before trying to fill buffer again.</param>
        public ByteStreamAsync(int size, int retrieveTrigger, Random randFallback, int numFallback)
            : base(size, randFallback, numFallback)
        {
            RetrieveTrigger = retrieveTrigger;

            _workerThread = new Thread(Work);
            _workerThread.Start();
        }
        #endregion

        #region Availability status.
        /// <summary>
        /// Determine if the requested number of bytes are available, if not
        /// then refill buffer asynchronously. Use Fallback RNG in the meantime.
        /// </summary>
        /// <param name="numBytes">Number of bytes requested.</param>
        /// <returns>Boolean indicating whether the requested number of bytes are available.</returns>
        protected override bool IsAvailable(int numBytes)
        {
            int queueCount = Queue.Count;

            if (FallbackCount > 0)
            {
                // Decrease the fallback-counter, thread-safe.
                lock (FallbackCountLock)
                {
                    FallbackCount = System.Math.Max(0, FallbackCount - numBytes);
                }
            }
            else if (queueCount < numBytes || queueCount < RetrieveTrigger)
            {
                // Resize buffer if more bytes are suddenly requested than it
                // has capacity for.
                if (BufferSize < numBytes)
                {
                    BufferSize = numBytes;
                }

                // Wake-up async. worker to refill the buffer.
                WakeupWorker();
            }

            return (queueCount >= numBytes);
        }
        #endregion

        #region Internal variables.
        /// <summary>
        /// Refill buffer asynchronously when its size falls below this.
        /// </summary>
        int RetrieveTrigger;

        /// <summary>
        /// Lock used for thread-safe updating of FallbackCount.
        /// </summary>
        Object FallbackCountLock = new Object();
        #endregion

        #region Worker Thread.
        /// <summary>
        /// The thread on which the buffer-filling is to be executed.
        /// </summary>
        Thread _workerThread;

        /// <summary>
        /// The signal used for waking up the worker-thread when it
        /// must fill the buffer.
        /// </summary>
        EventWaitHandle _waitHandle = new AutoResetEvent(false);

        /// <summary>
        /// Should worker-thread abort execution?
        /// </summary>
        bool _abortPending = false;

        /// <summary>
        /// Eternal-worker method.
        /// </summary>
        void Work()
        {
            while (!_abortPending)
            {
                // Fill the buffer with random bytes.
                FillBuffer();

                // Wait until buffer needs to be refilled.
                _waitHandle.WaitOne();
            }
        }

        /// <summary>
        /// Wakeup the worker so as to fill buffer.
        /// </summary>
        void WakeupWorker()
        {
            _waitHandle.Set();
        }
        #endregion

        #region IDisposable implementation.
        public void Dispose()
        {
            // Flag as abort pending.
            _abortPending = true;

            // Wakeup worker-thread so it will run until completion.
            WakeupWorker();

            // Join with worker-thread.
            _workerThread.Join();

            // Close the event-signalling.
            _waitHandle.Close();
        }
        #endregion

        #region Fill buffer.
        /// <summary>
        /// Same as for the basic ByteStream-class only the FallbackCount
        /// is updated in a thread-safe lock.
        /// </summary>
        public sealed override void FillBuffer()
        {
            int fillCount = FillCount;

            try
            {
                while (fillCount > 0)
                {
                    DoFillBuffer(System.Math.Min(fillCount, MaxRetrieveLength));

                    fillCount -= MaxRetrieveLength;
                }
            }
            catch
            {
                // Various internet errors may occur. Simply do nothing
                // and let the fallback RNG generate numbers instead
                // when the byte-buffer is discovered to be empty.

                lock (FallbackCountLock)
                {
                    FallbackCount = NumFallback;
                }
            }
        }

        /// <summary>
        /// Thread-safe version of ByteStream.AddBuffer().
        /// </summary>
        /// <param name="buffer">The buffer of bytes to add.</param>
        protected override void AddBuffer(IEnumerable<byte> buffer)
        {
            lock (Queue)
            {
                foreach (byte b in buffer)
                {
                    Queue.Enqueue(b);
                }
            }
        }
        #endregion

        #region Base-class overrides.
        /// <summary>
        /// Draw a random boolean with equal probability of true or false.
        /// </summary>
        public sealed override byte Byte()
        {
            byte b;

            if (IsAvailableByte())
            {
                lock (Queue)
                {
                    b = Queue.Dequeue();
                }
            }
            else
            {
                b = RandFallback.Byte();
            }

            return b;
        }

        /// <summary>
        /// Draw an array of random and uniform bytes.
        /// </summary>
        /// <param name="length">The array length requested.</param>
        public sealed override byte[] Bytes(int length)
        {
            byte[] arr;

            if (IsAvailableBytes(length))
            {
                arr = new byte[length];

                lock (Queue)
                {
                    for (int i = 0; i < length; i++)
                    {
                        arr[i] = Queue.Dequeue();
                    }
                }
            }
            else
            {
                arr = RandFallback.Bytes(length);
            }

            return arr;
        }
        #endregion
    }
}
