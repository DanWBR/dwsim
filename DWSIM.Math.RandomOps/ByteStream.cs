/// ------------------------------------------------------
/// RandomOps - (Pseudo) Random Number Generator For C#
/// Copyright (C) 2003-2010 Magnus Erik Hvass Pedersen.
/// Please see the file license.txt for license details.
/// RandomOps on the internet: http://www.Hvass-Labs.org/
/// ------------------------------------------------------

using System;
using System.Collections.Generic;

namespace RandomOps
{
    /// <summary>
    /// Base-class for an RNG which retrieves a stream of random
    /// bytes from somewhere, e.g. from a file, from the internet,
    /// or from a physical device such as a USB-device.
    /// </summary>
    public abstract class ByteStream : Random
    {
        #region Constructor.
        /// <summary>
        /// Construct the RNG-object.
        /// </summary>
        /// <param name="bufferSize">Number of random bytes the buffer holds.</param>
        /// <param name="randFallback">Fallback RNG to be used when buffer is empty.</param>
        /// <param name="numFallback">Use fallback RNG for this many bytes before trying to fill buffer again.</param>
        public ByteStream(int bufferSize, Random randFallback, int numFallback)
        {
            RandFallback = randFallback;
            NumFallback = numFallback;
            BufferSize = Math.Max(bufferSize, sizeof(UInt32));
            Queue = new Queue<byte>(BufferSize);

            // Fill the buffer so it is ready to use.
            FillBuffer();
        }
        #endregion

        #region Properties.
        /// <summary>
        /// The number of bytes to fill the buffer with.
        /// </summary>
        protected int FillCount
        {
            get { return BufferSize - Queue.Count; }
        }

        /// <summary>
        /// The maximum number of bytes that can be retrieved in a single
        /// call to DoFillBuffer(). Usually this is the same as BufferSize,
        /// but some sources allow a maximum number of bytes that can be
        /// retrieved each time or they will generate an error.
        /// </summary>
        protected virtual int MaxRetrieveLength
        {
            get { return BufferSize; }
        }
        #endregion

        #region Internal variables.
        /// <summary>
        /// The queue used for buffering the retrieved bytes.
        /// </summary>
        protected Queue<byte> Queue;

        /// <summary>
        /// Desired size of the byte-buffer.
        /// </summary>
        protected int BufferSize;

        /// <summary>
        /// 1.0/((double)UInt32.MaxValue + 2), for convenience and speed.
        /// </summary>
        double _randMaxPlusTwoInv = 1.0 / ((double)UInt32.MaxValue + 2);
        #endregion

        #region Fallback RNG.
        /// <summary>
        /// Fallback RNG.
        /// </summary>
        protected Random RandFallback;

        /// <summary>
        /// Number of bytes to retrieve from Fallback RNG
        /// whenever buffer becomes empty.
        /// </summary>
        protected int NumFallback
        {
            get;
            private set;
        }

        /// <summary>
        /// How many bytes yet to be retrieved from Fallback RNG
        /// before buffer will be attempted filled again.
        /// </summary>
        protected int FallbackCount = 0;
        #endregion

        #region Fill buffer.
        /// <summary>
        /// Override this so that it retrieves the requested number of bytes
        /// and calls AddBuffer() to add them to the buffer.
        /// </summary>
        /// <param name="length">Number of bytes to be retrieved.</param>
        protected abstract void DoFillBuffer(int length);

        /// <summary>
        /// Fill the buffer by calling DoFillBuffer(). The entire buffer
        /// may not be fillable in one call of DoFillBuffer(), e.g. due
        /// to restrictions on the source of the bytes, and hence several
        /// calls to DoFillBuffer() must be made.
        /// </summary>
        /// <exception>
        /// All exceptions cause the Fallback RNG to be used.
        /// </exception>
        public virtual void FillBuffer()
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

                FallbackCount = NumFallback;
            }
        }

        /// <summary>
        /// Fill the buffer with at least the number of bytes designated.
        /// </summary>
        /// <param name="size">Number of bytes requested to be in buffer.</param>
        public void FillBuffer(int size)
        {
            if (BufferSize < size)
            {
                BufferSize = size;
            }

            FillBuffer();
        }

        /// <summary>
        /// Add retrieved random bytes to the buffer.
        /// </summary>
        /// <param name="buffer">Random bytes to be added.</param>
        protected virtual void AddBuffer(IEnumerable<byte> buffer)
        {
            foreach (byte b in buffer)
            {
                Queue.Enqueue(b);
            }
        }
        #endregion

        #region Availability status.
        /// <summary>
        /// Is a single random byte currently available in the buffer?
        /// </summary>
        public bool IsAvailableByte()
        {
            return IsAvailable(sizeof(byte));
        }

        /// <summary>
        /// Is the given number of random bytes currently available in the buffer?
        /// </summary>
        public bool IsAvailableBytes(int length)
        {
            return IsAvailable(length);
        }

        /// <summary>
        /// Is a random number available in the buffer using the Uniform() method?
        /// </summary>
        public bool IsAvailableUniform()
        {
            return IsAvailable(sizeof(UInt32));
        }

        /// <summary>
        /// Determine if the requested number of bytes are available, if not
        /// then refill buffer.
        /// </summary>
        /// <param name="numBytes">Number of bytes requested.</param>
        /// <returns>Boolean indicating whether the requested number of bytes are available.</returns>
        protected virtual bool IsAvailable(int numBytes)
        {
            if (FallbackCount > 0)
            {
                // Decrease the fallback-counter.
                FallbackCount = System.Math.Max(0, FallbackCount - numBytes);
            }
            else if (Queue.Count < numBytes)
            {
                // Resize buffer if more bytes are suddenly requested than it
                // has capacity for.
                if (BufferSize < numBytes)
                {
                    BufferSize = numBytes;
                }

                // Fill the buffer and wait for the results (i.e. synchronous execution).
                FillBuffer();
            }

            return (Queue.Count >= numBytes);
        }
        #endregion

        #region Base-class overrides.
        /// <summary>
        /// Draw a random boolean with equal probability of true or false.
        /// Use the buffered random bytes if available, otherwise use Fallback RNG.
        /// </summary>
        public override byte Byte()
        {
            byte b = (IsAvailableByte()) ? (Queue.Dequeue()) : (RandFallback.Byte());

            return b;
        }

        /// <summary>
        /// Draw an array of random and uniform bytes.
        /// Use the buffered random bytes if available, otherwise use Fallback RNG.
        /// </summary>
        /// <param name="length">The array length requested.</param>
        public override byte[] Bytes(int length)
        {
            byte[] arr;

            if (IsAvailableBytes(length))
            {
                arr = new byte[length];

                for (int i = 0; i < length; i++)
                {
                    arr[i] = Queue.Dequeue();
                }
            }
            else
            {
                arr = RandFallback.Bytes(length);
            }

            return arr;
        }

        /// <summary>
        /// Draw a uniform random number in the exclusive range (0,1)
        /// This uses the Bytes() method to draw 4 random bytes, make them
        /// into an integer, and make that into a double. As such, it will
        /// indirectly use the Fallback RNG if no random bytes are available
        /// in the buffer.
        /// </summary>
        public sealed override double Uniform()
        {
            double value;

            if (IsAvailableUniform())
            {
                byte[] b = Bytes(sizeof(UInt32));

                UInt32 rand = BitConverter.ToUInt32(b, 0);
                double randPlusOne = (double)rand + 1;

                value = randPlusOne * _randMaxPlusTwoInv;
            }
            else
            {
                value = RandFallback.Uniform();
            }

            return value;
        }

        /// <summary>
        /// Draw a random boolean with equal probability of drawing true or false.
        /// This indirectly uses the Byte() method and will as such use the Fallback
        /// RNG if no buffered random bytes are available.
        /// </summary>
        public sealed override bool Bool()
        {
            byte b = Byte();
            return b <= (byte.MaxValue / 2);
        }
        #endregion
    }
}
