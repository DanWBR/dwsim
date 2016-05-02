/// ------------------------------------------------------
/// RandomOps - (Pseudo) Random Number Generator For C#
/// Copyright (C) 2003-2010 Magnus Erik Hvass Pedersen.
/// Please see the file license.txt for license details.
/// RandomOps on the internet: http://www.Hvass-Labs.org/
/// ------------------------------------------------------

using System;
using System.Diagnostics;

namespace RandomOps
{
    /// <summary>
    /// Pseudo-Random Number Generator (PRNG) base-class for a generator of UInt32 integers.
    /// </summary>
    /// <remarks>
    /// It is somewhat tricky to implement Index() using integer-operations and get the
    /// rounding right for all cases, so we reuse the base-class implementation which
    /// indirectly uses Uniform().
    /// </remarks>
    public abstract class RanInt32 : Random
    {
        #region Constructors.
        /// <summary>
        /// Constructs the PRNG-object.
        /// </summary>
        public RanInt32()
            : base()
        {
            _randMaxHalf = RandMax / 2;
            _randInv = 1.0/((double)RandMax + 2);
        }
        #endregion

        #region Internal variables.
        /// <summary>
        /// Used in Bool(), for convenience and speed.
        /// </summary>
        Int32 _randMaxHalf;

        /// <summary>
        /// Used in Uniform(), for convenience and speed.
        /// </summary>
        double _randInv;
        #endregion

        #region PRNG Implementation.
        /// <summary>
        /// Draw a random number in inclusive range {0, .., RandMax}
        /// </summary>
        public abstract Int32 Rand();

        /// <summary>
        /// The maximum possible value returned by Rand().
        /// </summary>
        public abstract Int32 RandMax
        {
            get;
        }

        /// <summary>
        /// Seed with the time of day.
        /// </summary>
        protected void Seed()
        {
            Int32 seed = (Int32)(DateTime.Now.Ticks % (long)RandMax);

            Seed(seed);
        }

        /// <summary>
        /// Seed with an integer.
        /// </summary>
        protected abstract void Seed(Int32 seed);
        #endregion

        #region Base-class overrides.
        /// <summary>
        /// Draw a uniform random number in the exclusive range (0,1)
        /// Thread-safe if Rand() is thread-safe.
        /// </summary>
        /// <remarks>
        /// Assumes that Rand() is in {0, .., RandMax}.
        /// </remarks>
        public override double Uniform()
        {
            double rand = (double)Rand() + 1;
            double value = rand * _randInv;

            Debug.Assert(value > 0 && value < 1);

            return value;
        }

        /// <summary>
        /// Draw a random boolean with equal probability of true or false.
        /// Thread-safe if Rand() is thread-safe.
        /// </summary>
        public override bool Bool()
        {
            return Rand() < _randMaxHalf;
        }

        /// <summary>
        /// Draw a random and uniform byte.
        /// Thread-safe if Rand() is thread-safe.
        /// </summary>
        /// <remarks>
        /// The least significant bits are not that statistically random,
        /// hence we must use the most significant bits by a bit-shift.
        /// </remarks>
        public override byte Byte()
        {
            Int32 r = Rand();
            Int32 value = r >> 23;

            Debug.Assert(value >= 0 && value <= System.Byte.MaxValue);

            byte b = (byte)value;

            return b;
        }
        #endregion
    }
}
