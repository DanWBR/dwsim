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
    /// Pseudo-Random Number Generator (PRNG) based on the RanQD1 (Quick and Dirty)
    /// algorithm from the book: 'Numerical Recipes in C' chapter 7.1. Not thread-safe.
    /// </summary>
    public class RanQD : RanUInt32
    {
        #region Constructors.
        /// <summary>
        /// Constructs the PRNG-object and seeds the PRNG with the current time of day.
        /// This is what you will mostly want to use.
        /// </summary>
        public RanQD()
            : base()
        {
            Seed();
        }

        /// <summary>
        /// Constructs the PRNG-object using the designated seed.
        /// This is useful if you want to repeat experiments with the
        /// same sequence of pseudo-random numbers.
        /// </summary>
        public RanQD(UInt32 seed)
            : base()
        {
            Seed(seed);
        }
        #endregion

        #region Internal definitions and variables
        static readonly UInt32 L1 = 1664525;
        static readonly UInt32 L2 = 1013904223;

        /// <summary>
        /// Is PRNG ready for use?
        /// </summary>
        bool IsReady = false;

        /// <summary>
        /// Iterator-variable.
        /// </summary>
        UInt32 Iter = 0;
        #endregion

        #region PRNG Implementation.
        /// <summary>
        /// Draw a random number in inclusive range {0, .., RandMax}
        /// </summary>
        public sealed override UInt32 Rand()
        {
            Debug.Assert(IsReady);

            Iter = L1 * Iter + L2;

            Debug.Assert(Iter >= 0 && Iter <= RandMax);

            return Iter;
        }

        /// <summary>
        /// The maximum possible value returned by Rand().
        /// </summary>
        public sealed override UInt32 RandMax
        {
            get { return UInt32.MaxValue; }
        }

        /// <summary>
        /// Seed with an integer.
        /// </summary>
        protected sealed override void Seed(UInt32 seed)
        {
            Iter = seed;
            IsReady = true;
        }
        #endregion

        #region Base-class overrides.
        /// <summary>
        /// Name of the RNG.
        /// </summary>
        public override string Name
        {
            get { return "RanQD"; }
        }
        #endregion
    }
}
