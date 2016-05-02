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
    /// Pseudo-Random Number Generator (PRNG) based on MWC256 by
    /// George Marsaglia. Period of this PRNG is about 2^8222.
    /// Not thread-safe.
    /// </summary>
    /// <remarks>
    /// This is a translation of the C source-code published 2003-05-13
    /// in the newsgroup comp.lang.c by George Marsaglia, published
    /// here with Marsaglia's authorization under the license in license.txt
    /// </remarks>
    public class MWC256 : RanUInt32Array
    {
        #region Constructors.
        /// <summary>
        /// Constructs the PRNG-object without a seed. Remember
        /// to seed it before drawing random numbers.
        /// </summary>
        public MWC256()
            : base()
        {
        }

        /// <summary>
        /// Constructs the PRNG-object using the designated seed.
        /// This is useful if you want to repeat experiments with the
        /// same sequence of pseudo-random numbers.
        /// </summary>
        public MWC256(UInt32[] seed)
            : base(seed)
        {
        }

        /// <summary>
        /// Constructs the PRNG-object and uses another RNG for seeding.
        /// </summary>
        public MWC256(Random rand)
            : base(rand)
        {
        }
        #endregion

        #region Internal definitions and variables
        /// <summary>
        /// Iterator array.
        /// </summary>
        UInt32[] Q = new UInt32[256];

        /// <summary>
        /// Carry variable.
        /// </summary>
        UInt32 C;

        /// <summary>
        /// Iteration counter.
        /// </summary>
        Byte Counter = 255;

        /// <summary>
        /// Is PRNG ready for use?
        /// </summary>
        bool IsReady = false;
        #endregion

        #region PRNG Implementation.
        /// <summary>
        /// Draw a random number in inclusive range {0, .., RandMax}
        /// </summary>
        public sealed override UInt32 Rand()
        {
            Debug.Assert(IsReady);

            UInt64 t = (UInt64)809430660 * Q[++Counter] + C;

            C = (UInt32)(t >> 32);

            UInt32 retVal = (UInt32)t;
            Q[Counter] = retVal;

            return retVal;
        }

        /// <summary>
        /// The maximum possible value returned by Rand().
        /// </summary>
        public sealed override UInt32 RandMax
        {
            get { return UInt32.MaxValue; }
        }

        /// <summary>
        /// Length of seed-array.
        /// </summary>
        public sealed override int SeedLength
        {
            get { return 257; }
        }

        /// <summary>
        /// Seed with an array.
        /// </summary>
        public sealed override void Seed(UInt32[] seed)
        {
            Debug.Assert(seed.Length == SeedLength);

            // First seed is used for C.
            C = seed[0] % 809430660;

            // Remaining seeds are used for Q.
            for (int i = 1; i < SeedLength; i++)
            {
                Q[i-1] = seed[i];
            }

            IsReady = true;
        }
        #endregion

        #region Base-class overrides.
        /// <summary>
        /// Name of the RNG.
        /// </summary>
        public override string Name
        {
            get { return "MWC256"; }
        }
        #endregion
    }
}
