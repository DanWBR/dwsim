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
    /// Pseudo-Random Number Generator (PRNG) based on CMWC4096 as
    /// described in the paper: G. Marsaglia, Random Number Generators,
    /// Journal of Modern Applied Statistical Methods, 2003, vol. 2, no. 1,
    /// p. 2-13. Period of this PRNG is about 2^131104. Not thread-safe.
    /// </summary>
    /// <remarks>
    /// This is a translation of the C source-code published 2003-05-13
    /// in the newsgroup comp.lang.c by George Marsaglia, published
    /// here with Marsaglia's authorization under the license in license.txt
    /// </remarks>
    public class CMWC4096 : RanUInt32Array
    {
        #region Constructors.
        /// <summary>
        /// Constructs the PRNG-object without a seed. Remember
        /// to seed it before drawing random numbers.
        /// </summary>
        public CMWC4096()
            : base()
        {
        }

        /// <summary>
        /// Constructs the PRNG-object using the designated seed.
        /// This is useful if you want to repeat experiments with the
        /// same sequence of pseudo-random numbers.
        /// </summary>
        public CMWC4096(UInt32[] seed)
            : base(seed)
        {
        }

        /// <summary>
        /// Constructs the PRNG-object and uses another RNG for seeding.
        /// </summary>
        public CMWC4096(Random rand)
            : base(rand)
        {
        }
        #endregion

        #region Internal definitions and variables
        /// <summary>
        /// Iterator array.
        /// </summary>
        UInt32[] Q = new UInt32[4096];

        /// <summary>
        /// Carry variable.
        /// </summary>
        UInt32 C;

        /// <summary>
        /// Iteration counter.
        /// </summary>
        uint Counter = 4095;

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

            Counter = (Counter + 1) & 4095;

            UInt64 t = (UInt64)18782 * Q[Counter] + C;
            
            C = (UInt32)(t >> 32);

            UInt32 x = (UInt32)(t + C);
            
            if (x < C)
            {
                x++;
                C++;
            }

            UInt32 retVal = 0xfffffffe - x;
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
            get { return 4097; }
        }

        /// <summary>
        /// Seed with an integer.
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
            get { return "CMWC4096"; }
        }
        #endregion
    }
}
