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
    /// Pseudo-Random Number Generator (PRNG) based on KISS as
    /// described in the paper: G. Marsaglia, Random Number Generators,
    /// Journal of Modern Applied Statistical Methods, 2003, vol. 2, no. 1,
    /// p. 2-13. Period of this PRNG is greater than 2^124. Not thread-safe.
    /// </summary>
    /// <remarks>
    /// This is a translation of the C source-code published in the
    /// paper cited above with Marsaglia's authorization, also under
    /// the license in license.txt
    /// </remarks>
    public class KISS : RanUInt32Array
    {
        #region Constructors.
        /// <summary>
        /// Constructs the PRNG-object without a seed. Remember
        /// to seed it before drawing random numbers.
        /// </summary>
        public KISS()
            : base()
        {
        }

        /// <summary>
        /// Constructs the PRNG-object using the designated seed.
        /// This is useful if you want to repeat experiments with the
        /// same sequence of pseudo-random numbers.
        /// </summary>
        public KISS(UInt32[] seed)
            : base(seed)
        {
        }

        /// <summary>
        /// Constructs the PRNG-object and uses another RNG for seeding.
        /// </summary>
        public KISS(Random rand)
            : base(rand)
        {
        }
        #endregion

        #region Public properties.
        /// <summary>
        /// Default seed.
        /// </summary>
        public static readonly UInt32[] SeedDefault = { 123456789, 362436000, 521288629, 7654321 };
        #endregion

        #region Internal definitions and variables
        /// <summary>
        /// Iterator variables.
        /// </summary>
        UInt32 X, Y, Z, C;

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

            X = 69069 * X + 12345;

            Y ^= Y << 13;
            Y ^= Y >> 17;
            Y ^= Y << 5;

            UInt64 t = 698769069 * Z + C;

            C = (UInt32)(t >> 32);

            Z = (UInt32)t;

            UInt32 retVal = X + Y + Z;

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
            get { return 4; }
        }

        /// <summary>
        /// Seed with an array.
        /// </summary>
        public sealed override void Seed(UInt32[] seed)
        {
            Debug.Assert(seed.Length == SeedLength);

            X = seed[0];
            Y = seed[1];
            Z = seed[2];
            C = seed[3];

            IsReady = true;
        }
        #endregion

        #region Base-class overrides.
        /// <summary>
        /// Name of the RNG.
        /// </summary>
        public override string Name
        {
            get { return "KISS"; }
        }
        #endregion
    }
}
