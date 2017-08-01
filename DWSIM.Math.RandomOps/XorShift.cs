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
    /// Pseudo-Random Number Generator (PRNG) based on XorShift as
    /// described in the paper: G. Marsaglia, Random Number Generators,
    /// Journal of Modern Applied Statistical Methods, 2003, vol. 2, no. 1,
    /// p. 2-13. Period of this PRNG is about 2^160. Not thread-safe.
    /// </summary>
    /// <remarks>
    /// This is a translation of the C source-code published 2003-05-13
    /// in the newsgroup comp.lang.c by George Marsaglia, published
    /// here with Marsaglia's authorization under the license in license.txt
    /// </remarks>
    public class XorShift : RanUInt32Array
    {
        #region Constructors.
        /// <summary>
        /// Constructs the PRNG-object without a seed. Remember
        /// to seed it before drawing random numbers.
        /// </summary>
        public XorShift()
            : base()
        {
        }

        /// <summary>
        /// Constructs the PRNG-object using the designated seed.
        /// This is useful if you want to repeat experiments with the
        /// same sequence of pseudo-random numbers.
        /// </summary>
        public XorShift(UInt32[] seed)
            : base(seed)
        {
        }

        /// <summary>
        /// Constructs the PRNG-object and uses another RNG for seeding.
        /// </summary>
        public XorShift(Random rand)
            : base(rand)
        {
        }
        #endregion

        #region Public properties.
        /// <summary>
        /// Default seed.
        /// </summary>
        public static readonly UInt32[] SeedDefault = { 123456789, 362436069, 521288629, 88675123, 886756453 };
        #endregion

        #region Internal definitions and variables
        /// <summary>
        /// Iterator variables.
        /// </summary>
        UInt32 X, Y, Z, W, V;

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

            UInt32 t = (X ^ (X >> 7));

            X=Y;
            Y=Z;
            Z=W;
            W=V;
            
            V = (V ^ (V << 6)) ^ (t ^ (t << 13));
            
            UInt32 retVal = (Y+Y+1)*V;

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
            get { return 5; }
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
            W = seed[3];
            V = seed[4];

            IsReady = true;
        }
        #endregion

        #region Base-class overrides.
        /// <summary>
        /// Name of the RNG.
        /// </summary>
        public override string Name
        {
            get { return "XorShift"; }
        }
        #endregion
    }
}
