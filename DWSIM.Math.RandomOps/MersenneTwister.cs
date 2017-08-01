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
    /// Pseudo-Random Number Generator (PRNG) using the Mersenne Twister
    /// algorithm by Makoto Matsumoto and Takuji Nishimura. This implementation
    /// is rewritten from their C source-code originally dated 2002/1/26.
    /// This PRNG has a very long period of 2^19937-1 (approximately 4.3 x 10^6001),
    /// and is hence known as MT19937. This implementation is the 32-bit version.
    /// Not thread-safe.
    /// </summary>
    /// <remarks>
    /// The original C source-code contains the following copyright notice which
    /// still holds for this more or less direct translation to the C# language:
    /// 
    /// Copyright (C) 1997 - 2002, Makoto Matsumoto and Takuji Nishimura,
    /// All rights reserved.
    ///
    /// Redistribution and use in source and binary forms, with or without
    /// modification, are permitted provided that the following conditions
    /// are met:
    /// 
    /// 1. Redistributions of source code must retain the above copyright
    ///    notice, this list of conditions and the following disclaimer.
    ///
    /// 2. Redistributions in binary form must reproduce the above copyright
    ///    notice, this list of conditions and the following disclaimer in the
    ///    documentation and/or other materials provided with the distribution.
    ///
    /// 3. The names of its contributors may not be used to endorse or promote 
    ///    products derived from this software without specific prior written 
    ///    permission.
    ///
    /// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    /// "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    /// LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
    /// A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE COPYRIGHT OWNER OR
    /// CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
    /// EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
    /// PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
    /// PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
    /// LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
    /// NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
    /// SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
    /// 
    /// Any feedback is very welcome.
    /// http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/emt.html
    /// email: m-mat @ math.sci.hiroshima-u.ac.jp (remove space)
    /// </remarks>
    public class MersenneTwister : RanUInt32
    {
        #region Constructors.
        /// <summary>
        /// Constructs the PRNG-object and seeds the PRNG with the current time of day.
        /// This is what you will mostly want to use.
        /// </summary>
        public MersenneTwister()
            : base()
        {
            Seed();
        }

        /// <summary>
        /// Constructs the PRNG-object using the designated seed.
        /// This is useful if you want to repeat experiments with the
        /// same sequence of pseudo-random numbers.
        /// </summary>
        public MersenneTwister(UInt32 seed)
            : base()
        {
            Seed(seed);
        }

        /// <summary>
        /// Constructs the PRNG-object using the designated array
        /// of seeds. Use this if you need to seed with more than 32 bits.
        /// </summary>
        public MersenneTwister(UInt32[] seeds)
            : base()
        {
            Seed(seeds);
        }
        #endregion

        #region Internal definitions and variables
        static readonly UInt32 N = 624;                     // Array-length.
        static readonly UInt32 M = 397;
        static readonly UInt32 MATRIX_A = 0x9908b0df;       // Constant vector a.
        static readonly UInt32 UPPER_MASK = 0x80000000;     // Most significant w-r bits.
        static readonly UInt32 LOWER_MASK = 0x7fffffff;     // Least significant r bits.
        static readonly UInt32[] mag01 = { 0x0, MATRIX_A };

        UInt32[] mt = new UInt32[N];                        // The array for the state vector.
        UInt32 mti;                                         // Index into mt-array.

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

            UInt32 y;

            if (mti >= N)
            {
                // Generate N words.

                int kk;

                for (kk = 0; kk < N - M; kk++)
                {
                    y = (mt[kk] & UPPER_MASK) | (mt[kk + 1] & LOWER_MASK);
                    mt[kk] = mt[kk + M] ^ (y >> 1) ^ mag01[y & 0x1];
                }

                for (; kk < N - 1; kk++)
                {
                    y = (mt[kk] & UPPER_MASK) | (mt[kk + 1] & LOWER_MASK);
                    mt[kk] = mt[kk + M - N] ^ (y >> 1) ^ mag01[y & 0x1];
                }

                y = (mt[N - 1] & UPPER_MASK) | (mt[0] & LOWER_MASK);
                mt[N - 1] = mt[M - 1] ^ (y >> 1) ^ mag01[y & 0x1];

                mti = 0;
            }

            y = mt[mti++];

            /* Tempering */
            y ^= (y >> 11);
            y ^= (y << 7) & 0x9d2c5680;
            y ^= (y << 15) & 0xefc60000;
            y ^= (y >> 18);

            Debug.Assert(y >= 0 && y <= RandMax);

            return y;
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
            mt[0] = seed;

            for (mti = 1; mti < N; mti++)
            {
                UInt32 lcg = 1812433253;
                mt[mti] = (lcg * (mt[mti - 1] ^ (mt[mti - 1] >> 30)) + mti);
            }

            IsReady = true;
        }

        /// <summary>
        /// Seed with an array of integers.
        /// </summary>
        protected void Seed(UInt32[] seeds)
        {
            Seed(19650218);

            UInt32 i = 1;
            UInt32 j = 0;
            UInt32 k = (N > seeds.Length) ? (N) : ((UInt32)seeds.Length);

            for (; k > 0; k--)
            {
                // Non-linear.
                mt[i] = (mt[i] ^ ((mt[i - 1] ^ (mt[i - 1] >> 30)) * 1664525)) + seeds[j] + j;

                i++;
                j++;

                if (i >= N)
                {
                    mt[0] = mt[N - 1];
                    i = 1;
                }

                if (j >= seeds.Length)
                {
                    j = 0;
                }
            }

            for (k = N - 1; k > 0; k--)
            {
                // Non-linear.
                mt[i] = (mt[i] ^ ((mt[i - 1] ^ (mt[i - 1] >> 30)) * 1566083941)) - i;

                i++;

                if (i >= N)
                {
                    mt[0] = mt[N - 1];
                    i = 1;
                }
            }

            // MSB is 1; assuring non-zero initial array.
            mt[0] = 0x80000000;
        }
        #endregion

        #region Base-class overrides.
        /// <summary>
        /// Name of the RNG.
        /// </summary>
        public override string Name
        {
            get { return "MersenneTwister19937"; }
        }
        #endregion
    }
}
