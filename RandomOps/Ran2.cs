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
    /// Pseudo-Random Number Generator (PRNG) based on the Ran2 algorithm from the book:
    /// 'Numerical Recipes in C' chapter 7.1 and which is originally
    /// due to L'Ecuyer with Bays-Durham shuffle and added safeguards.
    /// Period is greater than 2 * 10^18. Not thread-safe.
    /// </summary>
    /// <remarks>
    /// We MUST use division when generating random integers in a certain range.
    /// Do NOT use bit-manipulation because the low-order bits of are not that random!
    /// This works by default because Uniform() is used for creating integers through
    /// the implementation of the Index() methods in the Random base-class.
    /// </remarks>
    public class Ran2 : RanInt32
    {
        #region Constructors.
        /// <summary>
        /// Constructs the PRNG-object and seeds the PRNG with the current time of day.
        /// This is what you will mostly want to use.
        /// </summary>
        public Ran2()
            : base()
        {
            Seed();
        }

        /// <summary>
        /// Constructs the PRNG-object using the designated seed.
        /// This is useful if you want to repeat experiments with the
        /// same sequence of pseudo-random numbers.
        /// </summary>
        public Ran2(Int32 seed)
            : base()
        {
            Seed(seed);
        }
        #endregion

        #region Iterator-class (internal use only).
        class Iterator
        {
            Int32 IM, IA, IQ, IR;

            public Int32 Idum
            {
                get;
                private set;
            }

            public Iterator(Int32 im, Int32 ia, Int32 iq, Int32 ir)
            {
                IM = im;
                IA = ia;
                IQ = iq;
                IR = ir;
            }

            public void Seed(Int32 seed)
            {
                Idum = seed;
            }

            public Int32 DoRand()
            {
                Int32 k;

                k = Idum / IQ;

                Idum = IA * (Idum - k * IQ) - IR * k;

                if (Idum < 0)
                {
                    Idum += IM;
                }

                return Idum;
            }
        }
        #endregion

        #region Internal definitions and variables.
        static readonly Int32 IM0 = 2147483563;
        static readonly Int32 IM1 = 2147483399;
        static readonly Int32 IA0 = 40014;
        static readonly Int32 IA1 = 40692;
        static readonly Int32 IQ0 = 53668;
        static readonly Int32 IQ1 = 52774;
        static readonly Int32 IR0 = 12211;
        static readonly Int32 IR1 = 3791;
        static readonly Int32 NTAB = 32;
        static readonly Int32 IMM = IM0 - 1;
        static readonly Int32 NDIV = 1 + IMM / NTAB;
        static readonly Int32 WARMUP = 1024 + 8;
        static readonly Int32 WARMUP2 = 200;

        Iterator[] Iterators = { new Iterator(IM0, IA0, IQ0, IR0), new Iterator(IM1, IA1, IQ1, IR1) };

        Int32 iy = 0;
        Int32[] iv = new Int32[NTAB];

        /// <summary>
        /// Is PRNG ready for use?
        /// </summary>
        bool IsReady = false;
        #endregion

        #region PRNG Implementation.
        /// <summary>
        /// Draw a random number in inclusive range {0, .., RandMax}
        /// </summary>
        public sealed override Int32 Rand()
        {
            Debug.Assert(IsReady);

            Iterators[0].DoRand();
            Iterators[1].DoRand();

            {
                // Will be in the range 0..NTAB-1.
                int j = iy / NDIV;

                Debug.Assert(j >= 0 && j < NTAB);

                // Idum is shuffled, idum0 and idum1 are
                // combined to generate output.
                iy = iv[j] - Iterators[1].Idum;
                iv[j] = Iterators[0].Idum;

                if (iy < 1)
                {
                    iy += IMM;
                }
            }

            Debug.Assert(iy >= 0 && iy <= RandMax);

            return iy;
        }

        /// <summary>
        /// The maximum possible value returned by Rand().
        /// </summary>
        public sealed override Int32 RandMax
        {
            get { return IM0 - 1; }
        }

        /// <summary>
        /// Seed with an integer.
        /// </summary>
        protected sealed override void Seed(Int32 seed)
        {
            int j;

            // Ensure seed>0
            if (seed == 0)
            {
                seed = 1;
            }
            else if (seed < 0)
            {
                seed = -seed;
            }

            Iterators[0].Seed(seed);
            Iterators[1].Seed(seed);

            // Perform initial warm-ups.
            for (j = 0; j < WARMUP; j++)
            {
                Iterators[0].DoRand();
            }

            for (j = NTAB - 1; j >= 0; j--)
            {
                iv[j] = Iterators[0].DoRand();
            }

            iy = iv[0];

            // PRNG is now ready for use.
            IsReady = true;

            // Perform additional warm-ups.
            for (j = 0; j < WARMUP2; j++)
            {
                Rand();
            }
        }
        #endregion

        #region Base-class overrides.
        /// <summary>
        /// Name of the RNG.
        /// </summary>
        public override string Name
        {
            get { return "Ran2"; }
        }
        #endregion
    }
}
