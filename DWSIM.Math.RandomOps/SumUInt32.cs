/// ------------------------------------------------------
/// RandomOps - (Pseudo) Random Number Generator For C#
/// Copyright (C) 2003-2010 Magnus Erik Hvass Pedersen.
/// Please see the file license.txt for license details.
/// RandomOps on the internet: http://www.Hvass-Labs.org/
/// ------------------------------------------------------

using System;

namespace RandomOps
{
    /// <summary>
    /// Summing the output of multiple RNGs and taking modulo 2^32.
    /// Note that this assumes the RNGs have RandMax roughly equal
    /// to UInt32.MaxValue otherwise there will be a bias. Thread-safe
    /// if RNGs are thread-safe.
    /// </summary>
    /// <remarks>
    /// If you are using RNGs that have custom methods for generating
    /// random numbers then you need to extend this class in a fashion
    /// similar to that of the Uniform()-method.
    /// </remarks>
    public partial class SumUInt32 : RanUInt32
    {
        #region Constructor.
        /// <summary>
        /// Constructs the RNG-object from different RNG's.
        /// </summary>
        public SumUInt32(RanUInt32[] rands)
            : base()
        {
            Rands = rands;
        }
        #endregion

        #region Internal variables.
        /// <summary>
        /// The array of RNGs to sum.
        /// </summary>
        protected Random[] Rands
        {
            get;
            private set;
        }
        #endregion

        #region Base-class overrides.
        /// <summary>
        /// Name of the RNG.
        /// </summary>
        public override sealed string Name
        {
            get
            {
                string s = "Sum(";

                foreach (Random rand in Rands)
                {
                    s += rand.Name + ", ";
                }

                s += ")";

                return s;
            }
        }

        /// <summary>
        /// Draw a random number in inclusive range {0, .., RandMax}
        /// </summary>
        public override sealed UInt32 Rand()
        {
            UInt32 sum = 0;

            // Sum and modulo.
            foreach (RanUInt32 rand in Rands)
            {
                sum += rand.Rand();
            }

            return sum;
        }

        /// <summary>
        /// The maximum possible value returned by Rand().
        /// </summary>
        public override sealed UInt32 RandMax
        {
            get
            {
                return UInt32.MaxValue;
            }
        }
        #endregion
    }
}
