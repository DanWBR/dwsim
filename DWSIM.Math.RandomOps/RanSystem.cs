/// ------------------------------------------------------
/// RandomOps - (Pseudo) Random Number Generator For C#
/// Copyright (C) 2003-2010 Magnus Erik Hvass Pedersen.
/// Please see the file license.txt for license details.
/// RandomOps on the internet: http://www.Hvass-Labs.org/
/// ------------------------------------------------------

namespace RandomOps
{
    /// <summary>
    /// Wrapper for the .NET built-in PRNG. Not thread-safe by
    /// default.
    /// </summary>
    /// <remarks>
    /// Since the .NET implementation may change we cannot
    /// override and make a more efficient implementation of
    /// Byte() that would use bit-manipulation, because it
    /// would assume things about the implementation that might
    /// change.
    /// </remarks>
    public class RanSystem : Random
    {
        #region Constructors.
        /// <summary>
        /// Constructs the PRNG-object and seeds the PRNG with the current time of day.
        /// This is what you will mostly want to use.
        /// </summary>
        public RanSystem()
            : base()
        {
            Rand = new System.Random();
        }

        /// <summary>
        /// Constructs the PRNG-object using the designated seed.
        /// This is useful if you want to repeat experiments with the
        /// same sequence of pseudo-random numbers.
        /// </summary>
        public RanSystem(int seed)
            : base()
        {
            Rand = new System.Random(seed);
        }
        #endregion

        #region Internal variables.
        /// <summary>
        /// The .NET PRNG-object.
        /// </summary>
        System.Random Rand;
        #endregion

        #region Base-class overrides.
        /// <summary>
        /// Name of the RNG.
        /// </summary>
        public override string Name
        {
            get { return "RanSystem"; }
        }

        /// <summary>
        /// Draw a uniform random number in the exclusive range (0,1)
        /// </summary>
        /// <returns></returns>
        public sealed override double Uniform()
        {
            return Rand.NextDouble();
        }

        /// <summary>
        /// Draw a random boolean with equal probability of drawing true or false.
        /// </summary>
        /// <returns></returns>
        public sealed override bool Bool()
        {
            return Rand.Next(2) == 0;
        }

        /// <summary>
        /// Draw a random index from the range {0, .., n-1}
        /// </summary>
        /// <param name="n">The exclusive upper bound.</param>
        /// <returns></returns>
        public sealed override int Index(int n)
        {
            return Rand.Next(n);
        }

        /// <summary>
        /// Draw a random and uniform byte.
        /// </summary>
        public sealed override byte Byte()
        {
            // Re-use the Index() method.
            return Bytes(1)[0];
        }

        /// <summary>
        /// Draw an array of random and uniform bytes.
        /// </summary>
        /// <param name="length">The array length requested.</param>
        public sealed override byte[] Bytes(int length)
        {
            byte[] arr = new byte[length];

            Rand.NextBytes(arr);

            return arr;
        }
        #endregion
    }
}
