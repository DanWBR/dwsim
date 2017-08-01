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
    /// <remarks>
    /// Various methods for creating random integers.
    /// </remarks>
    public abstract partial class Random
    {
        /// <summary>
        /// Draw a random integer from the set {0, .., n-1} with uniform probability.
        /// Thread-safe if Uniform() is thread-safe.
        /// </summary>
        /// <remarks>
        /// The default implementation uses Uniform() to generate an integer.
        /// This is because many PRNGs must use division instead of modulo
        /// arithmetics due to the lower-order bits having little randomness.
        /// Assume Uniform cannot generate an exact value of one,
        /// otherwise this must have been taken into account to ensure
        /// uniform probability of choosing the different indices. 
        /// </remarks>
        public virtual int Index(int n)
        {
            Debug.Assert(n >= 1);

            double indexD = Uniform(0, n);

            Debug.Assert(indexD > 0 && indexD < n);

            int index = (int)Math.Truncate(indexD);

            Debug.Assert(index >= 0 && index <= n - 1);

            return index;
        }

        /// <summary>
        /// Draw two distinct integers from the set {0, .., n-1} with equal probability.
        /// Thread-safe if Uniform() is thread-safe.
        /// </summary>
        /// <param name="index1">Reference to the first integer drawn.</param>
        /// <param name="index2">Reference to the second integer drawn.</param>
        public virtual void Index2(int n, out int index1, out int index2)
        {
            Debug.Assert(n >= 2);

            int r1, r2;

            r1 = Index(n);
            r2 = Index(n - 1);

            int i1 = r1;
            int i2 = (r1 + r2 + 1) % n;

            Debug.Assert(i1 != i2);

            index1 = i1;
            index2 = i2;
        }

        /// <summary>
        /// Draw a random integer from the set {0, .., n-1} according
        /// to the supplied probability distribution. Probabilities are
        /// assumed to sum to one. Time-complexity is O(n),
        /// where n is the length of the probabilities-array.
        /// Use this function if you only need to draw a few random numbers,
        /// otherwise create an IndexDistribution-object.
        /// Thread-safe if Uniform() is thread-safe.
        /// </summary>
        /// <param name="probabilities">Probability distribution.</param>
        public int Index(double[] probabilities)
        {
            double sum = 0;
            double r = Uniform();

            int i;

            for (i = 0; sum < r && i < probabilities.Length; i++)
            {
                sum += probabilities[i];
            }

            return i - 1;
        }
    }
}
