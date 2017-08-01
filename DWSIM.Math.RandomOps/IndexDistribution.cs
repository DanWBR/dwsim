/// ------------------------------------------------------
/// RandomOps - (Pseudo) Random Number Generator For C#
/// Copyright (C) 2003-2010 Magnus Erik Hvass Pedersen.
/// Please see the file license.txt for license details.
/// RandomOps on the internet: http://www.Hvass-Labs.org/
/// ------------------------------------------------------

namespace RandomOps
{
    /// <summary>
    /// A set of integers enumerated from zero and upwards that
    /// can be drawn at random according to the given probability
    /// distribution. Probabilities are assumed to sum to one.
    /// Various functions for drawing numbers are supplied with
    /// different time-complexity advantages. Thread-safe if
    /// supplied RNG's Uniform() is thread-safe.
    /// </summary>
    public class IndexDistribution
    {
        #region Constructors.
        /// <summary>
        /// Create the object. Time-complexity for doing this is O(n),
        /// where n is the length of the probabilities-array. 
        /// </summary>
        /// <param name="rand">RNG object to use.</param>
        /// <param name="probabilities">Probability distribution.</param>
        public IndexDistribution(Random rand, double[] probabilities)
        {
            Rand = rand;

            ProbabilitySum = new double[probabilities.Length + 1];
            ProbabilitySum[0] = 0;

            double sum = 0;
            for (int i = 0; i < probabilities.Length; i++)
            {
                sum += probabilities[i];
                ProbabilitySum[i + 1] = sum;
            }
        }
        #endregion

        #region Public methods.
        /// <summary>
        /// Draw a random integer from {0, .., n-1} according to previously
        /// supplied probability distribution. Time-complexity is O(n),
        /// where n is the length of the probabilities-array supplied to
        /// CreateIndexDistribution().
        /// Use this function if you make repeated draws from the same
        /// set and the set-size is small.
        /// </summary>
        /// <returns></returns>
        public int DrawLinearSearch()
        {
            double r = Rand.Uniform();

            int i;

            for (i = 0; ProbabilitySum[i] < r && i < ProbabilitySum.Length-1; i++)
            {
            }

            return i - 1;
        }

        /// <summary>
        /// Draw a random integer from {0, .., n-1} according to previously
        /// supplied probability distribution. Time-complexity is O(log(n)),
        /// where n is the length of the probabilities-array supplied to
        /// CreateIndexDistribution().
        /// Use this function if you make repeated draws from the same
        /// set and the set-size is large.
        /// </summary>
        public int DrawBinarySearch()
        {
            double r = Rand.Uniform();
            int minIdx=0;
            int maxIdx = ProbabilitySum.Length - 1;

            while (minIdx < maxIdx - 1)
            {
                int middle = (maxIdx - minIdx) / 2 + minIdx;

                if (r < ProbabilitySum[middle])
                {
                    maxIdx = middle;
                }
                else
                {
                    minIdx = middle;
                }
            }

            return minIdx;
        }
        #endregion

        #region Public properties.
        /// <summary>
        /// RNG object being used.
        /// </summary>
        public Random Rand
        {
            get;
            set;
        }
        #endregion

        #region Internal variables.
        /// <summary>
        /// Intermediate summation of the probability-distribution.
        /// </summary>
        private double[] ProbabilitySum;
        #endregion
    }
}
