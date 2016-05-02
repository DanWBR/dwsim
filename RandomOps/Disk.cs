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
    /// Implements RNG of a disk.
    /// </remarks>
    public abstract partial class Random
    {
        /// <summary>
        /// Generate a uniform random point from the unit-radius 2-dimensional disk.
        /// Thread-safe if Uniform() is thread-safe.
        /// </summary>
        public virtual double[] Disk()
        {
            double[] x = new double[2];

            double sumSquares;

            Disk(out x[0], out x[1], out sumSquares);

            return x;
        }

        /// <summary>
        /// Generate a uniform random point from the unit-radius 2-dimensional disk.
        /// Thread-safe if Uniform() is thread-safe.
        /// </summary>
        /// <param name="x">Random point x</param>
        /// <param name="y">Random point y</param>
        /// <param name="sumSquares">Equals x*x + y*y</param>
        public virtual void Disk(out double x, out double y, out double sumSquares)
        {
            // Pick two uniform numbers in the square (-1,1) x (-1,1). See if the
            // numbers are inside the unit circle, and if they are not, try again.

            // Succesful points occupy the inside of the unit circle, whose area is
            // pi, or about 3.14. Since we sample uniformly from a square of size 4,
            // the probability of the loop succeeding in a single iteration is pi/4,
            // or about 0.7854. 
            // Probability of success in two iterations is therefore 0.954, in three
            // iterations it is about 0.990, and the probability of success in
            // four successive iterations is approximately 0.998, etc. -- provided there
            // is no correlation between calls to Uniform(-1, 1).

            // The average number of iterations before success is 1.27

            do
            {
                x = Uniform(-1, 1);
                y = Uniform(-1, 1);
                sumSquares = x * x + y * y;
            }
            while (sumSquares > 1);
        }
    }
}
