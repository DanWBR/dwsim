/// ------------------------------------------------------
/// RandomOps - (Pseudo) Random Number Generator For C#
/// Copyright (C) 2003-2010 Magnus Erik Hvass Pedersen.
/// Please see the file license.txt for license details.
/// RandomOps on the internet: http://www.Hvass-Labs.org/
/// ------------------------------------------------------

using System;

namespace RandomOps
{
    /// <remarks>
    /// Implements a Gaussian RNG using the Uniform() method.
    /// </remarks>
    public abstract partial class Random
    {
        /// <summary>
        /// Next Gaussian random number.
        /// </summary>
        double _gaussian;

        /// <summary>
        /// Does _gaussian hold a value?
        /// </summary>
        bool _gaussReady = false;

        /// <summary>
        /// Draw a Gaussian (or normally) distributed random number, with designated
        /// mean and deviation. Thread-safe if Gauss() is thread-safe.
        /// </summary>
        /// <param name="mean">The mean of the distribution, e.g. 0.</param>
        /// <param name="deviation">The deviation of the distribution, e.g. 1.</param>
        public virtual double Gauss(double mean, double deviation)
        {
            return deviation * Gauss() + mean;
        }

        /// <summary>
        /// Draw a Gaussian (or normally) distributed random number, with mean 0 and
        /// deviation 1. Not thread-safe.
        /// </summary>
        public virtual double Gauss()
        {
            double value;

            if (_gaussReady)
            {
                value = _gaussian;
                _gaussReady = false;
            }
            else
            {
                double v1, v2, rsq;

                // Pick two uniform numbers in the unit-radius 2-dim ball.
                Disk(out v1, out v2, out rsq);

                double fac = Math.Sqrt(-2.0 * Math.Log(rsq) / rsq);

                // Now make the Box-Muller transformation to get two normal deviates.
                // Return one and save the other for next time.
                _gaussian = v1 * fac;
                _gaussReady = true;

                value = v2 * fac;
            }

            return value;
        }
    }
}
