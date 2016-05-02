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
    /// Implements RNG of a circle. The method is taken from:
    /// [1] von Neumann, J. "Various Techniques Used in Connection with Random Digits."
    ///     NBS Appl. Math. Ser., No. 12. Washington, DC: U.S. Government Printing Office,
    ///     pp. 36-38, 1951.
    /// </remarks>
    public abstract partial class Random
    {
        /// <summary>
        /// Generate a uniform random point from the unit-radius 2-dimensional circle.
        /// Thread-safe if Disk() is thread-safe.
        /// </summary>
        public virtual double[] Circle()
        {
            double[] x = new double[2];

            Circle(out x[0], out x[1]);

            return x;
        }

        /// <summary>
        /// Generate a uniform random point from the unit-radius 2-dimensional circle.
        /// Thread-safe if Disk() is thread-safe.
        /// </summary>
        /// <param name="x">Random point x</param>
        /// <param name="y">Random point y</param>
        public virtual void Circle(out double x, out double y)
        {
            double v1, v2, s;

            Disk(out v1, out v2, out s);

            x = (v1 * v1 - v2 * v2) / s;
            y = 2 * v1 * v2 / s;
        }
    }
}
