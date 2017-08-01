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
    /// Implements RNG for a hypersphere. The methods are taken from:
    /// [1] Marsaglia, G. "Choosing a Point from the Surface of a Sphere."
    ///     Ann. Math. Stat. 43, 645-646, 1972. 
    /// [2] Muller, M. E. "A Note on a Method for Generating Points Uniformly
    ///     on n-Dimensional Spheres."
    ///     Comm. Assoc. Comput. Mach. 2, 19-20, Apr. 1959. 
    /// </remarks>
    public abstract partial class Random
    {
        /// <summary>
        /// Generate a uniform random point on the unit-radius 3-dimensional sphere.
        /// Thread-safe if Disk() is thread-safe.
        /// </summary>
        public virtual double[] Sphere3()
        {
            double[] x = new double[3];

            Sphere3(ref x);

            return x;
        }

        /// <summary>
        /// Generate a uniform random point on the unit-radius 3-dimensional sphere.
        /// Thread-safe if Disk() is thread-safe.
        /// </summary>
        /// <param name="x">Array to hold the random point.</param>
        public virtual void Sphere3(ref double[] x)
        {
            double v1, v2, s;

            // Pick two uniform numbers in the unit-radius 2-dim ball.
            Disk(out v1, out v2, out s);

            double a = Math.Sqrt(1 - s);

            x[0] = 2 * v1 * a;
            x[1] = 2 * v2 * a;
            x[2] = 1 - 2 * s;
        }

        /// <summary>
        /// Generate a uniform random point on the unit-radius 4-dimensional sphere.
        /// Thread-safe if Disk() is thread-safe.
        /// </summary>
        public virtual double[] Sphere4()
        {
            double[] x = new double[4];

            Sphere4(ref x);

            return x;
        }

        /// <summary>
        /// Generate a uniform random point on the unit-radius 4-dimensional sphere.
        /// Thread-safe if Disk() is thread-safe.
        /// </summary>
        /// <param name="x">Array to hold the random point.</param>
        public virtual void Sphere4(ref double[] x)
        {
            double v1, v2, v3, v4, s1, s2;

            // Pick uniform numbers in the unit-radius 2-dim ball.
            Disk(out v1, out v2, out s1);
            Disk(out v3, out v4, out s2);

            double a = Math.Sqrt((1 - s1) / s2);

            x[0] = v1;
            x[1] = v2;
            x[2] = v3 * a;
            x[3] = v4 * a;
        }

        /// <summary>
        /// Generate a uniform random point on the n-dimensional hypersphere.
        /// Thread-safe if Gauss() is thread-safe.
        /// </summary>
        /// <param name="n">Dimensionality of hypersphere.</param>
        /// <param name="r">Radius of hypersphere.</param>
        public virtual double[] Sphere(int n, double r)
        {
            Debug.Assert(n > 0);

            double[] x = new double[n];

            Sphere(ref x, r);

            return x;
        }

        /// <summary>
        /// Generate a uniform random point on the n-dimensional hypersphere.
        /// Thread-safe if Gauss() is thread-safe, and each thread supplies
        /// its own array x.
        /// </summary>
        /// <param name="x">Array to hold the random point.</param>
        /// <param name="r">Radius of hypersphere.</param>
        public virtual void Sphere(ref double[] x, double r)
        {
            Debug.Assert(x != null);

            int n = x.Length;
            Debug.Assert(n > 0);

            double sum = 0;
            int i;

            for (i = 0; i < n; i++)
            {
                // Draw a gaussian (aka. normal) random number.
                double a = Gauss();

                // Store the element.
                x[i] = a;

                // Accumulate sum of squared elements.
                sum += a * a;
            }

            // Adjust elements to get a certain radius.
            double rInv = r / Math.Sqrt(sum);

            for (i = 0; i < n; i++)
            {
                x[i] *= rInv;
            }
        }
    }
}
