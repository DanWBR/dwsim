/// ------------------------------------------------------
/// SwarmOps - Numeric and heuristic optimization for C#
/// Copyright (C) 2003-2011 Magnus Erik Hvass Pedersen.
/// Please see the file license.txt for license details.
/// SwarmOps on the internet: http://www.Hvass-Labs.org/
/// ------------------------------------------------------

using System.Diagnostics;

namespace SwarmOps.Optimizers
{
    /// <summary>
    /// Variants of crossover operator for Differential Evolution (DE),
    /// originally due to Storner and Price (1).
    /// </summary>
    /// <remarks>
    /// References:
    /// (1) R. Storn and K. Price. Differential evolution - a simple
    ///     and efficient heuristic for global optimization over
    ///     continuous spaces. Journal of Global Optimization,
    ///     11:341-359, 1997.
    /// </remarks>
    public static class DECrossover
    {
        #region Variants.
        /// <summary>
        /// Enumeration of DE crossover variants.
        /// </summary>
        public enum Variant
        {
            Rand1Bin,
            Best1Bin,
        }

        /// <summary>
        /// Name of DE crossover variant.
        /// </summary>
        /// <param name="kind"></param>
        /// <returns></returns>
        public static string Name(Variant variant)
        {
            string s;

            switch (variant)
            {
                case Variant.Best1Bin:
                    s = "Best1Bin";
                    break;

                case Variant.Rand1Bin:
                    s = "Rand1Bin";
                    break;

                default:
                    s = "Unknown";
                    break;
            }

            return s;
        }
        #endregion

        #region Crossover
        /// <summary>
        /// Perform DE crossover.
        /// </summary>
        /// <param name="crossover">Crossover variant to be performed.</param>
        /// <param name="CR">Crossover probability.</param>
        /// <param name="n">Dimensionality for problem.</param>
        /// <param name="w">Differential weight (vector).</param>
        /// <param name="x">Current agent position.</param>
        /// <param name="y">Potentially new agent position.</param>
        /// <param name="g">Population's best known position.</param>
        /// <param name="agents">Entire population.</param>
        /// <param name="randomSet">Random-set used for drawing distinct agents.</param>
        public static void DoCrossover(Variant crossover, double CR, int n, double[] w, double[] x, ref double[] y, double[] g, double[][] agents, RandomOps.Set randomSet)
        {
            // Agents used in crossover.
            double[] a, b, c;

            switch (crossover)
            {
                case Variant.Best1Bin:
                    {
                        // The first agent used in crossover is g.
                        a = g;

                        // Pick random and distinct agent-indices.
                        // Also distinct from agent x.
                        int R1 = randomSet.Draw();
                        int R2 = randomSet.Draw();

                        b = agents[R1];
                        c = agents[R2];
                    }
                    break;

                case Variant.Rand1Bin:
                default:
                    {
                        // Pick random and distinct agent-indices.
                        // Also distinct from agent x.
                        int R1 = randomSet.Draw();
                        int R2 = randomSet.Draw();
                        int R3 = randomSet.Draw();

                        // Refer to the randomly picked agents as a and b.
                        a = agents[R1];
                        b = agents[R2];
                        c = agents[R3];
                    }
                    break;
            }

            // Pick a random dimension.
            int R = Globals.Random.Index(n);

            // Compute potentially new position.
            for (int k = 0; k < n; k++)
            {
                if (k == R || Globals.Random.Bool(CR))
                {
                    y[k] = a[k] + w[k] * (b[k] - c[k]);
                }
                else
                {
                    y[k] = x[k];
                }
            }
        }
        #endregion
    }
}