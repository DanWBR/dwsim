/// ------------------------------------------------------
/// SwarmOps - Numeric and heuristic optimization for C#
/// Copyright (C) 2003-2011 Magnus Erik Hvass Pedersen.
/// Please see the file license.txt for license details.
/// SwarmOps on the internet: http://www.Hvass-Labs.org/
/// ------------------------------------------------------

using System.Collections.Generic;
using System.Linq;

namespace SwarmOps.Extensions
{
    /// <summary>
    /// Various extensions to System-classes.
    /// </summary>
    public static partial class ExtensionMethods
    {
        /// <summary>
        /// Computes the sum of squares of a sequence of System.Double values that
        /// are obtained by invoking a transform function on each element of the
        /// input sequence.
        /// </summary>
        /// <typeparam name="TSource">The type of the elements of source.</typeparam>
        /// <param name="source">A sequence of values to calculate the sum of squares of.</param>
        /// <param name="selector">A transform function to apply to each element.</param>
        /// <returns>The sum of squares of the sequence of values.</returns>
        public static double SumSquares<TSource>(this IEnumerable<TSource> source, System.Func<TSource, double> selector)
        {
            return source.Sum((elm) => { double x = selector(elm); return x * x; });
        }

        /// <summary>
        /// Computes the standard deviation of a sequence of System.Double values that
        /// are obtained by invoking a transform function on each element of the
        /// input sequence.
        /// </summary>
        /// <typeparam name="TSource">The type of the elements of source.</typeparam>
        /// <param name="source">A sequence of values to calculate the standard deviation of.</param>
        /// <param name="selector">A transform function to apply to each element.</param>
        /// <returns>The standard deviation of the sequence of values.</returns>
        public static double StdDev<TSource>(this IEnumerable<TSource> source, System.Func<TSource, double> selector)
        {
            double sumSquares = source.SumSquares(selector);
            double mean = source.Average(selector);

            double stdDev = System.Math.Sqrt(sumSquares / source.Count() - mean * mean);

            return stdDev;
        }
    }
}
