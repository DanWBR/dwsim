/// ------------------------------------------------------
/// SwarmOps - Numeric and heuristic optimization for C#
/// Copyright (C) 2003-2011 Magnus Erik Hvass Pedersen.
/// Please see the file license.txt for license details.
/// SwarmOps on the internet: http://www.Hvass-Labs.org/
/// ------------------------------------------------------

using System.Diagnostics;

namespace SwarmOps
{
    /// <summary>
    /// Compute statistical measures in accumulating manner, incl.
    /// mean, variance, std.deviation, min, max.
    /// </summary>
    public class StatisticsAccumulator
    {
        #region Constructors.
        /// <summary>
        /// Construct the object.
        /// </summary>
        public StatisticsAccumulator()
        {
            Clear();
        }
        #endregion

        #region Private fields.
        /// <summary>
        /// Accumulator variable.
        /// </summary>
        double Q;
        #endregion

        #region Public fields.
        /// <summary>
        /// Number of accumulations performed so far.
        /// </summary>
        public double Count
        {
            get;
            private set;
        }

        /// <summary>
        /// Mean.
        /// </summary>
        public double Mean
        {
            get;
            private set;
        }

        /// <summary>
        /// Min.
        /// </summary>
        public double Min
        {
            get;
            private set;
        }

        /// <summary>
        /// Max.
        /// </summary>
        public double Max
        {
            get;
            private set;
        }

        /// <summary>
        /// Variance.
        /// </summary>
        public double Variance
        {
            get { return Q / Count; }
        }

        /// <summary>
        /// Standard deviation.
        /// </summary>
        public double StandardDeviation
        {
            get { return System.Math.Sqrt(Variance); }
        }
        #endregion

        #region Public methods.
        /// <summary>
        /// Input data and accumulate variance.
        /// </summary>
        /// <param name="x">Data to input.</param>
        public void Accumulate(double x)
        {
            #region Mean and variance update.
            double meanOld = Mean;

            Mean = Mean + (x - Mean) / (Count + 1);
            Q = Q + (x - meanOld) * (x - Mean);

            Count++;
            #endregion

            #region Min and Max update.
            Min = System.Math.Min(Min, x);
            Max = System.Math.Max(Max, x);
            #endregion
        }

        /// <summary>
        /// Clear the accumulated variance.
        /// </summary>
        public void Clear()
        {
            Mean = 0;
            Q = 0;
            Count = 0;

            Min = System.Double.MaxValue;
            Max = System.Double.MinValue;
        }
        #endregion
    }
}
