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
    /// Compute quartiles.
    /// </summary>
    public class Quartiles
    {
        #region Constructors.
        /// <summary>
        /// Construct the object.
        /// </summary>
        public Quartiles()
        {
            Clear();
        }
        #endregion

        #region Public fields.
        /// <summary>
        /// Minimum element.
        /// </summary>
        public double? Min
        {
            get;
            private set;
        }

        /// <summary>
        /// Maximum element.
        /// </summary>
        public double? Max
        {
            get;
            private set;
        }

        /// <summary>
        /// Median, if array is odd-length the middle element
        /// is the median, otherwise if array is even-length the
        /// median is the mean of the two middle elements.
        /// </summary>
        public double? Median
        {
            get;
            private set;
        }

        /// <summary>
        /// First quartile, if between two elements the one
        /// closest to the median is taken.
        /// </summary>
        public double? Q1
        {
            get;
            private set;
        }

        /// <summary>
        /// Same as Median.
        /// </summary>
        public double? Q2
        {
            get { return Median; }
        }

        /// <summary>
        /// Third quartile, if between two elements the one
        /// closest to the median is taken.
        /// </summary>
        public double? Q3
        {
            get;
            private set;
        }

        /// <summary>
        /// Inter-quartile range, Q3-Q1.
        /// </summary>
        public double? IQR
        {
            get;
            private set;
        }
        #endregion

        #region Public methods.
        /// <summary>
        /// Clear all quartiles.
        /// </summary>
        public void Clear()
        {
            Min = null;
            Max = null;
            Median = null;
            Q1 = null;
            Q3 = null;
            IQR = null;
        }

        /// <summary>
        /// Compute quartiles for a sorted array of values.
        /// </summary>
        /// <param name="x">Sorted array of values.</param>
        public void Compute(double[] x)
        {
            if (x.Length > 0)
            {
                ComputeMedian(x);
                ComputeQ1(x);
                ComputeQ3(x);

                IQR = Q3 - Q1;

                Min = x[0];
                Max = x[x.Length - 1];
            }
            else
            {
                Clear();
            }
        }

        /// <summary>
        /// Compute quartiles for an unsorted array of values.
        /// Copies the source-array before sorting it.
        /// </summary>
        /// <param name="x">Un-sorted array of values.</param>
        public void ComputeUnsorted(double[] x)
        {
            double[] y = new double[x.Length];

            x.CopyTo(y, 0);

            System.Array.Sort(y);

            Compute(y);
        }

        /// <summary>
        /// Compute quartiles for an unsorted array of values.
        /// Sorts the source-array inplace.
        /// </summary>
        /// <param name="x">Un-sorted array of values.</param>
        public void ComputeUnsortedInplace(double[] x)
        {
            System.Array.Sort(x);

            Compute(x);
        }
        #endregion

        #region Private methods.
        /// <summary>
        /// Compute first quartile.
        /// </summary>
        /// <param name="x">Sorted array of values.</param>
        void ComputeQ1(double[] x)
        {
            double indexD = (x.Length + 1) * 0.25;
            int index = (int)System.Math.Ceiling(indexD);

            Q1 = x[index - 1];
        }

        /// <summary>
        /// Compute median.
        /// </summary>
        /// <param name="x">Sorted array of values.</param>
        void ComputeMedian(double[] x)
        {
            int index = (x.Length - 1) / 2;

            Median = (x.Length % 2 == 0) ? (0.5 * (x[index] + x[index + 1])) : (x[index]);
        }

        /// <summary>
        /// Compute third quartile.
        /// </summary>
        /// <param name="x">Sorted array of values.</param>
        void ComputeQ3(double[] x)
        {
            double indexD = (x.Length + 1) * 0.75;
            int index = (int)System.Math.Floor(indexD);

            Q3 = x[index - 1];
        }

        #endregion
    }
}
