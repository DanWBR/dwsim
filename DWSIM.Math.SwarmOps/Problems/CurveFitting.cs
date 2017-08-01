/// ------------------------------------------------------
/// SwarmOps - Numeric and heuristic optimization for C#
/// Copyright (C) 2003-2011 Magnus Erik Hvass Pedersen.
/// Please see the file license.txt for license details.
/// SwarmOps on the internet: http://www.Hvass-Labs.org/
/// ------------------------------------------------------

namespace SwarmOps.Problems
{
    /// <summary>
    /// Base-class for curvefitting by minimizing the Mean-Squared-Error (MSE),
    /// that is, minimizing Sum((y - f(x))^2), for all given data-pairs (x,y).
    /// </summary>
    public abstract class CurveFitting : Problem
    {
        #region Constructors.
        /// <summary>
        /// Create the object.
        /// </summary>
        /// <param name="x">X-axis values.</param>
        /// <param name="y">Y-axis values, curve to be fitted.</param>
        public CurveFitting(double[] x, double[] y)
            : base()
        {
            if (x.Length == 0 || y.Length == 0)
            {
                throw new System.ArgumentException("Zero-length array.");
            }

            if (x.Length != y.Length)
            {
                throw new System.ArgumentException("Different array lengths.");
            }

            X = x;
            Y = y;
        }
        #endregion

        #region Public fields.
        /// <summary>
        /// X-axis values.
        /// </summary>
        public double[] X
        {
            get;
            private set;
        }

        /// <summary>
        /// Y-axis values.
        /// </summary>
        public double[] Y
        {
            get;
            private set;
        }
        #endregion

        #region Public methods.
        /// <summary>
        /// Compute the fitted curve.
        /// </summary>
        /// <param name="parameters">Parameters to use for the curve-function.</param>
        /// <param name="x">X-axis values to map to Y-axis values for the fitted curve.</param>
        /// <returns>Fitted curve.</returns>
        public double[] ComputeY(double[] parameters, double[] x)
        {
            double[] y = new double[x.Length];

            for (int i = 0; i < x.Length; i++)
            {
                y[i] = ComputeY(parameters, x[i]);
            }

            return y;
        }
        #endregion

        #region Override these.
        /// <summary>
        /// Compute the value y given x using the curve-fitting function.
        /// </summary>
        /// <param name="parameters">Parameters for curve-fitting function.</param>
        /// <param name="x">X-axis value.</param>
        /// <returns>Computed Y-axis value.</returns>
        public abstract double ComputeY(double[] parameters, double x);
        #endregion

        #region Base-class overrides.
        /// <summary>
        /// Minimum possible fitness.
        /// </summary>
        public override double MinFitness
        {
            get { return 0; }
        }

        /// <summary>
        /// Compute and return fitness for the given parameters.
        /// </summary>
        /// <param name="parameters">Candidate solution.</param>
        public override double Fitness(double[] parameters)
        {
            double sse = 0;

            for (int i = 0; i < X.Length; i++)
            {
                double x = X[i];
                double y = Y[i];
                double computedY = ComputeY(parameters, x);

                sse += System.Math.Pow(computedY - y, 2);
            }

            double mse = sse / X.Length;

            return mse;
        }
        #endregion
    }
}
