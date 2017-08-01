/// ------------------------------------------------------
/// SwarmOps - Numeric and heuristic optimization for C#
/// Copyright (C) 2003-2011 Magnus Erik Hvass Pedersen.
/// Please see the file license.txt for license details.
/// SwarmOps on the internet: http://www.Hvass-Labs.org/
/// ------------------------------------------------------

using System.Diagnostics;
namespace SwarmOps.Problems
{
    /// <summary>
    /// Curve-fitting to the linear curve f(x) = A*x + B.
    /// This is meant as an example, you will want to use
    /// linear regression instead for real applications.
    /// </summary>
    public class CurveFittingLin : CurveFitting
    {
        #region Constructors.
        /// <summary>
        /// Create the object.
        /// </summary>
        /// <param name="x">X-axis values.</param>
        /// <param name="y">Y-axis values, curve to be fitted.</param>
        /// <param name="minA">Minimum value for A parameter.</param>
        /// <param name="maxA">Maximum value for A parameter.</param>
        /// <param name="minB">Minimum value for B parameter.</param>
        /// <param name="maxB">Maximum value for B parameter.</param>
        public CurveFittingLin(
            double[] x, double[] y,
            double minA, double maxA,
            double minB, double maxB)
            : base(x, y)
        {
            _lowerBound = new double[] { minA, minB };
            _lowerInit = _lowerBound;

            _upperBound = new double[] { maxA, maxB };
            _upperInit = _upperBound;
        }
        #endregion

        #region Get and set parameters.
        /// <summary>
        /// Get the A parameter.
        /// </summary>
        public double GetA(double[] parameters)
        {
            return parameters[0];
        }

        /// <summary>
        /// Set the A parameter.
        /// </summary>
        public void SetA(ref double[] parameters, double a)
        {
            parameters[0] = a;
        }

        /// <summary>
        /// Get the B parameter.
        /// </summary>
        public double GetB(double[] parameters)
        {
            return parameters[1];
        }

        /// <summary>
        /// Set the B parameter.
        /// </summary>
        public void SetB(ref double[] parameters, double b)
        {
            parameters[1] = b;
        }
        #endregion

        #region Base-class overrides, Problem.
        /// <summary>
        /// Name of the optimization problem.
        /// </summary>
        public override string Name
        {
            get { return "CurveFittingLinear"; }
        }

        /// <summary>
        /// Dimensionality of the optimization problem.
        /// </summary>
        public override int Dimensionality
        {
            get { return 2; }
        }

        /// <summary>
        /// Threshold for an acceptable fitness value.
        /// </summary>
        public override double AcceptableFitness
        {
            get
            {
                return 0.01;
            }
        }

        double[] _lowerBound;

        /// <summary>
        /// Lower boundary for the search-space.
        /// </summary>
        public override double[] LowerBound
        {
            get { return _lowerBound; }
        }

        double[] _upperBound;

        /// <summary>
        /// Upper boundary for the search-space.
        /// </summary>
        public override double[] UpperBound
        {
            get { return _upperBound; }
        }

        double[] _lowerInit;

        /// <summary>
        /// Lower initialization boundary.
        /// </summary>
        public override double[] LowerInit
        {
            get { return _lowerInit; }
        }

        double[] _upperInit;

        /// <summary>
        /// Upper initialization boundary.
        /// </summary>
        public override double[] UpperInit
        {
            get { return _upperInit; }
        }

        string[] _parameterName = { "a", "b" };

        /// <summary>
        /// Array with names of parameters.
        /// </summary>
        public override string[] ParameterName
        {
            get { return _parameterName; }
        }

        /// <summary>
        /// Has the gradient has been implemented?
        /// </summary>
        public override bool HasGradient
        {
            get { return true; }
        }

        /// <summary>
        /// Compute the gradient of the fitness-function.
        /// </summary>
        /// <param name="parameters">Candidate solution.</param>
        /// <param name="v">Array for holding the gradient.</param>
        public override int Gradient(double[] parameters, ref double[] v)
        {
            Debug.Assert(parameters != null && parameters.Length == Dimensionality);
            Debug.Assert(v != null && v.Length == Dimensionality);
            Debug.Assert(X.Length == Y.Length);

            // Get parameters.
            double a = GetA(parameters);
            double b = GetB(parameters);

            double gradientA = 0;
            double gradientB = 0;

            for (int i = 0; i < X.Length; i++)
            {
                double x = X[i];
                double y = Y[i];

                gradientA += x * (2 * x * a + b - y);
                gradientB += a * x + 2 * b - y;
            }

            SetA(ref v, gradientA);
            SetB(ref v, gradientB);

            return 0;
        }
        #endregion

        #region Base-class overrides, CurveFitting
        /// <summary>
        /// Compute the value y given x using the curve-fitting function.
        /// </summary>
        /// <param name="parameters">Parameters for curve-fitting function.</param>
        /// <param name="x">X-axis value.</param>
        /// <returns>Computed Y-axis value.</returns>
        public override double ComputeY(double[] parameters, double x)
        {
            double a = GetA(parameters);
            double b = GetB(parameters);

            return a * x + b;
        }

        #endregion
    }
}
