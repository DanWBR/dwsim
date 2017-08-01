#region Copyright © 2009, De Santiago-Castillo JA. All rights reserved.

//Copyright © 2009 Jose Antonio De Santiago-Castillo 
//E-mail:JAntonioDeSantiago@gmail.com
//Web: www.DotNumerics.com
//
#endregion

using System;
using System.Collections.Generic;
using System.Text;
using System.ComponentModel;

using DotNumerics.Optimization.TN;

namespace DotNumerics.Optimization
{
    /// <summary>
    /// Minimization a multivariate function using a Truncated Newton algorithm. This class can be used for unconstrained and bounded constrained minimization.
    /// </summary>
    public sealed class TruncatedNewton : xMinimizationBase
    {

        #region Fields

        TNDriver _TNDriver = new TNDriver();

        /// <summary>
        /// The accuracy of the computed function values.
        /// </summary>
        private double _Accuracy = 1E-10;

        /// <summary>
        /// The severity of the linesearch.
        /// </summary>
        private double _SearchSeverity = 0.25;

        /// <summary>
        /// The maximum allowable step in the linesearch.
        /// </summary>
        private double _MaximunStep = 10;


        #endregion

        #region Constructor

        /// <summary>
        /// Initializes a new instance of the MinTruncatedNewton class.
        /// </summary>
        public TruncatedNewton()
        {

        }


        #endregion


        #region Properties

        /// <summary>
        /// The accuracy of the computed function values.
        /// </summary>
        public double Accuracy
        {
            get { return _Accuracy; }
            set { _Accuracy = value; }
        }


        /// <summary>
        /// The severity of the linesearch.
        /// </summary>
        public double SearchSeverity
        {
            get { return _SearchSeverity; }
            set { _SearchSeverity = value; }
        }

        /// <summary>
        /// The maximum allowable step in the linesearch.
        /// </summary>
        public double MaximunStep
        {
            get { return _MaximunStep; }
            set { _MaximunStep = value; }
        }


        #endregion


        #region Public methods

        /// <summary>
        /// Computes the minimum point of a function of several variables.
        /// </summary>
        /// <param name="function">The function to minimize.</param>
        /// <param name="gradient">A delegate that computes the gradient.</param>
        /// <param name="initialGuess">Array of size N containing the initial guess. N is the number of variables.</param>
        /// <returns>Array containing the solution.</returns>
        public double[] ComputeMin(OptMultivariateFunction function, OptMultivariateGradient gradient, double[] initialGuess)
        {

            if (initialGuess == null) return new double[0];
            if (initialGuess.Length == 0) return new double[0];

            OptVariable[] variables = this.GetVariables(initialGuess);


            int maxFunc = this._MaxFunEvaluations;

            this._TNDriver.ETA = this._SearchSeverity;
            this._TNDriver.STEPMX = this._MaximunStep;

            double[] minimum = this._TNDriver.ComputeMin(function, gradient, variables, this._Tolerance, this._Accuracy, ref maxFunc);
            this._FunEvaluations = maxFunc;
            return minimum;
        }


        /// <summary>
        /// Computes the minimum point of a function of several variables.
        /// </summary>
        /// <param name="function">The function to minimize.</param>
        /// <param name="gradient">A delegate that computes the gradient.</param>
        /// <param name="variables">Array of size N containing the varaibles.</param>
        /// <returns>Array containing the solution.</returns>
        public double[] ComputeMin(OptMultivariateFunction function, OptMultivariateGradient gradient, OptVariable[] variables)
        {

            if (variables == null) return new double[0];
            if (variables.Length == 0) return new double[0];

            int maxFunc = this._MaxFunEvaluations;

            this._TNDriver.ETA = this._SearchSeverity;
            this._TNDriver.STEPMX = this._MaximunStep;

            double[] minimum = this._TNDriver.ComputeMin(function, gradient, variables, this._Tolerance, this._Accuracy, ref maxFunc);
            this._FunEvaluations = maxFunc;
            return minimum;
        }


        /// <summary>
        /// Computes the minimum point of a function of several variables.
        /// </summary>
        /// <param name="function">The function to minimize.</param>
        /// <param name="gradient">A delegate that computes the gradient.</param>
        /// <param name="variables">Array of size N containing the varaibles.</param>
        /// <returns>Array containing the solution.</returns>
        public double[] ComputeMin(OptMultivariateFunction function, OptMultivariateGradient gradient, OptBoundVariable[] variables)
        {

            if (variables == null) return new double[0];
            if (variables.Length == 0) return new double[0];

            int maxFunc = this._MaxFunEvaluations;

            this._TNDriver.ETA = this._SearchSeverity;
            this._TNDriver.STEPMX = this._MaximunStep;

            double[] minimum = this._TNDriver.ComputeMin(function, gradient, variables, this._Tolerance, this._Accuracy, ref maxFunc);
            this._FunEvaluations = maxFunc;
            return minimum;
        }

        #endregion


        #region private Methods

        private OptVariable[] GetVariables(double[] variablesArray)
        {
            OptVariable[] vars = new OptVariable[variablesArray.Length];

            for (int i = 0; i < variablesArray.Length; i++)
            {
                vars[i] = new OptVariable(variablesArray[i]);
            }

            return vars;
        }

        #endregion

    }



}
