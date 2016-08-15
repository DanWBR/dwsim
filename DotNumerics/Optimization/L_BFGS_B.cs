#region Copyright © 2009, De Santiago-Castillo JA. All rights reserved.

//Copyright © 2009 Jose Antonio De Santiago-Castillo 
//E-mail:JAntonioDeSantiago@gmail.com
//Web: www.DotNumerics.com
//
#endregion

using System;
using System.Collections.Generic;
using System.Text;

using DotNumerics.Optimization.LBFGSB;

namespace DotNumerics.Optimization
{
    /// <summary>
    /// Class used to minimizes a function of several variables by using Limited memory Broyden–Fletcher–Goldfarb–Shanno (L-BFGS) method. This class can be used for unconstrained and bounded constrained minimization.
    /// </summary>
    public class L_BFGS_B : xMinimizationBase
    {

        #region Fields

        LBFGSBDriver _Driver = new LBFGSBDriver();

        /// <summary>
        /// Accuracy factor
        /// </summary>
        private double _AccuracyFactor = 1E7;


        #endregion


        #region Constructor
        /// <summary>
        /// Initializes a new instance of the L_BFG_S class.
        /// </summary>
        public L_BFGS_B()
        {
        }

        #endregion


        #region Properties

        /// <summary>
        /// Accuracy factor. The iteration will stop when (f^k - f^{k+1})/max{|f^k|,|f^{k+1}|,1} is least than AccuracyFactor*epsmch
        /// where epsmch is the machine precision. Typical values for AccuracyFactor: 1E12 for low accuracy; 1E7 for moderate accuracy; 
        /// 1E1 for extremely high accuracy.
        /// </summary>
        public double AccuracyFactor
        {
            get { return _AccuracyFactor; }
            set { _AccuracyFactor = value; }
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
        public double[] ComputeMin(OptMultivariateFunction function, OptMultivariateGradient gradient,  double[] initialGuess)
        {

            if (initialGuess == null) return new double[0];
            if (initialGuess.Length == 0) return new double[0];

            OptVariable[] variables = this.GetVariables(initialGuess);

            int maxFunc = this._MaxFunEvaluations;

            double[] minimum= this._Driver.ComputeMin(function, gradient, variables, this._Tolerance, this._AccuracyFactor, ref maxFunc);
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

            double[] minimum = this._Driver.ComputeMin(function, gradient, variables, this._Tolerance, this._AccuracyFactor, ref maxFunc);
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

            double[] minimum = this._Driver.ComputeMin(function, gradient, variables, this._Tolerance, this._AccuracyFactor, ref maxFunc);
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
