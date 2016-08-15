#region Copyright © 2009, De Santiago-Castillo JA. All rights reserved.

//Copyright © 2009 Jose Antonio De Santiago-Castillo 
//E-mail:JAntonioDeSantiago@gmail.com
//Web: www.DotNumerics.com
//
#endregion

using System;
using System.Collections.Generic;
using System.Text;

using DotNumerics.Optimization.Cobyla;
using DotNumerics.Optimization.NelderMead;

namespace DotNumerics.Optimization
{
    /// <summary>
    /// Class used to minimizes a function of several variables by using the Nelder-Mead (Simplex) method. This class can be used for unconstrained and bounded constrained minimization.
    /// </summary>
    public class Simplex : xMinimizationBase
    {

        #region Fields

        CobylaDriver _CobylaDriver;
        DownhillDirver _AmoebaDirver;

        //COBYLA MeCOBYLA;

        //private OptMultivariateFunction Function;

        //private OptBoundVariable[] MeSimplexBoundVariableList;

        //private int MeNumBoundVariables = 0;
        //private double MeInitialStep = 1;

        //double[] W;
        //int[] IACT;

        #endregion


        #region Constructor

        /// <summary>
        /// Initializes a new instance of the Simplex class.
        /// </summary>
        public Simplex()
        {

        }


        #endregion


        #region Public methods

        #region double

        /// <summary>
        /// Computes the minimum point of a function of several variables.
        /// </summary>
        /// <param name="function">The function to minimize.</param>
        /// <param name="initialGuess">Array of size N containing the initial guess. N is the number of variables.</param>
        /// <returns>Array containing the solution.</returns>
        public double[] ComputeMin(OptMultivariateFunction function, double[] initialGuess)
        {

            if (initialGuess == null) return new double[0];
            if (initialGuess.Length == 0) return new double[0];

            OptSimplexVariable[] variables = this.GetVariables(initialGuess);

            double initialStep = this.GetAutomaticInitialStep(variables);

            int maxFunc= this._MaxFunEvaluations;

            return this.GetMinimum(function, variables, initialStep, this._Tolerance, ref maxFunc);
        }

        /// <summary>
        /// Computes the minimum point of a function of several variables.
        /// </summary>
        /// <param name="function">The function to minimize.</param>
        /// <param name="initialGuess">Array of size N containing the initial guess. N is the number of variables.</param>
        /// <param name="initialStep">The initial change of the variables.</param>
        /// <returns>Array containing the solution.</returns>
        public double[] ComputeMin(OptMultivariateFunction function, double[] initialGuess, double initialStep)
        {

            if (initialGuess == null) return new double[0];
            if (initialGuess.Length == 0) return new double[0];

            OptSimplexVariable[] variables = this.GetVariables(initialGuess);

            int maxFunc = this._MaxFunEvaluations;

            return this.GetMinimum(function, variables, initialStep, this._Tolerance, ref maxFunc);
        }


        #endregion

        #region OptSimplexVariable

        /// <summary>
        /// Computes the minimum point of a function of several variables.
        /// </summary>
        /// <param name="function">The function to minimize.</param>
        /// <param name="variables">Array of size N containing the varaibles.</param>
        /// <returns>Array containing the solution.</returns>
        public double[] ComputeMin(OptMultivariateFunction function, OptSimplexVariable[] variables)
        {

            if (variables == null) return new double[0];
            if (variables.Length == 0) return new double[0];

            double initialStep = this.GetAutomaticInitialStep(variables);
            int maxFunc = this._MaxFunEvaluations;

            return this.GetMinimum(function, variables, initialStep, this._Tolerance, ref maxFunc);
        }

        /// <summary>
        /// Computes the minimum point of a function of several variables.
        /// </summary>
        /// <param name="function">The function to minimize.</param>
        /// <param name="variables">Array of size N containing the varaibles.</param>
        /// <param name="initialStep">The initial change of the variables.</param>
        /// <returns>Array containing the solution.</returns>
        public double[] ComputeMin(OptMultivariateFunction function, OptSimplexVariable[] variables, float initialStep)
        {

            if (variables == null) return new double[0];
            if (variables.Length == 0) return new double[0];

            int maxFunc = this._MaxFunEvaluations;

            return this.GetMinimum(function, variables, initialStep, this._Tolerance, ref maxFunc);
        }

        #endregion

        #region OptVariable

        /// <summary>
        /// Computes the minimum point of a function of several variables.
        /// </summary>
        /// <param name="function">The function to minimize.</param>
        /// <param name="variables">Array of size N containing the initial guess. N is the number of variables.</param>
        /// <returns>Array containing the solution.</returns>
        public double[] ComputeMin(OptMultivariateFunction function, OptVariable[] variables)
        {

            if (variables == null) return new double[0];
            if (variables.Length == 0) return new double[0];

            OptSimplexVariable[] simplexVariables = this.GetVariables(variables);

            double initialStep = this.GetAutomaticInitialStep(simplexVariables);

            int maxFunc = this._MaxFunEvaluations;

            return this.GetMinimum(function, simplexVariables, initialStep, this._Tolerance, ref maxFunc);
        }

        /// <summary>
        /// Computes the minimum point of a function of several variables.
        /// </summary>
        /// <param name="function">The function to minimize.</param>
        /// <param name="variables">Array of size N containing the initial guess. N is the number of variables.</param>
        /// <param name="initialStep">The initial change of the variables.</param>
        /// <returns>Array containing the solution.</returns>
        public double[] ComputeMin(OptMultivariateFunction function, OptVariable[] variables, double initialStep)
        {

            if (variables == null) return new double[0];
            if (variables.Length == 0) return new double[0];

            OptSimplexVariable[] simplexVariables = this.GetVariables(variables);

            int maxFunc = this._MaxFunEvaluations;

            return this.GetMinimum(function, simplexVariables, initialStep, this._Tolerance, ref maxFunc);
        }

        #endregion

        #region OptSimplexBoundVariable

        /// <summary>
        /// Computes the minimum point of a function of several variables.
        /// </summary>
        /// <param name="function">The function to minimize.</param>
        /// <param name="variables">Array of size N containing the varaibles.</param>
        /// <returns>Array containing the solution.</returns>
        public double[] ComputeMin(OptMultivariateFunction function, OptSimplexBoundVariable[] variables)
        {

            if (variables == null) return new double[0];
            if (variables.Length == 0) return new double[0];

            double initialStep = this.GetAutomaticInitialStep(variables);
            int maxFunc = this._MaxFunEvaluations;

            return this.GetMinimum(function, variables, initialStep, this._Tolerance, ref maxFunc);
        }


        /// <summary>
        /// Computes the minimum point of a function of several variables.
        /// </summary>
        /// <param name="function">The function to minimize.</param>
        /// <param name="variables">Array of size N containing the varaibles.</param>
        /// <param name="initialStep">The initial change of the variables.</param>
        /// <returns>Array containing the solution.</returns>
        public double[] ComputeMin(OptMultivariateFunction function, OptSimplexBoundVariable[] variables, float initialStep)
        {

            if (variables == null) return new double[0];
            if (variables.Length == 0) return new double[0];

            int maxFunc = this._MaxFunEvaluations;

            return this.GetMinimum(function, variables, initialStep, this._Tolerance, ref maxFunc);
        }

        #endregion

        #region OptBoundVariable

        /// <summary>
        /// Computes the minimum point of a function of several variables.
        /// </summary>
        /// <param name="function">The function to minimize.</param>
        /// <param name="variables">Array of size N containing the varaibles.</param>
        /// <returns>Array containing the solution.</returns>
        public double[] ComputeMin(OptMultivariateFunction function, OptBoundVariable[] variables)
        {

            if (variables == null) return new double[0];
            if (variables.Length == 0) return new double[0];

            OptSimplexBoundVariable[] simplexVariables = this.GetVariables(variables);

            double initialStep = this.GetAutomaticInitialStep(simplexVariables);
            int maxFunc = this._MaxFunEvaluations;

            return this.GetMinimum(function, simplexVariables, initialStep, this._Tolerance, ref maxFunc);
        }


        /// <summary>
        /// Computes the minimum point of a function of several variables.
        /// </summary>
        /// <param name="function">The function to minimize.</param>
        /// <param name="variables">Array of size N containing the varaibles.</param>
        /// <param name="initialStep">The initial change of the variables.</param>
        /// <returns>Array containing the solution.</returns>
        public double[] ComputeMin(OptMultivariateFunction function, OptBoundVariable[] variables, float initialStep)
        {

            if (variables == null) return new double[0];
            if (variables.Length == 0) return new double[0];

            OptSimplexBoundVariable[] simplexVariables = this.GetVariables(variables);
            int maxFunc = this._MaxFunEvaluations;

            return this.GetMinimum(function, simplexVariables, initialStep, this._Tolerance, ref maxFunc);
        }

        #endregion


        #endregion



        #region Private Methods


        private double[] GetMinimum(OptMultivariateFunction function, OptSimplexVariable[] variables, double initialStep, double ftol, ref int nMax)
        {

            if (this._AmoebaDirver == null) this._AmoebaDirver = new DownhillDirver();

            double[] minimum = this._AmoebaDirver.ComputeMin(function, variables, initialStep, ftol, ref nMax);
            this.FunEvaluations = nMax;
            return minimum; 

        }

        private double[] GetMinimum(OptMultivariateFunction function, OptSimplexBoundVariable[] variables, double initialStep, double tolerance, ref int MAXFUN)
        {

            if (this._CobylaDriver == null) this._CobylaDriver = new CobylaDriver();

            double[] minimum = this._CobylaDriver.ComputeMin(function, variables, initialStep, tolerance, ref MAXFUN);

            this.FunEvaluations = MAXFUN;
            return minimum; 
        }



        private OptSimplexVariable[] GetVariables(double[] variablesArray)
        {
            OptSimplexVariable[] vars = new OptSimplexVariable[variablesArray.Length];

            for (int i = 0; i < variablesArray.Length; i++)
            {
                vars[i] = new OptSimplexVariable(variablesArray[i]);
            }

            return vars;
        }


        private OptSimplexVariable[] GetVariables(OptVariable[] variablesArray)
        {
            OptSimplexVariable[] vars = new OptSimplexVariable[variablesArray.Length];

            for (int i = 0; i < variablesArray.Length; i++)
            {

                OptVariable var = variablesArray[i];

                vars[i] = new OptSimplexVariable(var.Name, var.InitialGuess, var.Fixed);
            }

            return vars;
        }

        private OptSimplexBoundVariable[] GetVariables(OptBoundVariable[] variablesArray)
        {
            OptSimplexBoundVariable[] vars = new OptSimplexBoundVariable[variablesArray.Length];

            for (int i = 0; i < variablesArray.Length; i++)
            {

                OptBoundVariable var = variablesArray[i];

                vars[i] = new OptSimplexBoundVariable(var.Name, var.InitialGuess, var.Fixed, var.LowerBound, var.UpperBound);
            }

            return vars;
        }

        private double GetAutomaticInitialStep(OptSimplexBoundVariable[] variables)
        {

            int numFreeVariables = this.GetNumFreeVariable(variables);
            if (numFreeVariables == 0) return 1;

            double step = 0;

            double sum = 0;
            foreach (OptSimplexBoundVariable var in variables)
            {
                if (var.Fixed == false)
                {
                    //mod
                    sum += Math.Abs(var.InitialGuess / var.ScaleFactor);
                }
            }

            if (sum != 0)
            {
                step = sum / (2.0 * numFreeVariables);
            }
            else
            {
                step = 1;
            }

            return step;
        }


        private double GetAutomaticInitialStep(OptSimplexVariable[] variables)
        {

            int numFreeVariables = this.GetNumFreeVariable(variables);
            if (numFreeVariables == 0) return 1;

            double step = 0;

            double sum = 0;
            foreach (OptSimplexVariable var in variables)
            {
                if (var.Fixed == false)
                {
                    //mod
                    sum += Math.Abs(var.InitialGuess / var.ScaleFactor);
                }
            }

            if (sum != 0)
            {
                step = sum / (2.0 * numFreeVariables);
            }
            else
            {
                step = 1;
            }

            return step;
        }


        private int GetNumFreeVariable(OptSimplexBoundVariable[] variables)
        {
            int numFreeVariables = 0;
            foreach (OptSimplexBoundVariable var in variables)
            {
                if (var.Fixed == false) numFreeVariables++;
            }

            return numFreeVariables;
        }

        private int GetNumFreeVariable(OptSimplexVariable[] variables)
        {
            int numFreeVariables = 0;
            foreach (OptSimplexVariable var in variables)
            {
                if (var.Fixed == false) numFreeVariables++;
            }

            return numFreeVariables;
        }


        #endregion


    }
}
