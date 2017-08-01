using System;
using System.Collections.Generic;
using System.Text;

namespace DotNumerics.Optimization.NelderMead
{
    internal class DownhillDirver
    {

        #region Fields

        Downhill _downhill = new Downhill();

        private OptMultivariateFunction _Function;

        private OptSimplexVariable[] _SimplexVariableList;
        private double[] _ExternalVariables;

        private int _FunEvaluations = 0;


        #endregion


        #region Methods


        public double[] ComputeMin(OptMultivariateFunction function, OptSimplexVariable[] variables, double initialStep, double ftol, ref int nMax)
        {

            double[,] p;
            double[] y;
            bool stop = false;

            this._Function = function;
            this._SimplexVariableList = variables;
            this._FunEvaluations = 0;


            this._ExternalVariables = new double[variables.Length];


            int numFreeVariable = 0;
            for (int i = 0; i < this._SimplexVariableList.Length; i++)
            {
                this._ExternalVariables[i] = this._SimplexVariableList[i].InitialGuess;
                if (this._SimplexVariableList[i].Fixed == false) numFreeVariable++;
            }

            if (numFreeVariable == 0) return this._ExternalVariables;

            double[] minimum = new double[numFreeVariable];

            this.Initialize(function, initialStep, out p, out y);


            OptMultivariateFunction internalFunc = new OptMultivariateFunction(InternalFunction);


            this._downhill.Run(minimum, p, y, ftol, ref stop, internalFunc, ref nMax);

            //Se regresan las variables en escala original
            int varFreeVarIndex = 0;
            for (int i = 0; i < this._SimplexVariableList.Length; i++)
            {
                if (this._SimplexVariableList[i].Fixed == false)
                {
                    //mod
                    this._ExternalVariables[i] = minimum[varFreeVarIndex] * this._SimplexVariableList[i].ScaleFactor;
                    varFreeVarIndex++;
                }
            }

            nMax = this._FunEvaluations;

            return this._ExternalVariables;


        }

        private void Initialize(OptMultivariateFunction funk, double initialStep, out double[,] p, out double[] y)
        {
            #region Valores Iniciales


            int ndim = this._SimplexVariableList.Length;
            int mpts = ndim + 1;
            p = new double[mpts, ndim];
            y = new double[mpts];
            double[] Variables = new double[ndim];


            //Se pone el primer punto iagual al valor inicial
            for (int i = 0; i < ndim; i++)
            {
                Variables[i] = this._SimplexVariableList[i].InitialGuess;  //El primer punto es el vector original 
                //mod
                p[0, i] = Variables[i] / this._SimplexVariableList[i].ScaleFactor;
            }
            this._FunEvaluations++;
            y[0] = funk(Variables);

            for (int i = 0; i < ndim; i++)
            {
                //mod
                Variables[i] = this._SimplexVariableList[i].InitialGuess + initialStep * this._SimplexVariableList[i].ScaleFactor;
                //mod
                p[i + 1, i] = Variables[i] / this._SimplexVariableList[i].ScaleFactor;
                this._FunEvaluations++;
                y[i + 1] = funk(Variables);
            }

            #endregion
        }



        private double InternalFunction(double[] X)
        {
            double F = 0;

            this._FunEvaluations++;

            int varFreeVarIndex = 0;
            for (int i = 0; i < this._SimplexVariableList.Length; i++)
            {
                if (this._SimplexVariableList[i].Fixed == false)
                {
                    //mod
                    this._ExternalVariables[i] = X[varFreeVarIndex] * this._SimplexVariableList[i].ScaleFactor;
                    varFreeVarIndex++;
                }
            }

            F = this._Function(this._ExternalVariables);

            return F;
        }




        #endregion

    }
}
