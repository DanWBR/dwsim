using System;
using System.Collections.Generic;
using System.Text;

namespace DotNumerics.Optimization.Cobyla
{
    internal class CobylaDriver
    {

        #region Fields

        COBYLA MeCOBYLA;

        private OptMultivariateFunction Function;
        private OptSimplexBoundVariable[] MeSimplexBoundVariableList;
        protected double[] MeVariables;
        protected double[] MeFreeVariables;

        protected int MeNumFreeVariables = 0;
        private int MeNumBoundVariables = 0;
        private double MeInitialStep = 1;

        private int MeFunEvaluations = 0; 

        double[] W;
        int[] IACT;

        #endregion


        #region Constructor

        public CobylaDriver()
        {

            CALCFC fun = new CALCFC(InternalFunction);

            this.MeCOBYLA = new COBYLA(fun);

        }


        #endregion


        #region Methods

        public double[] ComputeMin(OptMultivariateFunction function, OptSimplexBoundVariable[] variables, double initialStep, double tolerance, ref int MAXFUN)
        {

            this.Function = function;
            this.MeFunEvaluations = 0;


            this.MeSimplexBoundVariableList = OptSimplexBoundVariable.GetClon(variables);
            this.MeInitialStep = initialStep;

            this.InitializeVariables();
            this.InitializeWorkSpace();

            if (this.MeNumFreeVariables == 0) return this.MeVariables;

            this.MeCOBYLA.Run(this.MeNumFreeVariables, this.MeNumBoundVariables, ref this.MeFreeVariables, 0, initialStep, tolerance, 0, ref MAXFUN, ref W, 0, ref IACT, 0);


            int varFreeVarIndex = 0;
            for (int i = 0; i < this.MeSimplexBoundVariableList.Length; i++)
            {
                if (this.MeSimplexBoundVariableList[i].Fixed == false)
                {
                    //mod
                    this.MeVariables[i] = this.MeFreeVariables[varFreeVarIndex] * this.MeSimplexBoundVariableList[i].ScaleFactor;
                    varFreeVarIndex++;
                }
            }

            MAXFUN = this.MeFunEvaluations;

            return this.MeVariables;

        }


        private void InitializeVariables()
        {
            this.MeNumFreeVariables = 0;
            this.MeNumBoundVariables = 0;

            foreach (OptSimplexBoundVariable var in this.MeSimplexBoundVariableList)
            {
                if (var.Fixed == false)
                {
                    this.MeNumFreeVariables++;

                    //Se invierten los limites de ser necesario
                    if (var.LowerBound > var.UpperBound)
                    {
                        double tempBound = var.LowerBound;
                        var.LowerBound = var.UpperBound;
                        var.UpperBound = tempBound;
                    }

                    //Se revisa que la variable inicial este dentro de los limites
                    if (var.InitialGuess < var.LowerBound)
                    {
                        var.InitialGuess = var.LowerBound;
                    }
                    if (var.InitialGuess > var.UpperBound)
                    {
                        var.InitialGuess = var.UpperBound;
                    }

                    if (var.InitialGuess == 0) var.InitialGuess = Math.Min(1, var.UpperBound); // Si es  0 no ocurre ningun cambio asi que se modifica el valor

                    if (var.LowerBound == double.NegativeInfinity && var.UpperBound == double.PositiveInfinity) var.UseBounds = false;
                    if (var.UseBounds == true) this.MeNumBoundVariables++;
                }
            }


            this.MeVariables = new double[this.MeSimplexBoundVariableList.Length];
            int index = 0;
            foreach (OptSimplexBoundVariable var in this.MeSimplexBoundVariableList)
            {
                this.MeVariables[index] = var.InitialGuess;
                index++;
            }

            this.MeFreeVariables = new double[this.MeNumFreeVariables];

            index = 0;
            foreach (OptSimplexBoundVariable var in this.MeSimplexBoundVariableList)
            {
                if (var.Fixed == false)
                {
                    //mod
                    this.MeFreeVariables[index] = var.InitialGuess / var.ScaleFactor;
                    index++;
                }
            }
        }



        private void InitializeWorkSpace()
        {

            //The arguments W and IACT provide real and
            // C     integer arrays that are used as working space. Their lengths must be at
            // C     least N*(3*N+2*M+11)+4*M+6 and M+1 respectively.
            int N = this.MeNumFreeVariables;
            int M = this.MeNumBoundVariables;
            int wDim = N * (3 * N + 2 * M + 11) + 4 * M + 6;
            this.W = new double[wDim];
            this.IACT = new int[M + 1];

        }


        #endregion


        private void InternalFunction(int N, int M, double[] X, int o_x, ref double F, ref double[] CON, int o_con)
        {
            this.MeFunEvaluations++;

            int varFreeVarIndex = 0;
            for (int i = 0; i < this.MeSimplexBoundVariableList.Length; i++)
            {
                if (this.MeSimplexBoundVariableList[i].Fixed == false)
                {
                    //mod
                    this.MeVariables[i] = X[varFreeVarIndex + o_x] * this.MeSimplexBoundVariableList[i].ScaleFactor;
                    varFreeVarIndex++;
                }
            }

            F = this.Function(this.MeVariables);

            varFreeVarIndex = 0;
            int index = 0;
            double valLower = 0;
            double valUpper = 0;
            double fact = Math.Max(1, Math.Abs(F));
            for (int i = 0; i < this.MeSimplexBoundVariableList.Length; i++)
            {
                if (this.MeSimplexBoundVariableList[i].Fixed == false)
                {
                    if (this.MeSimplexBoundVariableList[i].UseBounds)
                    {
                        if (fact == 0) fact = 1;
                        //mod
                        valLower = fact * (X[varFreeVarIndex + o_x] - this.MeSimplexBoundVariableList[i].LowerBound / this.MeSimplexBoundVariableList[i].ScaleFactor);
                        //mod
                        valUpper = fact * (this.MeSimplexBoundVariableList[i].UpperBound / this.MeSimplexBoundVariableList[i].ScaleFactor - X[varFreeVarIndex + o_x]);
                        CON[index + o_con] = Math.Min(valLower, valUpper);
                        index++;
                    }
                    varFreeVarIndex++;
                }
            }

        }



    }
}
