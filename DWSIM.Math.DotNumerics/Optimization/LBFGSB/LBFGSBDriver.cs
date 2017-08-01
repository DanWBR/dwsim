using System;
using System.Collections.Generic;
using System.Text;

namespace DotNumerics.Optimization.LBFGSB
{
    public class LBFGSBDriver
    {

        #region Fields

        SETULB _SETULB;

        //OptMultivariateFunction MeFunction;
        //OptMultivariateGradient MeGradient;

        protected int _NumFreeVariables = 0;
        protected double[] _ExternalVariables;
        protected double[] _FreeVariables;
        protected double[] _LowerBounds;
        protected double[] _UpperBounds;
        protected int[] _NBD;
        protected double[] _ExternalGradientArray;
        protected double[] _GradientArray;
        protected int M = 5;
        //protected double MeAccuracyFactor = 1E7;

        protected double[] WA;
        protected int[] IWA;
        protected bool[] LSAVE = new bool[4];
        protected int[] ISAVE = new int[44];
        protected double[] DSAVE = new double[29];

        protected OptVariable[] _OptVariable = null;
        protected OptBoundVariable[] _OptBoundVariable = null;

        #endregion


        #region Constructor

        public LBFGSBDriver()
        {

            this._SETULB = new SETULB();

        }

        #endregion


        #region Methods


        public double[] ComputeMin(OptMultivariateFunction function, OptMultivariateGradient gradient, OptVariable[] variables, double tolerance, double factr, ref int nMax)
        {

            double[] minimum;

            this.Initialize(variables);

            if (this._NumFreeVariables == 0) return this._ExternalVariables;

            minimum = this.Compute(function, gradient, tolerance, factr, ref nMax);

            return minimum;

        }


        public double[] ComputeMin(OptMultivariateFunction function, OptMultivariateGradient gradient, OptBoundVariable[] variables, double tolerance, double factr, ref int nMax)
        {

            double[] minimum;

            this.Initialize(variables);

            if (this._NumFreeVariables == 0) return this._ExternalVariables;

            minimum = this.Compute(function, gradient, tolerance, factr, ref nMax);

            return minimum;

        }



        private double[] Compute(OptMultivariateFunction function, OptMultivariateGradient gradient, double tolerance, double factr, ref int nMax)
        {
            double f = 0;

            BFGSTask TASK = BFGSTask.START;
            BFGSTask Csave = BFGSTask.START;
            int iprint = 0;

            bool Continue = true;

            int funcEvaluations = 0;

            while (Continue)
            {

                this._SETULB.Run(this._NumFreeVariables, this.M, ref this._FreeVariables, 0, this._LowerBounds, 0, this._UpperBounds, 0,
                    this._NBD, 0, ref f, ref  this._GradientArray, 0, factr, tolerance, ref WA, 0, ref IWA, 0, ref TASK, iprint, ref Csave,
                    ref LSAVE, 0, ref this.ISAVE, 0, ref DSAVE, 0);

                if (funcEvaluations <= nMax)
                {
                    if (TASK == BFGSTask.FG || TASK == BFGSTask.FG_LNSRCH || TASK == BFGSTask.FG_ST || TASK == BFGSTask.FG_START)
                    {
                        // c        the minimization routine has returned to request the
                        // c        function f and gradient g values at the current x.

                        // c        Compute function value f for the sample problem.

                        this.UpdateExternalVariables();

                        funcEvaluations++;
                        f = function(this._ExternalVariables);

                        this._ExternalGradientArray = gradient(this._ExternalVariables);

                        this.UpdateInternalGradient();

                        // c          go back to the minimization routine.
                        Continue = true;
                    }
                    else if (TASK == BFGSTask.NEW_X)
                    {
                        Continue = true;
                    }
                    else
                    {
                        Continue = false;
                    }
                }
                else
                {
                    Continue = false;
                }

            }

            this.UpdateExternalVariables();

            nMax = funcEvaluations;

            return this._ExternalVariables;

        }

        #region Update External Variables

        private void UpdateExternalVariables()
        {
            if (this._OptVariable != null)
            {
                this.UpdateExternalVariables(this._OptVariable);
            }
            else if (this._OptBoundVariable != null)
            {
                this.UpdateExternalVariables(this._OptBoundVariable);
            }
        }


        private void UpdateExternalVariables(OptVariable[] variables)
        {
            int index = 0;
            for (int i = 0; i < variables.Length; i++)
            {
                if (variables[i].Fixed == false)
                {
                    this._ExternalVariables[i] = this._FreeVariables[index];
                    index++;
                }
            }
        }

        private void UpdateExternalVariables(OptBoundVariable[] variables)
        {
            int index = 0;
            for (int i = 0; i < variables.Length; i++)
            {
                if (variables[i].Fixed == false)
                {
                    this._ExternalVariables[i] = this._FreeVariables[index];
                    index++;
                }
            }
        }


        #endregion


        #region Update Internal Gradient

        private void UpdateInternalGradient()
        {
            if (this._OptVariable != null)
            {
                this.UpdateInternalGradient(this._OptVariable);
            }
            else if (this._OptBoundVariable != null)
            {
                this.UpdateInternalGradient(this._OptBoundVariable);
            }
        }


        private void UpdateInternalGradient(OptVariable[] variables)
        {
            int index = 0;
            for (int i = 0; i < variables.Length; i++)
            {
                if (variables[i].Fixed == false)
                {
                    this._GradientArray[index] = this._ExternalGradientArray[i];
                    index++;
                }
            }
        }

        private void UpdateInternalGradient(OptBoundVariable[] variables)
        {
            int index = 0;
            for (int i = 0; i < variables.Length; i++)
            {
                if (variables[i].Fixed == false)
                {
                    this._GradientArray[index] = this._ExternalGradientArray[i];
                    index++;
                }
            }
        }


        #endregion


        private void Initialize(OptVariable[] variables)
        {

            this._OptVariable = variables;
            this._OptBoundVariable = null;

            this._ExternalVariables = new double[variables.Length];
            this._ExternalGradientArray = new double[variables.Length];

            int numFreeVariable = 0;
            for (int i = 0; i < variables.Length; i++)
            {
                this._ExternalVariables[i] = variables[i].InitialGuess;
                if (variables[i].Fixed == false) numFreeVariable++;
            }

            this._NumFreeVariables = numFreeVariable;

            this._FreeVariables = new double[numFreeVariable];

            int index = 0;
            for (int i = 0; i < variables.Length; i++)
            {
                if (variables[i].Fixed == false)
                {
                    this._FreeVariables[index] = variables[i].InitialGuess;
                    index++;
                }
            }

            this._LowerBounds = new double[numFreeVariable];
            this._UpperBounds = new double[numFreeVariable];
            this._NBD = new int[numFreeVariable]; //nbd(i)=0 if x(i) is unbounded,
            this._GradientArray = new double[numFreeVariable];

            //c        nmax is the dimension of the largest problem to be solved.
            //c        mmax is the maximum number of limited memory corrections.
            int NMAX = this._NumFreeVariables;
            int MMAX = this.M;

            // c     wa is a double precision working array of length 
            // c       (2mmax + 4)nmax + 12mmax^2 + 12mmax
            int WADimension = (2 * MMAX + 4) * NMAX + 12 * (MMAX * MMAX) + 12 * MMAX;
            this.WA = new double[WADimension];
            this.IWA = new int[3 * NMAX];
        }


        private void Initialize(OptBoundVariable[] variables)
        {

            this._OptVariable = null;
            this._OptBoundVariable = OptBoundVariable.GetClon(variables);

            this._ExternalVariables = new double[this._OptBoundVariable.Length];
            this._ExternalGradientArray = new double[this._OptBoundVariable.Length];

            this.CheckAndSetBounds(this._OptBoundVariable);

            int numFreeVariable = 0;
            for (int i = 0; i < this._OptBoundVariable.Length; i++)
            {
                this._ExternalVariables[i] = this._OptBoundVariable[i].InitialGuess;
                if (this._OptBoundVariable[i].Fixed == false) numFreeVariable++;
            }

            this._NumFreeVariables = numFreeVariable;
            this._FreeVariables = new double[numFreeVariable];

            int index = 0;
            for (int i = 0; i < this._OptBoundVariable.Length; i++)
            {
                if (this._OptBoundVariable[i].Fixed == false)
                {
                    this._FreeVariables[index] = this._OptBoundVariable[i].InitialGuess;
                    index++;
                }
            }


            this._GradientArray = new double[numFreeVariable];

            //c        nmax is the dimension of the largest problem to be solved.
            //c        mmax is the maximum number of limited memory corrections.
            int NMAX = this._NumFreeVariables;
            int MMAX = this.M;

            // c     wa is a double precision working array of length 
            // c       (2mmax + 4)nmax + 12mmax^2 + 12mmax
            int WADimension = (2 * MMAX + 4) * NMAX + 12 * (MMAX * MMAX) + 12 * MMAX;
            this.WA = new double[WADimension];
            this.IWA = new int[3 * NMAX];
        }



        private void CheckAndSetBounds(OptBoundVariable[] variables)
        {
            int numFreeVariable = 0;
            for (int i = 0; i < variables.Length; i++)
            {
                if (variables[i].Fixed == false) numFreeVariable++;
            }

            this._LowerBounds = new double[numFreeVariable];
            this._UpperBounds = new double[numFreeVariable];
            // c         nbd(i)=0 if x(i) is unbounded,
            // c                1 if x(i) has only a lower bound,
            // c                2 if x(i) has both lower and upper bounds, and
            // c                3 if x(i) has only an upper bound.
            this._NBD = new int[numFreeVariable];

            int index = 0;
            for (int i = 0; i < variables.Length; i++)
            {
                if (variables[i].Fixed == false)
                {
                    //Se invierten los limites de ser necesario
                    if (variables[i].LowerBound > variables[i].UpperBound)
                    {
                        double tempBound = variables[i].LowerBound;
                        variables[i].LowerBound = variables[i].UpperBound;
                        variables[i].UpperBound = tempBound;
                    }

                    //Se revisa que la variable inicial este dentro de los limites
                    if (variables[i].InitialGuess < variables[i].LowerBound)
                    {
                        variables[i].InitialGuess = variables[i].LowerBound;
                    }
                    if (variables[i].InitialGuess > variables[i].UpperBound)
                    {
                        variables[i].InitialGuess = variables[i].UpperBound;
                    }


                    this._LowerBounds[index] = variables[i].LowerBound;
                    this._UpperBounds[index] = variables[i].UpperBound;

                    if (this._LowerBounds[index] == double.NegativeInfinity && this._UpperBounds[index] == double.PositiveInfinity)
                    {
                        this._NBD[index] = 0; //nbd(i)=0 if x(i) is unbounded,
                    }
                    else if (this._LowerBounds[index] == double.NegativeInfinity)
                    {
                        this._NBD[index] = 3; //3 if x(i) has only an upper bound.
                    }
                    else if (this._UpperBounds[index] == double.PositiveInfinity)
                    {
                        this._NBD[index] = 3; //1 if x(i) has only a lower bound,
                    }
                    else
                    {
                        this._NBD[index] = 2; //2 if x(i) has both lower and upper bounds
                    }
                    index++;
                }
            }

        }


        #endregion





    }
}
