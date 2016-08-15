using System;
using System.Collections.Generic;
using System.Text;


namespace DotNumerics.Optimization.TN
{
    internal class TNDriver
    {

        #region Fields

        LMQNBC MeLMQNBC;
        LMQN MeLMQN;

        protected int MeNumFreeVariables = 0;
        protected double[] MeExternalVariables;
        protected double[] MeFreeVariables;
        protected double[] MeLowerBounds;
        protected double[] MeUpperBounds;
        protected double[] MeGradientArray;

        SFUN internalFunction;
        protected OptVariable[] MeOptVariable = null;
        protected OptBoundVariable[] MeOptBoundVariable = null;

        /// <summary>
        /// F  - ROUGH ESTIMATE OF FUNCTION VALUE AT SOLUTION
        /// </summary>
        private double MeF = 1;

        /// <summary>
        /// W      - (REAL*8) WORK VECTOR OF LENGTH AT LEAST 14*N
        /// </summary>
        private double[] MeW;

        /// <summary>
        /// (INTEGER) THE DECLARED DIMENSION OF W
        /// W      - (REAL*8) WORK VECTOR OF LENGTH AT LEAST 14*N
        /// </summary>
        private int MeLW;

        /// <summary>
        /// MAXIMUM NUMBER OF INNER ITERATIONS PER STEP
        /// </summary>
        private int MeMAXIT;

        /// <summary>
        /// DETERMINES QUANTITY OF PRINTED OUTPUT, 0 = NONE, 1 = ONE LINE PER MAJOR ITERATION.
        /// </summary>
        private int MeMSGLVL = 0;

        /// <summary>
        /// SEVERITY OF THE LINESEARCH
        /// </summary>
        private double MeETA = 0.25;

        /// <summary>
        /// MAXIMUM ALLOWABLE STEP IN THE LINESEARCH
        /// </summary>
        private double MeSTEPMX = 10;


        /// <summary>
        /// WORK VECTOR OF LENGTH AT LEAST N, USED
        /// TO RECORD WHICH VARIABLES ARE AT THEIR BOUNDS.
        /// </summary>
        private int[] MeIPIVOT;


        ///// <summary>
        ///// The number of function evaluations used to compute the minimum.
        ///// </summary>
        //private int MeFunEvaluations = 0;

        #endregion

        #region Constructor

        public TNDriver()
        {

        }

        #endregion


        #region

        /// <summary>
        /// SEVERITY OF THE LINESEARCH
        /// </summary>
        public double ETA
        {
            get { return MeETA; }
            set { MeETA = value; }
        }


        /// <summary>
        /// MAXIMUM ALLOWABLE STEP IN THE LINESEARCH
        /// </summary>
        public double STEPMX
        {
            get { return MeSTEPMX; }
            set { MeSTEPMX = value; }
        }

        ///// <summary>
        ///// The number of function evaluations used to compute the minimum.
        ///// </summary>
        //protected int FunEvaluations
        //{
        //    get { return MeFunEvaluations; }
        //    set { MeFunEvaluations = value; }
        //}

        #endregion


        #region Public Methods


        public double[] ComputeMin(OptMultivariateFunction function, OptMultivariateGradient gradient, OptVariable[] variables, double tolerance, double ACCRCY, ref int nMax)
        {

            if (this.MeLMQN == null) this.MeLMQN = new LMQN();

            this.Initialize(function, gradient, variables);

            if (this.MeNumFreeVariables == 0) return this.MeExternalVariables;

            int IERROR = 0;

            this.MeLMQN.Run(ref IERROR, this.MeNumFreeVariables, ref this.MeFreeVariables, 0, ref this.MeF,
                ref this.MeGradientArray, 0, ref this.MeW, 0, this.MeLW, this.internalFunction, this.MeMSGLVL, this.MeMAXIT, nMax,
                this.MeETA, this.MeSTEPMX, ACCRCY, tolerance);


            int index = 0;
            for (int i = 0; i < variables.Length; i++)
            {
                if (variables[i].Fixed == false)
                {
                    this.MeExternalVariables[i] = this.MeFreeVariables[index];
                    index++;
                }
            }

            nMax = this.internalFunction.FunEvaluations;

            return this.MeExternalVariables;
        }


        public double[] ComputeMin(OptMultivariateFunction function, OptMultivariateGradient gradient, OptBoundVariable[] variables, double tolerance, double ACCRCY, ref int nMax)
        {

            if (this.MeLMQNBC == null) this.MeLMQNBC = new LMQNBC();

            this.Initialize(function, gradient, variables);

            if (this.MeNumFreeVariables == 0) return this.MeExternalVariables;

            int IERROR = 0;
            int[] IPIVOT = new int[this.MeNumFreeVariables];

            this.MeLMQNBC.Run(ref IERROR, this.MeNumFreeVariables, ref this.MeFreeVariables, 0, ref this.MeF,
                ref this.MeGradientArray, 0, ref this.MeW, 0, this.MeLW, this.internalFunction, this.MeLowerBounds, 0, this.MeUpperBounds, 0,
               ref this.MeIPIVOT, 0, this.MeMSGLVL, this.MeMAXIT, nMax,this.MeETA, this.MeSTEPMX, ACCRCY, tolerance);


            int index = 0;
            for (int i = 0; i < variables.Length; i++)
            {
                if (variables[i].Fixed == false)
                {
                    this.MeExternalVariables[i] = this.MeFreeVariables[index];
                    index++;
                }
            }

            nMax = this.internalFunction.FunEvaluations;

            return this.MeExternalVariables;

        }


        #endregion


        #region Private Methods


        private void Initialize(OptMultivariateFunction function, OptMultivariateGradient gradient, OptVariable[] variables)
        {

            this.internalFunction = new SFUN(function, gradient, variables);

            this.MeOptVariable = variables;
            this.MeOptBoundVariable = null;

            this.MeExternalVariables = new double[variables.Length];

            int numFreeVariable = 0;
            for (int i = 0; i < variables.Length; i++)
            {
                this.MeExternalVariables[i] = variables[i].InitialGuess;
                if (variables[i].Fixed == false) numFreeVariable++;
            }

            this.MeF = function(this.MeExternalVariables);

            this.MeNumFreeVariables = numFreeVariable;

            this.MeFreeVariables = new double[numFreeVariable];
            this.MeGradientArray = new double[numFreeVariable];

            int index = 0;
            for (int i = 0; i < variables.Length; i++)
            {
                if (variables[i].Fixed == false)
                {
                    this.MeFreeVariables[index] = variables[i].InitialGuess;
                    index++;
                }
            }


            //W      - (REAL*8)(REAL*8) WORK VECTOR OF LENGTH AT LEAST 14*N
            //LW     - (INTEGER) THE DECLARED DIMENSION OF W
            this.MeLW = 14 * this.MeNumFreeVariables;
            this.MeW = new double[this.MeLW];

            this.MeMAXIT = this.MeNumFreeVariables / 2;

        }


        private void Initialize(OptMultivariateFunction function, OptMultivariateGradient gradient, OptBoundVariable[] variables)
        {

            this.internalFunction = new SFUN(function, gradient, variables);

            this.MeOptVariable = null;
            this.MeOptBoundVariable = OptBoundVariable.GetClon(variables);

            this.CheckAndSetBounds(this.MeOptBoundVariable);

            this.MeExternalVariables = new double[this.MeOptBoundVariable.Length];
            int numFreeVariable = 0;
            for (int i = 0; i < this.MeOptBoundVariable.Length; i++)
            {
                this.MeExternalVariables[i] = variables[i].InitialGuess;
                if (this.MeOptBoundVariable[i].Fixed == false) numFreeVariable++;
            }

            this.MeF = function(this.MeExternalVariables);

            this.MeNumFreeVariables = numFreeVariable;
            this.MeFreeVariables = new double[numFreeVariable];
            this.MeGradientArray = new double[numFreeVariable];

            int index = 0;
            for (int i = 0; i < this.MeOptBoundVariable.Length; i++)
            {
                if (this.MeOptBoundVariable[i].Fixed == false)
                {
                    this.MeFreeVariables[index] = this.MeOptBoundVariable[i].InitialGuess;
                    index++;
                }
            }


            //W      - (REAL*8)(REAL*8) WORK VECTOR OF LENGTH AT LEAST 14*N
            //LW     - (INTEGER) THE DECLARED DIMENSION OF W
            this.MeLW = 14 * this.MeNumFreeVariables;
            this.MeW = new double[this.MeLW];

            this.MeMAXIT = Math.Max(1, this.MeNumFreeVariables / 2);
        }


        private void CheckAndSetBounds(OptBoundVariable[] variables)
        {
            int numFreeVariable = 0;
            for (int i = 0; i < variables.Length; i++)
            {
                if (variables[i].Fixed == false) numFreeVariable++;
            }

            this.MeLowerBounds = new double[numFreeVariable];
            this.MeUpperBounds = new double[numFreeVariable];
            this.MeIPIVOT = new int[numFreeVariable];


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

                    this.MeLowerBounds[index] = variables[i].LowerBound;
                    this.MeUpperBounds[index] = variables[i].UpperBound;

                    // C LOW, UP - (REAL*8) VECTORS OF LENGTH AT LEAST N CONTAINING
                    // C           THE LOWER AND UPPER BOUNDS ON THE VARIABLES.  IF
                    // C           THERE ARE NO BOUNDS ON A PARTICULAR VARIABLE, SET
                    // C           THE BOUNDS TO -1.D38 AND 1.D38, RESPECTIVELY.
                    if (this.MeLowerBounds[index] == double.NegativeInfinity && this.MeUpperBounds[index] == double.PositiveInfinity)
                    {
                        this.MeLowerBounds[index] = 1E-38;
                        this.MeUpperBounds[index] = 1E38;

                    }
                    else if (this.MeLowerBounds[index] == double.NegativeInfinity)
                    {
                        this.MeLowerBounds[index] = 1E-38;
                    }
                    else if (this.MeUpperBounds[index] == double.PositiveInfinity)
                    {
                        this.MeUpperBounds[index] = 1E38;
                    }
                    index++;
                }

            }

        }

        #endregion

    }



    public interface ISFUN
    {
        void Run(int N, double[] X, int offset_x, ref double F, ref double[] G, int offset_g);
    }


    public class SFUN : ISFUN
    {

        #region Fields

        private OptMultivariateFunction MeFunction;
        private OptMultivariateGradient MeGradient;

        private double[] MeExternalVariables;
        protected double[] MeExternalGradientArray;
        //private int MeNParameters;

        protected OptVariable[] MeOptVariable = null;
        protected OptBoundVariable[] MeOptBoundVariable = null;

        /// <summary>
        /// The number of function evaluations used to compute the minimum.
        /// </summary>
        private int MeFunEvaluations = 1; //en la inicializacion ya se se realizo una llamada, asi que inicia en 1

        #endregion


        /// <summary>
        /// The number of function evaluations used to compute the minimum.
        /// </summary>
        public int FunEvaluations
        {
            get { return MeFunEvaluations; }
            set { MeFunEvaluations = value; }
        }


        public SFUN(OptMultivariateFunction function, OptMultivariateGradient gradient, OptVariable[] variables)
        {
            //this.MeNParameters = nParameters;
            this.MeFunction = function;
            this.MeGradient = gradient;

            this.MeOptVariable = variables;
            this.MeOptBoundVariable = null;

            this.MeExternalVariables = new double[variables.Length];
            this.MeExternalGradientArray = new double[variables.Length];

            this.MeExternalVariables = new double[variables.Length];

            for (int i = 0; i < variables.Length; i++)
            {
                this.MeExternalVariables[i] = variables[i].InitialGuess;
            }

        }


        internal SFUN(OptMultivariateFunction function, OptMultivariateGradient gradient, OptBoundVariable[] variables)
        {
            //this.MeNParameters = nParameters;
            this.MeFunction = function;
            this.MeGradient = gradient;

            this.MeOptVariable = null;
            this.MeOptBoundVariable = variables;

            this.MeExternalVariables = new double[variables.Length];
            this.MeExternalGradientArray = new double[variables.Length];

            for (int i = 0; i < variables.Length; i++)
            {
                this.MeExternalVariables[i] = variables[i].InitialGuess;
            }

        }

        public void Run(int N, double[] X, int o_x, ref double F, ref double[] G, int o_g)
        {

            #region                                         Array Index Correction
            //--------------------------------------------------------------------------------------------------------------------
            //                                              Array Index Correction
            //--------------------------------------------------------------------------------------------------------------------

            //int c_x = -1 + o_x; int c_g = -1 + o_g;

            #endregion

            this.MeFunEvaluations++;

            this.UpdateExternalVariables(X, o_x);

            F = this.MeFunction(this.MeExternalVariables);

            this.MeExternalGradientArray = this.MeGradient(this.MeExternalVariables);

            this.UpdateInternalGradient(G, o_g);

            return;
        }



        #region Update External Variables

        private void UpdateExternalVariables(double[] X, int o_x)
        {
            if (this.MeOptVariable != null)
            {
                this.UpdateExternalVariables(this.MeOptVariable,X, o_x);
            }
            else if (this.MeOptBoundVariable != null)
            {
                this.UpdateExternalVariables(this.MeOptBoundVariable,X, o_x);
            }
        }


        private void UpdateExternalVariables(OptVariable[] variables, double[] X, int o_x)
        {
            int index = 0;
            for (int i = 0; i < variables.Length; i++)
            {
                if (variables[i].Fixed == false)
                {
                    this.MeExternalVariables[i] = X[index + o_x];
                    index++;
                }
            }
        }

        private void UpdateExternalVariables(OptBoundVariable[] variables, double[] X, int o_x)
        {
            int index = 0;
            for (int i = 0; i < variables.Length; i++)
            {
                if (variables[i].Fixed == false)
                {
                    this.MeExternalVariables[i] = X[index + o_x];
                    index++;
                }
            }
        }


        #endregion


        #region Update Internal Gradient

        private void UpdateInternalGradient(double[] G, int o_g)
        {
            if (this.MeOptVariable != null)
            {
                this.UpdateInternalGradient(this.MeOptVariable, G, o_g);
            }
            else if (this.MeOptBoundVariable != null)
            {
                this.UpdateInternalGradient(this.MeOptBoundVariable, G, o_g);
            }
        }


        private void UpdateInternalGradient(OptVariable[] variables, double[] G, int o_g)
        {
            int index = 0;
            for (int i = 0; i < variables.Length; i++)
            {
                if (variables[i].Fixed == false)
                {
                    G[index + o_g] = this.MeExternalGradientArray[i];
                    index++;
                }
            }
        }

        private void UpdateInternalGradient(OptBoundVariable[] variables, double[] G, int o_g)
        {
            int index = 0;
            for (int i = 0; i < variables.Length; i++)
            {
                if (variables[i].Fixed == false)
                {
                    G[index + o_g] = this.MeExternalGradientArray[i];
                    index++;
                }
            }
        }


        #endregion




    }


}
