#region Copyright © 2009, De Santiago-Castillo JA. All rights reserved.

//Copyright © 2009 Jose Antonio De Santiago-Castillo 
//E-mail:JAntonioDeSantiago@gmail.com
//Web: www.DotNumerics.com
//
#endregion


using System;
using System.Collections.Generic;
using System.Text;

using DotNumerics.ODE.Dopri5;
using DotNumerics.ODE.Radau5;

namespace DotNumerics.ODE
{
    /// <summary>
    /// NAME (EXTERNAL) OF SUBROUTINE PROVIDING THE
    /// NUMERICAL SOLUTION DURING INTEGRATION. 
    /// IF IOUT.GE.1, IT IS CALLED AFTER EVERY SUCCESSFUL STEP.
    /// SUPPLY A DUMMY SUBROUTINE IF IOUT=0. 
    /// IT MUST HAVE THE FORM
    /// SUBROUTINE SOLOUTR (NR,XOLD,X,Y,N,CON,ICOMP,ND,
    /// RPAR,IPAR,IRTRN)
    /// DIMENSION Y(N),CON(5*ND),ICOMP(ND)
    /// ....  
    /// SOLOUTR FURNISHES THE SOLUTION "Y" AT THE NR-TH
    /// GRID-POINT "X" (THEREBY THE INITIAL VALUE IS
    /// THE FIRST GRID-POINT).
    /// "XOLD" IS THE PRECEEDING GRID-POINT.
    /// "IRTRN" SERVES TO INTERRUPT THE INTEGRATION. IF IRTRN
    /// IS SET .LT.0, DOPRI5 WILL RETURN TO THE CALLING PROGRAM.
    /// IF THE NUMERICAL SOLUTION IS ALTERED IN SOLOUTR,
    /// SET  IRTRN = 2
    /// 
    /// -----  CONTINUOUS OUTPUT: -----
    /// DURING CALLS TO "SOLOUTR", A CONTINUOUS SOLUTION
    /// FOR THE INTERVAL [XOLD,X] IS AVAILABLE THROUGH
    /// THE FUNCTION
    /// .GT..GT..GT.   CONTD5(I,S,CON,ICOMP,ND)   .LT..LT..LT.
    /// WHICH PROVIDES AN APPROXIMATION TO THE I-TH
    /// COMPONENT OF THE SOLUTION AT THE POINT S. THE VALUE
    /// S SHOULD LIE IN THE INTERVAL [XOLD,X].
    /// </summary>
    internal class RKSolOut: ISOLOUT, ISOLOUTR
    {

        protected enum SolutionOutType { Array, Delegate }


        #region Fields

        CONTD5 _Contd5ExpicitRK;

        CONTR5 _Contr5ImplicitRK;

        /// <summary>
        /// Number of equations
        /// </summary>
        protected int _NEquations;

        /// <summary>
        /// Valores iniciales.
        /// </summary>
        protected double[] _Y0;

        /// <summary>
        /// Indica si se usa un array para definir los puntos de inegracion.
        /// </summary>
        protected bool _IsTimeArrayUsed = false;

        /// <summary>
        /// El tiempo inicial de integracion
        /// </summary>
        protected double _T0 = 0;
        /// <summary>
        /// El tiempo final de inegracion
        /// </summary>
        protected double _TF = 1;
        //El incremento en el tiempo de integracion
        protected double _DeltaT = 1;
        /// <summary>
        /// El array que contiene los tiempos de integracion
        /// </summary>
        protected double[] _TSpan;

        /// <summary>
        /// El valor indice maximo para el cual la soluciones calculada. 
        /// </summary>
        protected int _MaxIndex = 0;

        /// <summary>
        /// El array que contiene el tiempo y los valores de la solucion
        /// </summary>
        protected double[,] _SolutionArray;

        /// <summary>
        /// El delegado que se llama cuando se calcula un punto en la inegracion (si es requerido).
        /// </summary>
        protected OdeSolution _SolutionOut;

        ///// <summary>
        ///// A value that indicate when a delegate is used for the solution output.
        ///// </summary>
        //private bool MeUseDelegateSolutionOut = true;

        /// <summary>
        /// Especifica como se regresara la solucion
        /// </summary>
        protected SolutionOutType _SolutionOutType;

        /// <summary>
        /// Un areglo que array que almacena la solucion para cada punto
        /// </summary>
        protected double[] _TemporalSolution;

        /// <summary>
        /// Indica si la integracion va en incrementos positivos del tiempo.
        /// </summary>
        protected bool _isDeltaPositive = true;


        protected int _Index = 1;

        //protected int MeSolutionLength;



        /// <summary>
        /// El valor que se esta calculando.
        /// </summary>
        protected double _T = 0;
        ///// <summary>
        ///// El indice en el que se esta calculando.
        ///// </summary>
        ///// 



        #endregion

        #region Constructor

        public RKSolOut(CONTD5 contd5ExpicitRK)
        {
            this._Contd5ExpicitRK = contd5ExpicitRK;
        }


        public RKSolOut(CONTR5 contr5ImplicitRK)
        {
            this._Contr5ImplicitRK = contr5ImplicitRK;
        }


        #endregion


        #region Methods

        internal void Initialize(double[] y0, double t0, double deltaT, double tf, OdeSolution solution)
        {
            this._SolutionOutType = SolutionOutType.Delegate;
            this.InitializeValues(y0, t0, deltaT, tf);

            this._SolutionOut = solution;

        }

        internal void Initialize(double[] y0, double t0, double deltaT, double tf, out double[,] solutionArray)
        {
            this._SolutionOutType = SolutionOutType.Array;
            this.InitializeValues(y0, t0, deltaT, tf);

            int NCols = y0.Length + 1;
            solutionArray = new double[this._MaxIndex, NCols];

            this._SolutionArray = solutionArray;

        }

        internal void Initialize(double[] y0, double[] tspan, OdeSolution solution)
        {
            this._SolutionOutType = SolutionOutType.Delegate;
            this.InitializeValues(y0, tspan);

            this._SolutionOut = solution;

        }

        internal void Initialize(double[] y0, double[] tspan, out double[,] solutionArray)
        {
            this._SolutionOutType = SolutionOutType.Array;
            this.InitializeValues(y0, tspan);

            int NCols = y0.Length + 1;
            solutionArray = new double[this._MaxIndex, NCols];

            this._SolutionArray = solutionArray;

        }


        private void InitializeValues(double[] y0, double t0, double deltaT, double tf)
        {
            this._IsTimeArrayUsed = false;

            this._NEquations = y0.Length;
            this._Y0 = y0;
            this._T0 = t0;
            this._DeltaT = deltaT;
            this._TF = tf;
            this._MaxIndex = (int)(Math.Abs(tf - t0) / Math.Abs(deltaT)) + 1;

            if (deltaT > 0) this._isDeltaPositive = true;
            else this._isDeltaPositive = false;

            this._TemporalSolution = new double[y0.Length];

        }


        private void InitializeValues(double[] y0, double[] tspan)
        {
            this._IsTimeArrayUsed = true;

            this._NEquations = y0.Length;
            this._Y0 = y0;

            this._TSpan = tspan;
            this._MaxIndex = tspan.Length;


            if ((tspan[1] - tspan[0]) > 0) this._isDeltaPositive = true;
            else this._isDeltaPositive = false;

            this._TemporalSolution = new double[y0.Length];

        }


        //protected void SetSolutionDimension(double[] y0, double t0, double deltaT, double tEnd)
        //{

        //    int NCols = y0.Length + 1;
        //    int NRens = (int)(Math.Abs(tEnd - t0) / Math.Abs(deltaT)) + 1;
        //    this.MeSolution = new double[NRens, NCols];

        //    for (int i = 0; i < NRens; i++)
        //    {
        //        this.MeSolution[i, 0] = t0 + deltaT * i;
        //    }
        //    //for (int j = 1; j < NCols; j++)
        //    //{
        //    //    this.MeSolution[0, j] = y0[j - 1];
        //    //}

        //}


        //private int CalculateMaxIndex(double t0, double deltaT, double tEnd)
        //{
        //    return 
        //}


        /// <summary>
        /// Calcula el valor temporal de la integracion para un idice dado.
        /// </summary>
        /// <param name="index">El indice corresponiente al tiempo deseado.</param>
        /// <returns>El tiempo de integracio.</returns>
        protected double GetTime(int index)
        {
            double t = 0;

            if (this._IsTimeArrayUsed == true)
            {
                t = this._TSpan[index];
            }
            else
            {
                t = _T0 + _DeltaT * index;
            }

            return t;
        }

        #endregion




        #region ISOLOUT Members

        void ISOLOUT.Run(int NR, double XOLD, double X, double[] Y, int o_y, int N, double[] CON,
            int o_con, int[] ICOMP, int o_icomp, int ND, double[] RPAR, int o_rpar, int IPAR, int IRTRN)
        {


            #region                                         Array Index Correction
            //--------------------------------------------------------------------------------------------------------------------
            //                                              Array Index Correction
            //--------------------------------------------------------------------------------------------------------------------

            int c_y = -1 + o_y; int c_con = -1 + o_con; int c_icomp = -1 + o_icomp; int c_rpar = -1 + o_rpar;

            #endregion


            if (NR == 1)
            {
                if (this._SolutionOutType == SolutionOutType.Array)
                {
                    this._SolutionArray[0, 0] = this._T0;
                    for (int j = 1; j <= this._NEquations; j++)
                    {
                        this._SolutionArray[0, j] = this._Y0[j - 1];
                    }
                }
                else
                {
                    this._SolutionOut(this._T0, this._Y0);
                }

                this._Index = 1;
            }


            bool MyContinue = true;

            while (MyContinue && this._Index < this._MaxIndex)
            {
                this._T = this.GetTime(this._Index);

                if ((this._isDeltaPositive && X >= this._T) || (this._isDeltaPositive == false && X <= this._T))
                {
                    if (this._SolutionOutType == SolutionOutType.Array)
                    {
                        this._SolutionArray[this._Index, 0] = this._T;
                        for (int j = 1; j <= this._NEquations; j++)
                        {
                            this._SolutionArray[this._Index, j] = this._Contd5ExpicitRK.Run(j, this._T, CON, o_con, ICOMP, o_icomp, ND);
                        }
                    }
                    else
                    {
                        for (int j = 0; j < this._NEquations; j++)
                        {
                            this._TemporalSolution[j] = this._Contd5ExpicitRK.Run(j + 1, this._T, CON, o_con, ICOMP, o_icomp, ND);
                        }
                        this._SolutionOut(this._T, this._TemporalSolution);
                    }

                    this._Index++;
                }
                else MyContinue = false;
            }
            return;
        }

        #endregion

        #region ISOLOUTR Members

        void ISOLOUTR.Run(int NR, double XOLD, double X, double[] Y, int o_y, double[] CONT, int o_cont, 
            int LRC, int N, double RPAR, int IPAR, int IRTRN)
        {

            #region                                         Array Index Correction
            //--------------------------------------------------------------------------------------------------------------------
            //                                              Array Index Correction
            //--------------------------------------------------------------------------------------------------------------------

            int c_y = -1 + o_y; int c_cont = -1 + o_cont;

            #endregion


            if (NR == 1)
            {
                if (this._SolutionOutType == SolutionOutType.Array)
                {
                    this._SolutionArray[0, 0] = this._T0;
                    for (int j = 1; j <= this._NEquations; j++)
                    {
                        this._SolutionArray[0, j] = this._Y0[j - 1];
                    }
                }
                else
                {
                    this._SolutionOut(this._T0, this._Y0);
                }

                this._Index = 1;
            }


            bool MyContinue = true;

            while (MyContinue && this._Index < this._MaxIndex)
            {
                this._T = this.GetTime(this._Index);

                if ((this._isDeltaPositive && X >= this._T) || (this._isDeltaPositive == false && X <= this._T))
                {
                    if (this._SolutionOutType == SolutionOutType.Array)
                    {
                        this._SolutionArray[this._Index, 0] = this._T;
                        for (int j = 1; j <= this._NEquations; j++)
                        {
                            this._SolutionArray[this._Index, j] = this._Contr5ImplicitRK.Run(j, this._T, CONT, o_cont, LRC);
                        }
                    }
                    else
                    {
                        for (int j = 0; j < this._NEquations; j++)
                        {
                            this._TemporalSolution[j] = this._Contr5ImplicitRK.Run(j + 1, this._T, CONT, o_cont, LRC);
                        }
                        this._SolutionOut(this._T, this._TemporalSolution);
                    }

                    this._Index++;
                }
                else MyContinue = false;
            }
            return;


        }

        #endregion
    }
}
