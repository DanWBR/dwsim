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

using DotNumerics.ODE.Dopri5;
using DotNumerics.ODE.Radau5;

namespace DotNumerics.ODE
{
    /// <summary>
    ///Represents a base class for the Runge-Kutta classes. 
    /// </summary>
    public abstract class xBaseOdeRungeKutta : xOdeBase
    {

        #region Fields

        /// <summary>
        /// The Sub for the solution.
        /// </summary>
        internal RKSolOut _RKSolOut;


        protected double MeTf = 0;

        /// <summary>
        /// The array containing the solution of the ODEs [T, Y1,Y2,...]
        /// </summary>
        protected double[,] _Solution;

        /// <summary>
        /// SWITCH FOR CALLING THE SUBROUTINE SOLOUTR:
        /// IOUT=0: SUBROUTINE IS NEVER CALLED
        /// IOUT=1: SUBROUTINE IS USED FOR OUTPUT.
        /// IOUT=2: DENSE OUTPUT IS PERFORMED IN SOLOUTR
        /// (IN THIS CASE WORK(5) MUST BE SPECIFIED)
        /// </summary>
        protected int _IOut = 2;

        /// <summary>
        /// REPORTS ON SUCCESSFULNESS UPON RETURN:
        /// IDID= 1  COMPUTATION SUCCESSFUL,
        /// IDID= 2  COMPUT. SUCCESSFUL (INTERRUPTED BY SOLOUTR)
        /// IDID=-1  INPUT IS NOT CONSISTENT,
        /// IDID=-2  LARGER NMAX IS NEEDED,
        /// IDID=-3  STEP SIZE BECOMES TOO SMALL.
        /// IDID=-4  PROBLEM IS PROBABLY STIFF (INTERRUPTED).
        /// </summary>
        protected int _IDID = 0;





        #endregion



        #region Public Methods


        /// <summary>
        ///  Computes the solution of the differntial equations.
        /// </summary>
        /// <param name="y0">A vector of size N containing the initial conditions ( at t0). N is the number of differential equations.</param>
        /// <param name="tspan">A vector specifying the interval of integration (t0,..,tf).</param>
        /// <returns>
        /// A matrix that contains the solution of the differential equations [T, y1,..,yN]. 
        /// The first column contains the time points and each row corresponds to the solution at a time returned in the corresponding row.
        /// </returns>
        public double[,] Solve(double[] y0, double[] tspan)
        {

            this.CheckTArray(tspan);

            base.SetInitialValues(tspan[0], y0);


            this.MeTf = tspan[tspan.Length - 1];

            this._RKSolOut.Initialize(y0, tspan, out this._Solution);

            this.Solve();

            return this._Solution;
        }

        /// <summary>
        /// Computes the solution of the differntial equations.
        /// </summary>
        /// <param name="y0">A vector of size N containing the initial conditions. N is the number of differential equations.</param>
        /// <param name="t0">The initial independent variable value.</param>
        /// <param name="deltaT">The step for the interval of integration (t0, t0+deltaT, t0+2*deltaT,...,tf).</param>
        /// <param name="tf">The final independent variable value.</param>
        /// <returns>
        /// A matrix that contains the solution of the differential equations [T, y1,..,yN]. 
        /// The first column contains the time points and each row corresponds to the solution at a time returned in the corresponding row.
        /// </returns>
        public double[,] Solve(double[] y0, double t0, double deltaT, double tf)
        {
            this.CheckArguments(t0, deltaT, tf);

            base.SetInitialValues(t0, y0);

            this.MeTf = tf;

            this._RKSolOut.Initialize(y0, t0, deltaT, tf, out this._Solution);

            this.Solve();

            return this._Solution;


        }

        /// <summary>
        /// Computes the solution of the differntial equations.
        /// </summary>
        /// <param name="y0">A vector of size N containing the initial conditions. N is the number of differential equations.</param>
        /// <param name="t0">The initial independent variable value.</param>
        /// <param name="deltaT">The step for the interval of integration (t0, t0+deltaT, t0+2*deltaT,...,tf).</param>
        /// <param name="tf">The final independent variable value.</param>
        /// <param name="solution">A delegate where to return the solution.</param>
        public void Solve(double[] y0, double t0, double deltaT, double tf, OdeSolution solution)
        {
            this.CheckArguments(t0, deltaT, tf);

            base.SetInitialValues(t0, y0);

            this.MeTf = tf;

            this._RKSolOut.Initialize(y0, t0, deltaT, tf, solution);

            this.Solve();

        }


        /// <summary>
        ///  Computes the solution of the differntial equations.
        /// </summary>
        /// <param name="y0">A vector of size N containing the initial conditions ( at t0). N is the number of differential equations.</param>
        /// <param name="tspan">A vector specifying the interval of integration (t0,..,tf).</param>
        /// <param name="solution">A delegate where to return the solution.</param>
        public void Solve(double[] y0, double[] tspan, OdeSolution solution)
        {

            this.CheckTArray(tspan);

            base.SetInitialValues(tspan[0], y0);

            this.MeTf = tspan[tspan.Length - 1];

            this._RKSolOut.Initialize(y0, tspan, solution);

            this.Solve();
        }


        #endregion



        internal abstract void Solve();



        #region Methods

        internal void InitializeRungeKutta(OdeFunction function, OdeJacobian jacobian, int numEquations)
        {
            //internal void InitializeRungeKutta(OdeFunction function, OdeJacobian jacobian, double[] y0, double t0, double deltaT, double tEnd)
            //this.TestArguments(t0, deltaT, tEnd);

            //this.MeT0 = t0;
            //this.MeY0 = y0;
            //this.MeTf = tEnd;


            this.InitializeInternal(function, jacobian, numEquations);

            // IOUT=2: DENSE OUTPUT IS PERFORMED IN SOLOUTR
            // (IN THIS CASE WORK(5) MUST BE SPECIFIED)
            if (this._IOut == 2)
            {
                this._IWork[4] = numEquations;
            }

            //this.SetSolutionDimension(y0, t0, deltaT, tEnd);

        }

        //internal void InitializeRungeKutta(OdeFunction function, OdeJacobian jacobian, double[] y0, double[] t)
        //{

        //    this.CheckTArray(t);

        //    this.MeT0 = t[0];
        //    this.MeY0 = y0;
        //    this.MeTf = t[t.Length - 1];

        //    this.InitializeRungeKuttaClasses();


        //    this.InitializeInternal(this.MeT0, y0, function, jacobian);

        //    // IOUT=2: DENSE OUTPUT IS PERFORMED IN SOLOUTR
        //    // (IN THIS CASE WORK(5) MUST BE SPECIFIED)
        //    if (this.MeIOut == 2)
        //    {
        //        this.MeIWork[4] = this.MeNEquations;
        //    }

        //    //this.SetSolutionDimension(y0, t);

        //}

        internal abstract void InitializeRungeKuttaClasses();



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

        //protected void SetSolutionDimension(double[] y0, double[] tspan)
        //{

        //    int NCols = y0.Length + 1;
        //    int NRens = tspan.Length;
        //    this.MeSolution = new double[NRens, NCols];

        //    for (int i = 0; i < tspan.Length; i++)
        //    {
        //        this.MeSolution[i, 0] = tspan[i];
        //    }
        //    //for (int j = 1; j < NCols; j++)
        //    //{
        //    //    this.MeSolution[0, j] = y0[j - 1];
        //    //}
        //}

        #endregion

    }

}
