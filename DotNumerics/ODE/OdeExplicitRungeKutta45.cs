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
using DotNumerics.FortranLibrary;

namespace DotNumerics.ODE
{
    /// <summary>
    /// Solves an initial-value problem for nonstiff ordinary differential equations using 
    /// the explicit Runge-Kutta method of order (4)5.
    /// dy(i)/dt = f(i,t,y(1),y(2),...,y(N)).  
    /// </summary>
    public sealed class OdeExplicitRungeKutta45 : xBaseOdeRungeKutta
    {

        #region Fields

        private DOPRI5 _Dopri5;
        private FAREN _Faren;
        //private SOLOUT Solout;

        private DOPCOR _dopcor;
        private HINIT _hinit;
        private CONTD5 _contd5;
        private CDOPRI _cdopri;
        CommonBlock _CONDO5 = new CommonBlock(2, 0, 0, 0);



        //private OdeFun MeFunction;
        //OdeJac MeJacobian;

        #endregion

        #region Constructor


        /// <summary>
        /// Initializes a new instance of the OdeExplicitRungeKutta45 class.
        /// </summary>
        public OdeExplicitRungeKutta45()
        {
            this.InitializeRungeKuttaClasses();

        }

        /// <summary>
        /// Initializes a new instance of the OdeExplicitRungeKutta45 class.
        /// </summary>
        /// <param name="function">A function that evaluates the right side of the differential equations.</param>
        /// <param name="numEquations">The number of differential equations.</param>
        public OdeExplicitRungeKutta45(OdeFunction function, int numEquations)
        {
            this.InitializeRungeKuttaClasses();
            base.InitializeRungeKutta(function, null, numEquations);

        }


        #endregion


        #region Methods

        /// <summary>
        /// Method that initialize the ODE to solve.
        /// </summary>
        /// <param name="function">A function that evaluates the right side of the differential equations.</param>
        /// <param name="numEquations">The number of differential equations.</param>
        public override void InitializeODEs(OdeFunction function, int numEquations)
        {
            base.InitializeRungeKutta(function, null, numEquations);

            this._InvokeSetInitialValues = true;
        }

        /// <summary>
        /// Method that initialize the ODE to solve.
        /// </summary>
        /// <param name="function">A function that evaluates the right side of the differential equations.</param>
        /// <param name="numEquations">The number of differential equations.</param>
        /// <param name="t0">The initial value for the independent variable.</param>
        /// <param name="y0">A vector of size N containing the initial conditions. N is the number of differential equations.</param>
        public override void InitializeODEs(OdeFunction function, int numEquations, double t0, double[] y0)
        {
            base.InitializeRungeKutta(function, null, numEquations);

            this.SetInitialValues(t0, y0);
        }

        #endregion


        #region Properties

        ///// <summary>
        ///// MeITol1 = An indicator for the type of error control.
        /////                 ITOL    RTOL       ATOL          EWT(i)
        ///// MeITol1 =1     scalar     scalar     RTOL*ABS(Y(i)) + ATOL
        ///// MeITol1 =2     scalar     array      RTOL*ABS(Y(i)) + ATOL(i)
        ///// MeITol1 =3     array      scalar     RTOL(i)*ABS(Y(i)) + ATOL
        ///// MeITol1 =4     array      array      RTOL(i)*ABS(Y(i)) + ATOL(i)
        ///// </summary>
        //public int ITol
        //{
        //    get { return MeITol; }
        //    set { MeITol = value; }
        //}



        #endregion

        #region Internal Metods

        internal override void InitializeRungeKuttaClasses()
        {
            _hinit = new HINIT();
            _cdopri = new CDOPRI();
            _contd5 = new CONTD5(_CONDO5);
            _dopcor = new DOPCOR(_hinit, _cdopri, _CONDO5);
            this._Dopri5 = new DOPRI5(_dopcor);

            base._RKSolOut = new RKSolOut(this._contd5);

        }

        internal override void InitializeExceptionMessages()
        {
            this._Errors = new string[5];
            this._Errors[0] = "";
            this._Errors[1] = "INPUT IS NOT CONSISTENT.";
            this._Errors[2] = "LARGER NMAX IS NEEDED.";
            this._Errors[3] = "STEP SIZE BECOMES TOO SMALL.";
            this._Errors[4] = "PROBLEM IS PROBABLY STIFF.";
        }

        internal override void InitializeFunctionAndJacobian(OdeFunction fun, OdeJacobian jac)
        {
            this._Faren = new FAREN(this._NEquations, fun);
        }

        internal override void InitializeWorkingSpace()
        {
            // C     WORK        ARRAY OF WORKING SPACE OF LENGTH "LWORK".
            // C                 WORK(1),...,WORK(20) SERVE AS PARAMETERS FOR THE CODE.
            // C                 FOR STANDARD USE, SET THEM TO ZERO BEFORE CALLING.
            // C                 "LWORK" MUST BE AT LEAST  8*N+5*NRDENS+21
            // C                 WHERE  NRDENS = IWORK(5)
            // C
            // C     LWORK       DECLARED LENGHT OF ARRAY "WORK".
            // C
            // C     IWORK       INTEGER WORKING SPACE OF LENGHT "LIWORK".
            // C                 IWORK(1),...,IWORK(20) SERVE AS PARAMETERS FOR THE CODE.
            // C                 FOR STANDARD USE, SET THEM TO ZERO BEFORE CALLING.
            // C                 "LIWORK" MUST BE AT LEAST NRDENS+21 .
            // C
            // C     LIWORK      DECLARED LENGHT OF ARRAY "IWORK".
            // C
            // C    IWORK(5)  = NRDENS = NUMBER OF COMPONENTS, FOR WHICH DENSE OUTPUT
            // C              IS REQUIRED; DEFAULT VALUE IS IWORK(5)=0;
            // C              FOR   0 < NRDENS < N   THE COMPONENTS (FOR WHICH DENSE
            // C              OUTPUT IS REQUIRED) HAVE TO BE SPECIFIED IN
            // C              IWORK(21),...,IWORK(NRDENS+20);
            // C              FOR  NRDENS=N  THIS IS DONE BY THE CODE.



            // C                 WHERE  NRDENS = IWORK(5)
            // C              FOR  NRDENS=N  THIS IS DONE BY THE CODE.
            int NRDENS = this._NEquations;
            // C                 "LWORK" MUST BE AT LEAST  8*N+5*NRDENS+21
            int LWORK = 8 * this._NEquations + 5 * NRDENS + 21;
            // C                 "LIWORK" MUST BE AT LEAST NRDENS+21 .
            int LIWORK = NRDENS + 21;


            this._Lrw = LWORK;
            this._Liw = LIWORK;


            this._RWork = new double[this._Lrw];
            this._IWork = new int[this._Liw];
        }

        #endregion


        #region Public Methods



        internal override void Solve()
        {
            bool WasSuccessfully = true;

            this._Dopri5.Run(this._NEquations, this._Faren, ref this._T0, ref this._Y0, 0, this.MeTf, this._RelTolArray, 0
            , this._AbsTolArray, 0, this._ITolRK, this._RKSolOut, this._IOut, ref this._RWork, 0, this._Lrw
            , ref this._IWork, 0, this._Liw, this._RPar, 0, this._IPar, 0, ref this._IDID);

            if (this._IDID < 0) WasSuccessfully = false;

            if (WasSuccessfully == false)
            {
                throw new Exception(this._Errors[-this._IDID]);
            }
        }


        #endregion


    }

}
