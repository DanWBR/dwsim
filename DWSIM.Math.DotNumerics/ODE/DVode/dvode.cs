#region Translated by Jose Antonio De Santiago-Castillo.

//Translated by Jose Antonio De Santiago-Castillo.
//E-mail:JAntonioDeSantiago@gmail.com
//Website: www.DotNumerics.com
//
//Fortran to C# Translation.
//Translated by:
//F2CSharp Version 0.72 (Dicember 7, 2009)
//Code Optimizations: , assignment operator, for-loop: array indexes
//
#endregion

using System;
using DotNumerics.FortranLibrary;

namespace DotNumerics.ODE.DVode
{

    #region The Class: DVODE
    
    // *DECK DVODE
    public class DVODE
    {
    
        #region Dependencies
        
        DVNLSD _dvnlsd; DUMACH _dumach; DVNORM _dvnorm; DCOPY _dcopy; DEWSET _dewset; DVHIN _dvhin; DSCAL _dscal; DVINDY _dvindy; 
        XERRWD _xerrwd;DVSTEP _dvstep; 
        #endregion
        #region Common variables
        
        #region Common Block: DVOD01 Declaration
        
        CommonBlock _dvod01;
        Odouble ACNRM; Odouble CCMXJ; Odouble CONP; Odouble CRATE; Odouble DRC; Odouble[] EL; int o_el; Odouble ETA; 
        Odouble ETAMAX;Odouble H; Odouble HMIN; Odouble HMXI; Odouble HNEW; Odouble HSCAL; Odouble PRL1; Odouble RC; Odouble RL1; 
        Odouble[] TAU; int o_tau;Odouble[] TQ; int o_tq; Odouble TN; Odouble UROUND; 
        Oint ICF; Oint INIT; Oint IPUP; Oint JCUR; Oint JSTART; Oint JSV; Oint KFLAG; Oint KUTH; Oint L; Oint LMAX; Oint LYH; 
        Oint LEWT;Oint LACOR; Oint LSAVF; Oint LWM; Oint LIWM; Oint LOCJS; Oint MAXORD; Oint METH; Oint MITER; Oint MSBJ; 
        Oint MXHNIL;Oint MXSTEP; Oint N; Oint NEWH; Oint NEWQ; Oint NHNIL; Oint NQ; Oint NQNYH; Oint NQWAIT; Oint NSLJ; Oint NSLP; 
        Oint NYH;
        #endregion
        #region Common Block: DVOD02 Declaration
        
        CommonBlock _dvod02;
        Odouble HU; 
        Oint NCFN; Oint NETF; Oint NFE; Oint NJE; Oint NLU; Oint NNI; Oint NQU; Oint NST; 
        #endregion
        #endregion
        #region Variables
        
        double FOUR = 0; double HUN = 0; double ONE = 0; double PT2 = 0; double TWO = 0; double ZERO = 0; 
        int[] MORD = new int[2]; int offset_mord = 0; int o_mord = -1;int MXHNL0 = 0; int MXSTP0 = 0; 
        #endregion
        public DVODE(DVNLSD dvnlsd, DUMACH dumach, DVNORM dvnorm, DCOPY dcopy, DEWSET dewset, DVHIN dvhin, DSCAL dscal, DVINDY dvindy, XERRWD xerrwd, DVSTEP dvstep
                     , CommonBlock DVOD01, CommonBlock DVOD02)
        {
    
            #region Set Dependencies
            
            this._dvnlsd = dvnlsd; this._dumach = dumach; this._dvnorm = dvnorm; this._dcopy = dcopy; this._dewset = dewset; 
            this._dvhin = dvhin;this._dscal = dscal; this._dvindy = dvindy; this._xerrwd = xerrwd; this._dvstep = dvstep; 
            #endregion
            #region Data Initialization
            
            //MORD(1)/12
            MORD[1 + o_mord] = 12;
            //MORD(2)/5
            MORD[2 + o_mord] = 5;
            //MXSTP0/500
            MXSTP0 = 500;
            //MXHNL0/10
            MXHNL0 = 10;
            //ZERO/0.0D0
            ZERO = 0.0E0;
            //ONE/1.0D0
            ONE = 1.0E0;
            //TWO/2.0D0
            TWO = 2.0E0;
            //FOUR/4.0D0
            FOUR = 4.0E0;
            //PT2/0.2D0
            PT2 = 0.2E0;
            //HUN/100.0D0
            HUN = 100.0E0;
            #endregion
            #region Common varaible Initialization
            
            #region Common Block: DVOD01 Initialization
            
            this._dvod01 = DVOD01;
            ACNRM = DVOD01.doubleData[0];
            CCMXJ = DVOD01.doubleData[1];
            CONP = DVOD01.doubleData[2];
            CRATE = DVOD01.doubleData[3];
            DRC = DVOD01.doubleData[4];
            //Start Array: EL  StartIndex: 5  LastIndex: 17
            EL = DVOD01.doubleData;
            o_el = 4;  //o_ = StartIndex-1
            //End Array: EL
            ETA = DVOD01.doubleData[18];
            ETAMAX = DVOD01.doubleData[19];
            H = DVOD01.doubleData[20];
            HMIN = DVOD01.doubleData[21];
            HMXI = DVOD01.doubleData[22];
            HNEW = DVOD01.doubleData[23];
            HSCAL = DVOD01.doubleData[24];
            PRL1 = DVOD01.doubleData[25];
            RC = DVOD01.doubleData[26];
            RL1 = DVOD01.doubleData[27];
            //Start Array: TAU  StartIndex: 28  LastIndex: 40
            TAU = DVOD01.doubleData;
            o_tau = 27;  //o_ = StartIndex-1
            //End Array: TAU
            //Start Array: TQ  StartIndex: 41  LastIndex: 45
            TQ = DVOD01.doubleData;
            o_tq = 40;  //o_ = StartIndex-1
            //End Array: TQ
            TN = DVOD01.doubleData[46];
            UROUND = DVOD01.doubleData[47];
            ICF = DVOD01.intData[0];
            INIT = DVOD01.intData[1];
            IPUP = DVOD01.intData[2];
            JCUR = DVOD01.intData[3];
            JSTART = DVOD01.intData[4];
            JSV = DVOD01.intData[5];
            KFLAG = DVOD01.intData[6];
            KUTH = DVOD01.intData[7];
            L = DVOD01.intData[8];
            LMAX = DVOD01.intData[9];
            LYH = DVOD01.intData[10];
            LEWT = DVOD01.intData[11];
            LACOR = DVOD01.intData[12];
            LSAVF = DVOD01.intData[13];
            LWM = DVOD01.intData[14];
            LIWM = DVOD01.intData[15];
            LOCJS = DVOD01.intData[16];
            MAXORD = DVOD01.intData[17];
            METH = DVOD01.intData[18];
            MITER = DVOD01.intData[19];
            MSBJ = DVOD01.intData[20];
            MXHNIL = DVOD01.intData[21];
            MXSTEP = DVOD01.intData[22];
            N = DVOD01.intData[23];
            NEWH = DVOD01.intData[24];
            NEWQ = DVOD01.intData[25];
            NHNIL = DVOD01.intData[26];
            NQ = DVOD01.intData[27];
            NQNYH = DVOD01.intData[28];
            NQWAIT = DVOD01.intData[29];
            NSLJ = DVOD01.intData[30];
            NSLP = DVOD01.intData[31];
            NYH = DVOD01.intData[32];
            #endregion
            #region Common Block: DVOD02 Initialization
            
            this._dvod02 = DVOD02;
            HU = DVOD02.doubleData[0];
            NCFN = DVOD02.intData[0];
            NETF = DVOD02.intData[1];
            NFE = DVOD02.intData[2];
            NJE = DVOD02.intData[3];
            NLU = DVOD02.intData[4];
            NNI = DVOD02.intData[5];
            NQU = DVOD02.intData[6];
            NST = DVOD02.intData[7];
            #endregion
            #endregion
        }
    
        public DVODE()
        {
    
            #region Initialization Common Blocks
            
            CommonBlock DVOD01 = new CommonBlock(48, 33, 0, 0);
            CommonBlock DVOD02 = new CommonBlock(1, 8, 0, 0);
            #endregion
            #region Dependencies (Initialization)
            
            DVNORM dvnorm = new DVNORM();
            DCOPY dcopy = new DCOPY();
            DSCAL dscal = new DSCAL();
            IDAMAX idamax = new IDAMAX();
            DAXPY daxpy = new DAXPY();
            DDOT ddot = new DDOT();
            DUMSUM dumsum = new DUMSUM();
            DEWSET dewset = new DEWSET();
            IUMACH iumach = new IUMACH();
            DVSET dvset = new DVSET(DVOD01);
            DGEFA dgefa = new DGEFA(idamax, dscal, daxpy);
            DACOPY dacopy = new DACOPY(dcopy);
            DGBFA dgbfa = new DGBFA(idamax, dscal, daxpy);
            DVJAC dvjac = new DVJAC(dvnorm, dcopy, dscal, dgefa, dacopy, dgbfa, DVOD01, DVOD02);
            DGESL dgesl = new DGESL(ddot, daxpy);
            DGBSL dgbsl = new DGBSL(ddot, daxpy);
            DVSOL dvsol = new DVSOL(dgesl, dgbsl, DVOD01);
            DVNLSD dvnlsd = new DVNLSD(dvnorm, dcopy, dvjac, dvsol, dscal, daxpy, DVOD01, DVOD02);
            DUMACH dumach = new DUMACH(dumsum);
            DVHIN dvhin = new DVHIN(dvnorm);
            IXSAV ixsav = new IXSAV(iumach);
            XERRWD xerrwd = new XERRWD(ixsav);
            DVINDY dvindy = new DVINDY(dscal, xerrwd, DVOD01, DVOD02);
            DVJUST dvjust = new DVJUST(daxpy, DVOD01);
            DVSTEP dvstep = new DVSTEP(dvnorm, dvjust, dscal, dvset, daxpy, dcopy, DVOD01, DVOD02);
            #endregion
            #region Set Dependencies
            
            this._dvnlsd = dvnlsd; this._dumach = dumach; this._dvnorm = dvnorm; this._dcopy = dcopy; this._dewset = dewset; 
            this._dvhin = dvhin;this._dscal = dscal; this._dvindy = dvindy; this._xerrwd = xerrwd; this._dvstep = dvstep; 
            #endregion
            #region Data Initialization
            
            //MORD(1)/12
            MORD[1 + o_mord] = 12;
            //MORD(2)/5
            MORD[2 + o_mord] = 5;
            //MXSTP0/500
            MXSTP0 = 500;
            //MXHNL0/10
            MXHNL0 = 10;
            //ZERO/0.0D0
            ZERO = 0.0E0;
            //ONE/1.0D0
            ONE = 1.0E0;
            //TWO/2.0D0
            TWO = 2.0E0;
            //FOUR/4.0D0
            FOUR = 4.0E0;
            //PT2/0.2D0
            PT2 = 0.2E0;
            //HUN/100.0D0
            HUN = 100.0E0;
            #endregion
            #region Common varaible Initialization
            
            #region Common Block: DVOD01 Initialization
            
            this._dvod01 = DVOD01;
            ACNRM = DVOD01.doubleData[0];
            CCMXJ = DVOD01.doubleData[1];
            CONP = DVOD01.doubleData[2];
            CRATE = DVOD01.doubleData[3];
            DRC = DVOD01.doubleData[4];
            //Start Array: EL  StartIndex: 5  LastIndex: 17
            EL = DVOD01.doubleData;
            o_el = 4;  //o_ = StartIndex-1
            //End Array: EL
            ETA = DVOD01.doubleData[18];
            ETAMAX = DVOD01.doubleData[19];
            H = DVOD01.doubleData[20];
            HMIN = DVOD01.doubleData[21];
            HMXI = DVOD01.doubleData[22];
            HNEW = DVOD01.doubleData[23];
            HSCAL = DVOD01.doubleData[24];
            PRL1 = DVOD01.doubleData[25];
            RC = DVOD01.doubleData[26];
            RL1 = DVOD01.doubleData[27];
            //Start Array: TAU  StartIndex: 28  LastIndex: 40
            TAU = DVOD01.doubleData;
            o_tau = 27;  //o_ = StartIndex-1
            //End Array: TAU
            //Start Array: TQ  StartIndex: 41  LastIndex: 45
            TQ = DVOD01.doubleData;
            o_tq = 40;  //o_ = StartIndex-1
            //End Array: TQ
            TN = DVOD01.doubleData[46];
            UROUND = DVOD01.doubleData[47];
            ICF = DVOD01.intData[0];
            INIT = DVOD01.intData[1];
            IPUP = DVOD01.intData[2];
            JCUR = DVOD01.intData[3];
            JSTART = DVOD01.intData[4];
            JSV = DVOD01.intData[5];
            KFLAG = DVOD01.intData[6];
            KUTH = DVOD01.intData[7];
            L = DVOD01.intData[8];
            LMAX = DVOD01.intData[9];
            LYH = DVOD01.intData[10];
            LEWT = DVOD01.intData[11];
            LACOR = DVOD01.intData[12];
            LSAVF = DVOD01.intData[13];
            LWM = DVOD01.intData[14];
            LIWM = DVOD01.intData[15];
            LOCJS = DVOD01.intData[16];
            MAXORD = DVOD01.intData[17];
            METH = DVOD01.intData[18];
            MITER = DVOD01.intData[19];
            MSBJ = DVOD01.intData[20];
            MXHNIL = DVOD01.intData[21];
            MXSTEP = DVOD01.intData[22];
            N = DVOD01.intData[23];
            NEWH = DVOD01.intData[24];
            NEWQ = DVOD01.intData[25];
            NHNIL = DVOD01.intData[26];
            NQ = DVOD01.intData[27];
            NQNYH = DVOD01.intData[28];
            NQWAIT = DVOD01.intData[29];
            NSLJ = DVOD01.intData[30];
            NSLP = DVOD01.intData[31];
            NYH = DVOD01.intData[32];
            #endregion
            #region Common Block: DVOD02 Initialization
            
            this._dvod02 = DVOD02;
            HU = DVOD02.doubleData[0];
            NCFN = DVOD02.intData[0];
            NETF = DVOD02.intData[1];
            NFE = DVOD02.intData[2];
            NJE = DVOD02.intData[3];
            NLU = DVOD02.intData[4];
            NNI = DVOD02.intData[5];
            NQU = DVOD02.intData[6];
            NST = DVOD02.intData[7];
            #endregion
            #endregion
        }
        /// <param name="F">
        /// = The name of the user-supplied subroutine defining the
        /// ODE system.  The system must be put in the first-order
        /// form dy/dt = f(t,y), where f is a vector-valued function
        /// of the scalar t and the vector y.  Subroutine F is to
        /// compute the function f.  It is to have the form
        /// SUBROUTINE F (NEQ, T, Y, YDOT, RPAR, IPAR)
        /// DOUBLE PRECISION T, Y(NEQ), YDOT(NEQ), RPAR
        /// where NEQ, T, and Y are input, and the array YDOT = f(t,y)
        /// is output.  Y and YDOT are arrays of length NEQ.
        /// Subroutine F should not alter Y(1),...,Y(NEQ).
        /// F must be declared EXTERNAL in the calling program.
        /// 
        /// Subroutine F may access user-defined real and integer
        /// work arrays RPAR and IPAR, which are to be dimensioned
        /// in the main program.
        /// 
        /// If quantities computed in the F routine are needed
        /// externally to DVODE, an extra call to F should be made
        /// for this purpose, for consistent and accurate results.
        /// If only the derivative dy/dt is needed, use DVINDY instead.
        ///</param>
        /// <param name="NEQ">
        /// = The size of the ODE system (number of first order
        /// ordinary differential equations).  Used only for input.
        /// NEQ may not be increased during the problem, but
        /// can be decreased (with ISTATE = 3 in the input).
        ///</param>
        /// <param name="Y">
        /// = A real array for the vector of dependent variables, of
        /// length NEQ or more.  Used for both input and output on the
        /// first call (ISTATE = 1), and only for output on other calls.
        /// On the first call, Y must contain the vector of initial
        /// values.  In the output, Y contains the computed solution
        /// evaluated at T.  If desired, the Y array may be used
        /// for other purposes between calls to the solver.
        /// 
        /// This array is passed as the Y argument in all calls to
        /// F and JAC.
        ///</param>
        /// <param name="T">
        /// = The independent variable.  In the input, T is used only on
        /// the first call, as the initial point of the integration.
        /// In the output, after each call, T is the value at which a
        /// computed solution Y is evaluated (usually the same as TOUT).
        /// On an error return, T is the farthest point reached.
        ///</param>
        /// <param name="TOUT">
        /// = The next value of t at which a computed solution is desired.
        /// Used only for input.
        /// 
        /// When starting the problem (ISTATE = 1), TOUT may be equal
        /// to T for one call, then should .ne. T for the next call.
        /// For the initial T, an input value of TOUT .ne. T is used
        /// in order to determine the direction of the integration
        /// (i.e. the algebraic sign of the step sizes) and the rough
        /// scale of the problem.  Integration in either direction
        /// (forward or backward in t) is permitted.
        /// 
        /// If ITASK = 2 or 5 (one-step modes), TOUT is ignored after
        /// the first call (i.e. the first call with TOUT .ne. T).
        /// Otherwise, TOUT is required on every call.
        /// 
        /// If ITASK = 1, 3, or 4, the values of TOUT need not be
        /// monotone, but a value of TOUT which backs up is limited
        /// to the current internal t interval, whose endpoints are
        /// TCUR - HU and TCUR.  (See optional output, below, for
        /// TCUR and HU.)
        ///</param>
        /// <param name="ITOL">
        /// = An indicator for the type of error control.  See
        /// description below under ATOL.  Used only for input.
        ///</param>
        /// <param name="RTOL">
        /// = A relative error tolerance parameter, either a scalar or
        /// an array of length NEQ.  See description below under ATOL.
        /// Input only.
        ///</param>
        /// <param name="ATOL">
        /// = An absolute error tolerance parameter, either a scalar or
        /// an array of length NEQ.  Input only.
        /// 
        /// The input parameters ITOL, RTOL, and ATOL determine
        /// the error control performed by the solver.  The solver will
        /// control the vector e = (e(i)) of estimated local errors
        /// in Y, according to an inequality of the form
        /// rms-norm of ( e(i)/EWT(i) )   .le.   1,
        /// where       EWT(i) = RTOL(i)*abs(Y(i)) + ATOL(i),
        /// and the rms-norm (root-mean-square norm) here is
        /// rms-norm(v) = sqrt(sum v(i)**2 / NEQ).  Here EWT = (EWT(i))
        /// is a vector of weights which must always be positive, and
        /// the values of RTOL and ATOL should all be non-negative.
        /// The following table gives the types (scalar/array) of
        /// RTOL and ATOL, and the corresponding form of EWT(i).
        /// 
        /// ITOL    RTOL       ATOL          EWT(i)
        /// 1     scalar     scalar     RTOL*ABS(Y(i)) + ATOL
        /// 2     scalar     array      RTOL*ABS(Y(i)) + ATOL(i)
        /// 3     array      scalar     RTOL(i)*ABS(Y(i)) + ATOL
        /// 4     array      array      RTOL(i)*ABS(Y(i)) + ATOL(i)
        /// 
        /// When either of these parameters is a scalar, it need not
        /// be dimensioned in the user's calling program.
        /// 
        /// If none of the above choices (with ITOL, RTOL, and ATOL
        /// fixed throughout the problem) is suitable, more general
        /// error controls can be obtained by substituting
        /// user-supplied routines for the setting of EWT and/or for
        /// the norm calculation.  See Part iv below.
        /// 
        /// If global errors are to be estimated by making a repeated
        /// run on the same problem with smaller tolerances, then all
        /// components of RTOL and ATOL (i.e. of EWT) should be scaled
        /// down uniformly.
        ///</param>
        /// <param name="ITASK">
        /// = An index specifying the task to be performed.
        /// Input only.  ITASK has the following values and meanings.
        /// 1  means normal computation of output values of y(t) at
        /// t = TOUT (by overshooting and interpolating).
        /// 2  means take one step only and return.
        /// 3  means stop at the first internal mesh point at or
        /// beyond t = TOUT and return.
        /// 4  means normal computation of output values of y(t) at
        /// t = TOUT but without overshooting t = TCRIT.
        /// TCRIT must be input as RWORK(1).  TCRIT may be equal to
        /// or beyond TOUT, but not behind it in the direction of
        /// integration.  This option is useful if the problem
        /// has a singularity at or beyond t = TCRIT.
        /// 5  means take one step, without passing TCRIT, and return.
        /// TCRIT must be input as RWORK(1).
        /// 
        /// Note:  If ITASK = 4 or 5 and the solver reaches TCRIT
        /// (within roundoff), it will return T = TCRIT (exactly) to
        /// indicate this (unless ITASK = 4 and TOUT comes before TCRIT,
        /// in which case answers at T = TOUT are returned first).
        ///</param>
        /// <param name="ISTATE">
        /// = an index used for input and output to specify the
        /// the state of the calculation.
        /// 
        /// In the input, the values of ISTATE are as follows.
        /// 1  means this is the first call for the problem
        /// (initializations will be done).  See note below.
        /// 2  means this is not the first call, and the calculation
        /// is to continue normally, with no change in any input
        /// parameters except possibly TOUT and ITASK.
        /// (If ITOL, RTOL, and/or ATOL are changed between calls
        /// with ISTATE = 2, the new values will be used but not
        /// tested for legality.)
        /// 3  means this is not the first call, and the
        /// calculation is to continue normally, but with
        /// a change in input parameters other than
        /// TOUT and ITASK.  Changes are allowed in
        /// NEQ, ITOL, RTOL, ATOL, IOPT, LRW, LIW, MF, ML, MU,
        /// and any of the optional input except H0.
        /// (See IWORK description for ML and MU.)
        /// Note:  A preliminary call with TOUT = T is not counted
        /// as a first call here, as no initialization or checking of
        /// input is done.  (Such a call is sometimes useful to include
        /// the initial conditions in the output.)
        /// Thus the first call for which TOUT .ne. T requires
        /// ISTATE = 1 in the input.
        /// 
        /// In the output, ISTATE has the following values and meanings.
        /// 1  means nothing was done, as TOUT was equal to T with
        /// ISTATE = 1 in the input.
        /// 2  means the integration was performed successfully.
        /// -1  means an excessive amount of work (more than MXSTEP
        /// steps) was done on this call, before completing the
        /// requested task, but the integration was otherwise
        /// successful as far as T.  (MXSTEP is an optional input
        /// and is normally 500.)  To continue, the user may
        /// simply reset ISTATE to a value .gt. 1 and call again.
        /// (The excess work step counter will be reset to 0.)
        /// In addition, the user may increase MXSTEP to avoid
        /// this error return.  (See optional input below.)
        /// -2  means too much accuracy was requested for the precision
        /// of the machine being used.  This was detected before
        /// completing the requested task, but the integration
        /// was successful as far as T.  To continue, the tolerance
        /// parameters must be reset, and ISTATE must be set
        /// to 3.  The optional output TOLSF may be used for this
        /// purpose.  (Note: If this condition is detected before
        /// taking any steps, then an illegal input return
        /// (ISTATE = -3) occurs instead.)
        /// -3  means illegal input was detected, before taking any
        /// integration steps.  See written message for details.
        /// Note:  If the solver detects an infinite loop of calls
        /// to the solver with illegal input, it will cause
        /// the run to stop.
        /// -4  means there were repeated error test failures on
        /// one attempted step, before completing the requested
        /// task, but the integration was successful as far as T.
        /// The problem may have a singularity, or the input
        /// may be inappropriate.
        /// -5  means there were repeated convergence test failures on
        /// one attempted step, before completing the requested
        /// task, but the integration was successful as far as T.
        /// This may be caused by an inaccurate Jacobian matrix,
        /// if one is being used.
        /// -6  means EWT(i) became zero for some i during the
        /// integration.  Pure relative error control (ATOL(i)=0.0)
        /// was requested on a variable which has now vanished.
        /// The integration was successful as far as T.
        /// 
        /// Note:  Since the normal output value of ISTATE is 2,
        /// it does not need to be reset for normal continuation.
        /// Also, since a negative input value of ISTATE will be
        /// regarded as illegal, a negative output value requires the
        /// user to change it, and possibly other input, before
        /// calling the solver again.
        ///</param>
        /// <param name="IOPT">
        /// = An integer flag to specify whether or not any optional
        /// input is being used on this call.  Input only.
        /// The optional input is listed separately below.
        /// IOPT = 0 means no optional input is being used.
        /// Default values will be used in all cases.
        /// IOPT = 1 means optional input is being used.
        ///</param>
        /// <param name="RWORK">
        /// = A real working array (double precision).
        /// The length of RWORK must be at least
        /// 20 + NYH*(MAXORD + 1) + 3*NEQ + LWM    where
        /// NYH    = the initial value of NEQ,
        /// MAXORD = 12 (if METH = 1) or 5 (if METH = 2) (unless a
        /// smaller value is given as an optional input),
        /// LWM = length of work space for matrix-related data:
        /// LWM = 0             if MITER = 0,
        /// LWM = 2*NEQ**2 + 2  if MITER = 1 or 2, and MF.gt.0,
        /// LWM = NEQ**2 + 2    if MITER = 1 or 2, and MF.lt.0,
        /// LWM = NEQ + 2       if MITER = 3,
        /// LWM = (3*ML+2*MU+2)*NEQ + 2 if MITER = 4 or 5, and MF.gt.0,
        /// LWM = (2*ML+MU+1)*NEQ + 2   if MITER = 4 or 5, and MF.lt.0.
        /// (See the MF description for METH and MITER.)
        /// Thus if MAXORD has its default value and NEQ is constant,
        /// this length is:
        /// 20 + 16*NEQ                    for MF = 10,
        /// 22 + 16*NEQ + 2*NEQ**2         for MF = 11 or 12,
        /// 22 + 16*NEQ + NEQ**2           for MF = -11 or -12,
        /// 22 + 17*NEQ                    for MF = 13,
        /// 22 + 18*NEQ + (3*ML+2*MU)*NEQ  for MF = 14 or 15,
        /// 22 + 17*NEQ + (2*ML+MU)*NEQ    for MF = -14 or -15,
        /// 20 +  9*NEQ                    for MF = 20,
        /// 22 +  9*NEQ + 2*NEQ**2         for MF = 21 or 22,
        /// 22 +  9*NEQ + NEQ**2           for MF = -21 or -22,
        /// 22 + 10*NEQ                    for MF = 23,
        /// 22 + 11*NEQ + (3*ML+2*MU)*NEQ  for MF = 24 or 25.
        /// 22 + 10*NEQ + (2*ML+MU)*NEQ    for MF = -24 or -25.
        /// The first 20 words of RWORK are reserved for conditional
        /// and optional input and optional output.
        /// 
        /// The following word in RWORK is a conditional input:
        /// RWORK(1) = TCRIT = critical value of t which the solver
        /// is not to overshoot.  Required if ITASK is
        /// 4 or 5, and ignored otherwise.  (See ITASK.)
        ///</param>
        /// <param name="LRW">
        /// = The length of the array RWORK, as declared by the user.
        /// (This will be checked by the solver.)
        ///</param>
        /// <param name="IWORK">
        /// = An integer work array.  The length of IWORK must be at least
        /// 30        if MITER = 0 or 3 (MF = 10, 13, 20, 23), or
        /// 30 + NEQ  otherwise (abs(MF) = 11,12,14,15,21,22,24,25).
        /// The first 30 words of IWORK are reserved for conditional and
        /// optional input and optional output.
        /// 
        /// The following 2 words in IWORK are conditional input:
        /// IWORK(1) = ML     These are the lower and upper
        /// IWORK(2) = MU     half-bandwidths, respectively, of the
        /// banded Jacobian, excluding the main diagonal.
        /// The band is defined by the matrix locations
        /// (i,j) with i-ML .le. j .le. i+MU.  ML and MU
        /// must satisfy  0 .le.  ML,MU  .le. NEQ-1.
        /// These are required if MITER is 4 or 5, and
        /// ignored otherwise.  ML and MU may in fact be
        /// the band parameters for a matrix to which
        /// df/dy is only approximately equal.
        ///</param>
        /// <param name="LIW">
        /// = the length of the array IWORK, as declared by the user.
        /// (This will be checked by the solver.)
        ///</param>
        /// <param name="JAC">
        /// = The name of the user-supplied routine (MITER = 1 or 4) to
        /// compute the Jacobian matrix, df/dy, as a function of
        /// the scalar t and the vector y.  It is to have the form
        /// SUBROUTINE JAC (NEQ, T, Y, ML, MU, PD, NROWPD,
        /// RPAR, IPAR)
        /// DOUBLE PRECISION T, Y(NEQ), PD(NROWPD,NEQ), RPAR
        /// where NEQ, T, Y, ML, MU, and NROWPD are input and the array
        /// PD is to be loaded with partial derivatives (elements of the
        /// Jacobian matrix) in the output.  PD must be given a first
        /// dimension of NROWPD.  T and Y have the same meaning as in
        /// Subroutine F.
        /// In the full matrix case (MITER = 1), ML and MU are
        /// ignored, and the Jacobian is to be loaded into PD in
        /// columnwise manner, with df(i)/dy(j) loaded into PD(i,j).
        /// In the band matrix case (MITER = 4), the elements
        /// within the band are to be loaded into PD in columnwise
        /// manner, with diagonal lines of df/dy loaded into the rows
        /// of PD. Thus df(i)/dy(j) is to be loaded into PD(i-j+MU+1,j).
        /// ML and MU are the half-bandwidth parameters. (See IWORK).
        /// The locations in PD in the two triangular areas which
        /// correspond to nonexistent matrix elements can be ignored
        /// or loaded arbitrarily, as they are overwritten by DVODE.
        /// JAC need not provide df/dy exactly.  A crude
        /// approximation (possibly with a smaller bandwidth) will do.
        /// In either case, PD is preset to zero by the solver,
        /// so that only the nonzero elements need be loaded by JAC.
        /// Each call to JAC is preceded by a call to F with the same
        /// arguments NEQ, T, and Y.  Thus to gain some efficiency,
        /// intermediate quantities shared by both calculations may be
        /// saved in a user COMMON block by F and not recomputed by JAC,
        /// if desired.  Also, JAC may alter the Y array, if desired.
        /// JAC must be declared external in the calling program.
        /// Subroutine JAC may access user-defined real and integer
        /// work arrays, RPAR and IPAR, whose dimensions are set by the
        /// user in the main program.
        ///</param>
        /// <param name="MF">
        /// = The method flag.  Used only for input.  The legal values of
        /// MF are 10, 11, 12, 13, 14, 15, 20, 21, 22, 23, 24, 25,
        /// -11, -12, -14, -15, -21, -22, -24, -25.
        /// MF is a signed two-digit integer, MF = JSV*(10*METH + MITER).
        /// JSV = SIGN(MF) indicates the Jacobian-saving strategy:
        /// JSV =  1 means a copy of the Jacobian is saved for reuse
        /// in the corrector iteration algorithm.
        /// JSV = -1 means a copy of the Jacobian is not saved
        /// (valid only for MITER = 1, 2, 4, or 5).
        /// METH indicates the basic linear multistep method:
        /// METH = 1 means the implicit Adams method.
        /// METH = 2 means the method based on backward
        /// differentiation formulas (BDF-s).
        /// MITER indicates the corrector iteration method:
        /// MITER = 0 means functional iteration (no Jacobian matrix
        /// is involved).
        /// MITER = 1 means chord iteration with a user-supplied
        /// full (NEQ by NEQ) Jacobian.
        /// MITER = 2 means chord iteration with an internally
        /// generated (difference quotient) full Jacobian
        /// (using NEQ extra calls to F per df/dy value).
        /// MITER = 3 means chord iteration with an internally
        /// generated diagonal Jacobian approximation
        /// (using 1 extra call to F per df/dy evaluation).
        /// MITER = 4 means chord iteration with a user-supplied
        /// banded Jacobian.
        /// MITER = 5 means chord iteration with an internally
        /// generated banded Jacobian (using ML+MU+1 extra
        /// calls to F per df/dy evaluation).
        /// If MITER = 1 or 4, the user must supply a subroutine JAC
        /// (the name is arbitrary) as described above under JAC.
        /// For other values of MITER, a dummy argument can be used.
        ///</param>
        /// <param name="RPAR">
        /// User-specified array used to communicate real parameters
        /// to user-supplied subroutines.  If RPAR is a vector, then
        /// it must be dimensioned in the user's main program.  If it
        /// is unused or it is a scalar, then it need not be
        /// dimensioned.
        ///</param>
        /// <param name="IPAR">
        /// User-specified array used to communicate integer parameter
        /// to user-supplied subroutines.  The comments on dimensioning
        /// RPAR apply to IPAR.
        ///</param>
        public void Run(IFEX F, int NEQ, ref double[] Y, int offset_y, ref double T, double TOUT, int ITOL
                         , double[] RTOL, int offset_rtol, double[] ATOL, int offset_atol, int ITASK, ref int ISTATE, int IOPT, ref double[] RWORK, int offset_rwork
                         , int LRW, ref int[] IWORK, int offset_iwork, int LIW, IJEX JAC, int MF, double[] RPAR, int offset_rpar
                         , int[] IPAR, int offset_ipar)
        {
            #region Variables
            
            bool IHIT = false; double ATOLI = 0; double BIG = 0; double EWTI = 0; double H0 = 0; double HMAX = 0; double HMX = 0; 
            double RH = 0;double RTOLI = 0; double SIZE = 0; double TCRIT = 0; double TNEXT = 0; double TOLSF = 0; double TP = 0; 
            int I = 0;int IER = 0; int IFLAG = 0; int IMXER = 0; int JCO = 0; int KGO = 0; int LENIW = 0; int LENJ = 0; 
            int LENP = 0;int LENRW = 0; int LENWM = 0; int LF0 = 0; int MBAND = 0; int MFA = 0; int ML = 0; int MU = 0; 
            int NITER = 0;int NSLAST = 0; string MSG = new string(' ', 80); 
            #endregion
            #region Array Index Correction
            
             int o_y = -1 + offset_y;  int o_rtol = -1 + offset_rtol;  int o_atol = -1 + offset_atol; 
             int o_rwork = -1 + offset_rwork; int o_iwork = -1 + offset_iwork;  int o_rpar = -1 + offset_rpar; 
             int o_ipar = -1 + offset_ipar;
            #endregion
            #region Prolog
            
            // C-----------------------------------------------------------------------
            // C DVODE: Variable-coefficient Ordinary Differential Equation solver,
            // C with fixed-leading-coefficient implementation.
            // C This version is in double precision.
            // C
            // C DVODE solves the initial value problem for stiff or nonstiff
            // C systems of first order ODEs,
            // C     dy/dt = f(t,y) ,  or, in component form,
            // C     dy(i)/dt = f(i) = f(i,t,y(1),y(2),...,y(NEQ)) (i = 1,...,NEQ).
            // C DVODE is a package based on the EPISODE and EPISODEB packages, and
            // C on the ODEPACK user interface standard, with minor modifications.
            // C-----------------------------------------------------------------------
            // C Authors:
            // C               Peter N. Brown and Alan C. Hindmarsh
            // C               Center for Applied Scientific Computing, L-561
            // C               Lawrence Livermore National Laboratory
            // C               Livermore, CA 94551
            // C and
            // C               George D. Byrne
            // C               Illinois Institute of Technology
            // C               Chicago, IL 60616
            // C-----------------------------------------------------------------------
            // C References:
            // C
            // C 1. P. N. Brown, G. D. Byrne, and A. C. Hindmarsh, "VODE: A Variable
            // C    Coefficient ODE Solver," SIAM J. Sci. Stat. Comput., 10 (1989),
            // C    pp. 1038-1051.  Also, LLNL Report UCRL-98412, June 1988.
            // C 2. G. D. Byrne and A. C. Hindmarsh, "A Polyalgorithm for the
            // C    Numerical Solution of Ordinary Differential Equations,"
            // C    ACM Trans. Math. Software, 1 (1975), pp. 71-96.
            // C 3. A. C. Hindmarsh and G. D. Byrne, "EPISODE: An Effective Package
            // C    for the Integration of Systems of Ordinary Differential
            // C    Equations," LLNL Report UCID-30112, Rev. 1, April 1977.
            // C 4. G. D. Byrne and A. C. Hindmarsh, "EPISODEB: An Experimental
            // C    Package for the Integration of Systems of Ordinary Differential
            // C    Equations with Banded Jacobians," LLNL Report UCID-30132, April
            // C    1976.
            // C 5. A. C. Hindmarsh, "ODEPACK, a Systematized Collection of ODE
            // C    Solvers," in Scientific Computing, R. S. Stepleman et al., eds.,
            // C    North-Holland, Amsterdam, 1983, pp. 55-64.
            // C 6. K. R. Jackson and R. Sacks-Davis, "An Alternative Implementation
            // C    of Variable Step-Size Multistep Formulas for Stiff ODEs," ACM
            // C    Trans. Math. Software, 6 (1980), pp. 295-318.
            // C-----------------------------------------------------------------------
            // C Summary of usage.
            // C
            // C Communication between the user and the DVODE package, for normal
            // C situations, is summarized here.  This summary describes only a subset
            // C of the full set of options available.  See the full description for
            // C details, including optional communication, nonstandard options,
            // C and instructions for special situations.  See also the example
            // C problem (with program and output) following this summary.
            // C
            // C A. First provide a subroutine of the form:
            // C           SUBROUTINE F (NEQ, T, Y, YDOT, RPAR, IPAR)
            // C           DOUBLE PRECISION T, Y(NEQ), YDOT(NEQ), RPAR
            // C which supplies the vector function f by loading YDOT(i) with f(i).
            // C
            // C B. Next determine (or guess) whether or not the problem is stiff.
            // C Stiffness occurs when the Jacobian matrix df/dy has an eigenvalue
            // C whose real part is negative and large in magnitude, compared to the
            // C reciprocal of the t span of interest.  If the problem is nonstiff,
            // C use a method flag MF = 10.  If it is stiff, there are four standard
            // C choices for MF (21, 22, 24, 25), and DVODE requires the Jacobian
            // C matrix in some form.  In these cases (MF .gt. 0), DVODE will use a
            // C saved copy of the Jacobian matrix.  If this is undesirable because of
            // C storage limitations, set MF to the corresponding negative value
            // C (-21, -22, -24, -25).  (See full description of MF below.)
            // C The Jacobian matrix is regarded either as full (MF = 21 or 22),
            // C or banded (MF = 24 or 25).  In the banded case, DVODE requires two
            // C half-bandwidth parameters ML and MU.  These are, respectively, the
            // C widths of the lower and upper parts of the band, excluding the main
            // C diagonal.  Thus the band consists of the locations (i,j) with
            // C i-ML .le. j .le. i+MU, and the full bandwidth is ML+MU+1.
            // C
            // C C. If the problem is stiff, you are encouraged to supply the Jacobian
            // C directly (MF = 21 or 24), but if this is not feasible, DVODE will
            // C compute it internally by difference quotients (MF = 22 or 25).
            // C If you are supplying the Jacobian, provide a subroutine of the form:
            // C           SUBROUTINE JAC (NEQ, T, Y, ML, MU, PD, NROWPD, RPAR, IPAR)
            // C           DOUBLE PRECISION T, Y(NEQ), PD(NROWPD,NEQ), RPAR
            // C which supplies df/dy by loading PD as follows:
            // C     For a full Jacobian (MF = 21), load PD(i,j) with df(i)/dy(j),
            // C the partial derivative of f(i) with respect to y(j).  (Ignore the
            // C ML and MU arguments in this case.)
            // C     For a banded Jacobian (MF = 24), load PD(i-j+MU+1,j) with
            // C df(i)/dy(j), i.e. load the diagonal lines of df/dy into the rows of
            // C PD from the top down.
            // C     In either case, only nonzero elements need be loaded.
            // C
            // C D. Write a main program which calls subroutine DVODE once for
            // C each point at which answers are desired.  This should also provide
            // C for possible use of logical unit 6 for output of error messages
            // C by DVODE.  On the first call to DVODE, supply arguments as follows:
            // C F      = Name of subroutine for right-hand side vector f.
            // C          This name must be declared external in calling program.
            // C NEQ    = Number of first order ODEs.
            // C Y      = Array of initial values, of length NEQ.
            // C T      = The initial value of the independent variable.
            // C TOUT   = First point where output is desired (.ne. T).
            // C ITOL   = 1 or 2 according as ATOL (below) is a scalar or array.
            // C RTOL   = Relative tolerance parameter (scalar).
            // C ATOL   = Absolute tolerance parameter (scalar or array).
            // C          The estimated local error in Y(i) will be controlled so as
            // C          to be roughly less (in magnitude) than
            // C             EWT(i) = RTOL*abs(Y(i)) + ATOL     if ITOL = 1, or
            // C             EWT(i) = RTOL*abs(Y(i)) + ATOL(i)  if ITOL = 2.
            // C          Thus the local error test passes if, in each component,
            // C          either the absolute error is less than ATOL (or ATOL(i)),
            // C          or the relative error is less than RTOL.
            // C          Use RTOL = 0.0 for pure absolute error control, and
            // C          use ATOL = 0.0 (or ATOL(i) = 0.0) for pure relative error
            // C          control.  Caution: Actual (global) errors may exceed these
            // C          local tolerances, so choose them conservatively.
            // C ITASK  = 1 for normal computation of output values of Y at t = TOUT.
            // C ISTATE = Integer flag (input and output).  Set ISTATE = 1.
            // C IOPT   = 0 to indicate no optional input used.
            // C RWORK  = Real work array of length at least:
            // C             20 + 16*NEQ                      for MF = 10,
            // C             22 +  9*NEQ + 2*NEQ**2           for MF = 21 or 22,
            // C             22 + 11*NEQ + (3*ML + 2*MU)*NEQ  for MF = 24 or 25.
            // C LRW    = Declared length of RWORK (in user's DIMENSION statement).
            // C IWORK  = Integer work array of length at least:
            // C             30        for MF = 10,
            // C             30 + NEQ  for MF = 21, 22, 24, or 25.
            // C          If MF = 24 or 25, input in IWORK(1),IWORK(2) the lower
            // C          and upper half-bandwidths ML,MU.
            // C LIW    = Declared length of IWORK (in user's DIMENSION statement).
            // C JAC    = Name of subroutine for Jacobian matrix (MF = 21 or 24).
            // C          If used, this name must be declared external in calling
            // C          program.  If not used, pass a dummy name.
            // C MF     = Method flag.  Standard values are:
            // C          10 for nonstiff (Adams) method, no Jacobian used.
            // C          21 for stiff (BDF) method, user-supplied full Jacobian.
            // C          22 for stiff method, internally generated full Jacobian.
            // C          24 for stiff method, user-supplied banded Jacobian.
            // C          25 for stiff method, internally generated banded Jacobian.
            // C RPAR,IPAR = user-defined real and integer arrays passed to F and JAC.
            // C Note that the main program must declare arrays Y, RWORK, IWORK,
            // C and possibly ATOL, RPAR, and IPAR.
            // C
            // C E. The output from the first call (or any call) is:
            // C      Y = Array of computed values of y(t) vector.
            // C      T = Corresponding value of independent variable (normally TOUT).
            // C ISTATE = 2  if DVODE was successful, negative otherwise.
            // C          -1 means excess work done on this call. (Perhaps wrong MF.)
            // C          -2 means excess accuracy requested. (Tolerances too small.)
            // C          -3 means illegal input detected. (See printed message.)
            // C          -4 means repeated error test failures. (Check all input.)
            // C          -5 means repeated convergence failures. (Perhaps bad
            // C             Jacobian supplied or wrong choice of MF or tolerances.)
            // C          -6 means error weight became zero during problem. (Solution
            // C             component i vanished, and ATOL or ATOL(i) = 0.)
            // C
            // C F. To continue the integration after a successful return, simply
            // C reset TOUT and call DVODE again.  No other parameters need be reset.
            // C
            // C-----------------------------------------------------------------------
            // C EXAMPLE PROBLEM
            // C
            // C The following is a simple example problem, with the coding
            // C needed for its solution by DVODE.  The problem is from chemical
            // C kinetics, and consists of the following three rate equations:
            // C     dy1/dt = -.04*y1 + 1.e4*y2*y3
            // C     dy2/dt = .04*y1 - 1.e4*y2*y3 - 3.e7*y2**2
            // C     dy3/dt = 3.e7*y2**2
            // C on the interval from t = 0.0 to t = 4.e10, with initial conditions
            // C y1 = 1.0, y2 = y3 = 0.  The problem is stiff.
            // C
            // C The following coding solves this problem with DVODE, using MF = 21
            // C and printing results at t = .4, 4., ..., 4.e10.  It uses
            // C ITOL = 2 and ATOL much smaller for y2 than y1 or y3 because
            // C y2 has much smaller values.
            // C At the end of the run, statistical quantities of interest are
            // C printed. (See optional output in the full description below.)
            // C To generate Fortran source code, replace C in column 1 with a blank
            // C in the coding below.
            // C
            // C     EXTERNAL FEX, JEX
            // C     DOUBLE PRECISION ATOL, RPAR, RTOL, RWORK, T, TOUT, Y
            // C     DIMENSION Y(3), ATOL(3), RWORK(67), IWORK(33)
            // C     NEQ = 3
            // C     Y(1) = 1.0D0
            // C     Y(2) = 0.0D0
            // C     Y(3) = 0.0D0
            // C     T = 0.0D0
            // C     TOUT = 0.4D0
            // C     ITOL = 2
            // C     RTOL = 1.D-4
            // C     ATOL(1) = 1.D-8
            // C     ATOL(2) = 1.D-14
            // C     ATOL(3) = 1.D-6
            // C     ITASK = 1
            // C     ISTATE = 1
            // C     IOPT = 0
            // C     LRW = 67
            // C     LIW = 33
            // C     MF = 21
            // C     DO 40 IOUT = 1,12
            // C       CALL DVODE(FEX,NEQ,Y,T,TOUT,ITOL,RTOL,ATOL,ITASK,ISTATE,
            // C    1            IOPT,RWORK,LRW,IWORK,LIW,JEX,MF,RPAR,IPAR)
            // C       WRITE(6,20)T,Y(1),Y(2),Y(3)
            // C 20    FORMAT(' At t =',D12.4,'   y =',3D14.6)
            // C       IF (ISTATE .LT. 0) GO TO 80
            // C 40    TOUT = TOUT*10.
            // C     WRITE(6,60) IWORK(11),IWORK(12),IWORK(13),IWORK(19),
            // C    1            IWORK(20),IWORK(21),IWORK(22)
            // C 60  FORMAT(/' No. steps =',I4,'   No. f-s =',I4,
            // C    1       '   No. J-s =',I4,'   No. LU-s =',I4/
            // C    2       '  No. nonlinear iterations =',I4/
            // C    3       '  No. nonlinear convergence failures =',I4/
            // C    4       '  No. error test failures =',I4/)
            // C     STOP
            // C 80  WRITE(6,90)ISTATE
            // C 90  FORMAT(///' Error halt: ISTATE =',I3)
            // C     STOP
            // C     END
            // C
            // C     SUBROUTINE FEX (NEQ, T, Y, YDOT, RPAR, IPAR)
            // C     DOUBLE PRECISION RPAR, T, Y, YDOT
            // C     DIMENSION Y(NEQ), YDOT(NEQ)
            // C     YDOT(1) = -.04D0*Y(1) + 1.D4*Y(2)*Y(3)
            // C     YDOT(3) = 3.D7*Y(2)*Y(2)
            // C     YDOT(2) = -YDOT(1) - YDOT(3)
            // C     RETURN
            // C     END
            // C
            // C     SUBROUTINE JEX (NEQ, T, Y, ML, MU, PD, NRPD, RPAR, IPAR)
            // C     DOUBLE PRECISION PD, RPAR, T, Y
            // C     DIMENSION Y(NEQ), PD(NRPD,NEQ)
            // C     PD(1,1) = -.04D0
            // C     PD(1,2) = 1.D4*Y(3)
            // C     PD(1,3) = 1.D4*Y(2)
            // C     PD(2,1) = .04D0
            // C     PD(2,3) = -PD(1,3)
            // C     PD(3,2) = 6.D7*Y(2)
            // C     PD(2,2) = -PD(1,2) - PD(3,2)
            // C     RETURN
            // C     END
            // C
            // C The following output was obtained from the above program on a
            // C Cray-1 computer with the CFT compiler.
            // C
            // C At t =  4.0000e-01   y =  9.851680e-01  3.386314e-05  1.479817e-02
            // C At t =  4.0000e+00   y =  9.055255e-01  2.240539e-05  9.445214e-02
            // C At t =  4.0000e+01   y =  7.158108e-01  9.184883e-06  2.841800e-01
            // C At t =  4.0000e+02   y =  4.505032e-01  3.222940e-06  5.494936e-01
            // C At t =  4.0000e+03   y =  1.832053e-01  8.942690e-07  8.167938e-01
            // C At t =  4.0000e+04   y =  3.898560e-02  1.621875e-07  9.610142e-01
            // C At t =  4.0000e+05   y =  4.935882e-03  1.984013e-08  9.950641e-01
            // C At t =  4.0000e+06   y =  5.166183e-04  2.067528e-09  9.994834e-01
            // C At t =  4.0000e+07   y =  5.201214e-05  2.080593e-10  9.999480e-01
            // C At t =  4.0000e+08   y =  5.213149e-06  2.085271e-11  9.999948e-01
            // C At t =  4.0000e+09   y =  5.183495e-07  2.073399e-12  9.999995e-01
            // C At t =  4.0000e+10   y =  5.450996e-08  2.180399e-13  9.999999e-01
            // C
            // C No. steps = 595   No. f-s = 832   No. J-s =  13   No. LU-s = 112
            // C  No. nonlinear iterations = 831
            // C  No. nonlinear convergence failures =   0
            // C  No. error test failures =  22
            // C-----------------------------------------------------------------------
            // C Full description of user interface to DVODE.
            // C
            // C The user interface to DVODE consists of the following parts.
            // C
            // C i.   The call sequence to subroutine DVODE, which is a driver
            // C      routine for the solver.  This includes descriptions of both
            // C      the call sequence arguments and of user-supplied routines.
            // C      Following these descriptions is
            // C        * a description of optional input available through the
            // C          call sequence,
            // C        * a description of optional output (in the work arrays), and
            // C        * instructions for interrupting and restarting a solution.
            // C
            // C ii.  Descriptions of other routines in the DVODE package that may be
            // C      (optionally) called by the user.  These provide the ability to
            // C      alter error message handling, save and restore the internal
            // C      COMMON, and obtain specified derivatives of the solution y(t).
            // C
            // C iii. Descriptions of COMMON blocks to be declared in overlay
            // C      or similar environments.
            // C
            // C iv.  Description of two routines in the DVODE package, either of
            // C      which the user may replace with his own version, if desired.
            // C      these relate to the measurement of errors.
            // C
            // C-----------------------------------------------------------------------
            // C Part i.  Call Sequence.
            // C
            // C The call sequence parameters used for input only are
            // C     F, NEQ, TOUT, ITOL, RTOL, ATOL, ITASK, IOPT, LRW, LIW, JAC, MF,
            // C and those used for both input and output are
            // C     Y, T, ISTATE.
            // C The work arrays RWORK and IWORK are also used for conditional and
            // C optional input and optional output.  (The term output here refers
            // C to the return from subroutine DVODE to the user's calling program.)
            // C
            // C The legality of input parameters will be thoroughly checked on the
            // C initial call for the problem, but not checked thereafter unless a
            // C change in input parameters is flagged by ISTATE = 3 in the input.
            // C
            // C The descriptions of the call arguments are as follows.
            // C
            // C F      = The name of the user-supplied subroutine defining the
            // C          ODE system.  The system must be put in the first-order
            // C          form dy/dt = f(t,y), where f is a vector-valued function
            // C          of the scalar t and the vector y.  Subroutine F is to
            // C          compute the function f.  It is to have the form
            // C               SUBROUTINE F (NEQ, T, Y, YDOT, RPAR, IPAR)
            // C               DOUBLE PRECISION T, Y(NEQ), YDOT(NEQ), RPAR
            // C          where NEQ, T, and Y are input, and the array YDOT = f(t,y)
            // C          is output.  Y and YDOT are arrays of length NEQ.
            // C          Subroutine F should not alter Y(1),...,Y(NEQ).
            // C          F must be declared EXTERNAL in the calling program.
            // C
            // C          Subroutine F may access user-defined real and integer
            // C          work arrays RPAR and IPAR, which are to be dimensioned
            // C          in the main program.
            // C
            // C          If quantities computed in the F routine are needed
            // C          externally to DVODE, an extra call to F should be made
            // C          for this purpose, for consistent and accurate results.
            // C          If only the derivative dy/dt is needed, use DVINDY instead.
            // C
            // C NEQ    = The size of the ODE system (number of first order
            // C          ordinary differential equations).  Used only for input.
            // C          NEQ may not be increased during the problem, but
            // C          can be decreased (with ISTATE = 3 in the input).
            // C
            // C Y      = A real array for the vector of dependent variables, of
            // C          length NEQ or more.  Used for both input and output on the
            // C          first call (ISTATE = 1), and only for output on other calls.
            // C          On the first call, Y must contain the vector of initial
            // C          values.  In the output, Y contains the computed solution
            // C          evaluated at T.  If desired, the Y array may be used
            // C          for other purposes between calls to the solver.
            // C
            // C          This array is passed as the Y argument in all calls to
            // C          F and JAC.
            // C
            // C T      = The independent variable.  In the input, T is used only on
            // C          the first call, as the initial point of the integration.
            // C          In the output, after each call, T is the value at which a
            // C          computed solution Y is evaluated (usually the same as TOUT).
            // C          On an error return, T is the farthest point reached.
            // C
            // C TOUT   = The next value of t at which a computed solution is desired.
            // C          Used only for input.
            // C
            // C          When starting the problem (ISTATE = 1), TOUT may be equal
            // C          to T for one call, then should .ne. T for the next call.
            // C          For the initial T, an input value of TOUT .ne. T is used
            // C          in order to determine the direction of the integration
            // C          (i.e. the algebraic sign of the step sizes) and the rough
            // C          scale of the problem.  Integration in either direction
            // C          (forward or backward in t) is permitted.
            // C
            // C          If ITASK = 2 or 5 (one-step modes), TOUT is ignored after
            // C          the first call (i.e. the first call with TOUT .ne. T).
            // C          Otherwise, TOUT is required on every call.
            // C
            // C          If ITASK = 1, 3, or 4, the values of TOUT need not be
            // C          monotone, but a value of TOUT which backs up is limited
            // C          to the current internal t interval, whose endpoints are
            // C          TCUR - HU and TCUR.  (See optional output, below, for
            // C          TCUR and HU.)
            // C
            // C ITOL   = An indicator for the type of error control.  See
            // C          description below under ATOL.  Used only for input.
            // C
            // C RTOL   = A relative error tolerance parameter, either a scalar or
            // C          an array of length NEQ.  See description below under ATOL.
            // C          Input only.
            // C
            // C ATOL   = An absolute error tolerance parameter, either a scalar or
            // C          an array of length NEQ.  Input only.
            // C
            // C          The input parameters ITOL, RTOL, and ATOL determine
            // C          the error control performed by the solver.  The solver will
            // C          control the vector e = (e(i)) of estimated local errors
            // C          in Y, according to an inequality of the form
            // C                      rms-norm of ( e(i)/EWT(i) )   .le.   1,
            // C          where       EWT(i) = RTOL(i)*abs(Y(i)) + ATOL(i),
            // C          and the rms-norm (root-mean-square norm) here is
            // C          rms-norm(v) = sqrt(sum v(i)**2 / NEQ).  Here EWT = (EWT(i))
            // C          is a vector of weights which must always be positive, and
            // C          the values of RTOL and ATOL should all be non-negative.
            // C          The following table gives the types (scalar/array) of
            // C          RTOL and ATOL, and the corresponding form of EWT(i).
            // C
            // C             ITOL    RTOL       ATOL          EWT(i)
            // C              1     scalar     scalar     RTOL*ABS(Y(i)) + ATOL
            // C              2     scalar     array      RTOL*ABS(Y(i)) + ATOL(i)
            // C              3     array      scalar     RTOL(i)*ABS(Y(i)) + ATOL
            // C              4     array      array      RTOL(i)*ABS(Y(i)) + ATOL(i)
            // C
            // C          When either of these parameters is a scalar, it need not
            // C          be dimensioned in the user's calling program.
            // C
            // C          If none of the above choices (with ITOL, RTOL, and ATOL
            // C          fixed throughout the problem) is suitable, more general
            // C          error controls can be obtained by substituting
            // C          user-supplied routines for the setting of EWT and/or for
            // C          the norm calculation.  See Part iv below.
            // C
            // C          If global errors are to be estimated by making a repeated
            // C          run on the same problem with smaller tolerances, then all
            // C          components of RTOL and ATOL (i.e. of EWT) should be scaled
            // C          down uniformly.
            // C
            // C ITASK  = An index specifying the task to be performed.
            // C          Input only.  ITASK has the following values and meanings.
            // C          1  means normal computation of output values of y(t) at
            // C             t = TOUT (by overshooting and interpolating).
            // C          2  means take one step only and return.
            // C          3  means stop at the first internal mesh point at or
            // C             beyond t = TOUT and return.
            // C          4  means normal computation of output values of y(t) at
            // C             t = TOUT but without overshooting t = TCRIT.
            // C             TCRIT must be input as RWORK(1).  TCRIT may be equal to
            // C             or beyond TOUT, but not behind it in the direction of
            // C             integration.  This option is useful if the problem
            // C             has a singularity at or beyond t = TCRIT.
            // C          5  means take one step, without passing TCRIT, and return.
            // C             TCRIT must be input as RWORK(1).
            // C
            // C          Note:  If ITASK = 4 or 5 and the solver reaches TCRIT
            // C          (within roundoff), it will return T = TCRIT (exactly) to
            // C          indicate this (unless ITASK = 4 and TOUT comes before TCRIT,
            // C          in which case answers at T = TOUT are returned first).
            // C
            // C ISTATE = an index used for input and output to specify the
            // C          the state of the calculation.
            // C
            // C          In the input, the values of ISTATE are as follows.
            // C          1  means this is the first call for the problem
            // C             (initializations will be done).  See note below.
            // C          2  means this is not the first call, and the calculation
            // C             is to continue normally, with no change in any input
            // C             parameters except possibly TOUT and ITASK.
            // C             (If ITOL, RTOL, and/or ATOL are changed between calls
            // C             with ISTATE = 2, the new values will be used but not
            // C             tested for legality.)
            // C          3  means this is not the first call, and the
            // C             calculation is to continue normally, but with
            // C             a change in input parameters other than
            // C             TOUT and ITASK.  Changes are allowed in
            // C             NEQ, ITOL, RTOL, ATOL, IOPT, LRW, LIW, MF, ML, MU,
            // C             and any of the optional input except H0.
            // C             (See IWORK description for ML and MU.)
            // C          Note:  A preliminary call with TOUT = T is not counted
            // C          as a first call here, as no initialization or checking of
            // C          input is done.  (Such a call is sometimes useful to include
            // C          the initial conditions in the output.)
            // C          Thus the first call for which TOUT .ne. T requires
            // C          ISTATE = 1 in the input.
            // C
            // C          In the output, ISTATE has the following values and meanings.
            // C           1  means nothing was done, as TOUT was equal to T with
            // C              ISTATE = 1 in the input.
            // C           2  means the integration was performed successfully.
            // C          -1  means an excessive amount of work (more than MXSTEP
            // C              steps) was done on this call, before completing the
            // C              requested task, but the integration was otherwise
            // C              successful as far as T.  (MXSTEP is an optional input
            // C              and is normally 500.)  To continue, the user may
            // C              simply reset ISTATE to a value .gt. 1 and call again.
            // C              (The excess work step counter will be reset to 0.)
            // C              In addition, the user may increase MXSTEP to avoid
            // C              this error return.  (See optional input below.)
            // C          -2  means too much accuracy was requested for the precision
            // C              of the machine being used.  This was detected before
            // C              completing the requested task, but the integration
            // C              was successful as far as T.  To continue, the tolerance
            // C              parameters must be reset, and ISTATE must be set
            // C              to 3.  The optional output TOLSF may be used for this
            // C              purpose.  (Note: If this condition is detected before
            // C              taking any steps, then an illegal input return
            // C              (ISTATE = -3) occurs instead.)
            // C          -3  means illegal input was detected, before taking any
            // C              integration steps.  See written message for details.
            // C              Note:  If the solver detects an infinite loop of calls
            // C              to the solver with illegal input, it will cause
            // C              the run to stop.
            // C          -4  means there were repeated error test failures on
            // C              one attempted step, before completing the requested
            // C              task, but the integration was successful as far as T.
            // C              The problem may have a singularity, or the input
            // C              may be inappropriate.
            // C          -5  means there were repeated convergence test failures on
            // C              one attempted step, before completing the requested
            // C              task, but the integration was successful as far as T.
            // C              This may be caused by an inaccurate Jacobian matrix,
            // C              if one is being used.
            // C          -6  means EWT(i) became zero for some i during the
            // C              integration.  Pure relative error control (ATOL(i)=0.0)
            // C              was requested on a variable which has now vanished.
            // C              The integration was successful as far as T.
            // C
            // C          Note:  Since the normal output value of ISTATE is 2,
            // C          it does not need to be reset for normal continuation.
            // C          Also, since a negative input value of ISTATE will be
            // C          regarded as illegal, a negative output value requires the
            // C          user to change it, and possibly other input, before
            // C          calling the solver again.
            // C
            // C IOPT   = An integer flag to specify whether or not any optional
            // C          input is being used on this call.  Input only.
            // C          The optional input is listed separately below.
            // C          IOPT = 0 means no optional input is being used.
            // C                   Default values will be used in all cases.
            // C          IOPT = 1 means optional input is being used.
            // C
            // C RWORK  = A real working array (double precision).
            // C          The length of RWORK must be at least
            // C             20 + NYH*(MAXORD + 1) + 3*NEQ + LWM    where
            // C          NYH    = the initial value of NEQ,
            // C          MAXORD = 12 (if METH = 1) or 5 (if METH = 2) (unless a
            // C                   smaller value is given as an optional input),
            // C          LWM = length of work space for matrix-related data:
            // C          LWM = 0             if MITER = 0,
            // C          LWM = 2*NEQ**2 + 2  if MITER = 1 or 2, and MF.gt.0,
            // C          LWM = NEQ**2 + 2    if MITER = 1 or 2, and MF.lt.0,
            // C          LWM = NEQ + 2       if MITER = 3,
            // C          LWM = (3*ML+2*MU+2)*NEQ + 2 if MITER = 4 or 5, and MF.gt.0,
            // C          LWM = (2*ML+MU+1)*NEQ + 2   if MITER = 4 or 5, and MF.lt.0.
            // C          (See the MF description for METH and MITER.)
            // C          Thus if MAXORD has its default value and NEQ is constant,
            // C          this length is:
            // C             20 + 16*NEQ                    for MF = 10,
            // C             22 + 16*NEQ + 2*NEQ**2         for MF = 11 or 12,
            // C             22 + 16*NEQ + NEQ**2           for MF = -11 or -12,
            // C             22 + 17*NEQ                    for MF = 13,
            // C             22 + 18*NEQ + (3*ML+2*MU)*NEQ  for MF = 14 or 15,
            // C             22 + 17*NEQ + (2*ML+MU)*NEQ    for MF = -14 or -15,
            // C             20 +  9*NEQ                    for MF = 20,
            // C             22 +  9*NEQ + 2*NEQ**2         for MF = 21 or 22,
            // C             22 +  9*NEQ + NEQ**2           for MF = -21 or -22,
            // C             22 + 10*NEQ                    for MF = 23,
            // C             22 + 11*NEQ + (3*ML+2*MU)*NEQ  for MF = 24 or 25.
            // C             22 + 10*NEQ + (2*ML+MU)*NEQ    for MF = -24 or -25.
            // C          The first 20 words of RWORK are reserved for conditional
            // C          and optional input and optional output.
            // C
            // C          The following word in RWORK is a conditional input:
            // C            RWORK(1) = TCRIT = critical value of t which the solver
            // C                       is not to overshoot.  Required if ITASK is
            // C                       4 or 5, and ignored otherwise.  (See ITASK.)
            // C
            // C LRW    = The length of the array RWORK, as declared by the user.
            // C          (This will be checked by the solver.)
            // C
            // C IWORK  = An integer work array.  The length of IWORK must be at least
            // C             30        if MITER = 0 or 3 (MF = 10, 13, 20, 23), or
            // C             30 + NEQ  otherwise (abs(MF) = 11,12,14,15,21,22,24,25).
            // C          The first 30 words of IWORK are reserved for conditional and
            // C          optional input and optional output.
            // C
            // C          The following 2 words in IWORK are conditional input:
            // C            IWORK(1) = ML     These are the lower and upper
            // C            IWORK(2) = MU     half-bandwidths, respectively, of the
            // C                       banded Jacobian, excluding the main diagonal.
            // C                       The band is defined by the matrix locations
            // C                       (i,j) with i-ML .le. j .le. i+MU.  ML and MU
            // C                       must satisfy  0 .le.  ML,MU  .le. NEQ-1.
            // C                       These are required if MITER is 4 or 5, and
            // C                       ignored otherwise.  ML and MU may in fact be
            // C                       the band parameters for a matrix to which
            // C                       df/dy is only approximately equal.
            // C
            // C LIW    = the length of the array IWORK, as declared by the user.
            // C          (This will be checked by the solver.)
            // C
            // C Note:  The work arrays must not be altered between calls to DVODE
            // C for the same problem, except possibly for the conditional and
            // C optional input, and except for the last 3*NEQ words of RWORK.
            // C The latter space is used for internal scratch space, and so is
            // C available for use by the user outside DVODE between calls, if
            // C desired (but not for use by F or JAC).
            // C
            // C JAC    = The name of the user-supplied routine (MITER = 1 or 4) to
            // C          compute the Jacobian matrix, df/dy, as a function of
            // C          the scalar t and the vector y.  It is to have the form
            // C               SUBROUTINE JAC (NEQ, T, Y, ML, MU, PD, NROWPD,
            // C                               RPAR, IPAR)
            // C               DOUBLE PRECISION T, Y(NEQ), PD(NROWPD,NEQ), RPAR
            // C          where NEQ, T, Y, ML, MU, and NROWPD are input and the array
            // C          PD is to be loaded with partial derivatives (elements of the
            // C          Jacobian matrix) in the output.  PD must be given a first
            // C          dimension of NROWPD.  T and Y have the same meaning as in
            // C          Subroutine F.
            // C               In the full matrix case (MITER = 1), ML and MU are
            // C          ignored, and the Jacobian is to be loaded into PD in
            // C          columnwise manner, with df(i)/dy(j) loaded into PD(i,j).
            // C               In the band matrix case (MITER = 4), the elements
            // C          within the band are to be loaded into PD in columnwise
            // C          manner, with diagonal lines of df/dy loaded into the rows
            // C          of PD. Thus df(i)/dy(j) is to be loaded into PD(i-j+MU+1,j).
            // C          ML and MU are the half-bandwidth parameters. (See IWORK).
            // C          The locations in PD in the two triangular areas which
            // C          correspond to nonexistent matrix elements can be ignored
            // C          or loaded arbitrarily, as they are overwritten by DVODE.
            // C               JAC need not provide df/dy exactly.  A crude
            // C          approximation (possibly with a smaller bandwidth) will do.
            // C               In either case, PD is preset to zero by the solver,
            // C          so that only the nonzero elements need be loaded by JAC.
            // C          Each call to JAC is preceded by a call to F with the same
            // C          arguments NEQ, T, and Y.  Thus to gain some efficiency,
            // C          intermediate quantities shared by both calculations may be
            // C          saved in a user COMMON block by F and not recomputed by JAC,
            // C          if desired.  Also, JAC may alter the Y array, if desired.
            // C          JAC must be declared external in the calling program.
            // C               Subroutine JAC may access user-defined real and integer
            // C          work arrays, RPAR and IPAR, whose dimensions are set by the
            // C          user in the main program.
            // C
            // C MF     = The method flag.  Used only for input.  The legal values of
            // C          MF are 10, 11, 12, 13, 14, 15, 20, 21, 22, 23, 24, 25,
            // C          -11, -12, -14, -15, -21, -22, -24, -25.
            // C          MF is a signed two-digit integer, MF = JSV*(10*METH + MITER).
            // C          JSV = SIGN(MF) indicates the Jacobian-saving strategy:
            // C            JSV =  1 means a copy of the Jacobian is saved for reuse
            // C                     in the corrector iteration algorithm.
            // C            JSV = -1 means a copy of the Jacobian is not saved
            // C                     (valid only for MITER = 1, 2, 4, or 5).
            // C          METH indicates the basic linear multistep method:
            // C            METH = 1 means the implicit Adams method.
            // C            METH = 2 means the method based on backward
            // C                     differentiation formulas (BDF-s).
            // C          MITER indicates the corrector iteration method:
            // C            MITER = 0 means functional iteration (no Jacobian matrix
            // C                      is involved).
            // C            MITER = 1 means chord iteration with a user-supplied
            // C                      full (NEQ by NEQ) Jacobian.
            // C            MITER = 2 means chord iteration with an internally
            // C                      generated (difference quotient) full Jacobian
            // C                      (using NEQ extra calls to F per df/dy value).
            // C            MITER = 3 means chord iteration with an internally
            // C                      generated diagonal Jacobian approximation
            // C                      (using 1 extra call to F per df/dy evaluation).
            // C            MITER = 4 means chord iteration with a user-supplied
            // C                      banded Jacobian.
            // C            MITER = 5 means chord iteration with an internally
            // C                      generated banded Jacobian (using ML+MU+1 extra
            // C                      calls to F per df/dy evaluation).
            // C          If MITER = 1 or 4, the user must supply a subroutine JAC
            // C          (the name is arbitrary) as described above under JAC.
            // C          For other values of MITER, a dummy argument can be used.
            // C
            // C RPAR     User-specified array used to communicate real parameters
            // C          to user-supplied subroutines.  If RPAR is a vector, then
            // C          it must be dimensioned in the user's main program.  If it
            // C          is unused or it is a scalar, then it need not be
            // C          dimensioned.
            // C
            // C IPAR     User-specified array used to communicate integer parameter
            // C          to user-supplied subroutines.  The comments on dimensioning
            // C          RPAR apply to IPAR.
            // C-----------------------------------------------------------------------
            // C Optional Input.
            // C
            // C The following is a list of the optional input provided for in the
            // C call sequence.  (See also Part ii.)  For each such input variable,
            // C this table lists its name as used in this documentation, its
            // C location in the call sequence, its meaning, and the default value.
            // C The use of any of this input requires IOPT = 1, and in that
            // C case all of this input is examined.  A value of zero for any
            // C of these optional input variables will cause the default value to be
            // C used.  Thus to use a subset of the optional input, simply preload
            // C locations 5 to 10 in RWORK and IWORK to 0.0 and 0 respectively, and
            // C then set those of interest to nonzero values.
            // C
            // C NAME    LOCATION      MEANING AND DEFAULT VALUE
            // C
            // C H0      RWORK(5)  The step size to be attempted on the first step.
            // C                   The default value is determined by the solver.
            // C
            // C HMAX    RWORK(6)  The maximum absolute step size allowed.
            // C                   The default value is infinite.
            // C
            // C HMIN    RWORK(7)  The minimum absolute step size allowed.
            // C                   The default value is 0.  (This lower bound is not
            // C                   enforced on the final step before reaching TCRIT
            // C                   when ITASK = 4 or 5.)
            // C
            // C MAXORD  IWORK(5)  The maximum order to be allowed.  The default
            // C                   value is 12 if METH = 1, and 5 if METH = 2.
            // C                   If MAXORD exceeds the default value, it will
            // C                   be reduced to the default value.
            // C                   If MAXORD is changed during the problem, it may
            // C                   cause the current order to be reduced.
            // C
            // C MXSTEP  IWORK(6)  Maximum number of (internally defined) steps
            // C                   allowed during one call to the solver.
            // C                   The default value is 500.
            // C
            // C MXHNIL  IWORK(7)  Maximum number of messages printed (per problem)
            // C                   warning that T + H = T on a step (H = step size).
            // C                   This must be positive to result in a non-default
            // C                   value.  The default value is 10.
            // C
            // C-----------------------------------------------------------------------
            // C Optional Output.
            // C
            // C As optional additional output from DVODE, the variables listed
            // C below are quantities related to the performance of DVODE
            // C which are available to the user.  These are communicated by way of
            // C the work arrays, but also have internal mnemonic names as shown.
            // C Except where stated otherwise, all of this output is defined
            // C on any successful return from DVODE, and on any return with
            // C ISTATE = -1, -2, -4, -5, or -6.  On an illegal input return
            // C (ISTATE = -3), they will be unchanged from their existing values
            // C (if any), except possibly for TOLSF, LENRW, and LENIW.
            // C On any error return, output relevant to the error will be defined,
            // C as noted below.
            // C
            // C NAME    LOCATION      MEANING
            // C
            // C HU      RWORK(11) The step size in t last used (successfully).
            // C
            // C HCUR    RWORK(12) The step size to be attempted on the next step.
            // C
            // C TCUR    RWORK(13) The current value of the independent variable
            // C                   which the solver has actually reached, i.e. the
            // C                   current internal mesh point in t.  In the output,
            // C                   TCUR will always be at least as far from the
            // C                   initial value of t as the current argument T,
            // C                   but may be farther (if interpolation was done).
            // C
            // C TOLSF   RWORK(14) A tolerance scale factor, greater than 1.0,
            // C                   computed when a request for too much accuracy was
            // C                   detected (ISTATE = -3 if detected at the start of
            // C                   the problem, ISTATE = -2 otherwise).  If ITOL is
            // C                   left unaltered but RTOL and ATOL are uniformly
            // C                   scaled up by a factor of TOLSF for the next call,
            // C                   then the solver is deemed likely to succeed.
            // C                   (The user may also ignore TOLSF and alter the
            // C                   tolerance parameters in any other way appropriate.)
            // C
            // C NST     IWORK(11) The number of steps taken for the problem so far.
            // C
            // C NFE     IWORK(12) The number of f evaluations for the problem so far.
            // C
            // C NJE     IWORK(13) The number of Jacobian evaluations so far.
            // C
            // C NQU     IWORK(14) The method order last used (successfully).
            // C
            // C NQCUR   IWORK(15) The order to be attempted on the next step.
            // C
            // C IMXER   IWORK(16) The index of the component of largest magnitude in
            // C                   the weighted local error vector ( e(i)/EWT(i) ),
            // C                   on an error return with ISTATE = -4 or -5.
            // C
            // C LENRW   IWORK(17) The length of RWORK actually required.
            // C                   This is defined on normal returns and on an illegal
            // C                   input return for insufficient storage.
            // C
            // C LENIW   IWORK(18) The length of IWORK actually required.
            // C                   This is defined on normal returns and on an illegal
            // C                   input return for insufficient storage.
            // C
            // C NLU     IWORK(19) The number of matrix LU decompositions so far.
            // C
            // C NNI     IWORK(20) The number of nonlinear (Newton) iterations so far.
            // C
            // C NCFN    IWORK(21) The number of convergence failures of the nonlinear
            // C                   solver so far.
            // C
            // C NETF    IWORK(22) The number of error test failures of the integrator
            // C                   so far.
            // C
            // C The following two arrays are segments of the RWORK array which
            // C may also be of interest to the user as optional output.
            // C For each array, the table below gives its internal name,
            // C its base address in RWORK, and its description.
            // C
            // C NAME    BASE ADDRESS      DESCRIPTION
            // C
            // C YH      21             The Nordsieck history array, of size NYH by
            // C                        (NQCUR + 1), where NYH is the initial value
            // C                        of NEQ.  For j = 0,1,...,NQCUR, column j+1
            // C                        of YH contains HCUR**j/factorial(j) times
            // C                        the j-th derivative of the interpolating
            // C                        polynomial currently representing the
            // C                        solution, evaluated at t = TCUR.
            // C
            // C ACOR     LENRW-NEQ+1   Array of size NEQ used for the accumulated
            // C                        corrections on each step, scaled in the output
            // C                        to represent the estimated local error in Y
            // C                        on the last step.  This is the vector e in
            // C                        the description of the error control.  It is
            // C                        defined only on a successful return from DVODE.
            // C
            // C-----------------------------------------------------------------------
            // C Interrupting and Restarting
            // C
            // C If the integration of a given problem by DVODE is to be
            // C interrrupted and then later continued, such as when restarting
            // C an interrupted run or alternating between two or more ODE problems,
            // C the user should save, following the return from the last DVODE call
            // C prior to the interruption, the contents of the call sequence
            // C variables and internal COMMON blocks, and later restore these
            // C values before the next DVODE call for that problem.  To save
            // C and restore the COMMON blocks, use subroutine DVSRCO, as
            // C described below in part ii.
            // C
            // C In addition, if non-default values for either LUN or MFLAG are
            // C desired, an extra call to XSETUN and/or XSETF should be made just
            // C before continuing the integration.  See Part ii below for details.
            // C
            // C-----------------------------------------------------------------------
            // C Part ii.  Other Routines Callable.
            // C
            // C The following are optional calls which the user may make to
            // C gain additional capabilities in conjunction with DVODE.
            // C (The routines XSETUN and XSETF are designed to conform to the
            // C SLATEC error handling package.)
            // C
            // C     FORM OF CALL                  FUNCTION
            // C  CALL XSETUN(LUN)           Set the logical unit number, LUN, for
            // C                             output of messages from DVODE, if
            // C                             the default is not desired.
            // C                             The default value of LUN is 6.
            // C
            // C  CALL XSETF(MFLAG)          Set a flag to control the printing of
            // C                             messages by DVODE.
            // C                             MFLAG = 0 means do not print. (Danger:
            // C                             This risks losing valuable information.)
            // C                             MFLAG = 1 means print (the default).
            // C
            // C                             Either of the above calls may be made at
            // C                             any time and will take effect immediately.
            // C
            // C  CALL DVSRCO(RSAV,ISAV,JOB) Saves and restores the contents of
            // C                             the internal COMMON blocks used by
            // C                             DVODE. (See Part iii below.)
            // C                             RSAV must be a real array of length 49
            // C                             or more, and ISAV must be an integer
            // C                             array of length 40 or more.
            // C                             JOB=1 means save COMMON into RSAV/ISAV.
            // C                             JOB=2 means restore COMMON from RSAV/ISAV.
            // C                                DVSRCO is useful if one is
            // C                             interrupting a run and restarting
            // C                             later, or alternating between two or
            // C                             more problems solved with DVODE.
            // C
            // C  CALL DVINDY(,,,,,)         Provide derivatives of y, of various
            // C        (See below.)         orders, at a specified point T, if
            // C                             desired.  It may be called only after
            // C                             a successful return from DVODE.
            // C
            // C The detailed instructions for using DVINDY are as follows.
            // C The form of the call is:
            // C
            // C  CALL DVINDY (T, K, RWORK(21), NYH, DKY, IFLAG)
            // C
            // C The input parameters are:
            // C
            // C T         = Value of independent variable where answers are desired
            // C             (normally the same as the T last returned by DVODE).
            // C             For valid results, T must lie between TCUR - HU and TCUR.
            // C             (See optional output for TCUR and HU.)
            // C K         = Integer order of the derivative desired.  K must satisfy
            // C             0 .le. K .le. NQCUR, where NQCUR is the current order
            // C             (see optional output).  The capability corresponding
            // C             to K = 0, i.e. computing y(T), is already provided
            // C             by DVODE directly.  Since NQCUR .ge. 1, the first
            // C             derivative dy/dt is always available with DVINDY.
            // C RWORK(21) = The base address of the history array YH.
            // C NYH       = Column length of YH, equal to the initial value of NEQ.
            // C
            // C The output parameters are:
            // C
            // C DKY       = A real array of length NEQ containing the computed value
            // C             of the K-th derivative of y(t).
            // C IFLAG     = Integer flag, returned as 0 if K and T were legal,
            // C             -1 if K was illegal, and -2 if T was illegal.
            // C             On an error return, a message is also written.
            // C-----------------------------------------------------------------------
            // C Part iii.  COMMON Blocks.
            // C If DVODE is to be used in an overlay situation, the user
            // C must declare, in the primary overlay, the variables in:
            // C   (1) the call sequence to DVODE,
            // C   (2) the two internal COMMON blocks
            // C         /DVOD01/  of length  81  (48 double precision words
            // C                         followed by 33 integer words),
            // C         /DVOD02/  of length  9  (1 double precision word
            // C                         followed by 8 integer words),
            // C
            // C If DVODE is used on a system in which the contents of internal
            // C COMMON blocks are not preserved between calls, the user should
            // C declare the above two COMMON blocks in his main program to insure
            // C that their contents are preserved.
            // C
            // C-----------------------------------------------------------------------
            // C Part iv.  Optionally Replaceable Solver Routines.
            // C
            // C Below are descriptions of two routines in the DVODE package which
            // C relate to the measurement of errors.  Either routine can be
            // C replaced by a user-supplied version, if desired.  However, since such
            // C a replacement may have a major impact on performance, it should be
            // C done only when absolutely necessary, and only with great caution.
            // C (Note: The means by which the package version of a routine is
            // C superseded by the user's version may be system-dependent.)
            // C
            // C (a) DEWSET.
            // C The following subroutine is called just before each internal
            // C integration step, and sets the array of error weights, EWT, as
            // C described under ITOL/RTOL/ATOL above:
            // C     SUBROUTINE DEWSET (NEQ, ITOL, RTOL, ATOL, YCUR, EWT)
            // C where NEQ, ITOL, RTOL, and ATOL are as in the DVODE call sequence,
            // C YCUR contains the current dependent variable vector, and
            // C EWT is the array of weights set by DEWSET.
            // C
            // C If the user supplies this subroutine, it must return in EWT(i)
            // C (i = 1,...,NEQ) a positive quantity suitable for comparison with
            // C errors in Y(i).  The EWT array returned by DEWSET is passed to the
            // C DVNORM routine (See below.), and also used by DVODE in the computation
            // C of the optional output IMXER, the diagonal Jacobian approximation,
            // C and the increments for difference quotient Jacobians.
            // C
            // C In the user-supplied version of DEWSET, it may be desirable to use
            // C the current values of derivatives of y.  Derivatives up to order NQ
            // C are available from the history array YH, described above under
            // C Optional Output.  In DEWSET, YH is identical to the YCUR array,
            // C extended to NQ + 1 columns with a column length of NYH and scale
            // C factors of h**j/factorial(j).  On the first call for the problem,
            // C given by NST = 0, NQ is 1 and H is temporarily set to 1.0.
            // C NYH is the initial value of NEQ.  The quantities NQ, H, and NST
            // C can be obtained by including in DEWSET the statements:
            // C     DOUBLE PRECISION RVOD, H, HU
            // C     COMMON /DVOD01/ RVOD(48), IVOD(33)
            // C     COMMON /DVOD02/ HU, NCFN, NETF, NFE, NJE, NLU, NNI, NQU, NST
            // C     NQ = IVOD(28)
            // C     H = RVOD(21)
            // C Thus, for example, the current value of dy/dt can be obtained as
            // C YCUR(NYH+i)/H  (i=1,...,NEQ)  (and the division by H is
            // C unnecessary when NST = 0).
            // C
            // C (b) DVNORM.
            // C The following is a real function routine which computes the weighted
            // C root-mean-square norm of a vector v:
            // C     D = DVNORM (N, V, W)
            // C where:
            // C   N = the length of the vector,
            // C   V = real array of length N containing the vector,
            // C   W = real array of length N containing weights,
            // C   D = sqrt( (1/N) * sum(V(i)*W(i))**2 ).
            // C DVNORM is called with N = NEQ and with W(i) = 1.0/EWT(i), where
            // C EWT is as set by subroutine DEWSET.
            // C
            // C If the user supplies this function, it should return a non-negative
            // C value of DVNORM suitable for use in the error control in DVODE.
            // C None of the arguments should be altered by DVNORM.
            // C For example, a user-supplied DVNORM routine might:
            // C   -substitute a max-norm of (V(i)*W(i)) for the rms-norm, or
            // C   -ignore some components of V in the norm, with the effect of
            // C    suppressing the error control on those components of Y.
            // C-----------------------------------------------------------------------
            // C REVISION HISTORY (YYYYMMDD)
            // C  19890615  Date Written.  Initial release.
            // C  19890922  Added interrupt/restart ability, minor changes throughout.
            // C  19910228  Minor revisions in line format,  prologue, etc.
            // C  19920227  Modifications by D. Pang:
            // C            (1) Applied subgennam to get generic intrinsic names.
            // C            (2) Changed intrinsic names to generic in comments.
            // C            (3) Added *DECK lines before each routine.
            // C  19920721  Names of routines and labeled Common blocks changed, so as
            // C            to be unique in combined single/double precision code (ACH).
            // C  19920722  Minor revisions to prologue (ACH).
            // C  19920831  Conversion to double precision done (ACH).
            // C  19921106  Fixed minor bug: ETAQ,ETAQM1 in DVSTEP SAVE statement (ACH).
            // C  19921118  Changed LUNSAV/MFLGSV to IXSAV (ACH).
            // C  19941222  Removed MF overwrite; attached sign to H in estimated second 
            // C            deriv. in DVHIN; misc. comment changes throughout (ACH).
            // C  19970515  Minor corrections to comments in prologue, DVJAC (ACH).
            // C  19981111  Corrected Block B by adding final line, GO TO 200 (ACH).
            // C  20020430  Various upgrades (ACH): Use ODEPACK error handler package.
            // C            Replaced D1MACH by DUMACH.  Various changes to main
            // C            prologue and other routine prologues.
            // C-----------------------------------------------------------------------
            // C Other Routines in the DVODE Package.
            // C
            // C In addition to subroutine DVODE, the DVODE package includes the
            // C following subroutines and function routines:
            // C  DVHIN     computes an approximate step size for the initial step.
            // C  DVINDY    computes an interpolated value of the y vector at t = TOUT.
            // C  DVSTEP    is the core integrator, which does one step of the
            // C            integration and the associated error control.
            // C  DVSET     sets all method coefficients and test constants.
            // C  DVNLSD    solves the underlying nonlinear system -- the corrector.
            // C  DVJAC     computes and preprocesses the Jacobian matrix J = df/dy
            // C            and the Newton iteration matrix P = I - (h/l1)*J.
            // C  DVSOL     manages solution of linear system in chord iteration.
            // C  DVJUST    adjusts the history array on a change of order.
            // C  DEWSET    sets the error weight vector EWT before each step.
            // C  DVNORM    computes the weighted r.m.s. norm of a vector.
            // C  DVSRCO    is a user-callable routine to save and restore
            // C            the contents of the internal COMMON blocks.
            // C  DACOPY    is a routine to copy one two-dimensional array to another.
            // C  DGEFA and DGESL   are routines from LINPACK for solving full
            // C            systems of linear algebraic equations.
            // C  DGBFA and DGBSL   are routines from LINPACK for solving banded
            // C            linear systems.
            // C  DAXPY, DSCAL, and DCOPY are basic linear algebra modules (BLAS).
            // C  DUMACH    sets the unit roundoff of the machine.
            // C  XERRWD, XSETUN, XSETF, IXSAV, and IUMACH handle the printing of all
            // C            error messages and warnings.  XERRWD is machine-dependent.
            // C Note:  DVNORM, DUMACH, IXSAV, and IUMACH are function routines.
            // C All the others are subroutines.
            // C
            // C-----------------------------------------------------------------------
            // C
            // C Type declarations for labeled COMMON block DVOD01 --------------------
            // C
            // C
            // C Type declarations for labeled COMMON block DVOD02 --------------------
            // C
            // C
            // C Type declarations for local variables --------------------------------
            // C
            // C
            // C Type declaration for function subroutines called ---------------------
            // C
            // C
            // C-----------------------------------------------------------------------
            // C The following Fortran-77 declaration is to cause the values of the
            // C listed (local) variables to be saved between calls to DVODE.
            // C-----------------------------------------------------------------------
            // C-----------------------------------------------------------------------
            // C The following internal COMMON blocks contain variables which are
            // C communicated between subroutines in the DVODE package, or which are
            // C to be saved between calls to DVODE.
            // C In each block, real variables precede integers.
            // C The block /DVOD01/ appears in subroutines DVODE, DVINDY, DVSTEP,
            // C DVSET, DVNLSD, DVJAC, DVSOL, DVJUST and DVSRCO.
            // C The block /DVOD02/ appears in subroutines DVODE, DVINDY, DVSTEP,
            // C DVNLSD, DVJAC, and DVSRCO.
            // C
            // C The variables stored in the internal COMMON blocks are as follows:
            // C
            // C ACNRM  = Weighted r.m.s. norm of accumulated correction vectors.
            // C CCMXJ  = Threshhold on DRC for updating the Jacobian. (See DRC.)
            // C CONP   = The saved value of TQ(5).
            // C CRATE  = Estimated corrector convergence rate constant.
            // C DRC    = Relative change in H*RL1 since last DVJAC call.
            // C EL     = Real array of integration coefficients.  See DVSET.
            // C ETA    = Saved tentative ratio of new to old H.
            // C ETAMAX = Saved maximum value of ETA to be allowed.
            // C H      = The step size.
            // C HMIN   = The minimum absolute value of the step size H to be used.
            // C HMXI   = Inverse of the maximum absolute value of H to be used.
            // C          HMXI = 0.0 is allowed and corresponds to an infinite HMAX.
            // C HNEW   = The step size to be attempted on the next step.
            // C HSCAL  = Stepsize in scaling of YH array.
            // C PRL1   = The saved value of RL1.
            // C RC     = Ratio of current H*RL1 to value on last DVJAC call.
            // C RL1    = The reciprocal of the coefficient EL(1).
            // C TAU    = Real vector of past NQ step sizes, length 13.
            // C TQ     = A real vector of length 5 in which DVSET stores constants
            // C          used for the convergence test, the error test, and the
            // C          selection of H at a new order.
            // C TN     = The independent variable, updated on each step taken.
            // C UROUND = The machine unit roundoff.  The smallest positive real number
            // C          such that  1.0 + UROUND .ne. 1.0
            // C ICF    = Integer flag for convergence failure in DVNLSD:
            // C            0 means no failures.
            // C            1 means convergence failure with out of date Jacobian
            // C                   (recoverable error).
            // C            2 means convergence failure with current Jacobian or
            // C                   singular matrix (unrecoverable error).
            // C INIT   = Saved integer flag indicating whether initialization of the
            // C          problem has been done (INIT = 1) or not.
            // C IPUP   = Saved flag to signal updating of Newton matrix.
            // C JCUR   = Output flag from DVJAC showing Jacobian status:
            // C            JCUR = 0 means J is not current.
            // C            JCUR = 1 means J is current.
            // C JSTART = Integer flag used as input to DVSTEP:
            // C            0  means perform the first step.
            // C            1  means take a new step continuing from the last.
            // C            -1 means take the next step with a new value of MAXORD,
            // C                  HMIN, HMXI, N, METH, MITER, and/or matrix parameters.
            // C          On return, DVSTEP sets JSTART = 1.
            // C JSV    = Integer flag for Jacobian saving, = sign(MF).
            // C KFLAG  = A completion code from DVSTEP with the following meanings:
            // C               0      the step was succesful.
            // C              -1      the requested error could not be achieved.
            // C              -2      corrector convergence could not be achieved.
            // C              -3, -4  fatal error in VNLS (can not occur here).
            // C KUTH   = Input flag to DVSTEP showing whether H was reduced by the
            // C          driver.  KUTH = 1 if H was reduced, = 0 otherwise.
            // C L      = Integer variable, NQ + 1, current order plus one.
            // C LMAX   = MAXORD + 1 (used for dimensioning).
            // C LOCJS  = A pointer to the saved Jacobian, whose storage starts at
            // C          WM(LOCJS), if JSV = 1.
            // C LYH, LEWT, LACOR, LSAVF, LWM, LIWM = Saved integer pointers
            // C          to segments of RWORK and IWORK.
            // C MAXORD = The maximum order of integration method to be allowed.
            // C METH/MITER = The method flags.  See MF.
            // C MSBJ   = The maximum number of steps between J evaluations, = 50.
            // C MXHNIL = Saved value of optional input MXHNIL.
            // C MXSTEP = Saved value of optional input MXSTEP.
            // C N      = The number of first-order ODEs, = NEQ.
            // C NEWH   = Saved integer to flag change of H.
            // C NEWQ   = The method order to be used on the next step.
            // C NHNIL  = Saved counter for occurrences of T + H = T.
            // C NQ     = Integer variable, the current integration method order.
            // C NQNYH  = Saved value of NQ*NYH.
            // C NQWAIT = A counter controlling the frequency of order changes.
            // C          An order change is about to be considered if NQWAIT = 1.
            // C NSLJ   = The number of steps taken as of the last Jacobian update.
            // C NSLP   = Saved value of NST as of last Newton matrix update.
            // C NYH    = Saved value of the initial value of NEQ.
            // C HU     = The step size in t last used.
            // C NCFN   = Number of nonlinear convergence failures so far.
            // C NETF   = The number of error test failures of the integrator so far.
            // C NFE    = The number of f evaluations for the problem so far.
            // C NJE    = The number of Jacobian evaluations so far.
            // C NLU    = The number of matrix LU decompositions so far.
            // C NNI    = Number of nonlinear iterations so far.
            // C NQU    = The method order last used.
            // C NST    = The number of steps taken for the problem so far.
            // C-----------------------------------------------------------------------
            // C
            // C-----------------------------------------------------------------------
            // C Block A.
            // C This code block is executed on every call.
            // C It tests ISTATE and ITASK for legality and branches appropriately.
            // C If ISTATE .gt. 1 but the flag INIT shows that initialization has
            // C not yet been done, an error return occurs.
            // C If ISTATE = 1 and TOUT = T, return immediately.
            // C-----------------------------------------------------------------------
            #endregion
            #region Body
            
            if (ISTATE < 1 || ISTATE > 3) goto LABEL601;
            if (ITASK < 1 || ITASK > 5) goto LABEL602;
            if (ISTATE == 1) goto LABEL10;
            if (INIT.v != 1) goto LABEL603;
            if (ISTATE == 2) goto LABEL200;
            goto LABEL20;
        LABEL10:  INIT.v = 0;
            if (TOUT == T) return;
            // C-----------------------------------------------------------------------
            // C Block B.
            // C The next code block is executed for the initial call (ISTATE = 1),
            // C or for a continuation call with parameter changes (ISTATE = 3).
            // C It contains checking of all input and various initializations.
            // C
            // C First check legality of the non-optional input NEQ, ITOL, IOPT,
            // C MF, ML, and MU.
            // C-----------------------------------------------------------------------
        LABEL20:  
            if (NEQ <= 0) goto LABEL604;
            if (ISTATE == 1) goto LABEL25;
            if (NEQ > N.v) goto LABEL605;
        LABEL25:  N.v = NEQ;
            if (ITOL < 1 || ITOL > 4) goto LABEL606;
            if (IOPT < 0 || IOPT > 1) goto LABEL607;
            JSV.v = FortranLib.Sign(1,MF);
            MFA = Math.Abs(MF);
            METH.v = MFA / 10;
            MITER.v = MFA - 10 * METH.v;
            if (METH.v < 1 || METH.v > 2) goto LABEL608;
            if (MITER.v < 0 || MITER.v > 5) goto LABEL608;
            if (MITER.v <= 3) goto LABEL30;
            ML = IWORK[1 + o_iwork];
            MU = IWORK[2 + o_iwork];
            if (ML < 0 || ML >= N.v) goto LABEL609;
            if (MU < 0 || MU >= N.v) goto LABEL610;
        LABEL30:;
            // C Next process and check the optional input. ---------------------------
            if (IOPT == 1) goto LABEL40;
            MAXORD.v = MORD[METH.v + o_mord];
            MXSTEP.v = MXSTP0;
            MXHNIL.v = MXHNL0;
            if (ISTATE == 1) H0 = ZERO;
            HMXI.v = ZERO;
            HMIN.v = ZERO;
            goto LABEL60;
        LABEL40:  MAXORD.v = IWORK[5 + o_iwork];
            if (MAXORD.v < 0) goto LABEL611;
            if (MAXORD.v == 0) MAXORD.v = 100;
            MAXORD.v = Math.Min(MAXORD.v, MORD[METH.v + o_mord]);
            MXSTEP.v = IWORK[6 + o_iwork];
            if (MXSTEP.v < 0) goto LABEL612;
            if (MXSTEP.v == 0) MXSTEP.v = MXSTP0;
            MXHNIL.v = IWORK[7 + o_iwork];
            if (MXHNIL.v < 0) goto LABEL613;
            if (MXHNIL.v == 0) MXHNIL.v = MXHNL0;
            if (ISTATE != 1) goto LABEL50;
            H0 = RWORK[5 + o_rwork];
            if ((TOUT - T) * H0 < ZERO) goto LABEL614;
        LABEL50:  HMAX = RWORK[6 + o_rwork];
            if (HMAX < ZERO) goto LABEL615;
            HMXI.v = ZERO;
            if (HMAX > ZERO) HMXI.v = ONE / HMAX;
            HMIN.v = RWORK[7 + o_rwork];
            if (HMIN.v < ZERO) goto LABEL616;
            // C-----------------------------------------------------------------------
            // C Set work array pointers and check lengths LRW and LIW.
            // C Pointers to segments of RWORK and IWORK are named by prefixing L to
            // C the name of the segment.  E.g., the segment YH starts at RWORK(LYH).
            // C Segments of RWORK (in order) are denoted  YH, WM, EWT, SAVF, ACOR.
            // C Within WM, LOCJS is the location of the saved Jacobian (JSV .gt. 0).
            // C-----------------------------------------------------------------------
        LABEL60:  LYH.v = 21;
            if (ISTATE == 1) NYH.v = N.v;
            LWM.v = LYH.v + (MAXORD.v + 1) * NYH.v;
            JCO = Math.Max(0, JSV.v);
            if (MITER.v == 0) LENWM = 0;
            if (MITER.v == 1 || MITER.v == 2)
            {
                LENWM = 2 + (1 + JCO) * N.v * N.v;
                LOCJS.v = N.v * N.v + 3;
            }
            if (MITER.v == 3) LENWM = 2 + N.v;
            if (MITER.v == 4 || MITER.v == 5)
            {
                MBAND = ML + MU + 1;
                LENP = (MBAND + ML) * N.v;
                LENJ = MBAND * N.v;
                LENWM = 2 + LENP + JCO * LENJ;
                LOCJS.v = LENP + 3;
            }
            LEWT.v = LWM.v + LENWM;
            LSAVF.v = LEWT.v + N.v;
            LACOR.v = LSAVF.v + N.v;
            LENRW = LACOR.v + N.v - 1;
            IWORK[17 + o_iwork] = LENRW;
            LIWM.v = 1;
            LENIW = 30 + N.v;
            if (MITER.v == 0 || MITER.v == 3) LENIW = 30;
            IWORK[18 + o_iwork] = LENIW;
            if (LENRW > LRW) goto LABEL617;
            if (LENIW > LIW) goto LABEL618;
            // C Check RTOL and ATOL for legality. ------------------------------------
            RTOLI = RTOL[1 + o_rtol];
            ATOLI = ATOL[1 + o_atol];
            for (I = 1; I <= N.v; I++)
            {
                if (ITOL >= 3) RTOLI = RTOL[I + o_rtol];
                if (ITOL == 2 || ITOL == 4) ATOLI = ATOL[I + o_atol];
                if (RTOLI < ZERO) goto LABEL619;
                if (ATOLI < ZERO) goto LABEL620;
            }
            if (ISTATE == 1) goto LABEL100;
            // C If ISTATE = 3, set flag to signal parameter changes to DVSTEP. -------
            JSTART.v =  - 1;
            if (NQ.v <= MAXORD.v) goto LABEL90;
            // C MAXORD was reduced below NQ.  Copy YH(*,MAXORD+2) into SAVF. ---------
            this._dcopy.Run(N.v, RWORK, LWM.v + o_rwork, 1, ref RWORK, LSAVF.v + o_rwork, 1);
            // C Reload WM(1) = RWORK(LWM), since LWM may have changed. ---------------
        LABEL90:  
            if (MITER.v > 0) RWORK[LWM.v + o_rwork] = Math.Sqrt(UROUND.v);
            goto LABEL200;
            // C-----------------------------------------------------------------------
            // C Block C.
            // C The next block is for the initial call only (ISTATE = 1).
            // C It contains all remaining initializations, the initial call to F,
            // C and the calculation of the initial step size.
            // C The error weights in EWT are inverted after being loaded.
            // C-----------------------------------------------------------------------
        LABEL100:  UROUND.v = this._dumach.Run();
            TN.v = T;
            if (ITASK != 4 && ITASK != 5) goto LABEL110;
            TCRIT = RWORK[1 + o_rwork];
            if ((TCRIT - TOUT) * (TOUT - T) < ZERO) goto LABEL625;
            if (H0 != ZERO && (T + H0 - TCRIT) * H0 > ZERO) H0 = TCRIT - T;
        LABEL110:  JSTART.v = 0;
            if (MITER.v > 0) RWORK[LWM.v + o_rwork] = Math.Sqrt(UROUND.v);
            CCMXJ.v = PT2;
            MSBJ.v = 50;
            NHNIL.v = 0;
            NST.v = 0;
            NJE.v = 0;
            NNI.v = 0;
            NCFN.v = 0;
            NETF.v = 0;
            NLU.v = 0;
            NSLJ.v = 0;
            NSLAST = 0;
            HU.v = ZERO;
            NQU.v = 0;
            // C Initial call to F.  (LF0 points to YH(*,2).) -------------------------
            LF0 = LYH.v + NYH.v;
            F.Run(N.v, T, Y, offset_y, ref RWORK, LF0 + o_rwork, RPAR[1 + o_rpar], IPAR[1 + o_ipar]);
            NFE.v = 1;
            // C Load the initial value vector in YH. ---------------------------------
            this._dcopy.Run(N.v, Y, offset_y, 1, ref RWORK, LYH.v + o_rwork, 1);
            // C Load and invert the EWT array.  (H is temporarily set to 1.0.) -------
            NQ.v = 1;
            H.v = ONE;
            this._dewset.Run(N.v, ITOL, RTOL, offset_rtol, ATOL, offset_atol, RWORK, LYH.v + o_rwork, ref RWORK, LEWT.v + o_rwork);
            for (I = 1; I <= N.v; I++)
            {
                if (RWORK[I + LEWT.v - 1 + o_rwork] <= ZERO) goto LABEL621;
                RWORK[I + LEWT.v - 1 + o_rwork] = ONE / RWORK[I + LEWT.v - 1 + o_rwork];
            }
            if (H0 != ZERO) goto LABEL180;
            // C Call DVHIN to set initial step size H0 to be attempted. --------------
            this._dvhin.Run(N.v, T, RWORK, LYH.v + o_rwork, RWORK, LF0 + o_rwork, F, RPAR, offset_rpar
                            , IPAR, offset_ipar, TOUT, UROUND.v, RWORK, LEWT.v + o_rwork, ITOL, ATOL, offset_atol
                            , ref Y, offset_y, ref RWORK, LACOR.v + o_rwork, ref H0, ref NITER, ref IER);
            NFE.v += NITER;
            if (IER != 0) goto LABEL622;
            // C Adjust H0 if necessary to meet HMAX bound. ---------------------------
        LABEL180:  RH = Math.Abs(H0) * HMXI.v;
            if (RH > ONE) H0 /= RH;
            // C Load H with H0 and scale YH(*,2) by H0. ------------------------------
            H.v = H0;
            this._dscal.Run(N.v, H0, ref RWORK, LF0 + o_rwork, 1);
            goto LABEL270;
            // C-----------------------------------------------------------------------
            // C Block D.
            // C The next code block is for continuation calls only (ISTATE = 2 or 3)
            // C and is to check stop conditions before taking a step.
            // C-----------------------------------------------------------------------
        LABEL200:  NSLAST = NST.v;
            KUTH.v = 0;
            switch (ITASK)
            {
                case 1: goto LABEL210;
                case 2: goto LABEL250;
                case 3: goto LABEL220;
                case 4: goto LABEL230;
                case 5: goto LABEL240;
            }
        LABEL210:  
            if ((TN.v - TOUT) * H.v < ZERO) goto LABEL250;
            this._dvindy.Run(TOUT, 0, RWORK, LYH.v + o_rwork, NYH.v, ref Y, offset_y, ref IFLAG);
            if (IFLAG != 0) goto LABEL627;
            T = TOUT;
            goto LABEL420;
        LABEL220:  TP = TN.v - HU.v * (ONE + HUN * UROUND.v);
            if ((TP - TOUT) * H.v > ZERO) goto LABEL623;
            if ((TN.v - TOUT) * H.v < ZERO) goto LABEL250;
            goto LABEL400;
        LABEL230:  TCRIT = RWORK[1 + o_rwork];
            if ((TN.v - TCRIT) * H.v > ZERO) goto LABEL624;
            if ((TCRIT - TOUT) * H.v < ZERO) goto LABEL625;
            if ((TN.v - TOUT) * H.v < ZERO) goto LABEL245;
            this._dvindy.Run(TOUT, 0, RWORK, LYH.v + o_rwork, NYH.v, ref Y, offset_y, ref IFLAG);
            if (IFLAG != 0) goto LABEL627;
            T = TOUT;
            goto LABEL420;
        LABEL240:  TCRIT = RWORK[1 + o_rwork];
            if ((TN.v - TCRIT) * H.v > ZERO) goto LABEL624;
        LABEL245:  HMX = Math.Abs(TN.v) + Math.Abs(H.v);
            IHIT = Math.Abs(TN.v - TCRIT) <= HUN * UROUND.v * HMX;
            if (IHIT) goto LABEL400;
            TNEXT = TN.v + HNEW.v * (ONE + FOUR * UROUND.v);
            if ((TNEXT - TCRIT) * H.v <= ZERO) goto LABEL250;
            H.v = (TCRIT - TN.v) * (ONE - FOUR * UROUND.v);
            KUTH.v = 1;
            // C-----------------------------------------------------------------------
            // C Block E.
            // C The next block is normally executed for all calls and contains
            // C the call to the one-step core integrator DVSTEP.
            // C
            // C This is a looping point for the integration steps.
            // C
            // C First check for too many steps being taken, update EWT (if not at
            // C start of problem), check for too much accuracy being requested, and
            // C check for H below the roundoff level in T.
            // C-----------------------------------------------------------------------
        LABEL250:;
            if ((NST.v - NSLAST) >= MXSTEP.v) goto LABEL500;
            this._dewset.Run(N.v, ITOL, RTOL, offset_rtol, ATOL, offset_atol, RWORK, LYH.v + o_rwork, ref RWORK, LEWT.v + o_rwork);
            for (I = 1; I <= N.v; I++)
            {
                if (RWORK[I + LEWT.v - 1 + o_rwork] <= ZERO) goto LABEL510;
                RWORK[I + LEWT.v - 1 + o_rwork] = ONE / RWORK[I + LEWT.v - 1 + o_rwork];
            }
        LABEL270:  TOLSF = UROUND.v * this._dvnorm.Run(N.v, RWORK, LYH.v + o_rwork, RWORK, LEWT.v + o_rwork);
            if (TOLSF <= ONE) goto LABEL280;
            TOLSF *= TWO;
            if (NST.v == 0) goto LABEL626;
            goto LABEL520;
        LABEL280:  
            if ((TN.v + H.v) != TN.v) goto LABEL290;
            NHNIL.v += 1;
            if (NHNIL.v > MXHNIL.v) goto LABEL290;
            FortranLib.Copy(ref MSG , "DVODE--  Warning: internal T (=R1) and H (=R2) are");
            this._xerrwd.Run(MSG, 50, 101, 1, 0, 0
                             , 0, 0, ZERO, ZERO);
            FortranLib.Copy(ref MSG , "      such that in the machine, T + H = T on the next step  ");
            this._xerrwd.Run(MSG, 60, 101, 1, 0, 0
                             , 0, 0, ZERO, ZERO);
            FortranLib.Copy(ref MSG , "      (H = step size). solver will continue anyway");
            this._xerrwd.Run(MSG, 50, 101, 1, 0, 0
                             , 0, 2, TN.v, H.v);
            if (NHNIL.v < MXHNIL.v) goto LABEL290;
            FortranLib.Copy(ref MSG , "DVODE--  Above warning has been issued I1 times.  ");
            this._xerrwd.Run(MSG, 50, 102, 1, 0, 0
                             , 0, 0, ZERO, ZERO);
            FortranLib.Copy(ref MSG , "      it will not be issued again for this problem");
            this._xerrwd.Run(MSG, 50, 102, 1, 1, MXHNIL.v
                             , 0, 0, ZERO, ZERO);
        LABEL290:;
            // C-----------------------------------------------------------------------
            // C CALL DVSTEP (Y, YH, NYH, YH, EWT, SAVF, VSAV, ACOR,
            // C              WM, IWM, F, JAC, F, DVNLSD, RPAR, IPAR)
            // C-----------------------------------------------------------------------
            this._dvstep.Run(ref Y, offset_y, ref RWORK, LYH.v + o_rwork, NYH.v, ref RWORK, LYH.v + o_rwork, RWORK, LEWT.v + o_rwork, ref RWORK, LSAVF.v + o_rwork
                             , Y, offset_y, ref RWORK, LACOR.v + o_rwork, ref RWORK, LWM.v + o_rwork, ref IWORK, LIWM.v + o_iwork, F, JAC
                             , F, this._dvnlsd, RPAR, offset_rpar, IPAR, offset_ipar);
            KGO = 1 - KFLAG.v;
            // C Branch on KFLAG.  Note: In this version, KFLAG can not be set to -3.
            // C  KFLAG .eq. 0,   -1,  -2
            switch (KGO)
            {
                case 1: goto LABEL300;
                case 2: goto LABEL530;
                case 3: goto LABEL540;
            }
            // C-----------------------------------------------------------------------
            // C Block F.
            // C The following block handles the case of a successful return from the
            // C core integrator (KFLAG = 0).  Test for stop conditions.
            // C-----------------------------------------------------------------------
        LABEL300:  INIT.v = 1;
            KUTH.v = 0;
            switch (ITASK)
            {
                case 1: goto LABEL310;
                case 2: goto LABEL400;
                case 3: goto LABEL330;
                case 4: goto LABEL340;
                case 5: goto LABEL350;
            }
            // C ITASK = 1.  If TOUT has been reached, interpolate. -------------------
        LABEL310:  
            if ((TN.v - TOUT) * H.v < ZERO) goto LABEL250;
            this._dvindy.Run(TOUT, 0, RWORK, LYH.v + o_rwork, NYH.v, ref Y, offset_y, ref IFLAG);
            T = TOUT;
            goto LABEL420;
            // C ITASK = 3.  Jump to exit if TOUT was reached. ------------------------
        LABEL330:  
            if ((TN.v - TOUT) * H.v >= ZERO) goto LABEL400;
            goto LABEL250;
            // C ITASK = 4.  See if TOUT or TCRIT was reached.  Adjust H if necessary.
        LABEL340:  
            if ((TN.v - TOUT) * H.v < ZERO) goto LABEL345;
            this._dvindy.Run(TOUT, 0, RWORK, LYH.v + o_rwork, NYH.v, ref Y, offset_y, ref IFLAG);
            T = TOUT;
            goto LABEL420;
        LABEL345:  HMX = Math.Abs(TN.v) + Math.Abs(H.v);
            IHIT = Math.Abs(TN.v - TCRIT) <= HUN * UROUND.v * HMX;
            if (IHIT) goto LABEL400;
            TNEXT = TN.v + HNEW.v * (ONE + FOUR * UROUND.v);
            if ((TNEXT - TCRIT) * H.v <= ZERO) goto LABEL250;
            H.v = (TCRIT - TN.v) * (ONE - FOUR * UROUND.v);
            KUTH.v = 1;
            goto LABEL250;
            // C ITASK = 5.  See if TCRIT was reached and jump to exit. ---------------
        LABEL350:  HMX = Math.Abs(TN.v) + Math.Abs(H.v);
            IHIT = Math.Abs(TN.v - TCRIT) <= HUN * UROUND.v * HMX;
            // C-----------------------------------------------------------------------
            // C Block G.
            // C The following block handles all successful returns from DVODE.
            // C If ITASK .ne. 1, Y is loaded from YH and T is set accordingly.
            // C ISTATE is set to 2, and the optional output is loaded into the work
            // C arrays before returning.
            // C-----------------------------------------------------------------------
        LABEL400:;
            this._dcopy.Run(N.v, RWORK, LYH.v + o_rwork, 1, ref Y, offset_y, 1);
            T = TN.v;
            if (ITASK != 4 && ITASK != 5) goto LABEL420;
            if (IHIT) T = TCRIT;
        LABEL420:  ISTATE = 2;
            RWORK[11 + o_rwork] = HU.v;
            RWORK[12 + o_rwork] = HNEW.v;
            RWORK[13 + o_rwork] = TN.v;
            IWORK[11 + o_iwork] = NST.v;
            IWORK[12 + o_iwork] = NFE.v;
            IWORK[13 + o_iwork] = NJE.v;
            IWORK[14 + o_iwork] = NQU.v;
            IWORK[15 + o_iwork] = NEWQ.v;
            IWORK[19 + o_iwork] = NLU.v;
            IWORK[20 + o_iwork] = NNI.v;
            IWORK[21 + o_iwork] = NCFN.v;
            IWORK[22 + o_iwork] = NETF.v;
            return;
            // C-----------------------------------------------------------------------
            // C Block H.
            // C The following block handles all unsuccessful returns other than
            // C those for illegal input.  First the error message routine is called.
            // C if there was an error test or convergence test failure, IMXER is set.
            // C Then Y is loaded from YH, and T is set to TN.
            // C The optional output is loaded into the work arrays before returning.
            // C-----------------------------------------------------------------------
            // C The maximum number of steps was taken before reaching TOUT. ----------
        LABEL500:  FortranLib.Copy(ref MSG , "DVODE--  At current T (=R1), MXSTEP (=I1) steps   ");
            this._xerrwd.Run(MSG, 50, 201, 1, 0, 0
                             , 0, 0, ZERO, ZERO);
            FortranLib.Copy(ref MSG , "      taken on this call before reaching TOUT     ");
            this._xerrwd.Run(MSG, 50, 201, 1, 1, MXSTEP.v
                             , 0, 1, TN.v, ZERO);
            ISTATE =  - 1;
            goto LABEL580;
            // C EWT(i) .le. 0.0 for some i (not at start of problem). ----------------
        LABEL510:  EWTI = RWORK[LEWT.v + I - 1 + o_rwork];
            FortranLib.Copy(ref MSG , "DVODE--  At T (=R1), EWT(I1) has become R2 .le. 0.");
            this._xerrwd.Run(MSG, 50, 202, 1, 1, I
                             , 0, 2, TN.v, EWTI);
            ISTATE =  - 6;
            goto LABEL580;
            // C Too much accuracy requested for machine precision. -------------------
        LABEL520:  FortranLib.Copy(ref MSG , "DVODE--  At T (=R1), too much accuracy requested  ");
            this._xerrwd.Run(MSG, 50, 203, 1, 0, 0
                             , 0, 0, ZERO, ZERO);
            FortranLib.Copy(ref MSG , "      for precision of machine:   see TOLSF (=R2) ");
            this._xerrwd.Run(MSG, 50, 203, 1, 0, 0
                             , 0, 2, TN.v, TOLSF);
            RWORK[14 + o_rwork] = TOLSF;
            ISTATE =  - 2;
            goto LABEL580;
            // C KFLAG = -1.  Error test failed repeatedly or with ABS(H) = HMIN. -----
        LABEL530:  FortranLib.Copy(ref MSG , "DVODE--  At T(=R1) and step size H(=R2), the error");
            this._xerrwd.Run(MSG, 50, 204, 1, 0, 0
                             , 0, 0, ZERO, ZERO);
            FortranLib.Copy(ref MSG , "      test failed repeatedly or with abs(H) = HMIN");
            this._xerrwd.Run(MSG, 50, 204, 1, 0, 0
                             , 0, 2, TN.v, H.v);
            ISTATE =  - 4;
            goto LABEL560;
            // C KFLAG = -2.  Convergence failed repeatedly or with ABS(H) = HMIN. ----
        LABEL540:  FortranLib.Copy(ref MSG , "DVODE--  At T (=R1) and step size H (=R2), the    ");
            this._xerrwd.Run(MSG, 50, 205, 1, 0, 0
                             , 0, 0, ZERO, ZERO);
            FortranLib.Copy(ref MSG , "      corrector convergence failed repeatedly     ");
            this._xerrwd.Run(MSG, 50, 205, 1, 0, 0
                             , 0, 0, ZERO, ZERO);
            FortranLib.Copy(ref MSG , "      or with abs(H) = HMIN   ");
            this._xerrwd.Run(MSG, 30, 205, 1, 0, 0
                             , 0, 2, TN.v, H.v);
            ISTATE =  - 5;
            // C Compute IMXER if relevant. -------------------------------------------
        LABEL560:  BIG = ZERO;
            IMXER = 1;
            for (I = 1; I <= N.v; I++)
            {
                SIZE = Math.Abs(RWORK[I + LACOR.v - 1 + o_rwork] * RWORK[I + LEWT.v - 1 + o_rwork]);
                if (BIG >= SIZE) goto LABEL570;
                BIG = SIZE;
                IMXER = I;
            LABEL570:;
            }
            IWORK[16 + o_iwork] = IMXER;
            // C Set Y vector, T, and optional output. --------------------------------
        LABEL580:;
            this._dcopy.Run(N.v, RWORK, LYH.v + o_rwork, 1, ref Y, offset_y, 1);
            T = TN.v;
            RWORK[11 + o_rwork] = HU.v;
            RWORK[12 + o_rwork] = H.v;
            RWORK[13 + o_rwork] = TN.v;
            IWORK[11 + o_iwork] = NST.v;
            IWORK[12 + o_iwork] = NFE.v;
            IWORK[13 + o_iwork] = NJE.v;
            IWORK[14 + o_iwork] = NQU.v;
            IWORK[15 + o_iwork] = NQ.v;
            IWORK[19 + o_iwork] = NLU.v;
            IWORK[20 + o_iwork] = NNI.v;
            IWORK[21 + o_iwork] = NCFN.v;
            IWORK[22 + o_iwork] = NETF.v;
            return;
            // C-----------------------------------------------------------------------
            // C Block I.
            // C The following block handles all error returns due to illegal input
            // C (ISTATE = -3), as detected before calling the core integrator.
            // C First the error message routine is called.   If the illegal input
            // C is a negative ISTATE, the run is aborted (apparent infinite loop).
            // C-----------------------------------------------------------------------
        LABEL601:  FortranLib.Copy(ref MSG , "DVODE--  ISTATE (=I1) illegal ");
            this._xerrwd.Run(MSG, 30, 1, 1, 1, ISTATE
                             , 0, 0, ZERO, ZERO);
            if (ISTATE < 0) goto LABEL800;
            goto LABEL700;
        LABEL602:  FortranLib.Copy(ref MSG , "DVODE--  ITASK (=I1) illegal  ");
            this._xerrwd.Run(MSG, 30, 2, 1, 1, ITASK
                             , 0, 0, ZERO, ZERO);
            goto LABEL700;
        LABEL603:  FortranLib.Copy(ref MSG , "DVODE--  ISTATE (=I1) .gt. 1 but DVODE not initialized      ");
            this._xerrwd.Run(MSG, 60, 3, 1, 1, ISTATE
                             , 0, 0, ZERO, ZERO);
            goto LABEL700;
        LABEL604:  FortranLib.Copy(ref MSG , "DVODE--  NEQ (=I1) .lt. 1     ");
            this._xerrwd.Run(MSG, 30, 4, 1, 1, NEQ
                             , 0, 0, ZERO, ZERO);
            goto LABEL700;
        LABEL605:  FortranLib.Copy(ref MSG , "DVODE--  ISTATE = 3 and NEQ increased (I1 to I2)  ");
            this._xerrwd.Run(MSG, 50, 5, 1, 2, N.v
                             , NEQ, 0, ZERO, ZERO);
            goto LABEL700;
        LABEL606:  FortranLib.Copy(ref MSG , "DVODE--  ITOL (=I1) illegal   ");
            this._xerrwd.Run(MSG, 30, 6, 1, 1, ITOL
                             , 0, 0, ZERO, ZERO);
            goto LABEL700;
        LABEL607:  FortranLib.Copy(ref MSG , "DVODE--  IOPT (=I1) illegal   ");
            this._xerrwd.Run(MSG, 30, 7, 1, 1, IOPT
                             , 0, 0, ZERO, ZERO);
            goto LABEL700;
        LABEL608:  FortranLib.Copy(ref MSG , "DVODE--  MF (=I1) illegal     ");
            this._xerrwd.Run(MSG, 30, 8, 1, 1, MF
                             , 0, 0, ZERO, ZERO);
            goto LABEL700;
        LABEL609:  FortranLib.Copy(ref MSG , "DVODE--  ML (=I1) illegal:  .lt.0 or .ge.NEQ (=I2)");
            this._xerrwd.Run(MSG, 50, 9, 1, 2, ML
                             , NEQ, 0, ZERO, ZERO);
            goto LABEL700;
        LABEL610:  FortranLib.Copy(ref MSG , "DVODE--  MU (=I1) illegal:  .lt.0 or .ge.NEQ (=I2)");
            this._xerrwd.Run(MSG, 50, 10, 1, 2, MU
                             , NEQ, 0, ZERO, ZERO);
            goto LABEL700;
        LABEL611:  FortranLib.Copy(ref MSG , "DVODE--  MAXORD (=I1) .lt. 0  ");
            this._xerrwd.Run(MSG, 30, 11, 1, 1, MAXORD.v
                             , 0, 0, ZERO, ZERO);
            goto LABEL700;
        LABEL612:  FortranLib.Copy(ref MSG , "DVODE--  MXSTEP (=I1) .lt. 0  ");
            this._xerrwd.Run(MSG, 30, 12, 1, 1, MXSTEP.v
                             , 0, 0, ZERO, ZERO);
            goto LABEL700;
        LABEL613:  FortranLib.Copy(ref MSG , "DVODE--  MXHNIL (=I1) .lt. 0  ");
            this._xerrwd.Run(MSG, 30, 13, 1, 1, MXHNIL.v
                             , 0, 0, ZERO, ZERO);
            goto LABEL700;
        LABEL614:  FortranLib.Copy(ref MSG , "DVODE--  TOUT (=R1) behind T (=R2)      ");
            this._xerrwd.Run(MSG, 40, 14, 1, 0, 0
                             , 0, 2, TOUT, T);
            FortranLib.Copy(ref MSG , "      integration direction is given by H0 (=R1)  ");
            this._xerrwd.Run(MSG, 50, 14, 1, 0, 0
                             , 0, 1, H0, ZERO);
            goto LABEL700;
        LABEL615:  FortranLib.Copy(ref MSG , "DVODE--  HMAX (=R1) .lt. 0.0  ");
            this._xerrwd.Run(MSG, 30, 15, 1, 0, 0
                             , 0, 1, HMAX, ZERO);
            goto LABEL700;
        LABEL616:  FortranLib.Copy(ref MSG , "DVODE--  HMIN (=R1) .lt. 0.0  ");
            this._xerrwd.Run(MSG, 30, 16, 1, 0, 0
                             , 0, 1, HMIN.v, ZERO);
            goto LABEL700;
        LABEL617:;
            FortranLib.Copy(ref MSG , "DVODE--  RWORK length needed, LENRW (=I1), exceeds LRW (=I2)");
            this._xerrwd.Run(MSG, 60, 17, 1, 2, LENRW
                             , LRW, 0, ZERO, ZERO);
            goto LABEL700;
        LABEL618:;
            FortranLib.Copy(ref MSG , "DVODE--  IWORK length needed, LENIW (=I1), exceeds LIW (=I2)");
            this._xerrwd.Run(MSG, 60, 18, 1, 2, LENIW
                             , LIW, 0, ZERO, ZERO);
            goto LABEL700;
        LABEL619:  FortranLib.Copy(ref MSG , "DVODE--  RTOL(I1) is R1 .lt. 0.0        ");
            this._xerrwd.Run(MSG, 40, 19, 1, 1, I
                             , 0, 1, RTOLI, ZERO);
            goto LABEL700;
        LABEL620:  FortranLib.Copy(ref MSG , "DVODE--  ATOL(I1) is R1 .lt. 0.0        ");
            this._xerrwd.Run(MSG, 40, 20, 1, 1, I
                             , 0, 1, ATOLI, ZERO);
            goto LABEL700;
        LABEL621:  EWTI = RWORK[LEWT.v + I - 1 + o_rwork];
            FortranLib.Copy(ref MSG , "DVODE--  EWT(I1) is R1 .le. 0.0         ");
            this._xerrwd.Run(MSG, 40, 21, 1, 1, I
                             , 0, 1, EWTI, ZERO);
            goto LABEL700;
        LABEL622:;
            FortranLib.Copy(ref MSG , "DVODE--  TOUT (=R1) too close to T(=R2) to start integration");
            this._xerrwd.Run(MSG, 60, 22, 1, 0, 0
                             , 0, 2, TOUT, T);
            goto LABEL700;
        LABEL623:;
            FortranLib.Copy(ref MSG , "DVODE--  ITASK = I1 and TOUT (=R1) behind TCUR - HU (= R2)  ");
            this._xerrwd.Run(MSG, 60, 23, 1, 1, ITASK
                             , 0, 2, TOUT, TP);
            goto LABEL700;
        LABEL624:;
            FortranLib.Copy(ref MSG , "DVODE--  ITASK = 4 or 5 and TCRIT (=R1) behind TCUR (=R2)   ");
            this._xerrwd.Run(MSG, 60, 24, 1, 0, 0
                             , 0, 2, TCRIT, TN.v);
            goto LABEL700;
        LABEL625:;
            FortranLib.Copy(ref MSG , "DVODE--  ITASK = 4 or 5 and TCRIT (=R1) behind TOUT (=R2)   ");
            this._xerrwd.Run(MSG, 60, 25, 1, 0, 0
                             , 0, 2, TCRIT, TOUT);
            goto LABEL700;
        LABEL626:  FortranLib.Copy(ref MSG , "DVODE--  At start of problem, too much accuracy   ");
            this._xerrwd.Run(MSG, 50, 26, 1, 0, 0
                             , 0, 0, ZERO, ZERO);
            FortranLib.Copy(ref MSG , "      requested for precision of machine:   see TOLSF (=R1) ");
            this._xerrwd.Run(MSG, 60, 26, 1, 0, 0
                             , 0, 1, TOLSF, ZERO);
            RWORK[14 + o_rwork] = TOLSF;
            goto LABEL700;
        LABEL627:  FortranLib.Copy(ref MSG , "DVODE--  Trouble from DVINDY.  ITASK = I1, TOUT = R1.       ");
            this._xerrwd.Run(MSG, 60, 27, 1, 1, ITASK
                             , 0, 1, TOUT, ZERO);
            // C
        LABEL700:;
            ISTATE =  - 3;
            return;
            // C
        LABEL800:  FortranLib.Copy(ref MSG , "DVODE--  Run aborted:  apparent infinite loop     ");
            this._xerrwd.Run(MSG, 50, 303, 2, 0, 0
                             , 0, 0, ZERO, ZERO);
            return;
            // C----------------------- End of Subroutine DVODE -----------------------
            #endregion
        }
    }

    #endregion


    #region The Class: DVHIN
    
    // *DECK DVHIN
    public class DVHIN
    {
    
        #region Dependencies
        
        DVNORM _dvnorm; 
        #endregion
        #region Variables
        
        double HALF = 0; double HUN = 0; double PT1 = 0; double TWO = 0; 
        #endregion
        public DVHIN(DVNORM dvnorm)
        {
    
            #region Set Dependencies
            
            this._dvnorm = dvnorm; 
            #endregion
            #region Data Initialization
            
            //HALF/0.5D0
            HALF = 0.5E0;
            //HUN/100.0D0
            HUN = 100.0E0;
            //PT1/0.1D0
            PT1 = 0.1E0;
            //TWO/2.0D0
            TWO = 2.0E0;
            #endregion
        }
    
        public DVHIN()
        {
    
            #region Dependencies (Initialization)
            
            DVNORM dvnorm = new DVNORM();
            #endregion
            #region Set Dependencies
            
            this._dvnorm = dvnorm; 
            #endregion
            #region Data Initialization
            
            //HALF/0.5D0
            HALF = 0.5E0;
            //HUN/100.0D0
            HUN = 100.0E0;
            //PT1/0.1D0
            PT1 = 0.1E0;
            //TWO/2.0D0
            TWO = 2.0E0;
            #endregion
        }
        /// <param name="N">
        /// = Size of ODE system, input.
        ///</param>
        /// <param name="T0">
        /// = Initial value of independent variable, input.
        ///</param>
        /// <param name="Y0">
        /// = Vector of initial conditions, input.
        ///</param>
        /// <param name="YDOT">
        /// = Vector of initial first derivatives, input.
        ///</param>
        /// <param name="F">
        /// = Name of subroutine for right-hand side f(t,y), input.
        ///</param>
        /// <param name="TOUT">
        /// = First output value of independent variable
        ///</param>
        /// <param name="UROUND">
        /// = Machine unit roundoff
        ///</param>
        /// <param name="H0">
        /// = Step size to be attempted, output.
        ///</param>
        /// <param name="NITER">
        /// = Number of iterations (and of f evaluations) to compute H0,
        /// output.
        ///</param>
        /// <param name="IER">
        /// = The error flag, returned with the value
        /// IER = 0  if no trouble occurred, or
        /// IER = -1 if TOUT and T0 are considered too close to proceed.
        ///</param>
        public void Run(int N, double T0, double[] Y0, int offset_y0, double[] YDOT, int offset_ydot, IFEX F, double[] RPAR, int offset_rpar
                         , int[] IPAR, int offset_ipar, double TOUT, double UROUND, double[] EWT, int offset_ewt, int ITOL, double[] ATOL, int offset_atol
                         , ref double[] Y, int offset_y, ref double[] TEMP, int offset_temp, ref double H0, ref int NITER, ref int IER)
        {
            #region Variables
            
            double AFI = 0; double ATOLI = 0; double DELYI = 0; double H = 0; double HG = 0; double HLB = 0; double HNEW = 0; 
            double HRAT = 0;double HUB = 0; double T1 = 0; double TDIST = 0; double TROUND = 0; double YDDNRM = 0; int I = 0; 
            int ITER = 0;
            #endregion
            #region Array Index Correction
            
             int o_y0 = -1 + offset_y0;  int o_ydot = -1 + offset_ydot;  int o_rpar = -1 + offset_rpar; 
             int o_ipar = -1 + offset_ipar; int o_ewt = -1 + offset_ewt;  int o_atol = -1 + offset_atol;  int o_y = -1 + offset_y; 
             int o_temp = -1 + offset_temp;
            #endregion
            #region Prolog
            
            // C-----------------------------------------------------------------------
            // C Call sequence input -- N, T0, Y0, YDOT, F, RPAR, IPAR, TOUT, UROUND,
            // C                        EWT, ITOL, ATOL, Y, TEMP
            // C Call sequence output -- H0, NITER, IER
            // C COMMON block variables accessed -- None
            // C
            // C Subroutines called by DVHIN:  F
            // C Function routines called by DVHI: DVNORM
            // C-----------------------------------------------------------------------
            // C This routine computes the step size, H0, to be attempted on the
            // C first step, when the user has not supplied a value for this.
            // C
            // C First we check that TOUT - T0 differs significantly from zero.  Then
            // C an iteration is done to approximate the initial second derivative
            // C and this is used to define h from w.r.m.s.norm(h**2 * yddot / 2) = 1.
            // C A bias factor of 1/2 is applied to the resulting h.
            // C The sign of H0 is inferred from the initial values of TOUT and T0.
            // C
            // C Communication with DVHIN is done with the following variables:
            // C
            // C N      = Size of ODE system, input.
            // C T0     = Initial value of independent variable, input.
            // C Y0     = Vector of initial conditions, input.
            // C YDOT   = Vector of initial first derivatives, input.
            // C F      = Name of subroutine for right-hand side f(t,y), input.
            // C RPAR, IPAR = Dummy names for user's real and integer work arrays.
            // C TOUT   = First output value of independent variable
            // C UROUND = Machine unit roundoff
            // C EWT, ITOL, ATOL = Error weights and tolerance parameters
            // C                   as described in the driver routine, input.
            // C Y, TEMP = Work arrays of length N.
            // C H0     = Step size to be attempted, output.
            // C NITER  = Number of iterations (and of f evaluations) to compute H0,
            // C          output.
            // C IER    = The error flag, returned with the value
            // C          IER = 0  if no trouble occurred, or
            // C          IER = -1 if TOUT and T0 are considered too close to proceed.
            // C-----------------------------------------------------------------------
            // C
            // C Type declarations for local variables --------------------------------
            // C
            // C
            // C Type declaration for function subroutines called ---------------------
            // C
            // C-----------------------------------------------------------------------
            // C The following Fortran-77 declaration is to cause the values of the
            // C listed (local) variables to be saved between calls to this integrator.
            // C-----------------------------------------------------------------------
            // C
            #endregion
            #region Body
            
            NITER = 0;
            TDIST = Math.Abs(TOUT - T0);
            TROUND = UROUND * Math.Max(Math.Abs(T0), Math.Abs(TOUT));
            if (TDIST < TWO * TROUND) goto LABEL100;
            // C
            // C Set a lower bound on h based on the roundoff level in T0 and TOUT. ---
            HLB = HUN * TROUND;
            // C Set an upper bound on h based on TOUT-T0 and the initial Y and YDOT. -
            HUB = PT1 * TDIST;
            ATOLI = ATOL[1 + o_atol];
            for (I = 1; I <= N; I++)
            {
                if (ITOL == 2 || ITOL == 4) ATOLI = ATOL[I + o_atol];
                DELYI = PT1 * Math.Abs(Y0[I + o_y0]) + ATOLI;
                AFI = Math.Abs(YDOT[I + o_ydot]);
                if (AFI * HUB > DELYI) HUB = DELYI / AFI;
            }
            // C
            // C Set initial guess for h as geometric mean of upper and lower bounds. -
            ITER = 0;
            HG = Math.Sqrt(HLB * HUB);
            // C If the bounds have crossed, exit with the mean value. ----------------
            if (HUB < HLB)
            {
                H0 = HG;
                goto LABEL90;
            }
            // C
            // C Looping point for iteration. -----------------------------------------
        LABEL50:;
            // C Estimate the second derivative as a difference quotient in f. --------
            H = FortranLib.Sign(HG,TOUT - T0);
            T1 = T0 + H;
            for (I = 1; I <= N; I++)
            {
                Y[I + o_y] = Y0[I + o_y0] + H * YDOT[I + o_ydot];
            }
            F.Run(N, T1, Y, offset_y, ref TEMP, offset_temp, RPAR[1 + o_rpar], IPAR[1 + o_ipar]);
            for (I = 1; I <= N; I++)
            {
                TEMP[I + o_temp] = (TEMP[I + o_temp] - YDOT[I + o_ydot]) / H;
            }
            YDDNRM = this._dvnorm.Run(N, TEMP, offset_temp, EWT, offset_ewt);
            // C Get the corresponding new value of h. --------------------------------
            if (YDDNRM * HUB * HUB > TWO)
            {
                HNEW = Math.Sqrt(TWO / YDDNRM);
            }
            else
            {
                HNEW = Math.Sqrt(HG * HUB);
            }
            ITER += 1;
            // C-----------------------------------------------------------------------
            // C Test the stopping conditions.
            // C Stop if the new and previous h values differ by a factor of .lt. 2.
            // C Stop if four iterations have been done.  Also, stop with previous h
            // C if HNEW/HG .gt. 2 after first iteration, as this probably means that
            // C the second derivative value is bad because of cancellation error.
            // C-----------------------------------------------------------------------
            if (ITER >= 4) goto LABEL80;
            HRAT = HNEW / HG;
            if ((HRAT > HALF) && (HRAT < TWO)) goto LABEL80;
            if ((ITER >= 2) && (HNEW > TWO * HG))
            {
                HNEW = HG;
                goto LABEL80;
            }
            HG = HNEW;
            goto LABEL50;
            // C
            // C Iteration done.  Apply bounds, bias factor, and sign.  Then exit. ----
        LABEL80:  H0 = HNEW * HALF;
            if (H0 < HLB) H0 = HLB;
            if (H0 > HUB) H0 = HUB;
        LABEL90:  H0 = FortranLib.Sign(H0,TOUT - T0);
            NITER = ITER;
            IER = 0;
            return;
            // C Error return for TOUT - T0 too small. --------------------------------
        LABEL100:  IER =  - 1;
            return;
            // C----------------------- End of Subroutine DVHIN -----------------------
            #endregion
        }
    }

    #endregion


    #region The Class: DVINDY
    
    // *DECK DVINDY
    public class DVINDY
    {
    
        #region Dependencies
        
        DSCAL _dscal; XERRWD _xerrwd; 
        #endregion
        #region Common variables
        
        #region Common Block: DVOD01 Declaration
        
        CommonBlock _dvod01;
        Odouble ACNRM; Odouble CCMXJ; Odouble CONP; Odouble CRATE; Odouble DRC; Odouble[] EL; int o_el; Odouble ETA; 
        Odouble ETAMAX;Odouble H; Odouble HMIN; Odouble HMXI; Odouble HNEW; Odouble HSCAL; Odouble PRL1; Odouble RC; Odouble RL1; 
        Odouble[] TAU; int o_tau;Odouble[] TQ; int o_tq; Odouble TN; Odouble UROUND; 
        Oint ICF; Oint INIT; Oint IPUP; Oint JCUR; Oint JSTART; Oint JSV; Oint KFLAG; Oint KUTH; Oint L; Oint LMAX; Oint LYH; 
        Oint LEWT;Oint LACOR; Oint LSAVF; Oint LWM; Oint LIWM; Oint LOCJS; Oint MAXORD; Oint METH; Oint MITER; Oint MSBJ; 
        Oint MXHNIL;Oint MXSTEP; Oint N; Oint NEWH; Oint NEWQ; Oint NHNIL; Oint NQ; Oint NQNYH; Oint NQWAIT; Oint NSLJ; Oint NSLP; 
        Oint NYH;
        #endregion
        #region Common Block: DVOD02 Declaration
        
        CommonBlock _dvod02;
        Odouble HU; 
        Oint NCFN; Oint NETF; Oint NFE; Oint NJE; Oint NLU; Oint NNI; Oint NQU; Oint NST; 
        #endregion
        #endregion
        #region Variables
        
        double HUN = 0; double ZERO = 0; 
        #endregion
        public DVINDY(DSCAL dscal, XERRWD xerrwd, CommonBlock DVOD01, CommonBlock DVOD02)
        {
    
            #region Set Dependencies
            
            this._dscal = dscal; this._xerrwd = xerrwd; 
            #endregion
            #region Data Initialization
            
            //HUN/100.0D0
            HUN = 100.0E0;
            //ZERO/0.0D0
            ZERO = 0.0E0;
            #endregion
            #region Common varaible Initialization
            
            #region Common Block: DVOD01 Initialization
            
            this._dvod01 = DVOD01;
            ACNRM = DVOD01.doubleData[0];
            CCMXJ = DVOD01.doubleData[1];
            CONP = DVOD01.doubleData[2];
            CRATE = DVOD01.doubleData[3];
            DRC = DVOD01.doubleData[4];
            //Start Array: EL  StartIndex: 5  LastIndex: 17
            EL = DVOD01.doubleData;
            o_el = 4;  //o_ = StartIndex-1
            //End Array: EL
            ETA = DVOD01.doubleData[18];
            ETAMAX = DVOD01.doubleData[19];
            H = DVOD01.doubleData[20];
            HMIN = DVOD01.doubleData[21];
            HMXI = DVOD01.doubleData[22];
            HNEW = DVOD01.doubleData[23];
            HSCAL = DVOD01.doubleData[24];
            PRL1 = DVOD01.doubleData[25];
            RC = DVOD01.doubleData[26];
            RL1 = DVOD01.doubleData[27];
            //Start Array: TAU  StartIndex: 28  LastIndex: 40
            TAU = DVOD01.doubleData;
            o_tau = 27;  //o_ = StartIndex-1
            //End Array: TAU
            //Start Array: TQ  StartIndex: 41  LastIndex: 45
            TQ = DVOD01.doubleData;
            o_tq = 40;  //o_ = StartIndex-1
            //End Array: TQ
            TN = DVOD01.doubleData[46];
            UROUND = DVOD01.doubleData[47];
            ICF = DVOD01.intData[0];
            INIT = DVOD01.intData[1];
            IPUP = DVOD01.intData[2];
            JCUR = DVOD01.intData[3];
            JSTART = DVOD01.intData[4];
            JSV = DVOD01.intData[5];
            KFLAG = DVOD01.intData[6];
            KUTH = DVOD01.intData[7];
            L = DVOD01.intData[8];
            LMAX = DVOD01.intData[9];
            LYH = DVOD01.intData[10];
            LEWT = DVOD01.intData[11];
            LACOR = DVOD01.intData[12];
            LSAVF = DVOD01.intData[13];
            LWM = DVOD01.intData[14];
            LIWM = DVOD01.intData[15];
            LOCJS = DVOD01.intData[16];
            MAXORD = DVOD01.intData[17];
            METH = DVOD01.intData[18];
            MITER = DVOD01.intData[19];
            MSBJ = DVOD01.intData[20];
            MXHNIL = DVOD01.intData[21];
            MXSTEP = DVOD01.intData[22];
            N = DVOD01.intData[23];
            NEWH = DVOD01.intData[24];
            NEWQ = DVOD01.intData[25];
            NHNIL = DVOD01.intData[26];
            NQ = DVOD01.intData[27];
            NQNYH = DVOD01.intData[28];
            NQWAIT = DVOD01.intData[29];
            NSLJ = DVOD01.intData[30];
            NSLP = DVOD01.intData[31];
            NYH = DVOD01.intData[32];
            #endregion
            #region Common Block: DVOD02 Initialization
            
            this._dvod02 = DVOD02;
            HU = DVOD02.doubleData[0];
            NCFN = DVOD02.intData[0];
            NETF = DVOD02.intData[1];
            NFE = DVOD02.intData[2];
            NJE = DVOD02.intData[3];
            NLU = DVOD02.intData[4];
            NNI = DVOD02.intData[5];
            NQU = DVOD02.intData[6];
            NST = DVOD02.intData[7];
            #endregion
            #endregion
        }
    
        public DVINDY()
        {
    
            #region Initialization Common Blocks
            
            CommonBlock DVOD01 = new CommonBlock(48, 33, 0, 0);
            CommonBlock DVOD02 = new CommonBlock(1, 8, 0, 0);
            #endregion
            #region Dependencies (Initialization)
            
            DSCAL dscal = new DSCAL();
            IUMACH iumach = new IUMACH();
            IXSAV ixsav = new IXSAV(iumach);
            XERRWD xerrwd = new XERRWD(ixsav);
            #endregion
            #region Set Dependencies
            
            this._dscal = dscal; this._xerrwd = xerrwd; 
            #endregion
            #region Data Initialization
            
            //HUN/100.0D0
            HUN = 100.0E0;
            //ZERO/0.0D0
            ZERO = 0.0E0;
            #endregion
            #region Common varaible Initialization
            
            #region Common Block: DVOD01 Initialization
            
            this._dvod01 = DVOD01;
            ACNRM = DVOD01.doubleData[0];
            CCMXJ = DVOD01.doubleData[1];
            CONP = DVOD01.doubleData[2];
            CRATE = DVOD01.doubleData[3];
            DRC = DVOD01.doubleData[4];
            //Start Array: EL  StartIndex: 5  LastIndex: 17
            EL = DVOD01.doubleData;
            o_el = 4;  //o_ = StartIndex-1
            //End Array: EL
            ETA = DVOD01.doubleData[18];
            ETAMAX = DVOD01.doubleData[19];
            H = DVOD01.doubleData[20];
            HMIN = DVOD01.doubleData[21];
            HMXI = DVOD01.doubleData[22];
            HNEW = DVOD01.doubleData[23];
            HSCAL = DVOD01.doubleData[24];
            PRL1 = DVOD01.doubleData[25];
            RC = DVOD01.doubleData[26];
            RL1 = DVOD01.doubleData[27];
            //Start Array: TAU  StartIndex: 28  LastIndex: 40
            TAU = DVOD01.doubleData;
            o_tau = 27;  //o_ = StartIndex-1
            //End Array: TAU
            //Start Array: TQ  StartIndex: 41  LastIndex: 45
            TQ = DVOD01.doubleData;
            o_tq = 40;  //o_ = StartIndex-1
            //End Array: TQ
            TN = DVOD01.doubleData[46];
            UROUND = DVOD01.doubleData[47];
            ICF = DVOD01.intData[0];
            INIT = DVOD01.intData[1];
            IPUP = DVOD01.intData[2];
            JCUR = DVOD01.intData[3];
            JSTART = DVOD01.intData[4];
            JSV = DVOD01.intData[5];
            KFLAG = DVOD01.intData[6];
            KUTH = DVOD01.intData[7];
            L = DVOD01.intData[8];
            LMAX = DVOD01.intData[9];
            LYH = DVOD01.intData[10];
            LEWT = DVOD01.intData[11];
            LACOR = DVOD01.intData[12];
            LSAVF = DVOD01.intData[13];
            LWM = DVOD01.intData[14];
            LIWM = DVOD01.intData[15];
            LOCJS = DVOD01.intData[16];
            MAXORD = DVOD01.intData[17];
            METH = DVOD01.intData[18];
            MITER = DVOD01.intData[19];
            MSBJ = DVOD01.intData[20];
            MXHNIL = DVOD01.intData[21];
            MXSTEP = DVOD01.intData[22];
            N = DVOD01.intData[23];
            NEWH = DVOD01.intData[24];
            NEWQ = DVOD01.intData[25];
            NHNIL = DVOD01.intData[26];
            NQ = DVOD01.intData[27];
            NQNYH = DVOD01.intData[28];
            NQWAIT = DVOD01.intData[29];
            NSLJ = DVOD01.intData[30];
            NSLP = DVOD01.intData[31];
            NYH = DVOD01.intData[32];
            #endregion
            #region Common Block: DVOD02 Initialization
            
            this._dvod02 = DVOD02;
            HU = DVOD02.doubleData[0];
            NCFN = DVOD02.intData[0];
            NETF = DVOD02.intData[1];
            NFE = DVOD02.intData[2];
            NJE = DVOD02.intData[3];
            NLU = DVOD02.intData[4];
            NNI = DVOD02.intData[5];
            NQU = DVOD02.intData[6];
            NST = DVOD02.intData[7];
            #endregion
            #endregion
        }
        /// <param name="IFLAG">
        /// is returned negative if either K or T is out of bounds.
        ///</param>
        public void Run(double T, int K, double[] YH, int offset_yh, int LDYH, ref double[] DKY, int offset_dky, ref int IFLAG)
        {
            #region Variables
            
            double C = 0; double R = 0; double S = 0; double TFUZZ = 0; double TN1 = 0; double TP = 0; int I = 0; int IC = 0; 
            int J = 0;int JB = 0; int JB2 = 0; int JJ = 0; int JJ1 = 0; int JP1 = 0; string MSG = new string(' ', 80); 
            #endregion
            #region Array Index Correction
            
             int o_yh = -1 - LDYH + offset_yh;  int o_dky = -1 + offset_dky; 
            #endregion
            #region Prolog
            
            // C-----------------------------------------------------------------------
            // C Call sequence input -- T, K, YH, LDYH
            // C Call sequence output -- DKY, IFLAG
            // C COMMON block variables accessed:
            // C     /DVOD01/ --  H, TN, UROUND, L, N, NQ
            // C     /DVOD02/ --  HU
            // C
            // C Subroutines called by DVINDY: DSCAL, XERRWD
            // C Function routines called by DVINDY: None
            // C-----------------------------------------------------------------------
            // C DVINDY computes interpolated values of the K-th derivative of the
            // C dependent variable vector y, and stores it in DKY.  This routine
            // C is called within the package with K = 0 and T = TOUT, but may
            // C also be called by the user for any K up to the current order.
            // C (See detailed instructions in the usage documentation.)
            // C-----------------------------------------------------------------------
            // C The computed values in DKY are gotten by interpolation using the
            // C Nordsieck history array YH.  This array corresponds uniquely to a
            // C vector-valued polynomial of degree NQCUR or less, and DKY is set
            // C to the K-th derivative of this polynomial at T.
            // C The formula for DKY is:
            // C              q
            // C  DKY(i)  =  sum  c(j,K) * (T - TN)**(j-K) * H**(-j) * YH(i,j+1)
            // C             j=K
            // C where  c(j,K) = j*(j-1)*...*(j-K+1), q = NQCUR, TN = TCUR, H = HCUR.
            // C The quantities  NQ = NQCUR, L = NQ+1, N, TN, and H are
            // C communicated by COMMON.  The above sum is done in reverse order.
            // C IFLAG is returned negative if either K or T is out of bounds.
            // C
            // C Discussion above and comments in driver explain all variables.
            // C-----------------------------------------------------------------------
            // C
            // C Type declarations for labeled COMMON block DVOD01 --------------------
            // C
            // C
            // C Type declarations for labeled COMMON block DVOD02 --------------------
            // C
            // C
            // C Type declarations for local variables --------------------------------
            // C
            // C-----------------------------------------------------------------------
            // C The following Fortran-77 declaration is to cause the values of the
            // C listed (local) variables to be saved between calls to this integrator.
            // C-----------------------------------------------------------------------
            // C
            // C
            // C
            #endregion
            #region Body
            
            IFLAG = 0;
            if (K < 0 || K > NQ.v) goto LABEL80;
            TFUZZ = HUN * UROUND.v * (TN.v + HU.v);
            TP = TN.v - HU.v - TFUZZ;
            TN1 = TN.v + TFUZZ;
            if ((T - TP) * (T - TN1) > ZERO) goto LABEL90;
            // C
            S = (T - TN.v) / H.v;
            IC = 1;
            if (K == 0) goto LABEL15;
            JJ1 = L.v - K;
            for (JJ = JJ1; JJ <= NQ.v; JJ++)
            {
                IC *= JJ;
            }
        LABEL15:  C = Convert.ToSingle(IC);
            for (I = 1; I <= N.v; I++)
            {
                DKY[I + o_dky] = C * YH[I+L.v * LDYH + o_yh];
            }
            if (K == NQ.v) goto LABEL55;
            JB2 = NQ.v - K;
            for (JB = 1; JB <= JB2; JB++)
            {
                J = NQ.v - JB;
                JP1 = J + 1;
                IC = 1;
                if (K == 0) goto LABEL35;
                JJ1 = JP1 - K;
                for (JJ = JJ1; JJ <= J; JJ++)
                {
                    IC *= JJ;
                }
            LABEL35:  C = Convert.ToSingle(IC);
                for (I = 1; I <= N.v; I++)
                {
                    DKY[I + o_dky] = C * YH[I+JP1 * LDYH + o_yh] + S * DKY[I + o_dky];
                }
            }
            if (K == 0) return;
        LABEL55:  R = Math.Pow(H.v, - K);
            this._dscal.Run(N.v, R, ref DKY, offset_dky, 1);
            return;
            // C
        LABEL80:  FortranLib.Copy(ref MSG , "DVINDY-- K (=I1) illegal      ");
            this._xerrwd.Run(MSG, 30, 51, 1, 1, K
                             , 0, 0, ZERO, ZERO);
            IFLAG =  - 1;
            return;
        LABEL90:  FortranLib.Copy(ref MSG , "DVINDY-- T (=R1) illegal      ");
            this._xerrwd.Run(MSG, 30, 52, 1, 0, 0
                             , 0, 1, T, ZERO);
            FortranLib.Copy(ref MSG , "      T not in interval TCUR - HU (= R1) to TCUR (=R2)      ");
            this._xerrwd.Run(MSG, 60, 52, 1, 0, 0
                             , 0, 2, TP, TN.v);
            IFLAG =  - 2;
            return;
            // C----------------------- End of Subroutine DVINDY ----------------------
            #endregion
        }
    }

    #endregion


    #region The Class: DVSTEP
    
    // *DECK DVSTEP
    public class DVSTEP
    {
    
        #region Dependencies
        
        DVNORM _dvnorm; DVJUST _dvjust; DSCAL _dscal; DVSET _dvset; DAXPY _daxpy; DCOPY _dcopy; 
        #endregion
        #region Common variables
        
        #region Common Block: DVOD01 Declaration
        
        CommonBlock _dvod01;
        Odouble ACNRM; Odouble CCMXJ; Odouble CONP; Odouble CRATE; Odouble DRC; Odouble[] EL; int o_el; Odouble ETA; 
        Odouble ETAMAX;Odouble H; Odouble HMIN; Odouble HMXI; Odouble HNEW; Odouble HSCAL; Odouble PRL1; Odouble RC; Odouble RL1; 
        Odouble[] TAU; int o_tau;Odouble[] TQ; int o_tq; Odouble TN; Odouble UROUND; 
        Oint ICF; Oint INIT; Oint IPUP; Oint JCUR; Oint JSTART; Oint JSV; Oint KFLAG; Oint KUTH; Oint L; Oint LMAX; Oint LYH; 
        Oint LEWT;Oint LACOR; Oint LSAVF; Oint LWM; Oint LIWM; Oint LOCJS; Oint MAXORD; Oint METH; Oint MITER; Oint MSBJ; 
        Oint MXHNIL;Oint MXSTEP; Oint N; Oint NEWH; Oint NEWQ; Oint NHNIL; Oint NQ; Oint NQNYH; Oint NQWAIT; Oint NSLJ; Oint NSLP; 
        Oint NYH;
        #endregion
        #region Common Block: DVOD02 Declaration
        
        CommonBlock _dvod02;
        Odouble HU; 
        Oint NCFN; Oint NETF; Oint NFE; Oint NJE; Oint NLU; Oint NNI; Oint NQU; Oint NST; 
        #endregion
        #endregion
        #region Variables
        
        double ADDON = 0; double BIAS1 = 0; double BIAS2 = 0; double BIAS3 = 0; double ETACF = 0; double ETAMIN = 0; 
        double ETAMX1 = 0;double ETAMX2 = 0; double ETAMX3 = 0; double ETAMXF = 0; double ETAQ = 0; double ETAQM1 = 0; 
        double ONE = 0;double ONEPSM = 0; double THRESH = 0; double ZERO = 0; int KFC = 0; int KFH = 0; int MXNCF = 0; 
        #endregion
        public DVSTEP(DVNORM dvnorm, DVJUST dvjust, DSCAL dscal, DVSET dvset, DAXPY daxpy, DCOPY dcopy, CommonBlock DVOD01, CommonBlock DVOD02)
        {
    
            #region Set Dependencies
            
            this._dvnorm = dvnorm; this._dvjust = dvjust; this._dscal = dscal; this._dvset = dvset; this._daxpy = daxpy; 
            this._dcopy = dcopy;
            #endregion
            #region Data Initialization
            
            //KFC/-3
            KFC =  - 3;
            //KFH/-7
            KFH =  - 7;
            //MXNCF/10
            MXNCF = 10;
            //ADDON/1.0D-6
            ADDON = 1.0E-6;
            //BIAS1/6.0D0
            BIAS1 = 6.0E0;
            //BIAS2/6.0D0
            BIAS2 = 6.0E0;
            //BIAS3/10.0D0
            BIAS3 = 10.0E0;
            //ETACF/0.25D0
            ETACF = 0.25E0;
            //ETAMIN/0.1D0
            ETAMIN = 0.1E0;
            //ETAMXF/0.2D0
            ETAMXF = 0.2E0;
            //ETAMX1/1.0D4
            ETAMX1 = 1.0E4;
            //ETAMX2/10.0D0
            ETAMX2 = 10.0E0;
            //ETAMX3/10.0D0
            ETAMX3 = 10.0E0;
            //ONEPSM/1.00001D0
            ONEPSM = 1.00001E0;
            //THRESH/1.5D0
            THRESH = 1.5E0;
            //ONE/1.0D0
            ONE = 1.0E0;
            //ZERO/0.0D0
            ZERO = 0.0E0;
            #endregion
            #region Common varaible Initialization
            
            #region Common Block: DVOD01 Initialization
            
            this._dvod01 = DVOD01;
            ACNRM = DVOD01.doubleData[0];
            CCMXJ = DVOD01.doubleData[1];
            CONP = DVOD01.doubleData[2];
            CRATE = DVOD01.doubleData[3];
            DRC = DVOD01.doubleData[4];
            //Start Array: EL  StartIndex: 5  LastIndex: 17
            EL = DVOD01.doubleData;
            o_el = 4;  //o_ = StartIndex-1
            //End Array: EL
            ETA = DVOD01.doubleData[18];
            ETAMAX = DVOD01.doubleData[19];
            H = DVOD01.doubleData[20];
            HMIN = DVOD01.doubleData[21];
            HMXI = DVOD01.doubleData[22];
            HNEW = DVOD01.doubleData[23];
            HSCAL = DVOD01.doubleData[24];
            PRL1 = DVOD01.doubleData[25];
            RC = DVOD01.doubleData[26];
            RL1 = DVOD01.doubleData[27];
            //Start Array: TAU  StartIndex: 28  LastIndex: 40
            TAU = DVOD01.doubleData;
            o_tau = 27;  //o_ = StartIndex-1
            //End Array: TAU
            //Start Array: TQ  StartIndex: 41  LastIndex: 45
            TQ = DVOD01.doubleData;
            o_tq = 40;  //o_ = StartIndex-1
            //End Array: TQ
            TN = DVOD01.doubleData[46];
            UROUND = DVOD01.doubleData[47];
            ICF = DVOD01.intData[0];
            INIT = DVOD01.intData[1];
            IPUP = DVOD01.intData[2];
            JCUR = DVOD01.intData[3];
            JSTART = DVOD01.intData[4];
            JSV = DVOD01.intData[5];
            KFLAG = DVOD01.intData[6];
            KUTH = DVOD01.intData[7];
            L = DVOD01.intData[8];
            LMAX = DVOD01.intData[9];
            LYH = DVOD01.intData[10];
            LEWT = DVOD01.intData[11];
            LACOR = DVOD01.intData[12];
            LSAVF = DVOD01.intData[13];
            LWM = DVOD01.intData[14];
            LIWM = DVOD01.intData[15];
            LOCJS = DVOD01.intData[16];
            MAXORD = DVOD01.intData[17];
            METH = DVOD01.intData[18];
            MITER = DVOD01.intData[19];
            MSBJ = DVOD01.intData[20];
            MXHNIL = DVOD01.intData[21];
            MXSTEP = DVOD01.intData[22];
            N = DVOD01.intData[23];
            NEWH = DVOD01.intData[24];
            NEWQ = DVOD01.intData[25];
            NHNIL = DVOD01.intData[26];
            NQ = DVOD01.intData[27];
            NQNYH = DVOD01.intData[28];
            NQWAIT = DVOD01.intData[29];
            NSLJ = DVOD01.intData[30];
            NSLP = DVOD01.intData[31];
            NYH = DVOD01.intData[32];
            #endregion
            #region Common Block: DVOD02 Initialization
            
            this._dvod02 = DVOD02;
            HU = DVOD02.doubleData[0];
            NCFN = DVOD02.intData[0];
            NETF = DVOD02.intData[1];
            NFE = DVOD02.intData[2];
            NJE = DVOD02.intData[3];
            NLU = DVOD02.intData[4];
            NNI = DVOD02.intData[5];
            NQU = DVOD02.intData[6];
            NST = DVOD02.intData[7];
            #endregion
            #endregion
        }
    
        public DVSTEP()
        {
    
            #region Initialization Common Blocks
            
            CommonBlock DVOD01 = new CommonBlock(48, 33, 0, 0);
            CommonBlock DVOD02 = new CommonBlock(1, 8, 0, 0);
            #endregion
            #region Dependencies (Initialization)
            
            DVNORM dvnorm = new DVNORM();
            DAXPY daxpy = new DAXPY();
            DSCAL dscal = new DSCAL();
            DCOPY dcopy = new DCOPY();
            DVSET dvset = new DVSET(DVOD01);
            DVJUST dvjust = new DVJUST(daxpy, DVOD01);
            #endregion
            #region Set Dependencies
            
            this._dvnorm = dvnorm; this._dvjust = dvjust; this._dscal = dscal; this._dvset = dvset; this._daxpy = daxpy; 
            this._dcopy = dcopy;
            #endregion
            #region Data Initialization
            
            //KFC/-3
            KFC =  - 3;
            //KFH/-7
            KFH =  - 7;
            //MXNCF/10
            MXNCF = 10;
            //ADDON/1.0D-6
            ADDON = 1.0E-6;
            //BIAS1/6.0D0
            BIAS1 = 6.0E0;
            //BIAS2/6.0D0
            BIAS2 = 6.0E0;
            //BIAS3/10.0D0
            BIAS3 = 10.0E0;
            //ETACF/0.25D0
            ETACF = 0.25E0;
            //ETAMIN/0.1D0
            ETAMIN = 0.1E0;
            //ETAMXF/0.2D0
            ETAMXF = 0.2E0;
            //ETAMX1/1.0D4
            ETAMX1 = 1.0E4;
            //ETAMX2/10.0D0
            ETAMX2 = 10.0E0;
            //ETAMX3/10.0D0
            ETAMX3 = 10.0E0;
            //ONEPSM/1.00001D0
            ONEPSM = 1.00001E0;
            //THRESH/1.5D0
            THRESH = 1.5E0;
            //ONE/1.0D0
            ONE = 1.0E0;
            //ZERO/0.0D0
            ZERO = 0.0E0;
            #endregion
            #region Common varaible Initialization
            
            #region Common Block: DVOD01 Initialization
            
            this._dvod01 = DVOD01;
            ACNRM = DVOD01.doubleData[0];
            CCMXJ = DVOD01.doubleData[1];
            CONP = DVOD01.doubleData[2];
            CRATE = DVOD01.doubleData[3];
            DRC = DVOD01.doubleData[4];
            //Start Array: EL  StartIndex: 5  LastIndex: 17
            EL = DVOD01.doubleData;
            o_el = 4;  //o_ = StartIndex-1
            //End Array: EL
            ETA = DVOD01.doubleData[18];
            ETAMAX = DVOD01.doubleData[19];
            H = DVOD01.doubleData[20];
            HMIN = DVOD01.doubleData[21];
            HMXI = DVOD01.doubleData[22];
            HNEW = DVOD01.doubleData[23];
            HSCAL = DVOD01.doubleData[24];
            PRL1 = DVOD01.doubleData[25];
            RC = DVOD01.doubleData[26];
            RL1 = DVOD01.doubleData[27];
            //Start Array: TAU  StartIndex: 28  LastIndex: 40
            TAU = DVOD01.doubleData;
            o_tau = 27;  //o_ = StartIndex-1
            //End Array: TAU
            //Start Array: TQ  StartIndex: 41  LastIndex: 45
            TQ = DVOD01.doubleData;
            o_tq = 40;  //o_ = StartIndex-1
            //End Array: TQ
            TN = DVOD01.doubleData[46];
            UROUND = DVOD01.doubleData[47];
            ICF = DVOD01.intData[0];
            INIT = DVOD01.intData[1];
            IPUP = DVOD01.intData[2];
            JCUR = DVOD01.intData[3];
            JSTART = DVOD01.intData[4];
            JSV = DVOD01.intData[5];
            KFLAG = DVOD01.intData[6];
            KUTH = DVOD01.intData[7];
            L = DVOD01.intData[8];
            LMAX = DVOD01.intData[9];
            LYH = DVOD01.intData[10];
            LEWT = DVOD01.intData[11];
            LACOR = DVOD01.intData[12];
            LSAVF = DVOD01.intData[13];
            LWM = DVOD01.intData[14];
            LIWM = DVOD01.intData[15];
            LOCJS = DVOD01.intData[16];
            MAXORD = DVOD01.intData[17];
            METH = DVOD01.intData[18];
            MITER = DVOD01.intData[19];
            MSBJ = DVOD01.intData[20];
            MXHNIL = DVOD01.intData[21];
            MXSTEP = DVOD01.intData[22];
            N = DVOD01.intData[23];
            NEWH = DVOD01.intData[24];
            NEWQ = DVOD01.intData[25];
            NHNIL = DVOD01.intData[26];
            NQ = DVOD01.intData[27];
            NQNYH = DVOD01.intData[28];
            NQWAIT = DVOD01.intData[29];
            NSLJ = DVOD01.intData[30];
            NSLP = DVOD01.intData[31];
            NYH = DVOD01.intData[32];
            #endregion
            #region Common Block: DVOD02 Initialization
            
            this._dvod02 = DVOD02;
            HU = DVOD02.doubleData[0];
            NCFN = DVOD02.intData[0];
            NETF = DVOD02.intData[1];
            NFE = DVOD02.intData[2];
            NJE = DVOD02.intData[3];
            NLU = DVOD02.intData[4];
            NNI = DVOD02.intData[5];
            NQU = DVOD02.intData[6];
            NST = DVOD02.intData[7];
            #endregion
            #endregion
        }
        /// <param name="Y">
        /// = An array of length N used for the dependent variable vector.
        ///</param>
        /// <param name="YH">
        /// = An LDYH by LMAX array containing the dependent variables
        /// and their approximate scaled derivatives, where
        /// LMAX = MAXORD + 1.  YH(i,j+1) contains the approximate
        /// j-th derivative of y(i), scaled by H**j/factorial(j)
        /// (j = 0,1,...,NQ).  On entry for the first step, the first
        /// two columns of YH must be set from the initial values.
        ///</param>
        /// <param name="LDYH">
        /// = A constant integer .ge. N, the first dimension of YH.
        /// N is the number of ODEs in the system.
        ///</param>
        /// <param name="YH1">
        /// = A one-dimensional array occupying the same space as YH.
        ///</param>
        /// <param name="EWT">
        /// = An array of length N containing multiplicative weights
        /// for local error measurements.  Local errors in y(i) are
        /// compared to 1.0/EWT(i) in various error tests.
        ///</param>
        /// <param name="SAVF">
        /// = An array of working storage, of length N.
        /// also used for input of YH(*,MAXORD+2) when JSTART = -1
        /// and MAXORD .lt. the current order NQ.
        ///</param>
        /// <param name="VSAV">
        /// = A work array of length N passed to subroutine VNLS.
        ///</param>
        /// <param name="ACOR">
        /// = A work array of length N, used for the accumulated
        /// corrections.  On a successful return, ACOR(i) contains
        /// the estimated one-step local error in y(i).
        ///</param>
        /// <param name="F">
        /// = Dummy name for the user supplied subroutine for f.
        ///</param>
        /// <param name="JAC">
        /// = Dummy name for the user supplied Jacobian subroutine.
        ///</param>
        /// <param name="PSOL">
        /// = Dummy name for the subroutine passed to VNLS, for
        /// possible use there.
        ///</param>
        /// <param name="VNLS">
        /// = Dummy name for the nonlinear system solving subroutine,
        /// whose real name is dependent on the method used.
        ///</param>
        public void Run(ref double[] Y, int offset_y, ref double[] YH, int offset_yh, int LDYH, ref double[] YH1, int offset_yh1, double[] EWT, int offset_ewt, ref double[] SAVF, int offset_savf
                         , double[] VSAV, int offset_vsav, ref double[] ACOR, int offset_acor, ref double[] WM, int offset_wm, ref int[] IWM, int offset_iwm, IFEX F, IJEX JAC
                         , IFEX PSOL, IDVNLSD VNLS, double[] RPAR, int offset_rpar, int[] IPAR, int offset_ipar)
        {
            #region Variables
            
            double CNQUOT = 0; double DDN = 0; double DSM = 0; double DUP = 0; double ETAQP1 = 0; double FLOTL = 0; double R = 0; 
            double TOLD = 0;int I = 0; int I1 = 0; int I2 = 0; int IBACK = 0; int J = 0; int JB = 0; int NCF = 0; int NFLAG = 0; 
            #endregion
            #region Array Index Correction
            
             int o_y = -1 + offset_y;  int o_yh = -1 - LDYH + offset_yh;  int o_yh1 = -1 + offset_yh1; 
             int o_ewt = -1 + offset_ewt; int o_savf = -1 + offset_savf;  int o_vsav = -1 + offset_vsav; 
             int o_acor = -1 + offset_acor; int o_wm = -1 + offset_wm;  int o_iwm = -1 + offset_iwm; 
             int o_rpar = -1 + offset_rpar; int o_ipar = -1 + offset_ipar; 
            #endregion
            #region Prolog
            
            // C-----------------------------------------------------------------------
            // C Call sequence input -- Y, YH, LDYH, YH1, EWT, SAVF, VSAV,
            // C                        ACOR, WM, IWM, F, JAC, PSOL, VNLS, RPAR, IPAR
            // C Call sequence output -- YH, ACOR, WM, IWM
            // C COMMON block variables accessed:
            // C     /DVOD01/  ACNRM, EL(13), H, HMIN, HMXI, HNEW, HSCAL, RC, TAU(13),
            // C               TQ(5), TN, JCUR, JSTART, KFLAG, KUTH,
            // C               L, LMAX, MAXORD, N, NEWQ, NQ, NQWAIT
            // C     /DVOD02/  HU, NCFN, NETF, NFE, NQU, NST
            // C
            // C Subroutines called by DVSTEP: F, DAXPY, DCOPY, DSCAL,
            // C                               DVJUST, VNLS, DVSET
            // C Function routines called by DVSTEP: DVNORM
            // C-----------------------------------------------------------------------
            // C DVSTEP performs one step of the integration of an initial value
            // C problem for a system of ordinary differential equations.
            // C DVSTEP calls subroutine VNLS for the solution of the nonlinear system
            // C arising in the time step.  Thus it is independent of the problem
            // C Jacobian structure and the type of nonlinear system solution method.
            // C DVSTEP returns a completion flag KFLAG (in COMMON).
            // C A return with KFLAG = -1 or -2 means either ABS(H) = HMIN or 10
            // C consecutive failures occurred.  On a return with KFLAG negative,
            // C the values of TN and the YH array are as of the beginning of the last
            // C step, and H is the last step size attempted.
            // C
            // C Communication with DVSTEP is done with the following variables:
            // C
            // C Y      = An array of length N used for the dependent variable vector.
            // C YH     = An LDYH by LMAX array containing the dependent variables
            // C          and their approximate scaled derivatives, where
            // C          LMAX = MAXORD + 1.  YH(i,j+1) contains the approximate
            // C          j-th derivative of y(i), scaled by H**j/factorial(j)
            // C          (j = 0,1,...,NQ).  On entry for the first step, the first
            // C          two columns of YH must be set from the initial values.
            // C LDYH   = A constant integer .ge. N, the first dimension of YH.
            // C          N is the number of ODEs in the system.
            // C YH1    = A one-dimensional array occupying the same space as YH.
            // C EWT    = An array of length N containing multiplicative weights
            // C          for local error measurements.  Local errors in y(i) are
            // C          compared to 1.0/EWT(i) in various error tests.
            // C SAVF   = An array of working storage, of length N.
            // C          also used for input of YH(*,MAXORD+2) when JSTART = -1
            // C          and MAXORD .lt. the current order NQ.
            // C VSAV   = A work array of length N passed to subroutine VNLS.
            // C ACOR   = A work array of length N, used for the accumulated
            // C          corrections.  On a successful return, ACOR(i) contains
            // C          the estimated one-step local error in y(i).
            // C WM,IWM = Real and integer work arrays associated with matrix
            // C          operations in VNLS.
            // C F      = Dummy name for the user supplied subroutine for f.
            // C JAC    = Dummy name for the user supplied Jacobian subroutine.
            // C PSOL   = Dummy name for the subroutine passed to VNLS, for
            // C          possible use there.
            // C VNLS   = Dummy name for the nonlinear system solving subroutine,
            // C          whose real name is dependent on the method used.
            // C RPAR, IPAR = Dummy names for user's real and integer work arrays.
            // C-----------------------------------------------------------------------
            // C
            // C Type declarations for labeled COMMON block DVOD01 --------------------
            // C
            // C
            // C Type declarations for labeled COMMON block DVOD02 --------------------
            // C
            // C
            // C Type declarations for local variables --------------------------------
            // C
            // C
            // C Type declaration for function subroutines called ---------------------
            // C
            // C-----------------------------------------------------------------------
            // C The following Fortran-77 declaration is to cause the values of the
            // C listed (local) variables to be saved between calls to this integrator.
            // C-----------------------------------------------------------------------
            // C-----------------------------------------------------------------------
            // C
            // C
            #endregion
            #region Body
            
            KFLAG.v = 0;
            TOLD = TN.v;
            NCF = 0;
            JCUR.v = 0;
            NFLAG = 0;
            if (JSTART.v > 0) goto LABEL20;
            if (JSTART.v ==  - 1) goto LABEL100;
            // C-----------------------------------------------------------------------
            // C On the first call, the order is set to 1, and other variables are
            // C initialized.  ETAMAX is the maximum ratio by which H can be increased
            // C in a single step.  It is normally 10, but is larger during the
            // C first step to compensate for the small initial H.  If a failure
            // C occurs (in corrector convergence or error test), ETAMAX is set to 1
            // C for the next increase.
            // C-----------------------------------------------------------------------
            LMAX.v = MAXORD.v + 1;
            NQ.v = 1;
            L.v = 2;
            NQNYH.v = NQ.v * LDYH;
            TAU[1 + o_tau].v = H.v;
            PRL1.v = ONE;
            RC.v = ZERO;
            ETAMAX.v = ETAMX1;
            NQWAIT.v = 2;
            HSCAL.v = H.v;
            goto LABEL200;
            // C-----------------------------------------------------------------------
            // C Take preliminary actions on a normal continuation step (JSTART.GT.0).
            // C If the driver changed H, then ETA must be reset and NEWH set to 1.
            // C If a change of order was dictated on the previous step, then
            // C it is done here and appropriate adjustments in the history are made.
            // C On an order decrease, the history array is adjusted by DVJUST.
            // C On an order increase, the history array is augmented by a column.
            // C On a change of step size H, the history array YH is rescaled.
            // C-----------------------------------------------------------------------
        LABEL20:;
            if (KUTH.v == 1)
            {
                ETA.v = Math.Min(ETA.v, H.v / HSCAL.v);
                NEWH.v = 1;
            }
        LABEL50:  
            if (NEWH.v == 0) goto LABEL200;
            if (NEWQ.v == NQ.v) goto LABEL150;
            if (NEWQ.v < NQ.v)
            {
                this._dvjust.Run(ref YH, offset_yh, LDYH,  - 1);
                NQ.v = NEWQ.v;
                L.v = NQ.v + 1;
                NQWAIT.v = L.v;
                goto LABEL150;
            }
            if (NEWQ.v > NQ.v)
            {
                this._dvjust.Run(ref YH, offset_yh, LDYH, 1);
                NQ.v = NEWQ.v;
                L.v = NQ.v + 1;
                NQWAIT.v = L.v;
                goto LABEL150;
            }
            // C-----------------------------------------------------------------------
            // C The following block handles preliminaries needed when JSTART = -1.
            // C If N was reduced, zero out part of YH to avoid undefined references.
            // C If MAXORD was reduced to a value less than the tentative order NEWQ,
            // C then NQ is set to MAXORD, and a new H ratio ETA is chosen.
            // C Otherwise, we take the same preliminary actions as for JSTART .gt. 0.
            // C In any case, NQWAIT is reset to L = NQ + 1 to prevent further
            // C changes in order for that many steps.
            // C The new H ratio ETA is limited by the input H if KUTH = 1,
            // C by HMIN if KUTH = 0, and by HMXI in any case.
            // C Finally, the history array YH is rescaled.
            // C-----------------------------------------------------------------------
        LABEL100:;
            LMAX.v = MAXORD.v + 1;
            if (N.v == LDYH) goto LABEL120;
            I1 = 1 + (NEWQ.v + 1) * LDYH;
            I2 = (MAXORD.v + 1) * LDYH;
            if (I1 > I2) goto LABEL120;
            for (I = I1; I <= I2; I++)
            {
                YH1[I + o_yh1] = ZERO;
            }
        LABEL120:  
            if (NEWQ.v <= MAXORD.v) goto LABEL140;
            FLOTL = Convert.ToSingle(LMAX.v);
            if (MAXORD.v < NQ.v - 1)
            {
                DDN = this._dvnorm.Run(N.v, SAVF, offset_savf, EWT, offset_ewt) / TQ[1 + o_tq].v;
                ETA.v = ONE / (Math.Pow(BIAS1 * DDN,ONE / FLOTL) + ADDON);
            }
            if (MAXORD.v == NQ.v && NEWQ.v == NQ.v + 1) ETA.v = ETAQ;
            if (MAXORD.v == NQ.v - 1 && NEWQ.v == NQ.v + 1)
            {
                ETA.v = ETAQM1;
                this._dvjust.Run(ref YH, offset_yh, LDYH,  - 1);
            }
            if (MAXORD.v == NQ.v - 1 && NEWQ.v == NQ.v)
            {
                DDN = this._dvnorm.Run(N.v, SAVF, offset_savf, EWT, offset_ewt) / TQ[1 + o_tq].v;
                ETA.v = ONE / (Math.Pow(BIAS1 * DDN,ONE / FLOTL) + ADDON);
                this._dvjust.Run(ref YH, offset_yh, LDYH,  - 1);
            }
            ETA.v = Math.Min(ETA.v, ONE);
            NQ.v = MAXORD.v;
            L.v = LMAX.v;
        LABEL140:  
            if (KUTH.v == 1) ETA.v = Math.Min(ETA.v, Math.Abs(H.v / HSCAL.v));
            if (KUTH.v == 0) ETA.v = Math.Max(ETA.v, HMIN.v / Math.Abs(HSCAL.v));
            ETA.v /= Math.Max(ONE, Math.Abs(HSCAL.v) * HMXI.v * ETA.v);
            NEWH.v = 1;
            NQWAIT.v = L.v;
            if (NEWQ.v <= MAXORD.v) goto LABEL50;
            // C Rescale the history array for a change in H by a factor of ETA. ------
        LABEL150:  R = ONE;
            for (J = 2; J <= L.v; J++)
            {
                R *= ETA.v;
                this._dscal.Run(N.v, R, ref YH, 1+J * LDYH + o_yh, 1);
            }
            H.v = HSCAL.v * ETA.v;
            HSCAL.v = H.v;
            RC.v *= ETA.v;
            NQNYH.v = NQ.v * LDYH;
            // C-----------------------------------------------------------------------
            // C This section computes the predicted values by effectively
            // C multiplying the YH array by the Pascal triangle matrix.
            // C DVSET is called to calculate all integration coefficients.
            // C RC is the ratio of new to old values of the coefficient H/EL(2)=h/l1.
            // C-----------------------------------------------------------------------
        LABEL200:  TN.v += H.v;
            I1 = NQNYH.v + 1;
            for (JB = 1; JB <= NQ.v; JB++)
            {
                I1 -= LDYH;
                for (I = I1; I <= NQNYH.v; I++)
                {
                    YH1[I + o_yh1] += YH1[I + LDYH + o_yh1];
                }
            }
            this._dvset.Run();
            RL1.v = ONE / EL[2 + o_el].v;
            RC.v = RC.v * (RL1.v / PRL1.v);
            PRL1.v = RL1.v;
            // C
            // C Call the nonlinear system solver. ------------------------------------
            // C
            VNLS.Run(ref Y, offset_y, YH, offset_yh, LDYH, VSAV, offset_vsav, ref SAVF, offset_savf, EWT, offset_ewt
                     , ref ACOR, offset_acor, ref IWM, offset_iwm, ref WM, offset_wm, F, JAC, PSOL
                     , ref NFLAG, RPAR, offset_rpar, IPAR, offset_ipar);
            // C
            if (NFLAG == 0) goto LABEL450;
            // C-----------------------------------------------------------------------
            // C The VNLS routine failed to achieve convergence (NFLAG .NE. 0).
            // C The YH array is retracted to its values before prediction.
            // C The step size H is reduced and the step is retried, if possible.
            // C Otherwise, an error exit is taken.
            // C-----------------------------------------------------------------------
            NCF += 1;
            NCFN.v += 1;
            ETAMAX.v = ONE;
            TN.v = TOLD;
            I1 = NQNYH.v + 1;
            for (JB = 1; JB <= NQ.v; JB++)
            {
                I1 -= LDYH;
                for (I = I1; I <= NQNYH.v; I++)
                {
                    YH1[I + o_yh1] -= YH1[I + LDYH + o_yh1];
                }
            }
            if (NFLAG <  - 1) goto LABEL680;
            if (Math.Abs(H.v) <= HMIN.v * ONEPSM) goto LABEL670;
            if (NCF == MXNCF) goto LABEL670;
            ETA.v = ETACF;
            ETA.v = Math.Max(ETA.v, HMIN.v / Math.Abs(H.v));
            NFLAG =  - 1;
            goto LABEL150;
            // C-----------------------------------------------------------------------
            // C The corrector has converged (NFLAG = 0).  The local error test is
            // C made and control passes to statement 500 if it fails.
            // C-----------------------------------------------------------------------
        LABEL450:;
            DSM = ACNRM.v / TQ[2 + o_tq].v;
            if (DSM > ONE) goto LABEL500;
            // C-----------------------------------------------------------------------
            // C After a successful step, update the YH and TAU arrays and decrement
            // C NQWAIT.  If NQWAIT is then 1 and NQ .lt. MAXORD, then ACOR is saved
            // C for use in a possible order increase on the next step.
            // C If ETAMAX = 1 (a failure occurred this step), keep NQWAIT .ge. 2.
            // C-----------------------------------------------------------------------
            KFLAG.v = 0;
            NST.v += 1;
            HU.v = H.v;
            NQU.v = NQ.v;
            for (IBACK = 1; IBACK <= NQ.v; IBACK++)
            {
                I = L.v - IBACK;
                TAU[I + 1 + o_tau].v = TAU[I + o_tau].v;
            }
            TAU[1 + o_tau].v = H.v;
            for (J = 1; J <= L.v; J++)
            {
                this._daxpy.Run(N.v, EL[J + o_el].v, ACOR, offset_acor, 1, ref YH, 1+J * LDYH + o_yh, 1);
            }
            NQWAIT.v -= 1;
            if ((L.v == LMAX.v) || (NQWAIT.v != 1)) goto LABEL490;
            this._dcopy.Run(N.v, ACOR, offset_acor, 1, ref YH, 1+LMAX.v * LDYH + o_yh, 1);
            CONP.v = TQ[5 + o_tq].v;
        LABEL490:  
            if (ETAMAX.v != ONE) goto LABEL560;
            if (NQWAIT.v < 2) NQWAIT.v = 2;
            NEWQ.v = NQ.v;
            NEWH.v = 0;
            ETA.v = ONE;
            HNEW.v = H.v;
            goto LABEL690;
            // C-----------------------------------------------------------------------
            // C The error test failed.  KFLAG keeps track of multiple failures.
            // C Restore TN and the YH array to their previous values, and prepare
            // C to try the step again.  Compute the optimum step size for the
            // C same order.  After repeated failures, H is forced to decrease
            // C more rapidly.
            // C-----------------------------------------------------------------------
        LABEL500:  KFLAG.v -= 1;
            NETF.v += 1;
            NFLAG =  - 2;
            TN.v = TOLD;
            I1 = NQNYH.v + 1;
            for (JB = 1; JB <= NQ.v; JB++)
            {
                I1 -= LDYH;
                for (I = I1; I <= NQNYH.v; I++)
                {
                    YH1[I + o_yh1] -= YH1[I + LDYH + o_yh1];
                }
            }
            if (Math.Abs(H.v) <= HMIN.v * ONEPSM) goto LABEL660;
            ETAMAX.v = ONE;
            if (KFLAG.v <= KFC) goto LABEL530;
            // C Compute ratio of new H to current H at the current order. ------------
            FLOTL = Convert.ToSingle(L.v);
            ETA.v = ONE / (Math.Pow(BIAS2 * DSM,ONE / FLOTL) + ADDON);
            ETA.v = Math.Max(ETA.v, Math.Max(HMIN.v / Math.Abs(H.v), ETAMIN));
            if ((KFLAG.v <=  - 2) && (ETA.v > ETAMXF)) ETA.v = ETAMXF;
            goto LABEL150;
            // C-----------------------------------------------------------------------
            // C Control reaches this section if 3 or more consecutive failures
            // C have occurred.  It is assumed that the elements of the YH array
            // C have accumulated errors of the wrong order.  The order is reduced
            // C by one, if possible.  Then H is reduced by a factor of 0.1 and
            // C the step is retried.  After a total of 7 consecutive failures,
            // C an exit is taken with KFLAG = -1.
            // C-----------------------------------------------------------------------
        LABEL530:  
            if (KFLAG.v == KFH) goto LABEL660;
            if (NQ.v == 1) goto LABEL540;
            ETA.v = Math.Max(ETAMIN, HMIN.v / Math.Abs(H.v));
            this._dvjust.Run(ref YH, offset_yh, LDYH,  - 1);
            L.v = NQ.v;
            NQ.v -= 1;
            NQWAIT.v = L.v;
            goto LABEL150;
        LABEL540:  ETA.v = Math.Max(ETAMIN, HMIN.v / Math.Abs(H.v));
            H.v *= ETA.v;
            HSCAL.v = H.v;
            TAU[1 + o_tau].v = H.v;
            F.Run(N.v, TN.v, Y, offset_y, ref SAVF, offset_savf, RPAR[1 + o_rpar], IPAR[1 + o_ipar]);
            NFE.v += 1;
            for (I = 1; I <= N.v; I++)
            {
                YH[I+2 * LDYH + o_yh] = H.v * SAVF[I + o_savf];
            }
            NQWAIT.v = 10;
            goto LABEL200;
            // C-----------------------------------------------------------------------
            // C If NQWAIT = 0, an increase or decrease in order by one is considered.
            // C Factors ETAQ, ETAQM1, ETAQP1 are computed by which H could
            // C be multiplied at order q, q-1, or q+1, respectively.
            // C The largest of these is determined, and the new order and
            // C step size set accordingly.
            // C A change of H or NQ is made only if H increases by at least a
            // C factor of THRESH.  If an order change is considered and rejected,
            // C then NQWAIT is set to 2 (reconsider it after 2 steps).
            // C-----------------------------------------------------------------------
            // C Compute ratio of new H to current H at the current order. ------------
        LABEL560:  FLOTL = Convert.ToSingle(L.v);
            ETAQ = ONE / (Math.Pow(BIAS2 * DSM,ONE / FLOTL) + ADDON);
            if (NQWAIT.v != 0) goto LABEL600;
            NQWAIT.v = 2;
            ETAQM1 = ZERO;
            if (NQ.v == 1) goto LABEL570;
            // C Compute ratio of new H to current H at the current order less one. ---
            DDN = this._dvnorm.Run(N.v, YH, 1+L.v * LDYH + o_yh, EWT, offset_ewt) / TQ[1 + o_tq].v;
            ETAQM1 = ONE / (Math.Pow(BIAS1 * DDN,ONE / (FLOTL - ONE)) + ADDON);
        LABEL570:  ETAQP1 = ZERO;
            if (L.v == LMAX.v) goto LABEL580;
            // C Compute ratio of new H to current H at current order plus one. -------
            CNQUOT = (TQ[5 + o_tq].v / CONP.v) * Math.Pow(H.v / TAU[2 + o_tau].v,L.v);
            for (I = 1; I <= N.v; I++)
            {
                SAVF[I + o_savf] = ACOR[I + o_acor] - CNQUOT * YH[I+LMAX.v * LDYH + o_yh];
            }
            DUP = this._dvnorm.Run(N.v, SAVF, offset_savf, EWT, offset_ewt) / TQ[3 + o_tq].v;
            ETAQP1 = ONE / (Math.Pow(BIAS3 * DUP,ONE / (FLOTL + ONE)) + ADDON);
        LABEL580:  
            if (ETAQ >= ETAQP1) goto LABEL590;
            if (ETAQP1 > ETAQM1) goto LABEL620;
            goto LABEL610;
        LABEL590:  
            if (ETAQ < ETAQM1) goto LABEL610;
        LABEL600:  ETA.v = ETAQ;
            NEWQ.v = NQ.v;
            goto LABEL630;
        LABEL610:  ETA.v = ETAQM1;
            NEWQ.v = NQ.v - 1;
            goto LABEL630;
        LABEL620:  ETA.v = ETAQP1;
            NEWQ.v = NQ.v + 1;
            this._dcopy.Run(N.v, ACOR, offset_acor, 1, ref YH, 1+LMAX.v * LDYH + o_yh, 1);
            // C Test tentative new H against THRESH, ETAMAX, and HMXI, then exit. ----
        LABEL630:  
            if (ETA.v < THRESH || ETAMAX.v == ONE) goto LABEL640;
            ETA.v = Math.Min(ETA.v, ETAMAX.v);
            ETA.v /= Math.Max(ONE, Math.Abs(H.v) * HMXI.v * ETA.v);
            NEWH.v = 1;
            HNEW.v = H.v * ETA.v;
            goto LABEL690;
        LABEL640:  NEWQ.v = NQ.v;
            NEWH.v = 0;
            ETA.v = ONE;
            HNEW.v = H.v;
            goto LABEL690;
            // C-----------------------------------------------------------------------
            // C All returns are made through this section.
            // C On a successful return, ETAMAX is reset and ACOR is scaled.
            // C-----------------------------------------------------------------------
        LABEL660:  KFLAG.v =  - 1;
            goto LABEL720;
        LABEL670:  KFLAG.v =  - 2;
            goto LABEL720;
        LABEL680:  
            if (NFLAG ==  - 2) KFLAG.v =  - 3;
            if (NFLAG ==  - 3) KFLAG.v =  - 4;
            goto LABEL720;
        LABEL690:  ETAMAX.v = ETAMX3;
            if (NST.v <= 10) ETAMAX.v = ETAMX2;
            R = ONE / TQ[2 + o_tq].v;
            this._dscal.Run(N.v, R, ref ACOR, offset_acor, 1);
        LABEL720:  JSTART.v = 1;
            return;
            // C----------------------- End of Subroutine DVSTEP ----------------------
            #endregion
        }
    }

    #endregion


    #region The Class: DVSET
    
    // *DECK DVSET
    public class DVSET
    {
    
        #region Common variables
        
        #region Common Block: DVOD01 Declaration
        
        CommonBlock _dvod01;
        Odouble ACNRM; Odouble CCMXJ; Odouble CONP; Odouble CRATE; Odouble DRC; Odouble[] EL; int o_el; Odouble ETA; 
        Odouble ETAMAX;Odouble H; Odouble HMIN; Odouble HMXI; Odouble HNEW; Odouble HSCAL; Odouble PRL1; Odouble RC; Odouble RL1; 
        Odouble[] TAU; int o_tau;Odouble[] TQ; int o_tq; Odouble TN; Odouble UROUND; 
        Oint ICF; Oint INIT; Oint IPUP; Oint JCUR; Oint JSTART; Oint JSV; Oint KFLAG; Oint KUTH; Oint L; Oint LMAX; Oint LYH; 
        Oint LEWT;Oint LACOR; Oint LSAVF; Oint LWM; Oint LIWM; Oint LOCJS; Oint MAXORD; Oint METH; Oint MITER; Oint MSBJ; 
        Oint MXHNIL;Oint MXSTEP; Oint N; Oint NEWH; Oint NEWQ; Oint NHNIL; Oint NQ; Oint NQNYH; Oint NQWAIT; Oint NSLJ; Oint NSLP; 
        Oint NYH;
        #endregion
        #endregion
        #region Variables
        
        double CORTES = 0; double[] EM = new double[13]; double ONE = 0; double SIX = 0; double TWO = 0; double ZERO = 0; 
        #endregion
        public DVSET(CommonBlock DVOD01)
        {
    
            #region Data Initialization
            
            //CORTES/0.1D0
            CORTES = 0.1E0;
            //ONE/1.0D0
            ONE = 1.0E0;
            //SIX/6.0D0
            SIX = 6.0E0;
            //TWO/2.0D0
            TWO = 2.0E0;
            //ZERO/0.0D0
            ZERO = 0.0E0;
            #endregion
            #region Common varaible Initialization
            
            #region Common Block: DVOD01 Initialization
            
            this._dvod01 = DVOD01;
            ACNRM = DVOD01.doubleData[0];
            CCMXJ = DVOD01.doubleData[1];
            CONP = DVOD01.doubleData[2];
            CRATE = DVOD01.doubleData[3];
            DRC = DVOD01.doubleData[4];
            //Start Array: EL  StartIndex: 5  LastIndex: 17
            EL = DVOD01.doubleData;
            o_el = 4;  //o_ = StartIndex-1
            //End Array: EL
            ETA = DVOD01.doubleData[18];
            ETAMAX = DVOD01.doubleData[19];
            H = DVOD01.doubleData[20];
            HMIN = DVOD01.doubleData[21];
            HMXI = DVOD01.doubleData[22];
            HNEW = DVOD01.doubleData[23];
            HSCAL = DVOD01.doubleData[24];
            PRL1 = DVOD01.doubleData[25];
            RC = DVOD01.doubleData[26];
            RL1 = DVOD01.doubleData[27];
            //Start Array: TAU  StartIndex: 28  LastIndex: 40
            TAU = DVOD01.doubleData;
            o_tau = 27;  //o_ = StartIndex-1
            //End Array: TAU
            //Start Array: TQ  StartIndex: 41  LastIndex: 45
            TQ = DVOD01.doubleData;
            o_tq = 40;  //o_ = StartIndex-1
            //End Array: TQ
            TN = DVOD01.doubleData[46];
            UROUND = DVOD01.doubleData[47];
            ICF = DVOD01.intData[0];
            INIT = DVOD01.intData[1];
            IPUP = DVOD01.intData[2];
            JCUR = DVOD01.intData[3];
            JSTART = DVOD01.intData[4];
            JSV = DVOD01.intData[5];
            KFLAG = DVOD01.intData[6];
            KUTH = DVOD01.intData[7];
            L = DVOD01.intData[8];
            LMAX = DVOD01.intData[9];
            LYH = DVOD01.intData[10];
            LEWT = DVOD01.intData[11];
            LACOR = DVOD01.intData[12];
            LSAVF = DVOD01.intData[13];
            LWM = DVOD01.intData[14];
            LIWM = DVOD01.intData[15];
            LOCJS = DVOD01.intData[16];
            MAXORD = DVOD01.intData[17];
            METH = DVOD01.intData[18];
            MITER = DVOD01.intData[19];
            MSBJ = DVOD01.intData[20];
            MXHNIL = DVOD01.intData[21];
            MXSTEP = DVOD01.intData[22];
            N = DVOD01.intData[23];
            NEWH = DVOD01.intData[24];
            NEWQ = DVOD01.intData[25];
            NHNIL = DVOD01.intData[26];
            NQ = DVOD01.intData[27];
            NQNYH = DVOD01.intData[28];
            NQWAIT = DVOD01.intData[29];
            NSLJ = DVOD01.intData[30];
            NSLP = DVOD01.intData[31];
            NYH = DVOD01.intData[32];
            #endregion
            #endregion
        }
    
        public DVSET()
        {
    
            #region Initialization Common Blocks
            
            CommonBlock DVOD01 = new CommonBlock(48, 33, 0, 0);
            #endregion
            #region Data Initialization
            
            //CORTES/0.1D0
            CORTES = 0.1E0;
            //ONE/1.0D0
            ONE = 1.0E0;
            //SIX/6.0D0
            SIX = 6.0E0;
            //TWO/2.0D0
            TWO = 2.0E0;
            //ZERO/0.0D0
            ZERO = 0.0E0;
            #endregion
            #region Common varaible Initialization
            
            #region Common Block: DVOD01 Initialization
            
            this._dvod01 = DVOD01;
            ACNRM = DVOD01.doubleData[0];
            CCMXJ = DVOD01.doubleData[1];
            CONP = DVOD01.doubleData[2];
            CRATE = DVOD01.doubleData[3];
            DRC = DVOD01.doubleData[4];
            //Start Array: EL  StartIndex: 5  LastIndex: 17
            EL = DVOD01.doubleData;
            o_el = 4;  //o_ = StartIndex-1
            //End Array: EL
            ETA = DVOD01.doubleData[18];
            ETAMAX = DVOD01.doubleData[19];
            H = DVOD01.doubleData[20];
            HMIN = DVOD01.doubleData[21];
            HMXI = DVOD01.doubleData[22];
            HNEW = DVOD01.doubleData[23];
            HSCAL = DVOD01.doubleData[24];
            PRL1 = DVOD01.doubleData[25];
            RC = DVOD01.doubleData[26];
            RL1 = DVOD01.doubleData[27];
            //Start Array: TAU  StartIndex: 28  LastIndex: 40
            TAU = DVOD01.doubleData;
            o_tau = 27;  //o_ = StartIndex-1
            //End Array: TAU
            //Start Array: TQ  StartIndex: 41  LastIndex: 45
            TQ = DVOD01.doubleData;
            o_tq = 40;  //o_ = StartIndex-1
            //End Array: TQ
            TN = DVOD01.doubleData[46];
            UROUND = DVOD01.doubleData[47];
            ICF = DVOD01.intData[0];
            INIT = DVOD01.intData[1];
            IPUP = DVOD01.intData[2];
            JCUR = DVOD01.intData[3];
            JSTART = DVOD01.intData[4];
            JSV = DVOD01.intData[5];
            KFLAG = DVOD01.intData[6];
            KUTH = DVOD01.intData[7];
            L = DVOD01.intData[8];
            LMAX = DVOD01.intData[9];
            LYH = DVOD01.intData[10];
            LEWT = DVOD01.intData[11];
            LACOR = DVOD01.intData[12];
            LSAVF = DVOD01.intData[13];
            LWM = DVOD01.intData[14];
            LIWM = DVOD01.intData[15];
            LOCJS = DVOD01.intData[16];
            MAXORD = DVOD01.intData[17];
            METH = DVOD01.intData[18];
            MITER = DVOD01.intData[19];
            MSBJ = DVOD01.intData[20];
            MXHNIL = DVOD01.intData[21];
            MXSTEP = DVOD01.intData[22];
            N = DVOD01.intData[23];
            NEWH = DVOD01.intData[24];
            NEWQ = DVOD01.intData[25];
            NHNIL = DVOD01.intData[26];
            NQ = DVOD01.intData[27];
            NQNYH = DVOD01.intData[28];
            NQWAIT = DVOD01.intData[29];
            NSLJ = DVOD01.intData[30];
            NSLP = DVOD01.intData[31];
            NYH = DVOD01.intData[32];
            #endregion
            #endregion
        }
        public void Run()
        {
            #region Variables
            
            double AHATN0 = 0; double ALPH0 = 0; double CNQM1 = 0; double CSUM = 0; double ELP = 0; 
            int offset_em = 0; int o_em = -1;double EM0 = 0; double FLOTI = 0; double FLOTL = 0; double FLOTNQ = 0; 
            double HSUM = 0;double RXI = 0; double RXIS = 0; double S = 0; double T1 = 0; double T2 = 0; double T3 = 0; 
            double T4 = 0;double T5 = 0; double T6 = 0; double XI = 0; int I = 0; int IBACK = 0; int J = 0; int JP1 = 0; 
            int NQM1 = 0;int NQM2 = 0; 
            #endregion
            #region Prolog
            
            // C-----------------------------------------------------------------------
            // C Call sequence communication: None
            // C COMMON block variables accessed:
            // C     /DVOD01/ -- EL(13), H, TAU(13), TQ(5), L(= NQ + 1),
            // C                 METH, NQ, NQWAIT
            // C
            // C Subroutines called by DVSET: None
            // C Function routines called by DVSET: None
            // C-----------------------------------------------------------------------
            // C DVSET is called by DVSTEP and sets coefficients for use there.
            // C
            // C For each order NQ, the coefficients in EL are calculated by use of
            // C  the generating polynomial lambda(x), with coefficients EL(i).
            // C      lambda(x) = EL(1) + EL(2)*x + ... + EL(NQ+1)*(x**NQ).
            // C For the backward differentiation formulas,
            // C                                     NQ-1
            // C      lambda(x) = (1 + x/xi*(NQ)) * product (1 + x/xi(i) ) .
            // C                                     i = 1
            // C For the Adams formulas,
            // C                              NQ-1
            // C      (d/dx) lambda(x) = c * product (1 + x/xi(i) ) ,
            // C                              i = 1
            // C      lambda(-1) = 0,    lambda(0) = 1,
            // C where c is a normalization constant.
            // C In both cases, xi(i) is defined by
            // C      H*xi(i) = t sub n  -  t sub (n-i)
            // C              = H + TAU(1) + TAU(2) + ... TAU(i-1).
            // C
            // C
            // C In addition to variables described previously, communication
            // C with DVSET uses the following:
            // C   TAU    = A vector of length 13 containing the past NQ values
            // C            of H.
            // C   EL     = A vector of length 13 in which vset stores the
            // C            coefficients for the corrector formula.
            // C   TQ     = A vector of length 5 in which vset stores constants
            // C            used for the convergence test, the error test, and the
            // C            selection of H at a new order.
            // C   METH   = The basic method indicator.
            // C   NQ     = The current order.
            // C   L      = NQ + 1, the length of the vector stored in EL, and
            // C            the number of columns of the YH array being used.
            // C   NQWAIT = A counter controlling the frequency of order changes.
            // C            An order change is about to be considered if NQWAIT = 1.
            // C-----------------------------------------------------------------------
            // C
            // C Type declarations for labeled COMMON block DVOD01 --------------------
            // C
            // C
            // C Type declarations for local variables --------------------------------
            // C
            // C
            // C-----------------------------------------------------------------------
            // C The following Fortran-77 declaration is to cause the values of the
            // C listed (local) variables to be saved between calls to this integrator.
            // C-----------------------------------------------------------------------
            // C
            // C
            // C
            #endregion
            #region Body
            
            FLOTL = Convert.ToSingle(L.v);
            NQM1 = NQ.v - 1;
            NQM2 = NQ.v - 2;
            switch (METH.v)
            {
                case 1: goto LABEL100;
                case 2: goto LABEL200;
            }
            // C
            // C Set coefficients for Adams methods. ----------------------------------
        LABEL100:  
            if (NQ.v != 1) goto LABEL110;
            EL[1 + o_el].v = ONE;
            EL[2 + o_el].v = ONE;
            TQ[1 + o_tq].v = ONE;
            TQ[2 + o_tq].v = TWO;
            TQ[3 + o_tq].v = SIX * TQ[2 + o_tq].v;
            TQ[5 + o_tq].v = ONE;
            goto LABEL300;
        LABEL110:  HSUM = H.v;
            EM[1 + o_em] = ONE;
            FLOTNQ = FLOTL - ONE;
            for (I = 2; I <= L.v; I++)
            {
                EM[I + o_em] = ZERO;
            }
            for (J = 1; J <= NQM1; J++)
            {
                if ((J != NQM1) || (NQWAIT.v != 1)) goto LABEL130;
                S = ONE;
                CSUM = ZERO;
                for (I = 1; I <= NQM1; I++)
                {
                    CSUM += S * EM[I + o_em] / Convert.ToSingle(I + 1);
                    S =  - S;
                }
                TQ[1 + o_tq].v = EM[NQM1 + o_em] / (FLOTNQ * CSUM);
            LABEL130:  RXI = H.v / HSUM;
                for (IBACK = 1; IBACK <= J; IBACK++)
                {
                    I = (J + 2) - IBACK;
                    EM[I + o_em] += EM[I - 1 + o_em] * RXI;
                }
                HSUM += TAU[J + o_tau].v;
            }
            // C Compute integral from -1 to 0 of polynomial and of x times it. -------
            S = ONE;
            EM0 = ZERO;
            CSUM = ZERO;
            for (I = 1; I <= NQ.v; I++)
            {
                FLOTI = Convert.ToSingle(I);
                EM0 += S * EM[I + o_em] / FLOTI;
                CSUM += S * EM[I + o_em] / (FLOTI + ONE);
                S =  - S;
            }
            // C In EL, form coefficients of normalized integrated polynomial. --------
            S = ONE / EM0;
            EL[1 + o_el].v = ONE;
            for (I = 1; I <= NQ.v; I++)
            {
                EL[I + 1 + o_el].v = S * EM[I + o_em] / Convert.ToSingle(I);
            }
            XI = HSUM / H.v;
            TQ[2 + o_tq].v = XI * EM0 / CSUM;
            TQ[5 + o_tq].v = XI / EL[L.v + o_el].v;
            if (NQWAIT.v != 1) goto LABEL300;
            // C For higher order control constant, multiply polynomial by 1+x/xi(q). -
            RXI = ONE / XI;
            for (IBACK = 1; IBACK <= NQ.v; IBACK++)
            {
                I = (L.v + 1) - IBACK;
                EM[I + o_em] += EM[I - 1 + o_em] * RXI;
            }
            // C Compute integral of polynomial. --------------------------------------
            S = ONE;
            CSUM = ZERO;
            for (I = 1; I <= L.v; I++)
            {
                CSUM += S * EM[I + o_em] / Convert.ToSingle(I + 1);
                S =  - S;
            }
            TQ[3 + o_tq].v = FLOTL * EM0 / CSUM;
            goto LABEL300;
            // C
            // C Set coefficients for BDF methods. ------------------------------------
        LABEL200:  
            for (I = 3; I <= L.v; I++)
            {
                EL[I + o_el].v = ZERO;
            }
            EL[1 + o_el].v = ONE;
            EL[2 + o_el].v = ONE;
            ALPH0 =  - ONE;
            AHATN0 =  - ONE;
            HSUM = H.v;
            RXI = ONE;
            RXIS = ONE;
            if (NQ.v == 1) goto LABEL240;
            for (J = 1; J <= NQM2; J++)
            {
                // C In EL, construct coefficients of (1+x/xi(1))*...*(1+x/xi(j+1)). ------
                HSUM += TAU[J + o_tau].v;
                RXI = H.v / HSUM;
                JP1 = J + 1;
                ALPH0 +=  - ONE / Convert.ToSingle(JP1);
                for (IBACK = 1; IBACK <= JP1; IBACK++)
                {
                    I = (J + 3) - IBACK;
                    EL[I + o_el].v += EL[I - 1 + o_el].v * RXI;
                }
            }
            ALPH0 +=  - ONE / Convert.ToSingle(NQ.v);
            RXIS =  - EL[2 + o_el].v - ALPH0;
            HSUM += TAU[NQM1 + o_tau].v;
            RXI = H.v / HSUM;
            AHATN0 =  - EL[2 + o_el].v - RXI;
            for (IBACK = 1; IBACK <= NQ.v; IBACK++)
            {
                I = (NQ.v + 2) - IBACK;
                EL[I + o_el].v += EL[I - 1 + o_el].v * RXIS;
            }
        LABEL240:  T1 = ONE - AHATN0 + ALPH0;
            T2 = ONE + Convert.ToSingle(NQ.v) * T1;
            TQ[2 + o_tq].v = Math.Abs(ALPH0 * T2 / T1);
            TQ[5 + o_tq].v = Math.Abs(T2 / (EL[L.v + o_el].v * RXI / RXIS));
            if (NQWAIT.v != 1) goto LABEL300;
            CNQM1 = RXIS / EL[L.v + o_el].v;
            T3 = ALPH0 + ONE / Convert.ToSingle(NQ.v);
            T4 = AHATN0 + RXI;
            ELP = T3 / (ONE - T4 + T3);
            TQ[1 + o_tq].v = Math.Abs(ELP / CNQM1);
            HSUM += TAU[NQ.v + o_tau].v;
            RXI = H.v / HSUM;
            T5 = ALPH0 - ONE / Convert.ToSingle(NQ.v + 1);
            T6 = AHATN0 - RXI;
            ELP = T2 / (ONE - T6 + T5);
            TQ[3 + o_tq].v = Math.Abs(ELP * RXI * (FLOTL + ONE) * T5);
        LABEL300:  TQ[4 + o_tq].v = CORTES * TQ[2 + o_tq].v;
            return;
            // C----------------------- End of Subroutine DVSET -----------------------
            #endregion
        }
    }

    #endregion


    #region The Class: DVJUST
    
    // *DECK DVJUST
    public class DVJUST
    {
    
        #region Dependencies
        
        DAXPY _daxpy; 
        #endregion
        #region Common variables
        
        #region Common Block: DVOD01 Declaration
        
        CommonBlock _dvod01;
        Odouble ACNRM; Odouble CCMXJ; Odouble CONP; Odouble CRATE; Odouble DRC; Odouble[] EL; int o_el; Odouble ETA; 
        Odouble ETAMAX;Odouble H; Odouble HMIN; Odouble HMXI; Odouble HNEW; Odouble HSCAL; Odouble PRL1; Odouble RC; Odouble RL1; 
        Odouble[] TAU; int o_tau;Odouble[] TQ; int o_tq; Odouble TN; Odouble UROUND; 
        Oint ICF; Oint INIT; Oint IPUP; Oint JCUR; Oint JSTART; Oint JSV; Oint KFLAG; Oint KUTH; Oint L; Oint LMAX; Oint LYH; 
        Oint LEWT;Oint LACOR; Oint LSAVF; Oint LWM; Oint LIWM; Oint LOCJS; Oint MAXORD; Oint METH; Oint MITER; Oint MSBJ; 
        Oint MXHNIL;Oint MXSTEP; Oint N; Oint NEWH; Oint NEWQ; Oint NHNIL; Oint NQ; Oint NQNYH; Oint NQWAIT; Oint NSLJ; Oint NSLP; 
        Oint NYH;
        #endregion
        #endregion
        #region Variables
        
        double ONE = 0; double ZERO = 0; 
        #endregion
        public DVJUST(DAXPY daxpy, CommonBlock DVOD01)
        {
    
            #region Set Dependencies
            
            this._daxpy = daxpy; 
            #endregion
            #region Data Initialization
            
            //ONE/1.0D0
            ONE = 1.0E0;
            //ZERO/0.0D0
            ZERO = 0.0E0;
            #endregion
            #region Common varaible Initialization
            
            #region Common Block: DVOD01 Initialization
            
            this._dvod01 = DVOD01;
            ACNRM = DVOD01.doubleData[0];
            CCMXJ = DVOD01.doubleData[1];
            CONP = DVOD01.doubleData[2];
            CRATE = DVOD01.doubleData[3];
            DRC = DVOD01.doubleData[4];
            //Start Array: EL  StartIndex: 5  LastIndex: 17
            EL = DVOD01.doubleData;
            o_el = 4;  //o_ = StartIndex-1
            //End Array: EL
            ETA = DVOD01.doubleData[18];
            ETAMAX = DVOD01.doubleData[19];
            H = DVOD01.doubleData[20];
            HMIN = DVOD01.doubleData[21];
            HMXI = DVOD01.doubleData[22];
            HNEW = DVOD01.doubleData[23];
            HSCAL = DVOD01.doubleData[24];
            PRL1 = DVOD01.doubleData[25];
            RC = DVOD01.doubleData[26];
            RL1 = DVOD01.doubleData[27];
            //Start Array: TAU  StartIndex: 28  LastIndex: 40
            TAU = DVOD01.doubleData;
            o_tau = 27;  //o_ = StartIndex-1
            //End Array: TAU
            //Start Array: TQ  StartIndex: 41  LastIndex: 45
            TQ = DVOD01.doubleData;
            o_tq = 40;  //o_ = StartIndex-1
            //End Array: TQ
            TN = DVOD01.doubleData[46];
            UROUND = DVOD01.doubleData[47];
            ICF = DVOD01.intData[0];
            INIT = DVOD01.intData[1];
            IPUP = DVOD01.intData[2];
            JCUR = DVOD01.intData[3];
            JSTART = DVOD01.intData[4];
            JSV = DVOD01.intData[5];
            KFLAG = DVOD01.intData[6];
            KUTH = DVOD01.intData[7];
            L = DVOD01.intData[8];
            LMAX = DVOD01.intData[9];
            LYH = DVOD01.intData[10];
            LEWT = DVOD01.intData[11];
            LACOR = DVOD01.intData[12];
            LSAVF = DVOD01.intData[13];
            LWM = DVOD01.intData[14];
            LIWM = DVOD01.intData[15];
            LOCJS = DVOD01.intData[16];
            MAXORD = DVOD01.intData[17];
            METH = DVOD01.intData[18];
            MITER = DVOD01.intData[19];
            MSBJ = DVOD01.intData[20];
            MXHNIL = DVOD01.intData[21];
            MXSTEP = DVOD01.intData[22];
            N = DVOD01.intData[23];
            NEWH = DVOD01.intData[24];
            NEWQ = DVOD01.intData[25];
            NHNIL = DVOD01.intData[26];
            NQ = DVOD01.intData[27];
            NQNYH = DVOD01.intData[28];
            NQWAIT = DVOD01.intData[29];
            NSLJ = DVOD01.intData[30];
            NSLP = DVOD01.intData[31];
            NYH = DVOD01.intData[32];
            #endregion
            #endregion
        }
    
        public DVJUST()
        {
    
            #region Initialization Common Blocks
            
            CommonBlock DVOD01 = new CommonBlock(48, 33, 0, 0);
            #endregion
            #region Dependencies (Initialization)
            
            DAXPY daxpy = new DAXPY();
            #endregion
            #region Set Dependencies
            
            this._daxpy = daxpy; 
            #endregion
            #region Data Initialization
            
            //ONE/1.0D0
            ONE = 1.0E0;
            //ZERO/0.0D0
            ZERO = 0.0E0;
            #endregion
            #region Common varaible Initialization
            
            #region Common Block: DVOD01 Initialization
            
            this._dvod01 = DVOD01;
            ACNRM = DVOD01.doubleData[0];
            CCMXJ = DVOD01.doubleData[1];
            CONP = DVOD01.doubleData[2];
            CRATE = DVOD01.doubleData[3];
            DRC = DVOD01.doubleData[4];
            //Start Array: EL  StartIndex: 5  LastIndex: 17
            EL = DVOD01.doubleData;
            o_el = 4;  //o_ = StartIndex-1
            //End Array: EL
            ETA = DVOD01.doubleData[18];
            ETAMAX = DVOD01.doubleData[19];
            H = DVOD01.doubleData[20];
            HMIN = DVOD01.doubleData[21];
            HMXI = DVOD01.doubleData[22];
            HNEW = DVOD01.doubleData[23];
            HSCAL = DVOD01.doubleData[24];
            PRL1 = DVOD01.doubleData[25];
            RC = DVOD01.doubleData[26];
            RL1 = DVOD01.doubleData[27];
            //Start Array: TAU  StartIndex: 28  LastIndex: 40
            TAU = DVOD01.doubleData;
            o_tau = 27;  //o_ = StartIndex-1
            //End Array: TAU
            //Start Array: TQ  StartIndex: 41  LastIndex: 45
            TQ = DVOD01.doubleData;
            o_tq = 40;  //o_ = StartIndex-1
            //End Array: TQ
            TN = DVOD01.doubleData[46];
            UROUND = DVOD01.doubleData[47];
            ICF = DVOD01.intData[0];
            INIT = DVOD01.intData[1];
            IPUP = DVOD01.intData[2];
            JCUR = DVOD01.intData[3];
            JSTART = DVOD01.intData[4];
            JSV = DVOD01.intData[5];
            KFLAG = DVOD01.intData[6];
            KUTH = DVOD01.intData[7];
            L = DVOD01.intData[8];
            LMAX = DVOD01.intData[9];
            LYH = DVOD01.intData[10];
            LEWT = DVOD01.intData[11];
            LACOR = DVOD01.intData[12];
            LSAVF = DVOD01.intData[13];
            LWM = DVOD01.intData[14];
            LIWM = DVOD01.intData[15];
            LOCJS = DVOD01.intData[16];
            MAXORD = DVOD01.intData[17];
            METH = DVOD01.intData[18];
            MITER = DVOD01.intData[19];
            MSBJ = DVOD01.intData[20];
            MXHNIL = DVOD01.intData[21];
            MXSTEP = DVOD01.intData[22];
            N = DVOD01.intData[23];
            NEWH = DVOD01.intData[24];
            NEWQ = DVOD01.intData[25];
            NHNIL = DVOD01.intData[26];
            NQ = DVOD01.intData[27];
            NQNYH = DVOD01.intData[28];
            NQWAIT = DVOD01.intData[29];
            NSLJ = DVOD01.intData[30];
            NSLP = DVOD01.intData[31];
            NYH = DVOD01.intData[32];
            #endregion
            #endregion
        }
        /// <param name="IORD">
        /// = An integer flag used when METH = 2 to indicate an order
        /// increase (IORD = +1) or an order decrease (IORD = -1).
        ///</param>
        public void Run(ref double[] YH, int offset_yh, int LDYH, int IORD)
        {
            #region Variables
            
            double ALPH0 = 0; double ALPH1 = 0; double HSUM = 0; double PROD = 0; double T1 = 0; double XI = 0; double XIOLD = 0; 
            int I = 0;int IBACK = 0; int J = 0; int JP1 = 0; int LP1 = 0; int NQM1 = 0; int NQM2 = 0; int NQP1 = 0; 
            #endregion
            #region Array Index Correction
            
             int o_yh = -1 - LDYH + offset_yh; 
            #endregion
            #region Prolog
            
            // C-----------------------------------------------------------------------
            // C Call sequence input -- YH, LDYH, IORD
            // C Call sequence output -- YH
            // C COMMON block input -- NQ, METH, LMAX, HSCAL, TAU(13), N
            // C COMMON block variables accessed:
            // C     /DVOD01/ -- HSCAL, TAU(13), LMAX, METH, N, NQ,
            // C
            // C Subroutines called by DVJUST: DAXPY
            // C Function routines called by DVJUST: None
            // C-----------------------------------------------------------------------
            // C This subroutine adjusts the YH array on reduction of order,
            // C and also when the order is increased for the stiff option (METH = 2).
            // C Communication with DVJUST uses the following:
            // C IORD  = An integer flag used when METH = 2 to indicate an order
            // C         increase (IORD = +1) or an order decrease (IORD = -1).
            // C HSCAL = Step size H used in scaling of Nordsieck array YH.
            // C         (If IORD = +1, DVJUST assumes that HSCAL = TAU(1).)
            // C See References 1 and 2 for details.
            // C-----------------------------------------------------------------------
            // C
            // C Type declarations for labeled COMMON block DVOD01 --------------------
            // C
            // C
            // C Type declarations for local variables --------------------------------
            // C
            // C-----------------------------------------------------------------------
            // C The following Fortran-77 declaration is to cause the values of the
            // C listed (local) variables to be saved between calls to this integrator.
            // C-----------------------------------------------------------------------
            // C
            // C
            // C
            #endregion
            #region Body
            
            if ((NQ.v == 2) && (IORD != 1)) return;
            NQM1 = NQ.v - 1;
            NQM2 = NQ.v - 2;
            switch (METH.v)
            {
                case 1: goto LABEL100;
                case 2: goto LABEL200;
            }
            // C-----------------------------------------------------------------------
            // C Nonstiff option...
            // C Check to see if the order is being increased or decreased.
            // C-----------------------------------------------------------------------
        LABEL100:;
            if (IORD == 1) goto LABEL180;
            // C Order decrease. ------------------------------------------------------
            for (J = 1; J <= LMAX.v; J++)
            {
                EL[J + o_el].v = ZERO;
            }
            EL[2 + o_el].v = ONE;
            HSUM = ZERO;
            for (J = 1; J <= NQM2; J++)
            {
                // C Construct coefficients of x*(x+xi(1))*...*(x+xi(j)). -----------------
                HSUM += TAU[J + o_tau].v;
                XI = HSUM / HSCAL.v;
                JP1 = J + 1;
                for (IBACK = 1; IBACK <= JP1; IBACK++)
                {
                    I = (J + 3) - IBACK;
                    EL[I + o_el].v = EL[I + o_el].v * XI + EL[I - 1 + o_el].v;
                }
            }
            // C Construct coefficients of integrated polynomial. ---------------------
            for (J = 2; J <= NQM1; J++)
            {
                EL[J + 1 + o_el].v = Convert.ToSingle(NQ.v) * EL[J + o_el].v / Convert.ToSingle(J);
            }
            // C Subtract correction terms from YH array. -----------------------------
            for (J = 3; J <= NQ.v; J++)
            {
                for (I = 1; I <= N.v; I++)
                {
                    YH[I+J * LDYH + o_yh] +=  - YH[I+L.v * LDYH + o_yh] * EL[J + o_el].v;
                }
            }
            return;
            // C Order increase. ------------------------------------------------------
            // C Zero out next column in YH array. ------------------------------------
        LABEL180:;
            LP1 = L.v + 1;
            for (I = 1; I <= N.v; I++)
            {
                YH[I+LP1 * LDYH + o_yh] = ZERO;
            }
            return;
            // C-----------------------------------------------------------------------
            // C Stiff option...
            // C Check to see if the order is being increased or decreased.
            // C-----------------------------------------------------------------------
        LABEL200:;
            if (IORD == 1) goto LABEL300;
            // C Order decrease. ------------------------------------------------------
            for (J = 1; J <= LMAX.v; J++)
            {
                EL[J + o_el].v = ZERO;
            }
            EL[3 + o_el].v = ONE;
            HSUM = ZERO;
            for (J = 1; J <= NQM2; J++)
            {
                // C Construct coefficients of x*x*(x+xi(1))*...*(x+xi(j)). ---------------
                HSUM += TAU[J + o_tau].v;
                XI = HSUM / HSCAL.v;
                JP1 = J + 1;
                for (IBACK = 1; IBACK <= JP1; IBACK++)
                {
                    I = (J + 4) - IBACK;
                    EL[I + o_el].v = EL[I + o_el].v * XI + EL[I - 1 + o_el].v;
                }
            }
            // C Subtract correction terms from YH array. -----------------------------
            for (J = 3; J <= NQ.v; J++)
            {
                for (I = 1; I <= N.v; I++)
                {
                    YH[I+J * LDYH + o_yh] +=  - YH[I+L.v * LDYH + o_yh] * EL[J + o_el].v;
                }
            }
            return;
            // C Order increase. ------------------------------------------------------
        LABEL300:  
            for (J = 1; J <= LMAX.v; J++)
            {
                EL[J + o_el].v = ZERO;
            }
            EL[3 + o_el].v = ONE;
            ALPH0 =  - ONE;
            ALPH1 = ONE;
            PROD = ONE;
            XIOLD = ONE;
            HSUM = HSCAL.v;
            if (NQ.v == 1) goto LABEL340;
            for (J = 1; J <= NQM1; J++)
            {
                // C Construct coefficients of x*x*(x+xi(1))*...*(x+xi(j)). ---------------
                JP1 = J + 1;
                HSUM += TAU[JP1 + o_tau].v;
                XI = HSUM / HSCAL.v;
                PROD *= XI;
                ALPH0 +=  - ONE / Convert.ToSingle(JP1);
                ALPH1 += ONE / XI;
                for (IBACK = 1; IBACK <= JP1; IBACK++)
                {
                    I = (J + 4) - IBACK;
                    EL[I + o_el].v = EL[I + o_el].v * XIOLD + EL[I - 1 + o_el].v;
                }
                XIOLD = XI;
            }
        LABEL340:;
            T1 = ( - ALPH0 - ALPH1) / PROD;
            // C Load column L + 1 in YH array. ---------------------------------------
            LP1 = L.v + 1;
            for (I = 1; I <= N.v; I++)
            {
                YH[I+LP1 * LDYH + o_yh] = T1 * YH[I+LMAX.v * LDYH + o_yh];
            }
            // C Add correction terms to YH array. ------------------------------------
            NQP1 = NQ.v + 1;
            for (J = 3; J <= NQP1; J++)
            {
                this._daxpy.Run(N.v, EL[J + o_el].v, YH, 1+LP1 * LDYH + o_yh, 1, ref YH, 1+J * LDYH + o_yh, 1);
            }
            return;
            // C----------------------- End of Subroutine DVJUST ----------------------
            #endregion
        }
    }

    #endregion


    #region The Class: DVNLSD
    
    // *DECK DVNLSD
    public class DVNLSD : IDVNLSD
    {
    
        #region Dependencies
        
        DVNORM _dvnorm; DCOPY _dcopy; DVJAC _dvjac; DVSOL _dvsol; DSCAL _dscal; DAXPY _daxpy; 
        #endregion
        #region Common variables
        
        #region Common Block: DVOD01 Declaration
        
        CommonBlock _dvod01;
        Odouble ACNRM; Odouble CCMXJ; Odouble CONP; Odouble CRATE; Odouble DRC; Odouble[] EL; int o_el; Odouble ETA; 
        Odouble ETAMAX;Odouble H; Odouble HMIN; Odouble HMXI; Odouble HNEW; Odouble HSCAL; Odouble PRL1; Odouble RC; Odouble RL1; 
        Odouble[] TAU; int o_tau;Odouble[] TQ; int o_tq; Odouble TN; Odouble UROUND; 
        Oint ICF; Oint INIT; Oint IPUP; Oint JCUR; Oint JSTART; Oint JSV; Oint KFLAG; Oint KUTH; Oint L; Oint LMAX; Oint LYH; 
        Oint LEWT;Oint LACOR; Oint LSAVF; Oint LWM; Oint LIWM; Oint LOCJS; Oint MAXORD; Oint METH; Oint MITER; Oint MSBJ; 
        Oint MXHNIL;Oint MXSTEP; Oint N; Oint NEWH; Oint NEWQ; Oint NHNIL; Oint NQ; Oint NQNYH; Oint NQWAIT; Oint NSLJ; Oint NSLP; 
        Oint NYH;
        #endregion
        #region Common Block: DVOD02 Declaration
        
        CommonBlock _dvod02;
        Odouble HU; 
        Oint NCFN; Oint NETF; Oint NFE; Oint NJE; Oint NLU; Oint NNI; Oint NQU; Oint NST; 
        #endregion
        #endregion
        #region Variables
        
        double CCMAX = 0; double CRDOWN = 0; double ONE = 0; double RDIV = 0; double TWO = 0; double ZERO = 0; int MAXCOR = 0; 
        int MSBP = 0;
        #endregion
        public DVNLSD(DVNORM dvnorm, DCOPY dcopy, DVJAC dvjac, DVSOL dvsol, DSCAL dscal, DAXPY daxpy, CommonBlock DVOD01, CommonBlock DVOD02)
        {
    
            #region Set Dependencies
            
            this._dvnorm = dvnorm; this._dcopy = dcopy; this._dvjac = dvjac; this._dvsol = dvsol; this._dscal = dscal; 
            this._daxpy = daxpy;
            #endregion
            #region Data Initialization
            
            //CCMAX/0.3D0
            CCMAX = 0.3E0;
            //CRDOWN/0.3D0
            CRDOWN = 0.3E0;
            //MAXCOR/3
            MAXCOR = 3;
            //MSBP/20
            MSBP = 20;
            //RDIV/2.0D0
            RDIV = 2.0E0;
            //ONE/1.0D0
            ONE = 1.0E0;
            //TWO/2.0D0
            TWO = 2.0E0;
            //ZERO/0.0D0
            ZERO = 0.0E0;
            #endregion
            #region Common varaible Initialization
            
            #region Common Block: DVOD01 Initialization
            
            this._dvod01 = DVOD01;
            ACNRM = DVOD01.doubleData[0];
            CCMXJ = DVOD01.doubleData[1];
            CONP = DVOD01.doubleData[2];
            CRATE = DVOD01.doubleData[3];
            DRC = DVOD01.doubleData[4];
            //Start Array: EL  StartIndex: 5  LastIndex: 17
            EL = DVOD01.doubleData;
            o_el = 4;  //o_ = StartIndex-1
            //End Array: EL
            ETA = DVOD01.doubleData[18];
            ETAMAX = DVOD01.doubleData[19];
            H = DVOD01.doubleData[20];
            HMIN = DVOD01.doubleData[21];
            HMXI = DVOD01.doubleData[22];
            HNEW = DVOD01.doubleData[23];
            HSCAL = DVOD01.doubleData[24];
            PRL1 = DVOD01.doubleData[25];
            RC = DVOD01.doubleData[26];
            RL1 = DVOD01.doubleData[27];
            //Start Array: TAU  StartIndex: 28  LastIndex: 40
            TAU = DVOD01.doubleData;
            o_tau = 27;  //o_ = StartIndex-1
            //End Array: TAU
            //Start Array: TQ  StartIndex: 41  LastIndex: 45
            TQ = DVOD01.doubleData;
            o_tq = 40;  //o_ = StartIndex-1
            //End Array: TQ
            TN = DVOD01.doubleData[46];
            UROUND = DVOD01.doubleData[47];
            ICF = DVOD01.intData[0];
            INIT = DVOD01.intData[1];
            IPUP = DVOD01.intData[2];
            JCUR = DVOD01.intData[3];
            JSTART = DVOD01.intData[4];
            JSV = DVOD01.intData[5];
            KFLAG = DVOD01.intData[6];
            KUTH = DVOD01.intData[7];
            L = DVOD01.intData[8];
            LMAX = DVOD01.intData[9];
            LYH = DVOD01.intData[10];
            LEWT = DVOD01.intData[11];
            LACOR = DVOD01.intData[12];
            LSAVF = DVOD01.intData[13];
            LWM = DVOD01.intData[14];
            LIWM = DVOD01.intData[15];
            LOCJS = DVOD01.intData[16];
            MAXORD = DVOD01.intData[17];
            METH = DVOD01.intData[18];
            MITER = DVOD01.intData[19];
            MSBJ = DVOD01.intData[20];
            MXHNIL = DVOD01.intData[21];
            MXSTEP = DVOD01.intData[22];
            N = DVOD01.intData[23];
            NEWH = DVOD01.intData[24];
            NEWQ = DVOD01.intData[25];
            NHNIL = DVOD01.intData[26];
            NQ = DVOD01.intData[27];
            NQNYH = DVOD01.intData[28];
            NQWAIT = DVOD01.intData[29];
            NSLJ = DVOD01.intData[30];
            NSLP = DVOD01.intData[31];
            NYH = DVOD01.intData[32];
            #endregion
            #region Common Block: DVOD02 Initialization
            
            this._dvod02 = DVOD02;
            HU = DVOD02.doubleData[0];
            NCFN = DVOD02.intData[0];
            NETF = DVOD02.intData[1];
            NFE = DVOD02.intData[2];
            NJE = DVOD02.intData[3];
            NLU = DVOD02.intData[4];
            NNI = DVOD02.intData[5];
            NQU = DVOD02.intData[6];
            NST = DVOD02.intData[7];
            #endregion
            #endregion
        }
    
        public DVNLSD()
        {
    
            #region Initialization Common Blocks
            
            CommonBlock DVOD01 = new CommonBlock(48, 33, 0, 0);
            CommonBlock DVOD02 = new CommonBlock(1, 8, 0, 0);
            #endregion
            #region Dependencies (Initialization)
            
            DVNORM dvnorm = new DVNORM();
            DCOPY dcopy = new DCOPY();
            DSCAL dscal = new DSCAL();
            IDAMAX idamax = new IDAMAX();
            DAXPY daxpy = new DAXPY();
            DDOT ddot = new DDOT();
            DGEFA dgefa = new DGEFA(idamax, dscal, daxpy);
            DACOPY dacopy = new DACOPY(dcopy);
            DGBFA dgbfa = new DGBFA(idamax, dscal, daxpy);
            DVJAC dvjac = new DVJAC(dvnorm, dcopy, dscal, dgefa, dacopy, dgbfa, DVOD01, DVOD02);
            DGESL dgesl = new DGESL(ddot, daxpy);
            DGBSL dgbsl = new DGBSL(ddot, daxpy);
            DVSOL dvsol = new DVSOL(dgesl, dgbsl, DVOD01);
            #endregion
            #region Set Dependencies
            
            this._dvnorm = dvnorm; this._dcopy = dcopy; this._dvjac = dvjac; this._dvsol = dvsol; this._dscal = dscal; 
            this._daxpy = daxpy;
            #endregion
            #region Data Initialization
            
            //CCMAX/0.3D0
            CCMAX = 0.3E0;
            //CRDOWN/0.3D0
            CRDOWN = 0.3E0;
            //MAXCOR/3
            MAXCOR = 3;
            //MSBP/20
            MSBP = 20;
            //RDIV/2.0D0
            RDIV = 2.0E0;
            //ONE/1.0D0
            ONE = 1.0E0;
            //TWO/2.0D0
            TWO = 2.0E0;
            //ZERO/0.0D0
            ZERO = 0.0E0;
            #endregion
            #region Common varaible Initialization
            
            #region Common Block: DVOD01 Initialization
            
            this._dvod01 = DVOD01;
            ACNRM = DVOD01.doubleData[0];
            CCMXJ = DVOD01.doubleData[1];
            CONP = DVOD01.doubleData[2];
            CRATE = DVOD01.doubleData[3];
            DRC = DVOD01.doubleData[4];
            //Start Array: EL  StartIndex: 5  LastIndex: 17
            EL = DVOD01.doubleData;
            o_el = 4;  //o_ = StartIndex-1
            //End Array: EL
            ETA = DVOD01.doubleData[18];
            ETAMAX = DVOD01.doubleData[19];
            H = DVOD01.doubleData[20];
            HMIN = DVOD01.doubleData[21];
            HMXI = DVOD01.doubleData[22];
            HNEW = DVOD01.doubleData[23];
            HSCAL = DVOD01.doubleData[24];
            PRL1 = DVOD01.doubleData[25];
            RC = DVOD01.doubleData[26];
            RL1 = DVOD01.doubleData[27];
            //Start Array: TAU  StartIndex: 28  LastIndex: 40
            TAU = DVOD01.doubleData;
            o_tau = 27;  //o_ = StartIndex-1
            //End Array: TAU
            //Start Array: TQ  StartIndex: 41  LastIndex: 45
            TQ = DVOD01.doubleData;
            o_tq = 40;  //o_ = StartIndex-1
            //End Array: TQ
            TN = DVOD01.doubleData[46];
            UROUND = DVOD01.doubleData[47];
            ICF = DVOD01.intData[0];
            INIT = DVOD01.intData[1];
            IPUP = DVOD01.intData[2];
            JCUR = DVOD01.intData[3];
            JSTART = DVOD01.intData[4];
            JSV = DVOD01.intData[5];
            KFLAG = DVOD01.intData[6];
            KUTH = DVOD01.intData[7];
            L = DVOD01.intData[8];
            LMAX = DVOD01.intData[9];
            LYH = DVOD01.intData[10];
            LEWT = DVOD01.intData[11];
            LACOR = DVOD01.intData[12];
            LSAVF = DVOD01.intData[13];
            LWM = DVOD01.intData[14];
            LIWM = DVOD01.intData[15];
            LOCJS = DVOD01.intData[16];
            MAXORD = DVOD01.intData[17];
            METH = DVOD01.intData[18];
            MITER = DVOD01.intData[19];
            MSBJ = DVOD01.intData[20];
            MXHNIL = DVOD01.intData[21];
            MXSTEP = DVOD01.intData[22];
            N = DVOD01.intData[23];
            NEWH = DVOD01.intData[24];
            NEWQ = DVOD01.intData[25];
            NHNIL = DVOD01.intData[26];
            NQ = DVOD01.intData[27];
            NQNYH = DVOD01.intData[28];
            NQWAIT = DVOD01.intData[29];
            NSLJ = DVOD01.intData[30];
            NSLP = DVOD01.intData[31];
            NYH = DVOD01.intData[32];
            #endregion
            #region Common Block: DVOD02 Initialization
            
            this._dvod02 = DVOD02;
            HU = DVOD02.doubleData[0];
            NCFN = DVOD02.intData[0];
            NETF = DVOD02.intData[1];
            NFE = DVOD02.intData[2];
            NJE = DVOD02.intData[3];
            NLU = DVOD02.intData[4];
            NNI = DVOD02.intData[5];
            NQU = DVOD02.intData[6];
            NST = DVOD02.intData[7];
            #endregion
            #endregion
        }
        /// <param name="Y">
        /// = The dependent variable, a vector of length N, input.
        ///</param>
        /// <param name="YH">
        /// = The Nordsieck (Taylor) array, LDYH by LMAX, input
        /// and output.  On input, it contains predicted values.
        ///</param>
        /// <param name="LDYH">
        /// = A constant .ge. N, the first dimension of YH, input.
        ///</param>
        /// <param name="VSAV">
        /// = Unused work array.
        ///</param>
        /// <param name="SAVF">
        /// = A work array of length N.
        ///</param>
        /// <param name="EWT">
        /// = An error weight vector of length N, input.
        ///</param>
        /// <param name="ACOR">
        /// = A work array of length N, used for the accumulated
        /// corrections to the predicted y vector.
        ///</param>
        /// <param name="F">
        /// = Dummy name for user supplied routine for f.
        ///</param>
        /// <param name="JAC">
        /// = Dummy name for user supplied Jacobian routine.
        ///</param>
        /// <param name="PDUM">
        /// = Unused dummy subroutine name.  Included for uniformity
        /// over collection of integrators.
        ///</param>
        /// <param name="NFLAG">
        /// = Input/output flag, with values and meanings as follows:
        /// INPUT
        /// 0 first call for this time step.
        /// -1 convergence failure in previous call to DVNLSD.
        /// -2 error test failure in DVSTEP.
        /// OUTPUT
        /// 0 successful completion of nonlinear solver.
        /// -1 convergence failure or singular matrix.
        /// -2 unrecoverable error in matrix preprocessing
        /// (cannot occur here).
        /// -3 unrecoverable error in solution (cannot occur
        /// here).
        ///</param>
        public void Run(ref double[] Y, int offset_y, double[] YH, int offset_yh, int LDYH, double[] VSAV, int offset_vsav, ref double[] SAVF, int offset_savf, double[] EWT, int offset_ewt
                         , ref double[] ACOR, int offset_acor, ref int[] IWM, int offset_iwm, ref double[] WM, int offset_wm, IFEX F, IJEX JAC, IFEX PDUM
                         , ref int NFLAG, double[] RPAR, int offset_rpar, int[] IPAR, int offset_ipar)
        {
            #region Variables
            
            double CSCALE = 0; double DCON = 0; double DEL = 0; double DELP = 0; int I = 0; int IERPJ = 0; int IERSL = 0; 
            int M = 0;
            #endregion
            #region Array Index Correction
            
             int o_y = -1 + offset_y;  int o_yh = -1 - LDYH + offset_yh;  int o_vsav = -1 + offset_vsav; 
             int o_savf = -1 + offset_savf; int o_ewt = -1 + offset_ewt;  int o_acor = -1 + offset_acor; 
             int o_iwm = -1 + offset_iwm; int o_wm = -1 + offset_wm;  int o_rpar = -1 + offset_rpar; 
             int o_ipar = -1 + offset_ipar;
            #endregion
            #region Prolog
            
            // C-----------------------------------------------------------------------
            // C Call sequence input -- Y, YH, LDYH, SAVF, EWT, ACOR, IWM, WM,
            // C                        F, JAC, NFLAG, RPAR, IPAR
            // C Call sequence output -- YH, ACOR, WM, IWM, NFLAG
            // C COMMON block variables accessed:
            // C     /DVOD01/ ACNRM, CRATE, DRC, H, RC, RL1, TQ(5), TN, ICF,
            // C                JCUR, METH, MITER, N, NSLP
            // C     /DVOD02/ HU, NCFN, NETF, NFE, NJE, NLU, NNI, NQU, NST
            // C
            // C Subroutines called by DVNLSD: F, DAXPY, DCOPY, DSCAL, DVJAC, DVSOL
            // C Function routines called by DVNLSD: DVNORM
            // C-----------------------------------------------------------------------
            // C Subroutine DVNLSD is a nonlinear system solver, which uses functional
            // C iteration or a chord (modified Newton) method.  For the chord method
            // C direct linear algebraic system solvers are used.  Subroutine DVNLSD
            // C then handles the corrector phase of this integration package.
            // C
            // C Communication with DVNLSD is done with the following variables. (For
            // C more details, please see the comments in the driver subroutine.)
            // C
            // C Y          = The dependent variable, a vector of length N, input.
            // C YH         = The Nordsieck (Taylor) array, LDYH by LMAX, input
            // C              and output.  On input, it contains predicted values.
            // C LDYH       = A constant .ge. N, the first dimension of YH, input.
            // C VSAV       = Unused work array.
            // C SAVF       = A work array of length N.
            // C EWT        = An error weight vector of length N, input.
            // C ACOR       = A work array of length N, used for the accumulated
            // C              corrections to the predicted y vector.
            // C WM,IWM     = Real and integer work arrays associated with matrix
            // C              operations in chord iteration (MITER .ne. 0).
            // C F          = Dummy name for user supplied routine for f.
            // C JAC        = Dummy name for user supplied Jacobian routine.
            // C PDUM       = Unused dummy subroutine name.  Included for uniformity
            // C              over collection of integrators.
            // C NFLAG      = Input/output flag, with values and meanings as follows:
            // C              INPUT
            // C                  0 first call for this time step.
            // C                 -1 convergence failure in previous call to DVNLSD.
            // C                 -2 error test failure in DVSTEP.
            // C              OUTPUT
            // C                  0 successful completion of nonlinear solver.
            // C                 -1 convergence failure or singular matrix.
            // C                 -2 unrecoverable error in matrix preprocessing
            // C                    (cannot occur here).
            // C                 -3 unrecoverable error in solution (cannot occur
            // C                    here).
            // C RPAR, IPAR = Dummy names for user's real and integer work arrays.
            // C
            // C IPUP       = Own variable flag with values and meanings as follows:
            // C              0,            do not update the Newton matrix.
            // C              MITER .ne. 0, update Newton matrix, because it is the
            // C                            initial step, order was changed, the error
            // C                            test failed, or an update is indicated by
            // C                            the scalar RC or step counter NST.
            // C
            // C For more details, see comments in driver subroutine.
            // C-----------------------------------------------------------------------
            // C Type declarations for labeled COMMON block DVOD01 --------------------
            // C
            // C
            // C Type declarations for labeled COMMON block DVOD02 --------------------
            // C
            // C
            // C Type declarations for local variables --------------------------------
            // C
            // C
            // C Type declaration for function subroutines called ---------------------
            // C
            // C-----------------------------------------------------------------------
            // C The following Fortran-77 declaration is to cause the values of the
            // C listed (local) variables to be saved between calls to this integrator.
            // C-----------------------------------------------------------------------
            // C
            // C
            // C-----------------------------------------------------------------------
            // C On the first step, on a change of method order, or after a
            // C nonlinear convergence failure with NFLAG = -2, set IPUP = MITER
            // C to force a Jacobian update when MITER .ne. 0.
            // C-----------------------------------------------------------------------
            #endregion
            #region Body
            
            if (JSTART.v == 0) NSLP.v = 0;
            if (NFLAG == 0) ICF.v = 0;
            if (NFLAG ==  - 2) IPUP.v = MITER.v;
            if ((JSTART.v == 0) || (JSTART.v ==  - 1)) IPUP.v = MITER.v;
            // C If this is functional iteration, set CRATE .eq. 1 and drop to 220
            if (MITER.v == 0)
            {
                CRATE.v = ONE;
                goto LABEL220;
            }
            // C-----------------------------------------------------------------------
            // C RC is the ratio of new to old values of the coefficient H/EL(2)=h/l1.
            // C When RC differs from 1 by more than CCMAX, IPUP is set to MITER
            // C to force DVJAC to be called, if a Jacobian is involved.
            // C In any case, DVJAC is called at least every MSBP steps.
            // C-----------------------------------------------------------------------
            DRC.v = Math.Abs(RC.v - ONE);
            if (DRC.v > CCMAX || NST.v >= NSLP.v + MSBP) IPUP.v = MITER.v;
            // C-----------------------------------------------------------------------
            // C Up to MAXCOR corrector iterations are taken.  A convergence test is
            // C made on the r.m.s. norm of each correction, weighted by the error
            // C weight vector EWT.  The sum of the corrections is accumulated in the
            // C vector ACOR(i).  The YH array is not altered in the corrector loop.
            // C-----------------------------------------------------------------------
        LABEL220:  M = 0;
            DELP = ZERO;
            this._dcopy.Run(N.v, YH, 1+1 * LDYH + o_yh, 1, ref Y, offset_y, 1);
            F.Run(N.v, TN.v, Y, offset_y, ref SAVF, offset_savf, RPAR[1 + o_rpar], IPAR[1 + o_ipar]);
            NFE.v += 1;
            if (IPUP.v <= 0) goto LABEL250;
            // C-----------------------------------------------------------------------
            // C If indicated, the matrix P = I - h*rl1*J is reevaluated and
            // C preprocessed before starting the corrector iteration.  IPUP is set
            // C to 0 as an indicator that this has been done.
            // C-----------------------------------------------------------------------
            this._dvjac.Run(ref Y, offset_y, YH, offset_yh, LDYH, EWT, offset_ewt, ref ACOR, offset_acor, SAVF, offset_savf
                            , ref WM, offset_wm, ref IWM, offset_iwm, F, JAC, ref IERPJ, RPAR, offset_rpar
                            , IPAR, offset_ipar);
            IPUP.v = 0;
            RC.v = ONE;
            DRC.v = ZERO;
            CRATE.v = ONE;
            NSLP.v = NST.v;
            // C If matrix is singular, take error return to force cut in step size. --
            if (IERPJ != 0) goto LABEL430;
        LABEL250:  
            for (I = 1; I <= N.v; I++)
            {
                ACOR[I + o_acor] = ZERO;
            }
            // C This is a looping point for the corrector iteration. -----------------
        LABEL270:  
            if (MITER.v != 0) goto LABEL350;
            // C-----------------------------------------------------------------------
            // C In the case of functional iteration, update Y directly from
            // C the result of the last function evaluation.
            // C-----------------------------------------------------------------------
            for (I = 1; I <= N.v; I++)
            {
                SAVF[I + o_savf] = RL1.v * (H.v * SAVF[I + o_savf] - YH[I+2 * LDYH + o_yh]);
            }
            for (I = 1; I <= N.v; I++)
            {
                Y[I + o_y] = SAVF[I + o_savf] - ACOR[I + o_acor];
            }
            DEL = this._dvnorm.Run(N.v, Y, offset_y, EWT, offset_ewt);
            for (I = 1; I <= N.v; I++)
            {
                Y[I + o_y] = YH[I+1 * LDYH + o_yh] + SAVF[I + o_savf];
            }
            this._dcopy.Run(N.v, SAVF, offset_savf, 1, ref ACOR, offset_acor, 1);
            goto LABEL400;
            // C-----------------------------------------------------------------------
            // C In the case of the chord method, compute the corrector error,
            // C and solve the linear system with that as right-hand side and
            // C P as coefficient matrix.  The correction is scaled by the factor
            // C 2/(1+RC) to account for changes in h*rl1 since the last DVJAC call.
            // C-----------------------------------------------------------------------
        LABEL350:  
            for (I = 1; I <= N.v; I++)
            {
                Y[I + o_y] = (RL1.v * H.v) * SAVF[I + o_savf] - (RL1.v * YH[I+2 * LDYH + o_yh] + ACOR[I + o_acor]);
            }
            this._dvsol.Run(ref WM, offset_wm, IWM, offset_iwm, ref Y, offset_y, ref IERSL);
            NNI.v += 1;
            if (IERSL > 0) goto LABEL410;
            if (METH.v == 2 && RC.v != ONE)
            {
                CSCALE = TWO / (ONE + RC.v);
                this._dscal.Run(N.v, CSCALE, ref Y, offset_y, 1);
            }
            DEL = this._dvnorm.Run(N.v, Y, offset_y, EWT, offset_ewt);
            this._daxpy.Run(N.v, ONE, Y, offset_y, 1, ref ACOR, offset_acor, 1);
            for (I = 1; I <= N.v; I++)
            {
                Y[I + o_y] = YH[I+1 * LDYH + o_yh] + ACOR[I + o_acor];
            }
            // C-----------------------------------------------------------------------
            // C Test for convergence.  If M .gt. 0, an estimate of the convergence
            // C rate constant is stored in CRATE, and this is used in the test.
            // C-----------------------------------------------------------------------
        LABEL400:  
            if (M != 0) CRATE.v = Math.Max(CRDOWN * CRATE.v, DEL / DELP);
            DCON = DEL * Math.Min(ONE, CRATE.v) / TQ[4 + o_tq].v;
            if (DCON <= ONE) goto LABEL450;
            M += 1;
            if (M == MAXCOR) goto LABEL410;
            if (M >= 2 && DEL > RDIV * DELP) goto LABEL410;
            DELP = DEL;
            F.Run(N.v, TN.v, Y, offset_y, ref SAVF, offset_savf, RPAR[1 + o_rpar], IPAR[1 + o_ipar]);
            NFE.v += 1;
            goto LABEL270;
            // C
        LABEL410:  
            if (MITER.v == 0 || JCUR.v == 1) goto LABEL430;
            ICF.v = 1;
            IPUP.v = MITER.v;
            goto LABEL220;
            // C
        LABEL430:;
            NFLAG =  - 1;
            ICF.v = 2;
            IPUP.v = MITER.v;
            return;
            // C
            // C Return for successful step. ------------------------------------------
        LABEL450:  NFLAG = 0;
            JCUR.v = 0;
            ICF.v = 0;
            if (M == 0) ACNRM.v = DEL;
            if (M > 0) ACNRM.v = this._dvnorm.Run(N.v, ACOR, offset_acor, EWT, offset_ewt);
            return;
            // C----------------------- End of Subroutine DVNLSD ----------------------
            #endregion
        }
    }

    #endregion


    #region The Class: DVJAC
    
    // *DECK DVJAC
    public class DVJAC
    {
    
        #region Dependencies
        
        DVNORM _dvnorm; DCOPY _dcopy; DSCAL _dscal; DGEFA _dgefa; DACOPY _dacopy; DGBFA _dgbfa; 
        #endregion
        #region Common variables
        
        #region Common Block: DVOD01 Declaration
        
        CommonBlock _dvod01;
        Odouble ACNRM; Odouble CCMXJ; Odouble CONP; Odouble CRATE; Odouble DRC; Odouble[] EL; int o_el; Odouble ETA; 
        Odouble ETAMAX;Odouble H; Odouble HMIN; Odouble HMXI; Odouble HNEW; Odouble HSCAL; Odouble PRL1; Odouble RC; Odouble RL1; 
        Odouble[] TAU; int o_tau;Odouble[] TQ; int o_tq; Odouble TN; Odouble UROUND; 
        Oint ICF; Oint INIT; Oint IPUP; Oint JCUR; Oint JSTART; Oint JSV; Oint KFLAG; Oint KUTH; Oint L; Oint LMAX; Oint LYH; 
        Oint LEWT;Oint LACOR; Oint LSAVF; Oint LWM; Oint LIWM; Oint LOCJS; Oint MAXORD; Oint METH; Oint MITER; Oint MSBJ; 
        Oint MXHNIL;Oint MXSTEP; Oint N; Oint NEWH; Oint NEWQ; Oint NHNIL; Oint NQ; Oint NQNYH; Oint NQWAIT; Oint NSLJ; Oint NSLP; 
        Oint NYH;
        #endregion
        #region Common Block: DVOD02 Declaration
        
        CommonBlock _dvod02;
        Odouble HU; 
        Oint NCFN; Oint NETF; Oint NFE; Oint NJE; Oint NLU; Oint NNI; Oint NQU; Oint NST; 
        #endregion
        #endregion
        #region Variables
        
        double ONE = 0; double PT1 = 0; double THOU = 0; double ZERO = 0; 
        #endregion
        public DVJAC(DVNORM dvnorm, DCOPY dcopy, DSCAL dscal, DGEFA dgefa, DACOPY dacopy, DGBFA dgbfa, CommonBlock DVOD01, CommonBlock DVOD02)
        {
    
            #region Set Dependencies
            
            this._dvnorm = dvnorm; this._dcopy = dcopy; this._dscal = dscal; this._dgefa = dgefa; this._dacopy = dacopy; 
            this._dgbfa = dgbfa;
            #endregion
            #region Data Initialization
            
            //ONE/1.0D0
            ONE = 1.0E0;
            //THOU/1000.0D0
            THOU = 1000.0E0;
            //ZERO/0.0D0
            ZERO = 0.0E0;
            //PT1/0.1D0
            PT1 = 0.1E0;
            #endregion
            #region Common varaible Initialization
            
            #region Common Block: DVOD01 Initialization
            
            this._dvod01 = DVOD01;
            ACNRM = DVOD01.doubleData[0];
            CCMXJ = DVOD01.doubleData[1];
            CONP = DVOD01.doubleData[2];
            CRATE = DVOD01.doubleData[3];
            DRC = DVOD01.doubleData[4];
            //Start Array: EL  StartIndex: 5  LastIndex: 17
            EL = DVOD01.doubleData;
            o_el = 4;  //o_ = StartIndex-1
            //End Array: EL
            ETA = DVOD01.doubleData[18];
            ETAMAX = DVOD01.doubleData[19];
            H = DVOD01.doubleData[20];
            HMIN = DVOD01.doubleData[21];
            HMXI = DVOD01.doubleData[22];
            HNEW = DVOD01.doubleData[23];
            HSCAL = DVOD01.doubleData[24];
            PRL1 = DVOD01.doubleData[25];
            RC = DVOD01.doubleData[26];
            RL1 = DVOD01.doubleData[27];
            //Start Array: TAU  StartIndex: 28  LastIndex: 40
            TAU = DVOD01.doubleData;
            o_tau = 27;  //o_ = StartIndex-1
            //End Array: TAU
            //Start Array: TQ  StartIndex: 41  LastIndex: 45
            TQ = DVOD01.doubleData;
            o_tq = 40;  //o_ = StartIndex-1
            //End Array: TQ
            TN = DVOD01.doubleData[46];
            UROUND = DVOD01.doubleData[47];
            ICF = DVOD01.intData[0];
            INIT = DVOD01.intData[1];
            IPUP = DVOD01.intData[2];
            JCUR = DVOD01.intData[3];
            JSTART = DVOD01.intData[4];
            JSV = DVOD01.intData[5];
            KFLAG = DVOD01.intData[6];
            KUTH = DVOD01.intData[7];
            L = DVOD01.intData[8];
            LMAX = DVOD01.intData[9];
            LYH = DVOD01.intData[10];
            LEWT = DVOD01.intData[11];
            LACOR = DVOD01.intData[12];
            LSAVF = DVOD01.intData[13];
            LWM = DVOD01.intData[14];
            LIWM = DVOD01.intData[15];
            LOCJS = DVOD01.intData[16];
            MAXORD = DVOD01.intData[17];
            METH = DVOD01.intData[18];
            MITER = DVOD01.intData[19];
            MSBJ = DVOD01.intData[20];
            MXHNIL = DVOD01.intData[21];
            MXSTEP = DVOD01.intData[22];
            N = DVOD01.intData[23];
            NEWH = DVOD01.intData[24];
            NEWQ = DVOD01.intData[25];
            NHNIL = DVOD01.intData[26];
            NQ = DVOD01.intData[27];
            NQNYH = DVOD01.intData[28];
            NQWAIT = DVOD01.intData[29];
            NSLJ = DVOD01.intData[30];
            NSLP = DVOD01.intData[31];
            NYH = DVOD01.intData[32];
            #endregion
            #region Common Block: DVOD02 Initialization
            
            this._dvod02 = DVOD02;
            HU = DVOD02.doubleData[0];
            NCFN = DVOD02.intData[0];
            NETF = DVOD02.intData[1];
            NFE = DVOD02.intData[2];
            NJE = DVOD02.intData[3];
            NLU = DVOD02.intData[4];
            NNI = DVOD02.intData[5];
            NQU = DVOD02.intData[6];
            NST = DVOD02.intData[7];
            #endregion
            #endregion
        }
    
        public DVJAC()
        {
    
            #region Initialization Common Blocks
            
            CommonBlock DVOD01 = new CommonBlock(48, 33, 0, 0);
            CommonBlock DVOD02 = new CommonBlock(1, 8, 0, 0);
            #endregion
            #region Dependencies (Initialization)
            
            DVNORM dvnorm = new DVNORM();
            DCOPY dcopy = new DCOPY();
            DSCAL dscal = new DSCAL();
            IDAMAX idamax = new IDAMAX();
            DAXPY daxpy = new DAXPY();
            DGEFA dgefa = new DGEFA(idamax, dscal, daxpy);
            DACOPY dacopy = new DACOPY(dcopy);
            DGBFA dgbfa = new DGBFA(idamax, dscal, daxpy);
            #endregion
            #region Set Dependencies
            
            this._dvnorm = dvnorm; this._dcopy = dcopy; this._dscal = dscal; this._dgefa = dgefa; this._dacopy = dacopy; 
            this._dgbfa = dgbfa;
            #endregion
            #region Data Initialization
            
            //ONE/1.0D0
            ONE = 1.0E0;
            //THOU/1000.0D0
            THOU = 1000.0E0;
            //ZERO/0.0D0
            ZERO = 0.0E0;
            //PT1/0.1D0
            PT1 = 0.1E0;
            #endregion
            #region Common varaible Initialization
            
            #region Common Block: DVOD01 Initialization
            
            this._dvod01 = DVOD01;
            ACNRM = DVOD01.doubleData[0];
            CCMXJ = DVOD01.doubleData[1];
            CONP = DVOD01.doubleData[2];
            CRATE = DVOD01.doubleData[3];
            DRC = DVOD01.doubleData[4];
            //Start Array: EL  StartIndex: 5  LastIndex: 17
            EL = DVOD01.doubleData;
            o_el = 4;  //o_ = StartIndex-1
            //End Array: EL
            ETA = DVOD01.doubleData[18];
            ETAMAX = DVOD01.doubleData[19];
            H = DVOD01.doubleData[20];
            HMIN = DVOD01.doubleData[21];
            HMXI = DVOD01.doubleData[22];
            HNEW = DVOD01.doubleData[23];
            HSCAL = DVOD01.doubleData[24];
            PRL1 = DVOD01.doubleData[25];
            RC = DVOD01.doubleData[26];
            RL1 = DVOD01.doubleData[27];
            //Start Array: TAU  StartIndex: 28  LastIndex: 40
            TAU = DVOD01.doubleData;
            o_tau = 27;  //o_ = StartIndex-1
            //End Array: TAU
            //Start Array: TQ  StartIndex: 41  LastIndex: 45
            TQ = DVOD01.doubleData;
            o_tq = 40;  //o_ = StartIndex-1
            //End Array: TQ
            TN = DVOD01.doubleData[46];
            UROUND = DVOD01.doubleData[47];
            ICF = DVOD01.intData[0];
            INIT = DVOD01.intData[1];
            IPUP = DVOD01.intData[2];
            JCUR = DVOD01.intData[3];
            JSTART = DVOD01.intData[4];
            JSV = DVOD01.intData[5];
            KFLAG = DVOD01.intData[6];
            KUTH = DVOD01.intData[7];
            L = DVOD01.intData[8];
            LMAX = DVOD01.intData[9];
            LYH = DVOD01.intData[10];
            LEWT = DVOD01.intData[11];
            LACOR = DVOD01.intData[12];
            LSAVF = DVOD01.intData[13];
            LWM = DVOD01.intData[14];
            LIWM = DVOD01.intData[15];
            LOCJS = DVOD01.intData[16];
            MAXORD = DVOD01.intData[17];
            METH = DVOD01.intData[18];
            MITER = DVOD01.intData[19];
            MSBJ = DVOD01.intData[20];
            MXHNIL = DVOD01.intData[21];
            MXSTEP = DVOD01.intData[22];
            N = DVOD01.intData[23];
            NEWH = DVOD01.intData[24];
            NEWQ = DVOD01.intData[25];
            NHNIL = DVOD01.intData[26];
            NQ = DVOD01.intData[27];
            NQNYH = DVOD01.intData[28];
            NQWAIT = DVOD01.intData[29];
            NSLJ = DVOD01.intData[30];
            NSLP = DVOD01.intData[31];
            NYH = DVOD01.intData[32];
            #endregion
            #region Common Block: DVOD02 Initialization
            
            this._dvod02 = DVOD02;
            HU = DVOD02.doubleData[0];
            NCFN = DVOD02.intData[0];
            NETF = DVOD02.intData[1];
            NFE = DVOD02.intData[2];
            NJE = DVOD02.intData[3];
            NLU = DVOD02.intData[4];
            NNI = DVOD02.intData[5];
            NQU = DVOD02.intData[6];
            NST = DVOD02.intData[7];
            #endregion
            #endregion
        }
        /// <param name="Y">
        /// = Vector containing predicted values on entry.
        ///</param>
        /// <param name="YH">
        /// = The Nordsieck array, an LDYH by LMAX array, input.
        ///</param>
        /// <param name="LDYH">
        /// = A constant .ge. N, the first dimension of YH, input.
        ///</param>
        /// <param name="EWT">
        /// = An error weight vector of length N.
        ///</param>
        /// <param name="SAVF">
        /// = Array containing f evaluated at predicted y, input.
        ///</param>
        /// <param name="WM">
        /// = Real work space for matrices.  In the output, it containS
        /// the inverse diagonal matrix if MITER = 3 and the LU
        /// decomposition of P if MITER is 1, 2 , 4, or 5.
        /// Storage of matrix elements starts at WM(3).
        /// Storage of the saved Jacobian starts at WM(LOCJS).
        /// WM also contains the following matrix-related data:
        /// WM(1) = SQRT(UROUND), used in numerical Jacobian step.
        /// WM(2) = H*RL1, saved for later use if MITER = 3.
        ///</param>
        /// <param name="IWM">
        /// = Integer work space containing pivot information,
        /// starting at IWM(31), if MITER is 1, 2, 4, or 5.
        /// IWM also contains band parameters ML = IWM(1) and
        /// MU = IWM(2) if MITER is 4 or 5.
        ///</param>
        /// <param name="F">
        /// = Dummy name for the user supplied subroutine for f.
        ///</param>
        /// <param name="JAC">
        /// = Dummy name for the user supplied Jacobian subroutine.
        ///</param>
        /// <param name="IERPJ">
        /// = Output error flag,  = 0 if no trouble, 1 if the P
        /// matrix is found to be singular.
        ///</param>
        public void Run(ref double[] Y, int offset_y, double[] YH, int offset_yh, int LDYH, double[] EWT, int offset_ewt, ref double[] FTEM, int offset_ftem, double[] SAVF, int offset_savf
                         , ref double[] WM, int offset_wm, ref int[] IWM, int offset_iwm, IFEX F, IJEX JAC, ref int IERPJ, double[] RPAR, int offset_rpar
                         , int[] IPAR, int offset_ipar)
        {
            #region Variables
            
            double CON = 0; double DI = 0; double FAC = 0; double HRL1 = 0; double R = 0; double R0 = 0; double SRUR = 0; 
            double YI = 0;double YJ = 0; double YJJ = 0; int I = 0; int I1 = 0; int I2 = 0; int IER = 0; int II = 0; int J = 0; 
            int J1 = 0;int JJ = 0; int JOK = 0; int LENP = 0; int MBA = 0; int MBAND = 0; int MEB1 = 0; int MEBAND = 0; 
            int ML = 0;int ML3 = 0; int MU = 0; int NP1 = 0; 
            #endregion
            #region Implicit Variables
            
            int YH_2 = 0; int YH_1 = 0; 
            #endregion
            #region Array Index Correction
            
             int o_y = -1 + offset_y;  int o_yh = -1 - LDYH + offset_yh;  int o_ewt = -1 + offset_ewt; 
             int o_ftem = -1 + offset_ftem; int o_savf = -1 + offset_savf;  int o_wm = -1 + offset_wm; 
             int o_iwm = -1 + offset_iwm; int o_rpar = -1 + offset_rpar;  int o_ipar = -1 + offset_ipar; 
            #endregion
            #region Prolog
            
            // C-----------------------------------------------------------------------
            // C Call sequence input -- Y, YH, LDYH, EWT, FTEM, SAVF, WM, IWM,
            // C                        F, JAC, RPAR, IPAR
            // C Call sequence output -- WM, IWM, IERPJ
            // C COMMON block variables accessed:
            // C     /DVOD01/  CCMXJ, DRC, H, RL1, TN, UROUND, ICF, JCUR, LOCJS,
            // C               MITER, MSBJ, N, NSLJ
            // C     /DVOD02/  NFE, NST, NJE, NLU
            // C
            // C Subroutines called by DVJAC: F, JAC, DACOPY, DCOPY, DGBFA, DGEFA,
            // C                              DSCAL
            // C Function routines called by DVJAC: DVNORM
            // C-----------------------------------------------------------------------
            // C DVJAC is called by DVNLSD to compute and process the matrix
            // C P = I - h*rl1*J , where J is an approximation to the Jacobian.
            // C Here J is computed by the user-supplied routine JAC if
            // C MITER = 1 or 4, or by finite differencing if MITER = 2, 3, or 5.
            // C If MITER = 3, a diagonal approximation to J is used.
            // C If JSV = -1, J is computed from scratch in all cases.
            // C If JSV = 1 and MITER = 1, 2, 4, or 5, and if the saved value of J is
            // C considered acceptable, then P is constructed from the saved J.
            // C J is stored in wm and replaced by P.  If MITER .ne. 3, P is then
            // C subjected to LU decomposition in preparation for later solution
            // C of linear systems with P as coefficient matrix. This is done
            // C by DGEFA if MITER = 1 or 2, and by DGBFA if MITER = 4 or 5.
            // C
            // C Communication with DVJAC is done with the following variables.  (For
            // C more details, please see the comments in the driver subroutine.)
            // C Y          = Vector containing predicted values on entry.
            // C YH         = The Nordsieck array, an LDYH by LMAX array, input.
            // C LDYH       = A constant .ge. N, the first dimension of YH, input.
            // C EWT        = An error weight vector of length N.
            // C SAVF       = Array containing f evaluated at predicted y, input.
            // C WM         = Real work space for matrices.  In the output, it containS
            // C              the inverse diagonal matrix if MITER = 3 and the LU
            // C              decomposition of P if MITER is 1, 2 , 4, or 5.
            // C              Storage of matrix elements starts at WM(3).
            // C              Storage of the saved Jacobian starts at WM(LOCJS).
            // C              WM also contains the following matrix-related data:
            // C              WM(1) = SQRT(UROUND), used in numerical Jacobian step.
            // C              WM(2) = H*RL1, saved for later use if MITER = 3.
            // C IWM        = Integer work space containing pivot information,
            // C              starting at IWM(31), if MITER is 1, 2, 4, or 5.
            // C              IWM also contains band parameters ML = IWM(1) and
            // C              MU = IWM(2) if MITER is 4 or 5.
            // C F          = Dummy name for the user supplied subroutine for f.
            // C JAC        = Dummy name for the user supplied Jacobian subroutine.
            // C RPAR, IPAR = Dummy names for user's real and integer work arrays.
            // C RL1        = 1/EL(2) (input).
            // C IERPJ      = Output error flag,  = 0 if no trouble, 1 if the P
            // C              matrix is found to be singular.
            // C JCUR       = Output flag to indicate whether the Jacobian matrix
            // C              (or approximation) is now current.
            // C              JCUR = 0 means J is not current.
            // C              JCUR = 1 means J is current.
            // C-----------------------------------------------------------------------
            // C
            // C Type declarations for labeled COMMON block DVOD01 --------------------
            // C
            // C
            // C Type declarations for labeled COMMON block DVOD02 --------------------
            // C
            // C
            // C Type declarations for local variables --------------------------------
            // C
            // C
            // C Type declaration for function subroutines called ---------------------
            // C
            // C-----------------------------------------------------------------------
            // C The following Fortran-77 declaration is to cause the values of the
            // C listed (local) variables to be saved between calls to this subroutine.
            // C-----------------------------------------------------------------------
            // C-----------------------------------------------------------------------
            // C
            // C
            #endregion
            #region Body
            
            IERPJ = 0;
            HRL1 = H.v * RL1.v;
            // C See whether J should be evaluated (JOK = -1) or not (JOK = 1). -------
            JOK = JSV.v;
            if (JSV.v == 1)
            {
                if (NST.v == 0 || NST.v > NSLJ.v + MSBJ.v) JOK =  - 1;
                if (ICF.v == 1 && DRC.v < CCMXJ.v) JOK =  - 1;
                if (ICF.v == 2) JOK =  - 1;
            }
            // C End of setting JOK. --------------------------------------------------
            // C
            if (JOK ==  - 1 && MITER.v == 1)
            {
                // C If JOK = -1 and MITER = 1, call JAC to evaluate Jacobian. ------------
                NJE.v += 1;
                NSLJ.v = NST.v;
                JCUR.v = 1;
                LENP = N.v * N.v;
                for (I = 1; I <= LENP; I++)
                {
                    WM[I + 2 + o_wm] = ZERO;
                }
                JAC.Run(N.v, TN.v, Y, offset_y, 0, 0, ref WM, 3 + o_wm
                        , N.v, RPAR[1 + o_rpar], IPAR[1 + o_ipar]);
                if (JSV.v == 1) this._dcopy.Run(LENP, WM, 3 + o_wm, 1, ref WM, LOCJS.v + o_wm, 1);
            }
            // C
            if (JOK ==  - 1 && MITER.v == 2)
            {
                // C If MITER = 2, make N calls to F to approximate the Jacobian. ---------
                NJE.v += 1;
                NSLJ.v = NST.v;
                JCUR.v = 1;
                FAC = this._dvnorm.Run(N.v, SAVF, offset_savf, EWT, offset_ewt);
                R0 = THOU * Math.Abs(H.v) * UROUND.v * Convert.ToSingle(N.v) * FAC;
                if (R0 == ZERO) R0 = ONE;
                SRUR = WM[1 + o_wm];
                J1 = 2;
                for (J = 1; J <= N.v; J++)
                {
                    YJ = Y[J + o_y];
                    R = Math.Max(SRUR * Math.Abs(YJ), R0 / EWT[J + o_ewt]);
                    Y[J + o_y] += R;
                    FAC = ONE / R;
                    F.Run(N.v, TN.v, Y, offset_y, ref FTEM, offset_ftem, RPAR[1 + o_rpar], IPAR[1 + o_ipar]);
                    for (I = 1; I <= N.v; I++)
                    {
                        WM[I + J1 + o_wm] = (FTEM[I + o_ftem] - SAVF[I + o_savf]) * FAC;
                    }
                    Y[J + o_y] = YJ;
                    J1 += N.v;
                }
                NFE.v += N.v;
                LENP = N.v * N.v;
                if (JSV.v == 1) this._dcopy.Run(LENP, WM, 3 + o_wm, 1, ref WM, LOCJS.v + o_wm, 1);
            }
            // C
            if (JOK == 1 && (MITER.v == 1 || MITER.v == 2))
            {
                JCUR.v = 0;
                LENP = N.v * N.v;
                this._dcopy.Run(LENP, WM, LOCJS.v + o_wm, 1, ref WM, 3 + o_wm, 1);
            }
            // C
            if (MITER.v == 1 || MITER.v == 2)
            {
                // C Multiply Jacobian by scalar, add identity, and do LU decomposition. --
                CON =  - HRL1;
                this._dscal.Run(LENP, CON, ref WM, 3 + o_wm, 1);
                J = 3;
                NP1 = N.v + 1;
                for (I = 1; I <= N.v; I++)
                {
                    WM[J + o_wm] += ONE;
                    J += NP1;
                }
                NLU.v += 1;
                this._dgefa.Run(ref WM, 3 + o_wm, N.v, N.v, ref IWM, 31 + o_iwm, ref IER);
                if (IER != 0) IERPJ = 1;
                return;
            }
            // C End of code block for MITER = 1 or 2. --------------------------------
            // C
            if (MITER.v == 3)
            {
                // C If MITER = 3, construct a diagonal approximation to J and P. ---------
                NJE.v += 1;
                JCUR.v = 1;
                WM[2 + o_wm] = HRL1;
                R = RL1.v * PT1;
                for (I = 1; I <= N.v; I++)
                {
                    Y[I + o_y] += R * (H.v * SAVF[I + o_savf] - YH[I+2 * LDYH + o_yh]);
                }
                F.Run(N.v, TN.v, Y, offset_y, ref WM, 3 + o_wm, RPAR[1 + o_rpar], IPAR[1 + o_ipar]);
                NFE.v += 1;
                YH_2 = 2 * LDYH + o_yh;
                for (I = 1; I <= N.v; I++)
                {
                    R0 = H.v * SAVF[I + o_savf] - YH[I + YH_2];
                    DI = PT1 * R0 - H.v * (WM[I + 2 + o_wm] - SAVF[I + o_savf]);
                    WM[I + 2 + o_wm] = ONE;
                    if (Math.Abs(R0) < UROUND.v / EWT[I + o_ewt]) goto LABEL320;
                    if (Math.Abs(DI) == ZERO) goto LABEL330;
                    WM[I + 2 + o_wm] = PT1 * R0 / DI;
                LABEL320:;
                }
                return;
            LABEL330:  IERPJ = 1;
                return;
            }
            // C End of code block for MITER = 3. -------------------------------------
            // C
            // C Set constants for MITER = 4 or 5. ------------------------------------
            ML = IWM[1 + o_iwm];
            MU = IWM[2 + o_iwm];
            ML3 = ML + 3;
            MBAND = ML + MU + 1;
            MEBAND = MBAND + ML;
            LENP = MEBAND * N.v;
            // C
            if (JOK ==  - 1 && MITER.v == 4)
            {
                // C If JOK = -1 and MITER = 4, call JAC to evaluate Jacobian. ------------
                NJE.v += 1;
                NSLJ.v = NST.v;
                JCUR.v = 1;
                for (I = 1; I <= LENP; I++)
                {
                    WM[I + 2 + o_wm] = ZERO;
                }
                JAC.Run(N.v, TN.v, Y, offset_y, ML, MU, ref WM, ML3 + o_wm
                        , MEBAND, RPAR[1 + o_rpar], IPAR[1 + o_ipar]);
                if (JSV.v == 1) this._dacopy.Run(MBAND, N.v, WM, ML3 + o_wm, MEBAND, ref WM, LOCJS.v + o_wm, MBAND);
            }
            // C
            if (JOK ==  - 1 && MITER.v == 5)
            {
                // C If MITER = 5, make ML+MU+1 calls to F to approximate the Jacobian. ---
                NJE.v += 1;
                NSLJ.v = NST.v;
                JCUR.v = 1;
                MBA = Math.Min(MBAND, N.v);
                MEB1 = MEBAND - 1;
                SRUR = WM[1 + o_wm];
                FAC = this._dvnorm.Run(N.v, SAVF, offset_savf, EWT, offset_ewt);
                R0 = THOU * Math.Abs(H.v) * UROUND.v * Convert.ToSingle(N.v) * FAC;
                if (R0 == ZERO) R0 = ONE;
                for (J = 1; J <= MBA; J++)
                {
                    for (I = J; (MBAND >= 0) ? (I <= N.v) : (I >= N.v); I += MBAND)
                    {
                        YI = Y[I + o_y];
                        R = Math.Max(SRUR * Math.Abs(YI), R0 / EWT[I + o_ewt]);
                        Y[I + o_y] += R;
                    }
                    F.Run(N.v, TN.v, Y, offset_y, ref FTEM, offset_ftem, RPAR[1 + o_rpar], IPAR[1 + o_ipar]);
                    YH_1 = 1 * LDYH + o_yh;
                    for (JJ = J; (MBAND >= 0) ? (JJ <= N.v) : (JJ >= N.v); JJ += MBAND)
                    {
                        Y[JJ + o_y] = YH[JJ + YH_1];
                        YJJ = Y[JJ + o_y];
                        R = Math.Max(SRUR * Math.Abs(YJJ), R0 / EWT[JJ + o_ewt]);
                        FAC = ONE / R;
                        I1 = Math.Max(JJ - MU, 1);
                        I2 = Math.Min(JJ + ML, N.v);
                        II = JJ * MEB1 - ML + 2;
                        for (I = I1; I <= I2; I++)
                        {
                            WM[II + I + o_wm] = (FTEM[I + o_ftem] - SAVF[I + o_savf]) * FAC;
                        }
                    }
                }
                NFE.v += MBA;
                if (JSV.v == 1) this._dacopy.Run(MBAND, N.v, WM, ML3 + o_wm, MEBAND, ref WM, LOCJS.v + o_wm, MBAND);
            }
            // C
            if (JOK == 1)
            {
                JCUR.v = 0;
                this._dacopy.Run(MBAND, N.v, WM, LOCJS.v + o_wm, MBAND, ref WM, ML3 + o_wm, MEBAND);
            }
            // C
            // C Multiply Jacobian by scalar, add identity, and do LU decomposition.
            CON =  - HRL1;
            this._dscal.Run(LENP, CON, ref WM, 3 + o_wm, 1);
            II = MBAND + 2;
            for (I = 1; I <= N.v; I++)
            {
                WM[II + o_wm] += ONE;
                II += MEBAND;
            }
            NLU.v += 1;
            this._dgbfa.Run(ref WM, 3 + o_wm, MEBAND, N.v, ML, MU, ref IWM, 31 + o_iwm
                            , ref IER);
            if (IER != 0) IERPJ = 1;
            return;
            // C End of code block for MITER = 4 or 5. --------------------------------
            // C
            // C----------------------- End of Subroutine DVJAC -----------------------
            #endregion
        }
    }

    #endregion


    #region The Class: DACOPY
    
    // *DECK DACOPY
    public class DACOPY
    {
    
        #region Dependencies
        
        DCOPY _dcopy; 
        #endregion
        public DACOPY(DCOPY dcopy)
        {
    
            #region Set Dependencies
            
            this._dcopy = dcopy; 
            #endregion
        }
    
        public DACOPY()
        {
    
            #region Dependencies (Initialization)
            
            DCOPY dcopy = new DCOPY();
            #endregion
            #region Set Dependencies
            
            this._dcopy = dcopy; 
            #endregion
        }
        public void Run(int NROW, int NCOL, double[] A, int offset_a, int NROWA, ref double[] B, int offset_b, int NROWB)
        {
            #region Variables
            
            int IC = 0; 
            #endregion
            #region Array Index Correction
            
             int o_a = -1 - NROWA + offset_a;  int o_b = -1 - NROWB + offset_b; 
            #endregion
            #region Prolog
            
            // C-----------------------------------------------------------------------
            // C Call sequence input -- NROW, NCOL, A, NROWA, NROWB
            // C Call sequence output -- B
            // C COMMON block variables accessed -- None
            // C
            // C Subroutines called by DACOPY: DCOPY
            // C Function routines called by DACOPY: None
            // C-----------------------------------------------------------------------
            // C This routine copies one rectangular array, A, to another, B,
            // C where A and B may have different row dimensions, NROWA and NROWB.
            // C The data copied consists of NROW rows and NCOL columns.
            // C-----------------------------------------------------------------------
            // C
            #endregion
            for (IC = 1; IC <= NCOL; IC++)
            {
                this._dcopy.Run(NROW, A, 1+IC * NROWA + o_a, 1, ref B, 1+IC * NROWB + o_b, 1);
            }
            // C
            return;
            // C----------------------- End of Subroutine DACOPY ----------------------
        }
    }

    #endregion


    #region The Class: DVSOL
    
    // *DECK DVSOL
    public class DVSOL
    {
    
        #region Dependencies
        
        DGESL _dgesl; DGBSL _dgbsl; 
        #endregion
        #region Common variables
        
        #region Common Block: DVOD01 Declaration
        
        CommonBlock _dvod01;
        Odouble ACNRM; Odouble CCMXJ; Odouble CONP; Odouble CRATE; Odouble DRC; Odouble[] EL; int o_el; Odouble ETA; 
        Odouble ETAMAX;Odouble H; Odouble HMIN; Odouble HMXI; Odouble HNEW; Odouble HSCAL; Odouble PRL1; Odouble RC; Odouble RL1; 
        Odouble[] TAU; int o_tau;Odouble[] TQ; int o_tq; Odouble TN; Odouble UROUND; 
        Oint ICF; Oint INIT; Oint IPUP; Oint JCUR; Oint JSTART; Oint JSV; Oint KFLAG; Oint KUTH; Oint L; Oint LMAX; Oint LYH; 
        Oint LEWT;Oint LACOR; Oint LSAVF; Oint LWM; Oint LIWM; Oint LOCJS; Oint MAXORD; Oint METH; Oint MITER; Oint MSBJ; 
        Oint MXHNIL;Oint MXSTEP; Oint N; Oint NEWH; Oint NEWQ; Oint NHNIL; Oint NQ; Oint NQNYH; Oint NQWAIT; Oint NSLJ; Oint NSLP; 
        Oint NYH;
        #endregion
        #endregion
        #region Variables
        
        double ONE = 0; double ZERO = 0; 
        #endregion
        public DVSOL(DGESL dgesl, DGBSL dgbsl, CommonBlock DVOD01)
        {
    
            #region Set Dependencies
            
            this._dgesl = dgesl; this._dgbsl = dgbsl; 
            #endregion
            #region Data Initialization
            
            //ONE/1.0D0
            ONE = 1.0E0;
            //ZERO/0.0D0
            ZERO = 0.0E0;
            #endregion
            #region Common varaible Initialization
            
            #region Common Block: DVOD01 Initialization
            
            this._dvod01 = DVOD01;
            ACNRM = DVOD01.doubleData[0];
            CCMXJ = DVOD01.doubleData[1];
            CONP = DVOD01.doubleData[2];
            CRATE = DVOD01.doubleData[3];
            DRC = DVOD01.doubleData[4];
            //Start Array: EL  StartIndex: 5  LastIndex: 17
            EL = DVOD01.doubleData;
            o_el = 4;  //o_ = StartIndex-1
            //End Array: EL
            ETA = DVOD01.doubleData[18];
            ETAMAX = DVOD01.doubleData[19];
            H = DVOD01.doubleData[20];
            HMIN = DVOD01.doubleData[21];
            HMXI = DVOD01.doubleData[22];
            HNEW = DVOD01.doubleData[23];
            HSCAL = DVOD01.doubleData[24];
            PRL1 = DVOD01.doubleData[25];
            RC = DVOD01.doubleData[26];
            RL1 = DVOD01.doubleData[27];
            //Start Array: TAU  StartIndex: 28  LastIndex: 40
            TAU = DVOD01.doubleData;
            o_tau = 27;  //o_ = StartIndex-1
            //End Array: TAU
            //Start Array: TQ  StartIndex: 41  LastIndex: 45
            TQ = DVOD01.doubleData;
            o_tq = 40;  //o_ = StartIndex-1
            //End Array: TQ
            TN = DVOD01.doubleData[46];
            UROUND = DVOD01.doubleData[47];
            ICF = DVOD01.intData[0];
            INIT = DVOD01.intData[1];
            IPUP = DVOD01.intData[2];
            JCUR = DVOD01.intData[3];
            JSTART = DVOD01.intData[4];
            JSV = DVOD01.intData[5];
            KFLAG = DVOD01.intData[6];
            KUTH = DVOD01.intData[7];
            L = DVOD01.intData[8];
            LMAX = DVOD01.intData[9];
            LYH = DVOD01.intData[10];
            LEWT = DVOD01.intData[11];
            LACOR = DVOD01.intData[12];
            LSAVF = DVOD01.intData[13];
            LWM = DVOD01.intData[14];
            LIWM = DVOD01.intData[15];
            LOCJS = DVOD01.intData[16];
            MAXORD = DVOD01.intData[17];
            METH = DVOD01.intData[18];
            MITER = DVOD01.intData[19];
            MSBJ = DVOD01.intData[20];
            MXHNIL = DVOD01.intData[21];
            MXSTEP = DVOD01.intData[22];
            N = DVOD01.intData[23];
            NEWH = DVOD01.intData[24];
            NEWQ = DVOD01.intData[25];
            NHNIL = DVOD01.intData[26];
            NQ = DVOD01.intData[27];
            NQNYH = DVOD01.intData[28];
            NQWAIT = DVOD01.intData[29];
            NSLJ = DVOD01.intData[30];
            NSLP = DVOD01.intData[31];
            NYH = DVOD01.intData[32];
            #endregion
            #endregion
        }
    
        public DVSOL()
        {
    
            #region Initialization Common Blocks
            
            CommonBlock DVOD01 = new CommonBlock(48, 33, 0, 0);
            #endregion
            #region Dependencies (Initialization)
            
            DDOT ddot = new DDOT();
            DAXPY daxpy = new DAXPY();
            DGESL dgesl = new DGESL(ddot, daxpy);
            DGBSL dgbsl = new DGBSL(ddot, daxpy);
            #endregion
            #region Set Dependencies
            
            this._dgesl = dgesl; this._dgbsl = dgbsl; 
            #endregion
            #region Data Initialization
            
            //ONE/1.0D0
            ONE = 1.0E0;
            //ZERO/0.0D0
            ZERO = 0.0E0;
            #endregion
            #region Common varaible Initialization
            
            #region Common Block: DVOD01 Initialization
            
            this._dvod01 = DVOD01;
            ACNRM = DVOD01.doubleData[0];
            CCMXJ = DVOD01.doubleData[1];
            CONP = DVOD01.doubleData[2];
            CRATE = DVOD01.doubleData[3];
            DRC = DVOD01.doubleData[4];
            //Start Array: EL  StartIndex: 5  LastIndex: 17
            EL = DVOD01.doubleData;
            o_el = 4;  //o_ = StartIndex-1
            //End Array: EL
            ETA = DVOD01.doubleData[18];
            ETAMAX = DVOD01.doubleData[19];
            H = DVOD01.doubleData[20];
            HMIN = DVOD01.doubleData[21];
            HMXI = DVOD01.doubleData[22];
            HNEW = DVOD01.doubleData[23];
            HSCAL = DVOD01.doubleData[24];
            PRL1 = DVOD01.doubleData[25];
            RC = DVOD01.doubleData[26];
            RL1 = DVOD01.doubleData[27];
            //Start Array: TAU  StartIndex: 28  LastIndex: 40
            TAU = DVOD01.doubleData;
            o_tau = 27;  //o_ = StartIndex-1
            //End Array: TAU
            //Start Array: TQ  StartIndex: 41  LastIndex: 45
            TQ = DVOD01.doubleData;
            o_tq = 40;  //o_ = StartIndex-1
            //End Array: TQ
            TN = DVOD01.doubleData[46];
            UROUND = DVOD01.doubleData[47];
            ICF = DVOD01.intData[0];
            INIT = DVOD01.intData[1];
            IPUP = DVOD01.intData[2];
            JCUR = DVOD01.intData[3];
            JSTART = DVOD01.intData[4];
            JSV = DVOD01.intData[5];
            KFLAG = DVOD01.intData[6];
            KUTH = DVOD01.intData[7];
            L = DVOD01.intData[8];
            LMAX = DVOD01.intData[9];
            LYH = DVOD01.intData[10];
            LEWT = DVOD01.intData[11];
            LACOR = DVOD01.intData[12];
            LSAVF = DVOD01.intData[13];
            LWM = DVOD01.intData[14];
            LIWM = DVOD01.intData[15];
            LOCJS = DVOD01.intData[16];
            MAXORD = DVOD01.intData[17];
            METH = DVOD01.intData[18];
            MITER = DVOD01.intData[19];
            MSBJ = DVOD01.intData[20];
            MXHNIL = DVOD01.intData[21];
            MXSTEP = DVOD01.intData[22];
            N = DVOD01.intData[23];
            NEWH = DVOD01.intData[24];
            NEWQ = DVOD01.intData[25];
            NHNIL = DVOD01.intData[26];
            NQ = DVOD01.intData[27];
            NQNYH = DVOD01.intData[28];
            NQWAIT = DVOD01.intData[29];
            NSLJ = DVOD01.intData[30];
            NSLP = DVOD01.intData[31];
            NYH = DVOD01.intData[32];
            #endregion
            #endregion
        }
        /// <param name="WM">
        /// = Real work space containing the inverse diagonal matrix if
        /// MITER = 3 and the LU decomposition of the matrix otherwise.
        /// Storage of matrix elements starts at WM(3).
        /// WM also contains the following matrix-related data:
        /// WM(1) = SQRT(UROUND) (not used here),
        /// WM(2) = HRL1, the previous value of H*RL1, used if MITER = 3.
        ///</param>
        /// <param name="IWM">
        /// = Integer work space containing pivot information, starting at
        /// IWM(31), if MITER is 1, 2, 4, or 5.  IWM also contains band
        /// parameters ML = IWM(1) and MU = IWM(2) if MITER is 4 or 5.
        ///</param>
        /// <param name="X">
        /// = The right-hand side vector on input, and the solution vector
        /// on output, of length N.
        ///</param>
        /// <param name="IERSL">
        /// = Output flag.  IERSL = 0 if no trouble occurred.
        /// IERSL = 1 if a singular matrix arose with MITER = 3.
        ///</param>
        public void Run(ref double[] WM, int offset_wm, int[] IWM, int offset_iwm, ref double[] X, int offset_x, ref int IERSL)
        {
            #region Variables
            
            int I = 0; int MEBAND = 0; int ML = 0; int MU = 0; double DI = 0; double HRL1 = 0; double PHRL1 = 0; double R = 0; 
            #endregion
            #region Array Index Correction
            
             int o_wm = -1 + offset_wm;  int o_iwm = -1 + offset_iwm;  int o_x = -1 + offset_x; 
            #endregion
            #region Prolog
            
            // C-----------------------------------------------------------------------
            // C Call sequence input -- WM, IWM, X
            // C Call sequence output -- X, IERSL
            // C COMMON block variables accessed:
            // C     /DVOD01/ -- H, RL1, MITER, N
            // C
            // C Subroutines called by DVSOL: DGESL, DGBSL
            // C Function routines called by DVSOL: None
            // C-----------------------------------------------------------------------
            // C This routine manages the solution of the linear system arising from
            // C a chord iteration.  It is called if MITER .ne. 0.
            // C If MITER is 1 or 2, it calls DGESL to accomplish this.
            // C If MITER = 3 it updates the coefficient H*RL1 in the diagonal
            // C matrix, and then computes the solution.
            // C If MITER is 4 or 5, it calls DGBSL.
            // C Communication with DVSOL uses the following variables:
            // C WM    = Real work space containing the inverse diagonal matrix if
            // C         MITER = 3 and the LU decomposition of the matrix otherwise.
            // C         Storage of matrix elements starts at WM(3).
            // C         WM also contains the following matrix-related data:
            // C         WM(1) = SQRT(UROUND) (not used here),
            // C         WM(2) = HRL1, the previous value of H*RL1, used if MITER = 3.
            // C IWM   = Integer work space containing pivot information, starting at
            // C         IWM(31), if MITER is 1, 2, 4, or 5.  IWM also contains band
            // C         parameters ML = IWM(1) and MU = IWM(2) if MITER is 4 or 5.
            // C X     = The right-hand side vector on input, and the solution vector
            // C         on output, of length N.
            // C IERSL = Output flag.  IERSL = 0 if no trouble occurred.
            // C         IERSL = 1 if a singular matrix arose with MITER = 3.
            // C-----------------------------------------------------------------------
            // C
            // C Type declarations for labeled COMMON block DVOD01 --------------------
            // C
            // C
            // C Type declarations for local variables --------------------------------
            // C
            // C-----------------------------------------------------------------------
            // C The following Fortran-77 declaration is to cause the values of the
            // C listed (local) variables to be saved between calls to this integrator.
            // C-----------------------------------------------------------------------
            // C
            // C
            // C
            #endregion
            #region Body
            
            IERSL = 0;
            switch (MITER.v)
            {
                case 1: goto LABEL100;
                case 2: goto LABEL100;
                case 3: goto LABEL300;
                case 4: goto LABEL400;
                case 5: goto LABEL400;
            }
        LABEL100:  this._dgesl.Run(WM, 3 + o_wm, N.v, N.v, IWM, 31 + o_iwm, ref X, offset_x, 0);
            return;
            // C
        LABEL300:  PHRL1 = WM[2 + o_wm];
            HRL1 = H.v * RL1.v;
            WM[2 + o_wm] = HRL1;
            if (HRL1 == PHRL1) goto LABEL330;
            R = HRL1 / PHRL1;
            for (I = 1; I <= N.v; I++)
            {
                DI = ONE - R * (ONE - ONE / WM[I + 2 + o_wm]);
                if (Math.Abs(DI) == ZERO) goto LABEL390;
                WM[I + 2 + o_wm] = ONE / DI;
            }
            // C
        LABEL330:  
            for (I = 1; I <= N.v; I++)
            {
                X[I + o_x] *= WM[I + 2 + o_wm];
            }
            return;
        LABEL390:  IERSL = 1;
            return;
            // C
        LABEL400:  ML = IWM[1 + o_iwm];
            MU = IWM[2 + o_iwm];
            MEBAND = 2 * ML + MU + 1;
            this._dgbsl.Run(WM, 3 + o_wm, MEBAND, N.v, ML, MU, IWM, 31 + o_iwm
                            , ref X, offset_x, 0);
            return;
            // C----------------------- End of Subroutine DVSOL -----------------------
            #endregion
        }
    }

    #endregion


    #region The Class: DVSRCO
    
    // *DECK DVSRCO
    public class DVSRCO
    {
    
        #region Common variables
        
        #region Common Block: DVOD01 Declaration
        
        CommonBlock _dvod01;
        Odouble[] RVOD1; int o_rvod1; 
        Oint[] IVOD1; int o_ivod1; 
        #endregion
        #region Common Block: DVOD02 Declaration
        
        CommonBlock _dvod02;
        Odouble[] RVOD2; int o_rvod2; 
        Oint[] IVOD2; int o_ivod2; 
        #endregion
        #endregion
        #region Variables
        
        int LENIV1 = 0; int LENIV2 = 0; int LENRV1 = 0; int LENRV2 = 0; 
        #endregion
        public DVSRCO(CommonBlock DVOD01, CommonBlock DVOD02)
        {
    
            #region Data Initialization
            
            //LENRV1/48
            LENRV1 = 48;
            //LENIV1/33
            LENIV1 = 33;
            //LENRV2/1
            LENRV2 = 1;
            //LENIV2/8
            LENIV2 = 8;
            #endregion
            #region Common varaible Initialization
            
            #region Common Block: DVOD01 Initialization
            
            this._dvod01 = DVOD01;
            //Start Array: RVOD1  StartIndex: 0  LastIndex: 47
            RVOD1 = DVOD01.doubleData;
            o_rvod1 = -1;  //o_ = StartIndex-1
            //End Array: RVOD1
            //Start Array: IVOD1  StartIndex: 0  LastIndex: 32
            IVOD1 = DVOD01.intData;
            o_ivod1 = -1;  //o_ = StartIndex-1
            //End Array: IVOD1
            #endregion
            #region Common Block: DVOD02 Initialization
            
            this._dvod02 = DVOD02;
            //Start Array: RVOD2  StartIndex: 0  LastIndex: 0
            RVOD2 = DVOD02.doubleData;
            o_rvod2 = -1;  //o_ = StartIndex-1
            //End Array: RVOD2
            //Start Array: IVOD2  StartIndex: 0  LastIndex: 7
            IVOD2 = DVOD02.intData;
            o_ivod2 = -1;  //o_ = StartIndex-1
            //End Array: IVOD2
            #endregion
            #endregion
        }
    
        public DVSRCO()
        {
    
            #region Initialization Common Blocks
            
            CommonBlock DVOD01 = new CommonBlock(48, 33, 0, 0);
            CommonBlock DVOD02 = new CommonBlock(1, 8, 0, 0);
            #endregion
            #region Data Initialization
            
            //LENRV1/48
            LENRV1 = 48;
            //LENIV1/33
            LENIV1 = 33;
            //LENRV2/1
            LENRV2 = 1;
            //LENIV2/8
            LENIV2 = 8;
            #endregion
            #region Common varaible Initialization
            
            #region Common Block: DVOD01 Initialization
            
            this._dvod01 = DVOD01;
            //Start Array: RVOD1  StartIndex: 0  LastIndex: 47
            RVOD1 = DVOD01.doubleData;
            o_rvod1 = -1;  //o_ = StartIndex-1
            //End Array: RVOD1
            //Start Array: IVOD1  StartIndex: 0  LastIndex: 32
            IVOD1 = DVOD01.intData;
            o_ivod1 = -1;  //o_ = StartIndex-1
            //End Array: IVOD1
            #endregion
            #region Common Block: DVOD02 Initialization
            
            this._dvod02 = DVOD02;
            //Start Array: RVOD2  StartIndex: 0  LastIndex: 0
            RVOD2 = DVOD02.doubleData;
            o_rvod2 = -1;  //o_ = StartIndex-1
            //End Array: RVOD2
            //Start Array: IVOD2  StartIndex: 0  LastIndex: 7
            IVOD2 = DVOD02.intData;
            o_ivod2 = -1;  //o_ = StartIndex-1
            //End Array: IVOD2
            #endregion
            #endregion
        }
        /// <param name="RSAV">
        /// = real array of length 49 or more.
        ///</param>
        /// <param name="ISAV">
        /// = integer array of length 41 or more.
        ///</param>
        /// <param name="JOB">
        /// = flag indicating to save or restore the COMMON blocks:
        /// JOB  = 1 if COMMON is to be saved (written to RSAV/ISAV).
        /// JOB  = 2 if COMMON is to be restored (read from RSAV/ISAV).
        /// A call with JOB = 2 presumes a prior call with JOB = 1.
        ///</param>
        public void Run(ref double[] RSAV, int offset_rsav, ref int[] ISAV, int offset_isav, int JOB)
        {
            #region Variables
            
            int I = 0; 
            #endregion
            #region Array Index Correction
            
             int o_rsav = -1 + offset_rsav;  int o_isav = -1 + offset_isav; 
            #endregion
            #region Prolog
            
            // C-----------------------------------------------------------------------
            // C Call sequence input -- RSAV, ISAV, JOB
            // C Call sequence output -- RSAV, ISAV
            // C COMMON block variables accessed -- All of /DVOD01/ and /DVOD02/
            // C
            // C Subroutines/functions called by DVSRCO: None
            // C-----------------------------------------------------------------------
            // C This routine saves or restores (depending on JOB) the contents of the
            // C COMMON blocks DVOD01 and DVOD02, which are used internally by DVODE.
            // C
            // C RSAV = real array of length 49 or more.
            // C ISAV = integer array of length 41 or more.
            // C JOB  = flag indicating to save or restore the COMMON blocks:
            // C        JOB  = 1 if COMMON is to be saved (written to RSAV/ISAV).
            // C        JOB  = 2 if COMMON is to be restored (read from RSAV/ISAV).
            // C        A call with JOB = 2 presumes a prior call with JOB = 1.
            // C-----------------------------------------------------------------------
            // C-----------------------------------------------------------------------
            // C The following Fortran-77 declaration is to cause the values of the
            // C listed (local) variables to be saved between calls to this integrator.
            // C-----------------------------------------------------------------------
            // C
            // C
            #endregion
            #region Body
            
            if (JOB == 2) goto LABEL100;
            for (I = 1; I <= LENRV1; I++)
            {
                RSAV[I + o_rsav] = RVOD1[I + o_rvod1].v;
            }
            for (I = 1; I <= LENRV2; I++)
            {
                RSAV[LENRV1 + I + o_rsav] = RVOD2[I + o_rvod2].v;
            }
            // C
            for (I = 1; I <= LENIV1; I++)
            {
                ISAV[I + o_isav] = IVOD1[I + o_ivod1].v;
            }
            for (I = 1; I <= LENIV2; I++)
            {
                ISAV[LENIV1 + I + o_isav] = IVOD2[I + o_ivod2].v;
            }
            // C
            return;
            // C
        LABEL100:;
            for (I = 1; I <= LENRV1; I++)
            {
                RVOD1[I + o_rvod1].v = RSAV[I + o_rsav];
            }
            for (I = 1; I <= LENRV2; I++)
            {
                RVOD2[I + o_rvod2].v = RSAV[LENRV1 + I + o_rsav];
            }
            // C
            for (I = 1; I <= LENIV1; I++)
            {
                IVOD1[I + o_ivod1].v = ISAV[I + o_isav];
            }
            for (I = 1; I <= LENIV2; I++)
            {
                IVOD2[I + o_ivod2].v = ISAV[LENIV1 + I + o_isav];
            }
            // C
            return;
            // C----------------------- End of Subroutine DVSRCO ----------------------
            #endregion
        }
    }

    #endregion


    #region The Class: DEWSET
    
    // *DECK DEWSET
    /// <summary>
    /// ***PURPOSE  Set error weight vector.
    /// ***TYPE      DOUBLE PRECISION (SEWSET-S, DEWSET-D)
    /// ***AUTHOR  Hindmarsh, Alan C., (LLNL)
    /// ***DESCRIPTION
    /// 
    /// This subroutine sets the error weight vector EWT according to
    /// EWT(i) = RTOL(i)*ABS(YCUR(i)) + ATOL(i),  i = 1,...,N,
    /// with the subscript on RTOL and/or ATOL possibly replaced by 1 above,
    /// depending on the value of ITOL.
    /// 
    /// ***SEE ALSO  DLSODE
    /// ***ROUTINES CALLED  (NONE)
    /// ***REVISION HISTORY  (YYMMDD)
    /// 791129  DATE WRITTEN
    /// 890501  Modified prologue to SLATEC/LDOC format.  (FNF)
    /// 890503  Minor cosmetic changes.  (FNF)
    /// 930809  Renamed to allow single/double precision versions. (ACH)
    /// ***END PROLOGUE  DEWSET
    /// **End
    /// 
    /// ***FIRST EXECUTABLE STATEMENT  DEWSET
    ///</summary>
    public class DEWSET
    {
    
        public DEWSET()
        {
    
        }
    
        /// <summary>
        /// ***PURPOSE  Set error weight vector.
        /// ***TYPE      DOUBLE PRECISION (SEWSET-S, DEWSET-D)
        /// ***AUTHOR  Hindmarsh, Alan C., (LLNL)
        /// ***DESCRIPTION
        /// 
        /// This subroutine sets the error weight vector EWT according to
        /// EWT(i) = RTOL(i)*ABS(YCUR(i)) + ATOL(i),  i = 1,...,N,
        /// with the subscript on RTOL and/or ATOL possibly replaced by 1 above,
        /// depending on the value of ITOL.
        /// 
        /// ***SEE ALSO  DLSODE
        /// ***ROUTINES CALLED  (NONE)
        /// ***REVISION HISTORY  (YYMMDD)
        /// 791129  DATE WRITTEN
        /// 890501  Modified prologue to SLATEC/LDOC format.  (FNF)
        /// 890503  Minor cosmetic changes.  (FNF)
        /// 930809  Renamed to allow single/double precision versions. (ACH)
        /// ***END PROLOGUE  DEWSET
        /// **End
        /// 
        /// ***FIRST EXECUTABLE STATEMENT  DEWSET
        ///</summary>
        public void Run(int N, int ITOL, double[] RTOL, int offset_rtol, double[] ATOL, int offset_atol, double[] YCUR, int offset_ycur, ref double[] EWT, int offset_ewt)
        {
            #region Variables
            
            int I = 0; 
            #endregion
            #region Array Index Correction
            
             int o_rtol = -1 + offset_rtol;  int o_atol = -1 + offset_atol;  int o_ycur = -1 + offset_ycur; 
             int o_ewt = -1 + offset_ewt;
            #endregion
            #region Prolog
            
            // C***BEGIN PROLOGUE  DEWSET
            // C***SUBSIDIARY
            // C***PURPOSE  Set error weight vector.
            // C***TYPE      DOUBLE PRECISION (SEWSET-S, DEWSET-D)
            // C***AUTHOR  Hindmarsh, Alan C., (LLNL)
            // C***DESCRIPTION
            // C
            // C  This subroutine sets the error weight vector EWT according to
            // C      EWT(i) = RTOL(i)*ABS(YCUR(i)) + ATOL(i),  i = 1,...,N,
            // C  with the subscript on RTOL and/or ATOL possibly replaced by 1 above,
            // C  depending on the value of ITOL.
            // C
            // C***SEE ALSO  DLSODE
            // C***ROUTINES CALLED  (NONE)
            // C***REVISION HISTORY  (YYMMDD)
            // C   791129  DATE WRITTEN
            // C   890501  Modified prologue to SLATEC/LDOC format.  (FNF)
            // C   890503  Minor cosmetic changes.  (FNF)
            // C   930809  Renamed to allow single/double precision versions. (ACH)
            // C***END PROLOGUE  DEWSET
            // C**End
            // C
            // C***FIRST EXECUTABLE STATEMENT  DEWSET
            #endregion
            #region Body
            
            switch (ITOL)
            {
                case 1: goto LABEL10;
                case 2: goto LABEL20;
                case 3: goto LABEL30;
                case 4: goto LABEL40;
            }
        LABEL10:;
            for (I = 1; I <= N; I++)
            {
                EWT[I + o_ewt] = RTOL[1 + o_rtol] * Math.Abs(YCUR[I + o_ycur]) + ATOL[1 + o_atol];
            }
            return;
        LABEL20:;
            for (I = 1; I <= N; I++)
            {
                EWT[I + o_ewt] = RTOL[1 + o_rtol] * Math.Abs(YCUR[I + o_ycur]) + ATOL[I + o_atol];
            }
            return;
        LABEL30:;
            for (I = 1; I <= N; I++)
            {
                EWT[I + o_ewt] = RTOL[I + o_rtol] * Math.Abs(YCUR[I + o_ycur]) + ATOL[1 + o_atol];
            }
            return;
        LABEL40:;
            for (I = 1; I <= N; I++)
            {
                EWT[I + o_ewt] = RTOL[I + o_rtol] * Math.Abs(YCUR[I + o_ycur]) + ATOL[I + o_atol];
            }
            return;
            // C----------------------- END OF SUBROUTINE DEWSET ----------------------
            #endregion
        }
    }

    #endregion


    #region The Class: DVNORM
    
    // *DECK DVNORM
    /// <summary>
    /// ***PURPOSE  Weighted root-mean-square vector norm.
    /// ***TYPE      DOUBLE PRECISION (SVNORM-S, DVNORM-D)
    /// ***AUTHOR  Hindmarsh, Alan C., (LLNL)
    /// ***DESCRIPTION
    /// 
    /// This function routine computes the weighted root-mean-square norm
    /// of the vector of length N contained in the array V, with weights
    /// contained in the array W of length N:
    /// DVNORM = SQRT( (1/N) * SUM( V(i)*W(i) )**2 )
    /// 
    /// ***SEE ALSO  DLSODE
    /// ***ROUTINES CALLED  (NONE)
    /// ***REVISION HISTORY  (YYMMDD)
    /// 791129  DATE WRITTEN
    /// 890501  Modified prologue to SLATEC/LDOC format.  (FNF)
    /// 890503  Minor cosmetic changes.  (FNF)
    /// 930809  Renamed to allow single/double precision versions. (ACH)
    /// ***END PROLOGUE  DVNORM
    /// **End
    /// 
    /// ***FIRST EXECUTABLE STATEMENT  DVNORM
    ///</summary>
    public class DVNORM
    {
    
        public DVNORM()
        {
    
        }
    
        /// <summary>
        /// ***PURPOSE  Weighted root-mean-square vector norm.
        /// ***TYPE      DOUBLE PRECISION (SVNORM-S, DVNORM-D)
        /// ***AUTHOR  Hindmarsh, Alan C., (LLNL)
        /// ***DESCRIPTION
        /// 
        /// This function routine computes the weighted root-mean-square norm
        /// of the vector of length N contained in the array V, with weights
        /// contained in the array W of length N:
        /// DVNORM = SQRT( (1/N) * SUM( V(i)*W(i) )**2 )
        /// 
        /// ***SEE ALSO  DLSODE
        /// ***ROUTINES CALLED  (NONE)
        /// ***REVISION HISTORY  (YYMMDD)
        /// 791129  DATE WRITTEN
        /// 890501  Modified prologue to SLATEC/LDOC format.  (FNF)
        /// 890503  Minor cosmetic changes.  (FNF)
        /// 930809  Renamed to allow single/double precision versions. (ACH)
        /// ***END PROLOGUE  DVNORM
        /// **End
        /// 
        /// ***FIRST EXECUTABLE STATEMENT  DVNORM
        ///</summary>
        public double Run(int N, double[] V, int offset_v, double[] W, int offset_w)
        {
        double dvnorm = 0;
            #region Variables
            
            int I = 0; double SUM = 0; 
            #endregion
            #region Array Index Correction
            
             int o_v = -1 + offset_v;  int o_w = -1 + offset_w; 
            #endregion
            #region Prolog
            
            // C***BEGIN PROLOGUE  DVNORM
            // C***SUBSIDIARY
            // C***PURPOSE  Weighted root-mean-square vector norm.
            // C***TYPE      DOUBLE PRECISION (SVNORM-S, DVNORM-D)
            // C***AUTHOR  Hindmarsh, Alan C., (LLNL)
            // C***DESCRIPTION
            // C
            // C  This function routine computes the weighted root-mean-square norm
            // C  of the vector of length N contained in the array V, with weights
            // C  contained in the array W of length N:
            // C    DVNORM = SQRT( (1/N) * SUM( V(i)*W(i) )**2 )
            // C
            // C***SEE ALSO  DLSODE
            // C***ROUTINES CALLED  (NONE)
            // C***REVISION HISTORY  (YYMMDD)
            // C   791129  DATE WRITTEN
            // C   890501  Modified prologue to SLATEC/LDOC format.  (FNF)
            // C   890503  Minor cosmetic changes.  (FNF)
            // C   930809  Renamed to allow single/double precision versions. (ACH)
            // C***END PROLOGUE  DVNORM
            // C**End
            // C
            // C***FIRST EXECUTABLE STATEMENT  DVNORM
            #endregion
            SUM = 0.0E0;
            for (I = 1; I <= N; I++)
            {
                SUM += Math.Pow(V[I + o_v] * W[I + o_w],2);
            }
            dvnorm = Math.Sqrt(SUM / N);
            return dvnorm;
            // C----------------------- END OF FUNCTION DVNORM ------------------------
        }
    }

    #endregion


    #region The Class: XERRWD
    
    // *DECK XERRWD
    /// <summary>
    /// ***PURPOSE  Write error message with values.
    /// ***CATEGORY  R3C
    /// ***TYPE      DOUBLE PRECISION (XERRWV-S, XERRWD-D)
    /// ***AUTHOR  Hindmarsh, Alan C., (LLNL)
    /// ***DESCRIPTION
    /// 
    /// Subroutines XERRWD, XSETF, XSETUN, and the function routine IXSAV,
    /// as given here, constitute a simplified version of the SLATEC error
    /// handling package.
    /// 
    /// All arguments are input arguments.
    /// 
    /// MSG    = The message (character array).
    /// NMES   = The length of MSG (number of characters).
    /// NERR   = The error number (not used).
    /// LEVEL  = The error level..
    /// 0 or 1 means recoverable (control returns to caller).
    /// 2 means fatal (run is aborted--see note below).
    /// NI     = Number of integers (0, 1, or 2) to be printed with message.
    /// I1,I2  = Integers to be printed, depending on NI.
    /// NR     = Number of reals (0, 1, or 2) to be printed with message.
    /// R1,R2  = Reals to be printed, depending on NR.
    /// 
    /// Note..  this routine is machine-dependent and specialized for use
    /// in limited context, in the following ways..
    /// 1. The argument MSG is assumed to be of type CHARACTER, and
    /// the message is printed with a format of (1X,A).
    /// 2. The message is assumed to take only one line.
    /// Multi-line messages are generated by repeated calls.
    /// 3. If LEVEL = 2, control passes to the statement   STOP
    /// to abort the run.  This statement may be machine-dependent.
    /// 4. R1 and R2 are assumed to be in double precision and are printed
    /// in D21.13 format.
    /// 
    /// ***ROUTINES CALLED  IXSAV
    /// ***REVISION HISTORY  (YYMMDD)
    /// 920831  DATE WRITTEN
    /// 921118  Replaced MFLGSV/LUNSAV by IXSAV. (ACH)
    /// 930329  Modified prologue to SLATEC format. (FNF)
    /// 930407  Changed MSG from CHARACTER*1 array to variable. (FNF)
    /// 930922  Minor cosmetic change. (FNF)
    /// ***END PROLOGUE  XERRWD
    /// 
    /// *Internal Notes:
    /// 
    /// For a different default logical unit number, IXSAV (or a subsidiary
    /// routine that it calls) will need to be modified.
    /// For a different run-abort command, change the statement following
    /// statement 100 at the end.
    /// -----------------------------------------------------------------------
    /// Subroutines called by XERRWD.. None
    /// Function routine called by XERRWD.. IXSAV
    /// -----------------------------------------------------------------------
    /// **End
    /// 
    /// Declare arguments.
    ///</summary>
    public class XERRWD
    {
    
        #region Dependencies
        
        IXSAV _ixsav; 
        #endregion
        public XERRWD(IXSAV ixsav)
        {
    
            #region Set Dependencies
            
            this._ixsav = ixsav; 
            #endregion
        }
    
        public XERRWD()
        {
    
            #region Dependencies (Initialization)
            
            IUMACH iumach = new IUMACH();
            IXSAV ixsav = new IXSAV(iumach);
            #endregion
            #region Set Dependencies
            
            this._ixsav = ixsav; 
            #endregion
        }
        /// <summary>
        /// ***PURPOSE  Write error message with values.
        /// ***CATEGORY  R3C
        /// ***TYPE      DOUBLE PRECISION (XERRWV-S, XERRWD-D)
        /// ***AUTHOR  Hindmarsh, Alan C., (LLNL)
        /// ***DESCRIPTION
        /// 
        /// Subroutines XERRWD, XSETF, XSETUN, and the function routine IXSAV,
        /// as given here, constitute a simplified version of the SLATEC error
        /// handling package.
        /// 
        /// All arguments are input arguments.
        /// 
        /// MSG    = The message (character array).
        /// NMES   = The length of MSG (number of characters).
        /// NERR   = The error number (not used).
        /// LEVEL  = The error level..
        /// 0 or 1 means recoverable (control returns to caller).
        /// 2 means fatal (run is aborted--see note below).
        /// NI     = Number of integers (0, 1, or 2) to be printed with message.
        /// I1,I2  = Integers to be printed, depending on NI.
        /// NR     = Number of reals (0, 1, or 2) to be printed with message.
        /// R1,R2  = Reals to be printed, depending on NR.
        /// 
        /// Note..  this routine is machine-dependent and specialized for use
        /// in limited context, in the following ways..
        /// 1. The argument MSG is assumed to be of type CHARACTER, and
        /// the message is printed with a format of (1X,A).
        /// 2. The message is assumed to take only one line.
        /// Multi-line messages are generated by repeated calls.
        /// 3. If LEVEL = 2, control passes to the statement   STOP
        /// to abort the run.  This statement may be machine-dependent.
        /// 4. R1 and R2 are assumed to be in double precision and are printed
        /// in D21.13 format.
        /// 
        /// ***ROUTINES CALLED  IXSAV
        /// ***REVISION HISTORY  (YYMMDD)
        /// 920831  DATE WRITTEN
        /// 921118  Replaced MFLGSV/LUNSAV by IXSAV. (ACH)
        /// 930329  Modified prologue to SLATEC format. (FNF)
        /// 930407  Changed MSG from CHARACTER*1 array to variable. (FNF)
        /// 930922  Minor cosmetic change. (FNF)
        /// ***END PROLOGUE  XERRWD
        /// 
        /// *Internal Notes:
        /// 
        /// For a different default logical unit number, IXSAV (or a subsidiary
        /// routine that it calls) will need to be modified.
        /// For a different run-abort command, change the statement following
        /// statement 100 at the end.
        /// -----------------------------------------------------------------------
        /// Subroutines called by XERRWD.. None
        /// Function routine called by XERRWD.. IXSAV
        /// -----------------------------------------------------------------------
        /// **End
        /// 
        /// Declare arguments.
        ///</summary>
        /// <param name="MSG">
        /// = The message (character array).
        ///</param>
        /// <param name="NMES">
        /// = The length of MSG (number of characters).
        ///</param>
        /// <param name="NERR">
        /// = The error number (not used).
        ///</param>
        /// <param name="LEVEL">
        /// = The error level..
        /// 0 or 1 means recoverable (control returns to caller).
        /// 2 means fatal (run is aborted--see note below).
        ///</param>
        /// <param name="NI">
        /// = Number of integers (0, 1, or 2) to be printed with message.
        ///</param>
        /// <param name="NR">
        /// = Number of reals (0, 1, or 2) to be printed with message.
        ///</param>
        public void Run(string MSG, int NMES, int NERR, int LEVEL, int NI, int I1
                         , int I2, int NR, double R1, double R2)
        {
            #region Variables
            
            int LUNIT = 0; int MESFLG = 0; 
            #endregion
            #region Prolog
            
            // C***BEGIN PROLOGUE  XERRWD
            // C***SUBSIDIARY
            // C***PURPOSE  Write error message with values.
            // C***CATEGORY  R3C
            // C***TYPE      DOUBLE PRECISION (XERRWV-S, XERRWD-D)
            // C***AUTHOR  Hindmarsh, Alan C., (LLNL)
            // C***DESCRIPTION
            // C
            // C  Subroutines XERRWD, XSETF, XSETUN, and the function routine IXSAV,
            // C  as given here, constitute a simplified version of the SLATEC error
            // C  handling package.
            // C
            // C  All arguments are input arguments.
            // C
            // C  MSG    = The message (character array).
            // C  NMES   = The length of MSG (number of characters).
            // C  NERR   = The error number (not used).
            // C  LEVEL  = The error level..
            // C           0 or 1 means recoverable (control returns to caller).
            // C           2 means fatal (run is aborted--see note below).
            // C  NI     = Number of integers (0, 1, or 2) to be printed with message.
            // C  I1,I2  = Integers to be printed, depending on NI.
            // C  NR     = Number of reals (0, 1, or 2) to be printed with message.
            // C  R1,R2  = Reals to be printed, depending on NR.
            // C
            // C  Note..  this routine is machine-dependent and specialized for use
            // C  in limited context, in the following ways..
            // C  1. The argument MSG is assumed to be of type CHARACTER, and
            // C     the message is printed with a format of (1X,A).
            // C  2. The message is assumed to take only one line.
            // C     Multi-line messages are generated by repeated calls.
            // C  3. If LEVEL = 2, control passes to the statement   STOP
            // C     to abort the run.  This statement may be machine-dependent.
            // C  4. R1 and R2 are assumed to be in double precision and are printed
            // C     in D21.13 format.
            // C
            // C***ROUTINES CALLED  IXSAV
            // C***REVISION HISTORY  (YYMMDD)
            // C   920831  DATE WRITTEN
            // C   921118  Replaced MFLGSV/LUNSAV by IXSAV. (ACH)
            // C   930329  Modified prologue to SLATEC format. (FNF)
            // C   930407  Changed MSG from CHARACTER*1 array to variable. (FNF)
            // C   930922  Minor cosmetic change. (FNF)
            // C***END PROLOGUE  XERRWD
            // C
            // C*Internal Notes:
            // C
            // C For a different default logical unit number, IXSAV (or a subsidiary
            // C routine that it calls) will need to be modified.
            // C For a different run-abort command, change the statement following
            // C statement 100 at the end.
            // C-----------------------------------------------------------------------
            // C Subroutines called by XERRWD.. None
            // C Function routine called by XERRWD.. IXSAV
            // C-----------------------------------------------------------------------
            // C**End
            // C
            // C  Declare arguments.
            // C
            // C
            // C  Declare local variables.
            // C
            // C
            // C  Get logical unit number and message print flag.
            // C
            // C***FIRST EXECUTABLE STATEMENT  XERRWD
            #endregion
            LUNIT = this._ixsav.Run(1, 0, false);
            MESFLG = this._ixsav.Run(2, 0, false);
            if (MESFLG == 0) goto LABEL100;
            // C
            // C  Write the message.
            // C
            //ERROR-ERROR      WRITE (LUNIT,10)  MSG;
            if (NI == 1) ;//ERROR-ERRORWRITE(LUNIT,20)I1
            if (NI == 2) ;//ERROR-ERRORWRITE(LUNIT,30)I1,I2
            if (NR == 1) ;//ERROR-ERRORWRITE(LUNIT,40)R1
            if (NR == 2) ;//ERROR-ERRORWRITE(LUNIT,50)R1,R2
            // C
            // C  Abort the run if LEVEL = 2.
            // C
        LABEL100:  
            if (LEVEL != 2) return;
            return;
            // C----------------------- End of Subroutine XERRWD ----------------------
        }
    }

    #endregion


    #region The Class: XSETF
    
    // *DECK XSETF
    /// <summary>
    /// ***PURPOSE  Reset the error print control flag.
    /// ***CATEGORY  R3A
    /// ***TYPE      ALL (XSETF-A)
    /// ***KEYWORDS  ERROR CONTROL
    /// ***AUTHOR  Hindmarsh, Alan C., (LLNL)
    /// ***DESCRIPTION
    /// 
    /// XSETF sets the error print control flag to MFLAG:
    /// MFLAG=1 means print all messages (the default).
    /// MFLAG=0 means no printing.
    /// 
    /// ***SEE ALSO  XERRWD, XERRWV
    /// ***REFERENCES  (NONE)
    /// ***ROUTINES CALLED  IXSAV
    /// ***REVISION HISTORY  (YYMMDD)
    /// 921118  DATE WRITTEN
    /// 930329  Added SLATEC format prologue. (FNF)
    /// 930407  Corrected SEE ALSO section. (FNF)
    /// 930922  Made user-callable, and other cosmetic changes. (FNF)
    /// ***END PROLOGUE  XSETF
    /// 
    /// Subroutines called by XSETF.. None
    /// Function routine called by XSETF.. IXSAV
    /// -----------------------------------------------------------------------
    /// **End
    /// 
    /// ***FIRST EXECUTABLE STATEMENT  XSETF
    ///</summary>
    public class XSETF
    {
    
        #region Dependencies
        
        IXSAV _ixsav; 
        #endregion
        public XSETF(IXSAV ixsav)
        {
    
            #region Set Dependencies
            
            this._ixsav = ixsav; 
            #endregion
        }
    
        public XSETF()
        {
    
            #region Dependencies (Initialization)
            
            IUMACH iumach = new IUMACH();
            IXSAV ixsav = new IXSAV(iumach);
            #endregion
            #region Set Dependencies
            
            this._ixsav = ixsav; 
            #endregion
        }
        /// <summary>
        /// ***PURPOSE  Reset the error print control flag.
        /// ***CATEGORY  R3A
        /// ***TYPE      ALL (XSETF-A)
        /// ***KEYWORDS  ERROR CONTROL
        /// ***AUTHOR  Hindmarsh, Alan C., (LLNL)
        /// ***DESCRIPTION
        /// 
        /// XSETF sets the error print control flag to MFLAG:
        /// MFLAG=1 means print all messages (the default).
        /// MFLAG=0 means no printing.
        /// 
        /// ***SEE ALSO  XERRWD, XERRWV
        /// ***REFERENCES  (NONE)
        /// ***ROUTINES CALLED  IXSAV
        /// ***REVISION HISTORY  (YYMMDD)
        /// 921118  DATE WRITTEN
        /// 930329  Added SLATEC format prologue. (FNF)
        /// 930407  Corrected SEE ALSO section. (FNF)
        /// 930922  Made user-callable, and other cosmetic changes. (FNF)
        /// ***END PROLOGUE  XSETF
        /// 
        /// Subroutines called by XSETF.. None
        /// Function routine called by XSETF.. IXSAV
        /// -----------------------------------------------------------------------
        /// **End
        /// 
        /// ***FIRST EXECUTABLE STATEMENT  XSETF
        ///</summary>
        public void Run(int MFLAG)
        {
            #region Variables
            
            int JUNK = 0; 
            #endregion
            #region Prolog
            
            // C***BEGIN PROLOGUE  XSETF
            // C***PURPOSE  Reset the error print control flag.
            // C***CATEGORY  R3A
            // C***TYPE      ALL (XSETF-A)
            // C***KEYWORDS  ERROR CONTROL
            // C***AUTHOR  Hindmarsh, Alan C., (LLNL)
            // C***DESCRIPTION
            // C
            // C   XSETF sets the error print control flag to MFLAG:
            // C      MFLAG=1 means print all messages (the default).
            // C      MFLAG=0 means no printing.
            // C
            // C***SEE ALSO  XERRWD, XERRWV
            // C***REFERENCES  (NONE)
            // C***ROUTINES CALLED  IXSAV
            // C***REVISION HISTORY  (YYMMDD)
            // C   921118  DATE WRITTEN
            // C   930329  Added SLATEC format prologue. (FNF)
            // C   930407  Corrected SEE ALSO section. (FNF)
            // C   930922  Made user-callable, and other cosmetic changes. (FNF)
            // C***END PROLOGUE  XSETF
            // C
            // C Subroutines called by XSETF.. None
            // C Function routine called by XSETF.. IXSAV
            // C-----------------------------------------------------------------------
            // C**End
            // C
            // C***FIRST EXECUTABLE STATEMENT  XSETF
            #endregion
            if (MFLAG == 0 || MFLAG == 1) JUNK = this._ixsav.Run(2, MFLAG, true);
            return;
            // C----------------------- End of Subroutine XSETF -----------------------
        }
    }

    #endregion


    #region The Class: XSETUN
    
    // *DECK XSETUN
    /// <summary>
    /// ***PURPOSE  Reset the logical unit number for error messages.
    /// ***CATEGORY  R3B
    /// ***TYPE      ALL (XSETUN-A)
    /// ***KEYWORDS  ERROR CONTROL
    /// ***DESCRIPTION
    /// 
    /// XSETUN sets the logical unit number for error messages to LUN.
    /// 
    /// ***AUTHOR  Hindmarsh, Alan C., (LLNL)
    /// ***SEE ALSO  XERRWD, XERRWV
    /// ***REFERENCES  (NONE)
    /// ***ROUTINES CALLED  IXSAV
    /// ***REVISION HISTORY  (YYMMDD)
    /// 921118  DATE WRITTEN
    /// 930329  Added SLATEC format prologue. (FNF)
    /// 930407  Corrected SEE ALSO section. (FNF)
    /// 930922  Made user-callable, and other cosmetic changes. (FNF)
    /// ***END PROLOGUE  XSETUN
    /// 
    /// Subroutines called by XSETUN.. None
    /// Function routine called by XSETUN.. IXSAV
    /// -----------------------------------------------------------------------
    /// **End
    /// 
    /// ***FIRST EXECUTABLE STATEMENT  XSETUN
    ///</summary>
    public class XSETUN
    {
    
        #region Dependencies
        
        IXSAV _ixsav; 
        #endregion
        public XSETUN(IXSAV ixsav)
        {
    
            #region Set Dependencies
            
            this._ixsav = ixsav; 
            #endregion
        }
    
        public XSETUN()
        {
    
            #region Dependencies (Initialization)
            
            IUMACH iumach = new IUMACH();
            IXSAV ixsav = new IXSAV(iumach);
            #endregion
            #region Set Dependencies
            
            this._ixsav = ixsav; 
            #endregion
        }
        /// <summary>
        /// ***PURPOSE  Reset the logical unit number for error messages.
        /// ***CATEGORY  R3B
        /// ***TYPE      ALL (XSETUN-A)
        /// ***KEYWORDS  ERROR CONTROL
        /// ***DESCRIPTION
        /// 
        /// XSETUN sets the logical unit number for error messages to LUN.
        /// 
        /// ***AUTHOR  Hindmarsh, Alan C., (LLNL)
        /// ***SEE ALSO  XERRWD, XERRWV
        /// ***REFERENCES  (NONE)
        /// ***ROUTINES CALLED  IXSAV
        /// ***REVISION HISTORY  (YYMMDD)
        /// 921118  DATE WRITTEN
        /// 930329  Added SLATEC format prologue. (FNF)
        /// 930407  Corrected SEE ALSO section. (FNF)
        /// 930922  Made user-callable, and other cosmetic changes. (FNF)
        /// ***END PROLOGUE  XSETUN
        /// 
        /// Subroutines called by XSETUN.. None
        /// Function routine called by XSETUN.. IXSAV
        /// -----------------------------------------------------------------------
        /// **End
        /// 
        /// ***FIRST EXECUTABLE STATEMENT  XSETUN
        ///</summary>
        public void Run(int LUN)
        {
            #region Variables
            
            int JUNK = 0; 
            #endregion
            #region Prolog
            
            // C***BEGIN PROLOGUE  XSETUN
            // C***PURPOSE  Reset the logical unit number for error messages.
            // C***CATEGORY  R3B
            // C***TYPE      ALL (XSETUN-A)
            // C***KEYWORDS  ERROR CONTROL
            // C***DESCRIPTION
            // C
            // C   XSETUN sets the logical unit number for error messages to LUN.
            // C
            // C***AUTHOR  Hindmarsh, Alan C., (LLNL)
            // C***SEE ALSO  XERRWD, XERRWV
            // C***REFERENCES  (NONE)
            // C***ROUTINES CALLED  IXSAV
            // C***REVISION HISTORY  (YYMMDD)
            // C   921118  DATE WRITTEN
            // C   930329  Added SLATEC format prologue. (FNF)
            // C   930407  Corrected SEE ALSO section. (FNF)
            // C   930922  Made user-callable, and other cosmetic changes. (FNF)
            // C***END PROLOGUE  XSETUN
            // C
            // C Subroutines called by XSETUN.. None
            // C Function routine called by XSETUN.. IXSAV
            // C-----------------------------------------------------------------------
            // C**End
            // C
            // C***FIRST EXECUTABLE STATEMENT  XSETUN
            #endregion
            if (LUN > 0) JUNK = this._ixsav.Run(1, LUN, true);
            return;
            // C----------------------- End of Subroutine XSETUN ----------------------
        }
    }

    #endregion


    #region The Class: IXSAV
    
    // *DECK IXSAV
    /// <summary>
    /// ***PURPOSE  Save and recall error message control parameters.
    /// ***CATEGORY  R3C
    /// ***TYPE      ALL (IXSAV-A)
    /// ***AUTHOR  Hindmarsh, Alan C., (LLNL)
    /// ***DESCRIPTION
    /// 
    /// IXSAV saves and recalls one of two error message parameters:
    /// LUNIT, the logical unit number to which messages are printed, and
    /// MESFLG, the message print flag.
    /// This is a modification of the SLATEC library routine J4SAVE.
    /// 
    /// Saved local variables..
    /// LUNIT  = Logical unit number for messages.  The default is obtained
    /// by a call to IUMACH (may be machine-dependent).
    /// MESFLG = Print control flag..
    /// 1 means print all messages (the default).
    /// 0 means no printing.
    /// 
    /// On input..
    /// IPAR   = Parameter indicator (1 for LUNIT, 2 for MESFLG).
    /// IVALUE = The value to be set for the parameter, if ISET = .TRUE.
    /// ISET   = Logical flag to indicate whether to read or write.
    /// If ISET = .TRUE., the parameter will be given
    /// the value IVALUE.  If ISET = .FALSE., the parameter
    /// will be unchanged, and IVALUE is a dummy argument.
    /// 
    /// On return..
    /// IXSAV = The (old) value of the parameter.
    /// 
    /// ***SEE ALSO  XERRWD, XERRWV
    /// ***ROUTINES CALLED  IUMACH
    /// ***REVISION HISTORY  (YYMMDD)
    /// 921118  DATE WRITTEN
    /// 930329  Modified prologue to SLATEC format. (FNF)
    /// 930915  Added IUMACH call to get default output unit.  (ACH)
    /// 930922  Minor cosmetic changes. (FNF)
    /// 010425  Type declaration for IUMACH added. (ACH)
    /// ***END PROLOGUE  IXSAV
    /// 
    /// Subroutines called by IXSAV.. None
    /// Function routine called by IXSAV.. IUMACH
    /// -----------------------------------------------------------------------
    /// **End
    /// -----------------------------------------------------------------------
    /// -----------------------------------------------------------------------
    /// The following Fortran-77 declaration is to cause the values of the
    /// listed (local) variables to be saved between calls to this routine.
    /// -----------------------------------------------------------------------
    /// 
    /// ***FIRST EXECUTABLE STATEMENT  IXSAV
    ///</summary>
    public class IXSAV
    {
    
        #region Dependencies
        
        IUMACH _iumach; 
        #endregion
        #region Variables
        
        int LUNIT = 0; int MESFLG = 0; 
        #endregion
        public IXSAV(IUMACH iumach)
        {
    
            #region Set Dependencies
            
            this._iumach = iumach; 
            #endregion
            #region Data Initialization
            
            //LUNIT/-1
            LUNIT =  - 1;
            //MESFLG/1
            MESFLG = 1;
            #endregion
        }
    
        public IXSAV()
        {
    
            #region Dependencies (Initialization)
            
            IUMACH iumach = new IUMACH();
            #endregion
            #region Set Dependencies
            
            this._iumach = iumach; 
            #endregion
            #region Data Initialization
            
            //LUNIT/-1
            LUNIT =  - 1;
            //MESFLG/1
            MESFLG = 1;
            #endregion
        }
        /// <summary>
        /// ***PURPOSE  Save and recall error message control parameters.
        /// ***CATEGORY  R3C
        /// ***TYPE      ALL (IXSAV-A)
        /// ***AUTHOR  Hindmarsh, Alan C., (LLNL)
        /// ***DESCRIPTION
        /// 
        /// IXSAV saves and recalls one of two error message parameters:
        /// LUNIT, the logical unit number to which messages are printed, and
        /// MESFLG, the message print flag.
        /// This is a modification of the SLATEC library routine J4SAVE.
        /// 
        /// Saved local variables..
        /// LUNIT  = Logical unit number for messages.  The default is obtained
        /// by a call to IUMACH (may be machine-dependent).
        /// MESFLG = Print control flag..
        /// 1 means print all messages (the default).
        /// 0 means no printing.
        /// 
        /// On input..
        /// IPAR   = Parameter indicator (1 for LUNIT, 2 for MESFLG).
        /// IVALUE = The value to be set for the parameter, if ISET = .TRUE.
        /// ISET   = Logical flag to indicate whether to read or write.
        /// If ISET = .TRUE., the parameter will be given
        /// the value IVALUE.  If ISET = .FALSE., the parameter
        /// will be unchanged, and IVALUE is a dummy argument.
        /// 
        /// On return..
        /// IXSAV = The (old) value of the parameter.
        /// 
        /// ***SEE ALSO  XERRWD, XERRWV
        /// ***ROUTINES CALLED  IUMACH
        /// ***REVISION HISTORY  (YYMMDD)
        /// 921118  DATE WRITTEN
        /// 930329  Modified prologue to SLATEC format. (FNF)
        /// 930915  Added IUMACH call to get default output unit.  (ACH)
        /// 930922  Minor cosmetic changes. (FNF)
        /// 010425  Type declaration for IUMACH added. (ACH)
        /// ***END PROLOGUE  IXSAV
        /// 
        /// Subroutines called by IXSAV.. None
        /// Function routine called by IXSAV.. IUMACH
        /// -----------------------------------------------------------------------
        /// **End
        /// -----------------------------------------------------------------------
        /// -----------------------------------------------------------------------
        /// The following Fortran-77 declaration is to cause the values of the
        /// listed (local) variables to be saved between calls to this routine.
        /// -----------------------------------------------------------------------
        /// 
        /// ***FIRST EXECUTABLE STATEMENT  IXSAV
        ///</summary>
        /// <param name="IPAR">
        /// = Parameter indicator (1 for LUNIT, 2 for MESFLG).
        ///</param>
        /// <param name="IVALUE">
        /// = The value to be set for the parameter, if ISET = .TRUE.
        ///</param>
        /// <param name="ISET">
        /// = Logical flag to indicate whether to read or write.
        /// If ISET = .TRUE., the parameter will be given
        /// the value IVALUE.  If ISET = .FALSE., the parameter
        /// will be unchanged, and IVALUE is a dummy argument.
        ///</param>
        public int Run(int IPAR, int IVALUE, bool ISET)
        {
        int ixsav = 0;
            #region Prolog
            
            // C***BEGIN PROLOGUE  IXSAV
            // C***SUBSIDIARY
            // C***PURPOSE  Save and recall error message control parameters.
            // C***CATEGORY  R3C
            // C***TYPE      ALL (IXSAV-A)
            // C***AUTHOR  Hindmarsh, Alan C., (LLNL)
            // C***DESCRIPTION
            // C
            // C  IXSAV saves and recalls one of two error message parameters:
            // C    LUNIT, the logical unit number to which messages are printed, and
            // C    MESFLG, the message print flag.
            // C  This is a modification of the SLATEC library routine J4SAVE.
            // C
            // C  Saved local variables..
            // C   LUNIT  = Logical unit number for messages.  The default is obtained
            // C            by a call to IUMACH (may be machine-dependent).
            // C   MESFLG = Print control flag..
            // C            1 means print all messages (the default).
            // C            0 means no printing.
            // C
            // C  On input..
            // C    IPAR   = Parameter indicator (1 for LUNIT, 2 for MESFLG).
            // C    IVALUE = The value to be set for the parameter, if ISET = .TRUE.
            // C    ISET   = Logical flag to indicate whether to read or write.
            // C             If ISET = .TRUE., the parameter will be given
            // C             the value IVALUE.  If ISET = .FALSE., the parameter
            // C             will be unchanged, and IVALUE is a dummy argument.
            // C
            // C  On return..
            // C    IXSAV = The (old) value of the parameter.
            // C
            // C***SEE ALSO  XERRWD, XERRWV
            // C***ROUTINES CALLED  IUMACH
            // C***REVISION HISTORY  (YYMMDD)
            // C   921118  DATE WRITTEN
            // C   930329  Modified prologue to SLATEC format. (FNF)
            // C   930915  Added IUMACH call to get default output unit.  (ACH)
            // C   930922  Minor cosmetic changes. (FNF)
            // C   010425  Type declaration for IUMACH added. (ACH)
            // C***END PROLOGUE  IXSAV
            // C
            // C Subroutines called by IXSAV.. None
            // C Function routine called by IXSAV.. IUMACH
            // C-----------------------------------------------------------------------
            // C**End
            // C-----------------------------------------------------------------------
            // C-----------------------------------------------------------------------
            // C The following Fortran-77 declaration is to cause the values of the
            // C listed (local) variables to be saved between calls to this routine.
            // C-----------------------------------------------------------------------
            // C
            // C***FIRST EXECUTABLE STATEMENT  IXSAV
            #endregion
            if (IPAR == 1)
            {
                if (LUNIT ==  - 1) LUNIT = this._iumach.Run();
                ixsav = LUNIT;
                if (ISET) LUNIT = IVALUE;
            }
            // C
            if (IPAR == 2)
            {
                ixsav = MESFLG;
                if (ISET) MESFLG = IVALUE;
            }
            // C
            return ixsav;
            // C----------------------- End of Function IXSAV -------------------------
        }
    }

    #endregion


    #region The Class: IUMACH
    
    // *DECK IUMACH
    /// <summary>
    /// ***PURPOSE  Provide standard output unit number.
    /// ***CATEGORY  R1
    /// ***TYPE      INTEGER (IUMACH-I)
    /// ***KEYWORDS  MACHINE CONSTANTS
    /// ***AUTHOR  Hindmarsh, Alan C., (LLNL)
    /// ***DESCRIPTION
    /// *Usage:
    /// INTEGER  LOUT, IUMACH
    /// LOUT = IUMACH()
    /// 
    /// *Function Return Values:
    /// LOUT : the standard logical unit for Fortran output.
    /// 
    /// ***REFERENCES  (NONE)
    /// ***ROUTINES CALLED  (NONE)
    /// ***REVISION HISTORY  (YYMMDD)
    /// 930915  DATE WRITTEN
    /// 930922  Made user-callable, and other cosmetic changes. (FNF)
    /// ***END PROLOGUE  IUMACH
    /// 
    /// *Internal Notes:
    /// The built-in value of 6 is standard on a wide range of Fortran
    /// systems.  This may be machine-dependent.
    /// **End
    /// ***FIRST EXECUTABLE STATEMENT  IUMACH
    ///</summary>
    public class IUMACH
    {
    
        public IUMACH()
        {
    
        }
    
        /// <summary>
        /// ***PURPOSE  Provide standard output unit number.
        /// ***CATEGORY  R1
        /// ***TYPE      INTEGER (IUMACH-I)
        /// ***KEYWORDS  MACHINE CONSTANTS
        /// ***AUTHOR  Hindmarsh, Alan C., (LLNL)
        /// ***DESCRIPTION
        /// *Usage:
        /// INTEGER  LOUT, IUMACH
        /// LOUT = IUMACH()
        /// 
        /// *Function Return Values:
        /// LOUT : the standard logical unit for Fortran output.
        /// 
        /// ***REFERENCES  (NONE)
        /// ***ROUTINES CALLED  (NONE)
        /// ***REVISION HISTORY  (YYMMDD)
        /// 930915  DATE WRITTEN
        /// 930922  Made user-callable, and other cosmetic changes. (FNF)
        /// ***END PROLOGUE  IUMACH
        /// 
        /// *Internal Notes:
        /// The built-in value of 6 is standard on a wide range of Fortran
        /// systems.  This may be machine-dependent.
        /// **End
        /// ***FIRST EXECUTABLE STATEMENT  IUMACH
        ///</summary>
        public int Run()
        {
        int iumach = 0;
            #region Prolog
            
            // C***BEGIN PROLOGUE  IUMACH
            // C***PURPOSE  Provide standard output unit number.
            // C***CATEGORY  R1
            // C***TYPE      INTEGER (IUMACH-I)
            // C***KEYWORDS  MACHINE CONSTANTS
            // C***AUTHOR  Hindmarsh, Alan C., (LLNL)
            // C***DESCRIPTION
            // C *Usage:
            // C        INTEGER  LOUT, IUMACH
            // C        LOUT = IUMACH()
            // C
            // C *Function Return Values:
            // C     LOUT : the standard logical unit for Fortran output.
            // C
            // C***REFERENCES  (NONE)
            // C***ROUTINES CALLED  (NONE)
            // C***REVISION HISTORY  (YYMMDD)
            // C   930915  DATE WRITTEN
            // C   930922  Made user-callable, and other cosmetic changes. (FNF)
            // C***END PROLOGUE  IUMACH
            // C
            // C*Internal Notes:
            // C  The built-in value of 6 is standard on a wide range of Fortran
            // C  systems.  This may be machine-dependent.
            // C**End
            // C***FIRST EXECUTABLE STATEMENT  IUMACH
            #endregion
            iumach = 6;
            // C
            return iumach;
            // C----------------------- End of Function IUMACH ------------------------
        }
    }

    #endregion


    #region The Class: DUMACH
    
    // *DECK DUMACH
    /// <summary>
    /// ***PURPOSE  Compute the unit roundoff of the machine.
    /// ***CATEGORY  R1
    /// ***TYPE      DOUBLE PRECISION (RUMACH-S, DUMACH-D)
    /// ***KEYWORDS  MACHINE CONSTANTS
    /// ***AUTHOR  Hindmarsh, Alan C., (LLNL)
    /// ***DESCRIPTION
    /// *Usage:
    /// DOUBLE PRECISION  A, DUMACH
    /// A = DUMACH()
    /// 
    /// *Function Return Values:
    /// A : the unit roundoff of the machine.
    /// 
    /// *Description:
    /// The unit roundoff is defined as the smallest positive machine
    /// number u such that  1.0 + u .ne. 1.0.  This is computed by DUMACH
    /// in a machine-independent manner.
    /// 
    /// ***REFERENCES  (NONE)
    /// ***ROUTINES CALLED  DUMSUM
    /// ***REVISION HISTORY  (YYYYMMDD)
    /// 19930216  DATE WRITTEN
    /// 19930818  Added SLATEC-format prologue.  (FNF)
    /// 20030707  Added DUMSUM to force normal storage of COMP.  (ACH)
    /// ***END PROLOGUE  DUMACH
    /// 
    /// ***FIRST EXECUTABLE STATEMENT  DUMACH
    ///</summary>
    public class DUMACH
    {
    
        #region Dependencies
        
        DUMSUM _dumsum; 
        #endregion
        public DUMACH(DUMSUM dumsum)
        {
    
            #region Set Dependencies
            
            this._dumsum = dumsum; 
            #endregion
        }
    
        public DUMACH()
        {
    
            #region Dependencies (Initialization)
            
            DUMSUM dumsum = new DUMSUM();
            #endregion
            #region Set Dependencies
            
            this._dumsum = dumsum; 
            #endregion
        }
        /// <summary>
        /// ***PURPOSE  Compute the unit roundoff of the machine.
        /// ***CATEGORY  R1
        /// ***TYPE      DOUBLE PRECISION (RUMACH-S, DUMACH-D)
        /// ***KEYWORDS  MACHINE CONSTANTS
        /// ***AUTHOR  Hindmarsh, Alan C., (LLNL)
        /// ***DESCRIPTION
        /// *Usage:
        /// DOUBLE PRECISION  A, DUMACH
        /// A = DUMACH()
        /// 
        /// *Function Return Values:
        /// A : the unit roundoff of the machine.
        /// 
        /// *Description:
        /// The unit roundoff is defined as the smallest positive machine
        /// number u such that  1.0 + u .ne. 1.0.  This is computed by DUMACH
        /// in a machine-independent manner.
        /// 
        /// ***REFERENCES  (NONE)
        /// ***ROUTINES CALLED  DUMSUM
        /// ***REVISION HISTORY  (YYYYMMDD)
        /// 19930216  DATE WRITTEN
        /// 19930818  Added SLATEC-format prologue.  (FNF)
        /// 20030707  Added DUMSUM to force normal storage of COMP.  (ACH)
        /// ***END PROLOGUE  DUMACH
        /// 
        /// ***FIRST EXECUTABLE STATEMENT  DUMACH
        ///</summary>
        public double Run()
        {
        double dumach = 0;
            #region Variables
            
            double U = 0; double COMP = 0; 
            #endregion
            #region Prolog
            
            // C***BEGIN PROLOGUE  DUMACH
            // C***PURPOSE  Compute the unit roundoff of the machine.
            // C***CATEGORY  R1
            // C***TYPE      DOUBLE PRECISION (RUMACH-S, DUMACH-D)
            // C***KEYWORDS  MACHINE CONSTANTS
            // C***AUTHOR  Hindmarsh, Alan C., (LLNL)
            // C***DESCRIPTION
            // C *Usage:
            // C        DOUBLE PRECISION  A, DUMACH
            // C        A = DUMACH()
            // C
            // C *Function Return Values:
            // C     A : the unit roundoff of the machine.
            // C
            // C *Description:
            // C     The unit roundoff is defined as the smallest positive machine
            // C     number u such that  1.0 + u .ne. 1.0.  This is computed by DUMACH
            // C     in a machine-independent manner.
            // C
            // C***REFERENCES  (NONE)
            // C***ROUTINES CALLED  DUMSUM
            // C***REVISION HISTORY  (YYYYMMDD)
            // C   19930216  DATE WRITTEN
            // C   19930818  Added SLATEC-format prologue.  (FNF)
            // C   20030707  Added DUMSUM to force normal storage of COMP.  (ACH)
            // C***END PROLOGUE  DUMACH
            // C
            // C***FIRST EXECUTABLE STATEMENT  DUMACH
            #endregion
            U = 1.0E0;
        LABEL10:  U *= 0.5E0;
            this._dumsum.Run(1.0E0, U, ref COMP);
            if (COMP != 1.0E0) goto LABEL10;
            dumach = U * 2.0E0;
            return dumach;
            // C----------------------- End of Function DUMACH ------------------------
        }
    }

    #endregion


    #region The Class: DUMSUM
    
    public class DUMSUM
    {
    
        public DUMSUM()
        {
    
        }
    
        public void Run(double A, double B, ref double C)
        {
            // C     Routine to force normal storing of A + B, for DUMACH.
            C = A + B;
            return;
        }
    }

    #endregion


    #region The Class: DGEFA
    
    // *DECK DGEFA
    /// <summary>
    /// ***PURPOSE  Factor a matrix using Gaussian elimination.
    /// ***CATEGORY  D2A1
    /// ***TYPE      DOUBLE PRECISION (SGEFA-S, DGEFA-D, CGEFA-C)
    /// ***KEYWORDS  GENERAL MATRIX, LINEAR ALGEBRA, LINPACK,
    /// MATRIX FACTORIZATION
    /// ***AUTHOR  Moler, C. B., (U. of New Mexico)
    /// ***DESCRIPTION
    /// 
    /// DGEFA factors a double precision matrix by Gaussian elimination.
    /// 
    /// DGEFA is usually called by DGECO, but it can be called
    /// directly with a saving in time if  RCOND  is not needed.
    /// (Time for DGECO) = (1 + 9/N)*(Time for DGEFA) .
    /// 
    /// On Entry
    /// 
    /// A       DOUBLE PRECISION(LDA, N)
    /// the matrix to be factored.
    /// 
    /// LDA     INTEGER
    /// the leading dimension of the array  A .
    /// 
    /// N       INTEGER
    /// the order of the matrix  A .
    /// 
    /// On Return
    /// 
    /// A       an upper triangular matrix and the multipliers
    /// which were used to obtain it.
    /// The factorization can be written  A = L*U  where
    /// L  is a product of permutation and unit lower
    /// triangular matrices and  U  is upper triangular.
    /// 
    /// IPVT    INTEGER(N)
    /// an integer vector of pivot indices.
    /// 
    /// INFO    INTEGER
    /// = 0  normal value.
    /// = K  if  U(K,K) .EQ. 0.0 .  This is not an error
    /// condition for this subroutine, but it does
    /// indicate that DGESL or DGEDI will divide by zero
    /// if called.  Use  RCOND  in DGECO for a reliable
    /// indication of singularity.
    /// 
    /// ***REFERENCES  J. J. Dongarra, J. R. Bunch, C. B. Moler, and G. W.
    /// Stewart, LINPACK Users' Guide, SIAM, 1979.
    /// ***ROUTINES CALLED  DAXPY, DSCAL, IDAMAX
    /// ***REVISION HISTORY  (YYMMDD)
    /// 780814  DATE WRITTEN
    /// 890831  Modified array declarations.  (WRB)
    /// 890831  REVISION DATE from Version 3.2
    /// 891214  Prologue converted to Version 4.0 format.  (BAB)
    /// 900326  Removed duplicate information from DESCRIPTION section.
    /// (WRB)
    /// 920501  Reformatted the REFERENCES section.  (WRB)
    /// ***END PROLOGUE  DGEFA
    ///</summary>
    public class DGEFA
    {
    
        #region Dependencies
        
        IDAMAX _idamax; DSCAL _dscal; DAXPY _daxpy; 
        #endregion
        public DGEFA(IDAMAX idamax, DSCAL dscal, DAXPY daxpy)
        {
    
            #region Set Dependencies
            
            this._idamax = idamax; this._dscal = dscal; this._daxpy = daxpy; 
            #endregion
        }
    
        public DGEFA()
        {
    
            #region Dependencies (Initialization)
            
            IDAMAX idamax = new IDAMAX();
            DSCAL dscal = new DSCAL();
            DAXPY daxpy = new DAXPY();
            #endregion
            #region Set Dependencies
            
            this._idamax = idamax; this._dscal = dscal; this._daxpy = daxpy; 
            #endregion
        }
        /// <summary>
        /// ***PURPOSE  Factor a matrix using Gaussian elimination.
        /// ***CATEGORY  D2A1
        /// ***TYPE      DOUBLE PRECISION (SGEFA-S, DGEFA-D, CGEFA-C)
        /// ***KEYWORDS  GENERAL MATRIX, LINEAR ALGEBRA, LINPACK,
        /// MATRIX FACTORIZATION
        /// ***AUTHOR  Moler, C. B., (U. of New Mexico)
        /// ***DESCRIPTION
        /// 
        /// DGEFA factors a double precision matrix by Gaussian elimination.
        /// 
        /// DGEFA is usually called by DGECO, but it can be called
        /// directly with a saving in time if  RCOND  is not needed.
        /// (Time for DGECO) = (1 + 9/N)*(Time for DGEFA) .
        /// 
        /// On Entry
        /// 
        /// A       DOUBLE PRECISION(LDA, N)
        /// the matrix to be factored.
        /// 
        /// LDA     INTEGER
        /// the leading dimension of the array  A .
        /// 
        /// N       INTEGER
        /// the order of the matrix  A .
        /// 
        /// On Return
        /// 
        /// A       an upper triangular matrix and the multipliers
        /// which were used to obtain it.
        /// The factorization can be written  A = L*U  where
        /// L  is a product of permutation and unit lower
        /// triangular matrices and  U  is upper triangular.
        /// 
        /// IPVT    INTEGER(N)
        /// an integer vector of pivot indices.
        /// 
        /// INFO    INTEGER
        /// = 0  normal value.
        /// = K  if  U(K,K) .EQ. 0.0 .  This is not an error
        /// condition for this subroutine, but it does
        /// indicate that DGESL or DGEDI will divide by zero
        /// if called.  Use  RCOND  in DGECO for a reliable
        /// indication of singularity.
        /// 
        /// ***REFERENCES  J. J. Dongarra, J. R. Bunch, C. B. Moler, and G. W.
        /// Stewart, LINPACK Users' Guide, SIAM, 1979.
        /// ***ROUTINES CALLED  DAXPY, DSCAL, IDAMAX
        /// ***REVISION HISTORY  (YYMMDD)
        /// 780814  DATE WRITTEN
        /// 890831  Modified array declarations.  (WRB)
        /// 890831  REVISION DATE from Version 3.2
        /// 891214  Prologue converted to Version 4.0 format.  (BAB)
        /// 900326  Removed duplicate information from DESCRIPTION section.
        /// (WRB)
        /// 920501  Reformatted the REFERENCES section.  (WRB)
        /// ***END PROLOGUE  DGEFA
        ///</summary>
        /// <param name="A">
        /// DOUBLE PRECISION(LDA, N)
        /// the matrix to be factored.
        ///</param>
        /// <param name="LDA">
        /// INTEGER
        /// the leading dimension of the array  A .
        ///</param>
        /// <param name="N">
        /// INTEGER
        /// the order of the matrix  A .
        ///</param>
        /// <param name="IPVT">
        /// INTEGER(N)
        /// an integer vector of pivot indices.
        ///</param>
        /// <param name="INFO">
        /// INTEGER
        /// = 0  normal value.
        /// = K  if  U(K,K) .EQ. 0.0 .  This is not an error
        /// condition for this subroutine, but it does
        /// indicate that DGESL or DGEDI will divide by zero
        /// if called.  Use  RCOND  in DGECO for a reliable
        /// indication of singularity.
        ///</param>
        public void Run(ref double[] A, int offset_a, int LDA, int N, ref int[] IPVT, int offset_ipvt, ref int INFO)
        {
            #region Variables
            
            double T = 0; int J = 0; int K = 0; int KP1 = 0; int L = 0; int NM1 = 0; 
            #endregion
            #region Array Index Correction
            
             int o_a = -1 - LDA + offset_a;  int o_ipvt = -1 + offset_ipvt; 
            #endregion
            #region Prolog
            
            // C***BEGIN PROLOGUE  DGEFA
            // C***PURPOSE  Factor a matrix using Gaussian elimination.
            // C***CATEGORY  D2A1
            // C***TYPE      DOUBLE PRECISION (SGEFA-S, DGEFA-D, CGEFA-C)
            // C***KEYWORDS  GENERAL MATRIX, LINEAR ALGEBRA, LINPACK,
            // C             MATRIX FACTORIZATION
            // C***AUTHOR  Moler, C. B., (U. of New Mexico)
            // C***DESCRIPTION
            // C
            // C     DGEFA factors a double precision matrix by Gaussian elimination.
            // C
            // C     DGEFA is usually called by DGECO, but it can be called
            // C     directly with a saving in time if  RCOND  is not needed.
            // C     (Time for DGECO) = (1 + 9/N)*(Time for DGEFA) .
            // C
            // C     On Entry
            // C
            // C        A       DOUBLE PRECISION(LDA, N)
            // C                the matrix to be factored.
            // C
            // C        LDA     INTEGER
            // C                the leading dimension of the array  A .
            // C
            // C        N       INTEGER
            // C                the order of the matrix  A .
            // C
            // C     On Return
            // C
            // C        A       an upper triangular matrix and the multipliers
            // C                which were used to obtain it.
            // C                The factorization can be written  A = L*U  where
            // C                L  is a product of permutation and unit lower
            // C                triangular matrices and  U  is upper triangular.
            // C
            // C        IPVT    INTEGER(N)
            // C                an integer vector of pivot indices.
            // C
            // C        INFO    INTEGER
            // C                = 0  normal value.
            // C                = K  if  U(K,K) .EQ. 0.0 .  This is not an error
            // C                     condition for this subroutine, but it does
            // C                     indicate that DGESL or DGEDI will divide by zero
            // C                     if called.  Use  RCOND  in DGECO for a reliable
            // C                     indication of singularity.
            // C
            // C***REFERENCES  J. J. Dongarra, J. R. Bunch, C. B. Moler, and G. W.
            // C                 Stewart, LINPACK Users' Guide, SIAM, 1979.
            // C***ROUTINES CALLED  DAXPY, DSCAL, IDAMAX
            // C***REVISION HISTORY  (YYMMDD)
            // C   780814  DATE WRITTEN
            // C   890831  Modified array declarations.  (WRB)
            // C   890831  REVISION DATE from Version 3.2
            // C   891214  Prologue converted to Version 4.0 format.  (BAB)
            // C   900326  Removed duplicate information from DESCRIPTION section.
            // C           (WRB)
            // C   920501  Reformatted the REFERENCES section.  (WRB)
            // C***END PROLOGUE  DGEFA
            // C
            // C
            // C     GAUSSIAN ELIMINATION WITH PARTIAL PIVOTING
            // C
            // C***FIRST EXECUTABLE STATEMENT  DGEFA
            #endregion
            #region Body
            
            INFO = 0;
            NM1 = N - 1;
            if (NM1 < 1) goto LABEL70;
            for (K = 1; K <= NM1; K++)
            {
                KP1 = K + 1;
                // C
                // C        FIND L = PIVOT INDEX
                // C
                L = this._idamax.Run(N - K + 1, A, K+K * LDA + o_a, 1) + K - 1;
                IPVT[K + o_ipvt] = L;
                // C
                // C        ZERO PIVOT IMPLIES THIS COLUMN ALREADY TRIANGULARIZED
                // C
                if (A[L+K * LDA + o_a] == 0.0E0) goto LABEL40;
                // C
                // C           INTERCHANGE IF NECESSARY
                // C
                if (L == K) goto LABEL10;
                T = A[L+K * LDA + o_a];
                A[L+K * LDA + o_a] = A[K+K * LDA + o_a];
                A[K+K * LDA + o_a] = T;
            LABEL10:;
                // C
                // C           COMPUTE MULTIPLIERS
                // C
                T =  - 1.0E0 / A[K+K * LDA + o_a];
                this._dscal.Run(N - K, T, ref A, K + 1+K * LDA + o_a, 1);
                // C
                // C           ROW ELIMINATION WITH COLUMN INDEXING
                // C
                for (J = KP1; J <= N; J++)
                {
                    T = A[L+J * LDA + o_a];
                    if (L == K) goto LABEL20;
                    A[L+J * LDA + o_a] = A[K+J * LDA + o_a];
                    A[K+J * LDA + o_a] = T;
                LABEL20:;
                    this._daxpy.Run(N - K, T, A, K + 1+K * LDA + o_a, 1, ref A, K + 1+J * LDA + o_a, 1);
                }
                goto LABEL50;
            LABEL40:;
                INFO = K;
            LABEL50:;
            }
        LABEL70:;
            IPVT[N + o_ipvt] = N;
            if (A[N+N * LDA + o_a] == 0.0E0) INFO = N;
            return;
            #endregion
        }
    }

    #endregion


    #region The Class: DGESL
    
    // *DECK DGESL
    /// <summary>
    /// ***PURPOSE  Solve the real system A*X=B or TRANS(A)*X=B using the
    /// factors computed by DGECO or DGEFA.
    /// ***CATEGORY  D2A1
    /// ***TYPE      DOUBLE PRECISION (SGESL-S, DGESL-D, CGESL-C)
    /// ***KEYWORDS  LINEAR ALGEBRA, LINPACK, MATRIX, SOLVE
    /// ***AUTHOR  Moler, C. B., (U. of New Mexico)
    /// ***DESCRIPTION
    /// 
    /// DGESL solves the double precision system
    /// A * X = B  or  TRANS(A) * X = B
    /// using the factors computed by DGECO or DGEFA.
    /// 
    /// On Entry
    /// 
    /// A       DOUBLE PRECISION(LDA, N)
    /// the output from DGECO or DGEFA.
    /// 
    /// LDA     INTEGER
    /// the leading dimension of the array  A .
    /// 
    /// N       INTEGER
    /// the order of the matrix  A .
    /// 
    /// IPVT    INTEGER(N)
    /// the pivot vector from DGECO or DGEFA.
    /// 
    /// B       DOUBLE PRECISION(N)
    /// the right hand side vector.
    /// 
    /// JOB     INTEGER
    /// = 0         to solve  A*X = B ,
    /// = nonzero   to solve  TRANS(A)*X = B  where
    /// TRANS(A)  is the transpose.
    /// 
    /// On Return
    /// 
    /// B       the solution vector  X .
    /// 
    /// Error Condition
    /// 
    /// A division by zero will occur if the input factor contains a
    /// zero on the diagonal.  Technically this indicates singularity
    /// but it is often caused by improper arguments or improper
    /// setting of LDA .  It will not occur if the subroutines are
    /// called correctly and if DGECO has set RCOND .GT. 0.0
    /// or DGEFA has set INFO .EQ. 0 .
    /// 
    /// To compute  INVERSE(A) * C  where  C  is a matrix
    /// with  P  columns
    /// CALL DGECO(A,LDA,N,IPVT,RCOND,Z)
    /// IF (RCOND is too small) GO TO ...
    /// DO 10 J = 1, P
    /// CALL DGESL(A,LDA,N,IPVT,C(1,J),0)
    /// 10 CONTINUE
    /// 
    /// ***REFERENCES  J. J. Dongarra, J. R. Bunch, C. B. Moler, and G. W.
    /// Stewart, LINPACK Users' Guide, SIAM, 1979.
    /// ***ROUTINES CALLED  DAXPY, DDOT
    /// ***REVISION HISTORY  (YYMMDD)
    /// 780814  DATE WRITTEN
    /// 890831  Modified array declarations.  (WRB)
    /// 890831  REVISION DATE from Version 3.2
    /// 891214  Prologue converted to Version 4.0 format.  (BAB)
    /// 900326  Removed duplicate information from DESCRIPTION section.
    /// (WRB)
    /// 920501  Reformatted the REFERENCES section.  (WRB)
    /// ***END PROLOGUE  DGESL
    /// 
    /// ***FIRST EXECUTABLE STATEMENT  DGESL
    ///</summary>
    public class DGESL
    {
    
        #region Dependencies
        
        DDOT _ddot; DAXPY _daxpy; 
        #endregion
        public DGESL(DDOT ddot, DAXPY daxpy)
        {
    
            #region Set Dependencies
            
            this._ddot = ddot; this._daxpy = daxpy; 
            #endregion
        }
    
        public DGESL()
        {
    
            #region Dependencies (Initialization)
            
            DDOT ddot = new DDOT();
            DAXPY daxpy = new DAXPY();
            #endregion
            #region Set Dependencies
            
            this._ddot = ddot; this._daxpy = daxpy; 
            #endregion
        }
        /// <summary>
        /// ***PURPOSE  Solve the real system A*X=B or TRANS(A)*X=B using the
        /// factors computed by DGECO or DGEFA.
        /// ***CATEGORY  D2A1
        /// ***TYPE      DOUBLE PRECISION (SGESL-S, DGESL-D, CGESL-C)
        /// ***KEYWORDS  LINEAR ALGEBRA, LINPACK, MATRIX, SOLVE
        /// ***AUTHOR  Moler, C. B., (U. of New Mexico)
        /// ***DESCRIPTION
        /// 
        /// DGESL solves the double precision system
        /// A * X = B  or  TRANS(A) * X = B
        /// using the factors computed by DGECO or DGEFA.
        /// 
        /// On Entry
        /// 
        /// A       DOUBLE PRECISION(LDA, N)
        /// the output from DGECO or DGEFA.
        /// 
        /// LDA     INTEGER
        /// the leading dimension of the array  A .
        /// 
        /// N       INTEGER
        /// the order of the matrix  A .
        /// 
        /// IPVT    INTEGER(N)
        /// the pivot vector from DGECO or DGEFA.
        /// 
        /// B       DOUBLE PRECISION(N)
        /// the right hand side vector.
        /// 
        /// JOB     INTEGER
        /// = 0         to solve  A*X = B ,
        /// = nonzero   to solve  TRANS(A)*X = B  where
        /// TRANS(A)  is the transpose.
        /// 
        /// On Return
        /// 
        /// B       the solution vector  X .
        /// 
        /// Error Condition
        /// 
        /// A division by zero will occur if the input factor contains a
        /// zero on the diagonal.  Technically this indicates singularity
        /// but it is often caused by improper arguments or improper
        /// setting of LDA .  It will not occur if the subroutines are
        /// called correctly and if DGECO has set RCOND .GT. 0.0
        /// or DGEFA has set INFO .EQ. 0 .
        /// 
        /// To compute  INVERSE(A) * C  where  C  is a matrix
        /// with  P  columns
        /// CALL DGECO(A,LDA,N,IPVT,RCOND,Z)
        /// IF (RCOND is too small) GO TO ...
        /// DO 10 J = 1, P
        /// CALL DGESL(A,LDA,N,IPVT,C(1,J),0)
        /// 10 CONTINUE
        /// 
        /// ***REFERENCES  J. J. Dongarra, J. R. Bunch, C. B. Moler, and G. W.
        /// Stewart, LINPACK Users' Guide, SIAM, 1979.
        /// ***ROUTINES CALLED  DAXPY, DDOT
        /// ***REVISION HISTORY  (YYMMDD)
        /// 780814  DATE WRITTEN
        /// 890831  Modified array declarations.  (WRB)
        /// 890831  REVISION DATE from Version 3.2
        /// 891214  Prologue converted to Version 4.0 format.  (BAB)
        /// 900326  Removed duplicate information from DESCRIPTION section.
        /// (WRB)
        /// 920501  Reformatted the REFERENCES section.  (WRB)
        /// ***END PROLOGUE  DGESL
        /// 
        /// ***FIRST EXECUTABLE STATEMENT  DGESL
        ///</summary>
        /// <param name="A">
        /// DOUBLE PRECISION(LDA, N)
        /// the output from DGECO or DGEFA.
        ///</param>
        /// <param name="LDA">
        /// INTEGER
        /// the leading dimension of the array  A .
        ///</param>
        /// <param name="N">
        /// INTEGER
        /// the order of the matrix  A .
        ///</param>
        /// <param name="IPVT">
        /// INTEGER(N)
        /// the pivot vector from DGECO or DGEFA.
        ///</param>
        /// <param name="B">
        /// DOUBLE PRECISION(N)
        /// the right hand side vector.
        ///</param>
        /// <param name="JOB">
        /// INTEGER
        /// = 0         to solve  A*X = B ,
        /// = nonzero   to solve  TRANS(A)*X = B  where
        /// TRANS(A)  is the transpose.
        ///</param>
        public void Run(double[] A, int offset_a, int LDA, int N, int[] IPVT, int offset_ipvt, ref double[] B, int offset_b, int JOB)
        {
            #region Variables
            
            double T = 0; int K = 0; int KB = 0; int L = 0; int NM1 = 0; 
            #endregion
            #region Array Index Correction
            
             int o_a = -1 - LDA + offset_a;  int o_ipvt = -1 + offset_ipvt;  int o_b = -1 + offset_b; 
            #endregion
            #region Prolog
            
            // C***BEGIN PROLOGUE  DGESL
            // C***PURPOSE  Solve the real system A*X=B or TRANS(A)*X=B using the
            // C            factors computed by DGECO or DGEFA.
            // C***CATEGORY  D2A1
            // C***TYPE      DOUBLE PRECISION (SGESL-S, DGESL-D, CGESL-C)
            // C***KEYWORDS  LINEAR ALGEBRA, LINPACK, MATRIX, SOLVE
            // C***AUTHOR  Moler, C. B., (U. of New Mexico)
            // C***DESCRIPTION
            // C
            // C     DGESL solves the double precision system
            // C     A * X = B  or  TRANS(A) * X = B
            // C     using the factors computed by DGECO or DGEFA.
            // C
            // C     On Entry
            // C
            // C        A       DOUBLE PRECISION(LDA, N)
            // C                the output from DGECO or DGEFA.
            // C
            // C        LDA     INTEGER
            // C                the leading dimension of the array  A .
            // C
            // C        N       INTEGER
            // C                the order of the matrix  A .
            // C
            // C        IPVT    INTEGER(N)
            // C                the pivot vector from DGECO or DGEFA.
            // C
            // C        B       DOUBLE PRECISION(N)
            // C                the right hand side vector.
            // C
            // C        JOB     INTEGER
            // C                = 0         to solve  A*X = B ,
            // C                = nonzero   to solve  TRANS(A)*X = B  where
            // C                            TRANS(A)  is the transpose.
            // C
            // C     On Return
            // C
            // C        B       the solution vector  X .
            // C
            // C     Error Condition
            // C
            // C        A division by zero will occur if the input factor contains a
            // C        zero on the diagonal.  Technically this indicates singularity
            // C        but it is often caused by improper arguments or improper
            // C        setting of LDA .  It will not occur if the subroutines are
            // C        called correctly and if DGECO has set RCOND .GT. 0.0
            // C        or DGEFA has set INFO .EQ. 0 .
            // C
            // C     To compute  INVERSE(A) * C  where  C  is a matrix
            // C     with  P  columns
            // C           CALL DGECO(A,LDA,N,IPVT,RCOND,Z)
            // C           IF (RCOND is too small) GO TO ...
            // C           DO 10 J = 1, P
            // C              CALL DGESL(A,LDA,N,IPVT,C(1,J),0)
            // C        10 CONTINUE
            // C
            // C***REFERENCES  J. J. Dongarra, J. R. Bunch, C. B. Moler, and G. W.
            // C                 Stewart, LINPACK Users' Guide, SIAM, 1979.
            // C***ROUTINES CALLED  DAXPY, DDOT
            // C***REVISION HISTORY  (YYMMDD)
            // C   780814  DATE WRITTEN
            // C   890831  Modified array declarations.  (WRB)
            // C   890831  REVISION DATE from Version 3.2
            // C   891214  Prologue converted to Version 4.0 format.  (BAB)
            // C   900326  Removed duplicate information from DESCRIPTION section.
            // C           (WRB)
            // C   920501  Reformatted the REFERENCES section.  (WRB)
            // C***END PROLOGUE  DGESL
            // C
            // C***FIRST EXECUTABLE STATEMENT  DGESL
            #endregion
            #region Body
            
            NM1 = N - 1;
            if (JOB != 0) goto LABEL50;
            // C
            // C        JOB = 0 , SOLVE  A * X = B
            // C        FIRST SOLVE  L*Y = B
            // C
            if (NM1 < 1) goto LABEL30;
            for (K = 1; K <= NM1; K++)
            {
                L = IPVT[K + o_ipvt];
                T = B[L + o_b];
                if (L == K) goto LABEL10;
                B[L + o_b] = B[K + o_b];
                B[K + o_b] = T;
            LABEL10:;
                this._daxpy.Run(N - K, T, A, K + 1+K * LDA + o_a, 1, ref B, K + 1 + o_b, 1);
            }
        LABEL30:;
            // C
            // C        NOW SOLVE  U*X = Y
            // C
            for (KB = 1; KB <= N; KB++)
            {
                K = N + 1 - KB;
                B[K + o_b] /= A[K+K * LDA + o_a];
                T =  - B[K + o_b];
                this._daxpy.Run(K - 1, T, A, 1+K * LDA + o_a, 1, ref B, 1 + o_b, 1);
            }
            goto LABEL100;
        LABEL50:;
            // C
            // C        JOB = NONZERO, SOLVE  TRANS(A) * X = B
            // C        FIRST SOLVE  TRANS(U)*Y = B
            // C
            for (K = 1; K <= N; K++)
            {
                T = this._ddot.Run(K - 1, A, 1+K * LDA + o_a, 1, B, 1 + o_b, 1);
                B[K + o_b] = (B[K + o_b] - T) / A[K+K * LDA + o_a];
            }
            // C
            // C        NOW SOLVE TRANS(L)*X = Y
            // C
            if (NM1 < 1) goto LABEL90;
            for (KB = 1; KB <= NM1; KB++)
            {
                K = N - KB;
                B[K + o_b] += this._ddot.Run(N - K, A, K + 1+K * LDA + o_a, 1, B, K + 1 + o_b, 1);
                L = IPVT[K + o_ipvt];
                if (L == K) goto LABEL70;
                T = B[L + o_b];
                B[L + o_b] = B[K + o_b];
                B[K + o_b] = T;
            LABEL70:;
            }
        LABEL90:;
        LABEL100:;
            return;
            #endregion
        }
    }

    #endregion


    #region The Class: DGBFA
    
    // *DECK DGBFA
    /// <summary>
    /// ***PURPOSE  Factor a band matrix using Gaussian elimination.
    /// ***CATEGORY  D2A2
    /// ***TYPE      DOUBLE PRECISION (SGBFA-S, DGBFA-D, CGBFA-C)
    /// ***KEYWORDS  BANDED, LINEAR ALGEBRA, LINPACK, MATRIX FACTORIZATION
    /// ***AUTHOR  Moler, C. B., (U. of New Mexico)
    /// ***DESCRIPTION
    /// 
    /// DGBFA factors a double precision band matrix by elimination.
    /// 
    /// DGBFA is usually called by DGBCO, but it can be called
    /// directly with a saving in time if  RCOND  is not needed.
    /// 
    /// On Entry
    /// 
    /// ABD     DOUBLE PRECISION(LDA, N)
    /// contains the matrix in band storage.  The columns
    /// of the matrix are stored in the columns of  ABD  and
    /// the diagonals of the matrix are stored in rows
    /// ML+1 through 2*ML+MU+1 of  ABD .
    /// See the comments below for details.
    /// 
    /// LDA     INTEGER
    /// the leading dimension of the array  ABD .
    /// LDA must be .GE. 2*ML + MU + 1 .
    /// 
    /// N       INTEGER
    /// the order of the original matrix.
    /// 
    /// ML      INTEGER
    /// number of diagonals below the main diagonal.
    /// 0 .LE. ML .LT.  N .
    /// 
    /// MU      INTEGER
    /// number of diagonals above the main diagonal.
    /// 0 .LE. MU .LT.  N .
    /// More efficient if  ML .LE. MU .
    /// On Return
    /// 
    /// ABD     an upper triangular matrix in band storage and
    /// the multipliers which were used to obtain it.
    /// The factorization can be written  A = L*U  where
    /// L  is a product of permutation and unit lower
    /// triangular matrices and  U  is upper triangular.
    /// 
    /// IPVT    INTEGER(N)
    /// an integer vector of pivot indices.
    /// 
    /// INFO    INTEGER
    /// = 0  normal value.
    /// = K  if  U(K,K) .EQ. 0.0 .  This is not an error
    /// condition for this subroutine, but it does
    /// indicate that DGBSL will divide by zero if
    /// called.  Use  RCOND  in DGBCO for a reliable
    /// indication of singularity.
    /// 
    /// Band Storage
    /// 
    /// If  A  is a band matrix, the following program segment
    /// will set up the input.
    /// 
    /// ML = (band width below the diagonal)
    /// MU = (band width above the diagonal)
    /// M = ML + MU + 1
    /// DO 20 J = 1, N
    /// I1 = MAX(1, J-MU)
    /// I2 = MIN(N, J+ML)
    /// DO 10 I = I1, I2
    /// K = I - J + M
    /// ABD(K,J) = A(I,J)
    /// 10    CONTINUE
    /// 20 CONTINUE
    /// 
    /// This uses rows  ML+1  through  2*ML+MU+1  of  ABD .
    /// In addition, the first  ML  rows in  ABD  are used for
    /// elements generated during the triangularization.
    /// The total number of rows needed in  ABD  is  2*ML+MU+1 .
    /// The  ML+MU by ML+MU  upper left triangle and the
    /// ML by ML  lower right triangle are not referenced.
    /// 
    /// ***REFERENCES  J. J. Dongarra, J. R. Bunch, C. B. Moler, and G. W.
    /// Stewart, LINPACK Users' Guide, SIAM, 1979.
    /// ***ROUTINES CALLED  DAXPY, DSCAL, IDAMAX
    /// ***REVISION HISTORY  (YYMMDD)
    /// 780814  DATE WRITTEN
    /// 890531  Changed all specific intrinsics to generic.  (WRB)
    /// 890831  Modified array declarations.  (WRB)
    /// 890831  REVISION DATE from Version 3.2
    /// 891214  Prologue converted to Version 4.0 format.  (BAB)
    /// 900326  Removed duplicate information from DESCRIPTION section.
    /// (WRB)
    /// 920501  Reformatted the REFERENCES section.  (WRB)
    /// ***END PROLOGUE  DGBFA
    ///</summary>
    public class DGBFA
    {
    
        #region Dependencies
        
        IDAMAX _idamax; DSCAL _dscal; DAXPY _daxpy; 
        #endregion
        public DGBFA(IDAMAX idamax, DSCAL dscal, DAXPY daxpy)
        {
    
            #region Set Dependencies
            
            this._idamax = idamax; this._dscal = dscal; this._daxpy = daxpy; 
            #endregion
        }
    
        public DGBFA()
        {
    
            #region Dependencies (Initialization)
            
            IDAMAX idamax = new IDAMAX();
            DSCAL dscal = new DSCAL();
            DAXPY daxpy = new DAXPY();
            #endregion
            #region Set Dependencies
            
            this._idamax = idamax; this._dscal = dscal; this._daxpy = daxpy; 
            #endregion
        }
        /// <summary>
        /// ***PURPOSE  Factor a band matrix using Gaussian elimination.
        /// ***CATEGORY  D2A2
        /// ***TYPE      DOUBLE PRECISION (SGBFA-S, DGBFA-D, CGBFA-C)
        /// ***KEYWORDS  BANDED, LINEAR ALGEBRA, LINPACK, MATRIX FACTORIZATION
        /// ***AUTHOR  Moler, C. B., (U. of New Mexico)
        /// ***DESCRIPTION
        /// 
        /// DGBFA factors a double precision band matrix by elimination.
        /// 
        /// DGBFA is usually called by DGBCO, but it can be called
        /// directly with a saving in time if  RCOND  is not needed.
        /// 
        /// On Entry
        /// 
        /// ABD     DOUBLE PRECISION(LDA, N)
        /// contains the matrix in band storage.  The columns
        /// of the matrix are stored in the columns of  ABD  and
        /// the diagonals of the matrix are stored in rows
        /// ML+1 through 2*ML+MU+1 of  ABD .
        /// See the comments below for details.
        /// 
        /// LDA     INTEGER
        /// the leading dimension of the array  ABD .
        /// LDA must be .GE. 2*ML + MU + 1 .
        /// 
        /// N       INTEGER
        /// the order of the original matrix.
        /// 
        /// ML      INTEGER
        /// number of diagonals below the main diagonal.
        /// 0 .LE. ML .LT.  N .
        /// 
        /// MU      INTEGER
        /// number of diagonals above the main diagonal.
        /// 0 .LE. MU .LT.  N .
        /// More efficient if  ML .LE. MU .
        /// On Return
        /// 
        /// ABD     an upper triangular matrix in band storage and
        /// the multipliers which were used to obtain it.
        /// The factorization can be written  A = L*U  where
        /// L  is a product of permutation and unit lower
        /// triangular matrices and  U  is upper triangular.
        /// 
        /// IPVT    INTEGER(N)
        /// an integer vector of pivot indices.
        /// 
        /// INFO    INTEGER
        /// = 0  normal value.
        /// = K  if  U(K,K) .EQ. 0.0 .  This is not an error
        /// condition for this subroutine, but it does
        /// indicate that DGBSL will divide by zero if
        /// called.  Use  RCOND  in DGBCO for a reliable
        /// indication of singularity.
        /// 
        /// Band Storage
        /// 
        /// If  A  is a band matrix, the following program segment
        /// will set up the input.
        /// 
        /// ML = (band width below the diagonal)
        /// MU = (band width above the diagonal)
        /// M = ML + MU + 1
        /// DO 20 J = 1, N
        /// I1 = MAX(1, J-MU)
        /// I2 = MIN(N, J+ML)
        /// DO 10 I = I1, I2
        /// K = I - J + M
        /// ABD(K,J) = A(I,J)
        /// 10    CONTINUE
        /// 20 CONTINUE
        /// 
        /// This uses rows  ML+1  through  2*ML+MU+1  of  ABD .
        /// In addition, the first  ML  rows in  ABD  are used for
        /// elements generated during the triangularization.
        /// The total number of rows needed in  ABD  is  2*ML+MU+1 .
        /// The  ML+MU by ML+MU  upper left triangle and the
        /// ML by ML  lower right triangle are not referenced.
        /// 
        /// ***REFERENCES  J. J. Dongarra, J. R. Bunch, C. B. Moler, and G. W.
        /// Stewart, LINPACK Users' Guide, SIAM, 1979.
        /// ***ROUTINES CALLED  DAXPY, DSCAL, IDAMAX
        /// ***REVISION HISTORY  (YYMMDD)
        /// 780814  DATE WRITTEN
        /// 890531  Changed all specific intrinsics to generic.  (WRB)
        /// 890831  Modified array declarations.  (WRB)
        /// 890831  REVISION DATE from Version 3.2
        /// 891214  Prologue converted to Version 4.0 format.  (BAB)
        /// 900326  Removed duplicate information from DESCRIPTION section.
        /// (WRB)
        /// 920501  Reformatted the REFERENCES section.  (WRB)
        /// ***END PROLOGUE  DGBFA
        ///</summary>
        /// <param name="ABD">
        /// DOUBLE PRECISION(LDA, N)
        /// contains the matrix in band storage.  The columns
        /// of the matrix are stored in the columns of  ABD  and
        /// the diagonals of the matrix are stored in rows
        /// ML+1 through 2*ML+MU+1 of  ABD .
        /// See the comments below for details.
        ///</param>
        /// <param name="LDA">
        /// INTEGER
        /// the leading dimension of the array  ABD .
        /// LDA must be .GE. 2*ML + MU + 1 .
        ///</param>
        /// <param name="N">
        /// INTEGER
        /// the order of the original matrix.
        ///</param>
        /// <param name="ML">
        /// INTEGER
        /// number of diagonals below the main diagonal.
        /// 0 .LE. ML .LT.  N .
        ///</param>
        /// <param name="MU">
        /// INTEGER
        /// number of diagonals above the main diagonal.
        /// 0 .LE. MU .LT.  N .
        /// More efficient if  ML .LE. MU .
        ///</param>
        /// <param name="IPVT">
        /// INTEGER(N)
        /// an integer vector of pivot indices.
        ///</param>
        /// <param name="INFO">
        /// INTEGER
        /// = 0  normal value.
        /// = K  if  U(K,K) .EQ. 0.0 .  This is not an error
        /// condition for this subroutine, but it does
        /// indicate that DGBSL will divide by zero if
        /// called.  Use  RCOND  in DGBCO for a reliable
        /// indication of singularity.
        ///</param>
        public void Run(ref double[] ABD, int offset_abd, int LDA, int N, int ML, int MU, ref int[] IPVT, int offset_ipvt
                         , ref int INFO)
        {
            #region Variables
            
            double T = 0; int I = 0; int I0 = 0; int J = 0; int JU = 0; int JZ = 0; int J0 = 0; int J1 = 0; int K = 0; 
            int KP1 = 0;int L = 0; int LM = 0; int M = 0; int MM = 0; int NM1 = 0; 
            #endregion
            #region Implicit Variables
            
            int ABD_JZ = 0; 
            #endregion
            #region Array Index Correction
            
             int o_abd = -1 - LDA + offset_abd;  int o_ipvt = -1 + offset_ipvt; 
            #endregion
            #region Prolog
            
            // C***BEGIN PROLOGUE  DGBFA
            // C***PURPOSE  Factor a band matrix using Gaussian elimination.
            // C***CATEGORY  D2A2
            // C***TYPE      DOUBLE PRECISION (SGBFA-S, DGBFA-D, CGBFA-C)
            // C***KEYWORDS  BANDED, LINEAR ALGEBRA, LINPACK, MATRIX FACTORIZATION
            // C***AUTHOR  Moler, C. B., (U. of New Mexico)
            // C***DESCRIPTION
            // C
            // C     DGBFA factors a double precision band matrix by elimination.
            // C
            // C     DGBFA is usually called by DGBCO, but it can be called
            // C     directly with a saving in time if  RCOND  is not needed.
            // C
            // C     On Entry
            // C
            // C        ABD     DOUBLE PRECISION(LDA, N)
            // C                contains the matrix in band storage.  The columns
            // C                of the matrix are stored in the columns of  ABD  and
            // C                the diagonals of the matrix are stored in rows
            // C                ML+1 through 2*ML+MU+1 of  ABD .
            // C                See the comments below for details.
            // C
            // C        LDA     INTEGER
            // C                the leading dimension of the array  ABD .
            // C                LDA must be .GE. 2*ML + MU + 1 .
            // C
            // C        N       INTEGER
            // C                the order of the original matrix.
            // C
            // C        ML      INTEGER
            // C                number of diagonals below the main diagonal.
            // C                0 .LE. ML .LT.  N .
            // C
            // C        MU      INTEGER
            // C                number of diagonals above the main diagonal.
            // C                0 .LE. MU .LT.  N .
            // C                More efficient if  ML .LE. MU .
            // C     On Return
            // C
            // C        ABD     an upper triangular matrix in band storage and
            // C                the multipliers which were used to obtain it.
            // C                The factorization can be written  A = L*U  where
            // C                L  is a product of permutation and unit lower
            // C                triangular matrices and  U  is upper triangular.
            // C
            // C        IPVT    INTEGER(N)
            // C                an integer vector of pivot indices.
            // C
            // C        INFO    INTEGER
            // C                = 0  normal value.
            // C                = K  if  U(K,K) .EQ. 0.0 .  This is not an error
            // C                     condition for this subroutine, but it does
            // C                     indicate that DGBSL will divide by zero if
            // C                     called.  Use  RCOND  in DGBCO for a reliable
            // C                     indication of singularity.
            // C
            // C     Band Storage
            // C
            // C           If  A  is a band matrix, the following program segment
            // C           will set up the input.
            // C
            // C                   ML = (band width below the diagonal)
            // C                   MU = (band width above the diagonal)
            // C                   M = ML + MU + 1
            // C                   DO 20 J = 1, N
            // C                      I1 = MAX(1, J-MU)
            // C                      I2 = MIN(N, J+ML)
            // C                      DO 10 I = I1, I2
            // C                         K = I - J + M
            // C                         ABD(K,J) = A(I,J)
            // C                10    CONTINUE
            // C                20 CONTINUE
            // C
            // C           This uses rows  ML+1  through  2*ML+MU+1  of  ABD .
            // C           In addition, the first  ML  rows in  ABD  are used for
            // C           elements generated during the triangularization.
            // C           The total number of rows needed in  ABD  is  2*ML+MU+1 .
            // C           The  ML+MU by ML+MU  upper left triangle and the
            // C           ML by ML  lower right triangle are not referenced.
            // C
            // C***REFERENCES  J. J. Dongarra, J. R. Bunch, C. B. Moler, and G. W.
            // C                 Stewart, LINPACK Users' Guide, SIAM, 1979.
            // C***ROUTINES CALLED  DAXPY, DSCAL, IDAMAX
            // C***REVISION HISTORY  (YYMMDD)
            // C   780814  DATE WRITTEN
            // C   890531  Changed all specific intrinsics to generic.  (WRB)
            // C   890831  Modified array declarations.  (WRB)
            // C   890831  REVISION DATE from Version 3.2
            // C   891214  Prologue converted to Version 4.0 format.  (BAB)
            // C   900326  Removed duplicate information from DESCRIPTION section.
            // C           (WRB)
            // C   920501  Reformatted the REFERENCES section.  (WRB)
            // C***END PROLOGUE  DGBFA
            // C
            // C
            // C***FIRST EXECUTABLE STATEMENT  DGBFA
            #endregion
            #region Body
            
            M = ML + MU + 1;
            INFO = 0;
            // C
            // C     ZERO INITIAL FILL-IN COLUMNS
            // C
            J0 = MU + 2;
            J1 = Math.Min(N, M) - 1;
            if (J1 < J0) goto LABEL30;
            for (JZ = J0; JZ <= J1; JZ++)
            {
                I0 = M + 1 - JZ;
                ABD_JZ = JZ * LDA + o_abd;
                for (I = I0; I <= ML; I++)
                {
                    ABD[I + ABD_JZ] = 0.0E0;
                }
            }
        LABEL30:;
            JZ = J1;
            JU = 0;
            // C
            // C     GAUSSIAN ELIMINATION WITH PARTIAL PIVOTING
            // C
            NM1 = N - 1;
            if (NM1 < 1) goto LABEL130;
            for (K = 1; K <= NM1; K++)
            {
                KP1 = K + 1;
                // C
                // C        ZERO NEXT FILL-IN COLUMN
                // C
                JZ += 1;
                if (JZ > N) goto LABEL50;
                if (ML < 1) goto LABEL50;
                ABD_JZ = JZ * LDA + o_abd;
                for (I = 1; I <= ML; I++)
                {
                    ABD[I + ABD_JZ] = 0.0E0;
                }
            LABEL50:;
                // C
                // C        FIND L = PIVOT INDEX
                // C
                LM = Math.Min(ML, N - K);
                L = this._idamax.Run(LM + 1, ABD, M+K * LDA + o_abd, 1) + M - 1;
                IPVT[K + o_ipvt] = L + K - M;
                // C
                // C        ZERO PIVOT IMPLIES THIS COLUMN ALREADY TRIANGULARIZED
                // C
                if (ABD[L+K * LDA + o_abd] == 0.0E0) goto LABEL100;
                // C
                // C           INTERCHANGE IF NECESSARY
                // C
                if (L == M) goto LABEL60;
                T = ABD[L+K * LDA + o_abd];
                ABD[L+K * LDA + o_abd] = ABD[M+K * LDA + o_abd];
                ABD[M+K * LDA + o_abd] = T;
            LABEL60:;
                // C
                // C           COMPUTE MULTIPLIERS
                // C
                T =  - 1.0E0 / ABD[M+K * LDA + o_abd];
                this._dscal.Run(LM, T, ref ABD, M + 1+K * LDA + o_abd, 1);
                // C
                // C           ROW ELIMINATION WITH COLUMN INDEXING
                // C
                JU = Math.Min(Math.Max(JU, MU + IPVT[K + o_ipvt]), N);
                MM = M;
                if (JU < KP1) goto LABEL90;
                for (J = KP1; J <= JU; J++)
                {
                    L -= 1;
                    MM -= 1;
                    T = ABD[L+J * LDA + o_abd];
                    if (L == MM) goto LABEL70;
                    ABD[L+J * LDA + o_abd] = ABD[MM+J * LDA + o_abd];
                    ABD[MM+J * LDA + o_abd] = T;
                LABEL70:;
                    this._daxpy.Run(LM, T, ABD, M + 1+K * LDA + o_abd, 1, ref ABD, MM + 1+J * LDA + o_abd, 1);
                }
            LABEL90:;
                goto LABEL110;
            LABEL100:;
                INFO = K;
            LABEL110:;
            }
        LABEL130:;
            IPVT[N + o_ipvt] = N;
            if (ABD[M+N * LDA + o_abd] == 0.0E0) INFO = N;
            return;
            #endregion
        }
    }

    #endregion


    #region The Class: DGBSL
    
    // *DECK DGBSL
    /// <summary>
    /// ***PURPOSE  Solve the real band system A*X=B or TRANS(A)*X=B using
    /// the factors computed by DGBCO or DGBFA.
    /// ***CATEGORY  D2A2
    /// ***TYPE      DOUBLE PRECISION (SGBSL-S, DGBSL-D, CGBSL-C)
    /// ***KEYWORDS  BANDED, LINEAR ALGEBRA, LINPACK, MATRIX, SOLVE
    /// ***AUTHOR  Moler, C. B., (U. of New Mexico)
    /// ***DESCRIPTION
    /// 
    /// DGBSL solves the double precision band system
    /// A * X = B  or  TRANS(A) * X = B
    /// using the factors computed by DGBCO or DGBFA.
    /// 
    /// On Entry
    /// 
    /// ABD     DOUBLE PRECISION(LDA, N)
    /// the output from DGBCO or DGBFA.
    /// 
    /// LDA     INTEGER
    /// the leading dimension of the array  ABD .
    /// 
    /// N       INTEGER
    /// the order of the original matrix.
    /// 
    /// ML      INTEGER
    /// number of diagonals below the main diagonal.
    /// 
    /// MU      INTEGER
    /// number of diagonals above the main diagonal.
    /// 
    /// IPVT    INTEGER(N)
    /// the pivot vector from DGBCO or DGBFA.
    /// 
    /// B       DOUBLE PRECISION(N)
    /// the right hand side vector.
    /// 
    /// JOB     INTEGER
    /// = 0         to solve  A*X = B ,
    /// = nonzero   to solve  TRANS(A)*X = B , where
    /// TRANS(A)  is the transpose.
    /// 
    /// On Return
    /// 
    /// B       the solution vector  X .
    /// 
    /// Error Condition
    /// 
    /// A division by zero will occur if the input factor contains a
    /// zero on the diagonal.  Technically this indicates singularity
    /// but it is often caused by improper arguments or improper
    /// setting of LDA .  It will not occur if the subroutines are
    /// called correctly and if DGBCO has set RCOND .GT. 0.0
    /// or DGBFA has set INFO .EQ. 0 .
    /// 
    /// To compute  INVERSE(A) * C  where  C  is a matrix
    /// with  P  columns
    /// CALL DGBCO(ABD,LDA,N,ML,MU,IPVT,RCOND,Z)
    /// IF (RCOND is too small) GO TO ...
    /// DO 10 J = 1, P
    /// CALL DGBSL(ABD,LDA,N,ML,MU,IPVT,C(1,J),0)
    /// 10 CONTINUE
    /// 
    /// ***REFERENCES  J. J. Dongarra, J. R. Bunch, C. B. Moler, and G. W.
    /// Stewart, LINPACK Users' Guide, SIAM, 1979.
    /// ***ROUTINES CALLED  DAXPY, DDOT
    /// ***REVISION HISTORY  (YYMMDD)
    /// 780814  DATE WRITTEN
    /// 890531  Changed all specific intrinsics to generic.  (WRB)
    /// 890831  Modified array declarations.  (WRB)
    /// 890831  REVISION DATE from Version 3.2
    /// 891214  Prologue converted to Version 4.0 format.  (BAB)
    /// 900326  Removed duplicate information from DESCRIPTION section.
    /// (WRB)
    /// 920501  Reformatted the REFERENCES section.  (WRB)
    /// ***END PROLOGUE  DGBSL
    /// 
    /// ***FIRST EXECUTABLE STATEMENT  DGBSL
    ///</summary>
    public class DGBSL
    {
    
        #region Dependencies
        
        DDOT _ddot; DAXPY _daxpy; 
        #endregion
        public DGBSL(DDOT ddot, DAXPY daxpy)
        {
    
            #region Set Dependencies
            
            this._ddot = ddot; this._daxpy = daxpy; 
            #endregion
        }
    
        public DGBSL()
        {
    
            #region Dependencies (Initialization)
            
            DDOT ddot = new DDOT();
            DAXPY daxpy = new DAXPY();
            #endregion
            #region Set Dependencies
            
            this._ddot = ddot; this._daxpy = daxpy; 
            #endregion
        }
        /// <summary>
        /// ***PURPOSE  Solve the real band system A*X=B or TRANS(A)*X=B using
        /// the factors computed by DGBCO or DGBFA.
        /// ***CATEGORY  D2A2
        /// ***TYPE      DOUBLE PRECISION (SGBSL-S, DGBSL-D, CGBSL-C)
        /// ***KEYWORDS  BANDED, LINEAR ALGEBRA, LINPACK, MATRIX, SOLVE
        /// ***AUTHOR  Moler, C. B., (U. of New Mexico)
        /// ***DESCRIPTION
        /// 
        /// DGBSL solves the double precision band system
        /// A * X = B  or  TRANS(A) * X = B
        /// using the factors computed by DGBCO or DGBFA.
        /// 
        /// On Entry
        /// 
        /// ABD     DOUBLE PRECISION(LDA, N)
        /// the output from DGBCO or DGBFA.
        /// 
        /// LDA     INTEGER
        /// the leading dimension of the array  ABD .
        /// 
        /// N       INTEGER
        /// the order of the original matrix.
        /// 
        /// ML      INTEGER
        /// number of diagonals below the main diagonal.
        /// 
        /// MU      INTEGER
        /// number of diagonals above the main diagonal.
        /// 
        /// IPVT    INTEGER(N)
        /// the pivot vector from DGBCO or DGBFA.
        /// 
        /// B       DOUBLE PRECISION(N)
        /// the right hand side vector.
        /// 
        /// JOB     INTEGER
        /// = 0         to solve  A*X = B ,
        /// = nonzero   to solve  TRANS(A)*X = B , where
        /// TRANS(A)  is the transpose.
        /// 
        /// On Return
        /// 
        /// B       the solution vector  X .
        /// 
        /// Error Condition
        /// 
        /// A division by zero will occur if the input factor contains a
        /// zero on the diagonal.  Technically this indicates singularity
        /// but it is often caused by improper arguments or improper
        /// setting of LDA .  It will not occur if the subroutines are
        /// called correctly and if DGBCO has set RCOND .GT. 0.0
        /// or DGBFA has set INFO .EQ. 0 .
        /// 
        /// To compute  INVERSE(A) * C  where  C  is a matrix
        /// with  P  columns
        /// CALL DGBCO(ABD,LDA,N,ML,MU,IPVT,RCOND,Z)
        /// IF (RCOND is too small) GO TO ...
        /// DO 10 J = 1, P
        /// CALL DGBSL(ABD,LDA,N,ML,MU,IPVT,C(1,J),0)
        /// 10 CONTINUE
        /// 
        /// ***REFERENCES  J. J. Dongarra, J. R. Bunch, C. B. Moler, and G. W.
        /// Stewart, LINPACK Users' Guide, SIAM, 1979.
        /// ***ROUTINES CALLED  DAXPY, DDOT
        /// ***REVISION HISTORY  (YYMMDD)
        /// 780814  DATE WRITTEN
        /// 890531  Changed all specific intrinsics to generic.  (WRB)
        /// 890831  Modified array declarations.  (WRB)
        /// 890831  REVISION DATE from Version 3.2
        /// 891214  Prologue converted to Version 4.0 format.  (BAB)
        /// 900326  Removed duplicate information from DESCRIPTION section.
        /// (WRB)
        /// 920501  Reformatted the REFERENCES section.  (WRB)
        /// ***END PROLOGUE  DGBSL
        /// 
        /// ***FIRST EXECUTABLE STATEMENT  DGBSL
        ///</summary>
        /// <param name="ABD">
        /// DOUBLE PRECISION(LDA, N)
        /// the output from DGBCO or DGBFA.
        ///</param>
        /// <param name="LDA">
        /// INTEGER
        /// the leading dimension of the array  ABD .
        ///</param>
        /// <param name="N">
        /// INTEGER
        /// the order of the original matrix.
        ///</param>
        /// <param name="ML">
        /// INTEGER
        /// number of diagonals below the main diagonal.
        ///</param>
        /// <param name="MU">
        /// INTEGER
        /// number of diagonals above the main diagonal.
        ///</param>
        /// <param name="IPVT">
        /// INTEGER(N)
        /// the pivot vector from DGBCO or DGBFA.
        ///</param>
        /// <param name="B">
        /// DOUBLE PRECISION(N)
        /// the right hand side vector.
        ///</param>
        /// <param name="JOB">
        /// INTEGER
        /// = 0         to solve  A*X = B ,
        /// = nonzero   to solve  TRANS(A)*X = B , where
        /// TRANS(A)  is the transpose.
        ///</param>
        public void Run(double[] ABD, int offset_abd, int LDA, int N, int ML, int MU, int[] IPVT, int offset_ipvt
                         , ref double[] B, int offset_b, int JOB)
        {
            #region Variables
            
            double T = 0; int K = 0; int KB = 0; int L = 0; int LA = 0; int LB = 0; int LM = 0; int M = 0; int NM1 = 0; 
            #endregion
            #region Array Index Correction
            
             int o_abd = -1 - LDA + offset_abd;  int o_ipvt = -1 + offset_ipvt;  int o_b = -1 + offset_b; 
            #endregion
            #region Prolog
            
            // C***BEGIN PROLOGUE  DGBSL
            // C***PURPOSE  Solve the real band system A*X=B or TRANS(A)*X=B using
            // C            the factors computed by DGBCO or DGBFA.
            // C***CATEGORY  D2A2
            // C***TYPE      DOUBLE PRECISION (SGBSL-S, DGBSL-D, CGBSL-C)
            // C***KEYWORDS  BANDED, LINEAR ALGEBRA, LINPACK, MATRIX, SOLVE
            // C***AUTHOR  Moler, C. B., (U. of New Mexico)
            // C***DESCRIPTION
            // C
            // C     DGBSL solves the double precision band system
            // C     A * X = B  or  TRANS(A) * X = B
            // C     using the factors computed by DGBCO or DGBFA.
            // C
            // C     On Entry
            // C
            // C        ABD     DOUBLE PRECISION(LDA, N)
            // C                the output from DGBCO or DGBFA.
            // C
            // C        LDA     INTEGER
            // C                the leading dimension of the array  ABD .
            // C
            // C        N       INTEGER
            // C                the order of the original matrix.
            // C
            // C        ML      INTEGER
            // C                number of diagonals below the main diagonal.
            // C
            // C        MU      INTEGER
            // C                number of diagonals above the main diagonal.
            // C
            // C        IPVT    INTEGER(N)
            // C                the pivot vector from DGBCO or DGBFA.
            // C
            // C        B       DOUBLE PRECISION(N)
            // C                the right hand side vector.
            // C
            // C        JOB     INTEGER
            // C                = 0         to solve  A*X = B ,
            // C                = nonzero   to solve  TRANS(A)*X = B , where
            // C                            TRANS(A)  is the transpose.
            // C
            // C     On Return
            // C
            // C        B       the solution vector  X .
            // C
            // C     Error Condition
            // C
            // C        A division by zero will occur if the input factor contains a
            // C        zero on the diagonal.  Technically this indicates singularity
            // C        but it is often caused by improper arguments or improper
            // C        setting of LDA .  It will not occur if the subroutines are
            // C        called correctly and if DGBCO has set RCOND .GT. 0.0
            // C        or DGBFA has set INFO .EQ. 0 .
            // C
            // C     To compute  INVERSE(A) * C  where  C  is a matrix
            // C     with  P  columns
            // C           CALL DGBCO(ABD,LDA,N,ML,MU,IPVT,RCOND,Z)
            // C           IF (RCOND is too small) GO TO ...
            // C           DO 10 J = 1, P
            // C              CALL DGBSL(ABD,LDA,N,ML,MU,IPVT,C(1,J),0)
            // C        10 CONTINUE
            // C
            // C***REFERENCES  J. J. Dongarra, J. R. Bunch, C. B. Moler, and G. W.
            // C                 Stewart, LINPACK Users' Guide, SIAM, 1979.
            // C***ROUTINES CALLED  DAXPY, DDOT
            // C***REVISION HISTORY  (YYMMDD)
            // C   780814  DATE WRITTEN
            // C   890531  Changed all specific intrinsics to generic.  (WRB)
            // C   890831  Modified array declarations.  (WRB)
            // C   890831  REVISION DATE from Version 3.2
            // C   891214  Prologue converted to Version 4.0 format.  (BAB)
            // C   900326  Removed duplicate information from DESCRIPTION section.
            // C           (WRB)
            // C   920501  Reformatted the REFERENCES section.  (WRB)
            // C***END PROLOGUE  DGBSL
            // C
            // C***FIRST EXECUTABLE STATEMENT  DGBSL
            #endregion
            #region Body
            
            M = MU + ML + 1;
            NM1 = N - 1;
            if (JOB != 0) goto LABEL50;
            // C
            // C        JOB = 0 , SOLVE  A * X = B
            // C        FIRST SOLVE L*Y = B
            // C
            if (ML == 0) goto LABEL30;
            if (NM1 < 1) goto LABEL30;
            for (K = 1; K <= NM1; K++)
            {
                LM = Math.Min(ML, N - K);
                L = IPVT[K + o_ipvt];
                T = B[L + o_b];
                if (L == K) goto LABEL10;
                B[L + o_b] = B[K + o_b];
                B[K + o_b] = T;
            LABEL10:;
                this._daxpy.Run(LM, T, ABD, M + 1+K * LDA + o_abd, 1, ref B, K + 1 + o_b, 1);
            }
        LABEL30:;
            // C
            // C        NOW SOLVE  U*X = Y
            // C
            for (KB = 1; KB <= N; KB++)
            {
                K = N + 1 - KB;
                B[K + o_b] /= ABD[M+K * LDA + o_abd];
                LM = Math.Min(K, M) - 1;
                LA = M - LM;
                LB = K - LM;
                T =  - B[K + o_b];
                this._daxpy.Run(LM, T, ABD, LA+K * LDA + o_abd, 1, ref B, LB + o_b, 1);
            }
            goto LABEL100;
        LABEL50:;
            // C
            // C        JOB = NONZERO, SOLVE  TRANS(A) * X = B
            // C        FIRST SOLVE  TRANS(U)*Y = B
            // C
            for (K = 1; K <= N; K++)
            {
                LM = Math.Min(K, M) - 1;
                LA = M - LM;
                LB = K - LM;
                T = this._ddot.Run(LM, ABD, LA+K * LDA + o_abd, 1, B, LB + o_b, 1);
                B[K + o_b] = (B[K + o_b] - T) / ABD[M+K * LDA + o_abd];
            }
            // C
            // C        NOW SOLVE TRANS(L)*X = Y
            // C
            if (ML == 0) goto LABEL90;
            if (NM1 < 1) goto LABEL90;
            for (KB = 1; KB <= NM1; KB++)
            {
                K = N - KB;
                LM = Math.Min(ML, N - K);
                B[K + o_b] += this._ddot.Run(LM, ABD, M + 1+K * LDA + o_abd, 1, B, K + 1 + o_b, 1);
                L = IPVT[K + o_ipvt];
                if (L == K) goto LABEL70;
                T = B[L + o_b];
                B[L + o_b] = B[K + o_b];
                B[K + o_b] = T;
            LABEL70:;
            }
        LABEL90:;
        LABEL100:;
            return;
            #endregion
        }
    }

    #endregion


    #region The Class: DAXPY
    
    // *DECK DAXPY
    /// <summary>
    /// ***PURPOSE  Compute a constant times a vector plus a vector.
    /// ***CATEGORY  D1A7
    /// ***TYPE      DOUBLE PRECISION (SAXPY-S, DAXPY-D, CAXPY-C)
    /// ***KEYWORDS  BLAS, LINEAR ALGEBRA, TRIAD, VECTOR
    /// ***AUTHOR  Lawson, C. L., (JPL)
    /// Hanson, R. J., (SNLA)
    /// Kincaid, D. R., (U. of Texas)
    /// Krogh, F. T., (JPL)
    /// ***DESCRIPTION
    /// 
    /// B L A S  Subprogram
    /// Description of Parameters
    /// 
    /// --Input--
    /// N  number of elements in input vector(s)
    /// DA  double precision scalar multiplier
    /// DX  double precision vector with N elements
    /// INCX  storage spacing between elements of DX
    /// DY  double precision vector with N elements
    /// INCY  storage spacing between elements of DY
    /// 
    /// --Output--
    /// DY  double precision result (unchanged if N .LE. 0)
    /// 
    /// Overwrite double precision DY with double precision DA*DX + DY.
    /// For I = 0 to N-1, replace  DY(LY+I*INCY) with DA*DX(LX+I*INCX) +
    /// DY(LY+I*INCY),
    /// where LX = 1 if INCX .GE. 0, else LX = 1+(1-N)*INCX, and LY is
    /// defined in a similar way using INCY.
    /// 
    /// ***REFERENCES  C. L. Lawson, R. J. Hanson, D. R. Kincaid and F. T.
    /// Krogh, Basic linear algebra subprograms for Fortran
    /// usage, Algorithm No. 539, Transactions on Mathematical
    /// Software 5, 3 (September 1979), pp. 308-323.
    /// ***ROUTINES CALLED  (NONE)
    /// ***REVISION HISTORY  (YYMMDD)
    /// 791001  DATE WRITTEN
    /// 890831  Modified array declarations.  (WRB)
    /// 890831  REVISION DATE from Version 3.2
    /// 891214  Prologue converted to Version 4.0 format.  (BAB)
    /// 920310  Corrected definition of LX in DESCRIPTION.  (WRB)
    /// 920501  Reformatted the REFERENCES section.  (WRB)
    /// ***END PROLOGUE  DAXPY
    /// ***FIRST EXECUTABLE STATEMENT  DAXPY
    ///</summary>
    public class DAXPY
    {
    
        public DAXPY()
        {
    
        }
    
        /// <summary>
        /// ***PURPOSE  Compute a constant times a vector plus a vector.
        /// ***CATEGORY  D1A7
        /// ***TYPE      DOUBLE PRECISION (SAXPY-S, DAXPY-D, CAXPY-C)
        /// ***KEYWORDS  BLAS, LINEAR ALGEBRA, TRIAD, VECTOR
        /// ***AUTHOR  Lawson, C. L., (JPL)
        /// Hanson, R. J., (SNLA)
        /// Kincaid, D. R., (U. of Texas)
        /// Krogh, F. T., (JPL)
        /// ***DESCRIPTION
        /// 
        /// B L A S  Subprogram
        /// Description of Parameters
        /// 
        /// --Input--
        /// N  number of elements in input vector(s)
        /// DA  double precision scalar multiplier
        /// DX  double precision vector with N elements
        /// INCX  storage spacing between elements of DX
        /// DY  double precision vector with N elements
        /// INCY  storage spacing between elements of DY
        /// 
        /// --Output--
        /// DY  double precision result (unchanged if N .LE. 0)
        /// 
        /// Overwrite double precision DY with double precision DA*DX + DY.
        /// For I = 0 to N-1, replace  DY(LY+I*INCY) with DA*DX(LX+I*INCX) +
        /// DY(LY+I*INCY),
        /// where LX = 1 if INCX .GE. 0, else LX = 1+(1-N)*INCX, and LY is
        /// defined in a similar way using INCY.
        /// 
        /// ***REFERENCES  C. L. Lawson, R. J. Hanson, D. R. Kincaid and F. T.
        /// Krogh, Basic linear algebra subprograms for Fortran
        /// usage, Algorithm No. 539, Transactions on Mathematical
        /// Software 5, 3 (September 1979), pp. 308-323.
        /// ***ROUTINES CALLED  (NONE)
        /// ***REVISION HISTORY  (YYMMDD)
        /// 791001  DATE WRITTEN
        /// 890831  Modified array declarations.  (WRB)
        /// 890831  REVISION DATE from Version 3.2
        /// 891214  Prologue converted to Version 4.0 format.  (BAB)
        /// 920310  Corrected definition of LX in DESCRIPTION.  (WRB)
        /// 920501  Reformatted the REFERENCES section.  (WRB)
        /// ***END PROLOGUE  DAXPY
        /// ***FIRST EXECUTABLE STATEMENT  DAXPY
        ///</summary>
        /// <param name="N">
        /// number of elements in input vector(s)
        ///</param>
        /// <param name="DA">
        /// double precision scalar multiplier
        ///</param>
        /// <param name="DX">
        /// double precision vector with N elements
        ///</param>
        /// <param name="INCX">
        /// storage spacing between elements of DX
        /// DY  double precision vector with N elements
        ///</param>
        /// <param name="DY">
        /// double precision vector with N elements
        ///</param>
        /// <param name="INCY">
        /// storage spacing between elements of DY
        ///</param>
        public void Run(int N, double DA, double[] DX, int offset_dx, int INCX, ref double[] DY, int offset_dy, int INCY)
        {
            #region Implicit Variables
            
            int IX = 0; int IY = 0; int M = 0; int I = 0; int MP1 = 0; int NS = 0; 
            #endregion
            #region Array Index Correction
            
             int o_dx = -1 + offset_dx;  int o_dy = -1 + offset_dy; 
            #endregion
            #region Prolog
            
            // C***BEGIN PROLOGUE  DAXPY
            // C***PURPOSE  Compute a constant times a vector plus a vector.
            // C***CATEGORY  D1A7
            // C***TYPE      DOUBLE PRECISION (SAXPY-S, DAXPY-D, CAXPY-C)
            // C***KEYWORDS  BLAS, LINEAR ALGEBRA, TRIAD, VECTOR
            // C***AUTHOR  Lawson, C. L., (JPL)
            // C           Hanson, R. J., (SNLA)
            // C           Kincaid, D. R., (U. of Texas)
            // C           Krogh, F. T., (JPL)
            // C***DESCRIPTION
            // C
            // C                B L A S  Subprogram
            // C    Description of Parameters
            // C
            // C     --Input--
            // C        N  number of elements in input vector(s)
            // C       DA  double precision scalar multiplier
            // C       DX  double precision vector with N elements
            // C     INCX  storage spacing between elements of DX
            // C       DY  double precision vector with N elements
            // C     INCY  storage spacing between elements of DY
            // C
            // C     --Output--
            // C       DY  double precision result (unchanged if N .LE. 0)
            // C
            // C     Overwrite double precision DY with double precision DA*DX + DY.
            // C     For I = 0 to N-1, replace  DY(LY+I*INCY) with DA*DX(LX+I*INCX) +
            // C       DY(LY+I*INCY),
            // C     where LX = 1 if INCX .GE. 0, else LX = 1+(1-N)*INCX, and LY is
            // C     defined in a similar way using INCY.
            // C
            // C***REFERENCES  C. L. Lawson, R. J. Hanson, D. R. Kincaid and F. T.
            // C                 Krogh, Basic linear algebra subprograms for Fortran
            // C                 usage, Algorithm No. 539, Transactions on Mathematical
            // C                 Software 5, 3 (September 1979), pp. 308-323.
            // C***ROUTINES CALLED  (NONE)
            // C***REVISION HISTORY  (YYMMDD)
            // C   791001  DATE WRITTEN
            // C   890831  Modified array declarations.  (WRB)
            // C   890831  REVISION DATE from Version 3.2
            // C   891214  Prologue converted to Version 4.0 format.  (BAB)
            // C   920310  Corrected definition of LX in DESCRIPTION.  (WRB)
            // C   920501  Reformatted the REFERENCES section.  (WRB)
            // C***END PROLOGUE  DAXPY
            // C***FIRST EXECUTABLE STATEMENT  DAXPY
            #endregion
            #region Body
            
            if (N <= 0 || DA == 0.0E0) return;
            if (INCX == INCY)
            {
                if (INCX - 1 < 0) goto LABEL5;
                else
                {
                    if (INCX - 1 > 0) goto LABEL60;
                    else goto LABEL20;
                }
            }
            // C
            // C     Code for unequal or nonpositive increments.
            // C
        LABEL5:  IX = 1;
            IY = 1;
            if (INCX < 0) IX = ( - N + 1) * INCX + 1;
            if (INCY < 0) IY = ( - N + 1) * INCY + 1;
            for (I = 1; I <= N; I++)
            {
                DY[IY + o_dy] += DA * DX[IX + o_dx];
                IX += INCX;
                IY += INCY;
            }
            return;
            // C
            // C     Code for both increments equal to 1.
            // C
            // C     Clean-up loop so remaining vector length is a multiple of 4.
            // C
        LABEL20:  M = FortranLib.Mod(N,4);
            if (M == 0) goto LABEL40;
            for (I = 1; I <= M; I++)
            {
                DY[I + o_dy] += DA * DX[I + o_dx];
            }
            if (N < 4) return;
        LABEL40:  MP1 = M + 1;
            for (I = MP1; I <= N; I += 4)
            {
                DY[I + o_dy] += DA * DX[I + o_dx];
                DY[I + 1 + o_dy] += DA * DX[I + 1 + o_dx];
                DY[I + 2 + o_dy] += DA * DX[I + 2 + o_dx];
                DY[I + 3 + o_dy] += DA * DX[I + 3 + o_dx];
            }
            return;
            // C
            // C     Code for equal, positive, non-unit increments.
            // C
        LABEL60:  NS = N * INCX;
            for (I = 1; (INCX >= 0) ? (I <= NS) : (I >= NS); I += INCX)
            {
                DY[I + o_dy] = DA * DX[I + o_dx] + DY[I + o_dy];
            }
            return;
            #endregion
        }
    }

    #endregion


    #region The Class: DCOPY
    
    // *DECK DCOPY
    /// <summary>
    /// ***PURPOSE  Copy a vector.
    /// ***CATEGORY  D1A5
    /// ***TYPE      DOUBLE PRECISION (SCOPY-S, DCOPY-D, CCOPY-C, ICOPY-I)
    /// ***KEYWORDS  BLAS, COPY, LINEAR ALGEBRA, VECTOR
    /// ***AUTHOR  Lawson, C. L., (JPL)
    /// Hanson, R. J., (SNLA)
    /// Kincaid, D. R., (U. of Texas)
    /// Krogh, F. T., (JPL)
    /// ***DESCRIPTION
    /// 
    /// B L A S  Subprogram
    /// Description of Parameters
    /// 
    /// --Input--
    /// N  number of elements in input vector(s)
    /// DX  double precision vector with N elements
    /// INCX  storage spacing between elements of DX
    /// DY  double precision vector with N elements
    /// INCY  storage spacing between elements of DY
    /// 
    /// --Output--
    /// DY  copy of vector DX (unchanged if N .LE. 0)
    /// 
    /// Copy double precision DX to double precision DY.
    /// For I = 0 to N-1, copy DX(LX+I*INCX) to DY(LY+I*INCY),
    /// where LX = 1 if INCX .GE. 0, else LX = 1+(1-N)*INCX, and LY is
    /// defined in a similar way using INCY.
    /// 
    /// ***REFERENCES  C. L. Lawson, R. J. Hanson, D. R. Kincaid and F. T.
    /// Krogh, Basic linear algebra subprograms for Fortran
    /// usage, Algorithm No. 539, Transactions on Mathematical
    /// Software 5, 3 (September 1979), pp. 308-323.
    /// ***ROUTINES CALLED  (NONE)
    /// ***REVISION HISTORY  (YYMMDD)
    /// 791001  DATE WRITTEN
    /// 890831  Modified array declarations.  (WRB)
    /// 890831  REVISION DATE from Version 3.2
    /// 891214  Prologue converted to Version 4.0 format.  (BAB)
    /// 920310  Corrected definition of LX in DESCRIPTION.  (WRB)
    /// 920501  Reformatted the REFERENCES section.  (WRB)
    /// ***END PROLOGUE  DCOPY
    /// ***FIRST EXECUTABLE STATEMENT  DCOPY
    ///</summary>
    public class DCOPY
    {
    
        public DCOPY()
        {
    
        }
    
        /// <summary>
        /// ***PURPOSE  Copy a vector.
        /// ***CATEGORY  D1A5
        /// ***TYPE      DOUBLE PRECISION (SCOPY-S, DCOPY-D, CCOPY-C, ICOPY-I)
        /// ***KEYWORDS  BLAS, COPY, LINEAR ALGEBRA, VECTOR
        /// ***AUTHOR  Lawson, C. L., (JPL)
        /// Hanson, R. J., (SNLA)
        /// Kincaid, D. R., (U. of Texas)
        /// Krogh, F. T., (JPL)
        /// ***DESCRIPTION
        /// 
        /// B L A S  Subprogram
        /// Description of Parameters
        /// 
        /// --Input--
        /// N  number of elements in input vector(s)
        /// DX  double precision vector with N elements
        /// INCX  storage spacing between elements of DX
        /// DY  double precision vector with N elements
        /// INCY  storage spacing between elements of DY
        /// 
        /// --Output--
        /// DY  copy of vector DX (unchanged if N .LE. 0)
        /// 
        /// Copy double precision DX to double precision DY.
        /// For I = 0 to N-1, copy DX(LX+I*INCX) to DY(LY+I*INCY),
        /// where LX = 1 if INCX .GE. 0, else LX = 1+(1-N)*INCX, and LY is
        /// defined in a similar way using INCY.
        /// 
        /// ***REFERENCES  C. L. Lawson, R. J. Hanson, D. R. Kincaid and F. T.
        /// Krogh, Basic linear algebra subprograms for Fortran
        /// usage, Algorithm No. 539, Transactions on Mathematical
        /// Software 5, 3 (September 1979), pp. 308-323.
        /// ***ROUTINES CALLED  (NONE)
        /// ***REVISION HISTORY  (YYMMDD)
        /// 791001  DATE WRITTEN
        /// 890831  Modified array declarations.  (WRB)
        /// 890831  REVISION DATE from Version 3.2
        /// 891214  Prologue converted to Version 4.0 format.  (BAB)
        /// 920310  Corrected definition of LX in DESCRIPTION.  (WRB)
        /// 920501  Reformatted the REFERENCES section.  (WRB)
        /// ***END PROLOGUE  DCOPY
        /// ***FIRST EXECUTABLE STATEMENT  DCOPY
        ///</summary>
        /// <param name="N">
        /// number of elements in input vector(s)
        ///</param>
        /// <param name="DX">
        /// double precision vector with N elements
        ///</param>
        /// <param name="INCX">
        /// storage spacing between elements of DX
        /// DY  double precision vector with N elements
        ///</param>
        /// <param name="DY">
        /// double precision vector with N elements
        ///</param>
        /// <param name="INCY">
        /// storage spacing between elements of DY
        ///</param>
        public void Run(int N, double[] DX, int offset_dx, int INCX, ref double[] DY, int offset_dy, int INCY)
        {
            #region Implicit Variables
            
            int IX = 0; int IY = 0; int M = 0; int I = 0; int MP1 = 0; int NS = 0; 
            #endregion
            #region Array Index Correction
            
             int o_dx = -1 + offset_dx;  int o_dy = -1 + offset_dy; 
            #endregion
            #region Prolog
            
            // C***BEGIN PROLOGUE  DCOPY
            // C***PURPOSE  Copy a vector.
            // C***CATEGORY  D1A5
            // C***TYPE      DOUBLE PRECISION (SCOPY-S, DCOPY-D, CCOPY-C, ICOPY-I)
            // C***KEYWORDS  BLAS, COPY, LINEAR ALGEBRA, VECTOR
            // C***AUTHOR  Lawson, C. L., (JPL)
            // C           Hanson, R. J., (SNLA)
            // C           Kincaid, D. R., (U. of Texas)
            // C           Krogh, F. T., (JPL)
            // C***DESCRIPTION
            // C
            // C                B L A S  Subprogram
            // C    Description of Parameters
            // C
            // C     --Input--
            // C        N  number of elements in input vector(s)
            // C       DX  double precision vector with N elements
            // C     INCX  storage spacing between elements of DX
            // C       DY  double precision vector with N elements
            // C     INCY  storage spacing between elements of DY
            // C
            // C     --Output--
            // C       DY  copy of vector DX (unchanged if N .LE. 0)
            // C
            // C     Copy double precision DX to double precision DY.
            // C     For I = 0 to N-1, copy DX(LX+I*INCX) to DY(LY+I*INCY),
            // C     where LX = 1 if INCX .GE. 0, else LX = 1+(1-N)*INCX, and LY is
            // C     defined in a similar way using INCY.
            // C
            // C***REFERENCES  C. L. Lawson, R. J. Hanson, D. R. Kincaid and F. T.
            // C                 Krogh, Basic linear algebra subprograms for Fortran
            // C                 usage, Algorithm No. 539, Transactions on Mathematical
            // C                 Software 5, 3 (September 1979), pp. 308-323.
            // C***ROUTINES CALLED  (NONE)
            // C***REVISION HISTORY  (YYMMDD)
            // C   791001  DATE WRITTEN
            // C   890831  Modified array declarations.  (WRB)
            // C   890831  REVISION DATE from Version 3.2
            // C   891214  Prologue converted to Version 4.0 format.  (BAB)
            // C   920310  Corrected definition of LX in DESCRIPTION.  (WRB)
            // C   920501  Reformatted the REFERENCES section.  (WRB)
            // C***END PROLOGUE  DCOPY
            // C***FIRST EXECUTABLE STATEMENT  DCOPY
            #endregion
            #region Body
            
            if (N <= 0) return;
            if (INCX == INCY)
            {
                if (INCX - 1 < 0) goto LABEL5;
                else
                {
                    if (INCX - 1 > 0) goto LABEL60;
                    else goto LABEL20;
                }
            }
            // C
            // C     Code for unequal or nonpositive increments.
            // C
        LABEL5:  IX = 1;
            IY = 1;
            if (INCX < 0) IX = ( - N + 1) * INCX + 1;
            if (INCY < 0) IY = ( - N + 1) * INCY + 1;
            for (I = 1; I <= N; I++)
            {
                DY[IY + o_dy] = DX[IX + o_dx];
                IX += INCX;
                IY += INCY;
            }
            return;
            // C
            // C     Code for both increments equal to 1.
            // C
            // C     Clean-up loop so remaining vector length is a multiple of 7.
            // C
        LABEL20:  M = FortranLib.Mod(N,7);
            if (M == 0) goto LABEL40;
            for (I = 1; I <= M; I++)
            {
                DY[I + o_dy] = DX[I + o_dx];
            }
            if (N < 7) return;
        LABEL40:  MP1 = M + 1;
            for (I = MP1; I <= N; I += 7)
            {
                DY[I + o_dy] = DX[I + o_dx];
                DY[I + 1 + o_dy] = DX[I + 1 + o_dx];
                DY[I + 2 + o_dy] = DX[I + 2 + o_dx];
                DY[I + 3 + o_dy] = DX[I + 3 + o_dx];
                DY[I + 4 + o_dy] = DX[I + 4 + o_dx];
                DY[I + 5 + o_dy] = DX[I + 5 + o_dx];
                DY[I + 6 + o_dy] = DX[I + 6 + o_dx];
            }
            return;
            // C
            // C     Code for equal, positive, non-unit increments.
            // C
        LABEL60:  NS = N * INCX;
            for (I = 1; (INCX >= 0) ? (I <= NS) : (I >= NS); I += INCX)
            {
                DY[I + o_dy] = DX[I + o_dx];
            }
            return;
            #endregion
        }
    }

    #endregion


    #region The Class: DDOT
    
    // *DECK DDOT
    /// <summary>
    /// ***PURPOSE  Compute the inner product of two vectors.
    /// ***CATEGORY  D1A4
    /// ***TYPE      DOUBLE PRECISION (SDOT-S, DDOT-D, CDOTU-C)
    /// ***KEYWORDS  BLAS, INNER PRODUCT, LINEAR ALGEBRA, VECTOR
    /// ***AUTHOR  Lawson, C. L., (JPL)
    /// Hanson, R. J., (SNLA)
    /// Kincaid, D. R., (U. of Texas)
    /// Krogh, F. T., (JPL)
    /// ***DESCRIPTION
    /// 
    /// B L A S  Subprogram
    /// Description of Parameters
    /// 
    /// --Input--
    /// N  number of elements in input vector(s)
    /// DX  double precision vector with N elements
    /// INCX  storage spacing between elements of DX
    /// DY  double precision vector with N elements
    /// INCY  storage spacing between elements of DY
    /// 
    /// --Output--
    /// DDOT  double precision dot product (zero if N .LE. 0)
    /// 
    /// Returns the dot product of double precision DX and DY.
    /// DDOT = sum for I = 0 to N-1 of  DX(LX+I*INCX) * DY(LY+I*INCY),
    /// where LX = 1 if INCX .GE. 0, else LX = 1+(1-N)*INCX, and LY is
    /// defined in a similar way using INCY.
    /// 
    /// ***REFERENCES  C. L. Lawson, R. J. Hanson, D. R. Kincaid and F. T.
    /// Krogh, Basic linear algebra subprograms for Fortran
    /// usage, Algorithm No. 539, Transactions on Mathematical
    /// Software 5, 3 (September 1979), pp. 308-323.
    /// ***ROUTINES CALLED  (NONE)
    /// ***REVISION HISTORY  (YYMMDD)
    /// 791001  DATE WRITTEN
    /// 890831  Modified array declarations.  (WRB)
    /// 890831  REVISION DATE from Version 3.2
    /// 891214  Prologue converted to Version 4.0 format.  (BAB)
    /// 920310  Corrected definition of LX in DESCRIPTION.  (WRB)
    /// 920501  Reformatted the REFERENCES section.  (WRB)
    /// ***END PROLOGUE  DDOT
    /// ***FIRST EXECUTABLE STATEMENT  DDOT
    ///</summary>
    public class DDOT
    {
    
        public DDOT()
        {
    
        }
    
        /// <summary>
        /// ***PURPOSE  Compute the inner product of two vectors.
        /// ***CATEGORY  D1A4
        /// ***TYPE      DOUBLE PRECISION (SDOT-S, DDOT-D, CDOTU-C)
        /// ***KEYWORDS  BLAS, INNER PRODUCT, LINEAR ALGEBRA, VECTOR
        /// ***AUTHOR  Lawson, C. L., (JPL)
        /// Hanson, R. J., (SNLA)
        /// Kincaid, D. R., (U. of Texas)
        /// Krogh, F. T., (JPL)
        /// ***DESCRIPTION
        /// 
        /// B L A S  Subprogram
        /// Description of Parameters
        /// 
        /// --Input--
        /// N  number of elements in input vector(s)
        /// DX  double precision vector with N elements
        /// INCX  storage spacing between elements of DX
        /// DY  double precision vector with N elements
        /// INCY  storage spacing between elements of DY
        /// 
        /// --Output--
        /// DDOT  double precision dot product (zero if N .LE. 0)
        /// 
        /// Returns the dot product of double precision DX and DY.
        /// DDOT = sum for I = 0 to N-1 of  DX(LX+I*INCX) * DY(LY+I*INCY),
        /// where LX = 1 if INCX .GE. 0, else LX = 1+(1-N)*INCX, and LY is
        /// defined in a similar way using INCY.
        /// 
        /// ***REFERENCES  C. L. Lawson, R. J. Hanson, D. R. Kincaid and F. T.
        /// Krogh, Basic linear algebra subprograms for Fortran
        /// usage, Algorithm No. 539, Transactions on Mathematical
        /// Software 5, 3 (September 1979), pp. 308-323.
        /// ***ROUTINES CALLED  (NONE)
        /// ***REVISION HISTORY  (YYMMDD)
        /// 791001  DATE WRITTEN
        /// 890831  Modified array declarations.  (WRB)
        /// 890831  REVISION DATE from Version 3.2
        /// 891214  Prologue converted to Version 4.0 format.  (BAB)
        /// 920310  Corrected definition of LX in DESCRIPTION.  (WRB)
        /// 920501  Reformatted the REFERENCES section.  (WRB)
        /// ***END PROLOGUE  DDOT
        /// ***FIRST EXECUTABLE STATEMENT  DDOT
        ///</summary>
        /// <param name="N">
        /// number of elements in input vector(s)
        ///</param>
        /// <param name="DX">
        /// double precision vector with N elements
        ///</param>
        /// <param name="INCX">
        /// storage spacing between elements of DX
        /// DY  double precision vector with N elements
        ///</param>
        /// <param name="DY">
        /// double precision vector with N elements
        ///</param>
        /// <param name="INCY">
        /// storage spacing between elements of DY
        ///</param>
        public double Run(int N, double[] DX, int offset_dx, int INCX, double[] DY, int offset_dy, int INCY)
        {
        double ddot = 0;
            #region Implicit Variables
            
            int IX = 0; int IY = 0; int M = 0; int I = 0; int MP1 = 0; int NS = 0; 
            #endregion
            #region Array Index Correction
            
             int o_dx = -1 + offset_dx;  int o_dy = -1 + offset_dy; 
            #endregion
            #region Prolog
            
            // C***BEGIN PROLOGUE  DDOT
            // C***PURPOSE  Compute the inner product of two vectors.
            // C***CATEGORY  D1A4
            // C***TYPE      DOUBLE PRECISION (SDOT-S, DDOT-D, CDOTU-C)
            // C***KEYWORDS  BLAS, INNER PRODUCT, LINEAR ALGEBRA, VECTOR
            // C***AUTHOR  Lawson, C. L., (JPL)
            // C           Hanson, R. J., (SNLA)
            // C           Kincaid, D. R., (U. of Texas)
            // C           Krogh, F. T., (JPL)
            // C***DESCRIPTION
            // C
            // C                B L A S  Subprogram
            // C    Description of Parameters
            // C
            // C     --Input--
            // C        N  number of elements in input vector(s)
            // C       DX  double precision vector with N elements
            // C     INCX  storage spacing between elements of DX
            // C       DY  double precision vector with N elements
            // C     INCY  storage spacing between elements of DY
            // C
            // C     --Output--
            // C     DDOT  double precision dot product (zero if N .LE. 0)
            // C
            // C     Returns the dot product of double precision DX and DY.
            // C     DDOT = sum for I = 0 to N-1 of  DX(LX+I*INCX) * DY(LY+I*INCY),
            // C     where LX = 1 if INCX .GE. 0, else LX = 1+(1-N)*INCX, and LY is
            // C     defined in a similar way using INCY.
            // C
            // C***REFERENCES  C. L. Lawson, R. J. Hanson, D. R. Kincaid and F. T.
            // C                 Krogh, Basic linear algebra subprograms for Fortran
            // C                 usage, Algorithm No. 539, Transactions on Mathematical
            // C                 Software 5, 3 (September 1979), pp. 308-323.
            // C***ROUTINES CALLED  (NONE)
            // C***REVISION HISTORY  (YYMMDD)
            // C   791001  DATE WRITTEN
            // C   890831  Modified array declarations.  (WRB)
            // C   890831  REVISION DATE from Version 3.2
            // C   891214  Prologue converted to Version 4.0 format.  (BAB)
            // C   920310  Corrected definition of LX in DESCRIPTION.  (WRB)
            // C   920501  Reformatted the REFERENCES section.  (WRB)
            // C***END PROLOGUE  DDOT
            // C***FIRST EXECUTABLE STATEMENT  DDOT
            #endregion
            #region Body
            
            ddot = 0.0E0;
            if (N <= 0) return ddot;
            if (INCX == INCY)
            {
                if (INCX - 1 < 0) goto LABEL5;
                else
                {
                    if (INCX - 1 > 0) goto LABEL60;
                    else goto LABEL20;
                }
            }
            // C
            // C     Code for unequal or nonpositive increments.
            // C
        LABEL5:  IX = 1;
            IY = 1;
            if (INCX < 0) IX = ( - N + 1) * INCX + 1;
            if (INCY < 0) IY = ( - N + 1) * INCY + 1;
            for (I = 1; I <= N; I++)
            {
                ddot += DX[IX + o_dx] * DY[IY + o_dy];
                IX += INCX;
                IY += INCY;
            }
            return ddot;
            // C
            // C     Code for both increments equal to 1.
            // C
            // C     Clean-up loop so remaining vector length is a multiple of 5.
            // C
        LABEL20:  M = FortranLib.Mod(N,5);
            if (M == 0) goto LABEL40;
            for (I = 1; I <= M; I++)
            {
                ddot += DX[I + o_dx] * DY[I + o_dy];
            }
            if (N < 5) return ddot;
        LABEL40:  MP1 = M + 1;
            for (I = MP1; I <= N; I += 5)
            {
                ddot += DX[I + o_dx] * DY[I + o_dy] + DX[I + 1 + o_dx] * DY[I + 1 + o_dy] + DX[I + 2 + o_dx] * DY[I + 2 + o_dy] + DX[I + 3 + o_dx] * DY[I + 3 + o_dy] + DX[I + 4 + o_dx] * DY[I + 4 + o_dy];
            }
            return ddot;
            // C
            // C     Code for equal, positive, non-unit increments.
            // C
        LABEL60:  NS = N * INCX;
            for (I = 1; (INCX >= 0) ? (I <= NS) : (I >= NS); I += INCX)
            {
                ddot += DX[I + o_dx] * DY[I + o_dy];
            }
            return ddot;
            #endregion
        }
    }

    #endregion


    #region The Class: DNRM2
    
    // *DECK DNRM2
    /// <summary>
    /// ***PURPOSE  Compute the Euclidean length (L2 norm) of a vector.
    /// ***CATEGORY  D1A3B
    /// ***TYPE      DOUBLE PRECISION (SNRM2-S, DNRM2-D, SCNRM2-C)
    /// ***KEYWORDS  BLAS, EUCLIDEAN LENGTH, EUCLIDEAN NORM, L2,
    /// LINEAR ALGEBRA, UNITARY, VECTOR
    /// ***AUTHOR  Lawson, C. L., (JPL)
    /// Hanson, R. J., (SNLA)
    /// Kincaid, D. R., (U. of Texas)
    /// Krogh, F. T., (JPL)
    /// ***DESCRIPTION
    /// 
    /// B L A S  Subprogram
    /// Description of parameters
    /// 
    /// --Input--
    /// N  number of elements in input vector(s)
    /// DX  double precision vector with N elements
    /// INCX  storage spacing between elements of DX
    /// 
    /// --Output--
    /// DNRM2  double precision result (zero if N .LE. 0)
    /// 
    /// Euclidean norm of the N-vector stored in DX with storage
    /// increment INCX.
    /// If N .LE. 0, return with result = 0.
    /// If N .GE. 1, then INCX must be .GE. 1
    /// 
    /// Four phase method using two built-in constants that are
    /// hopefully applicable to all machines.
    /// CUTLO = maximum of  SQRT(U/EPS)  over all known machines.
    /// CUTHI = minimum of  SQRT(V)      over all known machines.
    /// where
    /// EPS = smallest no. such that EPS + 1. .GT. 1.
    /// U   = smallest positive no.   (underflow limit)
    /// V   = largest  no.            (overflow  limit)
    /// 
    /// Brief outline of algorithm.
    /// 
    /// Phase 1 scans zero components.
    /// move to phase 2 when a component is nonzero and .LE. CUTLO
    /// move to phase 3 when a component is .GT. CUTLO
    /// move to phase 4 when a component is .GE. CUTHI/M
    /// where M = N for X() real and M = 2*N for complex.
    /// 
    /// Values for CUTLO and CUTHI.
    /// From the environmental parameters listed in the IMSL converter
    /// document the limiting values are as follows:
    /// CUTLO, S.P.   U/EPS = 2**(-102) for  Honeywell.  Close seconds are
    /// Univac and DEC at 2**(-103)
    /// Thus CUTLO = 2**(-51) = 4.44089E-16
    /// CUTHI, S.P.   V = 2**127 for Univac, Honeywell, and DEC.
    /// Thus CUTHI = 2**(63.5) = 1.30438E19
    /// CUTLO, D.P.   U/EPS = 2**(-67) for Honeywell and DEC.
    /// Thus CUTLO = 2**(-33.5) = 8.23181D-11
    /// CUTHI, D.P.   same as S.P.  CUTHI = 1.30438D19
    /// DATA CUTLO, CUTHI /8.232D-11,  1.304D19/
    /// DATA CUTLO, CUTHI /4.441E-16,  1.304E19/
    /// 
    /// ***REFERENCES  C. L. Lawson, R. J. Hanson, D. R. Kincaid and F. T.
    /// Krogh, Basic linear algebra subprograms for Fortran
    /// usage, Algorithm No. 539, Transactions on Mathematical
    /// Software 5, 3 (September 1979), pp. 308-323.
    /// ***ROUTINES CALLED  (NONE)
    /// ***REVISION HISTORY  (YYMMDD)
    /// 791001  DATE WRITTEN
    /// 890531  Changed all specific intrinsics to generic.  (WRB)
    /// 890831  Modified array declarations.  (WRB)
    /// 890831  REVISION DATE from Version 3.2
    /// 891214  Prologue converted to Version 4.0 format.  (BAB)
    /// 920501  Reformatted the REFERENCES section.  (WRB)
    /// ***END PROLOGUE  DNRM2
    /// 
    /// ***FIRST EXECUTABLE STATEMENT  DNRM2
    ///</summary>
    public class DNRM2
    {
    
        #region Variables
        
        double CUTLO = 0; double CUTHI = 0; double ZERO = 0; double ONE = 0; 
        #endregion
        public DNRM2()
        {
    
            #region Data Initialization
            
            //ZERO,ONE/0.0D0,1.0D0
            ZERO = 0.0E0;
            ONE = 1.0E0;
            //CUTLO,CUTHI/8.232D-11,1.304D19
            CUTLO = 8.232E-11;
            CUTHI = 1.304E19;
            #endregion
        }
    
        /// <summary>
        /// ***PURPOSE  Compute the Euclidean length (L2 norm) of a vector.
        /// ***CATEGORY  D1A3B
        /// ***TYPE      DOUBLE PRECISION (SNRM2-S, DNRM2-D, SCNRM2-C)
        /// ***KEYWORDS  BLAS, EUCLIDEAN LENGTH, EUCLIDEAN NORM, L2,
        /// LINEAR ALGEBRA, UNITARY, VECTOR
        /// ***AUTHOR  Lawson, C. L., (JPL)
        /// Hanson, R. J., (SNLA)
        /// Kincaid, D. R., (U. of Texas)
        /// Krogh, F. T., (JPL)
        /// ***DESCRIPTION
        /// 
        /// B L A S  Subprogram
        /// Description of parameters
        /// 
        /// --Input--
        /// N  number of elements in input vector(s)
        /// DX  double precision vector with N elements
        /// INCX  storage spacing between elements of DX
        /// 
        /// --Output--
        /// DNRM2  double precision result (zero if N .LE. 0)
        /// 
        /// Euclidean norm of the N-vector stored in DX with storage
        /// increment INCX.
        /// If N .LE. 0, return with result = 0.
        /// If N .GE. 1, then INCX must be .GE. 1
        /// 
        /// Four phase method using two built-in constants that are
        /// hopefully applicable to all machines.
        /// CUTLO = maximum of  SQRT(U/EPS)  over all known machines.
        /// CUTHI = minimum of  SQRT(V)      over all known machines.
        /// where
        /// EPS = smallest no. such that EPS + 1. .GT. 1.
        /// U   = smallest positive no.   (underflow limit)
        /// V   = largest  no.            (overflow  limit)
        /// 
        /// Brief outline of algorithm.
        /// 
        /// Phase 1 scans zero components.
        /// move to phase 2 when a component is nonzero and .LE. CUTLO
        /// move to phase 3 when a component is .GT. CUTLO
        /// move to phase 4 when a component is .GE. CUTHI/M
        /// where M = N for X() real and M = 2*N for complex.
        /// 
        /// Values for CUTLO and CUTHI.
        /// From the environmental parameters listed in the IMSL converter
        /// document the limiting values are as follows:
        /// CUTLO, S.P.   U/EPS = 2**(-102) for  Honeywell.  Close seconds are
        /// Univac and DEC at 2**(-103)
        /// Thus CUTLO = 2**(-51) = 4.44089E-16
        /// CUTHI, S.P.   V = 2**127 for Univac, Honeywell, and DEC.
        /// Thus CUTHI = 2**(63.5) = 1.30438E19
        /// CUTLO, D.P.   U/EPS = 2**(-67) for Honeywell and DEC.
        /// Thus CUTLO = 2**(-33.5) = 8.23181D-11
        /// CUTHI, D.P.   same as S.P.  CUTHI = 1.30438D19
        /// DATA CUTLO, CUTHI /8.232D-11,  1.304D19/
        /// DATA CUTLO, CUTHI /4.441E-16,  1.304E19/
        /// 
        /// ***REFERENCES  C. L. Lawson, R. J. Hanson, D. R. Kincaid and F. T.
        /// Krogh, Basic linear algebra subprograms for Fortran
        /// usage, Algorithm No. 539, Transactions on Mathematical
        /// Software 5, 3 (September 1979), pp. 308-323.
        /// ***ROUTINES CALLED  (NONE)
        /// ***REVISION HISTORY  (YYMMDD)
        /// 791001  DATE WRITTEN
        /// 890531  Changed all specific intrinsics to generic.  (WRB)
        /// 890831  Modified array declarations.  (WRB)
        /// 890831  REVISION DATE from Version 3.2
        /// 891214  Prologue converted to Version 4.0 format.  (BAB)
        /// 920501  Reformatted the REFERENCES section.  (WRB)
        /// ***END PROLOGUE  DNRM2
        /// 
        /// ***FIRST EXECUTABLE STATEMENT  DNRM2
        ///</summary>
        /// <param name="N">
        /// number of elements in input vector(s)
        ///</param>
        /// <param name="DX">
        /// double precision vector with N elements
        ///</param>
        /// <param name="INCX">
        /// storage spacing between elements of DX
        ///</param>
        public double Run(int N, double[] DX, int offset_dx, int INCX)
        {
        double dnrm2 = 0;
            #region Variables
            
            int NEXT = 0; double HITEST = 0; double SUM = 0; double XMAX = 0; 
            #endregion
            #region Implicit Variables
            
            int NN = 0; int I = 0; int J = 0; 
            #endregion
            #region Array Index Correction
            
             int o_dx = -1 + offset_dx; 
            #endregion
            #region Prolog
            
            // C***BEGIN PROLOGUE  DNRM2
            // C***PURPOSE  Compute the Euclidean length (L2 norm) of a vector.
            // C***CATEGORY  D1A3B
            // C***TYPE      DOUBLE PRECISION (SNRM2-S, DNRM2-D, SCNRM2-C)
            // C***KEYWORDS  BLAS, EUCLIDEAN LENGTH, EUCLIDEAN NORM, L2,
            // C             LINEAR ALGEBRA, UNITARY, VECTOR
            // C***AUTHOR  Lawson, C. L., (JPL)
            // C           Hanson, R. J., (SNLA)
            // C           Kincaid, D. R., (U. of Texas)
            // C           Krogh, F. T., (JPL)
            // C***DESCRIPTION
            // C
            // C                B L A S  Subprogram
            // C    Description of parameters
            // C
            // C     --Input--
            // C        N  number of elements in input vector(s)
            // C       DX  double precision vector with N elements
            // C     INCX  storage spacing between elements of DX
            // C
            // C     --Output--
            // C    DNRM2  double precision result (zero if N .LE. 0)
            // C
            // C     Euclidean norm of the N-vector stored in DX with storage
            // C     increment INCX.
            // C     If N .LE. 0, return with result = 0.
            // C     If N .GE. 1, then INCX must be .GE. 1
            // C
            // C     Four phase method using two built-in constants that are
            // C     hopefully applicable to all machines.
            // C         CUTLO = maximum of  SQRT(U/EPS)  over all known machines.
            // C         CUTHI = minimum of  SQRT(V)      over all known machines.
            // C     where
            // C         EPS = smallest no. such that EPS + 1. .GT. 1.
            // C         U   = smallest positive no.   (underflow limit)
            // C         V   = largest  no.            (overflow  limit)
            // C
            // C     Brief outline of algorithm.
            // C
            // C     Phase 1 scans zero components.
            // C     move to phase 2 when a component is nonzero and .LE. CUTLO
            // C     move to phase 3 when a component is .GT. CUTLO
            // C     move to phase 4 when a component is .GE. CUTHI/M
            // C     where M = N for X() real and M = 2*N for complex.
            // C
            // C     Values for CUTLO and CUTHI.
            // C     From the environmental parameters listed in the IMSL converter
            // C     document the limiting values are as follows:
            // C     CUTLO, S.P.   U/EPS = 2**(-102) for  Honeywell.  Close seconds are
            // C                   Univac and DEC at 2**(-103)
            // C                   Thus CUTLO = 2**(-51) = 4.44089E-16
            // C     CUTHI, S.P.   V = 2**127 for Univac, Honeywell, and DEC.
            // C                   Thus CUTHI = 2**(63.5) = 1.30438E19
            // C     CUTLO, D.P.   U/EPS = 2**(-67) for Honeywell and DEC.
            // C                   Thus CUTLO = 2**(-33.5) = 8.23181D-11
            // C     CUTHI, D.P.   same as S.P.  CUTHI = 1.30438D19
            // C     DATA CUTLO, CUTHI /8.232D-11,  1.304D19/
            // C     DATA CUTLO, CUTHI /4.441E-16,  1.304E19/
            // C
            // C***REFERENCES  C. L. Lawson, R. J. Hanson, D. R. Kincaid and F. T.
            // C                 Krogh, Basic linear algebra subprograms for Fortran
            // C                 usage, Algorithm No. 539, Transactions on Mathematical
            // C                 Software 5, 3 (September 1979), pp. 308-323.
            // C***ROUTINES CALLED  (NONE)
            // C***REVISION HISTORY  (YYMMDD)
            // C   791001  DATE WRITTEN
            // C   890531  Changed all specific intrinsics to generic.  (WRB)
            // C   890831  Modified array declarations.  (WRB)
            // C   890831  REVISION DATE from Version 3.2
            // C   891214  Prologue converted to Version 4.0 format.  (BAB)
            // C   920501  Reformatted the REFERENCES section.  (WRB)
            // C***END PROLOGUE  DNRM2
            // C
            // C***FIRST EXECUTABLE STATEMENT  DNRM2
            #endregion
            #region Body
            
            if (N > 0) goto LABEL10;
            dnrm2 = ZERO;
            goto LABEL300;
            // C
        LABEL10:  NEXT = 30;
            SUM = ZERO;
            NN = N * INCX;
            // C
            // C                                                 BEGIN MAIN LOOP
            // C
            I = 1;
        LABEL20:
                switch (NEXT)
                {
                    case 30: goto LABEL30;
                    case 50: goto LABEL50;
                    case 70: goto LABEL70;
                    case 110: goto LABEL110;
                }
        LABEL30:  
            if (Math.Abs(DX[I + o_dx]) > CUTLO) goto LABEL85;
            NEXT = 50;
            XMAX = ZERO;
            // C
            // C                        PHASE 1.  SUM IS ZERO
            // C
        LABEL50:  
            if (DX[I + o_dx] == ZERO) goto LABEL200;
            if (Math.Abs(DX[I + o_dx]) > CUTLO) goto LABEL85;
            // C
            // C                                PREPARE FOR PHASE 2.
            // C
            NEXT = 70;
            goto LABEL105;
            // C
            // C                                PREPARE FOR PHASE 4.
            // C
        LABEL100:  I = J;
            NEXT = 110;
            SUM = (SUM / DX[I + o_dx]) / DX[I + o_dx];
        LABEL105:  XMAX = Math.Abs(DX[I + o_dx]);
            goto LABEL115;
            // C
            // C                   PHASE 2.  SUM IS SMALL.
            // C                             SCALE TO AVOID DESTRUCTIVE UNDERFLOW.
            // C
        LABEL70:  
            if (Math.Abs(DX[I + o_dx]) > CUTLO) goto LABEL75;
            // C
            // C                     COMMON CODE FOR PHASES 2 AND 4.
            // C                     IN PHASE 4 SUM IS LARGE.  SCALE TO AVOID OVERFLOW.
            // C
        LABEL110:  
            if (Math.Abs(DX[I + o_dx]) <= XMAX) goto LABEL115;
            SUM = ONE + SUM * Math.Pow(XMAX / DX[I + o_dx],2);
            XMAX = Math.Abs(DX[I + o_dx]);
            goto LABEL200;
            // C
        LABEL115:  SUM += Math.Pow(DX[I + o_dx] / XMAX,2);
            goto LABEL200;
            // C
            // C                  PREPARE FOR PHASE 3.
            // C
        LABEL75:  SUM = (SUM * XMAX) * XMAX;
            // C
            // C     FOR REAL OR D.P. SET HITEST = CUTHI/N
            // C     FOR COMPLEX      SET HITEST = CUTHI/(2*N)
            // C
        LABEL85:  HITEST = CUTHI / N;
            // C
            // C                   PHASE 3.  SUM IS MID-RANGE.  NO SCALING.
            // C
            for (J = I; (INCX >= 0) ? (J <= NN) : (J >= NN); J += INCX)
            {
                if (Math.Abs(DX[J + o_dx]) >= HITEST) goto LABEL100;
                SUM += Math.Pow(DX[J + o_dx],2);
            }
            dnrm2 = Math.Sqrt(SUM);
            goto LABEL300;
            // C
        LABEL200:;
            I += INCX;
            if (I <= NN) goto LABEL20;
            // C
            // C              END OF MAIN LOOP.
            // C
            // C              COMPUTE SQUARE ROOT AND ADJUST FOR SCALING.
            // C
            dnrm2 = XMAX * Math.Sqrt(SUM);
        LABEL300:;
            return dnrm2;
            #endregion
        }
    }

    #endregion


    #region The Class: DSCAL
    
    // *DECK DSCAL
    /// <summary>
    /// ***PURPOSE  Multiply a vector by a constant.
    /// ***CATEGORY  D1A6
    /// ***TYPE      DOUBLE PRECISION (SSCAL-S, DSCAL-D, CSCAL-C)
    /// ***KEYWORDS  BLAS, LINEAR ALGEBRA, SCALE, VECTOR
    /// ***AUTHOR  Lawson, C. L., (JPL)
    /// Hanson, R. J., (SNLA)
    /// Kincaid, D. R., (U. of Texas)
    /// Krogh, F. T., (JPL)
    /// ***DESCRIPTION
    /// 
    /// B L A S  Subprogram
    /// Description of Parameters
    /// 
    /// --Input--
    /// N  number of elements in input vector(s)
    /// DA  double precision scale factor
    /// DX  double precision vector with N elements
    /// INCX  storage spacing between elements of DX
    /// 
    /// --Output--
    /// DX  double precision result (unchanged if N.LE.0)
    /// 
    /// Replace double precision DX by double precision DA*DX.
    /// For I = 0 to N-1, replace DX(IX+I*INCX) with  DA * DX(IX+I*INCX),
    /// where IX = 1 if INCX .GE. 0, else IX = 1+(1-N)*INCX.
    /// 
    /// ***REFERENCES  C. L. Lawson, R. J. Hanson, D. R. Kincaid and F. T.
    /// Krogh, Basic linear algebra subprograms for Fortran
    /// usage, Algorithm No. 539, Transactions on Mathematical
    /// Software 5, 3 (September 1979), pp. 308-323.
    /// ***ROUTINES CALLED  (NONE)
    /// ***REVISION HISTORY  (YYMMDD)
    /// 791001  DATE WRITTEN
    /// 890831  Modified array declarations.  (WRB)
    /// 890831  REVISION DATE from Version 3.2
    /// 891214  Prologue converted to Version 4.0 format.  (BAB)
    /// 900821  Modified to correct problem with a negative increment.
    /// (WRB)
    /// 920501  Reformatted the REFERENCES section.  (WRB)
    /// ***END PROLOGUE  DSCAL
    /// ***FIRST EXECUTABLE STATEMENT  DSCAL
    ///</summary>
    public class DSCAL
    {
    
        public DSCAL()
        {
    
        }
    
        /// <summary>
        /// ***PURPOSE  Multiply a vector by a constant.
        /// ***CATEGORY  D1A6
        /// ***TYPE      DOUBLE PRECISION (SSCAL-S, DSCAL-D, CSCAL-C)
        /// ***KEYWORDS  BLAS, LINEAR ALGEBRA, SCALE, VECTOR
        /// ***AUTHOR  Lawson, C. L., (JPL)
        /// Hanson, R. J., (SNLA)
        /// Kincaid, D. R., (U. of Texas)
        /// Krogh, F. T., (JPL)
        /// ***DESCRIPTION
        /// 
        /// B L A S  Subprogram
        /// Description of Parameters
        /// 
        /// --Input--
        /// N  number of elements in input vector(s)
        /// DA  double precision scale factor
        /// DX  double precision vector with N elements
        /// INCX  storage spacing between elements of DX
        /// 
        /// --Output--
        /// DX  double precision result (unchanged if N.LE.0)
        /// 
        /// Replace double precision DX by double precision DA*DX.
        /// For I = 0 to N-1, replace DX(IX+I*INCX) with  DA * DX(IX+I*INCX),
        /// where IX = 1 if INCX .GE. 0, else IX = 1+(1-N)*INCX.
        /// 
        /// ***REFERENCES  C. L. Lawson, R. J. Hanson, D. R. Kincaid and F. T.
        /// Krogh, Basic linear algebra subprograms for Fortran
        /// usage, Algorithm No. 539, Transactions on Mathematical
        /// Software 5, 3 (September 1979), pp. 308-323.
        /// ***ROUTINES CALLED  (NONE)
        /// ***REVISION HISTORY  (YYMMDD)
        /// 791001  DATE WRITTEN
        /// 890831  Modified array declarations.  (WRB)
        /// 890831  REVISION DATE from Version 3.2
        /// 891214  Prologue converted to Version 4.0 format.  (BAB)
        /// 900821  Modified to correct problem with a negative increment.
        /// (WRB)
        /// 920501  Reformatted the REFERENCES section.  (WRB)
        /// ***END PROLOGUE  DSCAL
        /// ***FIRST EXECUTABLE STATEMENT  DSCAL
        ///</summary>
        /// <param name="N">
        /// number of elements in input vector(s)
        ///</param>
        /// <param name="DA">
        /// double precision scale factor
        ///</param>
        /// <param name="DX">
        /// double precision vector with N elements
        ///</param>
        /// <param name="INCX">
        /// storage spacing between elements of DX
        ///</param>
        public void Run(int N, double DA, ref double[] DX, int offset_dx, int INCX)
        {
            #region Variables
            
            int I = 0; int IX = 0; int M = 0; int MP1 = 0; 
            #endregion
            #region Array Index Correction
            
             int o_dx = -1 + offset_dx; 
            #endregion
            #region Prolog
            
            // C***BEGIN PROLOGUE  DSCAL
            // C***PURPOSE  Multiply a vector by a constant.
            // C***CATEGORY  D1A6
            // C***TYPE      DOUBLE PRECISION (SSCAL-S, DSCAL-D, CSCAL-C)
            // C***KEYWORDS  BLAS, LINEAR ALGEBRA, SCALE, VECTOR
            // C***AUTHOR  Lawson, C. L., (JPL)
            // C           Hanson, R. J., (SNLA)
            // C           Kincaid, D. R., (U. of Texas)
            // C           Krogh, F. T., (JPL)
            // C***DESCRIPTION
            // C
            // C                B L A S  Subprogram
            // C    Description of Parameters
            // C
            // C     --Input--
            // C        N  number of elements in input vector(s)
            // C       DA  double precision scale factor
            // C       DX  double precision vector with N elements
            // C     INCX  storage spacing between elements of DX
            // C
            // C     --Output--
            // C       DX  double precision result (unchanged if N.LE.0)
            // C
            // C     Replace double precision DX by double precision DA*DX.
            // C     For I = 0 to N-1, replace DX(IX+I*INCX) with  DA * DX(IX+I*INCX),
            // C     where IX = 1 if INCX .GE. 0, else IX = 1+(1-N)*INCX.
            // C
            // C***REFERENCES  C. L. Lawson, R. J. Hanson, D. R. Kincaid and F. T.
            // C                 Krogh, Basic linear algebra subprograms for Fortran
            // C                 usage, Algorithm No. 539, Transactions on Mathematical
            // C                 Software 5, 3 (September 1979), pp. 308-323.
            // C***ROUTINES CALLED  (NONE)
            // C***REVISION HISTORY  (YYMMDD)
            // C   791001  DATE WRITTEN
            // C   890831  Modified array declarations.  (WRB)
            // C   890831  REVISION DATE from Version 3.2
            // C   891214  Prologue converted to Version 4.0 format.  (BAB)
            // C   900821  Modified to correct problem with a negative increment.
            // C           (WRB)
            // C   920501  Reformatted the REFERENCES section.  (WRB)
            // C***END PROLOGUE  DSCAL
            // C***FIRST EXECUTABLE STATEMENT  DSCAL
            #endregion
            #region Body
            
            if (N <= 0) return;
            if (INCX == 1) goto LABEL20;
            // C
            // C     Code for increment not equal to 1.
            // C
            IX = 1;
            if (INCX < 0) IX = ( - N + 1) * INCX + 1;
            for (I = 1; I <= N; I++)
            {
                DX[IX + o_dx] *= DA;
                IX += INCX;
            }
            return;
            // C
            // C     Code for increment equal to 1.
            // C
            // C     Clean-up loop so remaining vector length is a multiple of 5.
            // C
        LABEL20:  M = FortranLib.Mod(N,5);
            if (M == 0) goto LABEL40;
            for (I = 1; I <= M; I++)
            {
                DX[I + o_dx] *= DA;
            }
            if (N < 5) return;
        LABEL40:  MP1 = M + 1;
            for (I = MP1; I <= N; I += 5)
            {
                DX[I + o_dx] *= DA;
                DX[I + 1 + o_dx] *= DA;
                DX[I + 2 + o_dx] *= DA;
                DX[I + 3 + o_dx] *= DA;
                DX[I + 4 + o_dx] *= DA;
            }
            return;
            #endregion
        }
    }

    #endregion


    #region The Class: IDAMAX
    
    // *DECK IDAMAX
    /// <summary>
    /// ***PURPOSE  Find the smallest index of that component of a vector
    /// having the maximum magnitude.
    /// ***CATEGORY  D1A2
    /// ***TYPE      DOUBLE PRECISION (ISAMAX-S, IDAMAX-D, ICAMAX-C)
    /// ***KEYWORDS  BLAS, LINEAR ALGEBRA, MAXIMUM COMPONENT, VECTOR
    /// ***AUTHOR  Lawson, C. L., (JPL)
    /// Hanson, R. J., (SNLA)
    /// Kincaid, D. R., (U. of Texas)
    /// Krogh, F. T., (JPL)
    /// ***DESCRIPTION
    /// 
    /// B L A S  Subprogram
    /// Description of Parameters
    /// 
    /// --Input--
    /// N  number of elements in input vector(s)
    /// DX  double precision vector with N elements
    /// INCX  storage spacing between elements of DX
    /// 
    /// --Output--
    /// IDAMAX  smallest index (zero if N .LE. 0)
    /// 
    /// Find smallest index of maximum magnitude of double precision DX.
    /// IDAMAX = first I, I = 1 to N, to maximize ABS(DX(IX+(I-1)*INCX)),
    /// where IX = 1 if INCX .GE. 0, else IX = 1+(1-N)*INCX.
    /// 
    /// ***REFERENCES  C. L. Lawson, R. J. Hanson, D. R. Kincaid and F. T.
    /// Krogh, Basic linear algebra subprograms for Fortran
    /// usage, Algorithm No. 539, Transactions on Mathematical
    /// Software 5, 3 (September 1979), pp. 308-323.
    /// ***ROUTINES CALLED  (NONE)
    /// ***REVISION HISTORY  (YYMMDD)
    /// 791001  DATE WRITTEN
    /// 890531  Changed all specific intrinsics to generic.  (WRB)
    /// 890531  REVISION DATE from Version 3.2
    /// 891214  Prologue converted to Version 4.0 format.  (BAB)
    /// 900821  Modified to correct problem with a negative increment.
    /// (WRB)
    /// 920501  Reformatted the REFERENCES section.  (WRB)
    /// ***END PROLOGUE  IDAMAX
    /// ***FIRST EXECUTABLE STATEMENT  IDAMAX
    ///</summary>
    public class IDAMAX
    {
    
        public IDAMAX()
        {
    
        }
    
        /// <summary>
        /// ***PURPOSE  Find the smallest index of that component of a vector
        /// having the maximum magnitude.
        /// ***CATEGORY  D1A2
        /// ***TYPE      DOUBLE PRECISION (ISAMAX-S, IDAMAX-D, ICAMAX-C)
        /// ***KEYWORDS  BLAS, LINEAR ALGEBRA, MAXIMUM COMPONENT, VECTOR
        /// ***AUTHOR  Lawson, C. L., (JPL)
        /// Hanson, R. J., (SNLA)
        /// Kincaid, D. R., (U. of Texas)
        /// Krogh, F. T., (JPL)
        /// ***DESCRIPTION
        /// 
        /// B L A S  Subprogram
        /// Description of Parameters
        /// 
        /// --Input--
        /// N  number of elements in input vector(s)
        /// DX  double precision vector with N elements
        /// INCX  storage spacing between elements of DX
        /// 
        /// --Output--
        /// IDAMAX  smallest index (zero if N .LE. 0)
        /// 
        /// Find smallest index of maximum magnitude of double precision DX.
        /// IDAMAX = first I, I = 1 to N, to maximize ABS(DX(IX+(I-1)*INCX)),
        /// where IX = 1 if INCX .GE. 0, else IX = 1+(1-N)*INCX.
        /// 
        /// ***REFERENCES  C. L. Lawson, R. J. Hanson, D. R. Kincaid and F. T.
        /// Krogh, Basic linear algebra subprograms for Fortran
        /// usage, Algorithm No. 539, Transactions on Mathematical
        /// Software 5, 3 (September 1979), pp. 308-323.
        /// ***ROUTINES CALLED  (NONE)
        /// ***REVISION HISTORY  (YYMMDD)
        /// 791001  DATE WRITTEN
        /// 890531  Changed all specific intrinsics to generic.  (WRB)
        /// 890531  REVISION DATE from Version 3.2
        /// 891214  Prologue converted to Version 4.0 format.  (BAB)
        /// 900821  Modified to correct problem with a negative increment.
        /// (WRB)
        /// 920501  Reformatted the REFERENCES section.  (WRB)
        /// ***END PROLOGUE  IDAMAX
        /// ***FIRST EXECUTABLE STATEMENT  IDAMAX
        ///</summary>
        /// <param name="N">
        /// number of elements in input vector(s)
        ///</param>
        /// <param name="DX">
        /// double precision vector with N elements
        ///</param>
        /// <param name="INCX">
        /// storage spacing between elements of DX
        ///</param>
        public int Run(int N, double[] DX, int offset_dx, int INCX)
        {
        int idamax = 0;
            #region Variables
            
            double DMAX = 0; double XMAG = 0; int I = 0; int IX = 0; 
            #endregion
            #region Array Index Correction
            
             int o_dx = -1 + offset_dx; 
            #endregion
            #region Prolog
            
            // C***BEGIN PROLOGUE  IDAMAX
            // C***PURPOSE  Find the smallest index of that component of a vector
            // C            having the maximum magnitude.
            // C***CATEGORY  D1A2
            // C***TYPE      DOUBLE PRECISION (ISAMAX-S, IDAMAX-D, ICAMAX-C)
            // C***KEYWORDS  BLAS, LINEAR ALGEBRA, MAXIMUM COMPONENT, VECTOR
            // C***AUTHOR  Lawson, C. L., (JPL)
            // C           Hanson, R. J., (SNLA)
            // C           Kincaid, D. R., (U. of Texas)
            // C           Krogh, F. T., (JPL)
            // C***DESCRIPTION
            // C
            // C                B L A S  Subprogram
            // C    Description of Parameters
            // C
            // C     --Input--
            // C        N  number of elements in input vector(s)
            // C       DX  double precision vector with N elements
            // C     INCX  storage spacing between elements of DX
            // C
            // C     --Output--
            // C   IDAMAX  smallest index (zero if N .LE. 0)
            // C
            // C     Find smallest index of maximum magnitude of double precision DX.
            // C     IDAMAX = first I, I = 1 to N, to maximize ABS(DX(IX+(I-1)*INCX)),
            // C     where IX = 1 if INCX .GE. 0, else IX = 1+(1-N)*INCX.
            // C
            // C***REFERENCES  C. L. Lawson, R. J. Hanson, D. R. Kincaid and F. T.
            // C                 Krogh, Basic linear algebra subprograms for Fortran
            // C                 usage, Algorithm No. 539, Transactions on Mathematical
            // C                 Software 5, 3 (September 1979), pp. 308-323.
            // C***ROUTINES CALLED  (NONE)
            // C***REVISION HISTORY  (YYMMDD)
            // C   791001  DATE WRITTEN
            // C   890531  Changed all specific intrinsics to generic.  (WRB)
            // C   890531  REVISION DATE from Version 3.2
            // C   891214  Prologue converted to Version 4.0 format.  (BAB)
            // C   900821  Modified to correct problem with a negative increment.
            // C           (WRB)
            // C   920501  Reformatted the REFERENCES section.  (WRB)
            // C***END PROLOGUE  IDAMAX
            // C***FIRST EXECUTABLE STATEMENT  IDAMAX
            #endregion
            #region Body
            
            idamax = 0;
            if (N <= 0) return idamax;
            idamax = 1;
            if (N == 1) return idamax;
            // C
            if (INCX == 1) goto LABEL20;
            // C
            // C     Code for increments not equal to 1.
            // C
            IX = 1;
            if (INCX < 0) IX = ( - N + 1) * INCX + 1;
            DMAX = Math.Abs(DX[IX + o_dx]);
            IX += INCX;
            for (I = 2; I <= N; I++)
            {
                XMAG = Math.Abs(DX[IX + o_dx]);
                if (XMAG > DMAX)
                {
                    idamax = I;
                    DMAX = XMAG;
                }
                IX += INCX;
            }
            return idamax;
            // C
            // C     Code for increments equal to 1.
            // C
        LABEL20:  DMAX = Math.Abs(DX[1 + o_dx]);
            for (I = 2; I <= N; I++)
            {
                XMAG = Math.Abs(DX[I + o_dx]);
                if (XMAG > DMAX)
                {
                    idamax = I;
                    DMAX = XMAG;
                }
            }
            return idamax;
            #endregion
        }
    }

    #endregion

}
