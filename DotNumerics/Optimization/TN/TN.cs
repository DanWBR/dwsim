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

namespace DotNumerics.Optimization.TN
{

    #region The Class: TN
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    // C# to unbundle, sh this file (in an empty directory)
    // Cecho README 1>&2
    // Csed >README <<'//GO.SYSIN DD README' 's/^-//'
    // Ctn.f	Subroutines TN and TNBC, which solve unconstrained and simply
    // C	bounded optimization problems by a truncated Newton algorithm.
    // C	You must adapt subroutine MCHPR1 to your machine.  Comments in
    // C	TN and TNBC explain their usage.
    // C
    // Cblas.f	Standard BLAS (basic linear algebra subroutines) used by tn.f,
    // C	plus one nonstandard one (DXPY).
    // C
    // C
    // C	Sample calling programs:
    // C
    // Cmainc.f		Customized, no bounds.
    // Cmaincb.f	Customized, bounds on variables.
    // Cmainfc.f	Finite-differencing, customized, no bounds.
    // Cmains.f		Easy to use, no bounds.
    // Cmainsb.f	Easy to use, bounds on variables.
    // C
    // COutput from running these sample programs on an SGI 4D/240S computer
    // C	(IEEE arithmetic) appears in the corresponding .out files:
    // C	mainc.out is from mainc.f, maincb.out from maincb.f, etc.
    // C
    // CAll the Fortran files were written and supplied by
    // C
    // C	Stephen G. Nash
    // C	George Mason University
    // C	Dept. of Operational Research & Applied Statistics
    // C	Fairfax, VA 22030
    // C
    // C	SNASH%GMUVAX.BITNET@CUNYVM.CUNY.EDU
    // C//GO.SYSIN DD README
    // Cecho tn.f 1>&2
    // Csed >tn.f <<'//GO.SYSIN DD tn.f' 's/^-//'
    // C%% TRUNCATED-NEWTON METHOD:  SUBROUTINES
    // C   FOR OTHER MACHINES, MODIFY ROUTINE MCHPR1 (MACHINE EPSILON)
    // C   WRITTEN BY:  STEPHEN G. NASH
    // C                OPERATIONS RESEARCH AND APPLIED STATISTICS DEPT.
    // C                GEORGE MASON UNIVERSITY
    // C                FAIRFAX, VA 22030
    // C******************************************************************
    public class TN
    {
    
        #region Dependencies
        
        MCHPR1 _mchpr1; LMQN _lmqn; 
        #endregion
        public TN(MCHPR1 mchpr1, LMQN lmqn)
        {
    
            #region Set Dependencies
            
            this._mchpr1 = mchpr1; this._lmqn = lmqn; 
            #endregion
        }
    
        public TN()
        {
    
            #region Initialization Common Blocks
            
            CommonBlock SUBSCR = new CommonBlock(0, 15, 0, 0);
            #endregion
            #region Dependencies (Initialization)
            
            MCHPR1 mchpr1 = new MCHPR1();
            DNRM2 dnrm2 = new DNRM2();
            DDOT ddot = new DDOT();
            DCOPY dcopy = new DCOPY();
            ZTIME ztime = new ZTIME();
            SSBFGS ssbfgs = new SSBFGS();
            DAXPY daxpy = new DAXPY();
            NEGVEC negvec = new NEGVEC();
            GETPTC getptc = new GETPTC();
            LSOUT lsout = new LSOUT();
            DXPY dxpy = new DXPY();
            SETPAR setpar = new SETPAR(SUBSCR);
            GTIMS gtims = new GTIMS(SUBSCR);
            STEP1 step1 = new STEP1(mchpr1);
            CHKUCP chkucp = new CHKUCP(mchpr1, dnrm2);
            SETUCR setucr = new SETUCR(ddot);
            INITP3 initp3 = new INITP3(ddot, dcopy);
            INITPC initpc = new INITPC(initp3, SUBSCR);
            MSLV mslv = new MSLV(ddot, ssbfgs);
            MSOLVE msolve = new MSOLVE(mslv, SUBSCR);
            NDIA3 ndia3 = new NDIA3(ddot);
            MODLNP modlnp = new MODLNP(ddot, initpc, ztime, msolve, gtims, ndia3, daxpy, negvec, dcopy);
            LINDER linder = new LINDER(ddot, getptc, lsout, dcopy);
            LMQN lmqn = new LMQN(dnrm2, step1, ddot, setpar, chkucp, setucr, modlnp, dcopy, linder, dxpy
                                 , SUBSCR);
            #endregion
            #region Set Dependencies
            
            this._mchpr1 = mchpr1; this._lmqn = lmqn; 
            #endregion
        }
        /// <param name="IERROR">
        /// - (INTEGER) ERROR CODE
        /// ( 0 =.GT. NORMAL RETURN)
        /// ( 2 =.GT. MORE THAN MAXFUN EVALUATIONS)
        /// ( 3 =.GT. LINE SEARCH FAILED TO FIND
        /// (          LOWER POINT (MAY NOT BE SERIOUS)
        /// (-1 =.GT. ERROR IN INPUT PARAMETERS)
        ///</param>
        /// <param name="N">
        /// - (INTEGER) NUMBER OF VARIABLES
        ///</param>
        /// <param name="X">
        /// - (REAL*8) VECTOR OF LENGTH AT LEAST N; ON INPUT, AN INITIAL
        /// ESTIMATE OF THE SOLUTION; ON OUTPUT, THE COMPUTED SOLUTION.
        ///</param>
        /// <param name="F">
        /// - (REAL*8) ON INPUT, A ROUGH ESTIMATE OF THE VALUE OF THE
        /// OBJECTIVE FUNCTION AT THE SOLUTION; ON OUTPUT, THE VALUE
        /// OF THE OBJECTIVE FUNCTION AT THE SOLUTION
        ///</param>
        /// <param name="G">
        /// - (REAL*8) VECTOR OF LENGTH AT LEAST N; ON OUTPUT, THE FINAL
        /// VALUE OF THE GRADIENT
        ///</param>
        /// <param name="W">
        /// - (REAL*8) WORK VECTOR OF LENGTH AT LEAST 14*N
        ///</param>
        /// <param name="LW">
        /// - (INTEGER) THE DECLARED DIMENSION OF W
        ///</param>
        /// <param name="SFUN">
        /// - A USER-SPECIFIED SUBROUTINE THAT COMPUTES THE FUNCTION
        /// AND GRADIENT OF THE OBJECTIVE FUNCTION.  IT MUST HAVE
        /// THE CALLING SEQUENCE
        /// SUBROUTINE SFUN (N, X, F, G)
        /// INTEGER           N
        /// DOUBLE PRECISION  X(N), G(N), F
        ///</param>
        public void Run(ref int IERROR, int N, ref double[] X, int offset_x, ref double F, ref double[] G, int offset_g, ref double[] W, int offset_w
                         , int LW, ISFUN SFUN)
        {
            #region Variables
            
            double ETA = 0; double ACCRCY = 0; double XTOL = 0; double STEPMX = 0; double DSQRT = 0; 
            #endregion
            #region Implicit Variables
            
            int MAXIT = 0; int MSGLVL = 0; int MAXFUN = 0; int NMAX = 0; 
            #endregion
            #region Array Index Correction
            
             int o_x = -1 + offset_x;  int o_g = -1 + offset_g;  int o_w = -1 + offset_w; 
            #endregion
            #region Prolog
            
            // C
            // C THIS ROUTINE SOLVES THE OPTIMIZATION PROBLEM
            // C
            // C            MINIMIZE F(X)
            // C               X
            // C
            // C WHERE X IS A VECTOR OF N REAL VARIABLES.  THE METHOD USED IS
            // C A TRUNCATED-NEWTON ALGORITHM (SEE "NEWTON-TYPE MINIMIZATION VIA
            // C THE LANCZOS METHOD" BY S.G. NASH (SIAM J. NUMER. ANAL. 21 (1984),
            // C PP. 770-778).  THIS ALGORITHM FINDS A LOCAL MINIMUM OF F(X).  IT DOES
            // C NOT ASSUME THAT THE FUNCTION F IS CONVEX (AND SO CANNOT GUARANTEE A
            // C GLOBAL SOLUTION), BUT DOES ASSUME THAT THE FUNCTION IS BOUNDED BELOW.
            // C IT CAN SOLVE PROBLEMS HAVING ANY NUMBER OF VARIABLES, BUT IT IS
            // C ESPECIALLY USEFUL WHEN THE NUMBER OF VARIABLES (N) IS LARGE.
            // C
            // C SUBROUTINE PARAMETERS:
            // C
            // C IERROR - (INTEGER) ERROR CODE
            // C          ( 0 => NORMAL RETURN)
            // C          ( 2 => MORE THAN MAXFUN EVALUATIONS)
            // C          ( 3 => LINE SEARCH FAILED TO FIND
            // C          (          LOWER POINT (MAY NOT BE SERIOUS)
            // C          (-1 => ERROR IN INPUT PARAMETERS)
            // C N      - (INTEGER) NUMBER OF VARIABLES
            // C X      - (REAL*8) VECTOR OF LENGTH AT LEAST N; ON INPUT, AN INITIAL
            // C          ESTIMATE OF THE SOLUTION; ON OUTPUT, THE COMPUTED SOLUTION.
            // C G      - (REAL*8) VECTOR OF LENGTH AT LEAST N; ON OUTPUT, THE FINAL
            // C          VALUE OF THE GRADIENT
            // C F      - (REAL*8) ON INPUT, A ROUGH ESTIMATE OF THE VALUE OF THE
            // C          OBJECTIVE FUNCTION AT THE SOLUTION; ON OUTPUT, THE VALUE
            // C          OF THE OBJECTIVE FUNCTION AT THE SOLUTION
            // C W      - (REAL*8) WORK VECTOR OF LENGTH AT LEAST 14*N
            // C LW     - (INTEGER) THE DECLARED DIMENSION OF W
            // C SFUN   - A USER-SPECIFIED SUBROUTINE THAT COMPUTES THE FUNCTION
            // C          AND GRADIENT OF THE OBJECTIVE FUNCTION.  IT MUST HAVE
            // C          THE CALLING SEQUENCE
            // C             SUBROUTINE SFUN (N, X, F, G)
            // C             INTEGER           N
            // C             DOUBLE PRECISION  X(N), G(N), F
            // C
            // C THIS IS AN EASY-TO-USE DRIVER FOR THE MAIN OPTIMIZATION ROUTINE
            // C LMQN.  MORE EXPERIENCED USERS WHO WISH TO CUSTOMIZE PERFORMANCE
            // C OF THIS ALGORITHM SHOULD CALL LMQN DIRECTLY.
            // C
            // C----------------------------------------------------------------------
            // C THIS ROUTINE SETS UP ALL THE PARAMETERS FOR THE TRUNCATED-NEWTON
            // C ALGORITHM.  THE PARAMETERS ARE:
            // C
            // C ETA    - SEVERITY OF THE LINESEARCH
            // C MAXFUN - MAXIMUM ALLOWABLE NUMBER OF FUNCTION EVALUATIONS
            // C XTOL   - DESIRED ACCURACY FOR THE SOLUTION X*
            // C STEPMX - MAXIMUM ALLOWABLE STEP IN THE LINESEARCH
            // C ACCRCY - ACCURACY OF COMPUTED FUNCTION VALUES
            // C MSGLVL - DETERMINES QUANTITY OF PRINTED OUTPUT
            // C          0 = NONE, 1 = ONE LINE PER MAJOR ITERATION.
            // C MAXIT  - MAXIMUM NUMBER OF INNER ITERATIONS PER STEP
            // C
            // C
            // C SET UP PARAMETERS FOR THE OPTIMIZATION ROUTINE
            // C
            #endregion
            #region Body
            
            MAXIT = N / 2;
            if (MAXIT > 50) MAXIT = 50;
            if (MAXIT <= 0) MAXIT = 1;
            MSGLVL = 1;
            MAXFUN = 150 * N;
            ETA = .25E0;
            STEPMX = 1.0E1;
            ACCRCY = 1.0E2 * this._mchpr1.Run();
            XTOL = Math.Sqrt(ACCRCY);
            // C
            // C MINIMIZE THE FUNCTION
            // C
            this._lmqn.Run(ref IERROR, N, ref X, offset_x, ref F, ref G, offset_g, ref W, offset_w
                           , LW, SFUN, MSGLVL, MAXIT, MAXFUN, ETA
                           , STEPMX, ACCRCY, XTOL);
            // C
            // C PRINT THE RESULTS
            // C
            if (IERROR != 0) ;//ERROR-ERRORWRITE(*,800)IERROR
            //ERROR-ERROR      WRITE(*,810) F;
            if (MSGLVL < 1) return;
            //ERROR-ERROR      WRITE(*,820);
            NMAX = 10;
            if (N < NMAX) NMAX = N;
            //ERROR-ERROR      WRITE(*,830) (I,X(I),I=1,NMAX);
            return;
            #endregion
        }
    }

    #endregion


    #region The Class: TNBC
    
    // C
    // C
    public class TNBC
    {
    
        #region Dependencies
        
        MCHPR1 _mchpr1; LMQNBC _lmqnbc; 
        #endregion
        public TNBC(MCHPR1 mchpr1, LMQNBC lmqnbc)
        {
    
            #region Set Dependencies
            
            this._mchpr1 = mchpr1; this._lmqnbc = lmqnbc; 
            #endregion
        }
    
        public TNBC()
        {
    
            #region Initialization Common Blocks
            
            CommonBlock SUBSCR = new CommonBlock(0, 15, 0, 0);
            #endregion
            #region Dependencies (Initialization)
            
            MCHPR1 mchpr1 = new MCHPR1();
            DDOT ddot = new DDOT();
            DNRM2 dnrm2 = new DNRM2();
            CRASH crash = new CRASH();
            ZTIME ztime = new ZTIME();
            MONIT monit = new MONIT();
            DCOPY dcopy = new DCOPY();
            SSBFGS ssbfgs = new SSBFGS();
            DAXPY daxpy = new DAXPY();
            NEGVEC negvec = new NEGVEC();
            STPMAX stpmax = new STPMAX();
            GETPTC getptc = new GETPTC();
            LSOUT lsout = new LSOUT();
            MODZ modz = new MODZ();
            CNVTST cnvtst = new CNVTST();
            DXPY dxpy = new DXPY();
            SETPAR setpar = new SETPAR(SUBSCR);
            GTIMS gtims = new GTIMS(SUBSCR);
            STEP1 step1 = new STEP1(mchpr1);
            CHKUCP chkucp = new CHKUCP(mchpr1, dnrm2);
            SETUCR setucr = new SETUCR(ddot);
            INITP3 initp3 = new INITP3(ddot, dcopy);
            INITPC initpc = new INITPC(initp3, SUBSCR);
            MSLV mslv = new MSLV(ddot, ssbfgs);
            MSOLVE msolve = new MSOLVE(mslv, SUBSCR);
            NDIA3 ndia3 = new NDIA3(ddot);
            MODLNP modlnp = new MODLNP(ddot, initpc, ztime, msolve, gtims, ndia3, daxpy, negvec, dcopy);
            LINDER linder = new LINDER(ddot, getptc, lsout, dcopy);
            LMQNBC lmqnbc = new LMQNBC(ddot, dnrm2, step1, crash, setpar, chkucp, setucr, ztime, monit, modlnp
                                       , dcopy, stpmax, linder, modz, cnvtst, dxpy, SUBSCR);
            #endregion
            #region Set Dependencies
            
            this._mchpr1 = mchpr1; this._lmqnbc = lmqnbc; 
            #endregion
        }
        /// <param name="IERROR">
        /// - (INTEGER) ERROR CODE
        /// ( 0 =.GT. NORMAL RETURN
        /// ( 2 =.GT. MORE THAN MAXFUN EVALUATIONS
        /// ( 3 =.GT. LINE SEARCH FAILED TO FIND LOWER
        /// (          POINT (MAY NOT BE SERIOUS)
        /// (-1 =.GT. ERROR IN INPUT PARAMETERS
        ///</param>
        /// <param name="N">
        /// - (INTEGER) NUMBER OF VARIABLES
        ///</param>
        /// <param name="X">
        /// - (REAL*8) VECTOR OF LENGTH AT LEAST N; ON INPUT, AN INITIAL
        /// ESTIMATE OF THE SOLUTION; ON OUTPUT, THE COMPUTED SOLUTION.
        ///</param>
        /// <param name="F">
        /// - (REAL*8) ON INPUT, A ROUGH ESTIMATE OF THE VALUE OF THE
        /// OBJECTIVE FUNCTION AT THE SOLUTION; ON OUTPUT, THE VALUE
        /// OF THE OBJECTIVE FUNCTION AT THE SOLUTION
        ///</param>
        /// <param name="G">
        /// - (REAL*8) VECTOR OF LENGTH AT LEAST N; ON OUTPUT, THE FINAL
        /// VALUE OF THE GRADIENT
        ///</param>
        /// <param name="W">
        /// - (REAL*8) WORK VECTOR OF LENGTH AT LEAST 14*N
        ///</param>
        /// <param name="LW">
        /// - (INTEGER) THE DECLARED DIMENSION OF W
        ///</param>
        /// <param name="SFUN">
        /// - A USER-SPECIFIED SUBROUTINE THAT COMPUTES THE FUNCTION
        /// AND GRADIENT OF THE OBJECTIVE FUNCTION.  IT MUST HAVE
        /// THE CALLING SEQUENCE
        /// SUBROUTINE SFUN (N, X, F, G)
        /// INTEGER           N
        /// DOUBLE PRECISION  X(N), G(N), F
        ///</param>
        /// <param name="IPIVOT">
        /// - (INTEGER) WORK VECTOR OF LENGTH AT LEAST N, USED
        /// TO RECORD WHICH VARIABLES ARE AT THEIR BOUNDS.
        ///</param>
        public void Run(ref int IERROR, int N, ref double[] X, int offset_x, ref double F, ref double[] G, int offset_g, ref double[] W, int offset_w
                         , int LW, ISFUN SFUN, double[] LOW, int offset_low, double[] UP, int offset_up, ref int[] IPIVOT, int offset_ipivot)
        {
            #region Variables
            
            double ETA = 0; double ACCRCY = 0; double XTOL = 0; double STEPMX = 0; double DSQRT = 0; 
            #endregion
            #region Implicit Variables
            
            int MAXIT = 0; int MSGLVL = 0; int MAXFUN = 0; int NMAX = 0; 
            #endregion
            #region Array Index Correction
            
             int o_x = -1 + offset_x;  int o_g = -1 + offset_g;  int o_w = -1 + offset_w;  int o_low = -1 + offset_low; 
             int o_up = -1 + offset_up; int o_ipivot = -1 + offset_ipivot; 
            #endregion
            #region Prolog
            
            // C
            // C THIS ROUTINE SOLVES THE OPTIMIZATION PROBLEM
            // C
            // C   MINIMIZE     F(X)
            // C      X
            // C   SUBJECT TO   LOW <= X <= UP
            // C
            // C WHERE X IS A VECTOR OF N REAL VARIABLES.  THE METHOD USED IS
            // C A TRUNCATED-NEWTON ALGORITHM (SEE "NEWTON-TYPE MINIMIZATION VIA
            // C THE LANCZOS ALGORITHM" BY S.G. NASH (TECHNICAL REPORT 378, MATH.
            // C THE LANCZOS METHOD" BY S.G. NASH (SIAM J. NUMER. ANAL. 21 (1984),
            // C PP. 770-778).  THIS ALGORITHM FINDS A LOCAL MINIMUM OF F(X).  IT DOES
            // C NOT ASSUME THAT THE FUNCTION F IS CONVEX (AND SO CANNOT GUARANTEE A
            // C GLOBAL SOLUTION), BUT DOES ASSUME THAT THE FUNCTION IS BOUNDED BELOW.
            // C IT CAN SOLVE PROBLEMS HAVING ANY NUMBER OF VARIABLES, BUT IT IS
            // C ESPECIALLY USEFUL WHEN THE NUMBER OF VARIABLES (N) IS LARGE.
            // C
            // C SUBROUTINE PARAMETERS:
            // C
            // C IERROR  - (INTEGER) ERROR CODE
            // C           ( 0 => NORMAL RETURN
            // C           ( 2 => MORE THAN MAXFUN EVALUATIONS
            // C           ( 3 => LINE SEARCH FAILED TO FIND LOWER
            // C           (          POINT (MAY NOT BE SERIOUS)
            // C           (-1 => ERROR IN INPUT PARAMETERS
            // C N       - (INTEGER) NUMBER OF VARIABLES
            // C X       - (REAL*8) VECTOR OF LENGTH AT LEAST N; ON INPUT, AN INITIAL
            // C           ESTIMATE OF THE SOLUTION; ON OUTPUT, THE COMPUTED SOLUTION.
            // C G       - (REAL*8) VECTOR OF LENGTH AT LEAST N; ON OUTPUT, THE FINAL
            // C           VALUE OF THE GRADIENT
            // C F       - (REAL*8) ON INPUT, A ROUGH ESTIMATE OF THE VALUE OF THE
            // C           OBJECTIVE FUNCTION AT THE SOLUTION; ON OUTPUT, THE VALUE
            // C           OF THE OBJECTIVE FUNCTION AT THE SOLUTION
            // C W       - (REAL*8) WORK VECTOR OF LENGTH AT LEAST 14*N
            // C LW      - (INTEGER) THE DECLARED DIMENSION OF W
            // C SFUN    - A USER-SPECIFIED SUBROUTINE THAT COMPUTES THE FUNCTION
            // C           AND GRADIENT OF THE OBJECTIVE FUNCTION.  IT MUST HAVE
            // C           THE CALLING SEQUENCE
            // C             SUBROUTINE SFUN (N, X, F, G)
            // C             INTEGER           N
            // C             DOUBLE PRECISION  X(N), G(N), F
            // C LOW, UP - (REAL*8) VECTORS OF LENGTH AT LEAST N CONTAINING
            // C           THE LOWER AND UPPER BOUNDS ON THE VARIABLES.  IF
            // C           THERE ARE NO BOUNDS ON A PARTICULAR VARIABLE, SET
            // C           THE BOUNDS TO -1.D38 AND 1.D38, RESPECTIVELY.
            // C IPIVOT  - (INTEGER) WORK VECTOR OF LENGTH AT LEAST N, USED
            // C           TO RECORD WHICH VARIABLES ARE AT THEIR BOUNDS.
            // C
            // C THIS IS AN EASY-TO-USE DRIVER FOR THE MAIN OPTIMIZATION ROUTINE
            // C LMQNBC.  MORE EXPERIENCED USERS WHO WISH TO CUSTOMIZE PERFORMANCE
            // C OF THIS ALGORITHM SHOULD CALL LMQBC DIRECTLY.
            // C
            // C----------------------------------------------------------------------
            // C THIS ROUTINE SETS UP ALL THE PARAMETERS FOR THE TRUNCATED-NEWTON
            // C ALGORITHM.  THE PARAMETERS ARE:
            // C
            // C ETA    - SEVERITY OF THE LINESEARCH
            // C MAXFUN - MAXIMUM ALLOWABLE NUMBER OF FUNCTION EVALUATIONS
            // C XTOL   - DESIRED ACCURACY FOR THE SOLUTION X*
            // C STEPMX - MAXIMUM ALLOWABLE STEP IN THE LINESEARCH
            // C ACCRCY - ACCURACY OF COMPUTED FUNCTION VALUES
            // C MSGLVL - CONTROLS QUANTITY OF PRINTED OUTPUT
            // C          0 = NONE, 1 = ONE LINE PER MAJOR ITERATION.
            // C MAXIT  - MAXIMUM NUMBER OF INNER ITERATIONS PER STEP
            // C
            // C
            // C SET PARAMETERS FOR THE OPTIMIZATION ROUTINE
            // C
            #endregion
            #region Body
            
            MAXIT = N / 2;
            if (MAXIT > 50) MAXIT = 50;
            if (MAXIT <= 0) MAXIT = 1;
            MSGLVL = 1;
            MAXFUN = 150 * N;
            ETA = .25E0;
            STEPMX = 1.0E1;
            ACCRCY = 1.0E2 * this._mchpr1.Run();
            XTOL = Math.Sqrt(ACCRCY);
            // C
            // C MINIMIZE FUNCTION
            // C
            this._lmqnbc.Run(ref IERROR, N, ref X, offset_x, ref F, ref G, offset_g, ref W, offset_w
                             , LW, SFUN, LOW, offset_low, UP, offset_up, ref IPIVOT, offset_ipivot, MSGLVL
                             , MAXIT, MAXFUN, ETA, STEPMX, ACCRCY, XTOL);
            // C
            // C PRINT RESULTS
            // C
            if (IERROR != 0) ;//ERROR-ERRORWRITE(*,800)IERROR
            //ERROR-ERROR      WRITE(*,810) F;
            if (MSGLVL < 1) return;
            //ERROR-ERROR      WRITE(*,820);
            NMAX = 10;
            if (N < NMAX) NMAX = N;
            //ERROR-ERROR      WRITE(*,830) (I,X(I),I=1,NMAX);
            return;
            #endregion
        }
    }

    #endregion


    #region The Class: LMQN
    
    // C
    // C
    public class LMQN
    {
    
        #region Dependencies
        
        DNRM2 _dnrm2; STEP1 _step1; DDOT _ddot; SETPAR _setpar; CHKUCP _chkucp; SETUCR _setucr; MODLNP _modlnp; DCOPY _dcopy; 
        LINDER _linder;DXPY _dxpy; 
        #endregion
        #region Common variables
        
        #region Common Block: SUBSCR Declaration
        
        CommonBlock _subscr;
        Oint LGV; Oint LZ1; Oint LZK; Oint LV; Oint LSK; Oint LYK; Oint LDIAGB; Oint LSR; Oint LYR; Oint LOLDG; Oint LHG; 
        Oint LHYK;Oint LPK; Oint LEMAT; Oint LWTEST; 
        #endregion
        #endregion
        #region Variables
        
        int[] IPIVOT = new int[1]; 
        #endregion
        public LMQN(DNRM2 dnrm2, STEP1 step1, DDOT ddot, SETPAR setpar, CHKUCP chkucp, SETUCR setucr, MODLNP modlnp, DCOPY dcopy, LINDER linder, DXPY dxpy
                    , CommonBlock SUBSCR)
        {
    
            #region Set Dependencies
            
            this._dnrm2 = dnrm2; this._step1 = step1; this._ddot = ddot; this._setpar = setpar; this._chkucp = chkucp; 
            this._setucr = setucr;this._modlnp = modlnp; this._dcopy = dcopy; this._linder = linder; this._dxpy = dxpy; 
            #endregion
            #region Common varaible Initialization
            
            #region Common Block: SUBSCR Initialization
            
            this._subscr = SUBSCR;
            LGV = SUBSCR.intData[0];
            LZ1 = SUBSCR.intData[1];
            LZK = SUBSCR.intData[2];
            LV = SUBSCR.intData[3];
            LSK = SUBSCR.intData[4];
            LYK = SUBSCR.intData[5];
            LDIAGB = SUBSCR.intData[6];
            LSR = SUBSCR.intData[7];
            LYR = SUBSCR.intData[8];
            LOLDG = SUBSCR.intData[9];
            LHG = SUBSCR.intData[10];
            LHYK = SUBSCR.intData[11];
            LPK = SUBSCR.intData[12];
            LEMAT = SUBSCR.intData[13];
            LWTEST = SUBSCR.intData[14];
            #endregion
            #endregion
        }
    
        public LMQN()
        {
    
            #region Initialization Common Blocks
            
            CommonBlock SUBSCR = new CommonBlock(0, 15, 0, 0);
            #endregion
            #region Dependencies (Initialization)
            
            DNRM2 dnrm2 = new DNRM2();
            MCHPR1 mchpr1 = new MCHPR1();
            DDOT ddot = new DDOT();
            DCOPY dcopy = new DCOPY();
            ZTIME ztime = new ZTIME();
            SSBFGS ssbfgs = new SSBFGS();
            DAXPY daxpy = new DAXPY();
            NEGVEC negvec = new NEGVEC();
            GETPTC getptc = new GETPTC();
            LSOUT lsout = new LSOUT();
            DXPY dxpy = new DXPY();
            SETPAR setpar = new SETPAR(SUBSCR);
            GTIMS gtims = new GTIMS(SUBSCR);
            STEP1 step1 = new STEP1(mchpr1);
            CHKUCP chkucp = new CHKUCP(mchpr1, dnrm2);
            SETUCR setucr = new SETUCR(ddot);
            INITP3 initp3 = new INITP3(ddot, dcopy);
            INITPC initpc = new INITPC(initp3, SUBSCR);
            MSLV mslv = new MSLV(ddot, ssbfgs);
            MSOLVE msolve = new MSOLVE(mslv, SUBSCR);
            NDIA3 ndia3 = new NDIA3(ddot);
            MODLNP modlnp = new MODLNP(ddot, initpc, ztime, msolve, gtims, ndia3, daxpy, negvec, dcopy);
            LINDER linder = new LINDER(ddot, getptc, lsout, dcopy);
            #endregion
            #region Set Dependencies
            
            this._dnrm2 = dnrm2; this._step1 = step1; this._ddot = ddot; this._setpar = setpar; this._chkucp = chkucp; 
            this._setucr = setucr;this._modlnp = modlnp; this._dcopy = dcopy; this._linder = linder; this._dxpy = dxpy; 
            #endregion
            #region Common varaible Initialization
            
            #region Common Block: SUBSCR Initialization
            
            this._subscr = SUBSCR;
            LGV = SUBSCR.intData[0];
            LZ1 = SUBSCR.intData[1];
            LZK = SUBSCR.intData[2];
            LV = SUBSCR.intData[3];
            LSK = SUBSCR.intData[4];
            LYK = SUBSCR.intData[5];
            LDIAGB = SUBSCR.intData[6];
            LSR = SUBSCR.intData[7];
            LYR = SUBSCR.intData[8];
            LOLDG = SUBSCR.intData[9];
            LHG = SUBSCR.intData[10];
            LHYK = SUBSCR.intData[11];
            LPK = SUBSCR.intData[12];
            LEMAT = SUBSCR.intData[13];
            LWTEST = SUBSCR.intData[14];
            #endregion
            #endregion
        }
        public void Run(ref int IFAIL, int N, ref double[] X, int offset_x, ref double F, ref double[] G, int offset_g, ref double[] W, int offset_w
                         , int LW, ISFUN SFUN, int MSGLVL, int MAXIT, int MAXFUN, double ETA
                         , double STEPMX, double ACCRCY, double XTOL)
        {
            #region Variables
            
            int offset_ipivot = 0; int o_ipivot = -1; int I = 0; int ICYCLE = 0; int IOLDG = 0; int IPK = 0; int IYK = 0; 
            int NFTOTL = 0;int NITER = 0; int NM1 = 0; int NUMF = 0; int NWHY = 0; double ABSTOL = 0; double ALPHA = 0; 
            double DIFNEW = 0;double DIFOLD = 0; double EPSMCH = 0; double EPSRED = 0; double FKEEP = 0; double FM = 0; 
            double FNEW = 0;double FOLD = 0; double FSTOP = 0; double FTEST = 0; double GNORM = 0; double GSK = 0; double GTG = 0; 
            double GTPNEW = 0;double OLDF = 0; double OLDGTP = 0; double ONE = 0; double PE = 0; double PEPS = 0; 
            double PNORM = 0;double RELTOL = 0; double RTEPS = 0; double RTLEPS = 0; double RTOL = 0; double RTOLSQ = 0; 
            double SMALL = 0;double SPE = 0; double TINY = 0; double TNYTOL = 0; double TOLEPS = 0; double XNORM = 0; 
            double YKSK = 0;double YRSR = 0; double ZERO = 0; bool LRESET = false; bool UPD1 = false; double DABS = 0; 
            double DSQRT = 0;
            #endregion
            #region Implicit Variables
            
            int IRESET = 0; int NFEVAL = 0; int NMODIF = 0; int NLINCG = 0; int LHYR = 0; int IDIAGB = 0; int MODET = 0; 
            int ISK = 0;
            #endregion
            #region Array Index Correction
            
             int o_x = -1 + offset_x;  int o_g = -1 + offset_g;  int o_w = -1 + offset_w; 
            #endregion
            #region Prolog
            
            
            
            // C     ********Mi Modificacio EN LMQN*******	
            // C
            // C     Se define IPIVOT(1) pues tal y como esta la subrutina pasa un valor sin especificar 
            
            
            
            // C
            // C	********************
            
            
            
            
            // C
            // C THIS ROUTINE IS A TRUNCATED-NEWTON METHOD.
            // C THE TRUNCATED-NEWTON METHOD IS PRECONDITIONED BY A LIMITED-MEMORY
            // C QUASI-NEWTON METHOD (THIS PRECONDITIONING STRATEGY IS DEVELOPED
            // C IN THIS ROUTINE) WITH A FURTHER DIAGONAL SCALING (SEE ROUTINE NDIA3).
            // C FOR FURTHER DETAILS ON THE PARAMETERS, SEE ROUTINE TN.
            // C
            // C
            // C THE FOLLOWING IMSL AND STANDARD FUNCTIONS ARE USED
            // C
            // C
            // C INITIALIZE PARAMETERS AND CONSTANTS
            // C
            #endregion
            #region Body
            
            if (MSGLVL >=  - 2) ;//ERROR-ERRORWRITE(*,800)
            this._setpar.Run(N);
            UPD1 = true;
            IRESET = 0;
            NFEVAL = 0;
            NMODIF = 0;
            NLINCG = 0;
            FSTOP = F;
            ZERO = 0.0E0;
            ONE = 1.0E0;
            NM1 = N - 1;
            // C
            // C WITHIN THIS ROUTINE THE ARRAY W(LOLDG) IS SHARED BY W(LHYR)
            // C
            LHYR = LOLDG.v;
            // C
            // C CHECK PARAMETERS AND SET CONSTANTS
            // C
            this._chkucp.Run(LWTEST.v, MAXFUN, ref NWHY, N, ref ALPHA, ref EPSMCH
                             , ETA, ref PEPS, ref RTEPS, ref RTOL, ref RTOLSQ, STEPMX
                             , ref FTEST, XTOL, ref XNORM, X, offset_x, LW, ref SMALL
                             , ref TINY, ACCRCY);
            if (NWHY < 0) goto LABEL120;
            this._setucr.Run(SMALL, ref NFTOTL, ref NITER, N, F, ref FNEW
                             , ref FM, ref GTG, ref OLDF, SFUN, ref G, offset_g, X, offset_x);
            FOLD = FNEW;
            if (MSGLVL >= 1) ;//ERROR-ERRORWRITE(*,810)NITER,NFTOTL,NLINCG,FNEW,GTG
            // C
            // C CHECK FOR SMALL GRADIENT AT THE STARTING POINT.
            // C
            FTEST = ONE + Math.Abs(FNEW);
            if (GTG < 1.0E-4 * EPSMCH * FTEST * FTEST) goto LABEL90;
            // C
            // C SET INITIAL VALUES TO OTHER PARAMETERS
            // C
            ICYCLE = NM1;
            TOLEPS = RTOL + RTEPS;
            RTLEPS = RTOLSQ + EPSMCH;
            GNORM = Math.Sqrt(GTG);
            DIFNEW = ZERO;
            EPSRED = 5.0E-2;
            FKEEP = FNEW;
            // C
            // C SET THE DIAGONAL OF THE APPROXIMATE HESSIAN TO UNITY.
            // C
            IDIAGB = LDIAGB.v;
            for (I = 1; I <= N; I++)
            {
                W[IDIAGB + o_w] = ONE;
                IDIAGB += 1;
            }
            // C
            // C ..................START OF MAIN ITERATIVE LOOP..........
            // C
            // C COMPUTE THE NEW SEARCH DIRECTION
            // C
            MODET = MSGLVL - 3;
            this._modlnp.Run(MODET, ref W, LPK.v + o_w, ref W, LGV.v + o_w, ref W, LZ1.v + o_w, ref W, LV.v + o_w, ref W, LDIAGB.v + o_w
                             , ref W, LEMAT.v + o_w, X, offset_x, G, offset_g, ref W, LZK.v + o_w, N, ref W, offset_w
                             , LW, NITER, MAXIT, ref NFEVAL, NMODIF, ref NLINCG
                             , UPD1, YKSK, ref GSK, YRSR, LRESET, SFUN
                             , false, IPIVOT, offset_ipivot, ACCRCY, ref GTPNEW, GNORM, XNORM);
        LABEL20:;
            this._dcopy.Run(N, G, offset_g, 1, ref W, LOLDG.v + o_w, 1);
            PNORM = this._dnrm2.Run(N, W, LPK.v + o_w, 1);
            OLDF = FNEW;
            OLDGTP = GTPNEW;
            // C
            // C PREPARE TO COMPUTE THE STEP LENGTH
            // C
            PE = PNORM + EPSMCH;
            // C
            // C COMPUTE THE ABSOLUTE AND RELATIVE TOLERANCES FOR THE LINEAR SEARCH
            // C
            RELTOL = RTEPS * (XNORM + ONE) / PE;
            ABSTOL =  - EPSMCH * FTEST / (OLDGTP - EPSMCH);
            // C
            // C COMPUTE THE SMALLEST ALLOWABLE SPACING BETWEEN POINTS IN
            // C THE LINEAR SEARCH
            // C
            TNYTOL = EPSMCH * (XNORM + ONE) / PE;
            SPE = STEPMX / PE;
            // C
            // C SET THE INITIAL STEP LENGTH.
            // C
            ALPHA = this._step1.Run(FNEW, FM, OLDGTP, SPE);
            // C
            // C PERFORM THE LINEAR SEARCH
            // C
            this._linder.Run(N, SFUN, SMALL, EPSMCH, ref RELTOL, ref ABSTOL
                             , TNYTOL, ETA, ZERO, SPE, W, LPK.v + o_w, OLDGTP
                             , ref X, offset_x, ref FNEW, ref ALPHA, ref G, offset_g, ref NUMF, ref NWHY
                             , ref W, offset_w, LW);
            // C
            FOLD = FNEW;
            NITER += 1;
            NFTOTL += NUMF;
            GTG = this._ddot.Run(N, G, offset_g, 1, G, offset_g, 1);
            if (MSGLVL >= 1) ;//ERROR-ERRORWRITE(*,810)NITER,NFTOTL,NLINCG,FNEW,GTG
            if (NWHY < 0) goto LABEL120;
            if (NWHY == 0 || NWHY == 2) goto LABEL30;
            // C
            // C THE LINEAR SEARCH HAS FAILED TO FIND A LOWER POINT
            // C
            NWHY = 3;
            goto LABEL100;
        LABEL30:  
            if (NWHY <= 1) goto LABEL40;
            SFUN.Run(N, X, offset_x, ref FNEW, ref G, offset_g);
            NFTOTL += 1;
            // C
            // C TERMINATE IF MORE THAN MAXFUN EVALUTATIONS HAVE BEEN MADE
            // C
        LABEL40:  NWHY = 2;
            
            
            // C     Se modificaron las siguentes lineas
            // C     ********Mi Modificacion EN LMQN y LMQNCB*******
            // C	
            // C
            // C	Se cambio IF (NFTOTL .GT. MAXFUN)  por IF (NFTOTL + NFEVAL .GT. MAXFUN)
            // C
            // C	********************
            // C      IF (NFTOTL .GT. MAXFUN) GO TO 110
            
            if (NFTOTL + NFEVAL > MAXFUN) goto LABEL110;
            
            
            
            
            
            
            NWHY = 0;
            // C
            // C SET UP PARAMETERS USED IN CONVERGENCE AND RESETTING TESTS
            // C
            DIFOLD = DIFNEW;
            DIFNEW = OLDF - FNEW;
            // C
            // C IF THIS IS THE FIRST ITERATION OF A NEW CYCLE, COMPUTE THE
            // C PERCENTAGE REDUCTION FACTOR FOR THE RESETTING TEST.
            // C
            if (ICYCLE != 1) goto LABEL50;
            if (DIFNEW > 2.0E0 * DIFOLD) EPSRED += EPSRED;
            if (DIFNEW < 5.0E-1) EPSRED = 5.0E-1;
        LABEL50:;
            GNORM = Math.Sqrt(GTG);
            FTEST = ONE + Math.Abs(FNEW);
            XNORM = this._dnrm2.Run(N, X, offset_x, 1);
            // C
            // C TEST FOR CONVERGENCE
            // C
            if ((ALPHA * PNORM < TOLEPS * (ONE + XNORM) && Math.Abs(DIFNEW) < RTLEPS * FTEST && GTG < PEPS * FTEST * FTEST) || GTG < 1.0E-4 * ACCRCY * FTEST * FTEST) goto LABEL90;
            // C
            // C COMPUTE THE CHANGE IN THE ITERATES AND THE CORRESPONDING CHANGE
            // C IN THE GRADIENTS
            // C
            ISK = LSK.v;
            IPK = LPK.v;
            IYK = LYK.v;
            IOLDG = LOLDG.v;
            for (I = 1; I <= N; I++)
            {
                W[IYK + o_w] = G[I + o_g] - W[IOLDG + o_w];
                W[ISK + o_w] = ALPHA * W[IPK + o_w];
                IPK += 1;
                ISK += 1;
                IYK += 1;
                IOLDG += 1;
            }
            // C
            // C SET UP PARAMETERS USED IN UPDATING THE DIRECTION OF SEARCH.
            // C
            YKSK = this._ddot.Run(N, W, LYK.v + o_w, 1, W, LSK.v + o_w, 1);
            LRESET = false;
            if (ICYCLE == NM1 || DIFNEW < EPSRED * (FKEEP - FNEW)) LRESET = true;
            if (LRESET) goto LABEL70;
            YRSR = this._ddot.Run(N, W, LYR.v + o_w, 1, W, LSR.v + o_w, 1);
            if (YRSR <= ZERO) LRESET = true;
        LABEL70:;
            UPD1 = false;
            // C
            // C      COMPUTE THE NEW SEARCH DIRECTION
            // C
            MODET = MSGLVL - 3;
            this._modlnp.Run(MODET, ref W, LPK.v + o_w, ref W, LGV.v + o_w, ref W, LZ1.v + o_w, ref W, LV.v + o_w, ref W, LDIAGB.v + o_w
                             , ref W, LEMAT.v + o_w, X, offset_x, G, offset_g, ref W, LZK.v + o_w, N, ref W, offset_w
                             , LW, NITER, MAXIT, ref NFEVAL, NMODIF, ref NLINCG
                             , UPD1, YKSK, ref GSK, YRSR, LRESET, SFUN
                             , false, IPIVOT, offset_ipivot, ACCRCY, ref GTPNEW, GNORM, XNORM);
            if (LRESET) goto LABEL80;
            // C
            // C      STORE THE ACCUMULATED CHANGE IN THE POINT AND GRADIENT AS AN
            // C      "AVERAGE" DIRECTION FOR PRECONDITIONING.
            // C
            this._dxpy.Run(N, W, LSK.v + o_w, 1, ref W, LSR.v + o_w, 1);
            this._dxpy.Run(N, W, LYK.v + o_w, 1, ref W, LYR.v + o_w, 1);
            ICYCLE += 1;
            goto LABEL20;
            // C
            // C RESET
            // C
        LABEL80:  IRESET += 1;
            // C
            // C INITIALIZE THE SUM OF ALL THE CHANGES IN X.
            // C
            this._dcopy.Run(N, W, LSK.v + o_w, 1, ref W, LSR.v + o_w, 1);
            this._dcopy.Run(N, W, LYK.v + o_w, 1, ref W, LYR.v + o_w, 1);
            FKEEP = FNEW;
            ICYCLE = 1;
            goto LABEL20;
            // C
            // C ...............END OF MAIN ITERATION.......................
            // C
        LABEL90:  IFAIL = 0;
            F = FNEW;
            return;
        LABEL100:  OLDF = FNEW;
            // C
            // C LOCAL SEARCH HERE COULD BE INSTALLED HERE
            // C
        LABEL110:  F = OLDF;
            // C
            // C SET IFAIL
            // C
        LABEL120:  IFAIL = NWHY;
            return;
            #endregion
        }
    }

    #endregion


    #region The Class: LMQNBC
    
    // C
    // C
    public class LMQNBC
    {
    
        #region Dependencies
        
        DDOT _ddot; DNRM2 _dnrm2; STEP1 _step1; CRASH _crash; SETPAR _setpar; CHKUCP _chkucp; SETUCR _setucr; ZTIME _ztime; 
        MONIT _monit;MODLNP _modlnp; DCOPY _dcopy; STPMAX _stpmax; LINDER _linder; MODZ _modz; CNVTST _cnvtst; DXPY _dxpy; 
        #endregion
        #region Common variables
        
        #region Common Block: SUBSCR Declaration
        
        CommonBlock _subscr;
        Oint LGV; Oint LZ1; Oint LZK; Oint LV; Oint LSK; Oint LYK; Oint LDIAGB; Oint LSR; Oint LYR; Oint LOLDG; Oint LHG; 
        Oint LHYK;Oint LPK; Oint LEMAT; Oint LWTEST; 
        #endregion
        #endregion
        public LMQNBC(DDOT ddot, DNRM2 dnrm2, STEP1 step1, CRASH crash, SETPAR setpar, CHKUCP chkucp, SETUCR setucr, ZTIME ztime, MONIT monit, MODLNP modlnp
                      , DCOPY dcopy, STPMAX stpmax, LINDER linder, MODZ modz, CNVTST cnvtst, DXPY dxpy, CommonBlock SUBSCR)
        {
    
            #region Set Dependencies
            
            this._ddot = ddot; this._dnrm2 = dnrm2; this._step1 = step1; this._crash = crash; this._setpar = setpar; 
            this._chkucp = chkucp;this._setucr = setucr; this._ztime = ztime; this._monit = monit; this._modlnp = modlnp; 
            this._dcopy = dcopy;this._stpmax = stpmax; this._linder = linder; this._modz = modz; this._cnvtst = cnvtst; 
            this._dxpy = dxpy;
            #endregion
            #region Common varaible Initialization
            
            #region Common Block: SUBSCR Initialization
            
            this._subscr = SUBSCR;
            LGV = SUBSCR.intData[0];
            LZ1 = SUBSCR.intData[1];
            LZK = SUBSCR.intData[2];
            LV = SUBSCR.intData[3];
            LSK = SUBSCR.intData[4];
            LYK = SUBSCR.intData[5];
            LDIAGB = SUBSCR.intData[6];
            LSR = SUBSCR.intData[7];
            LYR = SUBSCR.intData[8];
            LOLDG = SUBSCR.intData[9];
            LHG = SUBSCR.intData[10];
            LHYK = SUBSCR.intData[11];
            LPK = SUBSCR.intData[12];
            LEMAT = SUBSCR.intData[13];
            LWTEST = SUBSCR.intData[14];
            #endregion
            #endregion
        }
    
        public LMQNBC()
        {
    
            #region Initialization Common Blocks
            
            CommonBlock SUBSCR = new CommonBlock(0, 15, 0, 0);
            #endregion
            #region Dependencies (Initialization)
            
            DDOT ddot = new DDOT();
            DNRM2 dnrm2 = new DNRM2();
            MCHPR1 mchpr1 = new MCHPR1();
            CRASH crash = new CRASH();
            ZTIME ztime = new ZTIME();
            MONIT monit = new MONIT();
            DCOPY dcopy = new DCOPY();
            SSBFGS ssbfgs = new SSBFGS();
            DAXPY daxpy = new DAXPY();
            NEGVEC negvec = new NEGVEC();
            STPMAX stpmax = new STPMAX();
            GETPTC getptc = new GETPTC();
            LSOUT lsout = new LSOUT();
            MODZ modz = new MODZ();
            CNVTST cnvtst = new CNVTST();
            DXPY dxpy = new DXPY();
            SETPAR setpar = new SETPAR(SUBSCR);
            GTIMS gtims = new GTIMS(SUBSCR);
            STEP1 step1 = new STEP1(mchpr1);
            CHKUCP chkucp = new CHKUCP(mchpr1, dnrm2);
            SETUCR setucr = new SETUCR(ddot);
            INITP3 initp3 = new INITP3(ddot, dcopy);
            INITPC initpc = new INITPC(initp3, SUBSCR);
            MSLV mslv = new MSLV(ddot, ssbfgs);
            MSOLVE msolve = new MSOLVE(mslv, SUBSCR);
            NDIA3 ndia3 = new NDIA3(ddot);
            MODLNP modlnp = new MODLNP(ddot, initpc, ztime, msolve, gtims, ndia3, daxpy, negvec, dcopy);
            LINDER linder = new LINDER(ddot, getptc, lsout, dcopy);
            #endregion
            #region Set Dependencies
            
            this._ddot = ddot; this._dnrm2 = dnrm2; this._step1 = step1; this._crash = crash; this._setpar = setpar; 
            this._chkucp = chkucp;this._setucr = setucr; this._ztime = ztime; this._monit = monit; this._modlnp = modlnp; 
            this._dcopy = dcopy;this._stpmax = stpmax; this._linder = linder; this._modz = modz; this._cnvtst = cnvtst; 
            this._dxpy = dxpy;
            #endregion
            #region Common varaible Initialization
            
            #region Common Block: SUBSCR Initialization
            
            this._subscr = SUBSCR;
            LGV = SUBSCR.intData[0];
            LZ1 = SUBSCR.intData[1];
            LZK = SUBSCR.intData[2];
            LV = SUBSCR.intData[3];
            LSK = SUBSCR.intData[4];
            LYK = SUBSCR.intData[5];
            LDIAGB = SUBSCR.intData[6];
            LSR = SUBSCR.intData[7];
            LYR = SUBSCR.intData[8];
            LOLDG = SUBSCR.intData[9];
            LHG = SUBSCR.intData[10];
            LHYK = SUBSCR.intData[11];
            LPK = SUBSCR.intData[12];
            LEMAT = SUBSCR.intData[13];
            LWTEST = SUBSCR.intData[14];
            #endregion
            #endregion
        }
        public void Run(ref int IFAIL, int N, ref double[] X, int offset_x, ref double F, ref double[] G, int offset_g, ref double[] W, int offset_w
                         , int LW, ISFUN SFUN, double[] LOW, int offset_low, double[] UP, int offset_up, ref int[] IPIVOT, int offset_ipivot, int MSGLVL
                         , int MAXIT, int MAXFUN, double ETA, double STEPMX, double ACCRCY, double XTOL)
        {
            #region Variables
            
            int I = 0; int ICYCLE = 0; int IOLDG = 0; int IPK = 0; int IYK = 0; int NFTOTL = 0; int NITER = 0; int NM1 = 0; 
            int NUMF = 0;int NWHY = 0; double ABSTOL = 0; double ALPHA = 0; double DIFNEW = 0; double DIFOLD = 0; 
            double EPSMCH = 0;double EPSRED = 0; double FKEEP = 0; double FLAST = 0; double FM = 0; double FNEW = 0; 
            double FOLD = 0;double FSTOP = 0; double FTEST = 0; double GNORM = 0; double GSK = 0; double GTG = 0; 
            double GTPNEW = 0;double OLDF = 0; double OLDGTP = 0; double ONE = 0; double PE = 0; double PEPS = 0; 
            double PNORM = 0;double RELTOL = 0; double RTEPS = 0; double RTLEPS = 0; double RTOL = 0; double RTOLSQ = 0; 
            double SMALL = 0;double SPE = 0; double TINY = 0; double TNYTOL = 0; double TOLEPS = 0; double XNORM = 0; 
            double YKSK = 0;double YRSR = 0; double ZERO = 0; bool CONV = false; bool LRESET = false; bool UPD1 = false; 
            bool NEWCON = false;double DABS = 0; double DSQRT = 0; 
            #endregion
            #region Implicit Variables
            
            int IER = 0; int IRESET = 0; int NFEVAL = 0; int NMODIF = 0; int NLINCG = 0; int LHYR = 0; int IDIAGB = 0; 
            int MODET = 0;int ISK = 0; 
            #endregion
            #region Array Index Correction
            
             int o_x = -1 + offset_x;  int o_g = -1 + offset_g;  int o_w = -1 + offset_w;  int o_low = -1 + offset_low; 
             int o_up = -1 + offset_up; int o_ipivot = -1 + offset_ipivot; 
            #endregion
            #region Prolog
            
            // C
            // C THIS ROUTINE IS A BOUNDS-CONSTRAINED TRUNCATED-NEWTON METHOD.
            // C THE TRUNCATED-NEWTON METHOD IS PRECONDITIONED BY A LIMITED-MEMORY
            // C QUASI-NEWTON METHOD (THIS PRECONDITIONING STRATEGY IS DEVELOPED
            // C IN THIS ROUTINE) WITH A FURTHER DIAGONAL SCALING (SEE ROUTINE NDIA3).
            // C FOR FURTHER DETAILS ON THE PARAMETERS, SEE ROUTINE TNBC.
            // C
            // C
            // C THE FOLLOWING STANDARD FUNCTIONS AND SYSTEM FUNCTIONS ARE USED
            // C
            // C
            // C CHECK THAT INITIAL X IS FEASIBLE AND THAT THE BOUNDS ARE CONSISTENT
            // C
            #endregion
            #region Body
            
            this._crash.Run(N, ref X, offset_x, ref IPIVOT, offset_ipivot, LOW, offset_low, UP, offset_up, ref IER);
            if (IER != 0) ;//ERROR-ERRORWRITE(*,800)
            if (IER != 0) return;
            if (MSGLVL >= 1) ;//ERROR-ERRORWRITE(*,810)
            // C
            // C INITIALIZE VARIABLES
            // C
            this._setpar.Run(N);
            UPD1 = true;
            IRESET = 0;
            NFEVAL = 0;
            NMODIF = 0;
            NLINCG = 0;
            FSTOP = F;
            CONV = false;
            ZERO = 0.0E0;
            ONE = 1.0E0;
            NM1 = N - 1;
            // C
            // C WITHIN THIS ROUTINE THE ARRAY W(LOLDG) IS SHARED BY W(LHYR)
            // C
            LHYR = LOLDG.v;
            // C
            // C CHECK PARAMETERS AND SET CONSTANTS
            // C
            this._chkucp.Run(LWTEST.v, MAXFUN, ref NWHY, N, ref ALPHA, ref EPSMCH
                             , ETA, ref PEPS, ref RTEPS, ref RTOL, ref RTOLSQ, STEPMX
                             , ref FTEST, XTOL, ref XNORM, X, offset_x, LW, ref SMALL
                             , ref TINY, ACCRCY);
            if (NWHY < 0) goto LABEL160;
            this._setucr.Run(SMALL, ref NFTOTL, ref NITER, N, F, ref FNEW
                             , ref FM, ref GTG, ref OLDF, SFUN, ref G, offset_g, X, offset_x);
            FOLD = FNEW;
            FLAST = FNEW;
            // C
            // C TEST THE LAGRANGE MULTIPLIERS TO SEE IF THEY ARE NON-NEGATIVE.
            // C BECAUSE THE CONSTRAINTS ARE ONLY LOWER BOUNDS, THE COMPONENTS
            // C OF THE GRADIENT CORRESPONDING TO THE ACTIVE CONSTRAINTS ARE THE
            // C LAGRANGE MULTIPLIERS.  AFTERWORDS, THE PROJECTED GRADIENT IS FORMED.
            // C
            for (I = 1; I <= N; I++)
            {
                if (IPIVOT[I + o_ipivot] == 2) goto LABEL10;
                if ( - IPIVOT[I + o_ipivot] * G[I + o_g] >= 0.0E0) goto LABEL10;
                IPIVOT[I + o_ipivot] = 0;
            LABEL10:;
            }
            this._ztime.Run(N, ref G, offset_g, IPIVOT, offset_ipivot);
            GTG = this._ddot.Run(N, G, offset_g, 1, G, offset_g, 1);
            if (MSGLVL >= 1)
            {
                this._monit.Run(N, X, offset_x, FNEW, G, offset_g, NITER, NFTOTL
                    , NFEVAL, Convert.ToInt32(LRESET), IPIVOT, offset_ipivot);
            }
            // C
            // C CHECK IF THE INITIAL POINT IS A LOCAL MINIMUM.
            // C
            FTEST = ONE + Math.Abs(FNEW);
            if (GTG < 1.0E-4 * EPSMCH * FTEST * FTEST) goto LABEL130;
            // C
            // C SET INITIAL VALUES TO OTHER PARAMETERS
            // C
            ICYCLE = NM1;
            TOLEPS = RTOL + RTEPS;
            RTLEPS = RTOLSQ + EPSMCH;
            GNORM = Math.Sqrt(GTG);
            DIFNEW = ZERO;
            EPSRED = 5.0E-2;
            FKEEP = FNEW;
            // C
            // C SET THE DIAGONAL OF THE APPROXIMATE HESSIAN TO UNITY.
            // C
            IDIAGB = LDIAGB.v;
            for (I = 1; I <= N; I++)
            {
                W[IDIAGB + o_w] = ONE;
                IDIAGB += 1;
            }
            // C
            // C ..................START OF MAIN ITERATIVE LOOP..........
            // C
            // C COMPUTE THE NEW SEARCH DIRECTION
            // C
            MODET = MSGLVL - 3;
            this._modlnp.Run(MODET, ref W, LPK.v + o_w, ref W, LGV.v + o_w, ref W, LZ1.v + o_w, ref W, LV.v + o_w, ref W, LDIAGB.v + o_w
                             , ref W, LEMAT.v + o_w, X, offset_x, G, offset_g, ref W, LZK.v + o_w, N, ref W, offset_w
                             , LW, NITER, MAXIT, ref NFEVAL, NMODIF, ref NLINCG
                             , UPD1, YKSK, ref GSK, YRSR, LRESET, SFUN
                             , true, IPIVOT, offset_ipivot, ACCRCY, ref GTPNEW, GNORM, XNORM);
        LABEL20:;
            this._dcopy.Run(N, G, offset_g, 1, ref W, LOLDG.v + o_w, 1);
            PNORM = this._dnrm2.Run(N, W, LPK.v + o_w, 1);
            OLDF = FNEW;
            OLDGTP = GTPNEW;
            // C
            // C PREPARE TO COMPUTE THE STEP LENGTH
            // C
            PE = PNORM + EPSMCH;
            // C
            // C COMPUTE THE ABSOLUTE AND RELATIVE TOLERANCES FOR THE LINEAR SEARCH
            // C
            RELTOL = RTEPS * (XNORM + ONE) / PE;
            ABSTOL =  - EPSMCH * FTEST / (OLDGTP - EPSMCH);
            // C
            // C COMPUTE THE SMALLEST ALLOWABLE SPACING BETWEEN POINTS IN
            // C THE LINEAR SEARCH
            // C
            TNYTOL = EPSMCH * (XNORM + ONE) / PE;
            this._stpmax.Run(STEPMX, PE, ref SPE, N, X, offset_x, W, LPK.v + o_w
                             , IPIVOT, offset_ipivot, LOW, offset_low, UP, offset_up);
            // C
            // C SET THE INITIAL STEP LENGTH.
            // C
            ALPHA = this._step1.Run(FNEW, FM, OLDGTP, SPE);
            // C
            // C PERFORM THE LINEAR SEARCH
            // C
            this._linder.Run(N, SFUN, SMALL, EPSMCH, ref RELTOL, ref ABSTOL
                             , TNYTOL, ETA, ZERO, SPE, W, LPK.v + o_w, OLDGTP
                             , ref X, offset_x, ref FNEW, ref ALPHA, ref G, offset_g, ref NUMF, ref NWHY
                             , ref W, offset_w, LW);
            NEWCON = false;
            if (Math.Abs(ALPHA - SPE) > 1.0E1 * EPSMCH) goto LABEL30;
            NEWCON = true;
            NWHY = 0;
            this._modz.Run(N, ref X, offset_x, W, LPK.v + o_w, ref IPIVOT, offset_ipivot, EPSMCH, LOW, offset_low
                           , UP, offset_up, ref FLAST, FNEW);
            FLAST = FNEW;
            // C
        LABEL30:  
            if (MSGLVL >= 3) ;//ERROR-ERRORWRITE(*,820)ALPHA,PNORM
            FOLD = FNEW;
            NITER += 1;
            NFTOTL += NUMF;
            // C
            // C IF REQUIRED, PRINT THE DETAILS OF THIS ITERATION
            // C
            if (MSGLVL >= 1)
            {
                this._monit.Run(N, X, offset_x, FNEW, G, offset_g, NITER, NFTOTL
                                , NFEVAL, Convert.ToInt32(LRESET), IPIVOT, offset_ipivot);
            }
            if (NWHY < 0) goto LABEL160;
            if (NWHY == 0 || NWHY == 2) goto LABEL40;
            // C
            // C THE LINEAR SEARCH HAS FAILED TO FIND A LOWER POINT
            // C
            NWHY = 3;
            goto LABEL140;
        LABEL40:  
            if (NWHY <= 1) goto LABEL50;
            SFUN.Run(N, X, offset_x, ref FNEW, ref G, offset_g);
            NFTOTL += 1;
            // C
            // C TERMINATE IF MORE THAN MAXFUN EVALUATIONS HAVE BEEN MADE
            // C
        LABEL50:  NWHY = 2;                        
            if (NFTOTL + NFEVAL > MAXFUN) goto LABEL150;
            NWHY = 0;
            // C
            // C SET UP PARAMETERS USED IN CONVERGENCE AND RESETTING TESTS
            // C
            DIFOLD = DIFNEW;
            DIFNEW = OLDF - FNEW;
            // C
            // C IF THIS IS THE FIRST ITERATION OF A NEW CYCLE, COMPUTE THE
            // C PERCENTAGE REDUCTION FACTOR FOR THE RESETTING TEST.
            // C
            if (ICYCLE != 1) goto LABEL60;
            if (DIFNEW > 2.0E0 * DIFOLD) EPSRED += EPSRED;
            if (DIFNEW < 5.0E-1) EPSRED = 5.0E-1;
        LABEL60:  this._dcopy.Run(N, G, offset_g, 1, ref W, LGV.v + o_w, 1);
            this._ztime.Run(N, ref W, LGV.v + o_w, IPIVOT, offset_ipivot);
            GTG = this._ddot.Run(N, W, LGV.v + o_w, 1, W, LGV.v + o_w, 1);
            GNORM = Math.Sqrt(GTG);
            FTEST = ONE + Math.Abs(FNEW);
            XNORM = this._dnrm2.Run(N, X, offset_x, 1);
            // C
            // C TEST FOR CONVERGENCE
            // C
            this._cnvtst.Run(ref CONV, ALPHA, PNORM, TOLEPS, XNORM, DIFNEW
                             , RTLEPS, FTEST, GTG, PEPS, EPSMCH, GTPNEW
                             , FNEW, ref FLAST, G, offset_g, ref IPIVOT, offset_ipivot, N, ACCRCY);
            if (CONV) goto LABEL130;
            this._ztime.Run(N, ref G, offset_g, IPIVOT, offset_ipivot);
            // C
            // C COMPUTE THE CHANGE IN THE ITERATES AND THE CORRESPONDING CHANGE
            // C IN THE GRADIENTS
            // C
            if (NEWCON) goto LABEL90;
            ISK = LSK.v;
            IPK = LPK.v;
            IYK = LYK.v;
            IOLDG = LOLDG.v;
            for (I = 1; I <= N; I++)
            {
                W[IYK + o_w] = G[I + o_g] - W[IOLDG + o_w];
                W[ISK + o_w] = ALPHA * W[IPK + o_w];
                IPK += 1;
                ISK += 1;
                IYK += 1;
                IOLDG += 1;
            }
            // C
            // C SET UP PARAMETERS USED IN UPDATING THE PRECONDITIONING STRATEGY.
            // C
            YKSK = this._ddot.Run(N, W, LYK.v + o_w, 1, W, LSK.v + o_w, 1);
            LRESET = false;
            if (ICYCLE == NM1 || DIFNEW < EPSRED * (FKEEP - FNEW)) LRESET = true;
            if (LRESET) goto LABEL80;
            YRSR = this._ddot.Run(N, W, LYR.v + o_w, 1, W, LSR.v + o_w, 1);
            if (YRSR <= ZERO) LRESET = true;
        LABEL80:;
            UPD1 = false;
            // C
            // C      COMPUTE THE NEW SEARCH DIRECTION
            // C
        LABEL90:  
            if (UPD1 && MSGLVL >= 3) ;//ERROR-ERRORWRITE(*,830)
            if (NEWCON && MSGLVL >= 3) ;//ERROR-ERRORWRITE(*,840)
            MODET = MSGLVL - 3;
            this._modlnp.Run(MODET, ref W, LPK.v + o_w, ref W, LGV.v + o_w, ref W, LZ1.v + o_w, ref W, LV.v + o_w, ref W, LDIAGB.v + o_w
                             , ref W, LEMAT.v + o_w, X, offset_x, G, offset_g, ref W, LZK.v + o_w, N, ref W, offset_w
                             , LW, NITER, MAXIT, ref NFEVAL, NMODIF, ref NLINCG
                             , UPD1, YKSK, ref GSK, YRSR, LRESET, SFUN
                             , true, IPIVOT, offset_ipivot, ACCRCY, ref GTPNEW, GNORM, XNORM);
            if (NEWCON) goto LABEL20;
            if (LRESET) goto LABEL110;
            // C
            // C COMPUTE THE ACCUMULATED STEP AND ITS CORRESPONDING
            // C GRADIENT DIFFERENCE.
            // C
            this._dxpy.Run(N, W, LSK.v + o_w, 1, ref W, LSR.v + o_w, 1);
            this._dxpy.Run(N, W, LYK.v + o_w, 1, ref W, LYR.v + o_w, 1);
            ICYCLE += 1;
            goto LABEL20;
            // C
            // C RESET
            // C
        LABEL110:  IRESET += 1;
            // C
            // C INITIALIZE THE SUM OF ALL THE CHANGES IN X.
            // C
            this._dcopy.Run(N, W, LSK.v + o_w, 1, ref W, LSR.v + o_w, 1);
            this._dcopy.Run(N, W, LYK.v + o_w, 1, ref W, LYR.v + o_w, 1);
            FKEEP = FNEW;
            ICYCLE = 1;
            goto LABEL20;
            // C
            // C ...............END OF MAIN ITERATION.......................
            // C
        LABEL130:  IFAIL = 0;
            F = FNEW;
            return;
        LABEL140:  OLDF = FNEW;
            // C
            // C LOCAL SEARCH COULD BE INSTALLED HERE
            // C
        LABEL150:  F = OLDF;
            if (MSGLVL >= 1)
            {
                this._monit.Run(N, X, offset_x, F, G, offset_g, NITER, NFTOTL
                                , NFEVAL, IRESET, IPIVOT, offset_ipivot);
            }
            // C
            // C SET IFAIL
            // C
        LABEL160:  IFAIL = NWHY;
            return;
            #endregion
        }
    }

    #endregion


    #region The Class: MONIT
    
    // C
    // C
    public class MONIT
    {
    
        public MONIT()
        {
    
        }
    
        public void Run(int N, double[] X, int offset_x, double F, double[] G, int offset_g, int NITER, int NFTOTL
                         , int NFEVAL, int IRESET, int[] IPIVOT, int offset_ipivot)
        {
            #region Variables
            
            double GTG = 0; 
            #endregion
            #region Implicit Variables
            
            int I = 0; 
            #endregion
            #region Array Index Correction
            
             int o_x = -1 + offset_x;  int o_g = -1 + offset_g;  int o_ipivot = -1 + offset_ipivot; 
            #endregion
            // C
            // C PRINT RESULTS OF CURRENT ITERATION
            // C
            // C
            GTG = 0.0E0;
            for (I = 1; I <= N; I++)
            {
                if (IPIVOT[I + o_ipivot] != 0) goto LABEL10;
                GTG += G[I + o_g] * G[I + o_g];
            LABEL10:;
            }
            //ERROR-ERROR      WRITE(*,800) NITER,NFTOTL,NFEVAL,F,GTG;
            return;
        }
    }

    #endregion


    #region The Class: ZTIME
    
    // C
    // C
    public class ZTIME
    {
    
        public ZTIME()
        {
    
        }
    
        public void Run(int N, ref double[] X, int offset_x, int[] IPIVOT, int offset_ipivot)
        {
            #region Implicit Variables
            
            int I = 0; 
            #endregion
            #region Array Index Correction
            
             int o_x = -1 + offset_x;  int o_ipivot = -1 + offset_ipivot; 
            #endregion
            // C
            // C THIS ROUTINE MULTIPLIES THE VECTOR X BY THE CONSTRAINT MATRIX Z
            // C
            for (I = 1; I <= N; I++)
            {
                if (IPIVOT[I + o_ipivot] != 0) X[I + o_x] = 0.0E0;
            }
            return;
        }
    }

    #endregion


    #region The Class: STPMAX
    
    // C
    // C
    public class STPMAX
    {
    
        public STPMAX()
        {
    
        }
    
        public void Run(double STEPMX, double PE, ref double SPE, int N, double[] X, int offset_x, double[] P, int offset_p
                         , int[] IPIVOT, int offset_ipivot, double[] LOW, int offset_low, double[] UP, int offset_up)
        {
            #region Variables
            
            double T = 0; 
            #endregion
            #region Implicit Variables
            
            int I = 0; 
            #endregion
            #region Array Index Correction
            
             int o_x = -1 + offset_x;  int o_p = -1 + offset_p;  int o_ipivot = -1 + offset_ipivot;  int o_low = -1 + offset_low; 
             int o_up = -1 + offset_up;
            #endregion
            // C
            // C COMPUTE THE MAXIMUM ALLOWABLE STEP LENGTH
            // C
            SPE = STEPMX / PE;
            // C SPE IS THE STANDARD (UNCONSTRAINED) MAX STEP
            for (I = 1; I <= N; I++)
            {
                if (IPIVOT[I + o_ipivot] != 0) goto LABEL10;
                if (P[I + o_p] == 0.0E0) goto LABEL10;
                if (P[I + o_p] > 0.0E0) goto LABEL5;
                T = LOW[I + o_low] - X[I + o_x];
                if (T > SPE * P[I + o_p]) SPE = T / P[I + o_p];
                goto LABEL10;
            LABEL5:  T = UP[I + o_up] - X[I + o_x];
                if (T < SPE * P[I + o_p]) SPE = T / P[I + o_p];
            LABEL10:;
            }
            return;
        }
    }

    #endregion


    #region The Class: MODZ
    
    // C
    // C
    public class MODZ
    {
    
        public MODZ()
        {
    
        }
    
        public void Run(int N, ref double[] X, int offset_x, double[] P, int offset_p, ref int[] IPIVOT, int offset_ipivot, double EPSMCH, double[] LOW, int offset_low
                         , double[] UP, int offset_up, ref double FLAST, double FNEW)
        {
            #region Variables
            
            double DABS = 0; double TOL = 0; 
            #endregion
            #region Implicit Variables
            
            int I = 0; 
            #endregion
            #region Array Index Correction
            
             int o_x = -1 + offset_x;  int o_p = -1 + offset_p;  int o_ipivot = -1 + offset_ipivot;  int o_low = -1 + offset_low; 
             int o_up = -1 + offset_up;
            #endregion
            // C
            // C UPDATE THE CONSTRAINT MATRIX IF A NEW CONSTRAINT IS ENCOUNTERED
            // C
            for (I = 1; I <= N; I++)
            {
                if (IPIVOT[I + o_ipivot] != 0) goto LABEL10;
                if (P[I + o_p] == 0.0E0) goto LABEL10;
                if (P[I + o_p] > 0.0E0) goto LABEL5;
                TOL = 1.0E1 * EPSMCH * (Math.Abs(LOW[I + o_low]) + 1.0E0);
                if (X[I + o_x] - LOW[I + o_low] > TOL) goto LABEL10;
                FLAST = FNEW;
                IPIVOT[I + o_ipivot] =  - 1;
                X[I + o_x] = LOW[I + o_low];
                goto LABEL10;
            LABEL5:  TOL = 1.0E1 * EPSMCH * (Math.Abs(UP[I + o_up]) + 1.0E0);
                if (UP[I + o_up] - X[I + o_x] > TOL) goto LABEL10;
                FLAST = FNEW;
                IPIVOT[I + o_ipivot] = 1;
                X[I + o_x] = UP[I + o_up];
            LABEL10:;
            }
            return;
        }
    }

    #endregion


    #region The Class: CNVTST
    
    // C
    // C
    public class CNVTST
    {
    
        public CNVTST()
        {
    
        }
    
        public void Run(ref bool CONV, double ALPHA, double PNORM, double TOLEPS, double XNORM, double DIFNEW
                         , double RTLEPS, double FTEST, double GTG, double PEPS, double EPSMCH, double GTPNEW
                         , double FNEW, ref double FLAST, double[] G, int offset_g, ref int[] IPIVOT, int offset_ipivot, int N, double ACCRCY)
        {
            #region Variables
            
            bool LTEST = false; double ONE = 0; double CMAX = 0; double T = 0; 
            #endregion
            #region Implicit Variables
            
            int IMAX = 0; int I = 0; 
            #endregion
            #region Array Index Correction
            
             int o_g = -1 + offset_g;  int o_ipivot = -1 + offset_ipivot; 
            #endregion
            // C
            // C TEST FOR CONVERGENCE
            // C
            #region Body
            
            IMAX = 0;
            CMAX = 0.0E0;
            LTEST = FLAST - FNEW <=  - 5.0E-1;
            for (I = 1; I <= N; I++)
            {
                if (IPIVOT[I + o_ipivot] == 0 || IPIVOT[I + o_ipivot] == 2) goto LABEL10;
                T =  - IPIVOT[I + o_ipivot] * G[I + o_g];
                if (T >= 0.0E0) goto LABEL10;
                CONV = false;
                if (LTEST) goto LABEL10;
                if (CMAX <= T) goto LABEL10;
                CMAX = T;
                IMAX = I;
            LABEL10:;
            }
            if (IMAX == 0) goto LABEL15;
            IPIVOT[IMAX + o_ipivot] = 0;
            FLAST = FNEW;
            return;
        LABEL15:;
            CONV = false;
            ONE = 1.0E0;
            if ((ALPHA * PNORM >= TOLEPS * (ONE + XNORM) || Math.Abs(DIFNEW) >= RTLEPS * FTEST || GTG >= PEPS * FTEST * FTEST) && GTG >= 1.0E-4 * ACCRCY * FTEST * FTEST) return;
            CONV = true;
            // C
            // C FOR DETAILS, SEE GILL, MURRAY, AND WRIGHT (1981, P. 308) AND
            // C FLETCHER (1981, P. 116).  THE MULTIPLIER TESTS (HERE, TESTING
            // C THE SIGN OF THE COMPONENTS OF THE GRADIENT) MAY STILL NEED TO
            // C MODIFIED TO INCORPORATE TOLERANCES FOR ZERO.
            // C
            return;
            #endregion
        }
    }

    #endregion


    #region The Class: CRASH
    
    // C
    // C
    public class CRASH
    {
    
        public CRASH()
        {
    
        }
    
        public void Run(int N, ref double[] X, int offset_x, ref int[] IPIVOT, int offset_ipivot, double[] LOW, int offset_low, double[] UP, int offset_up, ref int IER)
        {
            #region Implicit Variables
            
            int I = 0; 
            #endregion
            #region Array Index Correction
            
             int o_x = -1 + offset_x;  int o_ipivot = -1 + offset_ipivot;  int o_low = -1 + offset_low; 
             int o_up = -1 + offset_up;
            #endregion
            // C
            // C THIS INITIALIZES THE CONSTRAINT INFORMATION, AND ENSURES THAT THE
            // C INITIAL POINT SATISFIES  LOW <= X <= UP.
            // C THE CONSTRAINTS ARE CHECKED FOR CONSISTENCY.
            // C
            IER = 0;
            for (I = 1; I <= N; I++)
            {
                if (X[I + o_x] < LOW[I + o_low]) X[I + o_x] = LOW[I + o_low];
                if (X[I + o_x] > UP[I + o_up]) X[I + o_x] = UP[I + o_up];
                IPIVOT[I + o_ipivot] = 0;
                if (X[I + o_x] == LOW[I + o_low]) IPIVOT[I + o_ipivot] =  - 1;
                if (X[I + o_x] == UP[I + o_up]) IPIVOT[I + o_ipivot] = 1;
                if (UP[I + o_up] == LOW[I + o_low]) IPIVOT[I + o_ipivot] = 2;
                if (LOW[I + o_low] > UP[I + o_up]) IER =  - I;
            }
            return;
        }
    }

    #endregion


    #region The Class: MODLNP
    
    // C
    // C THE VECTORS SK AND YK, ALTHOUGH NOT IN THE CALL,
    // C ARE USED (VIA THEIR POSITION IN W) BY THE ROUTINE MSOLVE.
    // C
    public class MODLNP
    {
    
        #region Dependencies
        
        DDOT _ddot; INITPC _initpc; ZTIME _ztime; MSOLVE _msolve; GTIMS _gtims; NDIA3 _ndia3; DAXPY _daxpy; NEGVEC _negvec; 
        DCOPY _dcopy;
        #endregion
        public MODLNP(DDOT ddot, INITPC initpc, ZTIME ztime, MSOLVE msolve, GTIMS gtims, NDIA3 ndia3, DAXPY daxpy, NEGVEC negvec, DCOPY dcopy)
        {
    
            #region Set Dependencies
            
            this._ddot = ddot; this._initpc = initpc; this._ztime = ztime; this._msolve = msolve; this._gtims = gtims; 
            this._ndia3 = ndia3;this._daxpy = daxpy; this._negvec = negvec; this._dcopy = dcopy; 
            #endregion
        }
    
        public MODLNP()
        {
    
            #region Initialization Common Blocks
            
            CommonBlock SUBSCR = new CommonBlock(0, 15, 0, 0);
            #endregion
            #region Dependencies (Initialization)
            
            DDOT ddot = new DDOT();
            DCOPY dcopy = new DCOPY();
            ZTIME ztime = new ZTIME();
            SSBFGS ssbfgs = new SSBFGS();
            DAXPY daxpy = new DAXPY();
            NEGVEC negvec = new NEGVEC();
            GTIMS gtims = new GTIMS(SUBSCR);
            INITP3 initp3 = new INITP3(ddot, dcopy);
            INITPC initpc = new INITPC(initp3, SUBSCR);
            MSLV mslv = new MSLV(ddot, ssbfgs);
            MSOLVE msolve = new MSOLVE(mslv, SUBSCR);
            NDIA3 ndia3 = new NDIA3(ddot);
            #endregion
            #region Set Dependencies
            
            this._ddot = ddot; this._initpc = initpc; this._ztime = ztime; this._msolve = msolve; this._gtims = gtims; 
            this._ndia3 = ndia3;this._daxpy = daxpy; this._negvec = negvec; this._dcopy = dcopy; 
            #endregion
        }
        /// <param name="MODET">
        /// - INTEGER WHICH CONTROLS AMOUNT OF OUTPUT
        ///</param>
        /// <param name="ZSOL">
        /// - COMPUTED SEARCH DIRECTION
        ///</param>
        /// <param name="R">
        /// - RESIDUAL
        ///</param>
        /// <param name="G">
        /// - CURRENT GRADIENT
        ///</param>
        /// <param name="NITER">
        /// - NONLINEAR ITERATION #
        ///</param>
        public void Run(int MODET, ref double[] ZSOL, int offset_zsol, ref double[] GV, int offset_gv, ref double[] R, int offset_r, ref double[] V, int offset_v, ref double[] DIAGB, int offset_diagb
                         , ref double[] EMAT, int offset_emat, double[] X, int offset_x, double[] G, int offset_g, ref double[] ZK, int offset_zk, int N, ref double[] W, int offset_w
                         , int LW, int NITER, int MAXIT, ref int NFEVAL, int NMODIF, ref int NLINCG
                         , bool UPD1, double YKSK, ref double GSK, double YRSR, bool LRESET, ISFUN SFUN
                         , bool BOUNDS, int[] IPIVOT, int offset_ipivot, double ACCRCY, ref double GTP, double GNORM, double XNORM)
        {
            #region Variables
            
            double ALPHA = 0; double BETA = 0; double DELTA = 0; double PR = 0; double QOLD = 0; double QNEW = 0; 
            double QTEST = 0;double RHSNRM = 0; double RNORM = 0; double RZ = 0; double RZOLD = 0; double TOL = 0; double VGV = 0; 
            double DNRM2 = 0;bool FIRST = false; 
            #endregion
            #region Implicit Variables
            
            int I = 0; int K = 0; 
            #endregion
            #region Array Index Correction
            
             int o_zsol = -1 + offset_zsol;  int o_gv = -1 + offset_gv;  int o_r = -1 + offset_r;  int o_v = -1 + offset_v; 
             int o_diagb = -1 + offset_diagb; int o_emat = -1 + offset_emat;  int o_x = -1 + offset_x;  int o_g = -1 + offset_g; 
             int o_zk = -1 + offset_zk; int o_w = -1 + offset_w;  int o_ipivot = -1 + offset_ipivot; 
            #endregion
            #region Prolog
            
            // C
            // C THIS ROUTINE PERFORMS A PRECONDITIONED CONJUGATE-GRADIENT
            // C ITERATION IN ORDER TO SOLVE THE NEWTON EQUATIONS FOR A SEARCH
            // C DIRECTION FOR A TRUNCATED-NEWTON ALGORITHM.  WHEN THE VALUE OF THE
            // C QUADRATIC MODEL IS SUFFICIENTLY REDUCED,
            // C THE ITERATION IS TERMINATED.
            // C
            // C PARAMETERS
            // C
            // C MODET       - INTEGER WHICH CONTROLS AMOUNT OF OUTPUT
            // C ZSOL        - COMPUTED SEARCH DIRECTION
            // C G           - CURRENT GRADIENT
            // C GV,GZ1,V    - SCRATCH VECTORS
            // C R           - RESIDUAL
            // C DIAGB,EMAT  - DIAGONAL PRECONDITONING MATRIX
            // C NITER       - NONLINEAR ITERATION #
            // C FEVAL       - VALUE OF QUADRATIC FUNCTION
            // C
            // C *************************************************************
            // C INITIALIZATION
            // C *************************************************************
            // C
            // C GENERAL INITIALIZATION
            // C
            #endregion
            #region Body
            
            if (MODET > 0) ;//ERROR-ERRORWRITE(*,800)
            if (MAXIT == 0) return;
            FIRST = true;
            RHSNRM = GNORM;
            TOL = 1.0E-12;
            QOLD = 0.0E0;
            // C
            // C INITIALIZATION FOR PRECONDITIONED CONJUGATE-GRADIENT ALGORITHM
            // C
            this._initpc.Run(DIAGB, offset_diagb, ref EMAT, offset_emat, N, ref W, offset_w, LW, MODET
                             , UPD1, YKSK, GSK, YRSR, LRESET);
            for (I = 1; I <= N; I++)
            {
                R[I + o_r] =  - G[I + o_g];
                V[I + o_v] = 0.0E0;
                ZSOL[I + o_zsol] = 0.0E0;
            }
            // C
            // C ************************************************************
            // C MAIN ITERATION
            // C ************************************************************
            // C
            for (K = 1; K <= MAXIT; K++)
            {
                NLINCG += 1;
                if (MODET > 1) ;//ERROR-ERRORWRITE(*,810)K
                // C
                // C CG ITERATION TO SOLVE SYSTEM OF EQUATIONS
                // C
                if (BOUNDS) this._ztime.Run(N, ref R, offset_r, IPIVOT, offset_ipivot);
                this._msolve.Run(R, offset_r, ref ZK, offset_zk, N, ref W, offset_w, LW, UPD1
                                 , YKSK, ref GSK, YRSR, LRESET, FIRST);
                if (BOUNDS) this._ztime.Run(N, ref ZK, offset_zk, IPIVOT, offset_ipivot);
                RZ = this._ddot.Run(N, R, offset_r, 1, ZK, offset_zk, 1);
                if (RZ / RHSNRM < TOL) goto LABEL80;
                if (K == 1) BETA = 0.0E0;
                if (K > 1) BETA = RZ / RZOLD;
                for (I = 1; I <= N; I++)
                {
                    V[I + o_v] = ZK[I + o_zk] + BETA * V[I + o_v];
                }
                if (BOUNDS) this._ztime.Run(N, ref V, offset_v, IPIVOT, offset_ipivot);
                this._gtims.Run(V, offset_v, ref GV, offset_gv, N, X, offset_x, G, offset_g, ref W, offset_w
                                , LW, SFUN, ref FIRST, ref DELTA, ACCRCY, XNORM);
                if (BOUNDS) this._ztime.Run(N, ref GV, offset_gv, IPIVOT, offset_ipivot);
                NFEVAL += 1;
                VGV = this._ddot.Run(N, V, offset_v, 1, GV, offset_gv, 1);
                if (VGV / RHSNRM < TOL) goto LABEL50;
                this._ndia3.Run(N, ref EMAT, offset_emat, V, offset_v, GV, offset_gv, R, offset_r, VGV
                                , MODET);
                // C
                // C COMPUTE LINEAR STEP LENGTH
                // C
                ALPHA = RZ / VGV;
                if (MODET >= 1) ;//ERROR-ERRORWRITE(*,820)ALPHA
                // C
                // C COMPUTE CURRENT SOLUTION AND RELATED VECTORS
                // C
                this._daxpy.Run(N, ALPHA, V, offset_v, 1, ref ZSOL, offset_zsol, 1);
                this._daxpy.Run(N,  - ALPHA, GV, offset_gv, 1, ref R, offset_r, 1);
                // C
                // C TEST FOR CONVERGENCE
                // C
                GTP = this._ddot.Run(N, ZSOL, offset_zsol, 1, G, offset_g, 1);
                PR = this._ddot.Run(N, R, offset_r, 1, ZSOL, offset_zsol, 1);
                QNEW = 5.0E-1 * (GTP + PR);
                QTEST = K * (1.0E0 - QOLD / QNEW);
                if (QTEST < 0.0E0) goto LABEL70;
                QOLD = QNEW;
                if (QTEST <= 5.0E-1) goto LABEL70;
                // C
                // C PERFORM CAUTIONARY TEST
                // C
                if (GTP > 0) goto LABEL40;
                RZOLD = RZ;
            }
            // C
            // C TERMINATE ALGORITHM
            // C
            K -= 1;
            goto LABEL70;
            // C
            // C TRUNCATE ALGORITHM IN CASE OF AN EMERGENCY
            // C
        LABEL40:  
            if (MODET >=  - 1) ;//ERROR-ERRORWRITE(*,830)K
            this._daxpy.Run(N,  - ALPHA, V, offset_v, 1, ref ZSOL, offset_zsol, 1);
            GTP = this._ddot.Run(N, ZSOL, offset_zsol, 1, G, offset_g, 1);
            goto LABEL90;
        LABEL50:;
            if (MODET >  - 2) ;//ERROR-ERRORWRITE(*,840)
            
            if (K > 1) goto LABEL70;
            this._msolve.Run(G, offset_g, ref ZSOL, offset_zsol, N, ref W, offset_w, LW, UPD1
                             , YKSK, ref GSK, YRSR, LRESET, FIRST);
            this._negvec.Run(N, ref ZSOL, offset_zsol);
            if (BOUNDS) this._ztime.Run(N, ref ZSOL, offset_zsol, IPIVOT, offset_ipivot);
            GTP = this._ddot.Run(N, ZSOL, offset_zsol, 1, G, offset_g, 1);
        LABEL70:;
            if (MODET >=  - 1) ;//ERROR-ERRORWRITE(*,850)K,RNORM
            goto LABEL90;
        LABEL80:;
            if (MODET >=  - 1) ;//ERROR-ERRORWRITE(*,860)
            if (K > 1) goto LABEL70;
            this._dcopy.Run(N, G, offset_g, 1, ref ZSOL, offset_zsol, 1);
            this._negvec.Run(N, ref ZSOL, offset_zsol);
            if (BOUNDS) this._ztime.Run(N, ref ZSOL, offset_zsol, IPIVOT, offset_ipivot);
            GTP = this._ddot.Run(N, ZSOL, offset_zsol, 1, G, offset_g, 1);
            goto LABEL70;
            // C
            // C STORE (OR RESTORE) DIAGONAL PRECONDITIONING
            // C
        LABEL90:;
            this._dcopy.Run(N, EMAT, offset_emat, 1, ref DIAGB, offset_diagb, 1);
            return;
            #endregion
        }
    }

    #endregion


    #region The Class: NDIA3
    
    // C
    // C
    public class NDIA3
    {
    
        #region Dependencies
        
        DDOT _ddot; 
        #endregion
        public NDIA3(DDOT ddot)
        {
    
            #region Set Dependencies
            
            this._ddot = ddot; 
            #endregion
        }
    
        public NDIA3()
        {
    
            #region Dependencies (Initialization)
            
            DDOT ddot = new DDOT();
            #endregion
            #region Set Dependencies
            
            this._ddot = ddot; 
            #endregion
        }
        public void Run(int N, ref double[] E, int offset_e, double[] V, int offset_v, double[] GV, int offset_gv, double[] R, int offset_r, double VGV
                         , int MODET)
        {
            #region Variables
            
            double VR = 0; 
            #endregion
            #region Implicit Variables
            
            int I = 0; 
            #endregion
            #region Array Index Correction
            
             int o_e = -1 + offset_e;  int o_v = -1 + offset_v;  int o_gv = -1 + offset_gv;  int o_r = -1 + offset_r; 
            #endregion
            // C
            // C UPDATE THE PRECONDITIOING MATRIX BASED ON A DIAGONAL VERSION
            // C OF THE BFGS QUASI-NEWTON UPDATE.
            // C
            VR = this._ddot.Run(N, V, offset_v, 1, R, offset_r, 1);
            for (I = 1; I <= N; I++)
            {
                E[I + o_e] +=  - R[I + o_r] * R[I + o_r] / VR + GV[I + o_gv] * GV[I + o_gv] / VGV;
                if (E[I + o_e] > 1.0E-6) goto LABEL10;
                if (MODET >  - 2) ;//ERROR-ERRORWRITE(*,800)E(I)
                E[I + o_e] = 1.0E0;
            LABEL10:;
            }
            return;
        }
    }

    #endregion


    #region The Class: NEGVEC
    
    // C
    // C      SERVICE ROUTINES FOR OPTIMIZATION
    // C
    public class NEGVEC
    {
    
        public NEGVEC()
        {
    
        }
    
        public void Run(int N, ref double[] V, int offset_v)
        {
            #region Variables
            
            int I = 0; 
            #endregion
            #region Array Index Correction
            
             int o_v = -1 + offset_v; 
            #endregion
            // C
            // C NEGATIVE OF THE VECTOR V
            // C
            for (I = 1; I <= N; I++)
            {
                V[I + o_v] =  - V[I + o_v];
            }
            return;
        }
    }

    #endregion


    #region The Class: LSOUT
    
    // C
    // C
    public class LSOUT
    {
    
        public LSOUT()
        {
    
        }
    
        public void Run(int ILOC, int ITEST, double XMIN, double FMIN, double GMIN, double XW
                         , double FW, double GW, double U, double A, double B, double TOL
                         , double EPS, double SCXBD, double XLAMDA)
        {
            #region Variables
            
            double YA = 0; double YB = 0; double YBND = 0; double YW = 0; double YU = 0; 
            #endregion
            // C
            // C ERROR PRINTOUTS FOR GETPTC
            // C
            YU = XMIN + U;
            YA = A + XMIN;
            YB = B + XMIN;
            YW = XW + XMIN;
            YBND = SCXBD + XMIN;
            //ERROR-ERROR      WRITE(*,800);
            //ERROR-ERROR      WRITE(*,810) TOL,EPS;
            //ERROR-ERROR      WRITE(*,820) YA,YB;
            //ERROR-ERROR      WRITE(*,830) YBND;
            //ERROR-ERROR      WRITE(*,840) YW,FW,GW;
            //ERROR-ERROR      WRITE(*,850) XMIN,FMIN,GMIN;
            //ERROR-ERROR      WRITE(*,860) YU;
            //ERROR-ERROR      WRITE(*,870) ILOC,ITEST;
            return;
        }
    }

    #endregion


    #region The Class: STEP1
    
    // C
    // C
    public class STEP1
    {
    
        #region Dependencies
        
        MCHPR1 _mchpr1; 
        #endregion
        public STEP1(MCHPR1 mchpr1)
        {
    
            #region Set Dependencies
            
            this._mchpr1 = mchpr1; 
            #endregion
        }
    
        public STEP1()
        {
    
            #region Dependencies (Initialization)
            
            MCHPR1 mchpr1 = new MCHPR1();
            #endregion
            #region Set Dependencies
            
            this._mchpr1 = mchpr1; 
            #endregion
        }
        public double Run(double FNEW, double FM, double GTP, double SMAX)
        {
        double step1 = 0;
            #region Variables
            
            double ALPHA = 0; double D = 0; double EPSMCH = 0; double DABS = 0; 
            #endregion
            // C
            // C ********************************************************
            // C STEP1 RETURNS THE LENGTH OF THE INITIAL STEP TO BE TAKEN ALONG THE
            // C VECTOR P IN THE NEXT LINEAR SEARCH.
            // C ********************************************************
            // C
            EPSMCH = this._mchpr1.Run();
            D = Math.Abs(FNEW - FM);
            ALPHA = 1.0E0;
            if (2.0E0 * D <= ( - GTP) && D >= EPSMCH) ALPHA =  - 2.0E0 * D / GTP;
            if (ALPHA >= SMAX) ALPHA = SMAX;
            step1 = ALPHA;
            return step1;
        }
    }

    #endregion


    #region The Class: MCHPR1
    
    // C
    // C
    public class MCHPR1
    {
    
        public MCHPR1()
        {
    
        }
    
        public double Run()
        {
        double mchpr1 = 0;
            #region Variables
            
            double X = 0; 
            #endregion
            // C
            // C RETURNS THE VALUE OF EPSMCH, WHERE EPSMCH IS THE SMALLEST POSSIBLE
            // C REAL NUMBER SUCH THAT 1.0 + EPSMCH .GT. 1.0
            // C
            // C FOR VAX
            // C
            mchpr1 = 1.0E-17;
            // C
            // C FOR SUN
            // C
            // C     MCHPR1 = 1.0842021724855D-19
            return mchpr1;
        }
    }

    #endregion


    #region The Class: CHKUCP
    
    // C
    // C
    public class CHKUCP
    {
    
        #region Dependencies
        
        MCHPR1 _mchpr1; DNRM2 _dnrm2; 
        #endregion
        public CHKUCP(MCHPR1 mchpr1, DNRM2 dnrm2)
        {
    
            #region Set Dependencies
            
            this._mchpr1 = mchpr1; this._dnrm2 = dnrm2; 
            #endregion
        }
    
        public CHKUCP()
        {
    
            #region Dependencies (Initialization)
            
            MCHPR1 mchpr1 = new MCHPR1();
            DNRM2 dnrm2 = new DNRM2();
            #endregion
            #region Set Dependencies
            
            this._mchpr1 = mchpr1; this._dnrm2 = dnrm2; 
            #endregion
        }
        public void Run(int LWTEST, int MAXFUN, ref int NWHY, int N, ref double ALPHA, ref double EPSMCH
                         , double ETA, ref double PEPS, ref double RTEPS, ref double RTOL, ref double RTOLSQ, double STEPMX
                         , ref double TEST, double XTOL, ref double XNORM, double[] X, int offset_x, int LW, ref double SMALL
                         , ref double TINY, double ACCRCY)
        {
            #region Variables
            
            double DABS = 0; double DSQRT = 0; 
            #endregion
            #region Array Index Correction
            
             int o_x = -1 + offset_x; 
            #endregion
            // C
            // C CHECKS PARAMETERS AND SETS CONSTANTS WHICH ARE COMMON TO BOTH
            // C DERIVATIVE AND NON-DERIVATIVE ALGORITHMS
            // C
            #region Body
            
            EPSMCH = this._mchpr1.Run();
            SMALL = EPSMCH * EPSMCH;
            TINY = SMALL;
            NWHY =  - 1;
            RTEPS = Math.Sqrt(EPSMCH);
            RTOL = XTOL;
            if (Math.Abs(RTOL) < ACCRCY) RTOL = 1.0E1 * RTEPS;
            // C
            // C CHECK FOR ERRORS IN THE INPUT PARAMETERS
            // C
            if (LW < LWTEST || N < 1 || RTOL < 0.0E0 || ETA >= 1.0E0 || ETA < 0.0E0 || STEPMX < RTOL || MAXFUN < 1) return;
            NWHY = 0;
            // C
            // C SET CONSTANTS FOR LATER
            // C
            RTOLSQ = RTOL * RTOL;
            PEPS = Math.Pow(ACCRCY,0.6666E0);
            XNORM = this._dnrm2.Run(N, X, offset_x, 1);
            ALPHA = 0.0E0;
            TEST = 0.0E0;
            return;
            #endregion
        }
    }

    #endregion


    #region The Class: SETUCR
    
    // C
    // C
    public class SETUCR
    {
    
        #region Dependencies
        
        DDOT _ddot; 
        #endregion
        public SETUCR(DDOT ddot)
        {
    
            #region Set Dependencies
            
            this._ddot = ddot; 
            #endregion
        }
    
        public SETUCR()
        {
    
            #region Dependencies (Initialization)
            
            DDOT ddot = new DDOT();
            #endregion
            #region Set Dependencies
            
            this._ddot = ddot; 
            #endregion
        }
        public void Run(double SMALL, ref int NFTOTL, ref int NITER, int N, double F, ref double FNEW
                         , ref double FM, ref double GTG, ref double OLDF, ISFUN SFUN, ref double[] G, int offset_g, double[] X, int offset_x)
        {
            #region Array Index Correction
            
             int o_g = -1 + offset_g;  int o_x = -1 + offset_x; 
            #endregion
            // C
            // C CHECK INPUT PARAMETERS, COMPUTE THE INITIAL FUNCTION VALUE, SET
            // C CONSTANTS FOR THE SUBSEQUENT MINIMIZATION
            // C
            FM = F;
            // C
            // C COMPUTE THE INITIAL FUNCTION VALUE
            // C
            SFUN.Run(N, X, offset_x, ref FNEW, ref G, offset_g);
            NFTOTL = 1;
            // C
            // C SET CONSTANTS FOR LATER
            // C
            NITER = 0;
            OLDF = FNEW;
            GTG = this._ddot.Run(N, G, offset_g, 1, G, offset_g, 1);
            return;
        }
    }

    #endregion


    #region The Class: GTIMS
    
    // C
    // C
    public class GTIMS
    {
    
        #region Common variables
        
        #region Common Block: SUBSCR Declaration
        
        CommonBlock _subscr;
        Oint LGV; Oint LZ1; Oint LZK; Oint LV; Oint LSK; Oint LYK; Oint LDIAGB; Oint LSR; Oint LYR; Oint LHYR; Oint LHG; 
        Oint LHYK;Oint LPK; Oint LEMAT; Oint LWTEST; 
        #endregion
        #endregion
        public GTIMS(CommonBlock SUBSCR)
        {
    
            #region Common varaible Initialization
            
            #region Common Block: SUBSCR Initialization
            
            this._subscr = SUBSCR;
            LGV = SUBSCR.intData[0];
            LZ1 = SUBSCR.intData[1];
            LZK = SUBSCR.intData[2];
            LV = SUBSCR.intData[3];
            LSK = SUBSCR.intData[4];
            LYK = SUBSCR.intData[5];
            LDIAGB = SUBSCR.intData[6];
            LSR = SUBSCR.intData[7];
            LYR = SUBSCR.intData[8];
            LHYR = SUBSCR.intData[9];
            LHG = SUBSCR.intData[10];
            LHYK = SUBSCR.intData[11];
            LPK = SUBSCR.intData[12];
            LEMAT = SUBSCR.intData[13];
            LWTEST = SUBSCR.intData[14];
            #endregion
            #endregion
        }
    
        public GTIMS()
        {
    
            #region Initialization Common Blocks
            
            CommonBlock SUBSCR = new CommonBlock(0, 15, 0, 0);
            #endregion
            #region Common varaible Initialization
            
            #region Common Block: SUBSCR Initialization
            
            this._subscr = SUBSCR;
            LGV = SUBSCR.intData[0];
            LZ1 = SUBSCR.intData[1];
            LZK = SUBSCR.intData[2];
            LV = SUBSCR.intData[3];
            LSK = SUBSCR.intData[4];
            LYK = SUBSCR.intData[5];
            LDIAGB = SUBSCR.intData[6];
            LSR = SUBSCR.intData[7];
            LYR = SUBSCR.intData[8];
            LHYR = SUBSCR.intData[9];
            LHG = SUBSCR.intData[10];
            LHYK = SUBSCR.intData[11];
            LPK = SUBSCR.intData[12];
            LEMAT = SUBSCR.intData[13];
            LWTEST = SUBSCR.intData[14];
            #endregion
            #endregion
        }
        /// <param name="V">
        /// AND STORES THE RESULT IN THE VECTOR GV (FINITE-DIFFERENCE VERSION)
        ///</param>
        public void Run(double[] V, int offset_v, ref double[] GV, int offset_gv, int N, double[] X, int offset_x, double[] G, int offset_g, ref double[] W, int offset_w
                         , int LW, ISFUN SFUN, ref bool FIRST, ref double DELTA, double ACCRCY, double XNORM)
        {
            #region Variables
            
            double DINV = 0; double F = 0; double DSQRT = 0; 
            #endregion
            #region Implicit Variables
            
            int IHG = 0; int I = 0; 
            #endregion
            #region Array Index Correction
            
             int o_v = -1 + offset_v;  int o_gv = -1 + offset_gv;  int o_x = -1 + offset_x;  int o_g = -1 + offset_g; 
             int o_w = -1 + offset_w;
            #endregion
            // C
            // C THIS ROUTINE COMPUTES THE PRODUCT OF THE MATRIX G TIMES THE VECTOR
            // C V AND STORES THE RESULT IN THE VECTOR GV (FINITE-DIFFERENCE VERSION)
            // C
            if (!FIRST) goto LABEL20;
            DELTA = Math.Sqrt(ACCRCY) * (1.0E0 + XNORM);
            FIRST = false;
        LABEL20:;
            DINV = 1.0E0 / DELTA;
            IHG = LHG.v;
            for (I = 1; I <= N; I++)
            {
                W[IHG + o_w] = X[I + o_x] + DELTA * V[I + o_v];
                IHG += 1;
            }
            SFUN.Run(N, W, LHG.v + o_w, ref F, ref GV, offset_gv);
            for (I = 1; I <= N; I++)
            {
                GV[I + o_gv] = (GV[I + o_gv] - G[I + o_g]) * DINV;
            }
            return;
        }
    }

    #endregion


    #region The Class: MSOLVE
    
    // C
    // C
    public class MSOLVE
    {
    
        #region Dependencies
        
        MSLV _mslv; 
        #endregion
        #region Common variables
        
        #region Common Block: SUBSCR Declaration
        
        CommonBlock _subscr;
        Oint LGV; Oint LZ1; Oint LZK; Oint LV; Oint LSK; Oint LYK; Oint LDIAGB; Oint LSR; Oint LYR; Oint LHYR; Oint LHG; 
        Oint LHYK;Oint LPK; Oint LEMAT; Oint LWTEST; 
        #endregion
        #endregion
        public MSOLVE(MSLV mslv, CommonBlock SUBSCR)
        {
    
            #region Set Dependencies
            
            this._mslv = mslv; 
            #endregion
            #region Common varaible Initialization
            
            #region Common Block: SUBSCR Initialization
            
            this._subscr = SUBSCR;
            LGV = SUBSCR.intData[0];
            LZ1 = SUBSCR.intData[1];
            LZK = SUBSCR.intData[2];
            LV = SUBSCR.intData[3];
            LSK = SUBSCR.intData[4];
            LYK = SUBSCR.intData[5];
            LDIAGB = SUBSCR.intData[6];
            LSR = SUBSCR.intData[7];
            LYR = SUBSCR.intData[8];
            LHYR = SUBSCR.intData[9];
            LHG = SUBSCR.intData[10];
            LHYK = SUBSCR.intData[11];
            LPK = SUBSCR.intData[12];
            LEMAT = SUBSCR.intData[13];
            LWTEST = SUBSCR.intData[14];
            #endregion
            #endregion
        }
    
        public MSOLVE()
        {
    
            #region Initialization Common Blocks
            
            CommonBlock SUBSCR = new CommonBlock(0, 15, 0, 0);
            #endregion
            #region Dependencies (Initialization)
            
            DDOT ddot = new DDOT();
            SSBFGS ssbfgs = new SSBFGS();
            MSLV mslv = new MSLV(ddot, ssbfgs);
            #endregion
            #region Set Dependencies
            
            this._mslv = mslv; 
            #endregion
            #region Common varaible Initialization
            
            #region Common Block: SUBSCR Initialization
            
            this._subscr = SUBSCR;
            LGV = SUBSCR.intData[0];
            LZ1 = SUBSCR.intData[1];
            LZK = SUBSCR.intData[2];
            LV = SUBSCR.intData[3];
            LSK = SUBSCR.intData[4];
            LYK = SUBSCR.intData[5];
            LDIAGB = SUBSCR.intData[6];
            LSR = SUBSCR.intData[7];
            LYR = SUBSCR.intData[8];
            LHYR = SUBSCR.intData[9];
            LHG = SUBSCR.intData[10];
            LHYK = SUBSCR.intData[11];
            LPK = SUBSCR.intData[12];
            LEMAT = SUBSCR.intData[13];
            LWTEST = SUBSCR.intData[14];
            #endregion
            #endregion
        }
        public void Run(double[] G, int offset_g, ref double[] Y, int offset_y, int N, ref double[] W, int offset_w, int LW, bool UPD1
                         , double YKSK, ref double GSK, double YRSR, bool LRESET, bool FIRST)
        {
            #region Array Index Correction
            
             int o_g = -1 + offset_g;  int o_y = -1 + offset_y;  int o_w = -1 + offset_w; 
            #endregion
            // C
            // C THIS ROUTINE SETS UPT THE ARRAYS FOR MSLV
            // C
            this._mslv.Run(G, offset_g, ref Y, offset_y, N, W, LSK.v + o_w, W, LYK.v + o_w, W, LDIAGB.v + o_w
                           , W, LSR.v + o_w, W, LYR.v + o_w, ref W, LHYR.v + o_w, ref W, LHG.v + o_w, ref W, LHYK.v + o_w, UPD1
                           , YKSK, ref GSK, YRSR, LRESET, FIRST);
            return;
        }
    }

    #endregion


    #region The Class: MSLV
    
    public class MSLV
    {

        #region Variables

        double RDIAGB = 0; double YKHYK = 0; double GHYK = 0; double YKSR = 0; double YKHYR = 0; double YRHYR = 0;
        double GSR = 0; double GHYR = 0; double ONE = 0;
        #endregion
    
        #region Dependencies
        
        DDOT _ddot; SSBFGS _ssbfgs; 
        #endregion
        public MSLV(DDOT ddot, SSBFGS ssbfgs)
        {
    
            #region Set Dependencies
            
            this._ddot = ddot; this._ssbfgs = ssbfgs; 
            #endregion
        }
    
        public MSLV()
        {
    
            #region Dependencies (Initialization)
            
            DDOT ddot = new DDOT();
            SSBFGS ssbfgs = new SSBFGS();
            #endregion
            #region Set Dependencies
            
            this._ddot = ddot; this._ssbfgs = ssbfgs; 
            #endregion
        }
        public void Run(double[] G, int offset_g, ref double[] Y, int offset_y, int N, double[] SK, int offset_sk, double[] YK, int offset_yk, double[] DIAGB, int offset_diagb
                         , double[] SR, int offset_sr, double[] YR, int offset_yr, ref double[] HYR, int offset_hyr, ref double[] HG, int offset_hg, ref double[] HYK, int offset_hyk, bool UPD1
                         , double YKSK, ref double GSK, double YRSR, bool LRESET, bool FIRST)
        {

            #region Implicit Variables
            
            int I = 0; 
            #endregion
            #region Array Index Correction
            
             int o_g = -1 + offset_g;  int o_y = -1 + offset_y;  int o_sk = -1 + offset_sk;  int o_yk = -1 + offset_yk; 
             int o_diagb = -1 + offset_diagb; int o_sr = -1 + offset_sr;  int o_yr = -1 + offset_yr;  int o_hyr = -1 + offset_hyr; 
             int o_hg = -1 + offset_hg; int o_hyk = -1 + offset_hyk; 
            #endregion
            // C
            // C THIS ROUTINE ACTS AS A PRECONDITIONING STEP FOR THE
            // C LINEAR CONJUGATE-GRADIENT ROUTINE.  IT IS ALSO THE
            // C METHOD OF COMPUTING THE SEARCH DIRECTION FROM THE
            // C GRADIENT FOR THE NON-LINEAR CONJUGATE-GRADIENT CODE.
            // C IT REPRESENTS A TWO-STEP SELF-SCALED BFGS FORMULA.
            // C
            #region Body
            
            if (UPD1) goto LABEL100;
            ONE = 1.0E0;
            GSK = this._ddot.Run(N, G, offset_g, 1, SK, offset_sk, 1);
            if (LRESET) goto LABEL60;
            // C
            // C COMPUTE HG AND HY WHERE H IS THE INVERSE OF THE DIAGONALS
            // C
            for (I = 1; I <= N; I++)
            {
                RDIAGB = 1.0E0 / DIAGB[I + o_diagb];
                HG[I + o_hg] = G[I + o_g] * RDIAGB;
                if (FIRST) HYK[I + o_hyk] = YK[I + o_yk] * RDIAGB;
                if (FIRST) HYR[I + o_hyr] = YR[I + o_yr] * RDIAGB;
            }
            if (FIRST) YKSR = this._ddot.Run(N, YK, offset_yk, 1, SR, offset_sr, 1);
            if (FIRST) YKHYR = this._ddot.Run(N, YK, offset_yk, 1, HYR, offset_hyr, 1);
            GSR = this._ddot.Run(N, G, offset_g, 1, SR, offset_sr, 1);
            GHYR = this._ddot.Run(N, G, offset_g, 1, HYR, offset_hyr, 1);
            if (FIRST) YRHYR = this._ddot.Run(N, YR, offset_yr, 1, HYR, offset_hyr, 1);
            this._ssbfgs.Run(N, ONE, SR, offset_sr, YR, offset_yr, HG, offset_hg, HYR, offset_hyr
                             , YRSR, YRHYR, GSR, GHYR, ref HG, offset_hg);
            if (FIRST)
            {
                this._ssbfgs.Run(N, ONE, SR, offset_sr, YR, offset_yr, HYK, offset_hyk, HYR, offset_hyr
                                 , YRSR, YRHYR, YKSR, YKHYR, ref HYK, offset_hyk);
            }
            YKHYK = this._ddot.Run(N, HYK, offset_hyk, 1, YK, offset_yk, 1);
            GHYK = this._ddot.Run(N, HYK, offset_hyk, 1, G, offset_g, 1);
            this._ssbfgs.Run(N, ONE, SK, offset_sk, YK, offset_yk, HG, offset_hg, HYK, offset_hyk
                             , YKSK, YKHYK, GSK, GHYK, ref Y, offset_y);
            return;
        LABEL60:;
            // C
            // C COMPUTE GH AND HY WHERE H IS THE INVERSE OF THE DIAGONALS
            // C
            for (I = 1; I <= N; I++)
            {
                RDIAGB = 1.0E0 / DIAGB[I + o_diagb];
                HG[I + o_hg] = G[I + o_g] * RDIAGB;
                if (FIRST) HYK[I + o_hyk] = YK[I + o_yk] * RDIAGB;
            }
            if (FIRST) YKHYK = this._ddot.Run(N, YK, offset_yk, 1, HYK, offset_hyk, 1);
            GHYK = this._ddot.Run(N, G, offset_g, 1, HYK, offset_hyk, 1);
            this._ssbfgs.Run(N, ONE, SK, offset_sk, YK, offset_yk, HG, offset_hg, HYK, offset_hyk
                             , YKSK, YKHYK, GSK, GHYK, ref Y, offset_y);
            return;
        LABEL100:;
            for (I = 1; I <= N; I++)
            {
                Y[I + o_y] = G[I + o_g] / DIAGB[I + o_diagb];
            }
            return;
            #endregion
        }
    }

    #endregion


    #region The Class: SSBFGS
    
    // C
    // C
    public class SSBFGS
    {
    
        public SSBFGS()
        {
    
        }
    
        public void Run(int N, double GAMMA, double[] SJ, int offset_sj, double[] YJ, int offset_yj, double[] HJV, int offset_hjv, double[] HJYJ, int offset_hjyj
                         , double YJSJ, double YJHYJ, double VSJ, double VHYJ, ref double[] HJP1V, int offset_hjp1v)
        {
            #region Variables
            
            int I = 0; double BETA = 0; double DELTA = 0; 
            #endregion
            #region Array Index Correction
            
             int o_sj = -1 + offset_sj;  int o_yj = -1 + offset_yj;  int o_hjv = -1 + offset_hjv;  int o_hjyj = -1 + offset_hjyj; 
             int o_hjp1v = -1 + offset_hjp1v;
            #endregion
            // C
            // C SELF-SCALED BFGS
            // C
            DELTA = (1.0E0 + GAMMA * YJHYJ / YJSJ) * VSJ / YJSJ - GAMMA * VHYJ / YJSJ;
            BETA =  - GAMMA * VSJ / YJSJ;
            for (I = 1; I <= N; I++)
            {
                HJP1V[I + o_hjp1v] = GAMMA * HJV[I + o_hjv] + DELTA * SJ[I + o_sj] + BETA * HJYJ[I + o_hjyj];
            }
            return;
        }
    }

    #endregion


    #region The Class: INITPC
    
    // C
    // C ROUTINES TO INITIALIZE PRECONDITIONER
    // C
    public class INITPC
    {
    
        #region Dependencies
        
        INITP3 _initp3; 
        #endregion
        #region Common variables
        
        #region Common Block: SUBSCR Declaration
        
        CommonBlock _subscr;
        Oint LGV; Oint LZ1; Oint LZK; Oint LV; Oint LSK; Oint LYK; Oint LDIAGB; Oint LSR; Oint LYR; Oint LHYR; Oint LHG; 
        Oint LHYK;Oint LPK; Oint LEMAT; Oint LWTEST; 
        #endregion
        #endregion
        public INITPC(INITP3 initp3, CommonBlock SUBSCR)
        {
    
            #region Set Dependencies
            
            this._initp3 = initp3; 
            #endregion
            #region Common varaible Initialization
            
            #region Common Block: SUBSCR Initialization
            
            this._subscr = SUBSCR;
            LGV = SUBSCR.intData[0];
            LZ1 = SUBSCR.intData[1];
            LZK = SUBSCR.intData[2];
            LV = SUBSCR.intData[3];
            LSK = SUBSCR.intData[4];
            LYK = SUBSCR.intData[5];
            LDIAGB = SUBSCR.intData[6];
            LSR = SUBSCR.intData[7];
            LYR = SUBSCR.intData[8];
            LHYR = SUBSCR.intData[9];
            LHG = SUBSCR.intData[10];
            LHYK = SUBSCR.intData[11];
            LPK = SUBSCR.intData[12];
            LEMAT = SUBSCR.intData[13];
            LWTEST = SUBSCR.intData[14];
            #endregion
            #endregion
        }
    
        public INITPC()
        {
    
            #region Initialization Common Blocks
            
            CommonBlock SUBSCR = new CommonBlock(0, 15, 0, 0);
            #endregion
            #region Dependencies (Initialization)
            
            DDOT ddot = new DDOT();
            DCOPY dcopy = new DCOPY();
            INITP3 initp3 = new INITP3(ddot, dcopy);
            #endregion
            #region Set Dependencies
            
            this._initp3 = initp3; 
            #endregion
            #region Common varaible Initialization
            
            #region Common Block: SUBSCR Initialization
            
            this._subscr = SUBSCR;
            LGV = SUBSCR.intData[0];
            LZ1 = SUBSCR.intData[1];
            LZK = SUBSCR.intData[2];
            LV = SUBSCR.intData[3];
            LSK = SUBSCR.intData[4];
            LYK = SUBSCR.intData[5];
            LDIAGB = SUBSCR.intData[6];
            LSR = SUBSCR.intData[7];
            LYR = SUBSCR.intData[8];
            LHYR = SUBSCR.intData[9];
            LHG = SUBSCR.intData[10];
            LHYK = SUBSCR.intData[11];
            LPK = SUBSCR.intData[12];
            LEMAT = SUBSCR.intData[13];
            LWTEST = SUBSCR.intData[14];
            #endregion
            #endregion
        }
        public void Run(double[] DIAGB, int offset_diagb, ref double[] EMAT, int offset_emat, int N, ref double[] W, int offset_w, int LW, int MODET
                         , bool UPD1, double YKSK, double GSK, double YRSR, bool LRESET)
        {
            #region Array Index Correction
            
             int o_diagb = -1 + offset_diagb;  int o_emat = -1 + offset_emat;  int o_w = -1 + offset_w; 
            #endregion
            this._initp3.Run(DIAGB, offset_diagb, ref EMAT, offset_emat, N, LRESET, YKSK, YRSR
                             , ref W, LHYK.v + o_w, W, LSK.v + o_w, W, LYK.v + o_w, W, LSR.v + o_w, W, LYR.v + o_w, MODET
                             , UPD1);
            return;
        }
    }

    #endregion


    #region The Class: INITP3
    
    public class INITP3
    {
    
        #region Dependencies
        
        DDOT _ddot; DCOPY _dcopy; 
        #endregion
        public INITP3(DDOT ddot, DCOPY dcopy)
        {
    
            #region Set Dependencies
            
            this._ddot = ddot; this._dcopy = dcopy; 
            #endregion
        }
    
        public INITP3()
        {
    
            #region Dependencies (Initialization)
            
            DDOT ddot = new DDOT();
            DCOPY dcopy = new DCOPY();
            #endregion
            #region Set Dependencies
            
            this._ddot = ddot; this._dcopy = dcopy; 
            #endregion
        }
        public void Run(double[] DIAGB, int offset_diagb, ref double[] EMAT, int offset_emat, int N, bool LRESET, double YKSK, double YRSR
                         , ref double[] BSK, int offset_bsk, double[] SK, int offset_sk, double[] YK, int offset_yk, double[] SR, int offset_sr, double[] YR, int offset_yr, int MODET
                         , bool UPD1)
        {
            #region Variables
            
            double COND = 0; double SDS = 0; double SRDS = 0; double YRSK = 0; double TD = 0; double D1 = 0; double DN = 0; 
            #endregion
            #region Implicit Variables
            
            int I = 0; 
            #endregion
            #region Array Index Correction
            
             int o_diagb = -1 + offset_diagb;  int o_emat = -1 + offset_emat;  int o_bsk = -1 + offset_bsk; 
             int o_sk = -1 + offset_sk; int o_yk = -1 + offset_yk;  int o_sr = -1 + offset_sr;  int o_yr = -1 + offset_yr; 
            #endregion
            #region Body
            
            if (UPD1) goto LABEL90;
            if (LRESET) goto LABEL60;
            for (I = 1; I <= N; I++)
            {
                BSK[I + o_bsk] = DIAGB[I + o_diagb] * SR[I + o_sr];
            }
            SDS = this._ddot.Run(N, SR, offset_sr, 1, BSK, offset_bsk, 1);
            SRDS = this._ddot.Run(N, SK, offset_sk, 1, BSK, offset_bsk, 1);
            YRSK = this._ddot.Run(N, YR, offset_yr, 1, SK, offset_sk, 1);
            for (I = 1; I <= N; I++)
            {
                TD = DIAGB[I + o_diagb];
                BSK[I + o_bsk] = TD * SK[I + o_sk] - BSK[I + o_bsk] * SRDS / SDS + YR[I + o_yr] * YRSK / YRSR;
                EMAT[I + o_emat] = TD - TD * TD * SR[I + o_sr] * SR[I + o_sr] / SDS + YR[I + o_yr] * YR[I + o_yr] / YRSR;
            }
            SDS = this._ddot.Run(N, SK, offset_sk, 1, BSK, offset_bsk, 1);
            for (I = 1; I <= N; I++)
            {
                EMAT[I + o_emat] +=  - BSK[I + o_bsk] * BSK[I + o_bsk] / SDS + YK[I + o_yk] * YK[I + o_yk] / YKSK;
            }
            goto LABEL110;
        LABEL60:;
            for (I = 1; I <= N; I++)
            {
                BSK[I + o_bsk] = DIAGB[I + o_diagb] * SK[I + o_sk];
            }
            SDS = this._ddot.Run(N, SK, offset_sk, 1, BSK, offset_bsk, 1);
            for (I = 1; I <= N; I++)
            {
                TD = DIAGB[I + o_diagb];
                EMAT[I + o_emat] = TD - TD * TD * SK[I + o_sk] * SK[I + o_sk] / SDS + YK[I + o_yk] * YK[I + o_yk] / YKSK;
            }
            goto LABEL110;
        LABEL90:;
            this._dcopy.Run(N, DIAGB, offset_diagb, 1, ref EMAT, offset_emat, 1);
        LABEL110:;
            if (MODET < 1) return;
            D1 = EMAT[1 + o_emat];
            DN = EMAT[1 + o_emat];
            for (I = 1; I <= N; I++)
            {
                if (EMAT[I + o_emat] < D1) D1 = EMAT[I + o_emat];
                if (EMAT[I + o_emat] > DN) DN = EMAT[I + o_emat];
            }
            COND = DN / D1;
            //ERROR-ERROR      WRITE(*,800) D1,DN,COND;
            return;
            #endregion
        }
    }

    #endregion


    #region The Class: SETPAR
    
    // C
    // C
    public class SETPAR
    {
    
        #region Common variables
        
        #region Common Block: SUBSCR Declaration
        
        CommonBlock _subscr;
        Oint[] LSUB; int o_lsub; Oint LWTEST; 
        #endregion
        #endregion
        public SETPAR(CommonBlock SUBSCR)
        {
    
            #region Common varaible Initialization
            
            #region Common Block: SUBSCR Initialization
            
            this._subscr = SUBSCR;
            //Start Array: LSUB  StartIndex: 0  LastIndex: 13
            LSUB = SUBSCR.intData;
            o_lsub = -1;  //o_ = StartIndex-1
            //End Array: LSUB
            LWTEST = SUBSCR.intData[14];
            #endregion
            #endregion
        }
    
        public SETPAR()
        {
    
            #region Initialization Common Blocks
            
            CommonBlock SUBSCR = new CommonBlock(0, 15, 0, 0);
            #endregion
            #region Common varaible Initialization
            
            #region Common Block: SUBSCR Initialization
            
            this._subscr = SUBSCR;
            //Start Array: LSUB  StartIndex: 0  LastIndex: 13
            LSUB = SUBSCR.intData;
            o_lsub = -1;  //o_ = StartIndex-1
            //End Array: LSUB
            LWTEST = SUBSCR.intData[14];
            #endregion
            #endregion
        }
        public void Run(int N)
        {
            #region Implicit Variables
            
            int I = 0; 
            #endregion
            // C
            // C SET UP PARAMETERS FOR THE OPTIMIZATION ROUTINE
            // C
            for (I = 1; I <= 14; I++)
            {
                LSUB[I + o_lsub].v = (I - 1) * N + 1;
            }
            LWTEST.v = LSUB[14 + o_lsub].v + N - 1;
            return;
        }
    }

    #endregion


    #region The Class: LINDER
    
    // C
    // C      LINE SEARCH ALGORITHMS OF GILL AND MURRAY
    // C
    public class LINDER
    {
    
        #region Dependencies
        
        DDOT _ddot; GETPTC _getptc; LSOUT _lsout; DCOPY _dcopy; 
        #endregion
        public LINDER(DDOT ddot, GETPTC getptc, LSOUT lsout, DCOPY dcopy)
        {
    
            #region Set Dependencies
            
            this._ddot = ddot; this._getptc = getptc; this._lsout = lsout; this._dcopy = dcopy; 
            #endregion
        }
    
        public LINDER()
        {
    
            #region Dependencies (Initialization)
            
            DDOT ddot = new DDOT();
            GETPTC getptc = new GETPTC();
            LSOUT lsout = new LSOUT();
            DCOPY dcopy = new DCOPY();
            #endregion
            #region Set Dependencies
            
            this._ddot = ddot; this._getptc = getptc; this._lsout = lsout; this._dcopy = dcopy; 
            #endregion
        }
        public void Run(int N, ISFUN SFUN, double SMALL, double EPSMCH, ref double RELTOL, ref double ABSTOL
                         , double TNYTOL, double ETA, double SFTBND, double XBND, double[] P, int offset_p, double GTP
                         , ref double[] X, int offset_x, ref double F, ref double ALPHA, ref double[] G, int offset_g, ref int NFTOTL, ref int IFLAG
                         , ref double[] W, int offset_w, int LW)
        {
            #region Variables
            
            int I = 0; int IENTRY = 0; int ITEST = 0; int L = 0; int LG = 0; int LX = 0; int NUMF = 0; int ITCNT = 0; 
            double A = 0;double B = 0; double B1 = 0; double BIG = 0; double E = 0; double FACTOR = 0; double FMIN = 0; 
            double FPRESN = 0;double FU = 0; double FW = 0; double GMIN = 0; double GTEST1 = 0; double GTEST2 = 0; double GU = 0; 
            double GW = 0;double OLDF = 0; double SCXBND = 0; double STEP = 0; double TOL = 0; double U = 0; double XMIN = 0; 
            double XW = 0;double RMU = 0; double RTSMLL = 0; double UALPHA = 0; bool BRAKTD = false; double DSQRT = 0; 
            #endregion
            #region Implicit Variables
            
            int LSPRNT = 0; int NPRNT = 0; 
            #endregion
            #region Array Index Correction
            
             int o_p = -1 + offset_p;  int o_x = -1 + offset_x;  int o_g = -1 + offset_g;  int o_w = -1 + offset_w; 
            #endregion
            // C
            // C
            // C
            // C      THE FOLLOWING STANDARD FUNCTIONS AND SYSTEM FUNCTIONS ARE
            // C      CALLED WITHIN LINDER
            // C
            // C
            // C      ALLOCATE THE ADDRESSES FOR LOCAL WORKSPACE
            // C
            #region Body
            
            LX = 1;
            LG = LX + N;
            LSPRNT = 0;
            NPRNT = 10000;
            RTSMLL = Math.Sqrt(SMALL);
            BIG = 1.0E0 / SMALL;
            ITCNT = 0;
            // C
            // C      SET THE ESTIMATED RELATIVE PRECISION IN F(X).
            // C
            FPRESN = 10.0E0 * EPSMCH;
            NUMF = 0;
            U = ALPHA;
            FU = F;
            FMIN = F;
            GU = GTP;
            RMU = 1.0E-4;
            // C
            // C      FIRST ENTRY SETS UP THE INITIAL INTERVAL OF UNCERTAINTY.
            // C
            IENTRY = 1;
        LABEL10:;
            // C
            // C TEST FOR TOO MANY ITERATIONS
            // C
            ITCNT += 1;
            IFLAG = 1;
            if (ITCNT > 20) goto LABEL50;
            IFLAG = 0;
            this._getptc.Run(BIG, SMALL, RTSMLL, ref RELTOL, ref ABSTOL, TNYTOL
                             , FPRESN, ETA, RMU, XBND, ref U, ref FU
                             , ref GU, ref XMIN, ref FMIN, ref GMIN, ref XW, ref FW
                             , ref GW, ref A, ref B, ref OLDF, ref B1, ref SCXBND
                             , ref E, ref STEP, ref FACTOR, ref BRAKTD, ref GTEST1, ref GTEST2
                             , ref TOL, ref IENTRY, ref ITEST);
            // CLSOUT
            if (LSPRNT >= NPRNT)
            {
                this._lsout.Run(IENTRY, ITEST, XMIN, FMIN, GMIN, XW
                                , FW, GW, U, A, B, TOL
                                , RELTOL, SCXBND, XBND);
            }
            // C
            // C      IF ITEST=1, THE ALGORITHM REQUIRES THE FUNCTION VALUE TO BE
            // C      CALCULATED.
            // C
            if (ITEST != 1) goto LABEL30;
            UALPHA = XMIN + U;
            L = LX;
            for (I = 1; I <= N; I++)
            {
                W[L + o_w] = X[I + o_x] + UALPHA * P[I + o_p];
                L += 1;
            }
            SFUN.Run(N, W, LX + o_w, ref FU, ref W, LG + o_w);
            NUMF += 1;
            GU = this._ddot.Run(N, W, LG + o_w, 1, P, offset_p, 1);
            // C
            // C      THE GRADIENT VECTOR CORRESPONDING TO THE BEST POINT IS
            // C      OVERWRITTEN IF FU IS LESS THAN FMIN AND FU IS SUFFICIENTLY
            // C      LOWER THAN F AT THE ORIGIN.
            // C
            if (FU <= FMIN && FU <= OLDF - UALPHA * GTEST1) this._dcopy.Run(N, W, LG + o_w, 1, ref G, offset_g, 1);
            goto LABEL10;
            // C
            // C      IF ITEST=2 OR 3 A LOWER POINT COULD NOT BE FOUND
            // C
        LABEL30:;
            NFTOTL = NUMF;
            IFLAG = 1;
            if (ITEST != 0) goto LABEL50;
            // C
            // C      IF ITEST=0 A SUCCESSFUL SEARCH HAS BEEN MADE
            // C
            IFLAG = 0;
            F = FMIN;
            ALPHA = XMIN;
            for (I = 1; I <= N; I++)
            {
                X[I + o_x] += ALPHA * P[I + o_p];
            }
        LABEL50:  return;
            #endregion
        }
    }

    #endregion


    #region The Class: GETPTC
    
    // C
    // C
    public class GETPTC
    {
    
        public GETPTC()
        {
    
        }
    
        public void Run(double BIG, double SMALL, double RTSMLL, ref double RELTOL, ref double ABSTOL, double TNYTOL
                         , double FPRESN, double ETA, double RMU, double XBND, ref double U, ref double FU
                         , ref double GU, ref double XMIN, ref double FMIN, ref double GMIN, ref double XW, ref double FW
                         , ref double GW, ref double A, ref double B, ref double OLDF, ref double B1, ref double SCXBND
                         , ref double E, ref double STEP, ref double FACTOR, ref bool BRAKTD, ref double GTEST1, ref double GTEST2
                         , ref double TOL, ref int IENTRY, ref int ITEST)
        {
            #region Variables
            
            double DENOM = 0; bool CONVRG = false; double ABGMIN = 0; double ABGW = 0; double ABSR = 0; double A1 = 0; 
            double CHORDM = 0;double CHORDU = 0; double D1 = 0; double D2 = 0; double P = 0; double Q = 0; double R = 0; 
            double S = 0;double SCALE = 0; double SUMSQ = 0; double TWOTOL = 0; double XMIDPT = 0; double ZERO = 0; 
            double POINT1 = 0;double HALF = 0; double ONE = 0; double THREE = 0; double FIVE = 0; double ELEVEN = 0; 
            double DABS = 0;double DSQRT = 0; 
            #endregion
            #region Prolog
            
            // C
            // C ************************************************************
            // C GETPTC, AN ALGORITHM FOR FINDING A STEPLENGTH, CALLED REPEATEDLY BY
            // C ROUTINES WHICH REQUIRE A STEP LENGTH TO BE COMPUTED USING CUBIC
            // C INTERPOLATION. THE PARAMETERS CONTAIN INFORMATION ABOUT THE INTERVAL
            // C IN WHICH A LOWER POINT IS TO BE FOUND AND FROM THIS GETPTC COMPUTES A
            // C POINT AT WHICH THE FUNCTION CAN BE EVALUATED BY THE CALLING PROGRAM.
            // C THE VALUE OF THE INTEGER PARAMETERS IENTRY DETERMINES THE PATH TAKEN
            // C THROUGH THE CODE.
            // C ************************************************************
            // C
            // C
            // C THE FOLLOWING STANDARD FUNCTIONS AND SYSTEM FUNCTIONS ARE CALLED
            // C WITHIN GETPTC
            // C
            // C
            #endregion
            #region Body
            
            ZERO = 0.0E0;
            POINT1 = 1.0E-1;
            HALF = 5.0E-1;
            ONE = 1.0E0;
            THREE = 3.0E0;
            FIVE = 5.0E0;
            ELEVEN = 11.0E0;
            // C
            // C      BRANCH TO APPROPRIATE SECTION OF CODE DEPENDING ON THE
            // C      VALUE OF IENTRY.
            // C
            switch (IENTRY)
            {
                case 1: goto LABEL10;
                case 2: goto LABEL20;
            }
            // C
            // C      IENTRY=1
            // C      CHECK INPUT PARAMETERS
            // C
        LABEL10:  ITEST = 2;
            if (U <= ZERO || XBND <= TNYTOL || GU > ZERO) return;
            ITEST = 1;
            if (XBND < ABSTOL) ABSTOL = XBND;
            TOL = ABSTOL;
            TWOTOL = TOL + TOL;
            // C
            // C A AND B DEFINE THE INTERVAL OF UNCERTAINTY, X AND XW ARE POINTS
            // C WITH LOWEST AND SECOND LOWEST FUNCTION VALUES SO FAR OBTAINED.
            // C INITIALIZE A,SMIN,XW AT ORIGIN AND CORRESPONDING VALUES OF
            // C FUNCTION AND PROJECTION OF THE GRADIENT ALONG DIRECTION OF SEARCH
            // C AT VALUES FOR LATEST ESTIMATE AT MINIMUM.
            // C
            A = ZERO;
            XW = ZERO;
            XMIN = ZERO;
            OLDF = FU;
            FMIN = FU;
            FW = FU;
            GW = GU;
            GMIN = GU;
            STEP = U;
            FACTOR = FIVE;
            // C
            // C      THE MINIMUM HAS NOT YET BEEN BRACKETED.
            // C
            BRAKTD = false;
            // C
            // C SET UP XBND AS A BOUND ON THE STEP TO BE TAKEN. (XBND IS NOT COMPUTED
            // C EXPLICITLY BUT SCXBND IS ITS SCALED VALUE.)  SET THE UPPER BOUND
            // C ON THE INTERVAL OF UNCERTAINTY INITIALLY TO XBND + TOL(XBND).
            // C
            SCXBND = XBND;
            B = SCXBND + RELTOL * Math.Abs(SCXBND) + ABSTOL;
            E = B + B;
            B1 = B;
            // C
            // C COMPUTE THE CONSTANTS REQUIRED FOR THE TWO CONVERGENCE CRITERIA.
            // C
            GTEST1 =  - RMU * GU;
            GTEST2 =  - ETA * GU;
            // C
            // C SET IENTRY TO INDICATE THAT THIS IS THE FIRST ITERATION
            // C
            IENTRY = 2;
            goto LABEL210;
            // C
            // C IENTRY = 2
            // C
            // C UPDATE A,B,XW, AND XMIN
            // C
        LABEL20:  
            if (FU > FMIN) goto LABEL60;
            // C
            // C IF FUNCTION VALUE NOT INCREASED, NEW POINT BECOMES NEXT
            // C ORIGIN AND OTHER POINTS ARE SCALED ACCORDINGLY.
            // C
            CHORDU = OLDF - (XMIN + U) * GTEST1;
            if (FU <= CHORDU) goto LABEL30;
            // C
            // C THE NEW FUNCTION VALUE DOES NOT SATISFY THE SUFFICIENT DECREASE
            // C CRITERION. PREPARE TO MOVE THE UPPER BOUND TO THIS POINT AND
            // C FORCE THE INTERPOLATION SCHEME TO EITHER BISECT THE INTERVAL OF
            // C UNCERTAINTY OR TAKE THE LINEAR INTERPOLATION STEP WHICH ESTIMATES
            // C THE ROOT OF F(ALPHA)=CHORD(ALPHA).
            // C
            CHORDM = OLDF - XMIN * GTEST1;
            GU =  - GMIN;
            DENOM = CHORDM - FMIN;
            if (Math.Abs(DENOM) >= 1.0E-15) goto LABEL25;
            DENOM = 1.0E-15;
            if (CHORDM - FMIN < 0.0E0) DENOM =  - DENOM;
        LABEL25:;
            if (XMIN != ZERO) GU = GMIN * (CHORDU - FU) / DENOM;
            FU = HALF * U * (GMIN + GU) + FMIN;
            if (FU < FMIN) FU = FMIN;
            goto LABEL60;
        LABEL30:  FW = FMIN;
            FMIN = FU;
            GW = GMIN;
            GMIN = GU;
            XMIN += U;
            A -= U;
            B -= U;
            XW =  - U;
            SCXBND -= U;
            if (GU <= ZERO) goto LABEL40;
            B = ZERO;
            BRAKTD = true;
            goto LABEL50;
        LABEL40:  A = ZERO;
        LABEL50:  TOL = Math.Abs(XMIN) * RELTOL + ABSTOL;
            goto LABEL90;
            // C
            // C IF FUNCTION VALUE INCREASED, ORIGIN REMAINS UNCHANGED
            // C BUT NEW POINT MAY NOW QUALIFY AS W.
            // C
        LABEL60:  
            if (U < ZERO) goto LABEL70;
            B = U;
            BRAKTD = true;
            goto LABEL80;
        LABEL70:  A = U;
        LABEL80:  XW = U;
            FW = FU;
            GW = GU;
        LABEL90:  TWOTOL = TOL + TOL;
            XMIDPT = HALF * (A + B);
            // C
            // C CHECK TERMINATION CRITERIA
            // C
            CONVRG = Math.Abs(XMIDPT) <= TWOTOL - HALF * (B - A) || Math.Abs(GMIN) <= GTEST2 && FMIN < OLDF && (Math.Abs(XMIN - XBND) > TOL || !BRAKTD);
            if (!CONVRG) goto LABEL100;
            ITEST = 0;
            if (XMIN != ZERO) return;
            // C
            // C IF THE FUNCTION HAS NOT BEEN REDUCED, CHECK TO SEE THAT THE RELATIVE
            // C CHANGE IN F(X) IS CONSISTENT WITH THE ESTIMATE OF THE DELTA-
            // C UNIMODALITY CONSTANT, TOL.  IF THE CHANGE IN F(X) IS LARGER THAN
            // C EXPECTED, REDUCE THE VALUE OF TOL.
            // C
            ITEST = 3;
            if (Math.Abs(OLDF - FW) <= FPRESN * (ONE + Math.Abs(OLDF))) return;
            TOL *= POINT1;
            if (TOL < TNYTOL) return;
            RELTOL *= POINT1;
            ABSTOL *= POINT1;
            TWOTOL *= POINT1;
            // C
            // C CONTINUE WITH THE COMPUTATION OF A TRIAL STEP LENGTH
            // C
        LABEL100:  R = ZERO;
            Q = ZERO;
            S = ZERO;
            if (Math.Abs(E) <= TOL) goto LABEL150;
            // C
            // C FIT CUBIC THROUGH XMIN AND XW
            // C
            R = THREE * (FMIN - FW) / XW + GMIN + GW;
            ABSR = Math.Abs(R);
            Q = ABSR;
            if (GW == ZERO || GMIN == ZERO) goto LABEL140;
            // C
            // C COMPUTE THE SQUARE ROOT OF (R*R - GMIN*GW) IN A WAY
            // C WHICH AVOIDS UNDERFLOW AND OVERFLOW.
            // C
            ABGW = Math.Abs(GW);
            ABGMIN = Math.Abs(GMIN);
            S = Math.Sqrt(ABGMIN) * Math.Sqrt(ABGW);
            if ((GW / ABGW) * GMIN > ZERO) goto LABEL130;
            // C
            // C COMPUTE THE SQUARE ROOT OF R*R + S*S.
            // C
            SUMSQ = ONE;
            P = ZERO;
            if (ABSR >= S) goto LABEL110;
            // C
            // C THERE IS A POSSIBILITY OF OVERFLOW.
            // C
            if (S > RTSMLL) P = S * RTSMLL;
            if (ABSR >= P) SUMSQ = ONE + Math.Pow(ABSR / S,2);
            SCALE = S;
            goto LABEL120;
            // C
            // C THERE IS A POSSIBILITY OF UNDERFLOW.
            // C
        LABEL110:  
            if (ABSR > RTSMLL) P = ABSR * RTSMLL;
            if (S >= P) SUMSQ = ONE + Math.Pow(S / ABSR,2);
            SCALE = ABSR;
        LABEL120:  SUMSQ = Math.Sqrt(SUMSQ);
            Q = BIG;
            if (SCALE < BIG / SUMSQ) Q = SCALE * SUMSQ;
            goto LABEL140;
            // C
            // C COMPUTE THE SQUARE ROOT OF R*R - S*S
            // C
        LABEL130:  Q = Math.Sqrt(Math.Abs(R + S)) * Math.Sqrt(Math.Abs(R - S));
            if (R >= S || R <= ( - S)) goto LABEL140;
            R = ZERO;
            Q = ZERO;
            goto LABEL150;
            // C
            // C COMPUTE THE MINIMUM OF FITTED CUBIC
            // C
        LABEL140:  
            if (XW < ZERO) Q =  - Q;
            S = XW * (GMIN - R - Q);
            Q = GW - GMIN + Q + Q;
            if (Q > ZERO) S =  - S;
            if (Q <= ZERO) Q =  - Q;
            R = E;
            if (B1 != STEP || BRAKTD) E = STEP;
            // C
            // C CONSTRUCT AN ARTIFICIAL BOUND ON THE ESTIMATED STEPLENGTH
            // C
        LABEL150:  A1 = A;
            B1 = B;
            STEP = XMIDPT;
            if (BRAKTD) goto LABEL160;
            STEP =  - FACTOR * XW;
            if (STEP > SCXBND) STEP = SCXBND;
            if (STEP != SCXBND) FACTOR *= FIVE;
            goto LABEL170;
            // C
            // C IF THE MINIMUM IS BRACKETED BY 0 AND XW THE STEP MUST LIE
            // C WITHIN (A,B).
            // C
        LABEL160:  
            if ((A != ZERO || XW >= ZERO) && (B != ZERO || XW <= ZERO)) goto LABEL180;
            // C
            // C IF THE MINIMUM IS NOT BRACKETED BY 0 AND XW THE STEP MUST LIE
            // C WITHIN (A1,B1).
            // C
            D1 = XW;
            D2 = A;
            if (A == ZERO) D2 = B;
            // C THIS LINE MIGHT BE
            // C     IF (A .EQ. ZERO) D2 = E
            U =  - D1 / D2;
            STEP = FIVE * D2 * (POINT1 + ONE / U) / ELEVEN;
            if (U < ONE) STEP = HALF * D2 * Math.Sqrt(U);
        LABEL170:  
            if (STEP <= ZERO) A1 = STEP;
            if (STEP > ZERO) B1 = STEP;
            // C
            // C REJECT THE STEP OBTAINED BY INTERPOLATION IF IT LIES OUTSIDE THE
            // C REQUIRED INTERVAL OR IT IS GREATER THAN HALF THE STEP OBTAINED
            // C DURING THE LAST-BUT-ONE ITERATION.
            // C
        LABEL180:  
            if (Math.Abs(S) <= Math.Abs(HALF * Q * R) || S <= Q * A1 || S >= Q * B1) goto LABEL200;
            // C
            // C A CUBIC INTERPOLATION STEP
            // C
            STEP = S / Q;
            // C
            // C THE FUNCTION MUST NOT BE EVALUTATED TOO CLOSE TO A OR B.
            // C
            if (STEP - A >= TWOTOL && B - STEP >= TWOTOL) goto LABEL210;
            if (XMIDPT > ZERO) goto LABEL190;
            STEP =  - TOL;
            goto LABEL210;
        LABEL190:  STEP = TOL;
            goto LABEL210;
        LABEL200:  E = B - A;
            // C
            // C IF THE STEP IS TOO LARGE, REPLACE BY THE SCALED BOUND (SO AS TO
            // C COMPUTE THE NEW POINT ON THE BOUNDARY).
            // C
        LABEL210:  
            if (STEP < SCXBND) goto LABEL220;
            STEP = SCXBND;
            // C
            // C MOVE SXBD TO THE LEFT SO THAT SBND + TOL(XBND) = XBND.
            // C
            SCXBND +=  - (RELTOL * Math.Abs(XBND) + ABSTOL) / (ONE + RELTOL);
        LABEL220:  U = STEP;
            if (Math.Abs(STEP) < TOL && STEP < ZERO) U =  - TOL;
            if (Math.Abs(STEP) < TOL && STEP >= ZERO) U = TOL;
            ITEST = 1;
            return;
            #endregion
        }
    }

    #endregion


    #region The Class: DDOT
    
    // C//GO.SYSIN DD tn.f
    // Cecho blas.f 1>&2
    // Csed >blas.f <<'//GO.SYSIN DD blas.f' 's/^-//'
    // C%% TRUNCATED-NEWTON METHOD: BLAS
    // C   NOTE: ALL ROUTINES HERE ARE FROM LINPACK WITH THE EXCEPTION
    // C         OF DXPY (A VERSION OF DAXPY WITH A=1.0)
    // C   WRITTEN BY:  STEPHEN G. NASH
    // C                OPERATIONS RESEARCH AND APPLIED STATISTICS DEPT.
    // C                GEORGE MASON UNIVERSITY
    // C                FAIRFAX, VA 22030
    // C****************************************************************
    public class DDOT
    {
    
        public DDOT()
        {
    
        }
    
        public double Run(int N, double[] DX, int offset_dx, int INCX, double[] DY, int offset_dy, int INCY)
        {
        double ddot = 0;
            #region Variables
            
            double DTEMP = 0; int I = 0; int IX = 0; int IY = 0; int M = 0; int MP1 = 0; 
            #endregion
            #region Array Index Correction
            
             int o_dx = -1 + offset_dx;  int o_dy = -1 + offset_dy; 
            #endregion
            // C
            // C     FORMS THE DOT PRODUCT OF TWO VECTORS.
            // C     USES UNROLLED LOOPS FOR INCREMENTS EQUAL TO ONE.
            // C     JACK DONGARRA, LINPACK, 3/11/78.
            // C
            // C
            #region Body
            
            ddot = 0.0E0;
            DTEMP = 0.0E0;
            if (N <= 0) return ddot;
            if (INCX == 1 && INCY == 1) goto LABEL20;
            // C
            // C        CODE FOR UNEQUAL INCREMENTS OR EQUAL INCREMENTS
            // C          NOT EQUAL TO 1
            // C
            IX = 1;
            IY = 1;
            if (INCX < 0) IX = ( - N + 1) * INCX + 1;
            if (INCY < 0) IY = ( - N + 1) * INCY + 1;
            for (I = 1; I <= N; I++)
            {
                DTEMP += DX[IX + o_dx] * DY[IY + o_dy];
                IX += INCX;
                IY += INCY;
            }
            ddot = DTEMP;
            return ddot;
            // C
            // C        CODE FOR BOTH INCREMENTS EQUAL TO 1
            // C
            // C
            // C        CLEAN-UP LOOP
            // C
        LABEL20:  M = FortranLib.Mod(N,5);
            if (M == 0) goto LABEL40;
            for (I = 1; I <= M; I++)
            {
                DTEMP += DX[I + o_dx] * DY[I + o_dy];
            }
            if (N < 5) goto LABEL60;
        LABEL40:  MP1 = M + 1;
            for (I = MP1; I <= N; I += 5)
            {
                DTEMP += DX[I + o_dx] * DY[I + o_dy] + DX[I + 1 + o_dx] * DY[I + 1 + o_dy] + DX[I + 2 + o_dx] * DY[I + 2 + o_dy] + DX[I + 3 + o_dx] * DY[I + 3 + o_dy] + DX[I + 4 + o_dx] * DY[I + 4 + o_dy];
            }
        LABEL60:  ddot = DTEMP;
            return ddot;
            #endregion
        }
    }

    #endregion


    #region The Class: DAXPY
    
    public class DAXPY
    {
    
        public DAXPY()
        {
    
        }
    
        public void Run(int N, double DA, double[] DX, int offset_dx, int INCX, ref double[] DY, int offset_dy, int INCY)
        {
            #region Variables
            
            int I = 0; int IX = 0; int IY = 0; int M = 0; int MP1 = 0; 
            #endregion
            #region Array Index Correction
            
             int o_dx = -1 + offset_dx;  int o_dy = -1 + offset_dy; 
            #endregion
            // C
            // C     CONSTANT TIMES A VECTOR PLUS A VECTOR.
            // C     USES UNROLLED LOOPS FOR INCREMENTS EQUAL TO ONE.
            // C     JACK DONGARRA, LINPACK, 3/11/78.
            // C
            // C
            #region Body
            
            if (N <= 0) return;
            if (DA == 0.0E0) return;
            if (INCX == 1 && INCY == 1) goto LABEL20;
            // C
            // C        CODE FOR UNEQUAL INCREMENTS OR EQUAL INCREMENTS
            // C          NOT EQUAL TO 1
            // C
            IX = 1;
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
            // C        CODE FOR BOTH INCREMENTS EQUAL TO 1
            // C
            // C
            // C        CLEAN-UP LOOP
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
            #endregion
        }
    }

    #endregion


    #region The Class: DNRM2
    
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
            
            // C
            // C     EUCLIDEAN NORM OF THE N-VECTOR STORED IN DX() WITH STORAGE
            // C     INCREMENT INCX .
            // C     IF    N .LE. 0 RETURN WITH RESULT = 0.
            // C     IF N .GE. 1 THEN INCX MUST BE .GE. 1
            // C
            // C           C.L.LAWSON, 1978 JAN 08
            // C
            // C     FOUR PHASE METHOD     USING TWO BUILT-IN CONSTANTS THAT ARE
            // C     HOPEFULLY APPLICABLE TO ALL MACHINES.
            // C         CUTLO = MAXIMUM OF  DSQRT(U/EPS)  OVER ALL KNOWN MACHINES.
            // C         CUTHI = MINIMUM OF  DSQRT(V)      OVER ALL KNOWN MACHINES.
            // C     WHERE
            // C         EPS = SMALLEST NO. SUCH THAT EPS + 1. .GT. 1.
            // C         U   = SMALLEST POSITIVE NO.   (UNDERFLOW LIMIT)
            // C         V   = LARGEST  NO.            (OVERFLOW  LIMIT)
            // C
            // C     BRIEF OUTLINE OF ALGORITHM..
            // C
            // C     PHASE 1    SCANS ZERO COMPONENTS.
            // C     MOVE TO PHASE 2 WHEN A COMPONENT IS NONZERO AND .LE. CUTLO
            // C     MOVE TO PHASE 3 WHEN A COMPONENT IS .GT. CUTLO
            // C     MOVE TO PHASE 4 WHEN A COMPONENT IS .GE. CUTHI/M
            // C     WHERE M = N FOR X() REAL AND M = 2*N FOR COMPLEX.
            // C
            // C     VALUES FOR CUTLO AND CUTHI..
            // C     FROM THE ENVIRONMENTAL PARAMETERS LISTED IN THE IMSL CONVERTER
            // C     DOCUMENT THE LIMITING VALUES ARE AS FOLLOWS..
            // C     CUTLO, S.P.   U/EPS = 2**(-102) FOR  HONEYWELL.  CLOSE SECONDS ARE
            // C                   UNIVAC AND DEC AT 2**(-103)
            // C                   THUS CUTLO = 2**(-51) = 4.44089E-16
            // C     CUTHI, S.P.   V = 2**127 FOR UNIVAC, HONEYWELL, AND DEC.
            // C                   THUS CUTHI = 2**(63.5) = 1.30438E19
            // C     CUTLO, D.P.   U/EPS = 2**(-67) FOR HONEYWELL AND DEC.
            // C                   THUS CUTLO = 2**(-33.5) = 8.23181D-11
            // C     CUTHI, D.P.   SAME AS S.P.  CUTHI = 1.30438D19
            // C     DATA CUTLO, CUTHI / 8.232D-11,  1.304D19 /
            // C     DATA CUTLO, CUTHI / 4.441E-16,  1.304E19 /
            // C
            #endregion
            #region Body
            
            if (N > 0) goto LABEL10;
            dnrm2 = ZERO;
            goto LABEL300;
            // C
        LABEL10:  NEXT = 30;
            SUM = ZERO;
            NN = N * INCX;
            // C                                                 BEGIN MAIN LOOP
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
            // C
            // C                  PREPARE FOR PHASE 3.
            // C
        LABEL75:  SUM = (SUM * XMAX) * XMAX;
            // C
            // C
            // C     FOR REAL OR D.P. SET HITEST = CUTHI/N
            // C     FOR COMPLEX      SET HITEST = CUTHI/(2*N)
            // C
        LABEL85:  HITEST = CUTHI / Convert.ToSingle(N);
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


    #region The Class: DCOPY
    
    public class DCOPY
    {
    
        public DCOPY()
        {
    
        }
    
        public void Run(int N, double[] DX, int offset_dx, int INCX, ref double[] DY, int offset_dy, int INCY)
        {
            #region Variables
            
            int I = 0; int IX = 0; int IY = 0; int M = 0; int MP1 = 0; 
            #endregion
            #region Array Index Correction
            
             int o_dx = -1 + offset_dx;  int o_dy = -1 + offset_dy; 
            #endregion
            // C
            // C     COPIES A VECTOR, X, TO A VECTOR, Y.
            // C     USES UNROLLED LOOPS FOR INCREMENTS EQUAL TO ONE.
            // C     JACK DONGARRA, LINPACK, 3/11/78.
            // C
            // C
            #region Body
            
            if (N <= 0) return;
            if (INCX == 1 && INCY == 1) goto LABEL20;
            // C
            // C        CODE FOR UNEQUAL INCREMENTS OR EQUAL INCREMENTS
            // C          NOT EQUAL TO 1
            // C
            IX = 1;
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
            // C        CODE FOR BOTH INCREMENTS EQUAL TO 1
            // C
            // C
            // C        CLEAN-UP LOOP
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
            #endregion
        }
    }

    #endregion


    #region The Class: DXPY
    
    // C******************************************************************
    // C SPECIAL BLAS FOR Y = X+Y
    // C******************************************************************
    public class DXPY
    {
    
        public DXPY()
        {
    
        }
    
        public void Run(int N, double[] DX, int offset_dx, int INCX, ref double[] DY, int offset_dy, int INCY)
        {
            #region Variables
            
            int I = 0; int IX = 0; int IY = 0; int M = 0; int MP1 = 0; 
            #endregion
            #region Array Index Correction
            
             int o_dx = -1 + offset_dx;  int o_dy = -1 + offset_dy; 
            #endregion
            // C
            // C     VECTOR PLUS A VECTOR.
            // C     USES UNROLLED LOOPS FOR INCREMENTS EQUAL TO ONE.
            // C     STEPHEN G. NASH 5/30/89.
            // C
            // C
            #region Body
            
            if (N <= 0) return;
            if (INCX == 1 && INCY == 1) goto LABEL20;
            // C
            // C        CODE FOR UNEQUAL INCREMENTS OR EQUAL INCREMENTS
            // C          NOT EQUAL TO 1
            // C
            IX = 1;
            IY = 1;
            if (INCX < 0) IX = ( - N + 1) * INCX + 1;
            if (INCY < 0) IY = ( - N + 1) * INCY + 1;
            for (I = 1; I <= N; I++)
            {
                DY[IY + o_dy] += DX[IX + o_dx];
                IX += INCX;
                IY += INCY;
            }
            return;
            // C
            // C        CODE FOR BOTH INCREMENTS EQUAL TO 1
            // C
            // C
            // C        CLEAN-UP LOOP
            // C
        LABEL20:  M = FortranLib.Mod(N,4);
            if (M == 0) goto LABEL40;
            for (I = 1; I <= M; I++)
            {
                DY[I + o_dy] += DX[I + o_dx];
            }
            if (N < 4) return;
        LABEL40:  MP1 = M + 1;
            for (I = MP1; I <= N; I += 4)
            {
                DY[I + o_dy] += DX[I + o_dx];
                DY[I + 1 + o_dy] += DX[I + 1 + o_dx];
                DY[I + 2 + o_dy] += DX[I + 2 + o_dx];
                DY[I + 3 + o_dy] += DX[I + 3 + o_dx];
            }
            return;
            #endregion
        }
    }

    #endregion

    
}
