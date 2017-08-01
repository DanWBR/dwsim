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

namespace DotNumerics.ODE.Dopri5
{

    #region The Class: DOPRI5
    
    public class DOPRI5
    {
    
        #region Dependencies
        
        DOPCOR _dopcor; 
        #endregion
        public DOPRI5(DOPCOR dopcor)
        {
    
            #region Set Dependencies
            
            this._dopcor = dopcor; 
            #endregion
        }
    
        public DOPRI5()
        {
    
            #region Initialization Common Blocks
            
            CommonBlock CONDO5 = new CommonBlock(2, 0, 0, 0);
            #endregion
            #region Dependencies (Initialization)
            
            HINIT hinit = new HINIT();
            CDOPRI cdopri = new CDOPRI();
            DOPCOR dopcor = new DOPCOR(hinit, cdopri, CONDO5);
            #endregion
            #region Set Dependencies
            
            this._dopcor = dopcor; 
            #endregion
        }
        /// <param name="N">
        /// DIMENSION OF THE SYSTEM 
        ///</param>
        /// <param name="FCN">
        /// NAME (EXTERNAL) OF SUBROUTINE COMPUTING THE
        /// VALUE OF F(X,Y):
        /// SUBROUTINE FCN(N,X,Y,F,RPAR,IPAR)
        /// DOUBLE PRECISION X,Y(N),F(N)
        /// F(1)=...   ETC.
        ///</param>
        /// <param name="X">
        /// INITIAL X-VALUE
        ///</param>
        /// <param name="XEND">
        /// FINAL X-VALUE (XEND-X MAY BE POSITIVE OR NEGATIVE)
        ///</param>
        /// <param name="ITOL">
        /// SWITCH FOR RTOL AND ATOL:
        /// ITOL=0: BOTH RTOL AND ATOL ARE SCALARS.
        /// THE CODE KEEPS, ROUGHLY, THE LOCAL ERROR OF
        /// Y(I) BELOW RTOL*ABS(Y(I))+ATOL
        /// ITOL=1: BOTH RTOL AND ATOL ARE VECTORS.
        /// THE CODE KEEPS THE LOCAL ERROR OF Y(I) BELOW
        /// RTOL(I)*ABS(Y(I))+ATOL(I).
        ///</param>
        /// <param name="SOLOUT">
        /// NAME (EXTERNAL) OF SUBROUTINE PROVIDING THE
        /// NUMERICAL SOLUTION DURING INTEGRATION. 
        /// IF IOUT.GE.1, IT IS CALLED AFTER EVERY SUCCESSFUL STEP.
        /// SUPPLY A DUMMY SUBROUTINE IF IOUT=0. 
        /// IT MUST HAVE THE FORM
        /// SUBROUTINE SOLOUT (NR,XOLD,X,Y,N,CON,ICOMP,ND,
        /// RPAR,IPAR,IRTRN)
        /// DIMENSION Y(N),CON(5*ND),ICOMP(ND)
        /// ....  
        /// SOLOUT FURNISHES THE SOLUTION "Y" AT THE NR-TH
        /// GRID-POINT "X" (THEREBY THE INITIAL VALUE IS
        /// THE FIRST GRID-POINT).
        /// "XOLD" IS THE PRECEEDING GRID-POINT.
        /// "IRTRN" SERVES TO INTERRUPT THE INTEGRATION. IF IRTRN
        /// IS SET .LT.0, DOPRI5 WILL RETURN TO THE CALLING PROGRAM.
        /// IF THE NUMERICAL SOLUTION IS ALTERED IN SOLOUT,
        /// SET  IRTRN = 2
        /// 
        /// -----  CONTINUOUS OUTPUT: -----
        /// DURING CALLS TO "SOLOUT", A CONTINUOUS SOLUTION
        /// FOR THE INTERVAL [XOLD,X] IS AVAILABLE THROUGH
        /// THE FUNCTION
        /// .GT..GT..GT.   CONTD5(I,S,CON,ICOMP,ND)   .LT..LT..LT.
        /// WHICH PROVIDES AN APPROXIMATION TO THE I-TH
        /// COMPONENT OF THE SOLUTION AT THE POINT S. THE VALUE
        /// S SHOULD LIE IN THE INTERVAL [XOLD,X].
        ///</param>
        /// <param name="IOUT">
        /// SWITCH FOR CALLING THE SUBROUTINE SOLOUT:
        /// IOUT=0: SUBROUTINE IS NEVER CALLED
        /// IOUT=1: SUBROUTINE IS USED FOR OUTPUT.
        /// IOUT=2: DENSE OUTPUT IS PERFORMED IN SOLOUT
        /// (IN THIS CASE WORK(5) MUST BE SPECIFIED)
        ///</param>
        /// <param name="WORK">
        /// ARRAY OF WORKING SPACE OF LENGTH "LWORK".
        /// WORK(1),...,WORK(20) SERVE AS PARAMETERS FOR THE CODE.
        /// FOR STANDARD USE, SET THEM TO ZERO BEFORE CALLING.
        /// "LWORK" MUST BE AT LEAST  8*N+5*NRDENS+21
        /// WHERE  NRDENS = IWORK(5)
        ///</param>
        /// <param name="LWORK">
        /// DECLARED LENGHT OF ARRAY "WORK".
        ///</param>
        /// <param name="IWORK">
        /// INTEGER WORKING SPACE OF LENGHT "LIWORK".
        /// IWORK(1),...,IWORK(20) SERVE AS PARAMETERS FOR THE CODE.
        /// FOR STANDARD USE, SET THEM TO ZERO BEFORE CALLING.
        /// "LIWORK" MUST BE AT LEAST NRDENS+21 .
        ///</param>
        /// <param name="LIWORK">
        /// DECLARED LENGHT OF ARRAY "IWORK".
        ///</param>
        /// <param name="IDID">
        /// REPORTS ON SUCCESSFULNESS UPON RETURN:
        /// IDID= 1  COMPUTATION SUCCESSFUL,
        /// IDID= 2  COMPUT. SUCCESSFUL (INTERRUPTED BY SOLOUT)
        /// IDID=-1  INPUT IS NOT CONSISTENT,
        /// IDID=-2  LARGER NMAX IS NEEDED,
        /// IDID=-3  STEP SIZE BECOMES TOO SMALL.
        /// IDID=-4  PROBLEM IS PROBABLY STIFF (INTERRUPTED).
        ///</param>
        public void Run(int N, IFAREN FCN, ref double X, ref double[] Y, int offset_y, double XEND, double[] RTOL, int offset_rtol
                         , double[] ATOL, int offset_atol, int ITOL, ISOLOUT SOLOUT, int IOUT, ref double[] WORK, int offset_work, int LWORK
                         , ref int[] IWORK, int offset_iwork, int LIWORK, double[] RPAR, int offset_rpar, int[] IPAR, int offset_ipar, ref int IDID)
        {
            #region Variables
            
            bool ARRET = false; 
            #endregion
            #region Implicit Variables
            
            int NFCN = 0; int NSTEP = 0; int NACCPT = 0; int NREJCT = 0; int IPRINT = 0; int NMAX = 0; int METH = 0; 
            int NSTIFF = 0;int NRDENS = 0; int I = 0; double UROUND = 0; double SAFE = 0; double FAC1 = 0; double FAC2 = 0; 
            double BETA = 0;double HMAX = 0; double H = 0; int IEY1 = 0; int IEK1 = 0; int IEK2 = 0; int IEK3 = 0; int IEK4 = 0; 
            int IEK5 = 0;int IEK6 = 0; int IEYS = 0; int IECO = 0; int ISTORE = 0; int ICOMP = 0; 
            #endregion
            #region Array Index Correction
            
             int o_y = -1 + offset_y;  int o_rtol = -1 + offset_rtol;  int o_atol = -1 + offset_atol; 
             int o_work = -1 + offset_work; int o_iwork = -1 + offset_iwork;  int o_rpar = -1 + offset_rpar; 
             int o_ipar = -1 + offset_ipar;
            #endregion
            #region Prolog
            
            // C ----------------------------------------------------------
            // C     NUMERICAL SOLUTION OF A SYSTEM OF FIRST 0RDER
            // C     ORDINARY DIFFERENTIAL EQUATIONS  Y'=F(X,Y).
            // C     THIS IS AN EXPLICIT RUNGE-KUTTA METHOD OF ORDER (4)5  
            // C     DUE TO DORMAND & PRINCE (WITH STEPSIZE CONTROL AND
            // C     DENSE OUTPUT).
            // C
            // C     AUTHORS: E. HAIRER AND G. WANNER
            // C              UNIVERSITE DE GENEVE, DEPT. DE MATHEMATIQUES
            // C              CH-1211 GENEVE 24, SWITZERLAND 
            // C              E-MAIL:  Ernst.Hairer@math.unige.ch
            // C                       Gerhard.Wanner@math.unige.ch
            // C     
            // C     THIS CODE IS DESCRIBED IN:
            // C         E. HAIRER, S.P. NORSETT AND G. WANNER, SOLVING ORDINARY
            // C         DIFFERENTIAL EQUATIONS I. NONSTIFF PROBLEMS. 2ND EDITION.
            // C         SPRINGER SERIES IN COMPUTATIONAL MATHEMATICS,
            // C         SPRINGER-VERLAG (1993)               
            // C      
            // C     VERSION OF APRIL 25, 1996
            // C     (latest correction of a small bug: August 8, 2005)
            // C
            // C     INPUT PARAMETERS  
            // C     ----------------  
            // C     N           DIMENSION OF THE SYSTEM 
            // C
            // C     FCN         NAME (EXTERNAL) OF SUBROUTINE COMPUTING THE
            // C                 VALUE OF F(X,Y):
            // C                    SUBROUTINE FCN(N,X,Y,F,RPAR,IPAR)
            // C                    DOUBLE PRECISION X,Y(N),F(N)
            // C                    F(1)=...   ETC.
            // C
            // C     X           INITIAL X-VALUE
            // C
            // C     Y(N)        INITIAL VALUES FOR Y
            // C
            // C     XEND        FINAL X-VALUE (XEND-X MAY BE POSITIVE OR NEGATIVE)
            // C
            // C     RTOL,ATOL   RELATIVE AND ABSOLUTE ERROR TOLERANCES. THEY
            // C                 CAN BE BOTH SCALARS OR ELSE BOTH VECTORS OF LENGTH N.
            // C
            // C     ITOL        SWITCH FOR RTOL AND ATOL:
            // C                   ITOL=0: BOTH RTOL AND ATOL ARE SCALARS.
            // C                     THE CODE KEEPS, ROUGHLY, THE LOCAL ERROR OF
            // C                     Y(I) BELOW RTOL*ABS(Y(I))+ATOL
            // C                   ITOL=1: BOTH RTOL AND ATOL ARE VECTORS.
            // C                     THE CODE KEEPS THE LOCAL ERROR OF Y(I) BELOW
            // C                     RTOL(I)*ABS(Y(I))+ATOL(I).
            // C
            // C     SOLOUT      NAME (EXTERNAL) OF SUBROUTINE PROVIDING THE
            // C                 NUMERICAL SOLUTION DURING INTEGRATION. 
            // C                 IF IOUT.GE.1, IT IS CALLED AFTER EVERY SUCCESSFUL STEP.
            // C                 SUPPLY A DUMMY SUBROUTINE IF IOUT=0. 
            // C                 IT MUST HAVE THE FORM
            // C                    SUBROUTINE SOLOUT (NR,XOLD,X,Y,N,CON,ICOMP,ND,
            // C                                       RPAR,IPAR,IRTRN)
            // C                    DIMENSION Y(N),CON(5*ND),ICOMP(ND)
            // C                    ....  
            // C                 SOLOUT FURNISHES THE SOLUTION "Y" AT THE NR-TH
            // C                    GRID-POINT "X" (THEREBY THE INITIAL VALUE IS
            // C                    THE FIRST GRID-POINT).
            // C                 "XOLD" IS THE PRECEEDING GRID-POINT.
            // C                 "IRTRN" SERVES TO INTERRUPT THE INTEGRATION. IF IRTRN
            // C                    IS SET <0, DOPRI5 WILL RETURN TO THE CALLING PROGRAM.
            // C                    IF THE NUMERICAL SOLUTION IS ALTERED IN SOLOUT,
            // C                    SET  IRTRN = 2
            // C           
            // C          -----  CONTINUOUS OUTPUT: -----
            // C                 DURING CALLS TO "SOLOUT", A CONTINUOUS SOLUTION
            // C                 FOR THE INTERVAL [XOLD,X] IS AVAILABLE THROUGH
            // C                 THE FUNCTION
            // C                        >>>   CONTD5(I,S,CON,ICOMP,ND)   <<<
            // C                 WHICH PROVIDES AN APPROXIMATION TO THE I-TH
            // C                 COMPONENT OF THE SOLUTION AT THE POINT S. THE VALUE
            // C                 S SHOULD LIE IN THE INTERVAL [XOLD,X].
            // C
            // C     IOUT        SWITCH FOR CALLING THE SUBROUTINE SOLOUT:
            // C                    IOUT=0: SUBROUTINE IS NEVER CALLED
            // C                    IOUT=1: SUBROUTINE IS USED FOR OUTPUT.
            // C                    IOUT=2: DENSE OUTPUT IS PERFORMED IN SOLOUT
            // C                            (IN THIS CASE WORK(5) MUST BE SPECIFIED)
            // C
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
            // C     RPAR, IPAR  REAL AND INTEGER PARAMETERS (OR PARAMETER ARRAYS) WHICH  
            // C                 CAN BE USED FOR COMMUNICATION BETWEEN YOUR CALLING
            // C                 PROGRAM AND THE FCN, JAC, MAS, SOLOUT SUBROUTINES. 
            // C
            // C-----------------------------------------------------------------------
            // C 
            // C     SOPHISTICATED SETTING OF PARAMETERS
            // C     -----------------------------------
            // C              SEVERAL PARAMETERS (WORK(1),...,IWORK(1),...) ALLOW
            // C              TO ADAPT THE CODE TO THE PROBLEM AND TO THE NEEDS OF
            // C              THE USER. FOR ZERO INPUT, THE CODE CHOOSES DEFAULT VALUES.
            // C
            // C    WORK(1)   UROUND, THE ROUNDING UNIT, DEFAULT 2.3D-16.
            // C
            // C    WORK(2)   THE SAFETY FACTOR IN STEP SIZE PREDICTION,
            // C              DEFAULT 0.9D0.
            // C
            // C    WORK(3), WORK(4)   PARAMETERS FOR STEP SIZE SELECTION
            // C              THE NEW STEP SIZE IS CHOSEN SUBJECT TO THE RESTRICTION
            // C                 WORK(3) <= HNEW/HOLD <= WORK(4)
            // C              DEFAULT VALUES: WORK(3)=0.2D0, WORK(4)=10.D0
            // C
            // C    WORK(5)   IS THE "BETA" FOR STABILIZED STEP SIZE CONTROL
            // C              (SEE SECTION IV.2). LARGER VALUES OF BETA ( <= 0.1 )
            // C              MAKE THE STEP SIZE CONTROL MORE STABLE. DOPRI5 NEEDS
            // C              A LARGER BETA THAN HIGHAM & HALL. NEGATIVE WORK(5)
            // C              PROVOKE BETA=0.
            // C              DEFAULT 0.04D0.
            // C
            // C    WORK(6)   MAXIMAL STEP SIZE, DEFAULT XEND-X.
            // C
            // C    WORK(7)   INITIAL STEP SIZE, FOR WORK(7)=0.D0 AN INITIAL GUESS
            // C              IS COMPUTED WITH HELP OF THE FUNCTION HINIT
            // C
            // C    IWORK(1)  THIS IS THE MAXIMAL NUMBER OF ALLOWED STEPS.
            // C              THE DEFAULT VALUE (FOR IWORK(1)=0) IS 100000.
            // C
            // C    IWORK(2)  SWITCH FOR THE CHOICE OF THE COEFFICIENTS
            // C              IF IWORK(2).EQ.1  METHOD DOPRI5 OF DORMAND AND PRINCE
            // C              (TABLE 5.2 OF SECTION II.5).
            // C              AT THE MOMENT THIS IS THE ONLY POSSIBLE CHOICE.
            // C              THE DEFAULT VALUE (FOR IWORK(2)=0) IS IWORK(2)=1.
            // C
            // C    IWORK(3)  SWITCH FOR PRINTING ERROR MESSAGES
            // C              IF IWORK(3).LT.0 NO MESSAGES ARE BEING PRINTED
            // C              IF IWORK(3).GT.0 MESSAGES ARE PRINTED WITH
            // C              WRITE (IWORK(3),*) ...  
            // C              DEFAULT VALUE (FOR IWORK(3)=0) IS IWORK(3)=6
            // C
            // C    IWORK(4)  TEST FOR STIFFNESS IS ACTIVATED AFTER STEP NUMBER
            // C              J*IWORK(4) (J INTEGER), PROVIDED IWORK(4).GT.0.
            // C              FOR NEGATIVE IWORK(4) THE STIFFNESS TEST IS
            // C              NEVER ACTIVATED; DEFAULT VALUE IS IWORK(4)=1000
            // C
            // C    IWORK(5)  = NRDENS = NUMBER OF COMPONENTS, FOR WHICH DENSE OUTPUT
            // C              IS REQUIRED; DEFAULT VALUE IS IWORK(5)=0;
            // C              FOR   0 < NRDENS < N   THE COMPONENTS (FOR WHICH DENSE
            // C              OUTPUT IS REQUIRED) HAVE TO BE SPECIFIED IN
            // C              IWORK(21),...,IWORK(NRDENS+20);
            // C              FOR  NRDENS=N  THIS IS DONE BY THE CODE.
            // C
            // C----------------------------------------------------------------------
            // C
            // C     OUTPUT PARAMETERS 
            // C     ----------------- 
            // C     X           X-VALUE FOR WHICH THE SOLUTION HAS BEEN COMPUTED
            // C                 (AFTER SUCCESSFUL RETURN X=XEND).
            // C
            // C     Y(N)        NUMERICAL SOLUTION AT X
            // C 
            // C     H           PREDICTED STEP SIZE OF THE LAST ACCEPTED STEP
            // C
            // C     IDID        REPORTS ON SUCCESSFULNESS UPON RETURN:
            // C                   IDID= 1  COMPUTATION SUCCESSFUL,
            // C                   IDID= 2  COMPUT. SUCCESSFUL (INTERRUPTED BY SOLOUT)
            // C                   IDID=-1  INPUT IS NOT CONSISTENT,
            // C                   IDID=-2  LARGER NMAX IS NEEDED,
            // C                   IDID=-3  STEP SIZE BECOMES TOO SMALL.
            // C                   IDID=-4  PROBLEM IS PROBABLY STIFF (INTERRUPTED).
            // C
            // C   IWORK(17)  NFCN    NUMBER OF FUNCTION EVALUATIONS
            // C   IWORK(18)  NSTEP   NUMBER OF COMPUTED STEPS
            // C   IWORK(19)  NACCPT  NUMBER OF ACCEPTED STEPS
            // C   IWORK(20)  NREJCT  NUMBER OF REJECTED STEPS (DUE TO ERROR TEST),
            // C                      (STEP REJECTIONS IN THE FIRST STEP ARE NOT COUNTED)
            // C-----------------------------------------------------------------------
            // C *** *** *** *** *** *** *** *** *** *** *** *** ***
            // C          DECLARATIONS 
            // C *** *** *** *** *** *** *** *** *** *** *** *** ***
            // C *** *** *** *** *** *** ***
            // C        SETTING THE PARAMETERS 
            // C *** *** *** *** *** *** ***
            #endregion
            #region Body
            
            NFCN = 0;
            NSTEP = 0;
            NACCPT = 0;
            NREJCT = 0;
            ARRET = false;
            // C -------- IPRINT FOR MONITORING THE PRINTING
            if (IWORK[3 + o_iwork] == 0)
            {
                IPRINT = 6;
            }
            else
            {
                IPRINT = IWORK[3 + o_iwork];
            }
            // C -------- NMAX , THE MAXIMAL NUMBER OF STEPS ----- 
            if (IWORK[1 + o_iwork] == 0)
            {
                NMAX = 100000;
            }
            else
            {
                NMAX = IWORK[1 + o_iwork];
                if (NMAX <= 0)
                {
                    if (IPRINT > 0) ;//ERROR-ERRORWRITE(IPRINT,*)' WRONG INPUT IWORK(1)=',IWORK(1)
                    ARRET = true;
                }
            }
            // C -------- METH   COEFFICIENTS OF THE METHOD
            if (IWORK[2 + o_iwork] == 0)
            {
                METH = 1;
            }
            else
            {
                METH = IWORK[2 + o_iwork];
                if (METH <= 0 || METH >= 4)
                {
                    if (IPRINT > 0) ;//ERROR-ERRORWRITE(IPRINT,*)' CURIOUS INPUT IWORK(2)=',IWORK(2)
                    ARRET = true;
                }
            }
            // C -------- NSTIFF   PARAMETER FOR STIFFNESS DETECTION  
            NSTIFF = IWORK[4 + o_iwork];
            if (NSTIFF == 0) NSTIFF = 1000;
            if (NSTIFF < 0) NSTIFF = NMAX + 10;
            // C -------- NRDENS   NUMBER OF DENSE OUTPUT COMPONENTS
            NRDENS = IWORK[5 + o_iwork];
            if (NRDENS < 0 || NRDENS > N)
            {
                if (IPRINT > 0) ;//ERROR-ERRORWRITE(IPRINT,*)' CURIOUS INPUT IWORK(5)=',IWORK(5)
                ARRET = true;
            }
            else
            {
                if (NRDENS > 0 && IOUT < 2)
                {
                    if (IPRINT > 0) ;//ERROR-ERRORWRITE(IPRINT,*)' WARNING: PUT IOUT=2 FOR DENSE OUTPUT '
                }
                if (NRDENS == N)
                {
                    for (I = 1; I <= NRDENS; I++)
                    {
                        IWORK[20 + I + o_iwork] = I;
                    }
                }
            }
            // C -------- UROUND   SMALLEST NUMBER SATISFYING 1.D0+UROUND>1.D0  
            if (WORK[1 + o_work] == 0.0E0)
            {
                UROUND = 2.3E-16;
            }
            else
            {
                UROUND = WORK[1 + o_work];
                if (UROUND <= 1.0E-35 || UROUND >= 1.0E0)
                {
                    if (IPRINT > 0) ;//ERROR-ERRORWRITE(IPRINT,*)' WHICH MACHINE DO YOU HAVE? YOUR UROUND WAS:',WORK(1)
                    ARRET = true;
                }
            }
            // C -------  SAFETY FACTOR -------------
            if (WORK[2 + o_work] == 0.0E0)
            {
                SAFE = 0.9E0;
            }
            else
            {
                SAFE = WORK[2 + o_work];
                if (SAFE >= 1.0E0 || SAFE <= 1.0E-4)
                {
                    if (IPRINT > 0) ;//ERROR-ERRORWRITE(IPRINT,*)' CURIOUS INPUT FOR SAFETY FACTOR WORK(2)=',WORK(2)
                    ARRET = true;
                }
            }
            // C -------  FAC1,FAC2     PARAMETERS FOR STEP SIZE SELECTION
            if (WORK[3 + o_work] == 0.0E0)
            {
                FAC1 = 0.2E0;
            }
            else
            {
                FAC1 = WORK[3 + o_work];
            }
            if (WORK[4 + o_work] == 0.0E0)
            {
                FAC2 = 10.0E0;
            }
            else
            {
                FAC2 = WORK[4 + o_work];
            }
            // C --------- BETA FOR STEP CONTROL STABILIZATION -----------
            if (WORK[5 + o_work] == 0.0E0)
            {
                BETA = 0.04E0;
            }
            else
            {
                if (WORK[5 + o_work] < 0.0E0)
                {
                    BETA = 0.0E0;
                }
                else
                {
                    BETA = WORK[5 + o_work];
                    if (BETA > 0.2E0)
                    {
                        if (IPRINT > 0) ;//ERROR-ERRORWRITE(IPRINT,*)' CURIOUS INPUT FOR BETA: WORK(5)=',WORK(5)
                        ARRET = true;
                    }
                }
            }
            // C -------- MAXIMAL STEP SIZE
            if (WORK[6 + o_work] == 0.0E0)
            {
                HMAX = XEND - X;
            }
            else
            {
                HMAX = WORK[6 + o_work];
            }
            // C -------- INITIAL STEP SIZE
            H = WORK[7 + o_work];
            // C ------- PREPARE THE ENTRY-POINTS FOR THE ARRAYS IN WORK -----
            IEY1 = 21;
            IEK1 = IEY1 + N;
            IEK2 = IEK1 + N;
            IEK3 = IEK2 + N;
            IEK4 = IEK3 + N;
            IEK5 = IEK4 + N;
            IEK6 = IEK5 + N;
            IEYS = IEK6 + N;
            IECO = IEYS + N;
            // C ------ TOTAL STORAGE REQUIREMENT -----------
            ISTORE = IEYS + 5 * NRDENS - 1;
            if (ISTORE > LWORK)
            {
                if (IPRINT > 0) ;//ERROR-ERRORWRITE(IPRINT,*)' INSUFFICIENT STORAGE FOR WORK, MIN. LWORK=',ISTORE
                ARRET = true;
            }
            ICOMP = 21;
            ISTORE = ICOMP + NRDENS - 1;
            if (ISTORE > LIWORK)
            {
                if (IPRINT > 0) ;//ERROR-ERRORWRITE(IPRINT,*)' INSUFFICIENT STORAGE FOR IWORK, MIN. LIWORK=',ISTORE
                ARRET = true;
            }
            // C ------ WHEN A FAIL HAS OCCURED, WE RETURN WITH IDID=-1
            if (ARRET)
            {
                IDID =  - 1;
                return;
            }
            // C -------- CALL TO CORE INTEGRATOR ------------
            this._dopcor.Run(N, FCN, ref X, ref Y, offset_y, XEND, ref HMAX
                             , ref H, RTOL, offset_rtol, ATOL, offset_atol, ITOL, IPRINT, SOLOUT
                             , IOUT, ref IDID, NMAX, UROUND, METH, NSTIFF
                             , SAFE, BETA, FAC1, FAC2, ref WORK, IEY1 + o_work, ref WORK, IEK1 + o_work
                             , ref WORK, IEK2 + o_work, ref WORK, IEK3 + o_work, ref WORK, IEK4 + o_work, ref WORK, IEK5 + o_work, ref WORK, IEK6 + o_work, ref WORK, IEYS + o_work
                             , ref WORK, IECO + o_work, IWORK, ICOMP + o_iwork, NRDENS, RPAR, offset_rpar, IPAR, offset_ipar, ref NFCN
                             , ref NSTEP, ref NACCPT, ref NREJCT);
            WORK[7 + o_work] = H;
            IWORK[17 + o_iwork] = NFCN;
            IWORK[18 + o_iwork] = NSTEP;
            IWORK[19 + o_iwork] = NACCPT;
            IWORK[20 + o_iwork] = NREJCT;
            // C ----------- RETURN -----------
            return;
            #endregion
        }
    }

    #endregion


    #region The Class: DOPCOR
    
    // C
    // C
    // C
    // C  ----- ... AND HERE IS THE CORE INTEGRATOR  ----------
    // C
    public class DOPCOR
    {
    
        #region Dependencies
        
        HINIT _hinit; CDOPRI _cdopri; 
        #endregion
        #region Common variables
        
        #region Common Block: CONDO5 Declaration
        
        CommonBlock _condo5;
        Odouble XOLD; Odouble HOUT; 
        #endregion
        #endregion
        public DOPCOR(HINIT hinit, CDOPRI cdopri, CommonBlock CONDO5)
        {
    
            #region Set Dependencies
            
            this._hinit = hinit; this._cdopri = cdopri; 
            #endregion
            #region Common varaible Initialization
            
            #region Common Block: CONDO5 Initialization
            
            this._condo5 = CONDO5;
            XOLD = CONDO5.doubleData[0];
            HOUT = CONDO5.doubleData[1];
            #endregion
            #endregion
        }
    
        public DOPCOR()
        {
    
            #region Initialization Common Blocks
            
            CommonBlock CONDO5 = new CommonBlock(2, 0, 0, 0);
            #endregion
            #region Dependencies (Initialization)
            
            HINIT hinit = new HINIT();
            CDOPRI cdopri = new CDOPRI();
            #endregion
            #region Set Dependencies
            
            this._hinit = hinit; this._cdopri = cdopri; 
            #endregion
            #region Common varaible Initialization
            
            #region Common Block: CONDO5 Initialization
            
            this._condo5 = CONDO5;
            XOLD = CONDO5.doubleData[0];
            HOUT = CONDO5.doubleData[1];
            #endregion
            #endregion
        }
        public void Run(int N, IFAREN FCN, ref double X, ref double[] Y, int offset_y, double XEND, ref double HMAX
                         , ref double H, double[] RTOL, int offset_rtol, double[] ATOL, int offset_atol, int ITOL, int IPRINT, ISOLOUT SOLOUT
                         , int IOUT, ref int IDID, int NMAX, double UROUND, int METH, int NSTIFF
                         , double SAFE, double BETA, double FAC1, double FAC2, ref double[] Y1, int offset_y1, ref double[] K1, int offset_k1
                         , ref double[] K2, int offset_k2, ref double[] K3, int offset_k3, ref double[] K4, int offset_k4, ref double[] K5, int offset_k5, ref double[] K6, int offset_k6, ref double[] YSTI, int offset_ysti
                         , ref double[] CONT, int offset_cont, int[] ICOMP, int offset_icomp, int NRD, double[] RPAR, int offset_rpar, int[] IPAR, int offset_ipar, ref int NFCN
                         , ref int NSTEP, ref int NACCPT, ref int NREJCT)
        {
            #region Variables
            
            bool REJECT = false; bool LAST = false; 
            #endregion
            #region Implicit Variables
            
            double FACOLD = 0; double EXPO1 = 0; double FACC1 = 0; double FACC2 = 0; double POSNEG = 0; double ATOLI = 0; 
            double RTOLI = 0;double HLAMB = 0; int IASTI = 0; int IORD = 0; int IRTRN = 0; int I = 0; double A21 = 0; 
            double A31 = 0;double A32 = 0; double A41 = 0; double A42 = 0; double A43 = 0; double A51 = 0; double A52 = 0; 
            double A53 = 0;double A54 = 0; double A61 = 0; double A62 = 0; double A63 = 0; double A64 = 0; double A65 = 0; 
            double XPH = 0;double A71 = 0; double A73 = 0; double A74 = 0; double A75 = 0; double A76 = 0; int J = 0; 
            double D1 = 0;double D3 = 0; double D4 = 0; double D5 = 0; double D6 = 0; double D7 = 0; double E1 = 0; double E3 = 0; 
            double E4 = 0;double E5 = 0; double E6 = 0; double E7 = 0; double ERR = 0; double SK = 0; double FAC11 = 0; 
            double FAC = 0;double HNEW = 0; double STNUM = 0; double STDEN = 0; int NONSTI = 0; double YD0 = 0; double YDIFF = 0; 
            double BSPL = 0;double C2 = 0; double C3 = 0; double C4 = 0; double C5 = 0; 
            #endregion
            #region Array Index Correction
            
             int o_y = -1 + offset_y;  int o_rtol = -1 + offset_rtol;  int o_atol = -1 + offset_atol;  int o_y1 = -1 + offset_y1; 
             int o_k1 = -1 + offset_k1; int o_k2 = -1 + offset_k2;  int o_k3 = -1 + offset_k3;  int o_k4 = -1 + offset_k4; 
             int o_k5 = -1 + offset_k5; int o_k6 = -1 + offset_k6;  int o_ysti = -1 + offset_ysti;  int o_cont = -1 + offset_cont; 
             int o_icomp = -1 + offset_icomp; int o_rpar = -1 + offset_rpar;  int o_ipar = -1 + offset_ipar; 
            #endregion
            // C ----------------------------------------------------------
            // C     CORE INTEGRATOR FOR DOPRI5
            // C     PARAMETERS SAME AS IN DOPRI5 WITH WORKSPACE ADDED 
            // C ---------------------------------------------------------- 
            // C         DECLARATIONS 
            // C ---------------------------------------------------------- 
            // C *** *** *** *** *** *** ***
            // C  INITIALISATIONS
            // C *** *** *** *** *** *** *** 
            #region Body
            
            if (METH == 1)
            {
                this._cdopri.Run(ref C2, ref C3, ref C4, ref C5, ref E1, ref E3
                                 , ref E4, ref E5, ref E6, ref E7, ref A21, ref A31
                                 , ref A32, ref A41, ref A42, ref A43, ref A51, ref A52
                                 , ref A53, ref A54, ref A61, ref A62, ref A63, ref A64
                                 , ref A65, ref A71, ref A73, ref A74, ref A75, ref A76
                                 , ref D1, ref D3, ref D4, ref D5, ref D6, ref D7);
            }
            FACOLD = 1.0E-4;
            EXPO1 = 0.2E0 - BETA * 0.75E0;
            FACC1 = 1.0E0 / FAC1;
            FACC2 = 1.0E0 / FAC2;
            POSNEG = FortranLib.Sign(1.0E0,XEND - X);
            // C --- INITIAL PREPARATIONS   
            ATOLI = ATOL[1 + o_atol];
            RTOLI = RTOL[1 + o_rtol];
            LAST = false;
            HLAMB = 0.0E0;
            IASTI = 0;
            FCN.Run(N, X, Y, offset_y, ref K1, offset_k1, RPAR, offset_rpar, IPAR[1 + o_ipar]);
            HMAX = Math.Abs(HMAX);
            IORD = 5;
            if (H == 0.0E0) H = this._hinit.Run(N, FCN, X, Y, offset_y, XEND, POSNEG, K1, offset_k1, ref K2, offset_k2, ref K3, offset_k3, IORD, HMAX, ATOL, offset_atol, RTOL, offset_rtol, ITOL, RPAR, offset_rpar, IPAR, offset_ipar);
            NFCN += 2;
            REJECT = false;
            XOLD.v = X;
            if (IOUT != 0)
            {
                IRTRN = 1;
                HOUT.v = H;
                SOLOUT.Run(NACCPT + 1, XOLD.v, X, Y, offset_y, N, CONT, offset_cont
                           , ICOMP, offset_icomp, NRD, RPAR, offset_rpar, IPAR[1 + o_ipar], IRTRN);
                if (IRTRN < 0) goto LABEL79;
            }
            else
            {
                IRTRN = 0;
            }
            // C --- BASIC INTEGRATION STEP  
        LABEL1:;
            if (NSTEP > NMAX) goto LABEL78;
            if (0.1E0 * Math.Abs(H) <= Math.Abs(X) * UROUND) goto LABEL77;
            if ((X + 1.01E0 * H - XEND) * POSNEG > 0.0E0)
            {
                H = XEND - X;
                LAST = true;
            }
            NSTEP += 1;
            // C --- THE FIRST 6 STAGES
            if (IRTRN >= 2)
            {
                FCN.Run(N, X, Y, offset_y, ref K1, offset_k1, RPAR, offset_rpar, IPAR[1 + o_ipar]);
            }
            for (I = 1; I <= N; I++)
            {
                Y1[I + o_y1] = Y[I + o_y] + H * A21 * K1[I + o_k1];
            }
            FCN.Run(N, X + C2 * H, Y1, offset_y1, ref K2, offset_k2, RPAR, offset_rpar, IPAR[1 + o_ipar]);
            for (I = 1; I <= N; I++)
            {
                Y1[I + o_y1] = Y[I + o_y] + H * (A31 * K1[I + o_k1] + A32 * K2[I + o_k2]);
            }
            FCN.Run(N, X + C3 * H, Y1, offset_y1, ref K3, offset_k3, RPAR, offset_rpar, IPAR[1 + o_ipar]);
            for (I = 1; I <= N; I++)
            {
                Y1[I + o_y1] = Y[I + o_y] + H * (A41 * K1[I + o_k1] + A42 * K2[I + o_k2] + A43 * K3[I + o_k3]);
            }
            FCN.Run(N, X + C4 * H, Y1, offset_y1, ref K4, offset_k4, RPAR, offset_rpar, IPAR[1 + o_ipar]);
            for (I = 1; I <= N; I++)
            {
                Y1[I + o_y1] = Y[I + o_y] + H * (A51 * K1[I + o_k1] + A52 * K2[I + o_k2] + A53 * K3[I + o_k3] + A54 * K4[I + o_k4]);
            }
            FCN.Run(N, X + C5 * H, Y1, offset_y1, ref K5, offset_k5, RPAR, offset_rpar, IPAR[1 + o_ipar]);
            for (I = 1; I <= N; I++)
            {
                YSTI[I + o_ysti] = Y[I + o_y] + H * (A61 * K1[I + o_k1] + A62 * K2[I + o_k2] + A63 * K3[I + o_k3] + A64 * K4[I + o_k4] + A65 * K5[I + o_k5]);
            }
            XPH = X + H;
            FCN.Run(N, XPH, YSTI, offset_ysti, ref K6, offset_k6, RPAR, offset_rpar, IPAR[1 + o_ipar]);
            for (I = 1; I <= N; I++)
            {
                Y1[I + o_y1] = Y[I + o_y] + H * (A71 * K1[I + o_k1] + A73 * K3[I + o_k3] + A74 * K4[I + o_k4] + A75 * K5[I + o_k5] + A76 * K6[I + o_k6]);
            }
            FCN.Run(N, XPH, Y1, offset_y1, ref K2, offset_k2, RPAR, offset_rpar, IPAR[1 + o_ipar]);
            if (IOUT >= 2)
            {
                for (J = 1; J <= NRD; J++)
                {
                    I = ICOMP[J + o_icomp];
                    CONT[4 * NRD + J + o_cont] = H * (D1 * K1[I + o_k1] + D3 * K3[I + o_k3] + D4 * K4[I + o_k4] + D5 * K5[I + o_k5] + D6 * K6[I + o_k6] + D7 * K2[I + o_k2]);
                }
            }
            for (I = 1; I <= N; I++)
            {
                K4[I + o_k4] = (E1 * K1[I + o_k1] + E3 * K3[I + o_k3] + E4 * K4[I + o_k4] + E5 * K5[I + o_k5] + E6 * K6[I + o_k6] + E7 * K2[I + o_k2]) * H;
            }
            NFCN += 6;
            // C --- ERROR ESTIMATION  
            ERR = 0.0E0;
            if (ITOL == 0)
            {
                for (I = 1; I <= N; I++)
                {
                    SK = ATOLI + RTOLI * Math.Max(Math.Abs(Y[I + o_y]), Math.Abs(Y1[I + o_y1]));
                    ERR += Math.Pow(K4[I + o_k4] / SK,2);
                }
            }
            else
            {
                for (I = 1; I <= N; I++)
                {
                    SK = ATOL[I + o_atol] + RTOL[I + o_rtol] * Math.Max(Math.Abs(Y[I + o_y]), Math.Abs(Y1[I + o_y1]));
                    ERR += Math.Pow(K4[I + o_k4] / SK,2);
                }
            }
            ERR = Math.Sqrt(ERR / N);
            // C --- COMPUTATION OF HNEW
            FAC11 = Math.Pow(ERR,EXPO1);
            // C --- LUND-STABILIZATION
            FAC = FAC11 / Math.Pow(FACOLD,BETA);
            // C --- WE REQUIRE  FAC1 <= HNEW/H <= FAC2
            FAC = Math.Max(FACC2, Math.Min(FACC1, FAC / SAFE));
            HNEW = H / FAC;
            if (ERR <= 1.0E0)
            {
                // C --- STEP IS ACCEPTED  
                FACOLD = Math.Max(ERR, 1.0E-4);
                NACCPT += 1;
                // C ------- STIFFNESS DETECTION
                if (FortranLib.Mod(NACCPT,NSTIFF) == 0 || IASTI > 0)
                {
                    STNUM = 0.0E0;
                    STDEN = 0.0E0;
                    for (I = 1; I <= N; I++)
                    {
                        STNUM += Math.Pow(K2[I + o_k2] - K6[I + o_k6],2);
                        STDEN += Math.Pow(Y1[I + o_y1] - YSTI[I + o_ysti],2);
                    }
                    if (STDEN > 0.0E0) HLAMB = H * Math.Sqrt(STNUM / STDEN);
                    if (HLAMB > 3.25E0)
                    {
                        NONSTI = 0;
                        IASTI += 1;
                        if (IASTI == 15)
                        {
                            if (IPRINT > 0) ;//ERROR-ERRORWRITE(IPRINT,*)' THE PROBLEM SEEMS TO BECOME STIFF AT X = ',X
                            if (IPRINT <= 0) goto LABEL76;
                        }
                    }
                    else
                    {
                        NONSTI += 1;
                        if (NONSTI == 6) IASTI = 0;
                    }
                }
                if (IOUT >= 2)
                {
                    for (J = 1; J <= NRD; J++)
                    {
                        I = ICOMP[J + o_icomp];
                        YD0 = Y[I + o_y];
                        YDIFF = Y1[I + o_y1] - YD0;
                        BSPL = H * K1[I + o_k1] - YDIFF;
                        CONT[J + o_cont] = Y[I + o_y];
                        CONT[NRD + J + o_cont] = YDIFF;
                        CONT[2 * NRD + J + o_cont] = BSPL;
                        CONT[3 * NRD + J + o_cont] =  - H * K2[I + o_k2] + YDIFF - BSPL;
                    }
                }
                for (I = 1; I <= N; I++)
                {
                    K1[I + o_k1] = K2[I + o_k2];
                    Y[I + o_y] = Y1[I + o_y1];
                }
                XOLD.v = X;
                X = XPH;
                if (IOUT != 0)
                {
                    HOUT.v = H;
                    SOLOUT.Run(NACCPT + 1, XOLD.v, X, Y, offset_y, N, CONT, offset_cont
                               , ICOMP, offset_icomp, NRD, RPAR, offset_rpar, IPAR[1 + o_ipar], IRTRN);
                    if (IRTRN < 0) goto LABEL79;
                }
                // C ------- NORMAL EXIT
                if (LAST)
                {
                    H = HNEW;
                    IDID = 1;
                    return;
                }
                if (Math.Abs(HNEW) > HMAX) HNEW = POSNEG * HMAX;
                if (REJECT) HNEW = POSNEG * Math.Min(Math.Abs(HNEW), Math.Abs(H));
                REJECT = false;
            }
            else
            {
                // C --- STEP IS REJECTED   
                HNEW = H / Math.Min(FACC1, FAC11 / SAFE);
                REJECT = true;
                if (NACCPT >= 1) NREJCT += 1;
                LAST = false;
            }
            H = HNEW;
            goto LABEL1;
            // C --- FAIL EXIT
        LABEL76:;
            IDID =  - 4;
            return;
        LABEL77:;
            if (IPRINT > 0) ;//ERROR-ERRORWRITE(IPRINT,979)X
            if (IPRINT > 0) ;//ERROR-ERRORWRITE(IPRINT,*)' STEP SIZE T0O SMALL, H=',H
            IDID =  - 3;
            return;
        LABEL78:;
            if (IPRINT > 0) ;//ERROR-ERRORWRITE(IPRINT,979)X
            if (IPRINT > 0) ;//ERROR-ERRORWRITE(IPRINT,*)' MORE THAN NMAX =',NMAX,'STEPS ARE NEEDED'
            IDID =  - 2;
            return;
        LABEL79:;
            if (IPRINT > 0) ;//ERROR-ERRORWRITE(IPRINT,979)X
            IDID = 2;
            return;
            #endregion
        }
    }

    #endregion


    #region The Class: HINIT
    
    // C
    public class HINIT
    {
    
        public HINIT()
        {
    
        }
    
        public double Run(int N, IFAREN FCN, double X, double[] Y, int offset_y, double XEND, double POSNEG
                           , double[] F0, int offset_f0, ref double[] F1, int offset_f1, ref double[] Y1, int offset_y1, int IORD, double HMAX, double[] ATOL, int offset_atol
                           , double[] RTOL, int offset_rtol, int ITOL, double[] RPAR, int offset_rpar, int[] IPAR, int offset_ipar)
        {
        double hinit = 0;
            #region Implicit Variables
            
            double DNF = 0; double DNY = 0; double ATOLI = 0; double RTOLI = 0; double SK = 0; int I = 0; double H = 0; 
            double DER2 = 0;double DER12 = 0; double H1 = 0; 
            #endregion
            #region Array Index Correction
            
             int o_y = -1 + offset_y;  int o_f0 = -1 + offset_f0;  int o_f1 = -1 + offset_f1;  int o_y1 = -1 + offset_y1; 
             int o_atol = -1 + offset_atol; int o_rtol = -1 + offset_rtol;  int o_rpar = -1 + offset_rpar; 
             int o_ipar = -1 + offset_ipar;
            #endregion
            // C ----------------------------------------------------------
            // C ----  COMPUTATION OF AN INITIAL STEP SIZE GUESS
            // C ----------------------------------------------------------
            // C ---- COMPUTE A FIRST GUESS FOR EXPLICIT EULER AS
            // C ----   H = 0.01 * NORM (Y0) / NORM (F0)
            // C ---- THE INCREMENT FOR EXPLICIT EULER IS SMALL
            // C ---- COMPARED TO THE SOLUTION
            #region Body
            
            DNF = 0.0E0;
            DNY = 0.0E0;
            ATOLI = ATOL[1 + o_atol];
            RTOLI = RTOL[1 + o_rtol];
            if (ITOL == 0)
            {
                for (I = 1; I <= N; I++)
                {
                    SK = ATOLI + RTOLI * Math.Abs(Y[I + o_y]);
                    DNF += Math.Pow(F0[I + o_f0] / SK,2);
                    DNY += Math.Pow(Y[I + o_y] / SK,2);
                }
            }
            else
            {
                for (I = 1; I <= N; I++)
                {
                    SK = ATOL[I + o_atol] + RTOL[I + o_rtol] * Math.Abs(Y[I + o_y]);
                    DNF += Math.Pow(F0[I + o_f0] / SK,2);
                    DNY += Math.Pow(Y[I + o_y] / SK,2);
                }
            }
            if (DNF <= 1.0E-10 || DNY <= 1.0E-10)
            {
                H = 1.0E-6;
            }
            else
            {
                H = Math.Sqrt(DNY / DNF) * 0.01E0;
            }
            H = Math.Min(H, HMAX);
            H = FortranLib.Sign(H,POSNEG);
            // C ---- PERFORM AN EXPLICIT EULER STEP
            for (I = 1; I <= N; I++)
            {
                Y1[I + o_y1] = Y[I + o_y] + H * F0[I + o_f0];
            }
            FCN.Run(N, X + H, Y1, offset_y1, ref F1, offset_f1, RPAR, offset_rpar, IPAR[1 + o_ipar]);
            // C ---- ESTIMATE THE SECOND DERIVATIVE OF THE SOLUTION
            DER2 = 0.0E0;
            if (ITOL == 0)
            {
                for (I = 1; I <= N; I++)
                {
                    SK = ATOLI + RTOLI * Math.Abs(Y[I + o_y]);
                    DER2 += Math.Pow((F1[I + o_f1] - F0[I + o_f0]) / SK,2);
                }
            }
            else
            {
                for (I = 1; I <= N; I++)
                {
                    SK = ATOL[I + o_atol] + RTOL[I + o_rtol] * Math.Abs(Y[I + o_y]);
                    DER2 += Math.Pow((F1[I + o_f1] - F0[I + o_f0]) / SK,2);
                }
            }
            DER2 = Math.Sqrt(DER2) / H;
            // C ---- STEP SIZE IS COMPUTED SUCH THAT
            // C ----  H**IORD * MAX ( NORM (F0), NORM (DER2)) = 0.01
            DER12 = Math.Max(Math.Abs(DER2), Math.Sqrt(DNF));
            if (DER12 <= 1.0E-15)
            {
                H1 = Math.Max(1.0E-6, Math.Abs(H) * 1.0E-3);
            }
            else
            {
                H1 = Math.Pow(0.01E0 / DER12,1.0E0 / IORD);
            }
            H = Math.Min(100 * Math.Abs(H), Math.Min(H1, HMAX));
            hinit = FortranLib.Sign(H,POSNEG);
            return hinit;
            #endregion
        }
    }

    #endregion


    #region The Class: CONTD5
    
    // C
    public class CONTD5
    {
    
        #region Common variables
        
        #region Common Block: CONDO5 Declaration
        
        CommonBlock _condo5;
        Odouble XOLD; Odouble H; 
        #endregion
        #endregion
        public CONTD5(CommonBlock CONDO5)
        {
    
            #region Common varaible Initialization
            
            #region Common Block: CONDO5 Initialization
            
            this._condo5 = CONDO5;
            XOLD = CONDO5.doubleData[0];
            H = CONDO5.doubleData[1];
            #endregion
            #endregion
        }
    
        public CONTD5()
        {
    
            #region Initialization Common Blocks
            
            CommonBlock CONDO5 = new CommonBlock(2, 0, 0, 0);
            #endregion
            #region Common varaible Initialization
            
            #region Common Block: CONDO5 Initialization
            
            this._condo5 = CONDO5;
            XOLD = CONDO5.doubleData[0];
            H = CONDO5.doubleData[1];
            #endregion
            #endregion
        }
        public double Run(int II, double X, double[] CON, int offset_con, int[] ICOMP, int offset_icomp, int ND)
        {
        double contd5 = 0;
            #region Implicit Variables
            
            int I = 0; int J = 0; double THETA = 0; double THETA1 = 0; 
            #endregion
            #region Array Index Correction
            
             int o_con = -1 + offset_con;  int o_icomp = -1 + offset_icomp; 
            #endregion
            // C ----------------------------------------------------------
            // C     THIS FUNCTION CAN BE USED FOR CONTINUOUS OUTPUT IN CONNECTION
            // C     WITH THE OUTPUT-SUBROUTINE FOR DOPRI5. IT PROVIDES AN
            // C     APPROXIMATION TO THE II-TH COMPONENT OF THE SOLUTION AT X.
            // C ----------------------------------------------------------
            // C ----- COMPUTE PLACE OF II-TH COMPONENT 
            I = 0;
            for (J = 1; J <= ND; J++)
            {
                if (ICOMP[J + o_icomp] == II) I = J;
            }
            if (I == 0)
            {
                //ERROR-ERROR         WRITE (6,*) ' NO DENSE OUTPUT AVAILABLE FOR COMP.',II ;
                return contd5;
            }
            THETA = (X - XOLD.v) / H.v;
            THETA1 = 1.0E0 - THETA;
            contd5 = CON[I + o_con] + THETA * (CON[ND + I + o_con] + THETA1 * (CON[2 * ND + I + o_con] + THETA * (CON[3 * ND + I + o_con] + THETA1 * CON[4 * ND + I + o_con])));
            return contd5;
        }
    }

    #endregion


    #region The Class: CDOPRI
    
    // C
    public class CDOPRI
    {
    
        public CDOPRI()
        {
    
        }
    
        public void Run(ref double C2, ref double C3, ref double C4, ref double C5, ref double E1, ref double E3
                         , ref double E4, ref double E5, ref double E6, ref double E7, ref double A21, ref double A31
                         , ref double A32, ref double A41, ref double A42, ref double A43, ref double A51, ref double A52
                         , ref double A53, ref double A54, ref double A61, ref double A62, ref double A63, ref double A64
                         , ref double A65, ref double A71, ref double A73, ref double A74, ref double A75, ref double A76
                         , ref double D1, ref double D3, ref double D4, ref double D5, ref double D6, ref double D7)
        {
            // C ----------------------------------------------------------
            // C     RUNGE-KUTTA COEFFICIENTS OF DORMAND AND PRINCE (1980)
            // C ----------------------------------------------------------
            #region Body
            
            C2 = 0.2E0;
            C3 = 0.3E0;
            C4 = 0.8E0;
            C5 = 8.0E0 / 9.0E0;
            A21 = 0.2E0;
            A31 = 3.0E0 / 40.0E0;
            A32 = 9.0E0 / 40.0E0;
            A41 = 44.0E0 / 45.0E0;
            A42 =  - 56.0E0 / 15.0E0;
            A43 = 32.0E0 / 9.0E0;
            A51 = 19372.0E0 / 6561.0E0;
            A52 =  - 25360.0E0 / 2187.0E0;
            A53 = 64448.0E0 / 6561.0E0;
            A54 =  - 212.0E0 / 729.0E0;
            A61 = 9017.0E0 / 3168.0E0;
            A62 =  - 355.0E0 / 33.0E0;
            A63 = 46732.0E0 / 5247.0E0;
            A64 = 49.0E0 / 176.0E0;
            A65 =  - 5103.0E0 / 18656.0E0;
            A71 = 35.0E0 / 384.0E0;
            A73 = 500.0E0 / 1113.0E0;
            A74 = 125.0E0 / 192.0E0;
            A75 =  - 2187.0E0 / 6784.0E0;
            A76 = 11.0E0 / 84.0E0;
            E1 = 71.0E0 / 57600.0E0;
            E3 =  - 71.0E0 / 16695.0E0;
            E4 = 71.0E0 / 1920.0E0;
            E5 =  - 17253.0E0 / 339200.0E0;
            E6 = 22.0E0 / 525.0E0;
            E7 =  - 1.0E0 / 40.0E0;
            // C ---- DENSE OUTPUT OF SHAMPINE (1986)
            D1 =  - 12715105075.0E0 / 11282082432.0E0;
            D3 = 87487479700.0E0 / 32700410799.0E0;
            D4 =  - 10690763975.0E0 / 1880347072.0E0;
            D5 = 701980252875.0E0 / 199316789632.0E0;
            D6 =  - 1453857185.0E0 / 822651844.0E0;
            D7 = 69997945.0E0 / 29380423.0E0;
            return;
            #endregion
        }
    }

    #endregion

    
}
