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

namespace DotNumerics.ODE.Radau5
{

    #region The Class: RADAU5
    
    public class RADAU5
    {
    
        #region Dependencies
        
        RADCOR _radcor; 
        #endregion
        public RADAU5(RADCOR radcor)
        {
    
            #region Set Dependencies
            
            this._radcor = radcor; 
            #endregion
        }
    
        public RADAU5()
        {
    
            #region Initialization Common Blocks
            
            CommonBlock CONRA5 = new CommonBlock(4, 4, 0, 0);
            CommonBlock LINAL = new CommonBlock(0, 7, 0, 0);
            #endregion
            #region Dependencies (Initialization)
            
            DEC dec = new DEC();
            DECB decb = new DECB();
            ELMHES elmhes = new ELMHES();
            DECH dech = new DECH();
            DECC decc = new DECC();
            DECBC decbc = new DECBC();
            DECHC dechc = new DECHC();
            SOL sol = new SOL();
            SOLC solc = new SOLC();
            SOLB solb = new SOLB();
            SOLBC solbc = new SOLBC();
            SOLH solh = new SOLH();
            SOLHC solhc = new SOLHC();
            DECOMR decomr = new DECOMR(dec, decb, elmhes, dech, LINAL);
            DECOMC decomc = new DECOMC(decc, decbc, dechc, LINAL);
            SLVRAD slvrad = new SLVRAD(sol, solc, solb, solbc, solh, solhc, LINAL);
            ESTRAD estrad = new ESTRAD(sol, solb, solh, LINAL);
            RADCOR radcor = new RADCOR(decomr, decomc, slvrad, estrad, CONRA5, LINAL);
            #endregion
            #region Set Dependencies
            
            this._radcor = radcor; 
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
        /// RPAR, IPAR (SEE BELOW)
        ///</param>
        /// <param name="X">
        /// INITIAL X-VALUE
        ///</param>
        /// <param name="XEND">
        /// FINAL X-VALUE (XEND-X MAY BE POSITIVE OR NEGATIVE)
        ///</param>
        /// <param name="H">
        /// INITIAL STEP SIZE GUESS;
        /// FOR STIFF EQUATIONS WITH INITIAL TRANSIENT, 
        /// H=1.D0/(NORM OF F'), USUALLY 1.D-3 OR 1.D-5, IS GOOD.
        /// THIS CHOICE IS NOT VERY IMPORTANT, THE STEP SIZE IS
        /// QUICKLY ADAPTED. (IF H=0.D0, THE CODE PUTS H=1.D-6).
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
        /// <param name="JAC">
        /// NAME (EXTERNAL) OF THE SUBROUTINE WHICH COMPUTES
        /// THE PARTIAL DERIVATIVES OF F(X,Y) WITH RESPECT TO Y
        /// (THIS ROUTINE IS ONLY CALLED IF IJAC=1; SUPPLY
        /// A DUMMY SUBROUTINE IN THE CASE IJAC=0).
        /// FOR IJAC=1, THIS SUBROUTINE MUST HAVE THE FORM
        /// SUBROUTINE JAC(N,X,Y,DFY,LDFY,RPAR,IPAR)
        /// DOUBLE PRECISION X,Y(N),DFY(LDFY,N)
        /// DFY(1,1)= ...
        /// LDFY, THE COLUMN-LENGTH OF THE ARRAY, IS
        /// FURNISHED BY THE CALLING PROGRAM.
        /// IF (MLJAC.EQ.N) THE JACOBIAN IS SUPPOSED TO
        /// BE FULL AND THE PARTIAL DERIVATIVES ARE
        /// STORED IN DFY AS
        /// DFY(I,J) = PARTIAL F(I) / PARTIAL Y(J)
        /// ELSE, THE JACOBIAN IS TAKEN AS BANDED AND
        /// THE PARTIAL DERIVATIVES ARE STORED
        /// DIAGONAL-WISE AS
        /// DFY(I-J+MUJAC+1,J) = PARTIAL F(I) / PARTIAL Y(J).
        ///</param>
        /// <param name="IJAC">
        /// SWITCH FOR THE COMPUTATION OF THE JACOBIAN:
        /// IJAC=0: JACOBIAN IS COMPUTED INTERNALLY BY FINITE
        /// DIFFERENCES, SUBROUTINE "JAC" IS NEVER CALLED.
        /// IJAC=1: JACOBIAN IS SUPPLIED BY SUBROUTINE JAC.
        ///</param>
        /// <param name="MLJAC">
        /// SWITCH FOR THE BANDED STRUCTURE OF THE JACOBIAN:
        /// MLJAC=N: JACOBIAN IS A FULL MATRIX. THE LINEAR
        /// ALGEBRA IS DONE BY FULL-MATRIX GAUSS-ELIMINATION.
        /// 0.LE.MLJAC.LT.N: MLJAC IS THE LOWER BANDWITH OF JACOBIAN 
        /// MATRIX (.GE. NUMBER OF NON-ZERO DIAGONALS BELOW
        /// THE MAIN DIAGONAL).
        ///</param>
        /// <param name="MUJAC">
        /// UPPER BANDWITH OF JACOBIAN  MATRIX (.GE. NUMBER OF NON-
        /// ZERO DIAGONALS ABOVE THE MAIN DIAGONAL).
        /// NEED NOT BE DEFINED IF MLJAC=N.
        ///</param>
        /// <param name="MAS">
        /// NAME (EXTERNAL) OF SUBROUTINE COMPUTING THE MASS-
        /// MATRIX M.
        /// IF IMAS=0, THIS MATRIX IS ASSUMED TO BE THE IDENTITY
        /// MATRIX AND NEEDS NOT TO BE DEFINED;
        /// SUPPLY A DUMMY SUBROUTINE IN THIS CASE.
        /// IF IMAS=1, THE SUBROUTINE MAS IS OF THE FORM
        /// SUBROUTINE MAS(N,AM,LMAS,RPAR,IPAR)
        /// DOUBLE PRECISION AM(LMAS,N)
        /// AM(1,1)= ....
        /// IF (MLMAS.EQ.N) THE MASS-MATRIX IS STORED
        /// AS FULL MATRIX LIKE
        /// AM(I,J) = M(I,J)
        /// ELSE, THE MATRIX IS TAKEN AS BANDED AND STORED
        /// DIAGONAL-WISE AS
        /// AM(I-J+MUMAS+1,J) = M(I,J).
        ///</param>
        /// <param name="IMAS">
        /// GIVES INFORMATION ON THE MASS-MATRIX:
        /// IMAS=0: M IS SUPPOSED TO BE THE IDENTITY
        /// MATRIX, MAS IS NEVER CALLED.
        /// IMAS=1: MASS-MATRIX  IS SUPPLIED.
        ///</param>
        /// <param name="MLMAS">
        /// SWITCH FOR THE BANDED STRUCTURE OF THE MASS-MATRIX:
        /// MLMAS=N: THE FULL MATRIX CASE. THE LINEAR
        /// ALGEBRA IS DONE BY FULL-MATRIX GAUSS-ELIMINATION.
        /// 0.LE.MLMAS.LT.N: MLMAS IS THE LOWER BANDWITH OF THE
        /// MATRIX (.GE. NUMBER OF NON-ZERO DIAGONALS BELOW
        /// THE MAIN DIAGONAL).
        /// MLMAS IS SUPPOSED TO BE .LE. MLJAC.
        ///</param>
        /// <param name="MUMAS">
        /// UPPER BANDWITH OF MASS-MATRIX (.GE. NUMBER OF NON-
        /// ZERO DIAGONALS ABOVE THE MAIN DIAGONAL).
        /// NEED NOT BE DEFINED IF MLMAS=N.
        /// MUMAS IS SUPPOSED TO BE .LE. MUJAC.
        ///</param>
        /// <param name="SOLOUT">
        /// NAME (EXTERNAL) OF SUBROUTINE PROVIDING THE
        /// NUMERICAL SOLUTION DURING INTEGRATION. 
        /// IF IOUT=1, IT IS CALLED AFTER EVERY SUCCESSFUL STEP.
        /// SUPPLY A DUMMY SUBROUTINE IF IOUT=0. 
        /// IT MUST HAVE THE FORM
        /// SUBROUTINE SOLOUT (NR,XOLD,X,Y,CONT,LRC,N,
        /// RPAR,IPAR,IRTRN)
        /// DOUBLE PRECISION X,Y(N),CONT(LRC)
        /// ....  
        /// SOLOUT FURNISHES THE SOLUTION "Y" AT THE NR-TH
        /// GRID-POINT "X" (THEREBY THE INITIAL VALUE IS
        /// THE FIRST GRID-POINT).
        /// "XOLD" IS THE PRECEEDING GRID-POINT.
        /// "IRTRN" SERVES TO INTERRUPT THE INTEGRATION. IF IRTRN
        /// IS SET .LT.0, RADAU5 RETURNS TO THE CALLING PROGRAM.
        /// 
        /// -----  CONTINUOUS OUTPUT: -----
        /// DURING CALLS TO "SOLOUT", A CONTINUOUS SOLUTION
        /// FOR THE INTERVAL [XOLD,X] IS AVAILABLE THROUGH
        /// THE FUNCTION
        /// .GT..GT..GT.   CONTR5(I,S,CONT,LRC)   .LT..LT..LT.
        /// WHICH PROVIDES AN APPROXIMATION TO THE I-TH
        /// COMPONENT OF THE SOLUTION AT THE POINT S. THE VALUE
        /// S SHOULD LIE IN THE INTERVAL [XOLD,X].
        /// DO NOT CHANGE THE ENTRIES OF CONT(LRC), IF THE
        /// DENSE OUTPUT FUNCTION IS USED.
        ///</param>
        /// <param name="IOUT">
        /// SWITCH FOR CALLING THE SUBROUTINE SOLOUT:
        /// IOUT=0: SUBROUTINE IS NEVER CALLED
        /// IOUT=1: SUBROUTINE IS AVAILABLE FOR OUTPUT.
        ///</param>
        /// <param name="WORK">
        /// ARRAY OF WORKING SPACE OF LENGTH "LWORK".
        /// WORK(1), WORK(2),.., WORK(20) SERVE AS PARAMETERS
        /// FOR THE CODE. FOR STANDARD USE OF THE CODE
        /// WORK(1),..,WORK(20) MUST BE SET TO ZERO BEFORE
        /// CALLING. SEE BELOW FOR A MORE SOPHISTICATED USE.
        /// WORK(21),..,WORK(LWORK) SERVE AS WORKING SPACE
        /// FOR ALL VECTORS AND MATRICES.
        /// "LWORK" MUST BE AT LEAST
        /// N*(LJAC+LMAS+3*LE+12)+20
        /// WHERE
        /// LJAC=N              IF MLJAC=N (FULL JACOBIAN)
        /// LJAC=MLJAC+MUJAC+1  IF MLJAC.LT.N (BANDED JAC.)
        /// AND                  
        /// LMAS=0              IF IMAS=0
        /// LMAS=N              IF IMAS=1 AND MLMAS=N (FULL)
        /// LMAS=MLMAS+MUMAS+1  IF MLMAS.LT.N (BANDED MASS-M.)
        /// AND
        /// LE=N               IF MLJAC=N (FULL JACOBIAN)
        /// LE=2*MLJAC+MUJAC+1 IF MLJAC.LT.N (BANDED JAC.)
        /// 
        /// IN THE USUAL CASE WHERE THE JACOBIAN IS FULL AND THE
        /// MASS-MATRIX IS THE INDENTITY (IMAS=0), THE MINIMUM
        /// STORAGE REQUIREMENT IS 
        /// LWORK = 4*N*N+12*N+20.
        /// IF IWORK(9)=M1.GT.0 THEN "LWORK" MUST BE AT LEAST
        /// N*(LJAC+12)+(N-M1)*(LMAS+3*LE)+20
        /// WHERE IN THE DEFINITIONS OF LJAC, LMAS AND LE THE
        /// NUMBER N CAN BE REPLACED BY N-M1.
        ///</param>
        /// <param name="LWORK">
        /// DECLARED LENGTH OF ARRAY "WORK".
        ///</param>
        /// <param name="IWORK">
        /// INTEGER WORKING SPACE OF LENGTH "LIWORK".
        /// IWORK(1),IWORK(2),...,IWORK(20) SERVE AS PARAMETERS
        /// FOR THE CODE. FOR STANDARD USE, SET IWORK(1),..,
        /// IWORK(20) TO ZERO BEFORE CALLING.
        /// IWORK(21),...,IWORK(LIWORK) SERVE AS WORKING AREA.
        /// "LIWORK" MUST BE AT LEAST 3*N+20.
        ///</param>
        /// <param name="LIWORK">
        /// DECLARED LENGTH OF ARRAY "IWORK".
        ///</param>
        /// <param name="IDID">
        /// REPORTS ON SUCCESSFULNESS UPON RETURN:
        /// IDID= 1  COMPUTATION SUCCESSFUL,
        /// IDID= 2  COMPUT. SUCCESSFUL (INTERRUPTED BY SOLOUT)
        /// IDID=-1  INPUT IS NOT CONSISTENT,
        /// IDID=-2  LARGER NMAX IS NEEDED,
        /// IDID=-3  STEP SIZE BECOMES TOO SMALL,
        /// IDID=-4  MATRIX IS REPEATEDLY SINGULAR.
        ///</param>
        public void Run(int N, IFVPOL FCN, ref double X, ref double[] Y, int offset_y, double XEND, ref double H
                         , ref double[] RTOL, int offset_rtol, ref double[] ATOL, int offset_atol, int ITOL, IJVPOL JAC, int IJAC, ref int MLJAC
                         , ref int MUJAC, IBBAMPL MAS, int IMAS, int MLMAS, ref int MUMAS, ISOLOUTR SOLOUT
                         , int IOUT, ref double[] WORK, int offset_work, int LWORK, ref int[] IWORK, int offset_iwork, int LIWORK, double[] RPAR, int offset_rpar
                         , int[] IPAR, int offset_ipar, ref int IDID)
        {
            #region Variables
            
            bool IMPLCT = false; bool JBAND = false; bool ARRET = false; bool STARTN = false; bool PRED = false; 
            #endregion
            #region Implicit Variables
            
            int NFCN = 0; int NJAC = 0; int NSTEP = 0; int NACCPT = 0; int NREJCT = 0; int NDEC = 0; int NSOL = 0; 
            double UROUND = 0;double EXPM = 0; double QUOT = 0; int I = 0; int NMAX = 0; int NIT = 0; int NIND1 = 0; 
            int NIND2 = 0;int NIND3 = 0; int M1 = 0; int M2 = 0; int NM1 = 0; double SAFE = 0; double THET = 0; double TOLST = 0; 
            double FNEWT = 0;double QUOT1 = 0; double QUOT2 = 0; double HMAX = 0; double FACL = 0; double FACR = 0; int LDJAC = 0; 
            int LDE1 = 0;int LDMAS = 0; int IJOB = 0; int LDMAS2 = 0; int IEZ1 = 0; int IEZ2 = 0; int IEZ3 = 0; int IEY0 = 0; 
            int IESCAL = 0;int IEF1 = 0; int IEF2 = 0; int IEF3 = 0; int IECON = 0; int IEJAC = 0; int IEMAS = 0; int IEE1 = 0; 
            int IEE2R = 0;int IEE2I = 0; int ISTORE = 0; int IEIP1 = 0; int IEIP2 = 0; int IEIPH = 0; 
            #endregion
            #region Array Index Correction
            
             int o_y = -1 + offset_y;  int o_rtol = -1 + offset_rtol;  int o_atol = -1 + offset_atol; 
             int o_work = -1 + offset_work; int o_iwork = -1 + offset_iwork;  int o_rpar = -1 + offset_rpar; 
             int o_ipar = -1 + offset_ipar;
            #endregion
            #region Prolog
            
            // C ----------------------------------------------------------
            // C     NUMERICAL SOLUTION OF A STIFF (OR DIFFERENTIAL ALGEBRAIC)
            // C     SYSTEM OF FIRST 0RDER ORDINARY DIFFERENTIAL EQUATIONS
            // C                     M*Y'=F(X,Y).
            // C     THE SYSTEM CAN BE (LINEARLY) IMPLICIT (MASS-MATRIX M .NE. I)
            // C     OR EXPLICIT (M=I).
            // C     THE METHOD USED IS AN IMPLICIT RUNGE-KUTTA METHOD (RADAU IIA)
            // C     OF ORDER 5 WITH STEP SIZE CONTROL AND CONTINUOUS OUTPUT.
            // C     CF. SECTION IV.8
            // C
            // C     AUTHORS: E. HAIRER AND G. WANNER
            // C              UNIVERSITE DE GENEVE, DEPT. DE MATHEMATIQUES
            // C              CH-1211 GENEVE 24, SWITZERLAND 
            // C              E-MAIL:  Ernst.Hairer@math.unige.ch
            // C                       Gerhard.Wanner@math.unige.ch
            // C     
            // C     THIS CODE IS PART OF THE BOOK:
            // C         E. HAIRER AND G. WANNER, SOLVING ORDINARY DIFFERENTIAL
            // C         EQUATIONS II. STIFF AND DIFFERENTIAL-ALGEBRAIC PROBLEMS.
            // C         SPRINGER SERIES IN COMPUTATIONAL MATHEMATICS 14,
            // C         SPRINGER-VERLAG 1991, SECOND EDITION 1996.
            // C      
            // C     VERSION OF JULY 9, 1996
            // C     (latest small correction: January 18, 2002)
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
            // C                 RPAR, IPAR (SEE BELOW)
            // C
            // C     X           INITIAL X-VALUE
            // C
            // C     Y(N)        INITIAL VALUES FOR Y
            // C
            // C     XEND        FINAL X-VALUE (XEND-X MAY BE POSITIVE OR NEGATIVE)
            // C
            // C     H           INITIAL STEP SIZE GUESS;
            // C                 FOR STIFF EQUATIONS WITH INITIAL TRANSIENT, 
            // C                 H=1.D0/(NORM OF F'), USUALLY 1.D-3 OR 1.D-5, IS GOOD.
            // C                 THIS CHOICE IS NOT VERY IMPORTANT, THE STEP SIZE IS
            // C                 QUICKLY ADAPTED. (IF H=0.D0, THE CODE PUTS H=1.D-6).
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
            // C     JAC         NAME (EXTERNAL) OF THE SUBROUTINE WHICH COMPUTES
            // C                 THE PARTIAL DERIVATIVES OF F(X,Y) WITH RESPECT TO Y
            // C                 (THIS ROUTINE IS ONLY CALLED IF IJAC=1; SUPPLY
            // C                 A DUMMY SUBROUTINE IN THE CASE IJAC=0).
            // C                 FOR IJAC=1, THIS SUBROUTINE MUST HAVE THE FORM
            // C                    SUBROUTINE JAC(N,X,Y,DFY,LDFY,RPAR,IPAR)
            // C                    DOUBLE PRECISION X,Y(N),DFY(LDFY,N)
            // C                    DFY(1,1)= ...
            // C                 LDFY, THE COLUMN-LENGTH OF THE ARRAY, IS
            // C                 FURNISHED BY THE CALLING PROGRAM.
            // C                 IF (MLJAC.EQ.N) THE JACOBIAN IS SUPPOSED TO
            // C                    BE FULL AND THE PARTIAL DERIVATIVES ARE
            // C                    STORED IN DFY AS
            // C                       DFY(I,J) = PARTIAL F(I) / PARTIAL Y(J)
            // C                 ELSE, THE JACOBIAN IS TAKEN AS BANDED AND
            // C                    THE PARTIAL DERIVATIVES ARE STORED
            // C                    DIAGONAL-WISE AS
            // C                       DFY(I-J+MUJAC+1,J) = PARTIAL F(I) / PARTIAL Y(J).
            // C
            // C     IJAC        SWITCH FOR THE COMPUTATION OF THE JACOBIAN:
            // C                    IJAC=0: JACOBIAN IS COMPUTED INTERNALLY BY FINITE
            // C                       DIFFERENCES, SUBROUTINE "JAC" IS NEVER CALLED.
            // C                    IJAC=1: JACOBIAN IS SUPPLIED BY SUBROUTINE JAC.
            // C
            // C     MLJAC       SWITCH FOR THE BANDED STRUCTURE OF THE JACOBIAN:
            // C                    MLJAC=N: JACOBIAN IS A FULL MATRIX. THE LINEAR
            // C                       ALGEBRA IS DONE BY FULL-MATRIX GAUSS-ELIMINATION.
            // C                    0<=MLJAC<N: MLJAC IS THE LOWER BANDWITH OF JACOBIAN 
            // C                       MATRIX (>= NUMBER OF NON-ZERO DIAGONALS BELOW
            // C                       THE MAIN DIAGONAL).
            // C
            // C     MUJAC       UPPER BANDWITH OF JACOBIAN  MATRIX (>= NUMBER OF NON-
            // C                 ZERO DIAGONALS ABOVE THE MAIN DIAGONAL).
            // C                 NEED NOT BE DEFINED IF MLJAC=N.
            // C
            // C     ----   MAS,IMAS,MLMAS, AND MUMAS HAVE ANALOG MEANINGS      -----
            // C     ----   FOR THE "MASS MATRIX" (THE MATRIX "M" OF SECTION IV.8): -
            // C
            // C     MAS         NAME (EXTERNAL) OF SUBROUTINE COMPUTING THE MASS-
            // C                 MATRIX M.
            // C                 IF IMAS=0, THIS MATRIX IS ASSUMED TO BE THE IDENTITY
            // C                 MATRIX AND NEEDS NOT TO BE DEFINED;
            // C                 SUPPLY A DUMMY SUBROUTINE IN THIS CASE.
            // C                 IF IMAS=1, THE SUBROUTINE MAS IS OF THE FORM
            // C                    SUBROUTINE MAS(N,AM,LMAS,RPAR,IPAR)
            // C                    DOUBLE PRECISION AM(LMAS,N)
            // C                    AM(1,1)= ....
            // C                    IF (MLMAS.EQ.N) THE MASS-MATRIX IS STORED
            // C                    AS FULL MATRIX LIKE
            // C                         AM(I,J) = M(I,J)
            // C                    ELSE, THE MATRIX IS TAKEN AS BANDED AND STORED
            // C                    DIAGONAL-WISE AS
            // C                         AM(I-J+MUMAS+1,J) = M(I,J).
            // C
            // C     IMAS       GIVES INFORMATION ON THE MASS-MATRIX:
            // C                    IMAS=0: M IS SUPPOSED TO BE THE IDENTITY
            // C                       MATRIX, MAS IS NEVER CALLED.
            // C                    IMAS=1: MASS-MATRIX  IS SUPPLIED.
            // C
            // C     MLMAS       SWITCH FOR THE BANDED STRUCTURE OF THE MASS-MATRIX:
            // C                    MLMAS=N: THE FULL MATRIX CASE. THE LINEAR
            // C                       ALGEBRA IS DONE BY FULL-MATRIX GAUSS-ELIMINATION.
            // C                    0<=MLMAS<N: MLMAS IS THE LOWER BANDWITH OF THE
            // C                       MATRIX (>= NUMBER OF NON-ZERO DIAGONALS BELOW
            // C                       THE MAIN DIAGONAL).
            // C                 MLMAS IS SUPPOSED TO BE .LE. MLJAC.
            // C
            // C     MUMAS       UPPER BANDWITH OF MASS-MATRIX (>= NUMBER OF NON-
            // C                 ZERO DIAGONALS ABOVE THE MAIN DIAGONAL).
            // C                 NEED NOT BE DEFINED IF MLMAS=N.
            // C                 MUMAS IS SUPPOSED TO BE .LE. MUJAC.
            // C
            // C     SOLOUT      NAME (EXTERNAL) OF SUBROUTINE PROVIDING THE
            // C                 NUMERICAL SOLUTION DURING INTEGRATION. 
            // C                 IF IOUT=1, IT IS CALLED AFTER EVERY SUCCESSFUL STEP.
            // C                 SUPPLY A DUMMY SUBROUTINE IF IOUT=0. 
            // C                 IT MUST HAVE THE FORM
            // C                    SUBROUTINE SOLOUT (NR,XOLD,X,Y,CONT,LRC,N,
            // C                                       RPAR,IPAR,IRTRN)
            // C                    DOUBLE PRECISION X,Y(N),CONT(LRC)
            // C                    ....  
            // C                 SOLOUT FURNISHES THE SOLUTION "Y" AT THE NR-TH
            // C                    GRID-POINT "X" (THEREBY THE INITIAL VALUE IS
            // C                    THE FIRST GRID-POINT).
            // C                 "XOLD" IS THE PRECEEDING GRID-POINT.
            // C                 "IRTRN" SERVES TO INTERRUPT THE INTEGRATION. IF IRTRN
            // C                    IS SET <0, RADAU5 RETURNS TO THE CALLING PROGRAM.
            // C           
            // C          -----  CONTINUOUS OUTPUT: -----
            // C                 DURING CALLS TO "SOLOUT", A CONTINUOUS SOLUTION
            // C                 FOR THE INTERVAL [XOLD,X] IS AVAILABLE THROUGH
            // C                 THE FUNCTION
            // C                        >>>   CONTR5(I,S,CONT,LRC)   <<<
            // C                 WHICH PROVIDES AN APPROXIMATION TO THE I-TH
            // C                 COMPONENT OF THE SOLUTION AT THE POINT S. THE VALUE
            // C                 S SHOULD LIE IN THE INTERVAL [XOLD,X].
            // C                 DO NOT CHANGE THE ENTRIES OF CONT(LRC), IF THE
            // C                 DENSE OUTPUT FUNCTION IS USED.
            // C
            // C     IOUT        SWITCH FOR CALLING THE SUBROUTINE SOLOUT:
            // C                    IOUT=0: SUBROUTINE IS NEVER CALLED
            // C                    IOUT=1: SUBROUTINE IS AVAILABLE FOR OUTPUT.
            // C
            // C     WORK        ARRAY OF WORKING SPACE OF LENGTH "LWORK".
            // C                 WORK(1), WORK(2),.., WORK(20) SERVE AS PARAMETERS
            // C                 FOR THE CODE. FOR STANDARD USE OF THE CODE
            // C                 WORK(1),..,WORK(20) MUST BE SET TO ZERO BEFORE
            // C                 CALLING. SEE BELOW FOR A MORE SOPHISTICATED USE.
            // C                 WORK(21),..,WORK(LWORK) SERVE AS WORKING SPACE
            // C                 FOR ALL VECTORS AND MATRICES.
            // C                 "LWORK" MUST BE AT LEAST
            // C                             N*(LJAC+LMAS+3*LE+12)+20
            // C                 WHERE
            // C                    LJAC=N              IF MLJAC=N (FULL JACOBIAN)
            // C                    LJAC=MLJAC+MUJAC+1  IF MLJAC<N (BANDED JAC.)
            // C                 AND                  
            // C                    LMAS=0              IF IMAS=0
            // C                    LMAS=N              IF IMAS=1 AND MLMAS=N (FULL)
            // C                    LMAS=MLMAS+MUMAS+1  IF MLMAS<N (BANDED MASS-M.)
            // C                 AND
            // C                    LE=N               IF MLJAC=N (FULL JACOBIAN)
            // C                    LE=2*MLJAC+MUJAC+1 IF MLJAC<N (BANDED JAC.)
            // C
            // C                 IN THE USUAL CASE WHERE THE JACOBIAN IS FULL AND THE
            // C                 MASS-MATRIX IS THE INDENTITY (IMAS=0), THE MINIMUM
            // C                 STORAGE REQUIREMENT IS 
            // C                             LWORK = 4*N*N+12*N+20.
            // C                 IF IWORK(9)=M1>0 THEN "LWORK" MUST BE AT LEAST
            // C                          N*(LJAC+12)+(N-M1)*(LMAS+3*LE)+20
            // C                 WHERE IN THE DEFINITIONS OF LJAC, LMAS AND LE THE
            // C                 NUMBER N CAN BE REPLACED BY N-M1.
            // C
            // C     LWORK       DECLARED LENGTH OF ARRAY "WORK".
            // C
            // C     IWORK       INTEGER WORKING SPACE OF LENGTH "LIWORK".
            // C                 IWORK(1),IWORK(2),...,IWORK(20) SERVE AS PARAMETERS
            // C                 FOR THE CODE. FOR STANDARD USE, SET IWORK(1),..,
            // C                 IWORK(20) TO ZERO BEFORE CALLING.
            // C                 IWORK(21),...,IWORK(LIWORK) SERVE AS WORKING AREA.
            // C                 "LIWORK" MUST BE AT LEAST 3*N+20.
            // C
            // C     LIWORK      DECLARED LENGTH OF ARRAY "IWORK".
            // C
            // C     RPAR, IPAR  REAL AND INTEGER PARAMETERS (OR PARAMETER ARRAYS) WHICH  
            // C                 CAN BE USED FOR COMMUNICATION BETWEEN YOUR CALLING
            // C                 PROGRAM AND THE FCN, JAC, MAS, SOLOUT SUBROUTINES. 
            // C
            // C ----------------------------------------------------------------------
            // C 
            // C     SOPHISTICATED SETTING OF PARAMETERS
            // C     -----------------------------------
            // C              SEVERAL PARAMETERS OF THE CODE ARE TUNED TO MAKE IT WORK 
            // C              WELL. THEY MAY BE DEFINED BY SETTING WORK(1),...
            // C              AS WELL AS IWORK(1),... DIFFERENT FROM ZERO.
            // C              FOR ZERO INPUT, THE CODE CHOOSES DEFAULT VALUES:
            // C
            // C    IWORK(1)  IF IWORK(1).NE.0, THE CODE TRANSFORMS THE JACOBIAN
            // C              MATRIX TO HESSENBERG FORM. THIS IS PARTICULARLY
            // C              ADVANTAGEOUS FOR LARGE SYSTEMS WITH FULL JACOBIAN.
            // C              IT DOES NOT WORK FOR BANDED JACOBIAN (MLJAC<N)
            // C              AND NOT FOR IMPLICIT SYSTEMS (IMAS=1).
            // C
            // C    IWORK(2)  THIS IS THE MAXIMAL NUMBER OF ALLOWED STEPS.
            // C              THE DEFAULT VALUE (FOR IWORK(2)=0) IS 100000.
            // C
            // C    IWORK(3)  THE MAXIMUM NUMBER OF NEWTON ITERATIONS FOR THE
            // C              SOLUTION OF THE IMPLICIT SYSTEM IN EACH STEP.
            // C              THE DEFAULT VALUE (FOR IWORK(3)=0) IS 7.
            // C
            // C    IWORK(4)  IF IWORK(4).EQ.0 THE EXTRAPOLATED COLLOCATION SOLUTION
            // C              IS TAKEN AS STARTING VALUE FOR NEWTON'S METHOD.
            // C              IF IWORK(4).NE.0 ZERO STARTING VALUES ARE USED.
            // C              THE LATTER IS RECOMMENDED IF NEWTON'S METHOD HAS
            // C              DIFFICULTIES WITH CONVERGENCE (THIS IS THE CASE WHEN
            // C              NSTEP IS LARGER THAN NACCPT + NREJCT; SEE OUTPUT PARAM.).
            // C              DEFAULT IS IWORK(4)=0.
            // C
            // C       THE FOLLOWING 3 PARAMETERS ARE IMPORTANT FOR
            // C       DIFFERENTIAL-ALGEBRAIC SYSTEMS OF INDEX > 1.
            // C       THE FUNCTION-SUBROUTINE SHOULD BE WRITTEN SUCH THAT
            // C       THE INDEX 1,2,3 VARIABLES APPEAR IN THIS ORDER. 
            // C       IN ESTIMATING THE ERROR THE INDEX 2 VARIABLES ARE
            // C       MULTIPLIED BY H, THE INDEX 3 VARIABLES BY H**2.
            // C
            // C    IWORK(5)  DIMENSION OF THE INDEX 1 VARIABLES (MUST BE > 0). FOR 
            // C              ODE'S THIS EQUALS THE DIMENSION OF THE SYSTEM.
            // C              DEFAULT IWORK(5)=N.
            // C
            // C    IWORK(6)  DIMENSION OF THE INDEX 2 VARIABLES. DEFAULT IWORK(6)=0.
            // C
            // C    IWORK(7)  DIMENSION OF THE INDEX 3 VARIABLES. DEFAULT IWORK(7)=0.
            // C
            // C    IWORK(8)  SWITCH FOR STEP SIZE STRATEGY
            // C              IF IWORK(8).EQ.1  MOD. PREDICTIVE CONTROLLER (GUSTAFSSON)
            // C              IF IWORK(8).EQ.2  CLASSICAL STEP SIZE CONTROL
            // C              THE DEFAULT VALUE (FOR IWORK(8)=0) IS IWORK(8)=1.
            // C              THE CHOICE IWORK(8).EQ.1 SEEMS TO PRODUCE SAFER RESULTS;
            // C              FOR SIMPLE PROBLEMS, THE CHOICE IWORK(8).EQ.2 PRODUCES
            // C              OFTEN SLIGHTLY FASTER RUNS
            // C
            // C       IF THE DIFFERENTIAL SYSTEM HAS THE SPECIAL STRUCTURE THAT
            // C            Y(I)' = Y(I+M2)   FOR  I=1,...,M1,
            // C       WITH M1 A MULTIPLE OF M2, A SUBSTANTIAL GAIN IN COMPUTERTIME
            // C       CAN BE ACHIEVED BY SETTING THE PARAMETERS IWORK(9) AND IWORK(10).
            // C       E.G., FOR SECOND ORDER SYSTEMS P'=V, V'=G(P,V), WHERE P AND V ARE 
            // C       VECTORS OF DIMENSION N/2, ONE HAS TO PUT M1=M2=N/2.
            // C       FOR M1>0 SOME OF THE INPUT PARAMETERS HAVE DIFFERENT MEANINGS:
            // C       - JAC: ONLY THE ELEMENTS OF THE NON-TRIVIAL PART OF THE
            // C              JACOBIAN HAVE TO BE STORED
            // C              IF (MLJAC.EQ.N-M1) THE JACOBIAN IS SUPPOSED TO BE FULL
            // C                 DFY(I,J) = PARTIAL F(I+M1) / PARTIAL Y(J)
            // C                FOR I=1,N-M1 AND J=1,N.
            // C              ELSE, THE JACOBIAN IS BANDED ( M1 = M2 * MM )
            // C                 DFY(I-J+MUJAC+1,J+K*M2) = PARTIAL F(I+M1) / PARTIAL Y(J+K*M2)
            // C                FOR I=1,MLJAC+MUJAC+1 AND J=1,M2 AND K=0,MM.
            // C       - MLJAC: MLJAC=N-M1: IF THE NON-TRIVIAL PART OF THE JACOBIAN IS FULL
            // C                0<=MLJAC<N-M1: IF THE (MM+1) SUBMATRICES (FOR K=0,MM)
            // C                     PARTIAL F(I+M1) / PARTIAL Y(J+K*M2),  I,J=1,M2
            // C                    ARE BANDED, MLJAC IS THE MAXIMAL LOWER BANDWIDTH
            // C                    OF THESE MM+1 SUBMATRICES
            // C       - MUJAC: MAXIMAL UPPER BANDWIDTH OF THESE MM+1 SUBMATRICES
            // C                NEED NOT BE DEFINED IF MLJAC=N-M1
            // C       - MAS: IF IMAS=0 THIS MATRIX IS ASSUMED TO BE THE IDENTITY AND
            // C              NEED NOT BE DEFINED. SUPPLY A DUMMY SUBROUTINE IN THIS CASE.
            // C              IT IS ASSUMED THAT ONLY THE ELEMENTS OF RIGHT LOWER BLOCK OF
            // C              DIMENSION N-M1 DIFFER FROM THAT OF THE IDENTITY MATRIX.
            // C              IF (MLMAS.EQ.N-M1) THIS SUBMATRIX IS SUPPOSED TO BE FULL
            // C                 AM(I,J) = M(I+M1,J+M1)     FOR I=1,N-M1 AND J=1,N-M1.
            // C              ELSE, THE MASS MATRIX IS BANDED
            // C                 AM(I-J+MUMAS+1,J) = M(I+M1,J+M1)
            // C       - MLMAS: MLMAS=N-M1: IF THE NON-TRIVIAL PART OF M IS FULL
            // C                0<=MLMAS<N-M1: LOWER BANDWIDTH OF THE MASS MATRIX
            // C       - MUMAS: UPPER BANDWIDTH OF THE MASS MATRIX
            // C                NEED NOT BE DEFINED IF MLMAS=N-M1
            // C
            // C    IWORK(9)  THE VALUE OF M1.  DEFAULT M1=0.
            // C
            // C    IWORK(10) THE VALUE OF M2.  DEFAULT M2=M1.
            // C
            // C ----------
            // C
            // C    WORK(1)   UROUND, THE ROUNDING UNIT, DEFAULT 1.D-16.
            // C
            // C    WORK(2)   THE SAFETY FACTOR IN STEP SIZE PREDICTION,
            // C              DEFAULT 0.9D0.
            // C
            // C    WORK(3)   DECIDES WHETHER THE JACOBIAN SHOULD BE RECOMPUTED;
            // C              INCREASE WORK(3), TO 0.1 SAY, WHEN JACOBIAN EVALUATIONS
            // C              ARE COSTLY. FOR SMALL SYSTEMS WORK(3) SHOULD BE SMALLER 
            // C              (0.001D0, SAY). NEGATIV WORK(3) FORCES THE CODE TO
            // C              COMPUTE THE JACOBIAN AFTER EVERY ACCEPTED STEP.     
            // C              DEFAULT 0.001D0.
            // C
            // C    WORK(4)   STOPPING CRITERION FOR NEWTON'S METHOD, USUALLY CHOSEN <1.
            // C              SMALLER VALUES OF WORK(4) MAKE THE CODE SLOWER, BUT SAFER.
            // C              DEFAULT MIN(0.03D0,RTOL(1)**0.5D0)
            // C
            // C    WORK(5) AND WORK(6) : IF WORK(5) < HNEW/HOLD < WORK(6), THEN THE
            // C              STEP SIZE IS NOT CHANGED. THIS SAVES, TOGETHER WITH A
            // C              LARGE WORK(3), LU-DECOMPOSITIONS AND COMPUTING TIME FOR
            // C              LARGE SYSTEMS. FOR SMALL SYSTEMS ONE MAY HAVE
            // C              WORK(5)=1.D0, WORK(6)=1.2D0, FOR LARGE FULL SYSTEMS
            // C              WORK(5)=0.99D0, WORK(6)=2.D0 MIGHT BE GOOD.
            // C              DEFAULTS WORK(5)=1.D0, WORK(6)=1.2D0 .
            // C
            // C    WORK(7)   MAXIMAL STEP SIZE, DEFAULT XEND-X.
            // C
            // C    WORK(8), WORK(9)   PARAMETERS FOR STEP SIZE SELECTION
            // C              THE NEW STEP SIZE IS CHOSEN SUBJECT TO THE RESTRICTION
            // C                 WORK(8) <= HNEW/HOLD <= WORK(9)
            // C              DEFAULT VALUES: WORK(8)=0.2D0, WORK(9)=8.D0
            // C
            // C-----------------------------------------------------------------------
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
            // C                   IDID=-3  STEP SIZE BECOMES TOO SMALL,
            // C                   IDID=-4  MATRIX IS REPEATEDLY SINGULAR.
            // C
            // C   IWORK(14)  NFCN    NUMBER OF FUNCTION EVALUATIONS (THOSE FOR NUMERICAL
            // C                      EVALUATION OF THE JACOBIAN ARE NOT COUNTED)  
            // C   IWORK(15)  NJAC    NUMBER OF JACOBIAN EVALUATIONS (EITHER ANALYTICALLY
            // C                      OR NUMERICALLY)
            // C   IWORK(16)  NSTEP   NUMBER OF COMPUTED STEPS
            // C   IWORK(17)  NACCPT  NUMBER OF ACCEPTED STEPS
            // C   IWORK(18)  NREJCT  NUMBER OF REJECTED STEPS (DUE TO ERROR TEST),
            // C                      (STEP REJECTIONS IN THE FIRST STEP ARE NOT COUNTED)
            // C   IWORK(19)  NDEC    NUMBER OF LU-DECOMPOSITIONS OF BOTH MATRICES
            // C   IWORK(20)  NSOL    NUMBER OF FORWARD-BACKWARD SUBSTITUTIONS, OF BOTH
            // C                      SYSTEMS; THE NSTEP FORWARD-BACKWARD SUBSTITUTIONS,
            // C                      NEEDED FOR STEP SIZE SELECTION, ARE NOT COUNTED
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
            NJAC = 0;
            NSTEP = 0;
            NACCPT = 0;
            NREJCT = 0;
            NDEC = 0;
            NSOL = 0;
            ARRET = false;
            // C -------- UROUND   SMALLEST NUMBER SATISFYING 1.0D0+UROUND>1.0D0  
            if (WORK[1 + o_work] == 0.0E0)
            {
                UROUND = 1.0E-16;
            }
            else
            {
                UROUND = WORK[1 + o_work];
                if (UROUND <= 1.0E-19 || UROUND >= 1.0E0)
                {
                    //ERROR-ERROR            WRITE(6,*)' COEFFICIENTS HAVE 20 DIGITS, UROUND=',WORK(1);
                    ARRET = true;
                }
            }
            // C -------- CHECK AND CHANGE THE TOLERANCES
            EXPM = 2.0E0 / 3.0E0;
            if (ITOL == 0)
            {
                if (ATOL[1 + o_atol] <= 0.0E0 || RTOL[1 + o_rtol] <= 10.0E0 * UROUND)
                {
                    //ERROR-ERROR              WRITE (6,*) ' TOLERANCES ARE TOO SMALL';
                    ARRET = true;
                }
                else
                {
                    QUOT = ATOL[1 + o_atol] / RTOL[1 + o_rtol];
                    RTOL[1 + o_rtol] = 0.1E0 * Math.Pow(RTOL[1 + o_rtol],EXPM);
                    ATOL[1 + o_atol] = RTOL[1 + o_rtol] * QUOT;
                }
            }
            else
            {
                for (I = 1; I <= N; I++)
                {
                    if (ATOL[I + o_atol] <= 0.0E0 || RTOL[I + o_rtol] <= 10.0E0 * UROUND)
                    {
                        //ERROR-ERROR              WRITE (6,*) ' TOLERANCES(',I,') ARE TOO SMALL';
                        ARRET = true;
                    }
                    else
                    {
                        QUOT = ATOL[I + o_atol] / RTOL[I + o_rtol];
                        RTOL[I + o_rtol] = 0.1E0 * Math.Pow(RTOL[I + o_rtol],EXPM);
                        ATOL[I + o_atol] = RTOL[I + o_rtol] * QUOT;
                    }
                }
            }
            // C -------- NMAX , THE MAXIMAL NUMBER OF STEPS -----
            if (IWORK[2 + o_iwork] == 0)
            {
                NMAX = 100000;
            }
            else
            {
                NMAX = IWORK[2 + o_iwork];
                if (NMAX <= 0)
                {
                    //ERROR-ERROR            WRITE(6,*)' WRONG INPUT IWORK(2)=',IWORK(2);
                    ARRET = true;
                }
            }
            // C -------- NIT    MAXIMAL NUMBER OF NEWTON ITERATIONS
            if (IWORK[3 + o_iwork] == 0)
            {
                NIT = 7;
            }
            else
            {
                NIT = IWORK[3 + o_iwork];
                if (NIT <= 0)
                {
                    //ERROR-ERROR            WRITE(6,*)' CURIOUS INPUT IWORK(3)=',IWORK(3);
                    ARRET = true;
                }
            }
            // C -------- STARTN  SWITCH FOR STARTING VALUES OF NEWTON ITERATIONS
            if (IWORK[4 + o_iwork] == 0)
            {
                STARTN = false;
            }
            else
            {
                STARTN = true;
            }
            // C -------- PARAMETER FOR DIFFERENTIAL-ALGEBRAIC COMPONENTS
            NIND1 = IWORK[5 + o_iwork];
            NIND2 = IWORK[6 + o_iwork];
            NIND3 = IWORK[7 + o_iwork];
            if (NIND1 == 0) NIND1 = N;
            if (NIND1 + NIND2 + NIND3 != N)
            {
                //ERROR-ERROR       WRITE(6,*)' CURIOUS INPUT FOR IWORK(5,6,7)=',NIND1,NIND2,NIND3;
                ARRET = true;
            }
            // C -------- PRED   STEP SIZE CONTROL
            if (IWORK[8 + o_iwork] <= 1)
            {
                PRED = true;
            }
            else
            {
                PRED = false;
            }
            // C -------- PARAMETER FOR SECOND ORDER EQUATIONS
            M1 = IWORK[9 + o_iwork];
            M2 = IWORK[10 + o_iwork];
            NM1 = N - M1;
            if (M1 == 0) M2 = N;
            if (M2 == 0) M2 = M1;
            if (M1 < 0 || M2 < 0 || M1 + M2 > N)
            {
                //ERROR-ERROR       WRITE(6,*)' CURIOUS INPUT FOR IWORK(9,10)=',M1,M2;
                ARRET = true;
            }
            // C --------- SAFE     SAFETY FACTOR IN STEP SIZE PREDICTION
            if (WORK[2 + o_work] == 0.0E0)
            {
                SAFE = 0.9E0;
            }
            else
            {
                SAFE = WORK[2 + o_work];
                if (SAFE <= 0.001E0 || SAFE >= 1.0E0)
                {
                    //ERROR-ERROR            WRITE(6,*)' CURIOUS INPUT FOR WORK(2)=',WORK(2);
                    ARRET = true;
                }
            }
            // C ------ THET     DECIDES WHETHER THE JACOBIAN SHOULD BE RECOMPUTED;
            if (WORK[3 + o_work] == 0.0E0)
            {
                THET = 0.001E0;
            }
            else
            {
                THET = WORK[3 + o_work];
                if (THET >= 1.0E0)
                {
                    //ERROR-ERROR            WRITE(6,*)' CURIOUS INPUT FOR WORK(3)=',WORK(3);
                    ARRET = true;
                }
            }
            // C --- FNEWT   STOPPING CRITERION FOR NEWTON'S METHOD, USUALLY CHOSEN <1.
            TOLST = RTOL[1 + o_rtol];
            if (WORK[4 + o_work] == 0.0E0)
            {
                FNEWT = Math.Max(10 * UROUND / TOLST, Math.Min(0.03E0, Math.Pow(TOLST,0.5E0)));
            }
            else
            {
                FNEWT = WORK[4 + o_work];
                if (FNEWT <= UROUND / TOLST)
                {
                    //ERROR-ERROR            WRITE(6,*)' CURIOUS INPUT FOR WORK(4)=',WORK(4);
                    ARRET = true;
                }
            }
            // C --- QUOT1 AND QUOT2: IF QUOT1 < HNEW/HOLD < QUOT2, STEP SIZE = CONST.
            if (WORK[5 + o_work] == 0.0E0)
            {
                QUOT1 = 1.0E0;
            }
            else
            {
                QUOT1 = WORK[5 + o_work];
            }
            if (WORK[6 + o_work] == 0.0E0)
            {
                QUOT2 = 1.2E0;
            }
            else
            {
                QUOT2 = WORK[6 + o_work];
            }
            if (QUOT1 > 1.0E0 || QUOT2 < 1.0E0)
            {
                //ERROR-ERROR         WRITE(6,*)' CURIOUS INPUT FOR WORK(5,6)=',QUOT1,QUOT2;
                ARRET = true;
            }
            // C -------- MAXIMAL STEP SIZE
            if (WORK[7 + o_work] == 0.0E0)
            {
                HMAX = XEND - X;
            }
            else
            {
                HMAX = WORK[7 + o_work];
            }
            // C -------  FACL,FACR     PARAMETERS FOR STEP SIZE SELECTION
            if (WORK[8 + o_work] == 0.0E0)
            {
                FACL = 5.0E0;
            }
            else
            {
                FACL = 1.0E0 / WORK[8 + o_work];
            }
            if (WORK[9 + o_work] == 0.0E0)
            {
                FACR = 1.0E0 / 8.0E0;
            }
            else
            {
                FACR = 1.0E0 / WORK[9 + o_work];
            }
            if (FACL < 1.0E0 || FACR > 1.0E0)
            {
                //ERROR-ERROR            WRITE(6,*)' CURIOUS INPUT WORK(8,9)=',WORK(8),WORK(9);
                ARRET = true;
            }
            // C *** *** *** *** *** *** *** *** *** *** *** *** ***
            // C         COMPUTATION OF ARRAY ENTRIES
            // C *** *** *** *** *** *** *** *** *** *** *** *** ***
            // C ---- IMPLICIT, BANDED OR NOT ?
            IMPLCT = IMAS != 0;
            JBAND = MLJAC < NM1;
            // C -------- COMPUTATION OF THE ROW-DIMENSIONS OF THE 2-ARRAYS ---
            // C -- JACOBIAN  AND  MATRICES E1, E2
            if (JBAND)
            {
                LDJAC = MLJAC + MUJAC + 1;
                LDE1 = MLJAC + LDJAC;
            }
            else
            {
                MLJAC = NM1;
                MUJAC = NM1;
                LDJAC = NM1;
                LDE1 = NM1;
            }
            // C -- MASS MATRIX
            if (IMPLCT)
            {
                if (MLMAS != NM1)
                {
                    LDMAS = MLMAS + MUMAS + 1;
                    if (JBAND)
                    {
                        IJOB = 4;
                    }
                    else
                    {
                        IJOB = 3;
                    }
                }
                else
                {
                    MUMAS = NM1;
                    LDMAS = NM1;
                    IJOB = 5;
                }
                // C ------ BANDWITH OF "MAS" NOT SMALLER THAN BANDWITH OF "JAC"
                if (MLMAS > MLJAC || MUMAS > MUJAC)
                {
                    //ERROR-ERROR             WRITE (6,*) 'BANDWITH OF "MAS" NOT SMALLER THAN BANDWITH OF"JAC"';
                    ARRET = true;
                }
            }
            else
            {
                LDMAS = 0;
                if (JBAND)
                {
                    IJOB = 2;
                }
                else
                {
                    IJOB = 1;
                    if (N > 2 && IWORK[1 + o_iwork] != 0) IJOB = 7;
                }
            }
            LDMAS2 = Math.Max(1, LDMAS);
            // C ------ HESSENBERG OPTION ONLY FOR EXPLICIT EQU. WITH FULL JACOBIAN
            if ((IMPLCT || JBAND) && IJOB == 7)
            {
                //ERROR-ERROR         WRITE(6,*)' HESSENBERG OPTION ONLY FOR EXPLICIT EQUATIONS WITH FULL JACOBIAN';
                ARRET = true;
            }
            // C ------- PREPARE THE ENTRY-POINTS FOR THE ARRAYS IN WORK -----
            IEZ1 = 21;
            IEZ2 = IEZ1 + N;
            IEZ3 = IEZ2 + N;
            IEY0 = IEZ3 + N;
            IESCAL = IEY0 + N;
            IEF1 = IESCAL + N;
            IEF2 = IEF1 + N;
            IEF3 = IEF2 + N;
            IECON = IEF3 + N;
            IEJAC = IECON + 4 * N;
            IEMAS = IEJAC + N * LDJAC;
            IEE1 = IEMAS + NM1 * LDMAS;
            IEE2R = IEE1 + NM1 * LDE1;
            IEE2I = IEE2R + NM1 * LDE1;
            // C ------ TOTAL STORAGE REQUIREMENT -----------
            ISTORE = IEE2I + NM1 * LDE1 - 1;
            if (ISTORE > LWORK)
            {
                //ERROR-ERROR         WRITE(6,*)' INSUFFICIENT STORAGE FOR WORK, MIN. LWORK=',ISTORE;
                ARRET = true;
            }
            // C ------- ENTRY POINTS FOR INTEGER WORKSPACE -----
            IEIP1 = 21;
            IEIP2 = IEIP1 + NM1;
            IEIPH = IEIP2 + NM1;
            // C --------- TOTAL REQUIREMENT ---------------
            ISTORE = IEIPH + NM1 - 1;
            if (ISTORE > LIWORK)
            {
                //ERROR-ERROR         WRITE(6,*)' INSUFF. STORAGE FOR IWORK, MIN. LIWORK=',ISTORE;
                ARRET = true;
            }
            // C ------ WHEN A FAIL HAS OCCURED, WE RETURN WITH IDID=-1
            if (ARRET)
            {
                IDID =  - 1;
                return;
            }
            // C -------- CALL TO CORE INTEGRATOR ------------
            this._radcor.Run(N, FCN, ref X, ref Y, offset_y, XEND, HMAX
                             , ref H, RTOL, offset_rtol, ATOL, offset_atol, ITOL, JAC, IJAC
                             , MLJAC, MUJAC, MAS, MLMAS, MUMAS, SOLOUT
                             , IOUT, ref IDID, NMAX, UROUND, SAFE, THET
                             , FNEWT, QUOT1, QUOT2, NIT, ref IJOB, STARTN
                             , NIND1, NIND2, NIND3, PRED, FACL, FACR
                             , M1, M2, NM1, IMPLCT, JBAND, LDJAC
                             , LDE1, LDMAS2, ref WORK, IEZ1 + o_work, ref WORK, IEZ2 + o_work, ref WORK, IEZ3 + o_work, ref WORK, IEY0 + o_work
                             , ref WORK, IESCAL + o_work, ref WORK, IEF1 + o_work, ref WORK, IEF2 + o_work, ref WORK, IEF3 + o_work, ref WORK, IEJAC + o_work, ref WORK, IEE1 + o_work
                             , ref WORK, IEE2R + o_work, ref WORK, IEE2I + o_work, ref WORK, IEMAS + o_work, ref IWORK, IEIP1 + o_iwork, ref IWORK, IEIP2 + o_iwork, ref IWORK, IEIPH + o_iwork
                             , ref WORK, IECON + o_work, ref NFCN, ref NJAC, ref NSTEP, ref NACCPT, ref NREJCT
                             , ref NDEC, ref NSOL, RPAR, offset_rpar, IPAR, offset_ipar);
            IWORK[14 + o_iwork] = NFCN;
            IWORK[15 + o_iwork] = NJAC;
            IWORK[16 + o_iwork] = NSTEP;
            IWORK[17 + o_iwork] = NACCPT;
            IWORK[18 + o_iwork] = NREJCT;
            IWORK[19 + o_iwork] = NDEC;
            IWORK[20 + o_iwork] = NSOL;
            // C -------- RESTORE TOLERANCES
            EXPM = 1.0E0 / EXPM;
            if (ITOL == 0)
            {
                QUOT = ATOL[1 + o_atol] / RTOL[1 + o_rtol];
                RTOL[1 + o_rtol] = Math.Pow(10.0E0 * RTOL[1 + o_rtol],EXPM);
                ATOL[1 + o_atol] = RTOL[1 + o_rtol] * QUOT;
            }
            else
            {
                for (I = 1; I <= N; I++)
                {
                    QUOT = ATOL[I + o_atol] / RTOL[I + o_rtol];
                    RTOL[I + o_rtol] = Math.Pow(10.0E0 * RTOL[I + o_rtol],EXPM);
                    ATOL[I + o_atol] = RTOL[I + o_rtol] * QUOT;
                }
            }
            // C ----------- RETURN -----------
            return;
            #endregion
        }
    }

    #endregion


    #region The Class: RADCOR
    
    // C
    // C     END OF SUBROUTINE RADAU5
    // C
    // C ***********************************************************
    // C
    public class RADCOR
    {
    
        #region Dependencies
        
        DECOMR _decomr; DECOMC _decomc; SLVRAD _slvrad; ESTRAD _estrad; 
        #endregion
        #region Common variables
        
        #region Common Block: CONRA5 Declaration
        
        CommonBlock _conra5;
        Oint NN; Oint NN2; Oint NN3; Oint NN4; 
        Odouble XSOL; Odouble HSOL; Odouble C2M1; Odouble C1M1; 
        #endregion
        #region Common Block: LINAL Declaration
        
        CommonBlock _linal;
        Oint MLE; Oint MUE; Oint MBJAC; Oint MBB; Oint MDIAG; Oint MDIFF; Oint MBDIAG; 
        #endregion
        #endregion
        public RADCOR(DECOMR decomr, DECOMC decomc, SLVRAD slvrad, ESTRAD estrad, CommonBlock CONRA5, CommonBlock LINAL)
        {
    
            #region Set Dependencies
            
            this._decomr = decomr; this._decomc = decomc; this._slvrad = slvrad; this._estrad = estrad; 
            #endregion
            #region Common varaible Initialization
            
            #region Common Block: CONRA5 Initialization
            
            this._conra5 = CONRA5;
            NN = CONRA5.intData[0];
            NN2 = CONRA5.intData[1];
            NN3 = CONRA5.intData[2];
            NN4 = CONRA5.intData[3];
            XSOL = CONRA5.doubleData[0];
            HSOL = CONRA5.doubleData[1];
            C2M1 = CONRA5.doubleData[2];
            C1M1 = CONRA5.doubleData[3];
            #endregion
            #region Common Block: LINAL Initialization
            
            this._linal = LINAL;
            MLE = LINAL.intData[0];
            MUE = LINAL.intData[1];
            MBJAC = LINAL.intData[2];
            MBB = LINAL.intData[3];
            MDIAG = LINAL.intData[4];
            MDIFF = LINAL.intData[5];
            MBDIAG = LINAL.intData[6];
            #endregion
            #endregion
        }
    
        public RADCOR()
        {
    
            #region Initialization Common Blocks
            
            CommonBlock CONRA5 = new CommonBlock(4, 4, 0, 0);
            CommonBlock LINAL = new CommonBlock(0, 7, 0, 0);
            #endregion
            #region Dependencies (Initialization)
            
            DEC dec = new DEC();
            DECB decb = new DECB();
            ELMHES elmhes = new ELMHES();
            DECH dech = new DECH();
            DECC decc = new DECC();
            DECBC decbc = new DECBC();
            DECHC dechc = new DECHC();
            SOL sol = new SOL();
            SOLC solc = new SOLC();
            SOLB solb = new SOLB();
            SOLBC solbc = new SOLBC();
            SOLH solh = new SOLH();
            SOLHC solhc = new SOLHC();
            DECOMR decomr = new DECOMR(dec, decb, elmhes, dech, LINAL);
            DECOMC decomc = new DECOMC(decc, decbc, dechc, LINAL);
            SLVRAD slvrad = new SLVRAD(sol, solc, solb, solbc, solh, solhc, LINAL);
            ESTRAD estrad = new ESTRAD(sol, solb, solh, LINAL);
            #endregion
            #region Set Dependencies
            
            this._decomr = decomr; this._decomc = decomc; this._slvrad = slvrad; this._estrad = estrad; 
            #endregion
            #region Common varaible Initialization
            
            #region Common Block: CONRA5 Initialization
            
            this._conra5 = CONRA5;
            NN = CONRA5.intData[0];
            NN2 = CONRA5.intData[1];
            NN3 = CONRA5.intData[2];
            NN4 = CONRA5.intData[3];
            XSOL = CONRA5.doubleData[0];
            HSOL = CONRA5.doubleData[1];
            C2M1 = CONRA5.doubleData[2];
            C1M1 = CONRA5.doubleData[3];
            #endregion
            #region Common Block: LINAL Initialization
            
            this._linal = LINAL;
            MLE = LINAL.intData[0];
            MUE = LINAL.intData[1];
            MBJAC = LINAL.intData[2];
            MBB = LINAL.intData[3];
            MDIAG = LINAL.intData[4];
            MDIFF = LINAL.intData[5];
            MBDIAG = LINAL.intData[6];
            #endregion
            #endregion
        }
        public void Run(int N, IFVPOL FCN, ref double X, ref double[] Y, int offset_y, double XEND, double HMAX
                         , ref double H, double[] RTOL, int offset_rtol, double[] ATOL, int offset_atol, int ITOL, IJVPOL JAC, int IJAC
                         , int MLJAC, int MUJAC, IBBAMPL MAS, int MLMAS, int MUMAS, ISOLOUTR SOLOUT
                         , int IOUT, ref int IDID, int NMAX, double UROUND, double SAFE, double THET
                         , double FNEWT, double QUOT1, double QUOT2, int NIT, ref int IJOB, bool STARTN
                         , int NIND1, int NIND2, int NIND3, bool PRED, double FACL, double FACR
                         , int M1, int M2, int NM1, bool IMPLCT, bool BANDED, int LDJAC
                         , int LDE1, int LDMAS, ref double[] Z1, int offset_z1, ref double[] Z2, int offset_z2, ref double[] Z3, int offset_z3, ref double[] Y0, int offset_y0
                         , ref double[] SCAL, int offset_scal, ref double[] F1, int offset_f1, ref double[] F2, int offset_f2, ref double[] F3, int offset_f3, ref double[] FJAC, int offset_fjac, ref double[] E1, int offset_e1
                         , ref double[] E2R, int offset_e2r, ref double[] E2I, int offset_e2i, ref double[] FMAS, int offset_fmas, ref int[] IP1, int offset_ip1, ref int[] IP2, int offset_ip2, ref int[] IPHES, int offset_iphes
                         , ref double[] CONT, int offset_cont, ref int NFCN, ref int NJAC, ref int NSTEP, ref int NACCPT, ref int NREJCT
                         , ref int NDEC, ref int NSOL, double[] RPAR, int offset_rpar, int[] IPAR, int offset_ipar)
        {
            #region Variables
            
            bool REJECT = false; bool FIRST = false; bool CALJAC = false; bool CALHES = false; bool INDEX1 = false; 
            bool INDEX2 = false;bool INDEX3 = false; bool LAST = false; 
            #endregion
            #region Implicit Variables
            
            int LRC = 0; double SQ6 = 0; double C1 = 0; double C2 = 0; double C1MC2 = 0; double DD1 = 0; double DD2 = 0; 
            double DD3 = 0;double U1 = 0; double ALPH = 0; double BETA = 0; double CNO = 0; double T11 = 0; double T12 = 0; 
            double T13 = 0;double T21 = 0; double T22 = 0; double T23 = 0; double T31 = 0; double TI11 = 0; double TI12 = 0; 
            double TI13 = 0;double TI21 = 0; double TI22 = 0; double TI23 = 0; double TI31 = 0; double TI32 = 0; double TI33 = 0; 
            double POSNEG = 0;double HMAXN = 0; double HOLD = 0; double HOPT = 0; double FACCON = 0; double CFAC = 0; 
            int NSING = 0;double XOLD = 0; int IRTRN = 0; int NRSOL = 0; double XOSOL = 0; int I = 0; int NSOLU = 0; int N2 = 0; 
            int N3 = 0;double HHFAC = 0; int MUJACP = 0; int MD = 0; int J = 0; int K = 0; int MM = 0; int J1 = 0; int LBEG = 0; 
            int LEND = 0;int MUJACJ = 0; int L = 0; int FJAC_J = 0; double YSAFE = 0; double DELT = 0; int FJAC_I = 0; 
            double FAC1 = 0;double ALPHN = 0; double BETAN = 0; int IER = 0; double XPH = 0; double C3Q = 0; double C1Q = 0; 
            double C2Q = 0;double AK1 = 0; double AK2 = 0; double AK3 = 0; double Z1I = 0; double Z2I = 0; double Z3I = 0; 
            int NEWT = 0;double THETA = 0; double A1 = 0; double A2 = 0; double A3 = 0; double DYNO = 0; double DENOM = 0; 
            double THQ = 0;double DYNOLD = 0; double THQOLD = 0; double DYTH = 0; double QNEWT = 0; double F1I = 0; 
            double F2I = 0;double F3I = 0; double FAC = 0; double QUOT = 0; double ERR = 0; double HNEW = 0; double FACGUS = 0; 
            double HACC = 0;double ERRACC = 0; double AK = 0; double ACONT3 = 0; double QT = 0; 
            #endregion
            #region Array Index Correction
            
             int o_y = -1 + offset_y;  int o_rtol = -1 + offset_rtol;  int o_atol = -1 + offset_atol;  int o_z1 = -1 + offset_z1; 
             int o_z2 = -1 + offset_z2; int o_z3 = -1 + offset_z3;  int o_y0 = -1 + offset_y0;  int o_scal = -1 + offset_scal; 
             int o_f1 = -1 + offset_f1; int o_f2 = -1 + offset_f2;  int o_f3 = -1 + offset_f3; 
             int o_fjac = -1 - LDJAC + offset_fjac; int o_e1 = -1 - LDE1 + offset_e1;  int o_e2r = -1 - LDE1 + offset_e2r; 
             int o_e2i = -1 - LDE1 + offset_e2i; int o_fmas = -1 - LDMAS + offset_fmas;  int o_ip1 = -1 + offset_ip1; 
             int o_ip2 = -1 + offset_ip2; int o_iphes = -1 + offset_iphes;  int o_cont = -1 + offset_cont; 
             int o_rpar = -1 + offset_rpar; int o_ipar = -1 + offset_ipar; 
            #endregion
            // C ----------------------------------------------------------
            // C     CORE INTEGRATOR FOR RADAU5
            // C     PARAMETERS SAME AS IN RADAU5 WITH WORKSPACE ADDED 
            // C ---------------------------------------------------------- 
            // C         DECLARATIONS 
            // C ---------------------------------------------------------- 
            // C *** *** *** *** *** *** ***
            // C  INITIALISATIONS
            // C *** *** *** *** *** *** ***
            // C --------- DUPLIFY N FOR COMMON BLOCK CONT -----
            #region Body
            
            NN.v = N;
            NN2.v = 2 * N;
            NN3.v = 3 * N;
            LRC = 4 * N;
            // C -------- CHECK THE INDEX OF THE PROBLEM ----- 
            INDEX1 = NIND1 != 0;
            INDEX2 = NIND2 != 0;
            INDEX3 = NIND3 != 0;
            // C ------- COMPUTE MASS MATRIX FOR IMPLICIT CASE ----------
            if (IMPLCT) MAS.Run(NM1, ref FMAS, offset_fmas, LDMAS, RPAR, offset_rpar, IPAR[1 + o_ipar]);
            // C ---------- CONSTANTS ---------
            SQ6 = Math.Sqrt(6.0E0);
            C1 = (4.0E0 - SQ6) / 10.0E0;
            C2 = (4.0E0 + SQ6) / 10.0E0;
            C1M1.v = C1 - 1.0E0;
            C2M1.v = C2 - 1.0E0;
            C1MC2 = C1 - C2;
            DD1 =  - (13.0E0 + 7.0E0 * SQ6) / 3.0E0;
            DD2 = ( - 13.0E0 + 7.0E0 * SQ6) / 3.0E0;
            DD3 =  - 1.0E0 / 3.0E0;
            U1 = (6.0E0 + Math.Pow(81.0E0,1.0E0 / 3.0E0) - Math.Pow(9.0E0,1.0E0 / 3.0E0)) / 30.0E0;
            ALPH = (12.0E0 - Math.Pow(81.0E0,1.0E0 / 3.0E0) + Math.Pow(9.0E0,1.0E0 / 3.0E0)) / 60.0E0;
            BETA = (Math.Pow(81.0E0,1.0E0 / 3.0E0) + Math.Pow(9.0E0,1.0E0 / 3.0E0)) * Math.Sqrt(3.0E0) / 60.0E0;
            CNO = Math.Pow(ALPH,2) + Math.Pow(BETA,2);
            U1 = 1.0E0 / U1;
            ALPH /= CNO;
            BETA /= CNO;
            T11 = 9.1232394870892942792E-02;
            T12 =  - 0.14125529502095420843E0;
            T13 =  - 3.0029194105147424492E-02;
            T21 = 0.24171793270710701896E0;
            T22 = 0.20412935229379993199E0;
            T23 = 0.38294211275726193779E0;
            T31 = 0.96604818261509293619E0;
            TI11 = 4.3255798900631553510E0;
            TI12 = 0.33919925181580986954E0;
            TI13 = 0.54177053993587487119E0;
            TI21 =  - 4.1787185915519047273E0;
            TI22 =  - 0.32768282076106238708E0;
            TI23 = 0.47662355450055045196E0;
            TI31 =  - 0.50287263494578687595E0;
            TI32 = 2.5719269498556054292E0;
            TI33 =  - 0.59603920482822492497E0;
            if (M1 > 0) IJOB += 10;
            POSNEG = FortranLib.Sign(1.0E0,XEND - X);
            HMAXN = Math.Min(Math.Abs(HMAX), Math.Abs(XEND - X));
            if (Math.Abs(H) <= 10.0E0 * UROUND) H = 1.0E-6;
            H = Math.Min(Math.Abs(H), HMAXN);
            H = FortranLib.Sign(H,POSNEG);
            HOLD = H;
            REJECT = false;
            FIRST = true;
            LAST = false;
            if ((X + H * 1.0001E0 - XEND) * POSNEG >= 0.0E0)
            {
                H = XEND - X;
                LAST = true;
            }
            HOPT = H;
            FACCON = 1.0E0;
            CFAC = SAFE * (1 + 2 * NIT);
            NSING = 0;
            XOLD = X;
            if (IOUT != 0)
            {
                IRTRN = 1;
                NRSOL = 1;
                XOSOL = XOLD;
                XSOL.v = X;
                for (I = 1; I <= N; I++)
                {
                    CONT[I + o_cont] = Y[I + o_y];
                }
                NSOLU = N;
                HSOL.v = HOLD;
                SOLOUT.Run(NRSOL, XOSOL, XSOL.v, Y, offset_y, CONT, offset_cont, LRC
                           , NSOLU, RPAR[1 + o_rpar], IPAR[1 + o_ipar], IRTRN);
                if (IRTRN < 0) goto LABEL179;
            }
            MLE.v = MLJAC;
            MUE.v = MUJAC;
            MBJAC.v = MLJAC + MUJAC + 1;
            MBB.v = MLMAS + MUMAS + 1;
            MDIAG.v = MLE.v + MUE.v + 1;
            MDIFF.v = MLE.v + MUE.v - MUMAS;
            MBDIAG.v = MUMAS + 1;
            N2 = 2 * N;
            N3 = 3 * N;
            if (ITOL == 0)
            {
                for (I = 1; I <= N; I++)
                {
                    SCAL[I + o_scal] = ATOL[1 + o_atol] + RTOL[1 + o_rtol] * Math.Abs(Y[I + o_y]);
                }
            }
            else
            {
                for (I = 1; I <= N; I++)
                {
                    SCAL[I + o_scal] = ATOL[I + o_atol] + RTOL[I + o_rtol] * Math.Abs(Y[I + o_y]);
                }
            }
            HHFAC = H;
            FCN.Run(N, X, Y, offset_y, ref Y0, offset_y0, RPAR[1 + o_rpar], IPAR[1 + o_ipar]);
            NFCN += 1;
            // C --- BASIC INTEGRATION STEP  
        LABEL10:;
            // C *** *** *** *** *** *** ***
            // C  COMPUTATION OF THE JACOBIAN
            // C *** *** *** *** *** *** ***
            NJAC += 1;
            if (IJAC == 0)
            {
                // C --- COMPUTE JACOBIAN MATRIX NUMERICALLY
                if (BANDED)
                {
                    // C --- JACOBIAN IS BANDED
                    MUJACP = MUJAC + 1;
                    MD = Math.Min(MBJAC.v, M2);
                    for (MM = 1; MM <= M1 / M2 + 1; MM++)
                    {
                        for (K = 1; K <= MD; K++)
                        {
                            J = K + (MM - 1) * M2;
                        LABEL12:  F1[J + o_f1] = Y[J + o_y];
                            F2[J + o_f2] = Math.Sqrt(UROUND * Math.Max(1.0E-5, Math.Abs(Y[J + o_y])));
                            Y[J + o_y] += F2[J + o_f2];
                            J += MD;
                            if (J <= MM * M2) goto LABEL12;
                            FCN.Run(N, X, Y, offset_y, ref CONT, offset_cont, RPAR[1 + o_rpar], IPAR[1 + o_ipar]);
                            J = K + (MM - 1) * M2;
                            J1 = K;
                            LBEG = Math.Max(1, J1 - MUJAC) + M1;
                        LABEL14:  LEND = Math.Min(M2, J1 + MLJAC) + M1;
                            Y[J + o_y] = F1[J + o_f1];
                            MUJACJ = MUJACP - J1 - M1;
                            FJAC_J = J * LDJAC + o_fjac;
                            for (L = LBEG; L <= LEND; L++)
                            {
                                FJAC[L + MUJACJ + FJAC_J] = (CONT[L + o_cont] - Y0[L + o_y0]) / F2[J + o_f2];
                            }
                            J += MD;
                            J1 += MD;
                            LBEG = LEND + 1;
                            if (J <= MM * M2) goto LABEL14;
                        }
                    }
                }
                else
                {
                    // C --- JACOBIAN IS FULL
                    for (I = 1; I <= N; I++)
                    {
                        YSAFE = Y[I + o_y];
                        DELT = Math.Sqrt(UROUND * Math.Max(1.0E-5, Math.Abs(YSAFE)));
                        Y[I + o_y] = YSAFE + DELT;
                        FCN.Run(N, X, Y, offset_y, ref CONT, offset_cont, RPAR[1 + o_rpar], IPAR[1 + o_ipar]);
                        FJAC_I = I * LDJAC + o_fjac;
                        for (J = M1 + 1; J <= N; J++)
                        {
                            FJAC[J - M1 + FJAC_I] = (CONT[J + o_cont] - Y0[J + o_y0]) / DELT;
                        }
                        Y[I + o_y] = YSAFE;
                    }
                }
            }
            else
            {
                // C --- COMPUTE JACOBIAN MATRIX ANALYTICALLY
                JAC.Run(N, X, Y, offset_y, ref FJAC, offset_fjac, LDJAC, RPAR[1 + o_rpar]
                        , IPAR[1 + o_ipar]);
            }
            CALJAC = true;
            CALHES = true;
        LABEL20:;
            // C --- COMPUTE THE MATRICES E1 AND E2 AND THEIR DECOMPOSITIONS
            FAC1 = U1 / H;
            ALPHN = ALPH / H;
            BETAN = BETA / H;
            this._decomr.Run(N, ref FJAC, offset_fjac, LDJAC, FMAS, offset_fmas, LDMAS, MLMAS
                             , MUMAS, M1, M2, NM1, FAC1, ref E1, offset_e1
                             , LDE1, ref IP1, offset_ip1, ref IER, IJOB, ref CALHES, ref IPHES, offset_iphes);
            if (IER != 0) goto LABEL78;
            this._decomc.Run(N, FJAC, offset_fjac, LDJAC, FMAS, offset_fmas, LDMAS, MLMAS
                             , MUMAS, M1, M2, NM1, ALPHN, BETAN
                             , ref E2R, offset_e2r, ref E2I, offset_e2i, LDE1, ref IP2, offset_ip2, ref IER, IJOB);
            if (IER != 0) goto LABEL78;
            NDEC += 1;
        LABEL30:;
            NSTEP += 1;
            if (NSTEP > NMAX) goto LABEL178;
            if (0.1E0 * Math.Abs(H) <= Math.Abs(X) * UROUND) goto LABEL177;
            if (INDEX2)
            {
                for (I = NIND1 + 1; I <= NIND1 + NIND2; I++)
                {
                    SCAL[I + o_scal] /= HHFAC;
                }
            }
            if (INDEX3)
            {
                for (I = NIND1 + NIND2 + 1; I <= NIND1 + NIND2 + NIND3; I++)
                {
                    SCAL[I + o_scal] = SCAL[I + o_scal] / (HHFAC * HHFAC);
                }
            }
            XPH = X + H;
            // C *** *** *** *** *** *** ***
            // C  STARTING VALUES FOR NEWTON ITERATION
            // C *** *** *** *** *** *** ***
            if (FIRST || STARTN)
            {
                for (I = 1; I <= N; I++)
                {
                    Z1[I + o_z1] = 0.0E0;
                    Z2[I + o_z2] = 0.0E0;
                    Z3[I + o_z3] = 0.0E0;
                    F1[I + o_f1] = 0.0E0;
                    F2[I + o_f2] = 0.0E0;
                    F3[I + o_f3] = 0.0E0;
                }
            }
            else
            {
                C3Q = H / HOLD;
                C1Q = C1 * C3Q;
                C2Q = C2 * C3Q;
                for (I = 1; I <= N; I++)
                {
                    AK1 = CONT[I + N + o_cont];
                    AK2 = CONT[I + N2 + o_cont];
                    AK3 = CONT[I + N3 + o_cont];
                    Z1I = C1Q * (AK1 + (C1Q - C2M1.v) * (AK2 + (C1Q - C1M1.v) * AK3));
                    Z2I = C2Q * (AK1 + (C2Q - C2M1.v) * (AK2 + (C2Q - C1M1.v) * AK3));
                    Z3I = C3Q * (AK1 + (C3Q - C2M1.v) * (AK2 + (C3Q - C1M1.v) * AK3));
                    Z1[I + o_z1] = Z1I;
                    Z2[I + o_z2] = Z2I;
                    Z3[I + o_z3] = Z3I;
                    F1[I + o_f1] = TI11 * Z1I + TI12 * Z2I + TI13 * Z3I;
                    F2[I + o_f2] = TI21 * Z1I + TI22 * Z2I + TI23 * Z3I;
                    F3[I + o_f3] = TI31 * Z1I + TI32 * Z2I + TI33 * Z3I;
                }
            }
            // C *** *** *** *** *** *** ***
            // C  LOOP FOR THE SIMPLIFIED NEWTON ITERATION
            // C *** *** *** *** *** *** ***
            NEWT = 0;
            FACCON = Math.Pow(Math.Max(FACCON, UROUND),0.8E0);
            THETA = Math.Abs(THET);
        LABEL40:;
            if (NEWT >= NIT) goto LABEL78;
            // C ---     COMPUTE THE RIGHT-HAND SIDE
            for (I = 1; I <= N; I++)
            {
                CONT[I + o_cont] = Y[I + o_y] + Z1[I + o_z1];
            }
            FCN.Run(N, X + C1 * H, CONT, offset_cont, ref Z1, offset_z1, RPAR[1 + o_rpar], IPAR[1 + o_ipar]);
            for (I = 1; I <= N; I++)
            {
                CONT[I + o_cont] = Y[I + o_y] + Z2[I + o_z2];
            }
            FCN.Run(N, X + C2 * H, CONT, offset_cont, ref Z2, offset_z2, RPAR[1 + o_rpar], IPAR[1 + o_ipar]);
            for (I = 1; I <= N; I++)
            {
                CONT[I + o_cont] = Y[I + o_y] + Z3[I + o_z3];
            }
            FCN.Run(N, XPH, CONT, offset_cont, ref Z3, offset_z3, RPAR[1 + o_rpar], IPAR[1 + o_ipar]);
            NFCN += 3;
            // C ---     SOLVE THE LINEAR SYSTEMS
            for (I = 1; I <= N; I++)
            {
                A1 = Z1[I + o_z1];
                A2 = Z2[I + o_z2];
                A3 = Z3[I + o_z3];
                Z1[I + o_z1] = TI11 * A1 + TI12 * A2 + TI13 * A3;
                Z2[I + o_z2] = TI21 * A1 + TI22 * A2 + TI23 * A3;
                Z3[I + o_z3] = TI31 * A1 + TI32 * A2 + TI33 * A3;
            }
            this._slvrad.Run(N, FJAC, offset_fjac, LDJAC, MLJAC, MUJAC, FMAS, offset_fmas
                             , LDMAS, MLMAS, MUMAS, M1, M2, NM1
                             , FAC1, ALPHN, BETAN, E1, offset_e1, E2R, offset_e2r, E2I, offset_e2i
                             , LDE1, ref Z1, offset_z1, ref Z2, offset_z2, ref Z3, offset_z3, F1, offset_f1, F2, offset_f2
                             , F3, offset_f3, CONT[1 + o_cont], IP1, offset_ip1, IP2, offset_ip2, IPHES, offset_iphes, IER
                             , IJOB);
            NSOL += 1;
            NEWT += 1;
            DYNO = 0.0E0;
            for (I = 1; I <= N; I++)
            {
                DENOM = SCAL[I + o_scal];
                DYNO += Math.Pow(Z1[I + o_z1] / DENOM,2) + Math.Pow(Z2[I + o_z2] / DENOM,2) + Math.Pow(Z3[I + o_z3] / DENOM,2);
            }
            DYNO = Math.Sqrt(DYNO / N3);
            // C ---     BAD CONVERGENCE OR NUMBER OF ITERATIONS TO LARGE
            if (NEWT > 1 && NEWT < NIT)
            {
                THQ = DYNO / DYNOLD;
                if (NEWT == 2)
                {
                    THETA = THQ;
                }
                else
                {
                    THETA = Math.Sqrt(THQ * THQOLD);
                }
                THQOLD = THQ;
                if (THETA < 0.99E0)
                {
                    FACCON = THETA / (1.0E0 - THETA);
                    DYTH = FACCON * DYNO * Math.Pow(THETA,NIT - 1 - NEWT) / FNEWT;
                    if (DYTH >= 1.0E0)
                    {
                        QNEWT = Math.Max(1.0E-4, Math.Min(20.0E0, DYTH));
                        HHFAC = .8E0 * Math.Pow(QNEWT, - 1.0E0 / (4.0E0 + NIT - 1 - NEWT));
                        H *= HHFAC;
                        REJECT = true;
                        LAST = false;
                        if (CALJAC) goto LABEL20;
                        goto LABEL10;
                    }
                }
                else
                {
                    goto LABEL78;
                }
            }
            DYNOLD = Math.Max(DYNO, UROUND);
            for (I = 1; I <= N; I++)
            {
                F1I = F1[I + o_f1] + Z1[I + o_z1];
                F2I = F2[I + o_f2] + Z2[I + o_z2];
                F3I = F3[I + o_f3] + Z3[I + o_z3];
                F1[I + o_f1] = F1I;
                F2[I + o_f2] = F2I;
                F3[I + o_f3] = F3I;
                Z1[I + o_z1] = T11 * F1I + T12 * F2I + T13 * F3I;
                Z2[I + o_z2] = T21 * F1I + T22 * F2I + T23 * F3I;
                Z3[I + o_z3] = T31 * F1I + F2I;
            }
            if (FACCON * DYNO > FNEWT) goto LABEL40;
            // C --- ERROR ESTIMATION  
            this._estrad.Run(N, FJAC, offset_fjac, LDJAC, MLJAC, MUJAC, FMAS, offset_fmas
                             , LDMAS, MLMAS, MUMAS, H, DD1, DD2
                             , DD3, FCN, ref NFCN, Y0, offset_y0, Y, offset_y, IJOB
                             , X, M1, M2, NM1, E1, offset_e1, LDE1
                             , Z1, offset_z1, Z2, offset_z2, Z3, offset_z3, ref CONT, offset_cont, ref F1, offset_f1, ref F2, offset_f2
                             , IP1, offset_ip1, IPHES, offset_iphes, SCAL, offset_scal, ref ERR, FIRST, REJECT
                             , FAC1, RPAR, offset_rpar, IPAR, offset_ipar);
            // C --- COMPUTATION OF HNEW
            // C --- WE REQUIRE .2<=HNEW/H<=8.
            FAC = Math.Min(SAFE, CFAC / (NEWT + 2 * NIT));
            QUOT = Math.Max(FACR, Math.Min(FACL, Math.Pow(ERR,.25E0) / FAC));
            HNEW = H / QUOT;
            // C *** *** *** *** *** *** ***
            // C  IS THE ERROR SMALL ENOUGH ?
            // C *** *** *** *** *** *** ***
            if (ERR < 1.0E0)
            {
                // C --- STEP IS ACCEPTED  
                FIRST = false;
                NACCPT += 1;
                if (PRED)
                {
                    // C       --- PREDICTIVE CONTROLLER OF GUSTAFSSON
                    if (NACCPT > 1)
                    {
                        FACGUS = (HACC / H) * Math.Pow(Math.Pow(ERR,2) / ERRACC,0.25E0) / SAFE;
                        FACGUS = Math.Max(FACR, Math.Min(FACL, FACGUS));
                        QUOT = Math.Max(QUOT, FACGUS);
                        HNEW = H / QUOT;
                    }
                    HACC = H;
                    ERRACC = Math.Max(1.0E-2, ERR);
                }
                XOLD = X;
                HOLD = H;
                X = XPH;
                for (I = 1; I <= N; I++)
                {
                    Y[I + o_y] += Z3[I + o_z3];
                    Z2I = Z2[I + o_z2];
                    Z1I = Z1[I + o_z1];
                    CONT[I + N + o_cont] = (Z2I - Z3[I + o_z3]) / C2M1.v;
                    AK = (Z1I - Z2I) / C1MC2;
                    ACONT3 = Z1I / C1;
                    ACONT3 = (AK - ACONT3) / C2;
                    CONT[I + N2 + o_cont] = (AK - CONT[I + N + o_cont]) / C1M1.v;
                    CONT[I + N3 + o_cont] = CONT[I + N2 + o_cont] - ACONT3;
                }
                if (ITOL == 0)
                {
                    for (I = 1; I <= N; I++)
                    {
                        SCAL[I + o_scal] = ATOL[1 + o_atol] + RTOL[1 + o_rtol] * Math.Abs(Y[I + o_y]);
                    }
                }
                else
                {
                    for (I = 1; I <= N; I++)
                    {
                        SCAL[I + o_scal] = ATOL[I + o_atol] + RTOL[I + o_rtol] * Math.Abs(Y[I + o_y]);
                    }
                }
                if (IOUT != 0)
                {
                    NRSOL = NACCPT + 1;
                    XSOL.v = X;
                    XOSOL = XOLD;
                    for (I = 1; I <= N; I++)
                    {
                        CONT[I + o_cont] = Y[I + o_y];
                    }
                    NSOLU = N;
                    HSOL.v = HOLD;
                    SOLOUT.Run(NRSOL, XOSOL, XSOL.v, Y, offset_y, CONT, offset_cont, LRC
                               , NSOLU, RPAR[1 + o_rpar], IPAR[1 + o_ipar], IRTRN);
                    if (IRTRN < 0) goto LABEL179;
                }
                CALJAC = false;
                if (LAST)
                {
                    H = HOPT;
                    IDID = 1;
                    return;
                }
                FCN.Run(N, X, Y, offset_y, ref Y0, offset_y0, RPAR[1 + o_rpar], IPAR[1 + o_ipar]);
                NFCN += 1;
                HNEW = POSNEG * Math.Min(Math.Abs(HNEW), HMAXN);
                HOPT = HNEW;
                HOPT = Math.Min(H, HNEW);
                if (REJECT) HNEW = POSNEG * Math.Min(Math.Abs(HNEW), Math.Abs(H));
                REJECT = false;
                if ((X + HNEW / QUOT1 - XEND) * POSNEG >= 0.0E0)
                {
                    H = XEND - X;
                    LAST = true;
                }
                else
                {
                    QT = HNEW / H;
                    HHFAC = H;
                    if (THETA <= THET && QT >= QUOT1 && QT <= QUOT2) goto LABEL30;
                    H = HNEW;
                }
                HHFAC = H;
                if (THETA <= THET) goto LABEL20;
                goto LABEL10;
            }
            else
            {
                // C --- STEP IS REJECTED  
                REJECT = true;
                LAST = false;
                if (FIRST)
                {
                    H *= 0.1E0;
                    HHFAC = 0.1E0;
                }
                else
                {
                    HHFAC = HNEW / H;
                    H = HNEW;
                }
                if (NACCPT >= 1) NREJCT += 1;
                if (CALJAC) goto LABEL20;
                goto LABEL10;
            }
            // C --- UNEXPECTED STEP-REJECTION
        LABEL78:;
            if (IER != 0)
            {
                NSING += 1;
                if (NSING >= 5) goto LABEL176;
            }
            H *= 0.5E0;
            HHFAC = 0.5E0;
            REJECT = true;
            LAST = false;
            if (CALJAC) goto LABEL20;
            goto LABEL10;
            // C --- FAIL EXIT
        LABEL176:;
            //ERROR-ERROR      WRITE(6,979)X   ;
            //ERROR-ERROR      WRITE(6,*) ' MATRIX IS REPEATEDLY SINGULAR, IER=',IER;
            IDID =  - 4;
            return;
        LABEL177:;
            //ERROR-ERROR      WRITE(6,979)X   ;
            //ERROR-ERROR      WRITE(6,*) ' STEP SIZE T0O SMALL, H=',H;
            IDID =  - 3;
            return;
        LABEL178:;
            //ERROR-ERROR      WRITE(6,979)X   ;
            //ERROR-ERROR      WRITE(6,*) ' MORE THAN NMAX =',NMAX,'STEPS ARE NEEDED' ;
            IDID =  - 2;
            return;
            // C --- EXIT CAUSED BY SOLOUT
        LABEL179:;
            //ERROR-ERROR      WRITE(6,979)X;
            IDID = 2;
            return;
            #endregion
        }
    }

    #endregion


    #region The Class: CONTR5
    
    // C
    // C     END OF SUBROUTINE RADCOR
    // C
    // C ***********************************************************
    // C
    public class CONTR5
    {
    
        #region Common variables
        
        #region Common Block: CONRA5 Declaration
        
        CommonBlock _conra5;
        Oint NN; Oint NN2; Oint NN3; Oint NN4; 
        Odouble XSOL; Odouble HSOL; Odouble C2M1; Odouble C1M1; 
        #endregion
        #endregion
        public CONTR5(CommonBlock CONRA5)
        {
    
            #region Common varaible Initialization
            
            #region Common Block: CONRA5 Initialization
            
            this._conra5 = CONRA5;
            NN = CONRA5.intData[0];
            NN2 = CONRA5.intData[1];
            NN3 = CONRA5.intData[2];
            NN4 = CONRA5.intData[3];
            XSOL = CONRA5.doubleData[0];
            HSOL = CONRA5.doubleData[1];
            C2M1 = CONRA5.doubleData[2];
            C1M1 = CONRA5.doubleData[3];
            #endregion
            #endregion
        }
    
        public CONTR5()
        {
    
            #region Initialization Common Blocks
            
            CommonBlock CONRA5 = new CommonBlock(4, 4, 0, 0);
            #endregion
            #region Common varaible Initialization
            
            #region Common Block: CONRA5 Initialization
            
            this._conra5 = CONRA5;
            NN = CONRA5.intData[0];
            NN2 = CONRA5.intData[1];
            NN3 = CONRA5.intData[2];
            NN4 = CONRA5.intData[3];
            XSOL = CONRA5.doubleData[0];
            HSOL = CONRA5.doubleData[1];
            C2M1 = CONRA5.doubleData[2];
            C1M1 = CONRA5.doubleData[3];
            #endregion
            #endregion
        }
        public double Run(int I, double X, double[] CONT, int offset_cont, int LRC)
        {
        double contr5 = 0;
            #region Implicit Variables
            
            double S = 0; 
            #endregion
            #region Array Index Correction
            
             int o_cont = -1 + offset_cont; 
            #endregion
            // C ----------------------------------------------------------
            // C     THIS FUNCTION CAN BE USED FOR CONINUOUS OUTPUT. IT PROVIDES AN
            // C     APPROXIMATION TO THE I-TH COMPONENT OF THE SOLUTION AT X.
            // C     IT GIVES THE VALUE OF THE COLLOCATION POLYNOMIAL, DEFINED FOR
            // C     THE LAST SUCCESSFULLY COMPUTED STEP (BY RADAU5).
            // C ----------------------------------------------------------
            S = (X - XSOL.v) / HSOL.v;
            contr5 = CONT[I + o_cont] + S * (CONT[I + NN.v + o_cont] + (S - C2M1.v) * (CONT[I + NN2.v + o_cont] + (S - C1M1.v) * CONT[I + NN3.v + o_cont]));
            return contr5;
        }
    }

    #endregion

    // C
    // C     END OF FUNCTION CONTR5
    // C
    // C ***********************************************************
}
