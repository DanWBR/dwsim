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

namespace DotNumerics.LinearAlgebra.CSLapack
{
    /// <summary>
    /// -- LAPACK auxiliary routine (version 3.1) --
    /// Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
    /// November 2006
    /// Purpose
    /// =======
    /// 
    /// DLASY2 solves for the N1 by N2 matrix X, 1 .LE. N1,N2 .LE. 2, in
    /// 
    /// op(TL)*X + ISGN*X*op(TR) = SCALE*B,
    /// 
    /// where TL is N1 by N1, TR is N2 by N2, B is N1 by N2, and ISGN = 1 or
    /// -1.  op(T) = T or T', where T' denotes the transpose of T.
    /// 
    ///</summary>
    public class DLASY2
    {
    

        #region Dependencies
        
        IDAMAX _idamax; DLAMCH _dlamch; DCOPY _dcopy; DSWAP _dswap; 

        #endregion


        #region Variables
        
        const double ZERO = 0.0E+0; const double ONE = 1.0E+0; const double TWO = 2.0E+0; const double HALF = 0.5E+0; 
        const double EIGHT = 8.0E+0;bool[] BSWPIV = new bool[4];  int o_bswpiv = -1; 
        bool[] XSWPIV = new bool[4];  int o_xswpiv = -1;int[] JPIV = new int[4]; 
        int[] LOCL21 = new int[4];  int o_locl21 = -1;int[] LOCU12 = new int[4]; int o_locu12 = -1; 
        int[] LOCU22 = new int[4]; int o_locu22 = -1;double[] BTMP = new double[4]; 
        double[] T16 = new double[4 * 4];double[] TMP = new double[4]; double[] X2 = new double[2]; 

        #endregion

        public DLASY2(IDAMAX idamax, DLAMCH dlamch, DCOPY dcopy, DSWAP dswap)
        {
    

            #region Set Dependencies
            
            this._idamax = idamax; this._dlamch = dlamch; this._dcopy = dcopy; this._dswap = dswap; 

            #endregion


            #region Data Initialization
            
            //LOCU12/3,4,1,2
            LOCU12[1 + o_locu12] = 3;
            LOCU12[2 + o_locu12] = 4;
            LOCU12[3 + o_locu12] = 1;
            LOCU12[4 + o_locu12] = 2;
            //LOCL21/2,1,4,3
            LOCL21[1 + o_locl21] = 2;
            LOCL21[2 + o_locl21] = 1;
            LOCL21[3 + o_locl21] = 4;
            LOCL21[4 + o_locl21] = 3;
            //LOCU22/4,3,2,1
            LOCU22[1 + o_locu22] = 4;
            LOCU22[2 + o_locu22] = 3;
            LOCU22[3 + o_locu22] = 2;
            LOCU22[4 + o_locu22] = 1;
            //XSWPIV/.FALSE.,.FALSE.,.TRUE.,.TRUE.
            XSWPIV[1 + o_xswpiv] = false;
            XSWPIV[2 + o_xswpiv] = false;
            XSWPIV[3 + o_xswpiv] = true;
            XSWPIV[4 + o_xswpiv] = true;
            //BSWPIV/.FALSE.,.TRUE.,.FALSE.,.TRUE.
            BSWPIV[1 + o_bswpiv] = false;
            BSWPIV[2 + o_bswpiv] = true;
            BSWPIV[3 + o_bswpiv] = false;
            BSWPIV[4 + o_bswpiv] = true;

            #endregion

        }
    
        public DLASY2()
        {
    

            #region Dependencies (Initialization)
            
            IDAMAX idamax = new IDAMAX();
            LSAME lsame = new LSAME();
            DLAMC3 dlamc3 = new DLAMC3();
            DCOPY dcopy = new DCOPY();
            DSWAP dswap = new DSWAP();
            DLAMC1 dlamc1 = new DLAMC1(dlamc3);
            DLAMC4 dlamc4 = new DLAMC4(dlamc3);
            DLAMC5 dlamc5 = new DLAMC5(dlamc3);
            DLAMC2 dlamc2 = new DLAMC2(dlamc3, dlamc1, dlamc4, dlamc5);
            DLAMCH dlamch = new DLAMCH(lsame, dlamc2);

            #endregion


            #region Set Dependencies
            
            this._idamax = idamax; this._dlamch = dlamch; this._dcopy = dcopy; this._dswap = dswap; 

            #endregion


            #region Data Initialization
            
            //LOCU12/3,4,1,2
            LOCU12[1 + o_locu12] = 3;
            LOCU12[2 + o_locu12] = 4;
            LOCU12[3 + o_locu12] = 1;
            LOCU12[4 + o_locu12] = 2;
            //LOCL21/2,1,4,3
            LOCL21[1 + o_locl21] = 2;
            LOCL21[2 + o_locl21] = 1;
            LOCL21[3 + o_locl21] = 4;
            LOCL21[4 + o_locl21] = 3;
            //LOCU22/4,3,2,1
            LOCU22[1 + o_locu22] = 4;
            LOCU22[2 + o_locu22] = 3;
            LOCU22[3 + o_locu22] = 2;
            LOCU22[4 + o_locu22] = 1;
            //XSWPIV/.FALSE.,.FALSE.,.TRUE.,.TRUE.
            XSWPIV[1 + o_xswpiv] = false;
            XSWPIV[2 + o_xswpiv] = false;
            XSWPIV[3 + o_xswpiv] = true;
            XSWPIV[4 + o_xswpiv] = true;
            //BSWPIV/.FALSE.,.TRUE.,.FALSE.,.TRUE.
            BSWPIV[1 + o_bswpiv] = false;
            BSWPIV[2 + o_bswpiv] = true;
            BSWPIV[3 + o_bswpiv] = false;
            BSWPIV[4 + o_bswpiv] = true;

            #endregion

        }
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DLASY2 solves for the N1 by N2 matrix X, 1 .LE. N1,N2 .LE. 2, in
        /// 
        /// op(TL)*X + ISGN*X*op(TR) = SCALE*B,
        /// 
        /// where TL is N1 by N1, TR is N2 by N2, B is N1 by N2, and ISGN = 1 or
        /// -1.  op(T) = T or T', where T' denotes the transpose of T.
        /// 
        ///</summary>
        /// <param name="LTRANL">
        /// (input) LOGICAL
        /// On entry, LTRANL specifies the op(TL):
        /// = .FALSE., op(TL) = TL,
        /// = .TRUE., op(TL) = TL'.
        ///</param>
        /// <param name="LTRANR">
        /// (input) LOGICAL
        /// On entry, LTRANR specifies the op(TR):
        /// = .FALSE., op(TR) = TR,
        /// = .TRUE., op(TR) = TR'.
        ///</param>
        /// <param name="ISGN">
        /// (input) INTEGER
        /// On entry, ISGN specifies the sign of the equation
        /// as described before. ISGN may only be 1 or -1.
        ///</param>
        /// <param name="N1">
        /// (input) INTEGER
        /// On entry, N1 specifies the order of matrix TL.
        /// N1 may only be 0, 1 or 2.
        ///</param>
        /// <param name="N2">
        /// (input) INTEGER
        /// On entry, N2 specifies the order of matrix TR.
        /// N2 may only be 0, 1 or 2.
        ///</param>
        /// <param name="TL">
        /// (input) DOUBLE PRECISION array, dimension (LDTL,2)
        /// On entry, TL contains an N1 by N1 matrix.
        ///</param>
        /// <param name="LDTL">
        /// (input) INTEGER
        /// The leading dimension of the matrix TL. LDTL .GE. max(1,N1).
        ///</param>
        /// <param name="TR">
        /// (input) DOUBLE PRECISION array, dimension (LDTR,2)
        /// On entry, TR contains an N2 by N2 matrix.
        ///</param>
        /// <param name="LDTR">
        /// (input) INTEGER
        /// The leading dimension of the matrix TR. LDTR .GE. max(1,N2).
        ///</param>
        /// <param name="B">
        /// (input) DOUBLE PRECISION array, dimension (LDB,2)
        /// On entry, the N1 by N2 matrix B contains the right-hand
        /// side of the equation.
        ///</param>
        /// <param name="LDB">
        /// (input) INTEGER
        /// The leading dimension of the matrix B. LDB .GE. max(1,N1).
        ///</param>
        /// <param name="SCALE">
        /// (output) DOUBLE PRECISION
        /// On exit, SCALE contains the scale factor. SCALE is chosen
        /// less than or equal to 1 to prevent the solution overflowing.
        ///</param>
        /// <param name="X">
        /// (output) DOUBLE PRECISION array, dimension (LDX,2)
        /// On exit, X contains the N1 by N2 solution.
        ///</param>
        /// <param name="LDX">
        /// (input) INTEGER
        /// The leading dimension of the matrix X. LDX .GE. max(1,N1).
        ///</param>
        /// <param name="XNORM">
        /// (output) DOUBLE PRECISION
        /// On exit, XNORM is the infinity-norm of the solution.
        ///</param>
        /// <param name="INFO">
        /// (output) INTEGER
        /// On exit, INFO is set to
        /// 0: successful exit.
        /// 1: TL and TR have too close eigenvalues, so TL or
        /// TR is perturbed to get a nonsingular equation.
        /// NOTE: In the interests of speed, this routine does not
        /// check the inputs for errors.
        ///</param>
        public void Run(bool LTRANL, bool LTRANR, int ISGN, int N1, int N2, double[] TL, int offset_tl
                         , int LDTL, double[] TR, int offset_tr, int LDTR, double[] B, int offset_b, int LDB, ref double SCALE
                         , ref double[] X, int offset_x, int LDX, ref double XNORM, ref int INFO)
        {

            #region Variables
            
            bool BSWAP = false; bool XSWAP = false; int I = 0; int IP = 0; int IPIV = 0; int IPSV = 0; int J = 0; int JP = 0; 
            int JPSV = 0;int K = 0; double BET = 0; double EPS = 0; double GAM = 0; double L21 = 0; double SGN = 0; 
            double SMIN = 0;double SMLNUM = 0; double TAU1 = 0; double TEMP = 0; double U11 = 0; double U12 = 0; double U22 = 0; 
            double XMAX = 0; int o_jpiv = -1; int offset_btmp = 0; int o_btmp = -1; 
            int offset_t16 = 0; int o_t16 = -5;int offset_tmp = 0; int o_tmp = -1; int o_x2 = -1; 

            #endregion


            #region Implicit Variables
            
            int T16_I = 0; 

            #endregion


            #region Array Index Correction
            
             int o_tl = -1 - LDTL + offset_tl;  int o_tr = -1 - LDTR + offset_tr;  int o_b = -1 - LDB + offset_b; 
             int o_x = -1 - LDX + offset_x;

            #endregion


            #region Prolog
            
            // *
            // *  -- LAPACK auxiliary routine (version 3.1) --
            // *     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
            // *     November 2006
            // *
            // *     .. Scalar Arguments ..
            // *     ..
            // *     .. Array Arguments ..
            // *     ..
            // *
            // *  Purpose
            // *  =======
            // *
            // *  DLASY2 solves for the N1 by N2 matrix X, 1 <= N1,N2 <= 2, in
            // *
            // *         op(TL)*X + ISGN*X*op(TR) = SCALE*B,
            // *
            // *  where TL is N1 by N1, TR is N2 by N2, B is N1 by N2, and ISGN = 1 or
            // *  -1.  op(T) = T or T', where T' denotes the transpose of T.
            // *
            // *  Arguments
            // *  =========
            // *
            // *  LTRANL  (input) LOGICAL
            // *          On entry, LTRANL specifies the op(TL):
            // *             = .FALSE., op(TL) = TL,
            // *             = .TRUE., op(TL) = TL'.
            // *
            // *  LTRANR  (input) LOGICAL
            // *          On entry, LTRANR specifies the op(TR):
            // *            = .FALSE., op(TR) = TR,
            // *            = .TRUE., op(TR) = TR'.
            // *
            // *  ISGN    (input) INTEGER
            // *          On entry, ISGN specifies the sign of the equation
            // *          as described before. ISGN may only be 1 or -1.
            // *
            // *  N1      (input) INTEGER
            // *          On entry, N1 specifies the order of matrix TL.
            // *          N1 may only be 0, 1 or 2.
            // *
            // *  N2      (input) INTEGER
            // *          On entry, N2 specifies the order of matrix TR.
            // *          N2 may only be 0, 1 or 2.
            // *
            // *  TL      (input) DOUBLE PRECISION array, dimension (LDTL,2)
            // *          On entry, TL contains an N1 by N1 matrix.
            // *
            // *  LDTL    (input) INTEGER
            // *          The leading dimension of the matrix TL. LDTL >= max(1,N1).
            // *
            // *  TR      (input) DOUBLE PRECISION array, dimension (LDTR,2)
            // *          On entry, TR contains an N2 by N2 matrix.
            // *
            // *  LDTR    (input) INTEGER
            // *          The leading dimension of the matrix TR. LDTR >= max(1,N2).
            // *
            // *  B       (input) DOUBLE PRECISION array, dimension (LDB,2)
            // *          On entry, the N1 by N2 matrix B contains the right-hand
            // *          side of the equation.
            // *
            // *  LDB     (input) INTEGER
            // *          The leading dimension of the matrix B. LDB >= max(1,N1).
            // *
            // *  SCALE   (output) DOUBLE PRECISION
            // *          On exit, SCALE contains the scale factor. SCALE is chosen
            // *          less than or equal to 1 to prevent the solution overflowing.
            // *
            // *  X       (output) DOUBLE PRECISION array, dimension (LDX,2)
            // *          On exit, X contains the N1 by N2 solution.
            // *
            // *  LDX     (input) INTEGER
            // *          The leading dimension of the matrix X. LDX >= max(1,N1).
            // *
            // *  XNORM   (output) DOUBLE PRECISION
            // *          On exit, XNORM is the infinity-norm of the solution.
            // *
            // *  INFO    (output) INTEGER
            // *          On exit, INFO is set to
            // *             0: successful exit.
            // *             1: TL and TR have too close eigenvalues, so TL or
            // *                TR is perturbed to get a nonsingular equation.
            // *          NOTE: In the interests of speed, this routine does not
            // *                check the inputs for errors.
            // *
            // * =====================================================================
            // *
            // *     .. Parameters ..
            // *     ..
            // *     .. Local Scalars ..
            // *     ..
            // *     .. Local Arrays ..
            // *     ..
            // *     .. External Functions ..
            // *     ..
            // *     .. External Subroutines ..
            // *     ..
            // *     .. Intrinsic Functions ..
            //      INTRINSIC          ABS, MAX;
            // *     ..
            // *     .. Data statements ..
            // *     ..
            // *     .. Executable Statements ..
            // *
            // *     Do not check the input parameters for errors
            // *

            #endregion


            #region Body
            
            INFO = 0;
            // *
            // *     Quick return if possible
            // *
            if (N1 == 0 || N2 == 0) return;
            // *
            // *     Set constants to control overflow
            // *
            EPS = this._dlamch.Run("P");
            SMLNUM = this._dlamch.Run("S") / EPS;
            SGN = ISGN;
            // *
            K = N1 + N1 + N2 - 2;
            switch (K)
            {
                case 1: goto LABEL10;
                case 2: goto LABEL20;
                case 3: goto LABEL30;
                case 4: goto LABEL50;
            }
            // *
            // *     1 by 1: TL11*X + SGN*X*TR11 = B11
            // *
        LABEL10:;
            TAU1 = TL[1+1 * LDTL + o_tl] + SGN * TR[1+1 * LDTR + o_tr];
            BET = Math.Abs(TAU1);
            if (BET <= SMLNUM)
            {
                TAU1 = SMLNUM;
                BET = SMLNUM;
                INFO = 1;
            }
            // *
            SCALE = ONE;
            GAM = Math.Abs(B[1+1 * LDB + o_b]);
            if (SMLNUM * GAM > BET) SCALE = ONE / GAM;
            // *
            X[1+1 * LDX + o_x] = (B[1+1 * LDB + o_b] * SCALE) / TAU1;
            XNORM = Math.Abs(X[1+1 * LDX + o_x]);
            return;
            // *
            // *     1 by 2:
            // *     TL11*[X11 X12] + ISGN*[X11 X12]*op[TR11 TR12]  = [B11 B12]
            // *                                       [TR21 TR22]
            // *
        LABEL20:;
            // *
            SMIN = Math.Max(EPS * Math.Max(Math.Abs(TL[1+1 * LDTL + o_tl]), Math.Max(Math.Abs(TR[1+1 * LDTR + o_tr]), Math.Max(Math.Abs(TR[1+2 * LDTR + o_tr]), Math.Max(Math.Abs(TR[2+1 * LDTR + o_tr]), Math.Abs(TR[2+2 * LDTR + o_tr]))))), SMLNUM);
            TMP[1 + o_tmp] = TL[1+1 * LDTL + o_tl] + SGN * TR[1+1 * LDTR + o_tr];
            TMP[4 + o_tmp] = TL[1+1 * LDTL + o_tl] + SGN * TR[2+2 * LDTR + o_tr];
            if (LTRANR)
            {
                TMP[2 + o_tmp] = SGN * TR[2+1 * LDTR + o_tr];
                TMP[3 + o_tmp] = SGN * TR[1+2 * LDTR + o_tr];
            }
            else
            {
                TMP[2 + o_tmp] = SGN * TR[1+2 * LDTR + o_tr];
                TMP[3 + o_tmp] = SGN * TR[2+1 * LDTR + o_tr];
            }
            BTMP[1 + o_btmp] = B[1+1 * LDB + o_b];
            BTMP[2 + o_btmp] = B[1+2 * LDB + o_b];
            goto LABEL40;
            // *
            // *     2 by 1:
            // *          op[TL11 TL12]*[X11] + ISGN* [X11]*TR11  = [B11]
            // *            [TL21 TL22] [X21]         [X21]         [B21]
            // *
        LABEL30:;
            SMIN = Math.Max(EPS * Math.Max(Math.Abs(TR[1+1 * LDTR + o_tr]), Math.Max(Math.Abs(TL[1+1 * LDTL + o_tl]), Math.Max(Math.Abs(TL[1+2 * LDTL + o_tl]), Math.Max(Math.Abs(TL[2+1 * LDTL + o_tl]), Math.Abs(TL[2+2 * LDTL + o_tl]))))), SMLNUM);
            TMP[1 + o_tmp] = TL[1+1 * LDTL + o_tl] + SGN * TR[1+1 * LDTR + o_tr];
            TMP[4 + o_tmp] = TL[2+2 * LDTL + o_tl] + SGN * TR[1+1 * LDTR + o_tr];
            if (LTRANL)
            {
                TMP[2 + o_tmp] = TL[1+2 * LDTL + o_tl];
                TMP[3 + o_tmp] = TL[2+1 * LDTL + o_tl];
            }
            else
            {
                TMP[2 + o_tmp] = TL[2+1 * LDTL + o_tl];
                TMP[3 + o_tmp] = TL[1+2 * LDTL + o_tl];
            }
            BTMP[1 + o_btmp] = B[1+1 * LDB + o_b];
            BTMP[2 + o_btmp] = B[2+1 * LDB + o_b];
        LABEL40:;
            // *
            // *     Solve 2 by 2 system using complete pivoting.
            // *     Set pivots less than SMIN to SMIN.
            // *
            IPIV = this._idamax.Run(4, TMP, offset_tmp, 1);
            U11 = TMP[IPIV + o_tmp];
            if (Math.Abs(U11) <= SMIN)
            {
                INFO = 1;
                U11 = SMIN;
            }
            U12 = TMP[LOCU12[IPIV + o_locu12] + o_tmp];
            L21 = TMP[LOCL21[IPIV + o_locl21] + o_tmp] / U11;
            U22 = TMP[LOCU22[IPIV + o_locu22] + o_tmp] - U12 * L21;
            XSWAP = XSWPIV[IPIV + o_xswpiv];
            BSWAP = BSWPIV[IPIV + o_bswpiv];
            if (Math.Abs(U22) <= SMIN)
            {
                INFO = 1;
                U22 = SMIN;
            }
            if (BSWAP)
            {
                TEMP = BTMP[2 + o_btmp];
                BTMP[2 + o_btmp] = BTMP[1 + o_btmp] - L21 * TEMP;
                BTMP[1 + o_btmp] = TEMP;
            }
            else
            {
                BTMP[2 + o_btmp] +=  - L21 * BTMP[1 + o_btmp];
            }
            SCALE = ONE;
            if ((TWO * SMLNUM) * Math.Abs(BTMP[2 + o_btmp]) > Math.Abs(U22) || (TWO * SMLNUM) * Math.Abs(BTMP[1 + o_btmp]) > Math.Abs(U11))
            {
                SCALE = HALF / Math.Max(Math.Abs(BTMP[1 + o_btmp]), Math.Abs(BTMP[2 + o_btmp]));
                BTMP[1 + o_btmp] *= SCALE;
                BTMP[2 + o_btmp] *= SCALE;
            }
            X2[2 + o_x2] = BTMP[2 + o_btmp] / U22;
            X2[1 + o_x2] = BTMP[1 + o_btmp] / U11 - (U12 / U11) * X2[2 + o_x2];
            if (XSWAP)
            {
                TEMP = X2[2 + o_x2];
                X2[2 + o_x2] = X2[1 + o_x2];
                X2[1 + o_x2] = TEMP;
            }
            X[1+1 * LDX + o_x] = X2[1 + o_x2];
            if (N1 == 1)
            {
                X[1+2 * LDX + o_x] = X2[2 + o_x2];
                XNORM = Math.Abs(X[1+1 * LDX + o_x]) + Math.Abs(X[1+2 * LDX + o_x]);
            }
            else
            {
                X[2+1 * LDX + o_x] = X2[2 + o_x2];
                XNORM = Math.Max(Math.Abs(X[1+1 * LDX + o_x]), Math.Abs(X[2+1 * LDX + o_x]));
            }
            return;
            // *
            // *     2 by 2:
            // *     op[TL11 TL12]*[X11 X12] +ISGN* [X11 X12]*op[TR11 TR12] = [B11 B12]
            // *       [TL21 TL22] [X21 X22]        [X21 X22]   [TR21 TR22]   [B21 B22]
            // *
            // *     Solve equivalent 4 by 4 system using complete pivoting.
            // *     Set pivots less than SMIN to SMIN.
            // *
        LABEL50:;
            SMIN = Math.Max(Math.Abs(TR[1+1 * LDTR + o_tr]), Math.Max(Math.Abs(TR[1+2 * LDTR + o_tr]), Math.Max(Math.Abs(TR[2+1 * LDTR + o_tr]), Math.Abs(TR[2+2 * LDTR + o_tr]))));
            SMIN = Math.Max(SMIN, Math.Max(Math.Abs(TL[1+1 * LDTL + o_tl]), Math.Max(Math.Abs(TL[1+2 * LDTL + o_tl]), Math.Max(Math.Abs(TL[2+1 * LDTL + o_tl]), Math.Abs(TL[2+2 * LDTL + o_tl])))));
            SMIN = Math.Max(EPS * SMIN, SMLNUM);
            BTMP[1 + o_btmp] = ZERO;
            this._dcopy.Run(16, BTMP, offset_btmp, 0, ref T16, offset_t16, 1);
            T16[1+1 * 4 + o_t16] = TL[1+1 * LDTL + o_tl] + SGN * TR[1+1 * LDTR + o_tr];
            T16[2+2 * 4 + o_t16] = TL[2+2 * LDTL + o_tl] + SGN * TR[1+1 * LDTR + o_tr];
            T16[3+3 * 4 + o_t16] = TL[1+1 * LDTL + o_tl] + SGN * TR[2+2 * LDTR + o_tr];
            T16[4+4 * 4 + o_t16] = TL[2+2 * LDTL + o_tl] + SGN * TR[2+2 * LDTR + o_tr];
            if (LTRANL)
            {
                T16[1+2 * 4 + o_t16] = TL[2+1 * LDTL + o_tl];
                T16[2+1 * 4 + o_t16] = TL[1+2 * LDTL + o_tl];
                T16[3+4 * 4 + o_t16] = TL[2+1 * LDTL + o_tl];
                T16[4+3 * 4 + o_t16] = TL[1+2 * LDTL + o_tl];
            }
            else
            {
                T16[1+2 * 4 + o_t16] = TL[1+2 * LDTL + o_tl];
                T16[2+1 * 4 + o_t16] = TL[2+1 * LDTL + o_tl];
                T16[3+4 * 4 + o_t16] = TL[1+2 * LDTL + o_tl];
                T16[4+3 * 4 + o_t16] = TL[2+1 * LDTL + o_tl];
            }
            if (LTRANR)
            {
                T16[1+3 * 4 + o_t16] = SGN * TR[1+2 * LDTR + o_tr];
                T16[2+4 * 4 + o_t16] = SGN * TR[1+2 * LDTR + o_tr];
                T16[3+1 * 4 + o_t16] = SGN * TR[2+1 * LDTR + o_tr];
                T16[4+2 * 4 + o_t16] = SGN * TR[2+1 * LDTR + o_tr];
            }
            else
            {
                T16[1+3 * 4 + o_t16] = SGN * TR[2+1 * LDTR + o_tr];
                T16[2+4 * 4 + o_t16] = SGN * TR[2+1 * LDTR + o_tr];
                T16[3+1 * 4 + o_t16] = SGN * TR[1+2 * LDTR + o_tr];
                T16[4+2 * 4 + o_t16] = SGN * TR[1+2 * LDTR + o_tr];
            }
            BTMP[1 + o_btmp] = B[1+1 * LDB + o_b];
            BTMP[2 + o_btmp] = B[2+1 * LDB + o_b];
            BTMP[3 + o_btmp] = B[1+2 * LDB + o_b];
            BTMP[4 + o_btmp] = B[2+2 * LDB + o_b];
            // *
            // *     Perform elimination
            // *
            for (I = 1; I <= 3; I++)
            {
                XMAX = ZERO;
                for (IP = I; IP <= 4; IP++)
                {
                    for (JP = I; JP <= 4; JP++)
                    {
                        if (Math.Abs(T16[IP+JP * 4 + o_t16]) >= XMAX)
                        {
                            XMAX = Math.Abs(T16[IP+JP * 4 + o_t16]);
                            IPSV = IP;
                            JPSV = JP;
                        }
                    }
                }
                if (IPSV != I)
                {
                    this._dswap.Run(4, ref T16, IPSV+1 * 4 + o_t16, 4, ref T16, I+1 * 4 + o_t16, 4);
                    TEMP = BTMP[I + o_btmp];
                    BTMP[I + o_btmp] = BTMP[IPSV + o_btmp];
                    BTMP[IPSV + o_btmp] = TEMP;
                }
                if (JPSV != I) this._dswap.Run(4, ref T16, 1+JPSV * 4 + o_t16, 1, ref T16, 1+I * 4 + o_t16, 1);
                JPIV[I + o_jpiv] = JPSV;
                if (Math.Abs(T16[I+I * 4 + o_t16]) < SMIN)
                {
                    INFO = 1;
                    T16[I+I * 4 + o_t16] = SMIN;
                }
                T16_I = I * 4 + o_t16;
                for (J = I + 1; J <= 4; J++)
                {
                    T16[J + T16_I] /= T16[I+I * 4 + o_t16];
                    BTMP[J + o_btmp] +=  - T16[J + T16_I] * BTMP[I + o_btmp];
                    for (K = I + 1; K <= 4; K++)
                    {
                        T16[J+K * 4 + o_t16] +=  - T16[J+I * 4 + o_t16] * T16[I+K * 4 + o_t16];
                    }
                }
            }
            if (Math.Abs(T16[4+4 * 4 + o_t16]) < SMIN) T16[4+4 * 4 + o_t16] = SMIN;
            SCALE = ONE;
            if ((EIGHT * SMLNUM) * Math.Abs(BTMP[1 + o_btmp]) > Math.Abs(T16[1+1 * 4 + o_t16]) || (EIGHT * SMLNUM) * Math.Abs(BTMP[2 + o_btmp]) > Math.Abs(T16[2+2 * 4 + o_t16]) || (EIGHT * SMLNUM) * Math.Abs(BTMP[3 + o_btmp]) > Math.Abs(T16[3+3 * 4 + o_t16]) || (EIGHT * SMLNUM) * Math.Abs(BTMP[4 + o_btmp]) > Math.Abs(T16[4+4 * 4 + o_t16]))
            {
                SCALE = (ONE / EIGHT) / Math.Max(Math.Abs(BTMP[1 + o_btmp]), Math.Max(Math.Abs(BTMP[2 + o_btmp]), Math.Max(Math.Abs(BTMP[3 + o_btmp]), Math.Abs(BTMP[4 + o_btmp]))));
                BTMP[1 + o_btmp] *= SCALE;
                BTMP[2 + o_btmp] *= SCALE;
                BTMP[3 + o_btmp] *= SCALE;
                BTMP[4 + o_btmp] *= SCALE;
            }
            for (I = 1; I <= 4; I++)
            {
                K = 5 - I;
                TEMP = ONE / T16[K+K * 4 + o_t16];
                TMP[K + o_tmp] = BTMP[K + o_btmp] * TEMP;
                for (J = K + 1; J <= 4; J++)
                {
                    TMP[K + o_tmp] +=  - (TEMP * T16[K+J * 4 + o_t16]) * TMP[J + o_tmp];
                }
            }
            for (I = 1; I <= 3; I++)
            {
                if (JPIV[4 - I + o_jpiv] != 4 - I)
                {
                    TEMP = TMP[4 - I + o_tmp];
                    TMP[4 - I + o_tmp] = TMP[JPIV[4 - I + o_jpiv] + o_tmp];
                    TMP[JPIV[4 - I + o_jpiv] + o_tmp] = TEMP;
                }
            }
            X[1+1 * LDX + o_x] = TMP[1 + o_tmp];
            X[2+1 * LDX + o_x] = TMP[2 + o_tmp];
            X[1+2 * LDX + o_x] = TMP[3 + o_tmp];
            X[2+2 * LDX + o_x] = TMP[4 + o_tmp];
            XNORM = Math.Max(Math.Abs(TMP[1 + o_tmp]) + Math.Abs(TMP[3 + o_tmp]), Math.Abs(TMP[2 + o_tmp]) + Math.Abs(TMP[4 + o_tmp]));
            return;
            // *
            // *     End of DLASY2
            // *

            #endregion

        }
    }
}
