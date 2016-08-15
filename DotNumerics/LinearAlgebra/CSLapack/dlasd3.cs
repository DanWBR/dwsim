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
    /// DLASD3 finds all the square roots of the roots of the secular
    /// equation, as defined by the values in D and Z.  It makes the
    /// appropriate calls to DLASD4 and then updates the singular
    /// vectors by matrix multiplication.
    /// 
    /// This code makes very mild assumptions about floating point
    /// arithmetic. It will work on machines with a guard digit in
    /// add/subtract, or on those binary machines without guard digits
    /// which subtract like the Cray XMP, Cray YMP, Cray C 90, or Cray 2.
    /// It could conceivably fail on hexadecimal or decimal machines
    /// without guard digits, but we know of none.
    /// 
    /// DLASD3 is called from DLASD1.
    /// 
    ///</summary>
    public class DLASD3
    {
    

        #region Dependencies
        
        DLAMC3 _dlamc3; DNRM2 _dnrm2; DCOPY _dcopy; DGEMM _dgemm; DLACPY _dlacpy; DLASCL _dlascl; DLASD4 _dlasd4; XERBLA _xerbla; 

        #endregion


        #region Variables
        
        const double ONE = 1.0E+0; const double ZERO = 0.0E+0; const double NEGONE =  - 1.0E+0; 

        #endregion

        public DLASD3(DLAMC3 dlamc3, DNRM2 dnrm2, DCOPY dcopy, DGEMM dgemm, DLACPY dlacpy, DLASCL dlascl, DLASD4 dlasd4, XERBLA xerbla)
        {
    

            #region Set Dependencies
            
            this._dlamc3 = dlamc3; this._dnrm2 = dnrm2; this._dcopy = dcopy; this._dgemm = dgemm; this._dlacpy = dlacpy; 
            this._dlascl = dlascl;this._dlasd4 = dlasd4; this._xerbla = xerbla; 

            #endregion

        }
    
        public DLASD3()
        {
    

            #region Dependencies (Initialization)
            
            DLAMC3 dlamc3 = new DLAMC3();
            DNRM2 dnrm2 = new DNRM2();
            DCOPY dcopy = new DCOPY();
            LSAME lsame = new LSAME();
            XERBLA xerbla = new XERBLA();
            DLASD5 dlasd5 = new DLASD5();
            DGEMM dgemm = new DGEMM(lsame, xerbla);
            DLACPY dlacpy = new DLACPY(lsame);
            DLAMC1 dlamc1 = new DLAMC1(dlamc3);
            DLAMC4 dlamc4 = new DLAMC4(dlamc3);
            DLAMC5 dlamc5 = new DLAMC5(dlamc3);
            DLAMC2 dlamc2 = new DLAMC2(dlamc3, dlamc1, dlamc4, dlamc5);
            DLAMCH dlamch = new DLAMCH(lsame, dlamc2);
            DLASCL dlascl = new DLASCL(lsame, dlamch, xerbla);
            DLAED6 dlaed6 = new DLAED6(dlamch);
            DLASD4 dlasd4 = new DLASD4(dlaed6, dlasd5, dlamch);

            #endregion


            #region Set Dependencies
            
            this._dlamc3 = dlamc3; this._dnrm2 = dnrm2; this._dcopy = dcopy; this._dgemm = dgemm; this._dlacpy = dlacpy; 
            this._dlascl = dlascl;this._dlasd4 = dlasd4; this._xerbla = xerbla; 

            #endregion

        }
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DLASD3 finds all the square roots of the roots of the secular
        /// equation, as defined by the values in D and Z.  It makes the
        /// appropriate calls to DLASD4 and then updates the singular
        /// vectors by matrix multiplication.
        /// 
        /// This code makes very mild assumptions about floating point
        /// arithmetic. It will work on machines with a guard digit in
        /// add/subtract, or on those binary machines without guard digits
        /// which subtract like the Cray XMP, Cray YMP, Cray C 90, or Cray 2.
        /// It could conceivably fail on hexadecimal or decimal machines
        /// without guard digits, but we know of none.
        /// 
        /// DLASD3 is called from DLASD1.
        /// 
        ///</summary>
        /// <param name="NL">
        /// (input) INTEGER
        /// The row dimension of the upper block.  NL .GE. 1.
        ///</param>
        /// <param name="NR">
        /// (input) INTEGER
        /// The row dimension of the lower block.  NR .GE. 1.
        ///</param>
        /// <param name="SQRE">
        /// (input) INTEGER
        /// = 0: the lower block is an NR-by-NR square matrix.
        /// = 1: the lower block is an NR-by-(NR+1) rectangular matrix.
        /// 
        /// The bidiagonal matrix has N = NL + NR + 1 rows and
        /// M = N + SQRE .GE. N columns.
        ///</param>
        /// <param name="K">
        /// (input) INTEGER
        /// The size of the secular equation, 1 =.LT. K = .LT. N.
        ///</param>
        /// <param name="D">
        /// (output) DOUBLE PRECISION array, dimension(K)
        /// On exit the square roots of the roots of the secular equation,
        /// in ascending order.
        ///</param>
        /// <param name="Q">
        /// (workspace) DOUBLE PRECISION array,
        /// dimension at least (LDQ,K).
        ///</param>
        /// <param name="LDQ">
        /// (input) INTEGER
        /// The leading dimension of the array Q.  LDQ .GE. K.
        ///</param>
        /// <param name="DSIGMA">
        /// (input) DOUBLE PRECISION array, dimension(K)
        /// The first K elements of this array contain the old roots
        /// of the deflated updating problem.  These are the poles
        /// of the secular equation.
        ///</param>
        /// <param name="U">
        /// (output) DOUBLE PRECISION array, dimension (LDU, N)
        /// The last N - K columns of this matrix contain the deflated
        /// left singular vectors.
        ///</param>
        /// <param name="LDU">
        /// (input) INTEGER
        /// The leading dimension of the array U.  LDU .GE. N.
        ///</param>
        /// <param name="U2">
        /// (input/output) DOUBLE PRECISION array, dimension (LDU2, N)
        /// The first K columns of this matrix contain the non-deflated
        /// left singular vectors for the split problem.
        ///</param>
        /// <param name="LDU2">
        /// (input) INTEGER
        /// The leading dimension of the array U2.  LDU2 .GE. N.
        ///</param>
        /// <param name="VT">
        /// (output) DOUBLE PRECISION array, dimension (LDVT, M)
        /// The last M - K columns of VT' contain the deflated
        /// right singular vectors.
        ///</param>
        /// <param name="LDVT">
        /// (input) INTEGER
        /// The leading dimension of the array VT.  LDVT .GE. N.
        ///</param>
        /// <param name="VT2">
        /// (input/output) DOUBLE PRECISION array, dimension (LDVT2, N)
        /// The first K columns of VT2' contain the non-deflated
        /// right singular vectors for the split problem.
        ///</param>
        /// <param name="LDVT2">
        /// (input) INTEGER
        /// The leading dimension of the array VT2.  LDVT2 .GE. N.
        ///</param>
        /// <param name="IDXC">
        /// (input) INTEGER array, dimension ( N )
        /// The permutation used to arrange the columns of U (and rows of
        /// VT) into three groups:  the first group contains non-zero
        /// entries only at and above (or before) NL +1; the second
        /// contains non-zero entries only at and below (or after) NL+2;
        /// and the third is dense. The first column of U and the row of
        /// VT are treated separately, however.
        /// 
        /// The rows of the singular vectors found by DLASD4
        /// must be likewise permuted before the matrix multiplies can
        /// take place.
        ///</param>
        /// <param name="CTOT">
        /// (input) INTEGER array, dimension ( 4 )
        /// A count of the total number of the various types of columns
        /// in U (or rows in VT), as described in IDXC. The fourth column
        /// type is any column which has been deflated.
        ///</param>
        /// <param name="Z">
        /// (input) DOUBLE PRECISION array, dimension (K)
        /// The first K elements of this array contain the components
        /// of the deflation-adjusted updating row vector.
        ///</param>
        /// <param name="INFO">
        /// (output) INTEGER
        /// = 0:  successful exit.
        /// .LT. 0:  if INFO = -i, the i-th argument had an illegal value.
        /// .GT. 0:  if INFO = 1, an singular value did not converge
        ///</param>
        public void Run(int NL, int NR, int SQRE, int K, ref double[] D, int offset_d, ref double[] Q, int offset_q
                         , int LDQ, ref double[] DSIGMA, int offset_dsigma, ref double[] U, int offset_u, int LDU, double[] U2, int offset_u2, int LDU2
                         , ref double[] VT, int offset_vt, int LDVT, ref double[] VT2, int offset_vt2, int LDVT2, int[] IDXC, int offset_idxc, int[] CTOT, int offset_ctot
                         , ref double[] Z, int offset_z, ref int INFO)
        {

            #region Variables
            
            int CTEMP = 0; int I = 0; int J = 0; int JC = 0; int KTEMP = 0; int M = 0; int N = 0; int NLP1 = 0; int NLP2 = 0; 
            int NRP1 = 0;double RHO = 0; double TEMP = 0; 

            #endregion


            #region Implicit Variables
            
            int U_1 = 0; int U2_1 = 0; int U_K = 0; int VT_K = 0; int VT_I = 0; int U_I = 0; int Q_I = 0; int Q_1 = 0; 
            int Q_KTEMP = 0;

            #endregion


            #region Array Index Correction
            
             int o_d = -1 + offset_d;  int o_q = -1 - LDQ + offset_q;  int o_dsigma = -1 + offset_dsigma; 
             int o_u = -1 - LDU + offset_u; int o_u2 = -1 - LDU2 + offset_u2;  int o_vt = -1 - LDVT + offset_vt; 
             int o_vt2 = -1 - LDVT2 + offset_vt2; int o_idxc = -1 + offset_idxc;  int o_ctot = -1 + offset_ctot; 
             int o_z = -1 + offset_z;

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
            // *  DLASD3 finds all the square roots of the roots of the secular
            // *  equation, as defined by the values in D and Z.  It makes the
            // *  appropriate calls to DLASD4 and then updates the singular
            // *  vectors by matrix multiplication.
            // *
            // *  This code makes very mild assumptions about floating point
            // *  arithmetic. It will work on machines with a guard digit in
            // *  add/subtract, or on those binary machines without guard digits
            // *  which subtract like the Cray XMP, Cray YMP, Cray C 90, or Cray 2.
            // *  It could conceivably fail on hexadecimal or decimal machines
            // *  without guard digits, but we know of none.
            // *
            // *  DLASD3 is called from DLASD1.
            // *
            // *  Arguments
            // *  =========
            // *
            // *  NL     (input) INTEGER
            // *         The row dimension of the upper block.  NL >= 1.
            // *
            // *  NR     (input) INTEGER
            // *         The row dimension of the lower block.  NR >= 1.
            // *
            // *  SQRE   (input) INTEGER
            // *         = 0: the lower block is an NR-by-NR square matrix.
            // *         = 1: the lower block is an NR-by-(NR+1) rectangular matrix.
            // *
            // *         The bidiagonal matrix has N = NL + NR + 1 rows and
            // *         M = N + SQRE >= N columns.
            // *
            // *  K      (input) INTEGER
            // *         The size of the secular equation, 1 =< K = < N.
            // *
            // *  D      (output) DOUBLE PRECISION array, dimension(K)
            // *         On exit the square roots of the roots of the secular equation,
            // *         in ascending order.
            // *
            // *  Q      (workspace) DOUBLE PRECISION array,
            // *                     dimension at least (LDQ,K).
            // *
            // *  LDQ    (input) INTEGER
            // *         The leading dimension of the array Q.  LDQ >= K.
            // *
            // *  DSIGMA (input) DOUBLE PRECISION array, dimension(K)
            // *         The first K elements of this array contain the old roots
            // *         of the deflated updating problem.  These are the poles
            // *         of the secular equation.
            // *
            // *  U      (output) DOUBLE PRECISION array, dimension (LDU, N)
            // *         The last N - K columns of this matrix contain the deflated
            // *         left singular vectors.
            // *
            // *  LDU    (input) INTEGER
            // *         The leading dimension of the array U.  LDU >= N.
            // *
            // *  U2     (input/output) DOUBLE PRECISION array, dimension (LDU2, N)
            // *         The first K columns of this matrix contain the non-deflated
            // *         left singular vectors for the split problem.
            // *
            // *  LDU2   (input) INTEGER
            // *         The leading dimension of the array U2.  LDU2 >= N.
            // *
            // *  VT     (output) DOUBLE PRECISION array, dimension (LDVT, M)
            // *         The last M - K columns of VT' contain the deflated
            // *         right singular vectors.
            // *
            // *  LDVT   (input) INTEGER
            // *         The leading dimension of the array VT.  LDVT >= N.
            // *
            // *  VT2    (input/output) DOUBLE PRECISION array, dimension (LDVT2, N)
            // *         The first K columns of VT2' contain the non-deflated
            // *         right singular vectors for the split problem.
            // *
            // *  LDVT2  (input) INTEGER
            // *         The leading dimension of the array VT2.  LDVT2 >= N.
            // *
            // *  IDXC   (input) INTEGER array, dimension ( N )
            // *         The permutation used to arrange the columns of U (and rows of
            // *         VT) into three groups:  the first group contains non-zero
            // *         entries only at and above (or before) NL +1; the second
            // *         contains non-zero entries only at and below (or after) NL+2;
            // *         and the third is dense. The first column of U and the row of
            // *         VT are treated separately, however.
            // *
            // *         The rows of the singular vectors found by DLASD4
            // *         must be likewise permuted before the matrix multiplies can
            // *         take place.
            // *
            // *  CTOT   (input) INTEGER array, dimension ( 4 )
            // *         A count of the total number of the various types of columns
            // *         in U (or rows in VT), as described in IDXC. The fourth column
            // *         type is any column which has been deflated.
            // *
            // *  Z      (input) DOUBLE PRECISION array, dimension (K)
            // *         The first K elements of this array contain the components
            // *         of the deflation-adjusted updating row vector.
            // *
            // *  INFO   (output) INTEGER
            // *         = 0:  successful exit.
            // *         < 0:  if INFO = -i, the i-th argument had an illegal value.
            // *         > 0:  if INFO = 1, an singular value did not converge
            // *
            // *  Further Details
            // *  ===============
            // *
            // *  Based on contributions by
            // *     Ming Gu and Huan Ren, Computer Science Division, University of
            // *     California at Berkeley, USA
            // *
            // *  =====================================================================
            // *
            // *     .. Parameters ..
            // *     ..
            // *     .. Local Scalars ..
            // *     ..
            // *     .. External Functions ..
            // *     ..
            // *     .. External Subroutines ..
            // *     ..
            // *     .. Intrinsic Functions ..
            //      INTRINSIC          ABS, SIGN, SQRT;
            // *     ..
            // *     .. Executable Statements ..
            // *
            // *     Test the input parameters.
            // *

            #endregion


            #region Body
            
            INFO = 0;
            // *
            if (NL < 1)
            {
                INFO =  - 1;
            }
            else
            {
                if (NR < 1)
                {
                    INFO =  - 2;
                }
                else
                {
                    if ((SQRE != 1) && (SQRE != 0))
                    {
                        INFO =  - 3;
                    }
                }
            }
            // *
            N = NL + NR + 1;
            M = N + SQRE;
            NLP1 = NL + 1;
            NLP2 = NL + 2;
            // *
            if ((K < 1) || (K > N))
            {
                INFO =  - 4;
            }
            else
            {
                if (LDQ < K)
                {
                    INFO =  - 7;
                }
                else
                {
                    if (LDU < N)
                    {
                        INFO =  - 10;
                    }
                    else
                    {
                        if (LDU2 < N)
                        {
                            INFO =  - 12;
                        }
                        else
                        {
                            if (LDVT < M)
                            {
                                INFO =  - 14;
                            }
                            else
                            {
                                if (LDVT2 < M)
                                {
                                    INFO =  - 16;
                                }
                            }
                        }
                    }
                }
            }
            if (INFO != 0)
            {
                this._xerbla.Run("DLASD3",  - INFO);
                return;
            }
            // *
            // *     Quick return if possible
            // *
            if (K == 1)
            {
                D[1 + o_d] = Math.Abs(Z[1 + o_z]);
                this._dcopy.Run(M, VT2, 1+1 * LDVT2 + o_vt2, LDVT2, ref VT, 1+1 * LDVT + o_vt, LDVT);
                if (Z[1 + o_z] > ZERO)
                {
                    this._dcopy.Run(N, U2, 1+1 * LDU2 + o_u2, 1, ref U, 1+1 * LDU + o_u, 1);
                }
                else
                {
                    U_1 = 1 * LDU + o_u;
                    U2_1 = 1 * LDU2 + o_u2;
                    for (I = 1; I <= N; I++)
                    {
                        U[I + U_1] =  - U2[I + U2_1];
                    }
                }
                return;
            }
            // *
            // *     Modify values DSIGMA(i) to make sure all DSIGMA(i)-DSIGMA(j) can
            // *     be computed with high relative accuracy (barring over/underflow).
            // *     This is a problem on machines without a guard digit in
            // *     add/subtract (Cray XMP, Cray YMP, Cray C 90 and Cray 2).
            // *     The following code replaces DSIGMA(I) by 2*DSIGMA(I)-DSIGMA(I),
            // *     which on any of these machines zeros out the bottommost
            // *     bit of DSIGMA(I) if it is 1; this makes the subsequent
            // *     subtractions DSIGMA(I)-DSIGMA(J) unproblematic when cancellation
            // *     occurs. On binary machines with a guard digit (almost all
            // *     machines) it does not change DSIGMA(I) at all. On hexadecimal
            // *     and decimal machines with a guard digit, it slightly
            // *     changes the bottommost bits of DSIGMA(I). It does not account
            // *     for hexadecimal or decimal machines without guard digits
            // *     (we know of none). We use a subroutine call to compute
            // *     2*DSIGMA(I) to prevent optimizing compilers from eliminating
            // *     this code.
            // *
            for (I = 1; I <= K; I++)
            {
                DSIGMA[I + o_dsigma] = this._dlamc3.Run(DSIGMA[I + o_dsigma], DSIGMA[I + o_dsigma]) - DSIGMA[I + o_dsigma];
            }
            // *
            // *     Keep a copy of Z.
            // *
            this._dcopy.Run(K, Z, offset_z, 1, ref Q, offset_q, 1);
            // *
            // *     Normalize Z.
            // *
            RHO = this._dnrm2.Run(K, Z, offset_z, 1);
            this._dlascl.Run("G", 0, 0, RHO, ONE, K
                             , 1, ref Z, offset_z, K, ref INFO);
            RHO *= RHO;
            // *
            // *     Find the new singular values.
            // *
            for (J = 1; J <= K; J++)
            {
                this._dlasd4.Run(K, J, DSIGMA, offset_dsigma, Z, offset_z, ref U, 1+J * LDU + o_u, RHO
                                 , ref D[J + o_d], ref VT, 1+J * LDVT + o_vt, ref INFO);
                // *
                // *        If the zero finder fails, the computation is terminated.
                // *
                if (INFO != 0)
                {
                    return;
                }
            }
            // *
            // *     Compute updated Z.
            // *
            U_K = K * LDU + o_u;
            VT_K = K * LDVT + o_vt;
            for (I = 1; I <= K; I++)
            {
                Z[I + o_z] = U[I + U_K] * VT[I + VT_K];
                for (J = 1; J <= I - 1; J++)
                {
                    Z[I + o_z] = Z[I + o_z] * (U[I+J * LDU + o_u] * VT[I+J * LDVT + o_vt] / (DSIGMA[I + o_dsigma] - DSIGMA[J + o_dsigma]) / (DSIGMA[I + o_dsigma] + DSIGMA[J + o_dsigma]));
                }
                for (J = I; J <= K - 1; J++)
                {
                    Z[I + o_z] = Z[I + o_z] * (U[I+J * LDU + o_u] * VT[I+J * LDVT + o_vt] / (DSIGMA[I + o_dsigma] - DSIGMA[J + 1 + o_dsigma]) / (DSIGMA[I + o_dsigma] + DSIGMA[J + 1 + o_dsigma]));
                }
                Z[I + o_z] = FortranLib.Sign(Math.Sqrt(Math.Abs(Z[I + o_z])),Q[I+1 * LDQ + o_q]);
            }
            // *
            // *     Compute left singular vectors of the modified diagonal matrix,
            // *     and store related information for the right singular vectors.
            // *
            for (I = 1; I <= K; I++)
            {
                VT[1+I * LDVT + o_vt] = Z[1 + o_z] / U[1+I * LDU + o_u] / VT[1+I * LDVT + o_vt];
                U[1+I * LDU + o_u] = NEGONE;
                VT_I = I * LDVT + o_vt;
                U_I = I * LDU + o_u;
                for (J = 2; J <= K; J++)
                {
                    VT[J + VT_I] = Z[J + o_z] / U[J + U_I] / VT[J + VT_I];
                    U[J + U_I] = DSIGMA[J + o_dsigma] * VT[J + VT_I];
                }
                TEMP = this._dnrm2.Run(K, U, 1+I * LDU + o_u, 1);
                Q[1+I * LDQ + o_q] = U[1+I * LDU + o_u] / TEMP;
                Q_I = I * LDQ + o_q;
                for (J = 2; J <= K; J++)
                {
                    JC = IDXC[J + o_idxc];
                    Q[J + Q_I] = U[JC+I * LDU + o_u] / TEMP;
                }
            }
            // *
            // *     Update the left singular vector matrix.
            // *
            if (K == 2)
            {
                this._dgemm.Run("N", "N", N, K, K, ONE
                                , U2, offset_u2, LDU2, Q, offset_q, LDQ, ZERO, ref U, offset_u
                                , LDU);
                goto LABEL100;
            }
            if (CTOT[1 + o_ctot] > 0)
            {
                this._dgemm.Run("N", "N", NL, K, CTOT[1 + o_ctot], ONE
                                , U2, 1+2 * LDU2 + o_u2, LDU2, Q, 2+1 * LDQ + o_q, LDQ, ZERO, ref U, 1+1 * LDU + o_u
                                , LDU);
                if (CTOT[3 + o_ctot] > 0)
                {
                    KTEMP = 2 + CTOT[1 + o_ctot] + CTOT[2 + o_ctot];
                    this._dgemm.Run("N", "N", NL, K, CTOT[3 + o_ctot], ONE
                                    , U2, 1+KTEMP * LDU2 + o_u2, LDU2, Q, KTEMP+1 * LDQ + o_q, LDQ, ONE, ref U, 1+1 * LDU + o_u
                                    , LDU);
                }
            }
            else
            {
                if (CTOT[3 + o_ctot] > 0)
                {
                    KTEMP = 2 + CTOT[1 + o_ctot] + CTOT[2 + o_ctot];
                    this._dgemm.Run("N", "N", NL, K, CTOT[3 + o_ctot], ONE
                                    , U2, 1+KTEMP * LDU2 + o_u2, LDU2, Q, KTEMP+1 * LDQ + o_q, LDQ, ZERO, ref U, 1+1 * LDU + o_u
                                    , LDU);
                }
                else
                {
                    this._dlacpy.Run("F", NL, K, U2, offset_u2, LDU2, ref U, offset_u
                                     , LDU);
                }
            }
            this._dcopy.Run(K, Q, 1+1 * LDQ + o_q, LDQ, ref U, NLP1+1 * LDU + o_u, LDU);
            KTEMP = 2 + CTOT[1 + o_ctot];
            CTEMP = CTOT[2 + o_ctot] + CTOT[3 + o_ctot];
            this._dgemm.Run("N", "N", NR, K, CTEMP, ONE
                            , U2, NLP2+KTEMP * LDU2 + o_u2, LDU2, Q, KTEMP+1 * LDQ + o_q, LDQ, ZERO, ref U, NLP2+1 * LDU + o_u
                            , LDU);
            // *
            // *     Generate the right singular vectors.
            // *
        LABEL100:;
            Q_1 = 1 * LDQ + o_q;
            for (I = 1; I <= K; I++)
            {
                TEMP = this._dnrm2.Run(K, VT, 1+I * LDVT + o_vt, 1);
                Q[I + Q_1] = VT[1+I * LDVT + o_vt] / TEMP;
                for (J = 2; J <= K; J++)
                {
                    JC = IDXC[J + o_idxc];
                    Q[I+J * LDQ + o_q] = VT[JC+I * LDVT + o_vt] / TEMP;
                }
            }
            // *
            // *     Update the right singular vector matrix.
            // *
            if (K == 2)
            {
                this._dgemm.Run("N", "N", K, M, K, ONE
                                , Q, offset_q, LDQ, VT2, offset_vt2, LDVT2, ZERO, ref VT, offset_vt
                                , LDVT);
                return;
            }
            KTEMP = 1 + CTOT[1 + o_ctot];
            this._dgemm.Run("N", "N", K, NLP1, KTEMP, ONE
                            , Q, 1+1 * LDQ + o_q, LDQ, VT2, 1+1 * LDVT2 + o_vt2, LDVT2, ZERO, ref VT, 1+1 * LDVT + o_vt
                            , LDVT);
            KTEMP = 2 + CTOT[1 + o_ctot] + CTOT[2 + o_ctot];
            if (KTEMP <= LDVT2)
            {
                this._dgemm.Run("N", "N", K, NLP1, CTOT[3 + o_ctot], ONE
                                , Q, 1+KTEMP * LDQ + o_q, LDQ, VT2, KTEMP+1 * LDVT2 + o_vt2, LDVT2, ONE, ref VT, 1+1 * LDVT + o_vt
                                , LDVT);
            }
            // *
            KTEMP = CTOT[1 + o_ctot] + 1;
            NRP1 = NR + SQRE;
            if (KTEMP > 1)
            {
                Q_KTEMP = KTEMP * LDQ + o_q;
                Q_1 = 1 * LDQ + o_q;
                for (I = 1; I <= K; I++)
                {
                    Q[I + Q_KTEMP] = Q[I + Q_1];
                }
                for (I = NLP2; I <= M; I++)
                {
                    VT2[KTEMP+I * LDVT2 + o_vt2] = VT2[1+I * LDVT2 + o_vt2];
                }
            }
            CTEMP = 1 + CTOT[2 + o_ctot] + CTOT[3 + o_ctot];
            this._dgemm.Run("N", "N", K, NRP1, CTEMP, ONE
                            , Q, 1+KTEMP * LDQ + o_q, LDQ, VT2, KTEMP+NLP2 * LDVT2 + o_vt2, LDVT2, ZERO, ref VT, 1+NLP2 * LDVT + o_vt
                            , LDVT);
            // *
            return;
            // *
            // *     End of DLASD3
            // *

            #endregion

        }
    }
}
