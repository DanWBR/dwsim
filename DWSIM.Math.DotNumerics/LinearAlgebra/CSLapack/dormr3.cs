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
    /// -- LAPACK routine (version 3.1) --
    /// Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
    /// November 2006
    /// Purpose
    /// =======
    /// 
    /// DORMR3 overwrites the general real m by n matrix C with
    /// 
    /// Q * C  if SIDE = 'L' and TRANS = 'N', or
    /// 
    /// Q'* C  if SIDE = 'L' and TRANS = 'T', or
    /// 
    /// C * Q  if SIDE = 'R' and TRANS = 'N', or
    /// 
    /// C * Q' if SIDE = 'R' and TRANS = 'T',
    /// 
    /// where Q is a real orthogonal matrix defined as the product of k
    /// elementary reflectors
    /// 
    /// Q = H(1) H(2) . . . H(k)
    /// 
    /// as returned by DTZRZF. Q is of order m if SIDE = 'L' and of order n
    /// if SIDE = 'R'.
    /// 
    ///</summary>
    public class DORMR3
    {
    

        #region Dependencies
        
        LSAME _lsame; DLARZ _dlarz; XERBLA _xerbla; 

        #endregion

        public DORMR3(LSAME lsame, DLARZ dlarz, XERBLA xerbla)
        {
    

            #region Set Dependencies
            
            this._lsame = lsame; this._dlarz = dlarz; this._xerbla = xerbla; 

            #endregion

        }
    
        public DORMR3()
        {
    

            #region Dependencies (Initialization)
            
            LSAME lsame = new LSAME();
            DAXPY daxpy = new DAXPY();
            DCOPY dcopy = new DCOPY();
            XERBLA xerbla = new XERBLA();
            DGEMV dgemv = new DGEMV(lsame, xerbla);
            DGER dger = new DGER(xerbla);
            DLARZ dlarz = new DLARZ(daxpy, dcopy, dgemv, dger, lsame);

            #endregion


            #region Set Dependencies
            
            this._lsame = lsame; this._dlarz = dlarz; this._xerbla = xerbla; 

            #endregion

        }
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DORMR3 overwrites the general real m by n matrix C with
        /// 
        /// Q * C  if SIDE = 'L' and TRANS = 'N', or
        /// 
        /// Q'* C  if SIDE = 'L' and TRANS = 'T', or
        /// 
        /// C * Q  if SIDE = 'R' and TRANS = 'N', or
        /// 
        /// C * Q' if SIDE = 'R' and TRANS = 'T',
        /// 
        /// where Q is a real orthogonal matrix defined as the product of k
        /// elementary reflectors
        /// 
        /// Q = H(1) H(2) . . . H(k)
        /// 
        /// as returned by DTZRZF. Q is of order m if SIDE = 'L' and of order n
        /// if SIDE = 'R'.
        /// 
        ///</summary>
        /// <param name="SIDE">
        /// (input) CHARACTER*1
        /// = 'L': apply Q or Q' from the Left
        /// = 'R': apply Q or Q' from the Right
        ///</param>
        /// <param name="TRANS">
        /// (input) CHARACTER*1
        /// = 'N': apply Q  (No transpose)
        /// = 'T': apply Q' (Transpose)
        ///</param>
        /// <param name="M">
        /// (input) INTEGER
        /// The number of rows of the matrix C. M .GE. 0.
        ///</param>
        /// <param name="N">
        /// (input) INTEGER
        /// The number of columns of the matrix C. N .GE. 0.
        ///</param>
        /// <param name="K">
        /// (input) INTEGER
        /// The number of elementary reflectors whose product defines
        /// the matrix Q.
        /// If SIDE = 'L', M .GE. K .GE. 0;
        /// if SIDE = 'R', N .GE. K .GE. 0.
        ///</param>
        /// <param name="L">
        /// (input) INTEGER
        /// The number of columns of the matrix A containing
        /// the meaningful part of the Householder reflectors.
        /// If SIDE = 'L', M .GE. L .GE. 0, if SIDE = 'R', N .GE. L .GE. 0.
        ///</param>
        /// <param name="A">
        /// (input) DOUBLE PRECISION array, dimension
        /// (LDA,M) if SIDE = 'L',
        /// (LDA,N) if SIDE = 'R'
        /// The i-th row must contain the vector which defines the
        /// elementary reflector H(i), for i = 1,2,...,k, as returned by
        /// DTZRZF in the last k rows of its array argument A.
        /// A is modified by the routine but restored on exit.
        ///</param>
        /// <param name="LDA">
        /// (input) INTEGER
        /// The leading dimension of the array A. LDA .GE. max(1,K).
        ///</param>
        /// <param name="TAU">
        /// (input) DOUBLE PRECISION array, dimension (K)
        /// TAU(i) must contain the scalar factor of the elementary
        /// reflector H(i), as returned by DTZRZF.
        ///</param>
        /// <param name="C">
        /// * Q  if SIDE = 'R' and TRANS = 'N', or
        ///</param>
        /// <param name="LDC">
        /// (input) INTEGER
        /// The leading dimension of the array C. LDC .GE. max(1,M).
        ///</param>
        /// <param name="WORK">
        /// (workspace) DOUBLE PRECISION array, dimension
        /// (N) if SIDE = 'L',
        /// (M) if SIDE = 'R'
        ///</param>
        /// <param name="INFO">
        /// (output) INTEGER
        /// = 0: successful exit
        /// .LT. 0: if INFO = -i, the i-th argument had an illegal value
        ///</param>
        public void Run(string SIDE, string TRANS, int M, int N, int K, int L
                         , double[] A, int offset_a, int LDA, double[] TAU, int offset_tau, ref double[] C, int offset_c, int LDC, ref double[] WORK, int offset_work
                         , ref int INFO)
        {

            #region Variables
            
            bool LEFT = false; bool NOTRAN = false; int I = 0; int I1 = 0; int I2 = 0; int I3 = 0; int IC = 0; int JA = 0; 
            int JC = 0;int MI = 0; int NI = 0; int NQ = 0; 

            #endregion


            #region Array Index Correction
            
             int o_a = -1 - LDA + offset_a;  int o_tau = -1 + offset_tau;  int o_c = -1 - LDC + offset_c; 
             int o_work = -1 + offset_work;

            #endregion


            #region Strings
            
            SIDE = SIDE.Substring(0, 1);  TRANS = TRANS.Substring(0, 1);  

            #endregion


            #region Prolog
            
            // *
            // *  -- LAPACK routine (version 3.1) --
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
            // *  DORMR3 overwrites the general real m by n matrix C with
            // *
            // *        Q * C  if SIDE = 'L' and TRANS = 'N', or
            // *
            // *        Q'* C  if SIDE = 'L' and TRANS = 'T', or
            // *
            // *        C * Q  if SIDE = 'R' and TRANS = 'N', or
            // *
            // *        C * Q' if SIDE = 'R' and TRANS = 'T',
            // *
            // *  where Q is a real orthogonal matrix defined as the product of k
            // *  elementary reflectors
            // *
            // *        Q = H(1) H(2) . . . H(k)
            // *
            // *  as returned by DTZRZF. Q is of order m if SIDE = 'L' and of order n
            // *  if SIDE = 'R'.
            // *
            // *  Arguments
            // *  =========
            // *
            // *  SIDE    (input) CHARACTER*1
            // *          = 'L': apply Q or Q' from the Left
            // *          = 'R': apply Q or Q' from the Right
            // *
            // *  TRANS   (input) CHARACTER*1
            // *          = 'N': apply Q  (No transpose)
            // *          = 'T': apply Q' (Transpose)
            // *
            // *  M       (input) INTEGER
            // *          The number of rows of the matrix C. M >= 0.
            // *
            // *  N       (input) INTEGER
            // *          The number of columns of the matrix C. N >= 0.
            // *
            // *  K       (input) INTEGER
            // *          The number of elementary reflectors whose product defines
            // *          the matrix Q.
            // *          If SIDE = 'L', M >= K >= 0;
            // *          if SIDE = 'R', N >= K >= 0.
            // *
            // *  L       (input) INTEGER
            // *          The number of columns of the matrix A containing
            // *          the meaningful part of the Householder reflectors.
            // *          If SIDE = 'L', M >= L >= 0, if SIDE = 'R', N >= L >= 0.
            // *
            // *  A       (input) DOUBLE PRECISION array, dimension
            // *                               (LDA,M) if SIDE = 'L',
            // *                               (LDA,N) if SIDE = 'R'
            // *          The i-th row must contain the vector which defines the
            // *          elementary reflector H(i), for i = 1,2,...,k, as returned by
            // *          DTZRZF in the last k rows of its array argument A.
            // *          A is modified by the routine but restored on exit.
            // *
            // *  LDA     (input) INTEGER
            // *          The leading dimension of the array A. LDA >= max(1,K).
            // *
            // *  TAU     (input) DOUBLE PRECISION array, dimension (K)
            // *          TAU(i) must contain the scalar factor of the elementary
            // *          reflector H(i), as returned by DTZRZF.
            // *
            // *  C       (input/output) DOUBLE PRECISION array, dimension (LDC,N)
            // *          On entry, the m-by-n matrix C.
            // *          On exit, C is overwritten by Q*C or Q'*C or C*Q' or C*Q.
            // *
            // *  LDC     (input) INTEGER
            // *          The leading dimension of the array C. LDC >= max(1,M).
            // *
            // *  WORK    (workspace) DOUBLE PRECISION array, dimension
            // *                                   (N) if SIDE = 'L',
            // *                                   (M) if SIDE = 'R'
            // *
            // *  INFO    (output) INTEGER
            // *          = 0: successful exit
            // *          < 0: if INFO = -i, the i-th argument had an illegal value
            // *
            // *  Further Details
            // *  ===============
            // *
            // *  Based on contributions by
            // *    A. Petitet, Computer Science Dept., Univ. of Tenn., Knoxville, USA
            // *
            // *  =====================================================================
            // *
            // *     .. Local Scalars ..
            // *     ..
            // *     .. External Functions ..
            // *     ..
            // *     .. External Subroutines ..
            // *     ..
            // *     .. Intrinsic Functions ..
            //      INTRINSIC          MAX;
            // *     ..
            // *     .. Executable Statements ..
            // *
            // *     Test the input arguments
            // *

            #endregion


            #region Body
            
            INFO = 0;
            LEFT = this._lsame.Run(SIDE, "L");
            NOTRAN = this._lsame.Run(TRANS, "N");
            // *
            // *     NQ is the order of Q
            // *
            if (LEFT)
            {
                NQ = M;
            }
            else
            {
                NQ = N;
            }
            if (!LEFT && !this._lsame.Run(SIDE, "R"))
            {
                INFO =  - 1;
            }
            else
            {
                if (!NOTRAN && !this._lsame.Run(TRANS, "T"))
                {
                    INFO =  - 2;
                }
                else
                {
                    if (M < 0)
                    {
                        INFO =  - 3;
                    }
                    else
                    {
                        if (N < 0)
                        {
                            INFO =  - 4;
                        }
                        else
                        {
                            if (K < 0 || K > NQ)
                            {
                                INFO =  - 5;
                            }
                            else
                            {
                                if (L < 0 || (LEFT && (L > M)) || (!LEFT && (L > N)))
                                {
                                    INFO =  - 6;
                                }
                                else
                                {
                                    if (LDA < Math.Max(1, K))
                                    {
                                        INFO =  - 8;
                                    }
                                    else
                                    {
                                        if (LDC < Math.Max(1, M))
                                        {
                                            INFO =  - 11;
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
            if (INFO != 0)
            {
                this._xerbla.Run("DORMR3",  - INFO);
                return;
            }
            // *
            // *     Quick return if possible
            // *
            if (M == 0 || N == 0 || K == 0) return;
            // *
            if ((LEFT && !NOTRAN || !LEFT && NOTRAN))
            {
                I1 = 1;
                I2 = K;
                I3 = 1;
            }
            else
            {
                I1 = K;
                I2 = 1;
                I3 =  - 1;
            }
            // *
            if (LEFT)
            {
                NI = N;
                JA = M - L + 1;
                JC = 1;
            }
            else
            {
                MI = M;
                JA = N - L + 1;
                IC = 1;
            }
            // *
            for (I = I1; (I3 >= 0) ? (I <= I2) : (I >= I2); I += I3)
            {
                if (LEFT)
                {
                    // *
                    // *           H(i) or H(i)' is applied to C(i:m,1:n)
                    // *
                    MI = M - I + 1;
                    IC = I;
                }
                else
                {
                    // *
                    // *           H(i) or H(i)' is applied to C(1:m,i:n)
                    // *
                    NI = N - I + 1;
                    JC = I;
                }
                // *
                // *        Apply H(i) or H(i)'
                // *
                this._dlarz.Run(SIDE, MI, NI, L, A, I+JA * LDA + o_a, LDA
                                , TAU[I + o_tau], ref C, IC+JC * LDC + o_c, LDC, ref WORK, offset_work);
                // *
            }
            // *
            return;
            // *
            // *     End of DORMR3
            // *

            #endregion

        }
    }
}
