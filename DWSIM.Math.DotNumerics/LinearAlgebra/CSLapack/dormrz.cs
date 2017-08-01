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
    /// -- LAPACK routine (version 3.1.1) --
    /// Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
    /// January 2007
    /// Purpose
    /// =======
    /// 
    /// DORMRZ overwrites the general real M-by-N matrix C with
    /// 
    /// SIDE = 'L'     SIDE = 'R'
    /// TRANS = 'N':      Q * C          C * Q
    /// TRANS = 'T':      Q**T * C       C * Q**T
    /// 
    /// where Q is a real orthogonal matrix defined as the product of k
    /// elementary reflectors
    /// 
    /// Q = H(1) H(2) . . . H(k)
    /// 
    /// as returned by DTZRZF. Q is of order M if SIDE = 'L' and of order N
    /// if SIDE = 'R'.
    /// 
    ///</summary>
    public class DORMRZ
    {
    

        #region Dependencies
        
        LSAME _lsame; ILAENV _ilaenv; DLARZB _dlarzb; DLARZT _dlarzt; DORMR3 _dormr3; XERBLA _xerbla; 

        #endregion


        #region Variables
        
        const int NBMAX = 64; const int LDT = NBMAX + 1; double[] T = new double[LDT * NBMAX]; 

        #endregion

        public DORMRZ(LSAME lsame, ILAENV ilaenv, DLARZB dlarzb, DLARZT dlarzt, DORMR3 dormr3, XERBLA xerbla)
        {
    

            #region Set Dependencies
            
            this._lsame = lsame; this._ilaenv = ilaenv; this._dlarzb = dlarzb; this._dlarzt = dlarzt; this._dormr3 = dormr3; 
            this._xerbla = xerbla;

            #endregion

        }
    
        public DORMRZ()
        {
    

            #region Dependencies (Initialization)
            
            LSAME lsame = new LSAME();
            IEEECK ieeeck = new IEEECK();
            IPARMQ iparmq = new IPARMQ();
            DCOPY dcopy = new DCOPY();
            XERBLA xerbla = new XERBLA();
            DAXPY daxpy = new DAXPY();
            ILAENV ilaenv = new ILAENV(ieeeck, iparmq);
            DGEMM dgemm = new DGEMM(lsame, xerbla);
            DTRMM dtrmm = new DTRMM(lsame, xerbla);
            DLARZB dlarzb = new DLARZB(lsame, dcopy, dgemm, dtrmm, xerbla);
            DGEMV dgemv = new DGEMV(lsame, xerbla);
            DTRMV dtrmv = new DTRMV(lsame, xerbla);
            DLARZT dlarzt = new DLARZT(dgemv, dtrmv, xerbla, lsame);
            DGER dger = new DGER(xerbla);
            DLARZ dlarz = new DLARZ(daxpy, dcopy, dgemv, dger, lsame);
            DORMR3 dormr3 = new DORMR3(lsame, dlarz, xerbla);

            #endregion


            #region Set Dependencies
            
            this._lsame = lsame; this._ilaenv = ilaenv; this._dlarzb = dlarzb; this._dlarzt = dlarzt; this._dormr3 = dormr3; 
            this._xerbla = xerbla;

            #endregion

        }
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DORMRZ overwrites the general real M-by-N matrix C with
        /// 
        /// SIDE = 'L'     SIDE = 'R'
        /// TRANS = 'N':      Q * C          C * Q
        /// TRANS = 'T':      Q**T * C       C * Q**T
        /// 
        /// where Q is a real orthogonal matrix defined as the product of k
        /// elementary reflectors
        /// 
        /// Q = H(1) H(2) . . . H(k)
        /// 
        /// as returned by DTZRZF. Q is of order M if SIDE = 'L' and of order N
        /// if SIDE = 'R'.
        /// 
        ///</summary>
        /// <param name="SIDE">
        /// = 'L'     SIDE = 'R'
        ///</param>
        /// <param name="TRANS">
        /// (input) CHARACTER*1
        /// = 'N':  No transpose, apply Q;
        /// = 'T':  Transpose, apply Q**T.
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
        /// (input/output) DOUBLE PRECISION array, dimension (LDC,N)
        /// On entry, the M-by-N matrix C.
        /// On exit, C is overwritten by Q*C or Q**H*C or C*Q**H or C*Q.
        ///</param>
        /// <param name="LDC">
        /// (input) INTEGER
        /// The leading dimension of the array C. LDC .GE. max(1,M).
        ///</param>
        /// <param name="WORK">
        /// (workspace/output) DOUBLE PRECISION array, dimension (MAX(1,LWORK))
        /// On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
        ///</param>
        /// <param name="LWORK">
        /// (input) INTEGER
        /// The dimension of the array WORK.
        /// If SIDE = 'L', LWORK .GE. max(1,N);
        /// if SIDE = 'R', LWORK .GE. max(1,M).
        /// For optimum performance LWORK .GE. N*NB if SIDE = 'L', and
        /// LWORK .GE. M*NB if SIDE = 'R', where NB is the optimal
        /// blocksize.
        /// 
        /// If LWORK = -1, then a workspace query is assumed; the routine
        /// only calculates the optimal size of the WORK array, returns
        /// this value as the first entry of the WORK array, and no error
        /// message related to LWORK is issued by XERBLA.
        ///</param>
        /// <param name="INFO">
        /// (output) INTEGER
        /// = 0:  successful exit
        /// .LT. 0:  if INFO = -i, the i-th argument had an illegal value
        ///</param>
        public void Run(string SIDE, string TRANS, int M, int N, int K, int L
                         , double[] A, int offset_a, int LDA, double[] TAU, int offset_tau, ref double[] C, int offset_c, int LDC, ref double[] WORK, int offset_work
                         , int LWORK, ref int INFO)
        {

            #region Variables
            
            bool LEFT = false; bool LQUERY = false; bool NOTRAN = false; string TRANST = new string(' ', 1); int I = 0; 
            int I1 = 0;int I2 = 0; int I3 = 0; int IB = 0; int IC = 0; int IINFO = 0; int IWS = 0; int JA = 0; int JC = 0; 
            int LDWORK = 0;int LWKOPT = 0; int MI = 0; int NB = 0; int NBMIN = 0; int NI = 0; int NQ = 0; int NW = 0; 
            int offset_t = 0;

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
            // *  -- LAPACK routine (version 3.1.1) --
            // *     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
            // *     January 2007
            // *
            // *     .. Scalar Arguments ..
            // *     ..
            // *     .. Array Arguments ..
            // *     ..
            // *
            // *  Purpose
            // *  =======
            // *
            // *  DORMRZ overwrites the general real M-by-N matrix C with
            // *
            // *                  SIDE = 'L'     SIDE = 'R'
            // *  TRANS = 'N':      Q * C          C * Q
            // *  TRANS = 'T':      Q**T * C       C * Q**T
            // *
            // *  where Q is a real orthogonal matrix defined as the product of k
            // *  elementary reflectors
            // *
            // *        Q = H(1) H(2) . . . H(k)
            // *
            // *  as returned by DTZRZF. Q is of order M if SIDE = 'L' and of order N
            // *  if SIDE = 'R'.
            // *
            // *  Arguments
            // *  =========
            // *
            // *  SIDE    (input) CHARACTER*1
            // *          = 'L': apply Q or Q**T from the Left;
            // *          = 'R': apply Q or Q**T from the Right.
            // *
            // *  TRANS   (input) CHARACTER*1
            // *          = 'N':  No transpose, apply Q;
            // *          = 'T':  Transpose, apply Q**T.
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
            // *          On entry, the M-by-N matrix C.
            // *          On exit, C is overwritten by Q*C or Q**H*C or C*Q**H or C*Q.
            // *
            // *  LDC     (input) INTEGER
            // *          The leading dimension of the array C. LDC >= max(1,M).
            // *
            // *  WORK    (workspace/output) DOUBLE PRECISION array, dimension (MAX(1,LWORK))
            // *          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
            // *
            // *  LWORK   (input) INTEGER
            // *          The dimension of the array WORK.
            // *          If SIDE = 'L', LWORK >= max(1,N);
            // *          if SIDE = 'R', LWORK >= max(1,M).
            // *          For optimum performance LWORK >= N*NB if SIDE = 'L', and
            // *          LWORK >= M*NB if SIDE = 'R', where NB is the optimal
            // *          blocksize.
            // *
            // *          If LWORK = -1, then a workspace query is assumed; the routine
            // *          only calculates the optimal size of the WORK array, returns
            // *          this value as the first entry of the WORK array, and no error
            // *          message related to LWORK is issued by XERBLA.
            // *
            // *  INFO    (output) INTEGER
            // *          = 0:  successful exit
            // *          < 0:  if INFO = -i, the i-th argument had an illegal value
            // *
            // *  Further Details
            // *  ===============
            // *
            // *  Based on contributions by
            // *    A. Petitet, Computer Science Dept., Univ. of Tenn., Knoxville, USA
            // *
            // *  =====================================================================
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
            //      INTRINSIC          MAX, MIN;
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
            LQUERY = (LWORK ==  - 1);
            // *
            // *     NQ is the order of Q and NW is the minimum dimension of WORK
            // *
            if (LEFT)
            {
                NQ = M;
                NW = Math.Max(1, N);
            }
            else
            {
                NQ = N;
                NW = Math.Max(1, M);
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
            // *
            if (INFO == 0)
            {
                if (M == 0 || N == 0)
                {
                    LWKOPT = 1;
                }
                else
                {
                    // *
                    // *           Determine the block size.  NB may be at most NBMAX, where
                    // *           NBMAX is used to define the local array T.
                    // *
                    NB = Math.Min(NBMAX, this._ilaenv.Run(1, "DORMRQ", SIDE + TRANS, M, N, K,  - 1));
                    LWKOPT = NW * NB;
                }
                WORK[1 + o_work] = LWKOPT;
                // *
                if (LWORK < Math.Max(1, NW) && !LQUERY)
                {
                    INFO =  - 13;
                }
            }
            // *
            if (INFO != 0)
            {
                this._xerbla.Run("DORMRZ",  - INFO);
                return;
            }
            else
            {
                if (LQUERY)
                {
                    return;
                }
            }
            // *
            // *     Quick return if possible
            // *
            if (M == 0 || N == 0)
            {
                WORK[1 + o_work] = 1;
                return;
            }
            // *
            NBMIN = 2;
            LDWORK = NW;
            if (NB > 1 && NB < K)
            {
                IWS = NW * NB;
                if (LWORK < IWS)
                {
                    NB = LWORK / LDWORK;
                    NBMIN = Math.Max(2, this._ilaenv.Run(2, "DORMRQ", SIDE + TRANS, M, N, K,  - 1));
                }
            }
            else
            {
                IWS = NW;
            }
            // *
            if (NB < NBMIN || NB >= K)
            {
                // *
                // *        Use unblocked code
                // *
                this._dormr3.Run(SIDE, TRANS, M, N, K, L
                                 , A, offset_a, LDA, TAU, offset_tau, ref C, offset_c, LDC, ref WORK, offset_work
                                 , ref IINFO);
            }
            else
            {
                // *
                // *        Use blocked code
                // *
                if ((LEFT && !NOTRAN) || (!LEFT && NOTRAN))
                {
                    I1 = 1;
                    I2 = K;
                    I3 = NB;
                }
                else
                {
                    I1 = ((K - 1) / NB) * NB + 1;
                    I2 = 1;
                    I3 =  - NB;
                }
                // *
                if (LEFT)
                {
                    NI = N;
                    JC = 1;
                    JA = M - L + 1;
                }
                else
                {
                    MI = M;
                    IC = 1;
                    JA = N - L + 1;
                }
                // *
                if (NOTRAN)
                {
                    FortranLib.Copy(ref TRANST , "T");
                }
                else
                {
                    FortranLib.Copy(ref TRANST , "N");
                }
                // *
                for (I = I1; (I3 >= 0) ? (I <= I2) : (I >= I2); I += I3)
                {
                    IB = Math.Min(NB, K - I + 1);
                    // *
                    // *           Form the triangular factor of the block reflector
                    // *           H = H(i+ib-1) . . . H(i+1) H(i)
                    // *
                    this._dlarzt.Run("Backward", "Rowwise", L, IB, A, I+JA * LDA + o_a, LDA
                                     , TAU, I + o_tau, ref T, offset_t, LDT);
                    // *
                    if (LEFT)
                    {
                        // *
                        // *              H or H' is applied to C(i:m,1:n)
                        // *
                        MI = M - I + 1;
                        IC = I;
                    }
                    else
                    {
                        // *
                        // *              H or H' is applied to C(1:m,i:n)
                        // *
                        NI = N - I + 1;
                        JC = I;
                    }
                    // *
                    // *           Apply H or H'
                    // *
                    this._dlarzb.Run(SIDE, TRANST, "Backward", "Rowwise", MI, NI
                                     , IB, L, A, I+JA * LDA + o_a, LDA, T, offset_t, LDT
                                     , ref C, IC+JC * LDC + o_c, LDC, ref WORK, offset_work, LDWORK);
                }
                // *
            }
            // *
            WORK[1 + o_work] = LWKOPT;
            // *
            return;
            // *
            // *     End of DORMRZ
            // *

            #endregion

        }
    }
}
