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
    /// DORMTR overwrites the general real M-by-N matrix C with
    /// 
    /// SIDE = 'L'     SIDE = 'R'
    /// TRANS = 'N':      Q * C          C * Q
    /// TRANS = 'T':      Q**T * C       C * Q**T
    /// 
    /// where Q is a real orthogonal matrix of order nq, with nq = m if
    /// SIDE = 'L' and nq = n if SIDE = 'R'. Q is defined as the product of
    /// nq-1 elementary reflectors, as returned by DSYTRD:
    /// 
    /// if UPLO = 'U', Q = H(nq-1) . . . H(2) H(1);
    /// 
    /// if UPLO = 'L', Q = H(1) H(2) . . . H(nq-1).
    /// 
    ///</summary>
    public class DORMTR
    {
    

        #region Dependencies
        
        LSAME _lsame; ILAENV _ilaenv; DORMQL _dormql; DORMQR _dormqr; XERBLA _xerbla; 

        #endregion

        public DORMTR(LSAME lsame, ILAENV ilaenv, DORMQL dormql, DORMQR dormqr, XERBLA xerbla)
        {
    

            #region Set Dependencies
            
            this._lsame = lsame; this._ilaenv = ilaenv; this._dormql = dormql; this._dormqr = dormqr; this._xerbla = xerbla; 

            #endregion

        }
    
        public DORMTR()
        {
    

            #region Dependencies (Initialization)
            
            LSAME lsame = new LSAME();
            IEEECK ieeeck = new IEEECK();
            IPARMQ iparmq = new IPARMQ();
            DCOPY dcopy = new DCOPY();
            XERBLA xerbla = new XERBLA();
            ILAENV ilaenv = new ILAENV(ieeeck, iparmq);
            DGEMM dgemm = new DGEMM(lsame, xerbla);
            DTRMM dtrmm = new DTRMM(lsame, xerbla);
            DLARFB dlarfb = new DLARFB(lsame, dcopy, dgemm, dtrmm);
            DGEMV dgemv = new DGEMV(lsame, xerbla);
            DTRMV dtrmv = new DTRMV(lsame, xerbla);
            DLARFT dlarft = new DLARFT(dgemv, dtrmv, lsame);
            DGER dger = new DGER(xerbla);
            DLARF dlarf = new DLARF(dgemv, dger, lsame);
            DORM2L dorm2l = new DORM2L(lsame, dlarf, xerbla);
            DORMQL dormql = new DORMQL(lsame, ilaenv, dlarfb, dlarft, dorm2l, xerbla);
            DORM2R dorm2r = new DORM2R(lsame, dlarf, xerbla);
            DORMQR dormqr = new DORMQR(lsame, ilaenv, dlarfb, dlarft, dorm2r, xerbla);

            #endregion


            #region Set Dependencies
            
            this._lsame = lsame; this._ilaenv = ilaenv; this._dormql = dormql; this._dormqr = dormqr; this._xerbla = xerbla; 

            #endregion

        }
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DORMTR overwrites the general real M-by-N matrix C with
        /// 
        /// SIDE = 'L'     SIDE = 'R'
        /// TRANS = 'N':      Q * C          C * Q
        /// TRANS = 'T':      Q**T * C       C * Q**T
        /// 
        /// where Q is a real orthogonal matrix of order nq, with nq = m if
        /// SIDE = 'L' and nq = n if SIDE = 'R'. Q is defined as the product of
        /// nq-1 elementary reflectors, as returned by DSYTRD:
        /// 
        /// if UPLO = 'U', Q = H(nq-1) . . . H(2) H(1);
        /// 
        /// if UPLO = 'L', Q = H(1) H(2) . . . H(nq-1).
        /// 
        ///</summary>
        /// <param name="SIDE">
        /// = 'L'     SIDE = 'R'
        ///</param>
        /// <param name="UPLO">
        /// (input) CHARACTER*1
        /// = 'U': Upper triangle of A contains elementary reflectors
        /// from DSYTRD;
        /// = 'L': Lower triangle of A contains elementary reflectors
        /// from DSYTRD.
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
        /// <param name="A">
        /// (input) DOUBLE PRECISION array, dimension
        /// (LDA,M) if SIDE = 'L'
        /// (LDA,N) if SIDE = 'R'
        /// The vectors which define the elementary reflectors, as
        /// returned by DSYTRD.
        ///</param>
        /// <param name="LDA">
        /// (input) INTEGER
        /// The leading dimension of the array A.
        /// LDA .GE. max(1,M) if SIDE = 'L'; LDA .GE. max(1,N) if SIDE = 'R'.
        ///</param>
        /// <param name="TAU">
        /// (input) DOUBLE PRECISION array, dimension
        /// (M-1) if SIDE = 'L'
        /// (N-1) if SIDE = 'R'
        /// TAU(i) must contain the scalar factor of the elementary
        /// reflector H(i), as returned by DSYTRD.
        ///</param>
        /// <param name="C">
        /// (input/output) DOUBLE PRECISION array, dimension (LDC,N)
        /// On entry, the M-by-N matrix C.
        /// On exit, C is overwritten by Q*C or Q**T*C or C*Q**T or C*Q.
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
        public void Run(string SIDE, string UPLO, string TRANS, int M, int N, ref double[] A, int offset_a
                         , int LDA, double[] TAU, int offset_tau, ref double[] C, int offset_c, int LDC, ref double[] WORK, int offset_work, int LWORK
                         , ref int INFO)
        {

            #region Variables
            
            bool LEFT = false; bool LQUERY = false; bool UPPER = false; int I1 = 0; int I2 = 0; int IINFO = 0; int LWKOPT = 0; 
            int MI = 0;int NB = 0; int NI = 0; int NQ = 0; int NW = 0; 

            #endregion


            #region Array Index Correction
            
             int o_a = -1 - LDA + offset_a;  int o_tau = -1 + offset_tau;  int o_c = -1 - LDC + offset_c; 
             int o_work = -1 + offset_work;

            #endregion


            #region Strings
            
            SIDE = SIDE.Substring(0, 1);  UPLO = UPLO.Substring(0, 1);  TRANS = TRANS.Substring(0, 1);  

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
            // *  DORMTR overwrites the general real M-by-N matrix C with
            // *
            // *                  SIDE = 'L'     SIDE = 'R'
            // *  TRANS = 'N':      Q * C          C * Q
            // *  TRANS = 'T':      Q**T * C       C * Q**T
            // *
            // *  where Q is a real orthogonal matrix of order nq, with nq = m if
            // *  SIDE = 'L' and nq = n if SIDE = 'R'. Q is defined as the product of
            // *  nq-1 elementary reflectors, as returned by DSYTRD:
            // *
            // *  if UPLO = 'U', Q = H(nq-1) . . . H(2) H(1);
            // *
            // *  if UPLO = 'L', Q = H(1) H(2) . . . H(nq-1).
            // *
            // *  Arguments
            // *  =========
            // *
            // *  SIDE    (input) CHARACTER*1
            // *          = 'L': apply Q or Q**T from the Left;
            // *          = 'R': apply Q or Q**T from the Right.
            // *
            // *  UPLO    (input) CHARACTER*1
            // *          = 'U': Upper triangle of A contains elementary reflectors
            // *                 from DSYTRD;
            // *          = 'L': Lower triangle of A contains elementary reflectors
            // *                 from DSYTRD.
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
            // *  A       (input) DOUBLE PRECISION array, dimension
            // *                               (LDA,M) if SIDE = 'L'
            // *                               (LDA,N) if SIDE = 'R'
            // *          The vectors which define the elementary reflectors, as
            // *          returned by DSYTRD.
            // *
            // *  LDA     (input) INTEGER
            // *          The leading dimension of the array A.
            // *          LDA >= max(1,M) if SIDE = 'L'; LDA >= max(1,N) if SIDE = 'R'.
            // *
            // *  TAU     (input) DOUBLE PRECISION array, dimension
            // *                               (M-1) if SIDE = 'L'
            // *                               (N-1) if SIDE = 'R'
            // *          TAU(i) must contain the scalar factor of the elementary
            // *          reflector H(i), as returned by DSYTRD.
            // *
            // *  C       (input/output) DOUBLE PRECISION array, dimension (LDC,N)
            // *          On entry, the M-by-N matrix C.
            // *          On exit, C is overwritten by Q*C or Q**T*C or C*Q**T or C*Q.
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
            UPPER = this._lsame.Run(UPLO, "U");
            LQUERY = (LWORK ==  - 1);
            // *
            // *     NQ is the order of Q and NW is the minimum dimension of WORK
            // *
            if (LEFT)
            {
                NQ = M;
                NW = N;
            }
            else
            {
                NQ = N;
                NW = M;
            }
            if (!LEFT && !this._lsame.Run(SIDE, "R"))
            {
                INFO =  - 1;
            }
            else
            {
                if (!UPPER && !this._lsame.Run(UPLO, "L"))
                {
                    INFO =  - 2;
                }
                else
                {
                    if (!this._lsame.Run(TRANS, "N") && !this._lsame.Run(TRANS, "T"))
                    {
                        INFO =  - 3;
                    }
                    else
                    {
                        if (M < 0)
                        {
                            INFO =  - 4;
                        }
                        else
                        {
                            if (N < 0)
                            {
                                INFO =  - 5;
                            }
                            else
                            {
                                if (LDA < Math.Max(1, NQ))
                                {
                                    INFO =  - 7;
                                }
                                else
                                {
                                    if (LDC < Math.Max(1, M))
                                    {
                                        INFO =  - 10;
                                    }
                                    else
                                    {
                                        if (LWORK < Math.Max(1, NW) && !LQUERY)
                                        {
                                            INFO =  - 12;
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
                if (UPPER)
                {
                    if (LEFT)
                    {
                        NB = this._ilaenv.Run(1, "DORMQL", SIDE + TRANS, M - 1, N, M - 1,  - 1);
                    }
                    else
                    {
                        NB = this._ilaenv.Run(1, "DORMQL", SIDE + TRANS, M, N - 1, N - 1,  - 1);
                    }
                }
                else
                {
                    if (LEFT)
                    {
                        NB = this._ilaenv.Run(1, "DORMQR", SIDE + TRANS, M - 1, N, M - 1,  - 1);
                    }
                    else
                    {
                        NB = this._ilaenv.Run(1, "DORMQR", SIDE + TRANS, M, N - 1, N - 1,  - 1);
                    }
                }
                LWKOPT = Math.Max(1, NW) * NB;
                WORK[1 + o_work] = LWKOPT;
            }
            // *
            if (INFO != 0)
            {
                this._xerbla.Run("DORMTR",  - INFO);
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
            if (M == 0 || N == 0 || NQ == 1)
            {
                WORK[1 + o_work] = 1;
                return;
            }
            // *
            if (LEFT)
            {
                MI = M - 1;
                NI = N;
            }
            else
            {
                MI = M;
                NI = N - 1;
            }
            // *
            if (UPPER)
            {
                // *
                // *        Q was determined by a call to DSYTRD with UPLO = 'U'
                // *
                this._dormql.Run(SIDE, TRANS, MI, NI, NQ - 1, ref A, 1+2 * LDA + o_a
                                 , LDA, TAU, offset_tau, ref C, offset_c, LDC, ref WORK, offset_work, LWORK
                                 , ref IINFO);
            }
            else
            {
                // *
                // *        Q was determined by a call to DSYTRD with UPLO = 'L'
                // *
                if (LEFT)
                {
                    I1 = 2;
                    I2 = 1;
                }
                else
                {
                    I1 = 1;
                    I2 = 2;
                }
                this._dormqr.Run(SIDE, TRANS, MI, NI, NQ - 1, ref A, 2+1 * LDA + o_a
                                 , LDA, TAU, offset_tau, ref C, I1+I2 * LDC + o_c, LDC, ref WORK, offset_work, LWORK
                                 , ref IINFO);
            }
            WORK[1 + o_work] = LWKOPT;
            return;
            // *
            // *     End of DORMTR
            // *

            #endregion

        }
    }
}
