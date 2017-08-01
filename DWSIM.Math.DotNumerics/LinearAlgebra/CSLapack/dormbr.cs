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
    /// If VECT = 'Q', DORMBR overwrites the general real M-by-N matrix C
    /// with
    /// SIDE = 'L'     SIDE = 'R'
    /// TRANS = 'N':      Q * C          C * Q
    /// TRANS = 'T':      Q**T * C       C * Q**T
    /// 
    /// If VECT = 'P', DORMBR overwrites the general real M-by-N matrix C
    /// with
    /// SIDE = 'L'     SIDE = 'R'
    /// TRANS = 'N':      P * C          C * P
    /// TRANS = 'T':      P**T * C       C * P**T
    /// 
    /// Here Q and P**T are the orthogonal matrices determined by DGEBRD when
    /// reducing a real matrix A to bidiagonal form: A = Q * B * P**T. Q and
    /// P**T are defined as products of elementary reflectors H(i) and G(i)
    /// respectively.
    /// 
    /// Let nq = m if SIDE = 'L' and nq = n if SIDE = 'R'. Thus nq is the
    /// order of the orthogonal matrix Q or P**T that is applied.
    /// 
    /// If VECT = 'Q', A is assumed to have been an NQ-by-K matrix:
    /// if nq .GE. k, Q = H(1) H(2) . . . H(k);
    /// if nq .LT. k, Q = H(1) H(2) . . . H(nq-1).
    /// 
    /// If VECT = 'P', A is assumed to have been a K-by-NQ matrix:
    /// if k .LT. nq, P = G(1) G(2) . . . G(k);
    /// if k .GE. nq, P = G(1) G(2) . . . G(nq-1).
    /// 
    ///</summary>
    public class DORMBR
    {
    

        #region Dependencies
        
        LSAME _lsame; ILAENV _ilaenv; DORMLQ _dormlq; DORMQR _dormqr; XERBLA _xerbla; 

        #endregion

        public DORMBR(LSAME lsame, ILAENV ilaenv, DORMLQ dormlq, DORMQR dormqr, XERBLA xerbla)
        {
    

            #region Set Dependencies
            
            this._lsame = lsame; this._ilaenv = ilaenv; this._dormlq = dormlq; this._dormqr = dormqr; this._xerbla = xerbla; 

            #endregion

        }
    
        public DORMBR()
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
            DORML2 dorml2 = new DORML2(lsame, dlarf, xerbla);
            DORMLQ dormlq = new DORMLQ(lsame, ilaenv, dlarfb, dlarft, dorml2, xerbla);
            DORM2R dorm2r = new DORM2R(lsame, dlarf, xerbla);
            DORMQR dormqr = new DORMQR(lsame, ilaenv, dlarfb, dlarft, dorm2r, xerbla);

            #endregion


            #region Set Dependencies
            
            this._lsame = lsame; this._ilaenv = ilaenv; this._dormlq = dormlq; this._dormqr = dormqr; this._xerbla = xerbla; 

            #endregion

        }
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// If VECT = 'Q', DORMBR overwrites the general real M-by-N matrix C
        /// with
        /// SIDE = 'L'     SIDE = 'R'
        /// TRANS = 'N':      Q * C          C * Q
        /// TRANS = 'T':      Q**T * C       C * Q**T
        /// 
        /// If VECT = 'P', DORMBR overwrites the general real M-by-N matrix C
        /// with
        /// SIDE = 'L'     SIDE = 'R'
        /// TRANS = 'N':      P * C          C * P
        /// TRANS = 'T':      P**T * C       C * P**T
        /// 
        /// Here Q and P**T are the orthogonal matrices determined by DGEBRD when
        /// reducing a real matrix A to bidiagonal form: A = Q * B * P**T. Q and
        /// P**T are defined as products of elementary reflectors H(i) and G(i)
        /// respectively.
        /// 
        /// Let nq = m if SIDE = 'L' and nq = n if SIDE = 'R'. Thus nq is the
        /// order of the orthogonal matrix Q or P**T that is applied.
        /// 
        /// If VECT = 'Q', A is assumed to have been an NQ-by-K matrix:
        /// if nq .GE. k, Q = H(1) H(2) . . . H(k);
        /// if nq .LT. k, Q = H(1) H(2) . . . H(nq-1).
        /// 
        /// If VECT = 'P', A is assumed to have been a K-by-NQ matrix:
        /// if k .LT. nq, P = G(1) G(2) . . . G(k);
        /// if k .GE. nq, P = G(1) G(2) . . . G(nq-1).
        /// 
        ///</summary>
        /// <param name="VECT">
        /// (input) CHARACTER*1
        /// = 'Q': apply Q or Q**T;
        /// = 'P': apply P or P**T.
        ///</param>
        /// <param name="SIDE">
        /// (input) CHARACTER*1
        /// = 'L': apply Q, Q**T, P or P**T from the Left;
        /// = 'R': apply Q, Q**T, P or P**T from the Right.
        ///</param>
        /// <param name="TRANS">
        /// (input) CHARACTER*1
        /// = 'N':  No transpose, apply Q  or P;
        /// = 'T':  Transpose, apply Q**T or P**T.
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
        /// If VECT = 'Q', the number of columns in the original
        /// matrix reduced by DGEBRD.
        /// If VECT = 'P', the number of rows in the original
        /// matrix reduced by DGEBRD.
        /// K .GE. 0.
        ///</param>
        /// <param name="A">
        /// (input) DOUBLE PRECISION array, dimension
        /// (LDA,min(nq,K)) if VECT = 'Q'
        /// (LDA,nq)        if VECT = 'P'
        /// The vectors which define the elementary reflectors H(i) and
        /// G(i), whose products determine the matrices Q and P, as
        /// returned by DGEBRD.
        ///</param>
        /// <param name="LDA">
        /// (input) INTEGER
        /// The leading dimension of the array A.
        /// If VECT = 'Q', LDA .GE. max(1,nq);
        /// if VECT = 'P', LDA .GE. max(1,min(nq,K)).
        ///</param>
        /// <param name="TAU">
        /// (input) DOUBLE PRECISION array, dimension (min(nq,K))
        /// TAU(i) must contain the scalar factor of the elementary
        /// reflector H(i) or G(i) which determines Q or P, as returned
        /// by DGEBRD in the array argument TAUQ or TAUP.
        ///</param>
        /// <param name="C">
        /// (input/output) DOUBLE PRECISION array, dimension (LDC,N)
        /// On entry, the M-by-N matrix C.
        /// On exit, C is overwritten by Q*C or Q**T*C or C*Q**T or C*Q
        /// or P*C or P**T*C or C*P or C*P**T.
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
        public void Run(string VECT, string SIDE, string TRANS, int M, int N, int K
                         , ref double[] A, int offset_a, int LDA, double[] TAU, int offset_tau, ref double[] C, int offset_c, int LDC, ref double[] WORK, int offset_work
                         , int LWORK, ref int INFO)
        {

            #region Variables
            
            bool APPLYQ = false; bool LEFT = false; bool LQUERY = false; bool NOTRAN = false; string TRANST = new string(' ', 1); 
            int I1 = 0;int I2 = 0; int IINFO = 0; int LWKOPT = 0; int MI = 0; int NB = 0; int NI = 0; int NQ = 0; int NW = 0; 

            #endregion


            #region Array Index Correction
            
             int o_a = -1 - LDA + offset_a;  int o_tau = -1 + offset_tau;  int o_c = -1 - LDC + offset_c; 
             int o_work = -1 + offset_work;

            #endregion


            #region Strings
            
            VECT = VECT.Substring(0, 1);  SIDE = SIDE.Substring(0, 1);  TRANS = TRANS.Substring(0, 1);  

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
            // *  If VECT = 'Q', DORMBR overwrites the general real M-by-N matrix C
            // *  with
            // *                  SIDE = 'L'     SIDE = 'R'
            // *  TRANS = 'N':      Q * C          C * Q
            // *  TRANS = 'T':      Q**T * C       C * Q**T
            // *
            // *  If VECT = 'P', DORMBR overwrites the general real M-by-N matrix C
            // *  with
            // *                  SIDE = 'L'     SIDE = 'R'
            // *  TRANS = 'N':      P * C          C * P
            // *  TRANS = 'T':      P**T * C       C * P**T
            // *
            // *  Here Q and P**T are the orthogonal matrices determined by DGEBRD when
            // *  reducing a real matrix A to bidiagonal form: A = Q * B * P**T. Q and
            // *  P**T are defined as products of elementary reflectors H(i) and G(i)
            // *  respectively.
            // *
            // *  Let nq = m if SIDE = 'L' and nq = n if SIDE = 'R'. Thus nq is the
            // *  order of the orthogonal matrix Q or P**T that is applied.
            // *
            // *  If VECT = 'Q', A is assumed to have been an NQ-by-K matrix:
            // *  if nq >= k, Q = H(1) H(2) . . . H(k);
            // *  if nq < k, Q = H(1) H(2) . . . H(nq-1).
            // *
            // *  If VECT = 'P', A is assumed to have been a K-by-NQ matrix:
            // *  if k < nq, P = G(1) G(2) . . . G(k);
            // *  if k >= nq, P = G(1) G(2) . . . G(nq-1).
            // *
            // *  Arguments
            // *  =========
            // *
            // *  VECT    (input) CHARACTER*1
            // *          = 'Q': apply Q or Q**T;
            // *          = 'P': apply P or P**T.
            // *
            // *  SIDE    (input) CHARACTER*1
            // *          = 'L': apply Q, Q**T, P or P**T from the Left;
            // *          = 'R': apply Q, Q**T, P or P**T from the Right.
            // *
            // *  TRANS   (input) CHARACTER*1
            // *          = 'N':  No transpose, apply Q  or P;
            // *          = 'T':  Transpose, apply Q**T or P**T.
            // *
            // *  M       (input) INTEGER
            // *          The number of rows of the matrix C. M >= 0.
            // *
            // *  N       (input) INTEGER
            // *          The number of columns of the matrix C. N >= 0.
            // *
            // *  K       (input) INTEGER
            // *          If VECT = 'Q', the number of columns in the original
            // *          matrix reduced by DGEBRD.
            // *          If VECT = 'P', the number of rows in the original
            // *          matrix reduced by DGEBRD.
            // *          K >= 0.
            // *
            // *  A       (input) DOUBLE PRECISION array, dimension
            // *                                (LDA,min(nq,K)) if VECT = 'Q'
            // *                                (LDA,nq)        if VECT = 'P'
            // *          The vectors which define the elementary reflectors H(i) and
            // *          G(i), whose products determine the matrices Q and P, as
            // *          returned by DGEBRD.
            // *
            // *  LDA     (input) INTEGER
            // *          The leading dimension of the array A.
            // *          If VECT = 'Q', LDA >= max(1,nq);
            // *          if VECT = 'P', LDA >= max(1,min(nq,K)).
            // *
            // *  TAU     (input) DOUBLE PRECISION array, dimension (min(nq,K))
            // *          TAU(i) must contain the scalar factor of the elementary
            // *          reflector H(i) or G(i) which determines Q or P, as returned
            // *          by DGEBRD in the array argument TAUQ or TAUP.
            // *
            // *  C       (input/output) DOUBLE PRECISION array, dimension (LDC,N)
            // *          On entry, the M-by-N matrix C.
            // *          On exit, C is overwritten by Q*C or Q**T*C or C*Q**T or C*Q
            // *          or P*C or P**T*C or C*P or C*P**T.
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
            //      INTRINSIC          MAX, MIN;
            // *     ..
            // *     .. Executable Statements ..
            // *
            // *     Test the input arguments
            // *

            #endregion


            #region Body
            
            INFO = 0;
            APPLYQ = this._lsame.Run(VECT, "Q");
            LEFT = this._lsame.Run(SIDE, "L");
            NOTRAN = this._lsame.Run(TRANS, "N");
            LQUERY = (LWORK ==  - 1);
            // *
            // *     NQ is the order of Q or P and NW is the minimum dimension of WORK
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
            if (!APPLYQ && !this._lsame.Run(VECT, "P"))
            {
                INFO =  - 1;
            }
            else
            {
                if (!LEFT && !this._lsame.Run(SIDE, "R"))
                {
                    INFO =  - 2;
                }
                else
                {
                    if (!NOTRAN && !this._lsame.Run(TRANS, "T"))
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
                                if (K < 0)
                                {
                                    INFO =  - 6;
                                }
                                else
                                {
                                    if ((APPLYQ && LDA < Math.Max(1, NQ)) || (!APPLYQ && LDA < Math.Max(1, Math.Min(NQ, K))))
                                    {
                                        INFO =  - 8;
                                    }
                                    else
                                    {
                                        if (LDC < Math.Max(1, M))
                                        {
                                            INFO =  - 11;
                                        }
                                        else
                                        {
                                            if (LWORK < Math.Max(1, NW) && !LQUERY)
                                            {
                                                INFO =  - 13;
                                            }
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
                if (APPLYQ)
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
                else
                {
                    if (LEFT)
                    {
                        NB = this._ilaenv.Run(1, "DORMLQ", SIDE + TRANS, M - 1, N, M - 1,  - 1);
                    }
                    else
                    {
                        NB = this._ilaenv.Run(1, "DORMLQ", SIDE + TRANS, M, N - 1, N - 1,  - 1);
                    }
                }
                LWKOPT = Math.Max(1, NW) * NB;
                WORK[1 + o_work] = LWKOPT;
            }
            // *
            if (INFO != 0)
            {
                this._xerbla.Run("DORMBR",  - INFO);
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
            WORK[1 + o_work] = 1;
            if (M == 0 || N == 0) return;
            // *
            if (APPLYQ)
            {
                // *
                // *        Apply Q
                // *
                if (NQ >= K)
                {
                    // *
                    // *           Q was determined by a call to DGEBRD with nq >= k
                    // *
                    this._dormqr.Run(SIDE, TRANS, M, N, K, ref A, offset_a
                                     , LDA, TAU, offset_tau, ref C, offset_c, LDC, ref WORK, offset_work, LWORK
                                     , ref IINFO);
                }
                else
                {
                    if (NQ > 1)
                    {
                        // *
                        // *           Q was determined by a call to DGEBRD with nq < k
                        // *
                        if (LEFT)
                        {
                            MI = M - 1;
                            NI = N;
                            I1 = 2;
                            I2 = 1;
                        }
                        else
                        {
                            MI = M;
                            NI = N - 1;
                            I1 = 1;
                            I2 = 2;
                        }
                        this._dormqr.Run(SIDE, TRANS, MI, NI, NQ - 1, ref A, 2+1 * LDA + o_a
                                         , LDA, TAU, offset_tau, ref C, I1+I2 * LDC + o_c, LDC, ref WORK, offset_work, LWORK
                                         , ref IINFO);
                    }
                }
            }
            else
            {
                // *
                // *        Apply P
                // *
                if (NOTRAN)
                {
                    FortranLib.Copy(ref TRANST , "T");
                }
                else
                {
                    FortranLib.Copy(ref TRANST , "N");
                }
                if (NQ > K)
                {
                    // *
                    // *           P was determined by a call to DGEBRD with nq > k
                    // *
                    this._dormlq.Run(SIDE, TRANST, M, N, K, ref A, offset_a
                                     , LDA, TAU, offset_tau, ref C, offset_c, LDC, ref WORK, offset_work, LWORK
                                     , ref IINFO);
                }
                else
                {
                    if (NQ > 1)
                    {
                        // *
                        // *           P was determined by a call to DGEBRD with nq <= k
                        // *
                        if (LEFT)
                        {
                            MI = M - 1;
                            NI = N;
                            I1 = 2;
                            I2 = 1;
                        }
                        else
                        {
                            MI = M;
                            NI = N - 1;
                            I1 = 1;
                            I2 = 2;
                        }
                        this._dormlq.Run(SIDE, TRANST, MI, NI, NQ - 1, ref A, 1+2 * LDA + o_a
                                         , LDA, TAU, offset_tau, ref C, I1+I2 * LDC + o_c, LDC, ref WORK, offset_work, LWORK
                                         , ref IINFO);
                    }
                }
            }
            WORK[1 + o_work] = LWKOPT;
            return;
            // *
            // *     End of DORMBR
            // *

            #endregion

        }
    }
}
