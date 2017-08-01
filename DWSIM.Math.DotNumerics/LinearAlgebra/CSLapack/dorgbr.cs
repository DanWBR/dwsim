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
    /// DORGBR generates one of the real orthogonal matrices Q or P**T
    /// determined by DGEBRD when reducing a real matrix A to bidiagonal
    /// form: A = Q * B * P**T.  Q and P**T are defined as products of
    /// elementary reflectors H(i) or G(i) respectively.
    /// 
    /// If VECT = 'Q', A is assumed to have been an M-by-K matrix, and Q
    /// is of order M:
    /// if m .GE. k, Q = H(1) H(2) . . . H(k) and DORGBR returns the first n
    /// columns of Q, where m .GE. n .GE. k;
    /// if m .LT. k, Q = H(1) H(2) . . . H(m-1) and DORGBR returns Q as an
    /// M-by-M matrix.
    /// 
    /// If VECT = 'P', A is assumed to have been a K-by-N matrix, and P**T
    /// is of order N:
    /// if k .LT. n, P**T = G(k) . . . G(2) G(1) and DORGBR returns the first m
    /// rows of P**T, where n .GE. m .GE. k;
    /// if k .GE. n, P**T = G(n-1) . . . G(2) G(1) and DORGBR returns P**T as
    /// an N-by-N matrix.
    /// 
    ///</summary>
    public class DORGBR
    {
    

        #region Dependencies
        
        LSAME _lsame; ILAENV _ilaenv; DORGLQ _dorglq; DORGQR _dorgqr; XERBLA _xerbla; 

        #endregion


        #region Variables
        
        const double ZERO = 0.0E+0; const double ONE = 1.0E+0; 

        #endregion

        public DORGBR(LSAME lsame, ILAENV ilaenv, DORGLQ dorglq, DORGQR dorgqr, XERBLA xerbla)
        {
    

            #region Set Dependencies
            
            this._lsame = lsame; this._ilaenv = ilaenv; this._dorglq = dorglq; this._dorgqr = dorgqr; this._xerbla = xerbla; 

            #endregion

        }
    
        public DORGBR()
        {
    

            #region Dependencies (Initialization)
            
            LSAME lsame = new LSAME();
            IEEECK ieeeck = new IEEECK();
            IPARMQ iparmq = new IPARMQ();
            DCOPY dcopy = new DCOPY();
            XERBLA xerbla = new XERBLA();
            DSCAL dscal = new DSCAL();
            ILAENV ilaenv = new ILAENV(ieeeck, iparmq);
            DGEMM dgemm = new DGEMM(lsame, xerbla);
            DTRMM dtrmm = new DTRMM(lsame, xerbla);
            DLARFB dlarfb = new DLARFB(lsame, dcopy, dgemm, dtrmm);
            DGEMV dgemv = new DGEMV(lsame, xerbla);
            DTRMV dtrmv = new DTRMV(lsame, xerbla);
            DLARFT dlarft = new DLARFT(dgemv, dtrmv, lsame);
            DGER dger = new DGER(xerbla);
            DLARF dlarf = new DLARF(dgemv, dger, lsame);
            DORGL2 dorgl2 = new DORGL2(dlarf, dscal, xerbla);
            DORGLQ dorglq = new DORGLQ(dlarfb, dlarft, dorgl2, xerbla, ilaenv);
            DORG2R dorg2r = new DORG2R(dlarf, dscal, xerbla);
            DORGQR dorgqr = new DORGQR(dlarfb, dlarft, dorg2r, xerbla, ilaenv);

            #endregion


            #region Set Dependencies
            
            this._lsame = lsame; this._ilaenv = ilaenv; this._dorglq = dorglq; this._dorgqr = dorgqr; this._xerbla = xerbla; 

            #endregion

        }
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DORGBR generates one of the real orthogonal matrices Q or P**T
        /// determined by DGEBRD when reducing a real matrix A to bidiagonal
        /// form: A = Q * B * P**T.  Q and P**T are defined as products of
        /// elementary reflectors H(i) or G(i) respectively.
        /// 
        /// If VECT = 'Q', A is assumed to have been an M-by-K matrix, and Q
        /// is of order M:
        /// if m .GE. k, Q = H(1) H(2) . . . H(k) and DORGBR returns the first n
        /// columns of Q, where m .GE. n .GE. k;
        /// if m .LT. k, Q = H(1) H(2) . . . H(m-1) and DORGBR returns Q as an
        /// M-by-M matrix.
        /// 
        /// If VECT = 'P', A is assumed to have been a K-by-N matrix, and P**T
        /// is of order N:
        /// if k .LT. n, P**T = G(k) . . . G(2) G(1) and DORGBR returns the first m
        /// rows of P**T, where n .GE. m .GE. k;
        /// if k .GE. n, P**T = G(n-1) . . . G(2) G(1) and DORGBR returns P**T as
        /// an N-by-N matrix.
        /// 
        ///</summary>
        /// <param name="VECT">
        /// (input) CHARACTER*1
        /// Specifies whether the matrix Q or the matrix P**T is
        /// required, as defined in the transformation applied by DGEBRD:
        /// = 'Q':  generate Q;
        /// = 'P':  generate P**T.
        ///</param>
        /// <param name="M">
        /// (input) INTEGER
        /// The number of rows of the matrix Q or P**T to be returned.
        /// M .GE. 0.
        ///</param>
        /// <param name="N">
        /// (input) INTEGER
        /// The number of columns of the matrix Q or P**T to be returned.
        /// N .GE. 0.
        /// If VECT = 'Q', M .GE. N .GE. min(M,K);
        /// if VECT = 'P', N .GE. M .GE. min(N,K).
        ///</param>
        /// <param name="K">
        /// (input) INTEGER
        /// If VECT = 'Q', the number of columns in the original M-by-K
        /// matrix reduced by DGEBRD.
        /// If VECT = 'P', the number of rows in the original K-by-N
        /// matrix reduced by DGEBRD.
        /// K .GE. 0.
        ///</param>
        /// <param name="A">
        /// (input/output) DOUBLE PRECISION array, dimension (LDA,N)
        /// On entry, the vectors which define the elementary reflectors,
        /// as returned by DGEBRD.
        /// On exit, the M-by-N matrix Q or P**T.
        ///</param>
        /// <param name="LDA">
        /// (input) INTEGER
        /// The leading dimension of the array A. LDA .GE. max(1,M).
        ///</param>
        /// <param name="TAU">
        /// (input) DOUBLE PRECISION array, dimension
        /// (min(M,K)) if VECT = 'Q'
        /// (min(N,K)) if VECT = 'P'
        /// TAU(i) must contain the scalar factor of the elementary
        /// reflector H(i) or G(i), which determines Q or P**T, as
        /// returned by DGEBRD in its array argument TAUQ or TAUP.
        ///</param>
        /// <param name="WORK">
        /// (workspace/output) DOUBLE PRECISION array, dimension (MAX(1,LWORK))
        /// On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
        ///</param>
        /// <param name="LWORK">
        /// (input) INTEGER
        /// The dimension of the array WORK. LWORK .GE. max(1,min(M,N)).
        /// For optimum performance LWORK .GE. min(M,N)*NB, where NB
        /// is the optimal blocksize.
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
        public void Run(string VECT, int M, int N, int K, ref double[] A, int offset_a, int LDA
                         , double[] TAU, int offset_tau, ref double[] WORK, int offset_work, int LWORK, ref int INFO)
        {

            #region Variables
            
            bool LQUERY = false; bool WANTQ = false; int I = 0; int IINFO = 0; int J = 0; int LWKOPT = 0; int MN = 0; int NB = 0; 

            #endregion


            #region Implicit Variables
            
            int A_J = 0; int A_0 = 0; int A_1 = 0; 

            #endregion


            #region Array Index Correction
            
             int o_a = -1 - LDA + offset_a;  int o_tau = -1 + offset_tau;  int o_work = -1 + offset_work; 

            #endregion


            #region Strings
            
            VECT = VECT.Substring(0, 1);  

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
            // *  DORGBR generates one of the real orthogonal matrices Q or P**T
            // *  determined by DGEBRD when reducing a real matrix A to bidiagonal
            // *  form: A = Q * B * P**T.  Q and P**T are defined as products of
            // *  elementary reflectors H(i) or G(i) respectively.
            // *
            // *  If VECT = 'Q', A is assumed to have been an M-by-K matrix, and Q
            // *  is of order M:
            // *  if m >= k, Q = H(1) H(2) . . . H(k) and DORGBR returns the first n
            // *  columns of Q, where m >= n >= k;
            // *  if m < k, Q = H(1) H(2) . . . H(m-1) and DORGBR returns Q as an
            // *  M-by-M matrix.
            // *
            // *  If VECT = 'P', A is assumed to have been a K-by-N matrix, and P**T
            // *  is of order N:
            // *  if k < n, P**T = G(k) . . . G(2) G(1) and DORGBR returns the first m
            // *  rows of P**T, where n >= m >= k;
            // *  if k >= n, P**T = G(n-1) . . . G(2) G(1) and DORGBR returns P**T as
            // *  an N-by-N matrix.
            // *
            // *  Arguments
            // *  =========
            // *
            // *  VECT    (input) CHARACTER*1
            // *          Specifies whether the matrix Q or the matrix P**T is
            // *          required, as defined in the transformation applied by DGEBRD:
            // *          = 'Q':  generate Q;
            // *          = 'P':  generate P**T.
            // *
            // *  M       (input) INTEGER
            // *          The number of rows of the matrix Q or P**T to be returned.
            // *          M >= 0.
            // *
            // *  N       (input) INTEGER
            // *          The number of columns of the matrix Q or P**T to be returned.
            // *          N >= 0.
            // *          If VECT = 'Q', M >= N >= min(M,K);
            // *          if VECT = 'P', N >= M >= min(N,K).
            // *
            // *  K       (input) INTEGER
            // *          If VECT = 'Q', the number of columns in the original M-by-K
            // *          matrix reduced by DGEBRD.
            // *          If VECT = 'P', the number of rows in the original K-by-N
            // *          matrix reduced by DGEBRD.
            // *          K >= 0.
            // *
            // *  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
            // *          On entry, the vectors which define the elementary reflectors,
            // *          as returned by DGEBRD.
            // *          On exit, the M-by-N matrix Q or P**T.
            // *
            // *  LDA     (input) INTEGER
            // *          The leading dimension of the array A. LDA >= max(1,M).
            // *
            // *  TAU     (input) DOUBLE PRECISION array, dimension
            // *                                (min(M,K)) if VECT = 'Q'
            // *                                (min(N,K)) if VECT = 'P'
            // *          TAU(i) must contain the scalar factor of the elementary
            // *          reflector H(i) or G(i), which determines Q or P**T, as
            // *          returned by DGEBRD in its array argument TAUQ or TAUP.
            // *
            // *  WORK    (workspace/output) DOUBLE PRECISION array, dimension (MAX(1,LWORK))
            // *          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
            // *
            // *  LWORK   (input) INTEGER
            // *          The dimension of the array WORK. LWORK >= max(1,min(M,N)).
            // *          For optimum performance LWORK >= min(M,N)*NB, where NB
            // *          is the optimal blocksize.
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
            // *     .. Parameters ..
            // *     ..
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
            WANTQ = this._lsame.Run(VECT, "Q");
            MN = Math.Min(M, N);
            LQUERY = (LWORK ==  - 1);
            if (!WANTQ && !this._lsame.Run(VECT, "P"))
            {
                INFO =  - 1;
            }
            else
            {
                if (M < 0)
                {
                    INFO =  - 2;
                }
                else
                {
                    if (N < 0 || (WANTQ && (N > M || N < Math.Min(M, K))) || (!WANTQ && (M > N || M < Math.Min(N, K))))
                    {
                        INFO =  - 3;
                    }
                    else
                    {
                        if (K < 0)
                        {
                            INFO =  - 4;
                        }
                        else
                        {
                            if (LDA < Math.Max(1, M))
                            {
                                INFO =  - 6;
                            }
                            else
                            {
                                if (LWORK < Math.Max(1, MN) && !LQUERY)
                                {
                                    INFO =  - 9;
                                }
                            }
                        }
                    }
                }
            }
            // *
            if (INFO == 0)
            {
                if (WANTQ)
                {
                    NB = this._ilaenv.Run(1, "DORGQR", " ", M, N, K,  - 1);
                }
                else
                {
                    NB = this._ilaenv.Run(1, "DORGLQ", " ", M, N, K,  - 1);
                }
                LWKOPT = Math.Max(1, MN) * NB;
                WORK[1 + o_work] = LWKOPT;
            }
            // *
            if (INFO != 0)
            {
                this._xerbla.Run("DORGBR",  - INFO);
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
            if (WANTQ)
            {
                // *
                // *        Form Q, determined by a call to DGEBRD to reduce an m-by-k
                // *        matrix
                // *
                if (M >= K)
                {
                    // *
                    // *           If m >= k, assume m >= n >= k
                    // *
                    this._dorgqr.Run(M, N, K, ref A, offset_a, LDA, TAU, offset_tau
                                     , ref WORK, offset_work, LWORK, ref IINFO);
                    // *
                }
                else
                {
                    // *
                    // *           If m < k, assume m = n
                    // *
                    // *           Shift the vectors which define the elementary reflectors one
                    // *           column to the right, and set the first row and column of Q
                    // *           to those of the unit matrix
                    // *
                    for (J = M; J >= 2; J +=  - 1)
                    {
                        A[1+J * LDA + o_a] = ZERO;
                        A_J = J * LDA + o_a;
                        A_0 = (J - 1) * LDA + o_a;
                        for (I = J + 1; I <= M; I++)
                        {
                            A[I + A_J] = A[I + A_0];
                        }
                    }
                    A[1+1 * LDA + o_a] = ONE;
                    A_1 = 1 * LDA + o_a;
                    for (I = 2; I <= M; I++)
                    {
                        A[I + A_1] = ZERO;
                    }
                    if (M > 1)
                    {
                        // *
                        // *              Form Q(2:m,2:m)
                        // *
                        this._dorgqr.Run(M - 1, M - 1, M - 1, ref A, 2+2 * LDA + o_a, LDA, TAU, offset_tau
                                         , ref WORK, offset_work, LWORK, ref IINFO);
                    }
                }
            }
            else
            {
                // *
                // *        Form P', determined by a call to DGEBRD to reduce a k-by-n
                // *        matrix
                // *
                if (K < N)
                {
                    // *
                    // *           If k < n, assume k <= m <= n
                    // *
                    this._dorglq.Run(M, N, K, ref A, offset_a, LDA, TAU, offset_tau
                                     , ref WORK, offset_work, LWORK, ref IINFO);
                    // *
                }
                else
                {
                    // *
                    // *           If k >= n, assume m = n
                    // *
                    // *           Shift the vectors which define the elementary reflectors one
                    // *           row downward, and set the first row and column of P' to
                    // *           those of the unit matrix
                    // *
                    A[1+1 * LDA + o_a] = ONE;
                    A_1 = 1 * LDA + o_a;
                    for (I = 2; I <= N; I++)
                    {
                        A[I + A_1] = ZERO;
                    }
                    for (J = 2; J <= N; J++)
                    {
                        A_J = J * LDA + o_a;
                        for (I = J - 1; I >= 2; I +=  - 1)
                        {
                            A[I + A_J] = A[I - 1 + A_J];
                        }
                        A[1+J * LDA + o_a] = ZERO;
                    }
                    if (N > 1)
                    {
                        // *
                        // *              Form P'(2:n,2:n)
                        // *
                        this._dorglq.Run(N - 1, N - 1, N - 1, ref A, 2+2 * LDA + o_a, LDA, TAU, offset_tau
                                         , ref WORK, offset_work, LWORK, ref IINFO);
                    }
                }
            }
            WORK[1 + o_work] = LWKOPT;
            return;
            // *
            // *     End of DORGBR
            // *

            #endregion

        }
    }
}
