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
    /// DGEBRD reduces a general real M-by-N matrix A to upper or lower
    /// bidiagonal form B by an orthogonal transformation: Q**T * A * P = B.
    /// 
    /// If m .GE. n, B is upper bidiagonal; if m .LT. n, B is lower bidiagonal.
    /// 
    ///</summary>
    public class DGEBRD
    {
    

        #region Dependencies
        
        DGEBD2 _dgebd2; DGEMM _dgemm; DLABRD _dlabrd; XERBLA _xerbla; ILAENV _ilaenv; 

        #endregion


        #region Variables
        
        const double ONE = 1.0E+0; 

        #endregion

        public DGEBRD(DGEBD2 dgebd2, DGEMM dgemm, DLABRD dlabrd, XERBLA xerbla, ILAENV ilaenv)
        {
    

            #region Set Dependencies
            
            this._dgebd2 = dgebd2; this._dgemm = dgemm; this._dlabrd = dlabrd; this._xerbla = xerbla; this._ilaenv = ilaenv; 

            #endregion

        }
    
        public DGEBRD()
        {
    

            #region Dependencies (Initialization)
            
            LSAME lsame = new LSAME();
            XERBLA xerbla = new XERBLA();
            DLAMC3 dlamc3 = new DLAMC3();
            DLAPY2 dlapy2 = new DLAPY2();
            DNRM2 dnrm2 = new DNRM2();
            DSCAL dscal = new DSCAL();
            IEEECK ieeeck = new IEEECK();
            IPARMQ iparmq = new IPARMQ();
            DGEMV dgemv = new DGEMV(lsame, xerbla);
            DGER dger = new DGER(xerbla);
            DLARF dlarf = new DLARF(dgemv, dger, lsame);
            DLAMC1 dlamc1 = new DLAMC1(dlamc3);
            DLAMC4 dlamc4 = new DLAMC4(dlamc3);
            DLAMC5 dlamc5 = new DLAMC5(dlamc3);
            DLAMC2 dlamc2 = new DLAMC2(dlamc3, dlamc1, dlamc4, dlamc5);
            DLAMCH dlamch = new DLAMCH(lsame, dlamc2);
            DLARFG dlarfg = new DLARFG(dlamch, dlapy2, dnrm2, dscal);
            DGEBD2 dgebd2 = new DGEBD2(dlarf, dlarfg, xerbla);
            DGEMM dgemm = new DGEMM(lsame, xerbla);
            DLABRD dlabrd = new DLABRD(dgemv, dlarfg, dscal);
            ILAENV ilaenv = new ILAENV(ieeeck, iparmq);

            #endregion


            #region Set Dependencies
            
            this._dgebd2 = dgebd2; this._dgemm = dgemm; this._dlabrd = dlabrd; this._xerbla = xerbla; this._ilaenv = ilaenv; 

            #endregion

        }
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DGEBRD reduces a general real M-by-N matrix A to upper or lower
        /// bidiagonal form B by an orthogonal transformation: Q**T * A * P = B.
        /// 
        /// If m .GE. n, B is upper bidiagonal; if m .LT. n, B is lower bidiagonal.
        /// 
        ///</summary>
        /// <param name="M">
        /// (input) INTEGER
        /// The number of rows in the matrix A.  M .GE. 0.
        ///</param>
        /// <param name="N">
        /// (input) INTEGER
        /// The number of columns in the matrix A.  N .GE. 0.
        ///</param>
        /// <param name="A">
        /// (input/output) DOUBLE PRECISION array, dimension (LDA,N)
        /// On entry, the M-by-N general matrix to be reduced.
        /// On exit,
        /// if m .GE. n, the diagonal and the first superdiagonal are
        /// overwritten with the upper bidiagonal matrix B; the
        /// elements below the diagonal, with the array TAUQ, represent
        /// the orthogonal matrix Q as a product of elementary
        /// reflectors, and the elements above the first superdiagonal,
        /// with the array TAUP, represent the orthogonal matrix P as
        /// a product of elementary reflectors;
        /// if m .LT. n, the diagonal and the first subdiagonal are
        /// overwritten with the lower bidiagonal matrix B; the
        /// elements below the first subdiagonal, with the array TAUQ,
        /// represent the orthogonal matrix Q as a product of
        /// elementary reflectors, and the elements above the diagonal,
        /// with the array TAUP, represent the orthogonal matrix P as
        /// a product of elementary reflectors.
        /// See Further Details.
        ///</param>
        /// <param name="LDA">
        /// (input) INTEGER
        /// The leading dimension of the array A.  LDA .GE. max(1,M).
        ///</param>
        /// <param name="D">
        /// (output) DOUBLE PRECISION array, dimension (min(M,N))
        /// The diagonal elements of the bidiagonal matrix B:
        /// D(i) = A(i,i).
        ///</param>
        /// <param name="E">
        /// (output) DOUBLE PRECISION array, dimension (min(M,N)-1)
        /// The off-diagonal elements of the bidiagonal matrix B:
        /// if m .GE. n, E(i) = A(i,i+1) for i = 1,2,...,n-1;
        /// if m .LT. n, E(i) = A(i+1,i) for i = 1,2,...,m-1.
        ///</param>
        /// <param name="TAUQ">
        /// (output) DOUBLE PRECISION array dimension (min(M,N))
        /// The scalar factors of the elementary reflectors which
        /// represent the orthogonal matrix Q. See Further Details.
        ///</param>
        /// <param name="TAUP">
        /// (output) DOUBLE PRECISION array, dimension (min(M,N))
        /// The scalar factors of the elementary reflectors which
        /// represent the orthogonal matrix P. See Further Details.
        ///</param>
        /// <param name="WORK">
        /// (workspace/output) DOUBLE PRECISION array, dimension (MAX(1,LWORK))
        /// On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
        ///</param>
        /// <param name="LWORK">
        /// (input) INTEGER
        /// The length of the array WORK.  LWORK .GE. max(1,M,N).
        /// For optimum performance LWORK .GE. (M+N)*NB, where NB
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
        /// .LT. 0:  if INFO = -i, the i-th argument had an illegal value.
        ///</param>
        public void Run(int M, int N, ref double[] A, int offset_a, int LDA, ref double[] D, int offset_d, ref double[] E, int offset_e
                         , ref double[] TAUQ, int offset_tauq, ref double[] TAUP, int offset_taup, ref double[] WORK, int offset_work, int LWORK, ref int INFO)
        {

            #region Variables
            
            bool LQUERY = false; int I = 0; int IINFO = 0; int J = 0; int LDWRKX = 0; int LDWRKY = 0; int LWKOPT = 0; 
            int MINMN = 0;int NB = 0; int NBMIN = 0; int NX = 0; double WS = 0; 

            #endregion


            #region Array Index Correction
            
             int o_a = -1 - LDA + offset_a;  int o_d = -1 + offset_d;  int o_e = -1 + offset_e;  int o_tauq = -1 + offset_tauq; 
             int o_taup = -1 + offset_taup; int o_work = -1 + offset_work; 

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
            // *  DGEBRD reduces a general real M-by-N matrix A to upper or lower
            // *  bidiagonal form B by an orthogonal transformation: Q**T * A * P = B.
            // *
            // *  If m >= n, B is upper bidiagonal; if m < n, B is lower bidiagonal.
            // *
            // *  Arguments
            // *  =========
            // *
            // *  M       (input) INTEGER
            // *          The number of rows in the matrix A.  M >= 0.
            // *
            // *  N       (input) INTEGER
            // *          The number of columns in the matrix A.  N >= 0.
            // *
            // *  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
            // *          On entry, the M-by-N general matrix to be reduced.
            // *          On exit,
            // *          if m >= n, the diagonal and the first superdiagonal are
            // *            overwritten with the upper bidiagonal matrix B; the
            // *            elements below the diagonal, with the array TAUQ, represent
            // *            the orthogonal matrix Q as a product of elementary
            // *            reflectors, and the elements above the first superdiagonal,
            // *            with the array TAUP, represent the orthogonal matrix P as
            // *            a product of elementary reflectors;
            // *          if m < n, the diagonal and the first subdiagonal are
            // *            overwritten with the lower bidiagonal matrix B; the
            // *            elements below the first subdiagonal, with the array TAUQ,
            // *            represent the orthogonal matrix Q as a product of
            // *            elementary reflectors, and the elements above the diagonal,
            // *            with the array TAUP, represent the orthogonal matrix P as
            // *            a product of elementary reflectors.
            // *          See Further Details.
            // *
            // *  LDA     (input) INTEGER
            // *          The leading dimension of the array A.  LDA >= max(1,M).
            // *
            // *  D       (output) DOUBLE PRECISION array, dimension (min(M,N))
            // *          The diagonal elements of the bidiagonal matrix B:
            // *          D(i) = A(i,i).
            // *
            // *  E       (output) DOUBLE PRECISION array, dimension (min(M,N)-1)
            // *          The off-diagonal elements of the bidiagonal matrix B:
            // *          if m >= n, E(i) = A(i,i+1) for i = 1,2,...,n-1;
            // *          if m < n, E(i) = A(i+1,i) for i = 1,2,...,m-1.
            // *
            // *  TAUQ    (output) DOUBLE PRECISION array dimension (min(M,N))
            // *          The scalar factors of the elementary reflectors which
            // *          represent the orthogonal matrix Q. See Further Details.
            // *
            // *  TAUP    (output) DOUBLE PRECISION array, dimension (min(M,N))
            // *          The scalar factors of the elementary reflectors which
            // *          represent the orthogonal matrix P. See Further Details.
            // *
            // *  WORK    (workspace/output) DOUBLE PRECISION array, dimension (MAX(1,LWORK))
            // *          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
            // *
            // *  LWORK   (input) INTEGER
            // *          The length of the array WORK.  LWORK >= max(1,M,N).
            // *          For optimum performance LWORK >= (M+N)*NB, where NB
            // *          is the optimal blocksize.
            // *
            // *          If LWORK = -1, then a workspace query is assumed; the routine
            // *          only calculates the optimal size of the WORK array, returns
            // *          this value as the first entry of the WORK array, and no error
            // *          message related to LWORK is issued by XERBLA.
            // *
            // *  INFO    (output) INTEGER
            // *          = 0:  successful exit
            // *          < 0:  if INFO = -i, the i-th argument had an illegal value.
            // *
            // *  Further Details
            // *  ===============
            // *
            // *  The matrices Q and P are represented as products of elementary
            // *  reflectors:
            // *
            // *  If m >= n,
            // *
            // *     Q = H(1) H(2) . . . H(n)  and  P = G(1) G(2) . . . G(n-1)
            // *
            // *  Each H(i) and G(i) has the form:
            // *
            // *     H(i) = I - tauq * v * v'  and G(i) = I - taup * u * u'
            // *
            // *  where tauq and taup are real scalars, and v and u are real vectors;
            // *  v(1:i-1) = 0, v(i) = 1, and v(i+1:m) is stored on exit in A(i+1:m,i);
            // *  u(1:i) = 0, u(i+1) = 1, and u(i+2:n) is stored on exit in A(i,i+2:n);
            // *  tauq is stored in TAUQ(i) and taup in TAUP(i).
            // *
            // *  If m < n,
            // *
            // *     Q = H(1) H(2) . . . H(m-1)  and  P = G(1) G(2) . . . G(m)
            // *
            // *  Each H(i) and G(i) has the form:
            // *
            // *     H(i) = I - tauq * v * v'  and G(i) = I - taup * u * u'
            // *
            // *  where tauq and taup are real scalars, and v and u are real vectors;
            // *  v(1:i) = 0, v(i+1) = 1, and v(i+2:m) is stored on exit in A(i+2:m,i);
            // *  u(1:i-1) = 0, u(i) = 1, and u(i+1:n) is stored on exit in A(i,i+1:n);
            // *  tauq is stored in TAUQ(i) and taup in TAUP(i).
            // *
            // *  The contents of A on exit are illustrated by the following examples:
            // *
            // *  m = 6 and n = 5 (m > n):          m = 5 and n = 6 (m < n):
            // *
            // *    (  d   e   u1  u1  u1 )           (  d   u1  u1  u1  u1  u1 )
            // *    (  v1  d   e   u2  u2 )           (  e   d   u2  u2  u2  u2 )
            // *    (  v1  v2  d   e   u3 )           (  v1  e   d   u3  u3  u3 )
            // *    (  v1  v2  v3  d   e  )           (  v1  v2  e   d   u4  u4 )
            // *    (  v1  v2  v3  v4  d  )           (  v1  v2  v3  e   d   u5 )
            // *    (  v1  v2  v3  v4  v5 )
            // *
            // *  where d and e denote diagonal and off-diagonal elements of B, vi
            // *  denotes an element of the vector defining H(i), and ui an element of
            // *  the vector defining G(i).
            // *
            // *  =====================================================================
            // *
            // *     .. Parameters ..
            // *     ..
            // *     .. Local Scalars ..
            // *     ..
            // *     .. External Subroutines ..
            // *     ..
            // *     .. Intrinsic Functions ..
            //      INTRINSIC          DBLE, MAX, MIN;
            // *     ..
            // *     .. External Functions ..
            // *     ..
            // *     .. Executable Statements ..
            // *
            // *     Test the input parameters
            // *

            #endregion


            #region Body
            
            INFO = 0;
            NB = Math.Max(1, this._ilaenv.Run(1, "DGEBRD", " ", M, N,  - 1,  - 1));
            LWKOPT = (M + N) * NB;
            WORK[1 + o_work] = Convert.ToDouble(LWKOPT);
            LQUERY = (LWORK ==  - 1);
            if (M < 0)
            {
                INFO =  - 1;
            }
            else
            {
                if (N < 0)
                {
                    INFO =  - 2;
                }
                else
                {
                    if (LDA < Math.Max(1, M))
                    {
                        INFO =  - 4;
                    }
                    else
                    {
                        if (LWORK < Math.Max(1, Math.Max(M, N)) && !LQUERY)
                        {
                            INFO =  - 10;
                        }
                    }
                }
            }
            if (INFO < 0)
            {
                this._xerbla.Run("DGEBRD",  - INFO);
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
            MINMN = Math.Min(M, N);
            if (MINMN == 0)
            {
                WORK[1 + o_work] = 1;
                return;
            }
            // *
            WS = Math.Max(M, N);
            LDWRKX = M;
            LDWRKY = N;
            // *
            if (NB > 1 && NB < MINMN)
            {
                // *
                // *        Set the crossover point NX.
                // *
                NX = Math.Max(NB, this._ilaenv.Run(3, "DGEBRD", " ", M, N,  - 1,  - 1));
                // *
                // *        Determine when to switch from blocked to unblocked code.
                // *
                if (NX < MINMN)
                {
                    WS = (M + N) * NB;
                    if (LWORK < WS)
                    {
                        // *
                        // *              Not enough work space for the optimal NB, consider using
                        // *              a smaller block size.
                        // *
                        NBMIN = this._ilaenv.Run(2, "DGEBRD", " ", M, N,  - 1,  - 1);
                        if (LWORK >= (M + N) * NBMIN)
                        {
                            NB = LWORK / (M + N);
                        }
                        else
                        {
                            NB = 1;
                            NX = MINMN;
                        }
                    }
                }
            }
            else
            {
                NX = MINMN;
            }
            // *
            for (I = 1; (NB >= 0) ? (I <= MINMN - NX) : (I >= MINMN - NX); I += NB)
            {
                // *
                // *        Reduce rows and columns i:i+nb-1 to bidiagonal form and return
                // *        the matrices X and Y which are needed to update the unreduced
                // *        part of the matrix
                // *
                this._dlabrd.Run(M - I + 1, N - I + 1, NB, ref A, I+I * LDA + o_a, LDA, ref D, I + o_d
                                 , ref E, I + o_e, ref TAUQ, I + o_tauq, ref TAUP, I + o_taup, ref WORK, offset_work, LDWRKX, ref WORK, LDWRKX * NB + 1 + o_work
                                 , LDWRKY);
                // *
                // *        Update the trailing submatrix A(i+nb:m,i+nb:n), using an update
                // *        of the form  A := A - V*Y' - X*U'
                // *
                this._dgemm.Run("No transpose", "Transpose", M - I - NB + 1, N - I - NB + 1, NB,  - ONE
                                , A, I + NB+I * LDA + o_a, LDA, WORK, LDWRKX * NB + NB + 1 + o_work, LDWRKY, ONE, ref A, I + NB+(I + NB) * LDA + o_a
                                , LDA);
                this._dgemm.Run("No transpose", "No transpose", M - I - NB + 1, N - I - NB + 1, NB,  - ONE
                                , WORK, NB + 1 + o_work, LDWRKX, A, I+(I + NB) * LDA + o_a, LDA, ONE, ref A, I + NB+(I + NB) * LDA + o_a
                                , LDA);
                // *
                // *        Copy diagonal and off-diagonal elements of B back into A
                // *
                if (M >= N)
                {
                    for (J = I; J <= I + NB - 1; J++)
                    {
                        A[J+J * LDA + o_a] = D[J + o_d];
                        A[J+(J + 1) * LDA + o_a] = E[J + o_e];
                    }
                }
                else
                {
                    for (J = I; J <= I + NB - 1; J++)
                    {
                        A[J+J * LDA + o_a] = D[J + o_d];
                        A[J + 1+J * LDA + o_a] = E[J + o_e];
                    }
                }
            }
            // *
            // *     Use unblocked code to reduce the remainder of the matrix
            // *
            this._dgebd2.Run(M - I + 1, N - I + 1, ref A, I+I * LDA + o_a, LDA, ref D, I + o_d, ref E, I + o_e
                             , ref TAUQ, I + o_tauq, ref TAUP, I + o_taup, ref WORK, offset_work, ref IINFO);
            WORK[1 + o_work] = WS;
            return;
            // *
            // *     End of DGEBRD
            // *

            #endregion

        }
    }
}
