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
    /// DGEBD2 reduces a real general m by n matrix A to upper or lower
    /// bidiagonal form B by an orthogonal transformation: Q' * A * P = B.
    /// 
    /// If m .GE. n, B is upper bidiagonal; if m .LT. n, B is lower bidiagonal.
    /// 
    ///</summary>
    public class DGEBD2
    {
    

        #region Dependencies
        
        DLARF _dlarf; DLARFG _dlarfg; XERBLA _xerbla; 

        #endregion


        #region Variables
        
        const double ZERO = 0.0E+0; const double ONE = 1.0E+0; 

        #endregion

        public DGEBD2(DLARF dlarf, DLARFG dlarfg, XERBLA xerbla)
        {
    

            #region Set Dependencies
            
            this._dlarf = dlarf; this._dlarfg = dlarfg; this._xerbla = xerbla; 

            #endregion

        }
    
        public DGEBD2()
        {
    

            #region Dependencies (Initialization)
            
            LSAME lsame = new LSAME();
            XERBLA xerbla = new XERBLA();
            DLAMC3 dlamc3 = new DLAMC3();
            DLAPY2 dlapy2 = new DLAPY2();
            DNRM2 dnrm2 = new DNRM2();
            DSCAL dscal = new DSCAL();
            DGEMV dgemv = new DGEMV(lsame, xerbla);
            DGER dger = new DGER(xerbla);
            DLARF dlarf = new DLARF(dgemv, dger, lsame);
            DLAMC1 dlamc1 = new DLAMC1(dlamc3);
            DLAMC4 dlamc4 = new DLAMC4(dlamc3);
            DLAMC5 dlamc5 = new DLAMC5(dlamc3);
            DLAMC2 dlamc2 = new DLAMC2(dlamc3, dlamc1, dlamc4, dlamc5);
            DLAMCH dlamch = new DLAMCH(lsame, dlamc2);
            DLARFG dlarfg = new DLARFG(dlamch, dlapy2, dnrm2, dscal);

            #endregion


            #region Set Dependencies
            
            this._dlarf = dlarf; this._dlarfg = dlarfg; this._xerbla = xerbla; 

            #endregion

        }
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DGEBD2 reduces a real general m by n matrix A to upper or lower
        /// bidiagonal form B by an orthogonal transformation: Q' * A * P = B.
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
        /// On entry, the m by n general matrix to be reduced.
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
        /// (workspace) DOUBLE PRECISION array, dimension (max(M,N))
        ///</param>
        /// <param name="INFO">
        /// (output) INTEGER
        /// = 0: successful exit.
        /// .LT. 0: if INFO = -i, the i-th argument had an illegal value.
        ///</param>
        public void Run(int M, int N, ref double[] A, int offset_a, int LDA, ref double[] D, int offset_d, ref double[] E, int offset_e
                         , ref double[] TAUQ, int offset_tauq, ref double[] TAUP, int offset_taup, ref double[] WORK, int offset_work, ref int INFO)
        {

            #region Variables
            
            int I = 0; 

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
            // *  DGEBD2 reduces a real general m by n matrix A to upper or lower
            // *  bidiagonal form B by an orthogonal transformation: Q' * A * P = B.
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
            // *          On entry, the m by n general matrix to be reduced.
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
            // *  WORK    (workspace) DOUBLE PRECISION array, dimension (max(M,N))
            // *
            // *  INFO    (output) INTEGER
            // *          = 0: successful exit.
            // *          < 0: if INFO = -i, the i-th argument had an illegal value.
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
            //      INTRINSIC          MAX, MIN;
            // *     ..
            // *     .. Executable Statements ..
            // *
            // *     Test the input parameters
            // *

            #endregion


            #region Body
            
            INFO = 0;
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
                }
            }
            if (INFO < 0)
            {
                this._xerbla.Run("DGEBD2",  - INFO);
                return;
            }
            // *
            if (M >= N)
            {
                // *
                // *        Reduce to upper bidiagonal form
                // *
                for (I = 1; I <= N; I++)
                {
                    // *
                    // *           Generate elementary reflector H(i) to annihilate A(i+1:m,i)
                    // *
                    this._dlarfg.Run(M - I + 1, ref A[I+I * LDA + o_a], ref A, Math.Min(I + 1, M)+I * LDA + o_a, 1, ref TAUQ[I + o_tauq]);
                    D[I + o_d] = A[I+I * LDA + o_a];
                    A[I+I * LDA + o_a] = ONE;
                    // *
                    // *           Apply H(i) to A(i:m,i+1:n) from the left
                    // *
                    if (I < N)
                    {
                        this._dlarf.Run("Left", M - I + 1, N - I, A, I+I * LDA + o_a, 1, TAUQ[I + o_tauq]
                                        , ref A, I+(I + 1) * LDA + o_a, LDA, ref WORK, offset_work);
                    }
                    A[I+I * LDA + o_a] = D[I + o_d];
                    // *
                    if (I < N)
                    {
                        // *
                        // *              Generate elementary reflector G(i) to annihilate
                        // *              A(i,i+2:n)
                        // *
                        this._dlarfg.Run(N - I, ref A[I+(I + 1) * LDA + o_a], ref A, I+Math.Min(I + 2, N) * LDA + o_a, LDA, ref TAUP[I + o_taup]);
                        E[I + o_e] = A[I+(I + 1) * LDA + o_a];
                        A[I+(I + 1) * LDA + o_a] = ONE;
                        // *
                        // *              Apply G(i) to A(i+1:m,i+1:n) from the right
                        // *
                        this._dlarf.Run("Right", M - I, N - I, A, I+(I + 1) * LDA + o_a, LDA, TAUP[I + o_taup]
                                        , ref A, I + 1+(I + 1) * LDA + o_a, LDA, ref WORK, offset_work);
                        A[I+(I + 1) * LDA + o_a] = E[I + o_e];
                    }
                    else
                    {
                        TAUP[I + o_taup] = ZERO;
                    }
                }
            }
            else
            {
                // *
                // *        Reduce to lower bidiagonal form
                // *
                for (I = 1; I <= M; I++)
                {
                    // *
                    // *           Generate elementary reflector G(i) to annihilate A(i,i+1:n)
                    // *
                    this._dlarfg.Run(N - I + 1, ref A[I+I * LDA + o_a], ref A, I+Math.Min(I + 1, N) * LDA + o_a, LDA, ref TAUP[I + o_taup]);
                    D[I + o_d] = A[I+I * LDA + o_a];
                    A[I+I * LDA + o_a] = ONE;
                    // *
                    // *           Apply G(i) to A(i+1:m,i:n) from the right
                    // *
                    if (I < M)
                    {
                        this._dlarf.Run("Right", M - I, N - I + 1, A, I+I * LDA + o_a, LDA, TAUP[I + o_taup]
                                        , ref A, I + 1+I * LDA + o_a, LDA, ref WORK, offset_work);
                    }
                    A[I+I * LDA + o_a] = D[I + o_d];
                    // *
                    if (I < M)
                    {
                        // *
                        // *              Generate elementary reflector H(i) to annihilate
                        // *              A(i+2:m,i)
                        // *
                        this._dlarfg.Run(M - I, ref A[I + 1+I * LDA + o_a], ref A, Math.Min(I + 2, M)+I * LDA + o_a, 1, ref TAUQ[I + o_tauq]);
                        E[I + o_e] = A[I + 1+I * LDA + o_a];
                        A[I + 1+I * LDA + o_a] = ONE;
                        // *
                        // *              Apply H(i) to A(i+1:m,i+1:n) from the left
                        // *
                        this._dlarf.Run("Left", M - I, N - I, A, I + 1+I * LDA + o_a, 1, TAUQ[I + o_tauq]
                                        , ref A, I + 1+(I + 1) * LDA + o_a, LDA, ref WORK, offset_work);
                        A[I + 1+I * LDA + o_a] = E[I + o_e];
                    }
                    else
                    {
                        TAUQ[I + o_tauq] = ZERO;
                    }
                }
            }
            return;
            // *
            // *     End of DGEBD2
            // *

            #endregion

        }
    }
}
