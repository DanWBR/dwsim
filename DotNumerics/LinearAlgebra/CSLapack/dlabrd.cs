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
    /// DLABRD reduces the first NB rows and columns of a real general
    /// m by n matrix A to upper or lower bidiagonal form by an orthogonal
    /// transformation Q' * A * P, and returns the matrices X and Y which
    /// are needed to apply the transformation to the unreduced part of A.
    /// 
    /// If m .GE. n, A is reduced to upper bidiagonal form; if m .LT. n, to lower
    /// bidiagonal form.
    /// 
    /// This is an auxiliary routine called by DGEBRD
    /// 
    ///</summary>
    public class DLABRD
    {
    

        #region Dependencies
        
        DGEMV _dgemv; DLARFG _dlarfg; DSCAL _dscal; 

        #endregion


        #region Variables
        
        const double ZERO = 0.0E0; const double ONE = 1.0E0; 

        #endregion

        public DLABRD(DGEMV dgemv, DLARFG dlarfg, DSCAL dscal)
        {
    

            #region Set Dependencies
            
            this._dgemv = dgemv; this._dlarfg = dlarfg; this._dscal = dscal; 

            #endregion

        }
    
        public DLABRD()
        {
    

            #region Dependencies (Initialization)
            
            LSAME lsame = new LSAME();
            XERBLA xerbla = new XERBLA();
            DLAMC3 dlamc3 = new DLAMC3();
            DLAPY2 dlapy2 = new DLAPY2();
            DNRM2 dnrm2 = new DNRM2();
            DSCAL dscal = new DSCAL();
            DGEMV dgemv = new DGEMV(lsame, xerbla);
            DLAMC1 dlamc1 = new DLAMC1(dlamc3);
            DLAMC4 dlamc4 = new DLAMC4(dlamc3);
            DLAMC5 dlamc5 = new DLAMC5(dlamc3);
            DLAMC2 dlamc2 = new DLAMC2(dlamc3, dlamc1, dlamc4, dlamc5);
            DLAMCH dlamch = new DLAMCH(lsame, dlamc2);
            DLARFG dlarfg = new DLARFG(dlamch, dlapy2, dnrm2, dscal);

            #endregion


            #region Set Dependencies
            
            this._dgemv = dgemv; this._dlarfg = dlarfg; this._dscal = dscal; 

            #endregion

        }
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DLABRD reduces the first NB rows and columns of a real general
        /// m by n matrix A to upper or lower bidiagonal form by an orthogonal
        /// transformation Q' * A * P, and returns the matrices X and Y which
        /// are needed to apply the transformation to the unreduced part of A.
        /// 
        /// If m .GE. n, A is reduced to upper bidiagonal form; if m .LT. n, to lower
        /// bidiagonal form.
        /// 
        /// This is an auxiliary routine called by DGEBRD
        /// 
        ///</summary>
        /// <param name="M">
        /// (input) INTEGER
        /// The number of rows in the matrix A.
        ///</param>
        /// <param name="N">
        /// (input) INTEGER
        /// The number of columns in the matrix A.
        ///</param>
        /// <param name="NB">
        /// (input) INTEGER
        /// The number of leading rows and columns of A to be reduced.
        ///</param>
        /// <param name="A">
        /// (input/output) DOUBLE PRECISION array, dimension (LDA,N)
        /// On entry, the m by n general matrix to be reduced.
        /// On exit, the first NB rows and columns of the matrix are
        /// overwritten; the rest of the array is unchanged.
        /// If m .GE. n, elements on and below the diagonal in the first NB
        /// columns, with the array TAUQ, represent the orthogonal
        /// matrix Q as a product of elementary reflectors; and
        /// elements above the diagonal in the first NB rows, with the
        /// array TAUP, represent the orthogonal matrix P as a product
        /// of elementary reflectors.
        /// If m .LT. n, elements below the diagonal in the first NB
        /// columns, with the array TAUQ, represent the orthogonal
        /// matrix Q as a product of elementary reflectors, and
        /// elements on and above the diagonal in the first NB rows,
        /// with the array TAUP, represent the orthogonal matrix P as
        /// a product of elementary reflectors.
        /// See Further Details.
        ///</param>
        /// <param name="LDA">
        /// (input) INTEGER
        /// The leading dimension of the array A.  LDA .GE. max(1,M).
        ///</param>
        /// <param name="D">
        /// (output) DOUBLE PRECISION array, dimension (NB)
        /// The diagonal elements of the first NB rows and columns of
        /// the reduced matrix.  D(i) = A(i,i).
        ///</param>
        /// <param name="E">
        /// (output) DOUBLE PRECISION array, dimension (NB)
        /// The off-diagonal elements of the first NB rows and columns of
        /// the reduced matrix.
        ///</param>
        /// <param name="TAUQ">
        /// (output) DOUBLE PRECISION array dimension (NB)
        /// The scalar factors of the elementary reflectors which
        /// represent the orthogonal matrix Q. See Further Details.
        ///</param>
        /// <param name="TAUP">
        /// (output) DOUBLE PRECISION array, dimension (NB)
        /// The scalar factors of the elementary reflectors which
        /// represent the orthogonal matrix P. See Further Details.
        ///</param>
        /// <param name="X">
        /// (output) DOUBLE PRECISION array, dimension (LDX,NB)
        /// The m-by-nb matrix X required to update the unreduced part
        /// of A.
        ///</param>
        /// <param name="LDX">
        /// (input) INTEGER
        /// The leading dimension of the array X. LDX .GE. M.
        ///</param>
        /// <param name="Y">
        /// (output) DOUBLE PRECISION array, dimension (LDY,NB)
        /// The n-by-nb matrix Y required to update the unreduced part
        /// of A.
        ///</param>
        /// <param name="LDY">
        /// (input) INTEGER
        /// The leading dimension of the array Y. LDY .GE. N.
        ///</param>
        public void Run(int M, int N, int NB, ref double[] A, int offset_a, int LDA, ref double[] D, int offset_d
                         , ref double[] E, int offset_e, ref double[] TAUQ, int offset_tauq, ref double[] TAUP, int offset_taup, ref double[] X, int offset_x, int LDX, ref double[] Y, int offset_y
                         , int LDY)
        {

            #region Variables
            
            int I = 0; 

            #endregion


            #region Array Index Correction
            
             int o_a = -1 - LDA + offset_a;  int o_d = -1 + offset_d;  int o_e = -1 + offset_e;  int o_tauq = -1 + offset_tauq; 
             int o_taup = -1 + offset_taup; int o_x = -1 - LDX + offset_x;  int o_y = -1 - LDY + offset_y; 

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
            // *  DLABRD reduces the first NB rows and columns of a real general
            // *  m by n matrix A to upper or lower bidiagonal form by an orthogonal
            // *  transformation Q' * A * P, and returns the matrices X and Y which
            // *  are needed to apply the transformation to the unreduced part of A.
            // *
            // *  If m >= n, A is reduced to upper bidiagonal form; if m < n, to lower
            // *  bidiagonal form.
            // *
            // *  This is an auxiliary routine called by DGEBRD
            // *
            // *  Arguments
            // *  =========
            // *
            // *  M       (input) INTEGER
            // *          The number of rows in the matrix A.
            // *
            // *  N       (input) INTEGER
            // *          The number of columns in the matrix A.
            // *
            // *  NB      (input) INTEGER
            // *          The number of leading rows and columns of A to be reduced.
            // *
            // *  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
            // *          On entry, the m by n general matrix to be reduced.
            // *          On exit, the first NB rows and columns of the matrix are
            // *          overwritten; the rest of the array is unchanged.
            // *          If m >= n, elements on and below the diagonal in the first NB
            // *            columns, with the array TAUQ, represent the orthogonal
            // *            matrix Q as a product of elementary reflectors; and
            // *            elements above the diagonal in the first NB rows, with the
            // *            array TAUP, represent the orthogonal matrix P as a product
            // *            of elementary reflectors.
            // *          If m < n, elements below the diagonal in the first NB
            // *            columns, with the array TAUQ, represent the orthogonal
            // *            matrix Q as a product of elementary reflectors, and
            // *            elements on and above the diagonal in the first NB rows,
            // *            with the array TAUP, represent the orthogonal matrix P as
            // *            a product of elementary reflectors.
            // *          See Further Details.
            // *
            // *  LDA     (input) INTEGER
            // *          The leading dimension of the array A.  LDA >= max(1,M).
            // *
            // *  D       (output) DOUBLE PRECISION array, dimension (NB)
            // *          The diagonal elements of the first NB rows and columns of
            // *          the reduced matrix.  D(i) = A(i,i).
            // *
            // *  E       (output) DOUBLE PRECISION array, dimension (NB)
            // *          The off-diagonal elements of the first NB rows and columns of
            // *          the reduced matrix.
            // *
            // *  TAUQ    (output) DOUBLE PRECISION array dimension (NB)
            // *          The scalar factors of the elementary reflectors which
            // *          represent the orthogonal matrix Q. See Further Details.
            // *
            // *  TAUP    (output) DOUBLE PRECISION array, dimension (NB)
            // *          The scalar factors of the elementary reflectors which
            // *          represent the orthogonal matrix P. See Further Details.
            // *
            // *  X       (output) DOUBLE PRECISION array, dimension (LDX,NB)
            // *          The m-by-nb matrix X required to update the unreduced part
            // *          of A.
            // *
            // *  LDX     (input) INTEGER
            // *          The leading dimension of the array X. LDX >= M.
            // *
            // *  Y       (output) DOUBLE PRECISION array, dimension (LDY,NB)
            // *          The n-by-nb matrix Y required to update the unreduced part
            // *          of A.
            // *
            // *  LDY     (input) INTEGER
            // *          The leading dimension of the array Y. LDY >= N.
            // *
            // *  Further Details
            // *  ===============
            // *
            // *  The matrices Q and P are represented as products of elementary
            // *  reflectors:
            // *
            // *     Q = H(1) H(2) . . . H(nb)  and  P = G(1) G(2) . . . G(nb)
            // *
            // *  Each H(i) and G(i) has the form:
            // *
            // *     H(i) = I - tauq * v * v'  and G(i) = I - taup * u * u'
            // *
            // *  where tauq and taup are real scalars, and v and u are real vectors.
            // *
            // *  If m >= n, v(1:i-1) = 0, v(i) = 1, and v(i:m) is stored on exit in
            // *  A(i:m,i); u(1:i) = 0, u(i+1) = 1, and u(i+1:n) is stored on exit in
            // *  A(i,i+1:n); tauq is stored in TAUQ(i) and taup in TAUP(i).
            // *
            // *  If m < n, v(1:i) = 0, v(i+1) = 1, and v(i+1:m) is stored on exit in
            // *  A(i+2:m,i); u(1:i-1) = 0, u(i) = 1, and u(i:n) is stored on exit in
            // *  A(i,i+1:n); tauq is stored in TAUQ(i) and taup in TAUP(i).
            // *
            // *  The elements of the vectors v and u together form the m-by-nb matrix
            // *  V and the nb-by-n matrix U' which are needed, with X and Y, to apply
            // *  the transformation to the unreduced part of the matrix, using a block
            // *  update of the form:  A := A - V*Y' - X*U'.
            // *
            // *  The contents of A on exit are illustrated by the following examples
            // *  with nb = 2:
            // *
            // *  m = 6 and n = 5 (m > n):          m = 5 and n = 6 (m < n):
            // *
            // *    (  1   1   u1  u1  u1 )           (  1   u1  u1  u1  u1  u1 )
            // *    (  v1  1   1   u2  u2 )           (  1   1   u2  u2  u2  u2 )
            // *    (  v1  v2  a   a   a  )           (  v1  1   a   a   a   a  )
            // *    (  v1  v2  a   a   a  )           (  v1  v2  a   a   a   a  )
            // *    (  v1  v2  a   a   a  )           (  v1  v2  a   a   a   a  )
            // *    (  v1  v2  a   a   a  )
            // *
            // *  where a denotes an element of the original matrix which is unchanged,
            // *  vi denotes an element of the vector defining H(i), and ui an element
            // *  of the vector defining G(i).
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
            //      INTRINSIC          MIN;
            // *     ..
            // *     .. Executable Statements ..
            // *
            // *     Quick return if possible
            // *

            #endregion


            #region Body
            
            if (M <= 0 || N <= 0) return;
            // *
            if (M >= N)
            {
                // *
                // *        Reduce to upper bidiagonal form
                // *
                for (I = 1; I <= NB; I++)
                {
                    // *
                    // *           Update A(i:m,i)
                    // *
                    this._dgemv.Run("No transpose", M - I + 1, I - 1,  - ONE, A, I+1 * LDA + o_a, LDA
                                    , Y, I+1 * LDY + o_y, LDY, ONE, ref A, I+I * LDA + o_a, 1);
                    this._dgemv.Run("No transpose", M - I + 1, I - 1,  - ONE, X, I+1 * LDX + o_x, LDX
                                    , A, 1+I * LDA + o_a, 1, ONE, ref A, I+I * LDA + o_a, 1);
                    // *
                    // *           Generate reflection Q(i) to annihilate A(i+1:m,i)
                    // *
                    this._dlarfg.Run(M - I + 1, ref A[I+I * LDA + o_a], ref A, Math.Min(I + 1, M)+I * LDA + o_a, 1, ref TAUQ[I + o_tauq]);
                    D[I + o_d] = A[I+I * LDA + o_a];
                    if (I < N)
                    {
                        A[I+I * LDA + o_a] = ONE;
                        // *
                        // *              Compute Y(i+1:n,i)
                        // *
                        this._dgemv.Run("Transpose", M - I + 1, N - I, ONE, A, I+(I + 1) * LDA + o_a, LDA
                                        , A, I+I * LDA + o_a, 1, ZERO, ref Y, I + 1+I * LDY + o_y, 1);
                        this._dgemv.Run("Transpose", M - I + 1, I - 1, ONE, A, I+1 * LDA + o_a, LDA
                                        , A, I+I * LDA + o_a, 1, ZERO, ref Y, 1+I * LDY + o_y, 1);
                        this._dgemv.Run("No transpose", N - I, I - 1,  - ONE, Y, I + 1+1 * LDY + o_y, LDY
                                        , Y, 1+I * LDY + o_y, 1, ONE, ref Y, I + 1+I * LDY + o_y, 1);
                        this._dgemv.Run("Transpose", M - I + 1, I - 1, ONE, X, I+1 * LDX + o_x, LDX
                                        , A, I+I * LDA + o_a, 1, ZERO, ref Y, 1+I * LDY + o_y, 1);
                        this._dgemv.Run("Transpose", I - 1, N - I,  - ONE, A, 1+(I + 1) * LDA + o_a, LDA
                                        , Y, 1+I * LDY + o_y, 1, ONE, ref Y, I + 1+I * LDY + o_y, 1);
                        this._dscal.Run(N - I, TAUQ[I + o_tauq], ref Y, I + 1+I * LDY + o_y, 1);
                        // *
                        // *              Update A(i,i+1:n)
                        // *
                        this._dgemv.Run("No transpose", N - I, I,  - ONE, Y, I + 1+1 * LDY + o_y, LDY
                                        , A, I+1 * LDA + o_a, LDA, ONE, ref A, I+(I + 1) * LDA + o_a, LDA);
                        this._dgemv.Run("Transpose", I - 1, N - I,  - ONE, A, 1+(I + 1) * LDA + o_a, LDA
                                        , X, I+1 * LDX + o_x, LDX, ONE, ref A, I+(I + 1) * LDA + o_a, LDA);
                        // *
                        // *              Generate reflection P(i) to annihilate A(i,i+2:n)
                        // *
                        this._dlarfg.Run(N - I, ref A[I+(I + 1) * LDA + o_a], ref A, I+Math.Min(I + 2, N) * LDA + o_a, LDA, ref TAUP[I + o_taup]);
                        E[I + o_e] = A[I+(I + 1) * LDA + o_a];
                        A[I+(I + 1) * LDA + o_a] = ONE;
                        // *
                        // *              Compute X(i+1:m,i)
                        // *
                        this._dgemv.Run("No transpose", M - I, N - I, ONE, A, I + 1+(I + 1) * LDA + o_a, LDA
                                        , A, I+(I + 1) * LDA + o_a, LDA, ZERO, ref X, I + 1+I * LDX + o_x, 1);
                        this._dgemv.Run("Transpose", N - I, I, ONE, Y, I + 1+1 * LDY + o_y, LDY
                                        , A, I+(I + 1) * LDA + o_a, LDA, ZERO, ref X, 1+I * LDX + o_x, 1);
                        this._dgemv.Run("No transpose", M - I, I,  - ONE, A, I + 1+1 * LDA + o_a, LDA
                                        , X, 1+I * LDX + o_x, 1, ONE, ref X, I + 1+I * LDX + o_x, 1);
                        this._dgemv.Run("No transpose", I - 1, N - I, ONE, A, 1+(I + 1) * LDA + o_a, LDA
                                        , A, I+(I + 1) * LDA + o_a, LDA, ZERO, ref X, 1+I * LDX + o_x, 1);
                        this._dgemv.Run("No transpose", M - I, I - 1,  - ONE, X, I + 1+1 * LDX + o_x, LDX
                                        , X, 1+I * LDX + o_x, 1, ONE, ref X, I + 1+I * LDX + o_x, 1);
                        this._dscal.Run(M - I, TAUP[I + o_taup], ref X, I + 1+I * LDX + o_x, 1);
                    }
                }
            }
            else
            {
                // *
                // *        Reduce to lower bidiagonal form
                // *
                for (I = 1; I <= NB; I++)
                {
                    // *
                    // *           Update A(i,i:n)
                    // *
                    this._dgemv.Run("No transpose", N - I + 1, I - 1,  - ONE, Y, I+1 * LDY + o_y, LDY
                                    , A, I+1 * LDA + o_a, LDA, ONE, ref A, I+I * LDA + o_a, LDA);
                    this._dgemv.Run("Transpose", I - 1, N - I + 1,  - ONE, A, 1+I * LDA + o_a, LDA
                                    , X, I+1 * LDX + o_x, LDX, ONE, ref A, I+I * LDA + o_a, LDA);
                    // *
                    // *           Generate reflection P(i) to annihilate A(i,i+1:n)
                    // *
                    this._dlarfg.Run(N - I + 1, ref A[I+I * LDA + o_a], ref A, I+Math.Min(I + 1, N) * LDA + o_a, LDA, ref TAUP[I + o_taup]);
                    D[I + o_d] = A[I+I * LDA + o_a];
                    if (I < M)
                    {
                        A[I+I * LDA + o_a] = ONE;
                        // *
                        // *              Compute X(i+1:m,i)
                        // *
                        this._dgemv.Run("No transpose", M - I, N - I + 1, ONE, A, I + 1+I * LDA + o_a, LDA
                                        , A, I+I * LDA + o_a, LDA, ZERO, ref X, I + 1+I * LDX + o_x, 1);
                        this._dgemv.Run("Transpose", N - I + 1, I - 1, ONE, Y, I+1 * LDY + o_y, LDY
                                        , A, I+I * LDA + o_a, LDA, ZERO, ref X, 1+I * LDX + o_x, 1);
                        this._dgemv.Run("No transpose", M - I, I - 1,  - ONE, A, I + 1+1 * LDA + o_a, LDA
                                        , X, 1+I * LDX + o_x, 1, ONE, ref X, I + 1+I * LDX + o_x, 1);
                        this._dgemv.Run("No transpose", I - 1, N - I + 1, ONE, A, 1+I * LDA + o_a, LDA
                                        , A, I+I * LDA + o_a, LDA, ZERO, ref X, 1+I * LDX + o_x, 1);
                        this._dgemv.Run("No transpose", M - I, I - 1,  - ONE, X, I + 1+1 * LDX + o_x, LDX
                                        , X, 1+I * LDX + o_x, 1, ONE, ref X, I + 1+I * LDX + o_x, 1);
                        this._dscal.Run(M - I, TAUP[I + o_taup], ref X, I + 1+I * LDX + o_x, 1);
                        // *
                        // *              Update A(i+1:m,i)
                        // *
                        this._dgemv.Run("No transpose", M - I, I - 1,  - ONE, A, I + 1+1 * LDA + o_a, LDA
                                        , Y, I+1 * LDY + o_y, LDY, ONE, ref A, I + 1+I * LDA + o_a, 1);
                        this._dgemv.Run("No transpose", M - I, I,  - ONE, X, I + 1+1 * LDX + o_x, LDX
                                        , A, 1+I * LDA + o_a, 1, ONE, ref A, I + 1+I * LDA + o_a, 1);
                        // *
                        // *              Generate reflection Q(i) to annihilate A(i+2:m,i)
                        // *
                        this._dlarfg.Run(M - I, ref A[I + 1+I * LDA + o_a], ref A, Math.Min(I + 2, M)+I * LDA + o_a, 1, ref TAUQ[I + o_tauq]);
                        E[I + o_e] = A[I + 1+I * LDA + o_a];
                        A[I + 1+I * LDA + o_a] = ONE;
                        // *
                        // *              Compute Y(i+1:n,i)
                        // *
                        this._dgemv.Run("Transpose", M - I, N - I, ONE, A, I + 1+(I + 1) * LDA + o_a, LDA
                                        , A, I + 1+I * LDA + o_a, 1, ZERO, ref Y, I + 1+I * LDY + o_y, 1);
                        this._dgemv.Run("Transpose", M - I, I - 1, ONE, A, I + 1+1 * LDA + o_a, LDA
                                        , A, I + 1+I * LDA + o_a, 1, ZERO, ref Y, 1+I * LDY + o_y, 1);
                        this._dgemv.Run("No transpose", N - I, I - 1,  - ONE, Y, I + 1+1 * LDY + o_y, LDY
                                        , Y, 1+I * LDY + o_y, 1, ONE, ref Y, I + 1+I * LDY + o_y, 1);
                        this._dgemv.Run("Transpose", M - I, I, ONE, X, I + 1+1 * LDX + o_x, LDX
                                        , A, I + 1+I * LDA + o_a, 1, ZERO, ref Y, 1+I * LDY + o_y, 1);
                        this._dgemv.Run("Transpose", I, N - I,  - ONE, A, 1+(I + 1) * LDA + o_a, LDA
                                        , Y, 1+I * LDY + o_y, 1, ONE, ref Y, I + 1+I * LDY + o_y, 1);
                        this._dscal.Run(N - I, TAUQ[I + o_tauq], ref Y, I + 1+I * LDY + o_y, 1);
                    }
                }
            }
            return;
            // *
            // *     End of DLABRD
            // *

            #endregion

        }
    }
}
