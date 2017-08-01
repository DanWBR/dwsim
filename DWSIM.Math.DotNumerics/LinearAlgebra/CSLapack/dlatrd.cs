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
    /// DLATRD reduces NB rows and columns of a real symmetric matrix A to
    /// symmetric tridiagonal form by an orthogonal similarity
    /// transformation Q' * A * Q, and returns the matrices V and W which are
    /// needed to apply the transformation to the unreduced part of A.
    /// 
    /// If UPLO = 'U', DLATRD reduces the last NB rows and columns of a
    /// matrix, of which the upper triangle is supplied;
    /// if UPLO = 'L', DLATRD reduces the first NB rows and columns of a
    /// matrix, of which the lower triangle is supplied.
    /// 
    /// This is an auxiliary routine called by DSYTRD.
    /// 
    ///</summary>
    public class DLATRD
    {
    

        #region Dependencies
        
        DAXPY _daxpy; DGEMV _dgemv; DLARFG _dlarfg; DSCAL _dscal; DSYMV _dsymv; LSAME _lsame; DDOT _ddot; 

        #endregion


        #region Variables
        
        const double ZERO = 0.0E+0; const double ONE = 1.0E+0; const double HALF = 0.5E+0; 

        #endregion

        public DLATRD(DAXPY daxpy, DGEMV dgemv, DLARFG dlarfg, DSCAL dscal, DSYMV dsymv, LSAME lsame, DDOT ddot)
        {
    

            #region Set Dependencies
            
            this._daxpy = daxpy; this._dgemv = dgemv; this._dlarfg = dlarfg; this._dscal = dscal; this._dsymv = dsymv; 
            this._lsame = lsame;this._ddot = ddot; 

            #endregion

        }
    
        public DLATRD()
        {
    

            #region Dependencies (Initialization)
            
            DAXPY daxpy = new DAXPY();
            LSAME lsame = new LSAME();
            XERBLA xerbla = new XERBLA();
            DLAMC3 dlamc3 = new DLAMC3();
            DLAPY2 dlapy2 = new DLAPY2();
            DNRM2 dnrm2 = new DNRM2();
            DSCAL dscal = new DSCAL();
            DDOT ddot = new DDOT();
            DGEMV dgemv = new DGEMV(lsame, xerbla);
            DLAMC1 dlamc1 = new DLAMC1(dlamc3);
            DLAMC4 dlamc4 = new DLAMC4(dlamc3);
            DLAMC5 dlamc5 = new DLAMC5(dlamc3);
            DLAMC2 dlamc2 = new DLAMC2(dlamc3, dlamc1, dlamc4, dlamc5);
            DLAMCH dlamch = new DLAMCH(lsame, dlamc2);
            DLARFG dlarfg = new DLARFG(dlamch, dlapy2, dnrm2, dscal);
            DSYMV dsymv = new DSYMV(lsame, xerbla);

            #endregion


            #region Set Dependencies
            
            this._daxpy = daxpy; this._dgemv = dgemv; this._dlarfg = dlarfg; this._dscal = dscal; this._dsymv = dsymv; 
            this._lsame = lsame;this._ddot = ddot; 

            #endregion

        }
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DLATRD reduces NB rows and columns of a real symmetric matrix A to
        /// symmetric tridiagonal form by an orthogonal similarity
        /// transformation Q' * A * Q, and returns the matrices V and W which are
        /// needed to apply the transformation to the unreduced part of A.
        /// 
        /// If UPLO = 'U', DLATRD reduces the last NB rows and columns of a
        /// matrix, of which the upper triangle is supplied;
        /// if UPLO = 'L', DLATRD reduces the first NB rows and columns of a
        /// matrix, of which the lower triangle is supplied.
        /// 
        /// This is an auxiliary routine called by DSYTRD.
        /// 
        ///</summary>
        /// <param name="UPLO">
        /// (input) CHARACTER*1
        /// Specifies whether the upper or lower triangular part of the
        /// symmetric matrix A is stored:
        /// = 'U': Upper triangular
        /// = 'L': Lower triangular
        ///</param>
        /// <param name="N">
        /// (input) INTEGER
        /// The order of the matrix A.
        ///</param>
        /// <param name="NB">
        /// (input) INTEGER
        /// The number of rows and columns to be reduced.
        ///</param>
        /// <param name="A">
        /// (input/output) DOUBLE PRECISION array, dimension (LDA,N)
        /// On entry, the symmetric matrix A.  If UPLO = 'U', the leading
        /// n-by-n upper triangular part of A contains the upper
        /// triangular part of the matrix A, and the strictly lower
        /// triangular part of A is not referenced.  If UPLO = 'L', the
        /// leading n-by-n lower triangular part of A contains the lower
        /// triangular part of the matrix A, and the strictly upper
        /// triangular part of A is not referenced.
        /// On exit:
        /// if UPLO = 'U', the last NB columns have been reduced to
        /// tridiagonal form, with the diagonal elements overwriting
        /// the diagonal elements of A; the elements above the diagonal
        /// with the array TAU, represent the orthogonal matrix Q as a
        /// product of elementary reflectors;
        /// if UPLO = 'L', the first NB columns have been reduced to
        /// tridiagonal form, with the diagonal elements overwriting
        /// the diagonal elements of A; the elements below the diagonal
        /// with the array TAU, represent the  orthogonal matrix Q as a
        /// product of elementary reflectors.
        /// See Further Details.
        ///</param>
        /// <param name="LDA">
        /// (input) INTEGER
        /// The leading dimension of the array A.  LDA .GE. (1,N).
        ///</param>
        /// <param name="E">
        /// (output) DOUBLE PRECISION array, dimension (N-1)
        /// If UPLO = 'U', E(n-nb:n-1) contains the superdiagonal
        /// elements of the last NB columns of the reduced matrix;
        /// if UPLO = 'L', E(1:nb) contains the subdiagonal elements of
        /// the first NB columns of the reduced matrix.
        ///</param>
        /// <param name="TAU">
        /// (output) DOUBLE PRECISION array, dimension (N-1)
        /// The scalar factors of the elementary reflectors, stored in
        /// TAU(n-nb:n-1) if UPLO = 'U', and in TAU(1:nb) if UPLO = 'L'.
        /// See Further Details.
        ///</param>
        /// <param name="W">
        /// (output) DOUBLE PRECISION array, dimension (LDW,NB)
        /// The n-by-nb matrix W required to update the unreduced part
        /// of A.
        ///</param>
        /// <param name="LDW">
        /// (input) INTEGER
        /// The leading dimension of the array W. LDW .GE. max(1,N).
        ///</param>
        public void Run(string UPLO, int N, int NB, ref double[] A, int offset_a, int LDA, ref double[] E, int offset_e
                         , ref double[] TAU, int offset_tau, ref double[] W, int offset_w, int LDW)
        {

            #region Variables
            
            int I = 0; int IW = 0; double ALPHA = 0; 

            #endregion


            #region Array Index Correction
            
             int o_a = -1 - LDA + offset_a;  int o_e = -1 + offset_e;  int o_tau = -1 + offset_tau; 
             int o_w = -1 - LDW + offset_w;

            #endregion


            #region Strings
            
            UPLO = UPLO.Substring(0, 1);  

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
            // *  DLATRD reduces NB rows and columns of a real symmetric matrix A to
            // *  symmetric tridiagonal form by an orthogonal similarity
            // *  transformation Q' * A * Q, and returns the matrices V and W which are
            // *  needed to apply the transformation to the unreduced part of A.
            // *
            // *  If UPLO = 'U', DLATRD reduces the last NB rows and columns of a
            // *  matrix, of which the upper triangle is supplied;
            // *  if UPLO = 'L', DLATRD reduces the first NB rows and columns of a
            // *  matrix, of which the lower triangle is supplied.
            // *
            // *  This is an auxiliary routine called by DSYTRD.
            // *
            // *  Arguments
            // *  =========
            // *
            // *  UPLO    (input) CHARACTER*1
            // *          Specifies whether the upper or lower triangular part of the
            // *          symmetric matrix A is stored:
            // *          = 'U': Upper triangular
            // *          = 'L': Lower triangular
            // *
            // *  N       (input) INTEGER
            // *          The order of the matrix A.
            // *
            // *  NB      (input) INTEGER
            // *          The number of rows and columns to be reduced.
            // *
            // *  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
            // *          On entry, the symmetric matrix A.  If UPLO = 'U', the leading
            // *          n-by-n upper triangular part of A contains the upper
            // *          triangular part of the matrix A, and the strictly lower
            // *          triangular part of A is not referenced.  If UPLO = 'L', the
            // *          leading n-by-n lower triangular part of A contains the lower
            // *          triangular part of the matrix A, and the strictly upper
            // *          triangular part of A is not referenced.
            // *          On exit:
            // *          if UPLO = 'U', the last NB columns have been reduced to
            // *            tridiagonal form, with the diagonal elements overwriting
            // *            the diagonal elements of A; the elements above the diagonal
            // *            with the array TAU, represent the orthogonal matrix Q as a
            // *            product of elementary reflectors;
            // *          if UPLO = 'L', the first NB columns have been reduced to
            // *            tridiagonal form, with the diagonal elements overwriting
            // *            the diagonal elements of A; the elements below the diagonal
            // *            with the array TAU, represent the  orthogonal matrix Q as a
            // *            product of elementary reflectors.
            // *          See Further Details.
            // *
            // *  LDA     (input) INTEGER
            // *          The leading dimension of the array A.  LDA >= (1,N).
            // *
            // *  E       (output) DOUBLE PRECISION array, dimension (N-1)
            // *          If UPLO = 'U', E(n-nb:n-1) contains the superdiagonal
            // *          elements of the last NB columns of the reduced matrix;
            // *          if UPLO = 'L', E(1:nb) contains the subdiagonal elements of
            // *          the first NB columns of the reduced matrix.
            // *
            // *  TAU     (output) DOUBLE PRECISION array, dimension (N-1)
            // *          The scalar factors of the elementary reflectors, stored in
            // *          TAU(n-nb:n-1) if UPLO = 'U', and in TAU(1:nb) if UPLO = 'L'.
            // *          See Further Details.
            // *
            // *  W       (output) DOUBLE PRECISION array, dimension (LDW,NB)
            // *          The n-by-nb matrix W required to update the unreduced part
            // *          of A.
            // *
            // *  LDW     (input) INTEGER
            // *          The leading dimension of the array W. LDW >= max(1,N).
            // *
            // *  Further Details
            // *  ===============
            // *
            // *  If UPLO = 'U', the matrix Q is represented as a product of elementary
            // *  reflectors
            // *
            // *     Q = H(n) H(n-1) . . . H(n-nb+1).
            // *
            // *  Each H(i) has the form
            // *
            // *     H(i) = I - tau * v * v'
            // *
            // *  where tau is a real scalar, and v is a real vector with
            // *  v(i:n) = 0 and v(i-1) = 1; v(1:i-1) is stored on exit in A(1:i-1,i),
            // *  and tau in TAU(i-1).
            // *
            // *  If UPLO = 'L', the matrix Q is represented as a product of elementary
            // *  reflectors
            // *
            // *     Q = H(1) H(2) . . . H(nb).
            // *
            // *  Each H(i) has the form
            // *
            // *     H(i) = I - tau * v * v'
            // *
            // *  where tau is a real scalar, and v is a real vector with
            // *  v(1:i) = 0 and v(i+1) = 1; v(i+1:n) is stored on exit in A(i+1:n,i),
            // *  and tau in TAU(i).
            // *
            // *  The elements of the vectors v together form the n-by-nb matrix V
            // *  which is needed, with W, to apply the transformation to the unreduced
            // *  part of the matrix, using a symmetric rank-2k update of the form:
            // *  A := A - V*W' - W*V'.
            // *
            // *  The contents of A on exit are illustrated by the following examples
            // *  with n = 5 and nb = 2:
            // *
            // *  if UPLO = 'U':                       if UPLO = 'L':
            // *
            // *    (  a   a   a   v4  v5 )              (  d                  )
            // *    (      a   a   v4  v5 )              (  1   d              )
            // *    (          a   1   v5 )              (  v1  1   a          )
            // *    (              d   1  )              (  v1  v2  a   a      )
            // *    (                  d  )              (  v1  v2  a   a   a  )
            // *
            // *  where d denotes a diagonal element of the reduced matrix, a denotes
            // *  an element of the original matrix that is unchanged, and vi denotes
            // *  an element of the vector defining H(i).
            // *
            // *  =====================================================================
            // *
            // *     .. Parameters ..
            // *     ..
            // *     .. Local Scalars ..
            // *     ..
            // *     .. External Subroutines ..
            // *     ..
            // *     .. External Functions ..
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
            
            if (N <= 0) return;
            // *
            if (this._lsame.Run(UPLO, "U"))
            {
                // *
                // *        Reduce last NB columns of upper triangle
                // *
                for (I = N; I >= N - NB + 1; I +=  - 1)
                {
                    IW = I - N + NB;
                    if (I < N)
                    {
                        // *
                        // *              Update A(1:i,i)
                        // *
                        this._dgemv.Run("No transpose", I, N - I,  - ONE, A, 1+(I + 1) * LDA + o_a, LDA
                                        , W, I+(IW + 1) * LDW + o_w, LDW, ONE, ref A, 1+I * LDA + o_a, 1);
                        this._dgemv.Run("No transpose", I, N - I,  - ONE, W, 1+(IW + 1) * LDW + o_w, LDW
                                        , A, I+(I + 1) * LDA + o_a, LDA, ONE, ref A, 1+I * LDA + o_a, 1);
                    }
                    if (I > 1)
                    {
                        // *
                        // *              Generate elementary reflector H(i) to annihilate
                        // *              A(1:i-2,i)
                        // *
                        this._dlarfg.Run(I - 1, ref A[I - 1+I * LDA + o_a], ref A, 1+I * LDA + o_a, 1, ref TAU[I - 1 + o_tau]);
                        E[I - 1 + o_e] = A[I - 1+I * LDA + o_a];
                        A[I - 1+I * LDA + o_a] = ONE;
                        // *
                        // *              Compute W(1:i-1,i)
                        // *
                        this._dsymv.Run("Upper", I - 1, ONE, A, offset_a, LDA, A, 1+I * LDA + o_a
                                        , 1, ZERO, ref W, 1+IW * LDW + o_w, 1);
                        if (I < N)
                        {
                            this._dgemv.Run("Transpose", I - 1, N - I, ONE, W, 1+(IW + 1) * LDW + o_w, LDW
                                            , A, 1+I * LDA + o_a, 1, ZERO, ref W, I + 1+IW * LDW + o_w, 1);
                            this._dgemv.Run("No transpose", I - 1, N - I,  - ONE, A, 1+(I + 1) * LDA + o_a, LDA
                                            , W, I + 1+IW * LDW + o_w, 1, ONE, ref W, 1+IW * LDW + o_w, 1);
                            this._dgemv.Run("Transpose", I - 1, N - I, ONE, A, 1+(I + 1) * LDA + o_a, LDA
                                            , A, 1+I * LDA + o_a, 1, ZERO, ref W, I + 1+IW * LDW + o_w, 1);
                            this._dgemv.Run("No transpose", I - 1, N - I,  - ONE, W, 1+(IW + 1) * LDW + o_w, LDW
                                            , W, I + 1+IW * LDW + o_w, 1, ONE, ref W, 1+IW * LDW + o_w, 1);
                        }
                        this._dscal.Run(I - 1, TAU[I - 1 + o_tau], ref W, 1+IW * LDW + o_w, 1);
                        ALPHA =  - HALF * TAU[I - 1 + o_tau] * this._ddot.Run(I - 1, W, 1+IW * LDW + o_w, 1, A, 1+I * LDA + o_a, 1);
                        this._daxpy.Run(I - 1, ALPHA, A, 1+I * LDA + o_a, 1, ref W, 1+IW * LDW + o_w, 1);
                    }
                    // *
                }
            }
            else
            {
                // *
                // *        Reduce first NB columns of lower triangle
                // *
                for (I = 1; I <= NB; I++)
                {
                    // *
                    // *           Update A(i:n,i)
                    // *
                    this._dgemv.Run("No transpose", N - I + 1, I - 1,  - ONE, A, I+1 * LDA + o_a, LDA
                                    , W, I+1 * LDW + o_w, LDW, ONE, ref A, I+I * LDA + o_a, 1);
                    this._dgemv.Run("No transpose", N - I + 1, I - 1,  - ONE, W, I+1 * LDW + o_w, LDW
                                    , A, I+1 * LDA + o_a, LDA, ONE, ref A, I+I * LDA + o_a, 1);
                    if (I < N)
                    {
                        // *
                        // *              Generate elementary reflector H(i) to annihilate
                        // *              A(i+2:n,i)
                        // *
                        this._dlarfg.Run(N - I, ref A[I + 1+I * LDA + o_a], ref A, Math.Min(I + 2, N)+I * LDA + o_a, 1, ref TAU[I + o_tau]);
                        E[I + o_e] = A[I + 1+I * LDA + o_a];
                        A[I + 1+I * LDA + o_a] = ONE;
                        // *
                        // *              Compute W(i+1:n,i)
                        // *
                        this._dsymv.Run("Lower", N - I, ONE, A, I + 1+(I + 1) * LDA + o_a, LDA, A, I + 1+I * LDA + o_a
                                        , 1, ZERO, ref W, I + 1+I * LDW + o_w, 1);
                        this._dgemv.Run("Transpose", N - I, I - 1, ONE, W, I + 1+1 * LDW + o_w, LDW
                                        , A, I + 1+I * LDA + o_a, 1, ZERO, ref W, 1+I * LDW + o_w, 1);
                        this._dgemv.Run("No transpose", N - I, I - 1,  - ONE, A, I + 1+1 * LDA + o_a, LDA
                                        , W, 1+I * LDW + o_w, 1, ONE, ref W, I + 1+I * LDW + o_w, 1);
                        this._dgemv.Run("Transpose", N - I, I - 1, ONE, A, I + 1+1 * LDA + o_a, LDA
                                        , A, I + 1+I * LDA + o_a, 1, ZERO, ref W, 1+I * LDW + o_w, 1);
                        this._dgemv.Run("No transpose", N - I, I - 1,  - ONE, W, I + 1+1 * LDW + o_w, LDW
                                        , W, 1+I * LDW + o_w, 1, ONE, ref W, I + 1+I * LDW + o_w, 1);
                        this._dscal.Run(N - I, TAU[I + o_tau], ref W, I + 1+I * LDW + o_w, 1);
                        ALPHA =  - HALF * TAU[I + o_tau] * this._ddot.Run(N - I, W, I + 1+I * LDW + o_w, 1, A, I + 1+I * LDA + o_a, 1);
                        this._daxpy.Run(N - I, ALPHA, A, I + 1+I * LDA + o_a, 1, ref W, I + 1+I * LDW + o_w, 1);
                    }
                    // *
                }
            }
            // *
            return;
            // *
            // *     End of DLATRD
            // *

            #endregion

        }
    }
}
