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
    /// DSYTD2 reduces a real symmetric matrix A to symmetric tridiagonal
    /// form T by an orthogonal similarity transformation: Q' * A * Q = T.
    /// 
    ///</summary>
    public class DSYTD2
    {
    

        #region Dependencies
        
        DAXPY _daxpy; DLARFG _dlarfg; DSYMV _dsymv; DSYR2 _dsyr2; XERBLA _xerbla; LSAME _lsame; DDOT _ddot; 

        #endregion


        #region Variables
        
        const double ONE = 1.0E0; const double ZERO = 0.0E0; const double HALF = 1.0E0 / 2.0E0; 

        #endregion

        public DSYTD2(DAXPY daxpy, DLARFG dlarfg, DSYMV dsymv, DSYR2 dsyr2, XERBLA xerbla, LSAME lsame, DDOT ddot)
        {
    

            #region Set Dependencies
            
            this._daxpy = daxpy; this._dlarfg = dlarfg; this._dsymv = dsymv; this._dsyr2 = dsyr2; this._xerbla = xerbla; 
            this._lsame = lsame;this._ddot = ddot; 

            #endregion

        }
    
        public DSYTD2()
        {
    

            #region Dependencies (Initialization)
            
            DAXPY daxpy = new DAXPY();
            LSAME lsame = new LSAME();
            DLAMC3 dlamc3 = new DLAMC3();
            DLAPY2 dlapy2 = new DLAPY2();
            DNRM2 dnrm2 = new DNRM2();
            DSCAL dscal = new DSCAL();
            XERBLA xerbla = new XERBLA();
            DDOT ddot = new DDOT();
            DLAMC1 dlamc1 = new DLAMC1(dlamc3);
            DLAMC4 dlamc4 = new DLAMC4(dlamc3);
            DLAMC5 dlamc5 = new DLAMC5(dlamc3);
            DLAMC2 dlamc2 = new DLAMC2(dlamc3, dlamc1, dlamc4, dlamc5);
            DLAMCH dlamch = new DLAMCH(lsame, dlamc2);
            DLARFG dlarfg = new DLARFG(dlamch, dlapy2, dnrm2, dscal);
            DSYMV dsymv = new DSYMV(lsame, xerbla);
            DSYR2 dsyr2 = new DSYR2(lsame, xerbla);

            #endregion


            #region Set Dependencies
            
            this._daxpy = daxpy; this._dlarfg = dlarfg; this._dsymv = dsymv; this._dsyr2 = dsyr2; this._xerbla = xerbla; 
            this._lsame = lsame;this._ddot = ddot; 

            #endregion

        }
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DSYTD2 reduces a real symmetric matrix A to symmetric tridiagonal
        /// form T by an orthogonal similarity transformation: Q' * A * Q = T.
        /// 
        ///</summary>
        /// <param name="UPLO">
        /// (input) CHARACTER*1
        /// Specifies whether the upper or lower triangular part of the
        /// symmetric matrix A is stored:
        /// = 'U':  Upper triangular
        /// = 'L':  Lower triangular
        ///</param>
        /// <param name="N">
        /// (input) INTEGER
        /// The order of the matrix A.  N .GE. 0.
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
        /// On exit, if UPLO = 'U', the diagonal and first superdiagonal
        /// of A are overwritten by the corresponding elements of the
        /// tridiagonal matrix T, and the elements above the first
        /// superdiagonal, with the array TAU, represent the orthogonal
        /// matrix Q as a product of elementary reflectors; if UPLO
        /// = 'L', the diagonal and first subdiagonal of A are over-
        /// written by the corresponding elements of the tridiagonal
        /// matrix T, and the elements below the first subdiagonal, with
        /// the array TAU, represent the orthogonal matrix Q as a product
        /// of elementary reflectors. See Further Details.
        ///</param>
        /// <param name="LDA">
        /// (input) INTEGER
        /// The leading dimension of the array A.  LDA .GE. max(1,N).
        ///</param>
        /// <param name="D">
        /// (output) DOUBLE PRECISION array, dimension (N)
        /// The diagonal elements of the tridiagonal matrix T:
        /// D(i) = A(i,i).
        ///</param>
        /// <param name="E">
        /// (output) DOUBLE PRECISION array, dimension (N-1)
        /// The off-diagonal elements of the tridiagonal matrix T:
        /// E(i) = A(i,i+1) if UPLO = 'U', E(i) = A(i+1,i) if UPLO = 'L'.
        ///</param>
        /// <param name="TAU">
        /// (output) DOUBLE PRECISION array, dimension (N-1)
        /// The scalar factors of the elementary reflectors (see Further
        /// Details).
        ///</param>
        /// <param name="INFO">
        /// (output) INTEGER
        /// = 0:  successful exit
        /// .LT. 0:  if INFO = -i, the i-th argument had an illegal value.
        ///</param>
        public void Run(string UPLO, int N, ref double[] A, int offset_a, int LDA, ref double[] D, int offset_d, ref double[] E, int offset_e
                         , ref double[] TAU, int offset_tau, ref int INFO)
        {

            #region Variables
            
            bool UPPER = false; int I = 0; double ALPHA = 0; double TAUI = 0; 

            #endregion


            #region Array Index Correction
            
             int o_a = -1 - LDA + offset_a;  int o_d = -1 + offset_d;  int o_e = -1 + offset_e;  int o_tau = -1 + offset_tau; 

            #endregion


            #region Strings
            
            UPLO = UPLO.Substring(0, 1);  

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
            // *  DSYTD2 reduces a real symmetric matrix A to symmetric tridiagonal
            // *  form T by an orthogonal similarity transformation: Q' * A * Q = T.
            // *
            // *  Arguments
            // *  =========
            // *
            // *  UPLO    (input) CHARACTER*1
            // *          Specifies whether the upper or lower triangular part of the
            // *          symmetric matrix A is stored:
            // *          = 'U':  Upper triangular
            // *          = 'L':  Lower triangular
            // *
            // *  N       (input) INTEGER
            // *          The order of the matrix A.  N >= 0.
            // *
            // *  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
            // *          On entry, the symmetric matrix A.  If UPLO = 'U', the leading
            // *          n-by-n upper triangular part of A contains the upper
            // *          triangular part of the matrix A, and the strictly lower
            // *          triangular part of A is not referenced.  If UPLO = 'L', the
            // *          leading n-by-n lower triangular part of A contains the lower
            // *          triangular part of the matrix A, and the strictly upper
            // *          triangular part of A is not referenced.
            // *          On exit, if UPLO = 'U', the diagonal and first superdiagonal
            // *          of A are overwritten by the corresponding elements of the
            // *          tridiagonal matrix T, and the elements above the first
            // *          superdiagonal, with the array TAU, represent the orthogonal
            // *          matrix Q as a product of elementary reflectors; if UPLO
            // *          = 'L', the diagonal and first subdiagonal of A are over-
            // *          written by the corresponding elements of the tridiagonal
            // *          matrix T, and the elements below the first subdiagonal, with
            // *          the array TAU, represent the orthogonal matrix Q as a product
            // *          of elementary reflectors. See Further Details.
            // *
            // *  LDA     (input) INTEGER
            // *          The leading dimension of the array A.  LDA >= max(1,N).
            // *
            // *  D       (output) DOUBLE PRECISION array, dimension (N)
            // *          The diagonal elements of the tridiagonal matrix T:
            // *          D(i) = A(i,i).
            // *
            // *  E       (output) DOUBLE PRECISION array, dimension (N-1)
            // *          The off-diagonal elements of the tridiagonal matrix T:
            // *          E(i) = A(i,i+1) if UPLO = 'U', E(i) = A(i+1,i) if UPLO = 'L'.
            // *
            // *  TAU     (output) DOUBLE PRECISION array, dimension (N-1)
            // *          The scalar factors of the elementary reflectors (see Further
            // *          Details).
            // *
            // *  INFO    (output) INTEGER
            // *          = 0:  successful exit
            // *          < 0:  if INFO = -i, the i-th argument had an illegal value.
            // *
            // *  Further Details
            // *  ===============
            // *
            // *  If UPLO = 'U', the matrix Q is represented as a product of elementary
            // *  reflectors
            // *
            // *     Q = H(n-1) . . . H(2) H(1).
            // *
            // *  Each H(i) has the form
            // *
            // *     H(i) = I - tau * v * v'
            // *
            // *  where tau is a real scalar, and v is a real vector with
            // *  v(i+1:n) = 0 and v(i) = 1; v(1:i-1) is stored on exit in
            // *  A(1:i-1,i+1), and tau in TAU(i).
            // *
            // *  If UPLO = 'L', the matrix Q is represented as a product of elementary
            // *  reflectors
            // *
            // *     Q = H(1) H(2) . . . H(n-1).
            // *
            // *  Each H(i) has the form
            // *
            // *     H(i) = I - tau * v * v'
            // *
            // *  where tau is a real scalar, and v is a real vector with
            // *  v(1:i) = 0 and v(i+1) = 1; v(i+2:n) is stored on exit in A(i+2:n,i),
            // *  and tau in TAU(i).
            // *
            // *  The contents of A on exit are illustrated by the following examples
            // *  with n = 5:
            // *
            // *  if UPLO = 'U':                       if UPLO = 'L':
            // *
            // *    (  d   e   v2  v3  v4 )              (  d                  )
            // *    (      d   e   v3  v4 )              (  e   d              )
            // *    (          d   e   v4 )              (  v1  e   d          )
            // *    (              d   e  )              (  v1  v2  e   d      )
            // *    (                  d  )              (  v1  v2  v3  e   d  )
            // *
            // *  where d and e denote diagonal and off-diagonal elements of T, and vi
            // *  denotes an element of the vector defining H(i).
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
            //      INTRINSIC          MAX, MIN;
            // *     ..
            // *     .. Executable Statements ..
            // *
            // *     Test the input parameters
            // *

            #endregion


            #region Body
            
            INFO = 0;
            UPPER = this._lsame.Run(UPLO, "U");
            if (!UPPER && !this._lsame.Run(UPLO, "L"))
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
                    if (LDA < Math.Max(1, N))
                    {
                        INFO =  - 4;
                    }
                }
            }
            if (INFO != 0)
            {
                this._xerbla.Run("DSYTD2",  - INFO);
                return;
            }
            // *
            // *     Quick return if possible
            // *
            if (N <= 0) return;
            // *
            if (UPPER)
            {
                // *
                // *        Reduce the upper triangle of A
                // *
                for (I = N - 1; I >= 1; I +=  - 1)
                {
                    // *
                    // *           Generate elementary reflector H(i) = I - tau * v * v'
                    // *           to annihilate A(1:i-1,i+1)
                    // *
                    this._dlarfg.Run(I, ref A[I+(I + 1) * LDA + o_a], ref A, 1+(I + 1) * LDA + o_a, 1, ref TAUI);
                    E[I + o_e] = A[I+(I + 1) * LDA + o_a];
                    // *
                    if (TAUI != ZERO)
                    {
                        // *
                        // *              Apply H(i) from both sides to A(1:i,1:i)
                        // *
                        A[I+(I + 1) * LDA + o_a] = ONE;
                        // *
                        // *              Compute  x := tau * A * v  storing x in TAU(1:i)
                        // *
                        this._dsymv.Run(UPLO, I, TAUI, A, offset_a, LDA, A, 1+(I + 1) * LDA + o_a
                                        , 1, ZERO, ref TAU, offset_tau, 1);
                        // *
                        // *              Compute  w := x - 1/2 * tau * (x'*v) * v
                        // *
                        ALPHA =  - HALF * TAUI * this._ddot.Run(I, TAU, offset_tau, 1, A, 1+(I + 1) * LDA + o_a, 1);
                        this._daxpy.Run(I, ALPHA, A, 1+(I + 1) * LDA + o_a, 1, ref TAU, offset_tau, 1);
                        // *
                        // *              Apply the transformation as a rank-2 update:
                        // *                 A := A - v * w' - w * v'
                        // *
                        this._dsyr2.Run(UPLO, I,  - ONE, A, 1+(I + 1) * LDA + o_a, 1, TAU, offset_tau
                                        , 1, ref A, offset_a, LDA);
                        // *
                        A[I+(I + 1) * LDA + o_a] = E[I + o_e];
                    }
                    D[I + 1 + o_d] = A[I + 1+(I + 1) * LDA + o_a];
                    TAU[I + o_tau] = TAUI;
                }
                D[1 + o_d] = A[1+1 * LDA + o_a];
            }
            else
            {
                // *
                // *        Reduce the lower triangle of A
                // *
                for (I = 1; I <= N - 1; I++)
                {
                    // *
                    // *           Generate elementary reflector H(i) = I - tau * v * v'
                    // *           to annihilate A(i+2:n,i)
                    // *
                    this._dlarfg.Run(N - I, ref A[I + 1+I * LDA + o_a], ref A, Math.Min(I + 2, N)+I * LDA + o_a, 1, ref TAUI);
                    E[I + o_e] = A[I + 1+I * LDA + o_a];
                    // *
                    if (TAUI != ZERO)
                    {
                        // *
                        // *              Apply H(i) from both sides to A(i+1:n,i+1:n)
                        // *
                        A[I + 1+I * LDA + o_a] = ONE;
                        // *
                        // *              Compute  x := tau * A * v  storing y in TAU(i:n-1)
                        // *
                        this._dsymv.Run(UPLO, N - I, TAUI, A, I + 1+(I + 1) * LDA + o_a, LDA, A, I + 1+I * LDA + o_a
                                        , 1, ZERO, ref TAU, I + o_tau, 1);
                        // *
                        // *              Compute  w := x - 1/2 * tau * (x'*v) * v
                        // *
                        ALPHA =  - HALF * TAUI * this._ddot.Run(N - I, TAU, I + o_tau, 1, A, I + 1+I * LDA + o_a, 1);
                        this._daxpy.Run(N - I, ALPHA, A, I + 1+I * LDA + o_a, 1, ref TAU, I + o_tau, 1);
                        // *
                        // *              Apply the transformation as a rank-2 update:
                        // *                 A := A - v * w' - w * v'
                        // *
                        this._dsyr2.Run(UPLO, N - I,  - ONE, A, I + 1+I * LDA + o_a, 1, TAU, I + o_tau
                                        , 1, ref A, I + 1+(I + 1) * LDA + o_a, LDA);
                        // *
                        A[I + 1+I * LDA + o_a] = E[I + o_e];
                    }
                    D[I + o_d] = A[I+I * LDA + o_a];
                    TAU[I + o_tau] = TAUI;
                }
                D[N + o_d] = A[N+N * LDA + o_a];
            }
            // *
            return;
            // *
            // *     End of DSYTD2
            // *

            #endregion

        }
    }
}
