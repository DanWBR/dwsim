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
    /// -- LAPACK auxiliary routine (version 3.0) --
    /// Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
    /// Courant Institute, Argonne National Lab, and Rice University
    /// June 30, 1992
    /// Purpose
    /// =======
    /// 
    /// DLATRS solves one of the triangular systems
    /// 
    /// A *x = s*b  or  A'*x = s*b
    /// 
    /// with scaling to prevent overflow.  Here A is an upper or lower
    /// triangular matrix, A' denotes the transpose of A, x and b are
    /// n-element vectors, and s is a scaling factor, usually less than
    /// or equal to 1, chosen so that the components of x will be less than
    /// the overflow threshold.  If the unscaled problem will not cause
    /// overflow, the Level 2 BLAS routine DTRSV is called.  If the matrix A
    /// is singular (A(j,j) = 0 for some j), then s is set to 0 and a
    /// non-trivial solution to A*x = 0 is returned.
    /// 
    ///</summary>
    public class DLATRS
    {
    

        #region Dependencies
        
        LSAME _lsame; IDAMAX _idamax; DASUM _dasum; DDOT _ddot; DLAMCH _dlamch; DAXPY _daxpy; DSCAL _dscal; DTRSV _dtrsv; 
        XERBLA _xerbla;

        #endregion


        #region Variables
        
        const double ZERO = 0.0E+0; const double HALF = 0.5E+0; const double ONE = 1.0E+0; 

        #endregion

        public DLATRS(LSAME lsame, IDAMAX idamax, DASUM dasum, DDOT ddot, DLAMCH dlamch, DAXPY daxpy, DSCAL dscal, DTRSV dtrsv, XERBLA xerbla)
        {
    

            #region Set Dependencies
            
            this._lsame = lsame; this._idamax = idamax; this._dasum = dasum; this._ddot = ddot; this._dlamch = dlamch; 
            this._daxpy = daxpy;this._dscal = dscal; this._dtrsv = dtrsv; this._xerbla = xerbla; 

            #endregion

        }
    
        public DLATRS()
        {
    

            #region Dependencies (Initialization)
            
            LSAME lsame = new LSAME();
            IDAMAX idamax = new IDAMAX();
            DASUM dasum = new DASUM();
            DDOT ddot = new DDOT();
            DLAMC3 dlamc3 = new DLAMC3();
            DAXPY daxpy = new DAXPY();
            DSCAL dscal = new DSCAL();
            XERBLA xerbla = new XERBLA();
            DLAMC1 dlamc1 = new DLAMC1(dlamc3);
            DLAMC4 dlamc4 = new DLAMC4(dlamc3);
            DLAMC5 dlamc5 = new DLAMC5(dlamc3);
            DLAMC2 dlamc2 = new DLAMC2(dlamc3, dlamc1, dlamc4, dlamc5);
            DLAMCH dlamch = new DLAMCH(lsame, dlamc2);
            DTRSV dtrsv = new DTRSV(lsame, xerbla);

            #endregion


            #region Set Dependencies
            
            this._lsame = lsame; this._idamax = idamax; this._dasum = dasum; this._ddot = ddot; this._dlamch = dlamch; 
            this._daxpy = daxpy;this._dscal = dscal; this._dtrsv = dtrsv; this._xerbla = xerbla; 

            #endregion

        }
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DLATRS solves one of the triangular systems
        /// 
        /// A *x = s*b  or  A'*x = s*b
        /// 
        /// with scaling to prevent overflow.  Here A is an upper or lower
        /// triangular matrix, A' denotes the transpose of A, x and b are
        /// n-element vectors, and s is a scaling factor, usually less than
        /// or equal to 1, chosen so that the components of x will be less than
        /// the overflow threshold.  If the unscaled problem will not cause
        /// overflow, the Level 2 BLAS routine DTRSV is called.  If the matrix A
        /// is singular (A(j,j) = 0 for some j), then s is set to 0 and a
        /// non-trivial solution to A*x = 0 is returned.
        /// 
        ///</summary>
        /// <param name="UPLO">
        /// (input) CHARACTER*1
        /// Specifies whether the matrix A is upper or lower triangular.
        /// = 'U':  Upper triangular
        /// = 'L':  Lower triangular
        ///</param>
        /// <param name="TRANS">
        /// (input) CHARACTER*1
        /// Specifies the operation applied to A.
        /// = 'N':  Solve A * x = s*b  (No transpose)
        /// = 'T':  Solve A'* x = s*b  (Transpose)
        /// = 'C':  Solve A'* x = s*b  (Conjugate transpose = Transpose)
        ///</param>
        /// <param name="DIAG">
        /// (input) CHARACTER*1
        /// Specifies whether or not the matrix A is unit triangular.
        /// = 'N':  Non-unit triangular
        /// = 'U':  Unit triangular
        ///</param>
        /// <param name="NORMIN">
        /// (input) CHARACTER*1
        /// Specifies whether CNORM has been set or not.
        /// = 'Y':  CNORM contains the column norms on entry
        /// = 'N':  CNORM is not set on entry.  On exit, the norms will
        /// be computed and stored in CNORM.
        ///</param>
        /// <param name="N">
        /// (input) INTEGER
        /// The order of the matrix A.  N .GE. 0.
        ///</param>
        /// <param name="A">
        /// *x = s*b  or  A'*x = s*b
        ///</param>
        /// <param name="LDA">
        /// (input) INTEGER
        /// The leading dimension of the array A.  LDA .GE. max (1,N).
        ///</param>
        /// <param name="X">
        /// (input/output) DOUBLE PRECISION array, dimension (N)
        /// On entry, the right hand side b of the triangular system.
        /// On exit, X is overwritten by the solution vector x.
        ///</param>
        /// <param name="SCALE">
        /// (output) DOUBLE PRECISION
        /// The scaling factor s for the triangular system
        /// A * x = s*b  or  A'* x = s*b.
        /// If SCALE = 0, the matrix A is singular or badly scaled, and
        /// the vector x is an exact or approximate solution to A*x = 0.
        ///</param>
        /// <param name="CNORM">
        /// (input or output) DOUBLE PRECISION array, dimension (N)
        /// 
        /// If NORMIN = 'Y', CNORM is an input argument and CNORM(j)
        /// contains the norm of the off-diagonal part of the j-th column
        /// of A.  If TRANS = 'N', CNORM(j) must be greater than or equal
        /// to the infinity-norm, and if TRANS = 'T' or 'C', CNORM(j)
        /// must be greater than or equal to the 1-norm.
        /// 
        /// If NORMIN = 'N', CNORM is an output argument and CNORM(j)
        /// returns the 1-norm of the offdiagonal part of the j-th column
        /// of A.
        ///</param>
        /// <param name="INFO">
        /// (output) INTEGER
        /// = 0:  successful exit
        /// .LT. 0:  if INFO = -k, the k-th argument had an illegal value
        ///</param>
        public void Run(string UPLO, string TRANS, string DIAG, string NORMIN, int N, double[] A, int offset_a
                         , int LDA, ref double[] X, int offset_x, ref double SCALE, ref double[] CNORM, int offset_cnorm, ref int INFO)
        {

            #region Variables
            
            bool NOTRAN = false; bool NOUNIT = false; bool UPPER = false; int I = 0; int IMAX = 0; int J = 0; int JFIRST = 0; 
            int JINC = 0;int JLAST = 0; double BIGNUM = 0; double GROW = 0; double REC = 0; double SMLNUM = 0; double SUMJ = 0; 
            double TJJ = 0;double TJJS = 0; double TMAX = 0; double TSCAL = 0; double USCAL = 0; double XBND = 0; double XJ = 0; 
            double XMAX = 0;

            #endregion


            #region Implicit Variables
            
            int A_J = 0; 

            #endregion


            #region Array Index Correction
            
             int o_a = -1 - LDA + offset_a;  int o_x = -1 + offset_x;  int o_cnorm = -1 + offset_cnorm; 

            #endregion


            #region Strings
            
            UPLO = UPLO.Substring(0, 1);  TRANS = TRANS.Substring(0, 1);  DIAG = DIAG.Substring(0, 1);  
            NORMIN = NORMIN.Substring(0, 1); 

            #endregion


            #region Prolog
            
            // *
            // *  -- LAPACK auxiliary routine (version 3.0) --
            // *     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
            // *     Courant Institute, Argonne National Lab, and Rice University
            // *     June 30, 1992
            // *
            // *     .. Scalar Arguments ..
            // *     ..
            // *     .. Array Arguments ..
            // *     ..
            // *
            // *  Purpose
            // *  =======
            // *
            // *  DLATRS solves one of the triangular systems
            // *
            // *     A *x = s*b  or  A'*x = s*b
            // *
            // *  with scaling to prevent overflow.  Here A is an upper or lower
            // *  triangular matrix, A' denotes the transpose of A, x and b are
            // *  n-element vectors, and s is a scaling factor, usually less than
            // *  or equal to 1, chosen so that the components of x will be less than
            // *  the overflow threshold.  If the unscaled problem will not cause
            // *  overflow, the Level 2 BLAS routine DTRSV is called.  If the matrix A
            // *  is singular (A(j,j) = 0 for some j), then s is set to 0 and a
            // *  non-trivial solution to A*x = 0 is returned.
            // *
            // *  Arguments
            // *  =========
            // *
            // *  UPLO    (input) CHARACTER*1
            // *          Specifies whether the matrix A is upper or lower triangular.
            // *          = 'U':  Upper triangular
            // *          = 'L':  Lower triangular
            // *
            // *  TRANS   (input) CHARACTER*1
            // *          Specifies the operation applied to A.
            // *          = 'N':  Solve A * x = s*b  (No transpose)
            // *          = 'T':  Solve A'* x = s*b  (Transpose)
            // *          = 'C':  Solve A'* x = s*b  (Conjugate transpose = Transpose)
            // *
            // *  DIAG    (input) CHARACTER*1
            // *          Specifies whether or not the matrix A is unit triangular.
            // *          = 'N':  Non-unit triangular
            // *          = 'U':  Unit triangular
            // *
            // *  NORMIN  (input) CHARACTER*1
            // *          Specifies whether CNORM has been set or not.
            // *          = 'Y':  CNORM contains the column norms on entry
            // *          = 'N':  CNORM is not set on entry.  On exit, the norms will
            // *                  be computed and stored in CNORM.
            // *
            // *  N       (input) INTEGER
            // *          The order of the matrix A.  N >= 0.
            // *
            // *  A       (input) DOUBLE PRECISION array, dimension (LDA,N)
            // *          The triangular matrix A.  If UPLO = 'U', the leading n by n
            // *          upper triangular part of the array A contains the upper
            // *          triangular matrix, and the strictly lower triangular part of
            // *          A is not referenced.  If UPLO = 'L', the leading n by n lower
            // *          triangular part of the array A contains the lower triangular
            // *          matrix, and the strictly upper triangular part of A is not
            // *          referenced.  If DIAG = 'U', the diagonal elements of A are
            // *          also not referenced and are assumed to be 1.
            // *
            // *  LDA     (input) INTEGER
            // *          The leading dimension of the array A.  LDA >= max (1,N).
            // *
            // *  X       (input/output) DOUBLE PRECISION array, dimension (N)
            // *          On entry, the right hand side b of the triangular system.
            // *          On exit, X is overwritten by the solution vector x.
            // *
            // *  SCALE   (output) DOUBLE PRECISION
            // *          The scaling factor s for the triangular system
            // *             A * x = s*b  or  A'* x = s*b.
            // *          If SCALE = 0, the matrix A is singular or badly scaled, and
            // *          the vector x is an exact or approximate solution to A*x = 0.
            // *
            // *  CNORM   (input or output) DOUBLE PRECISION array, dimension (N)
            // *
            // *          If NORMIN = 'Y', CNORM is an input argument and CNORM(j)
            // *          contains the norm of the off-diagonal part of the j-th column
            // *          of A.  If TRANS = 'N', CNORM(j) must be greater than or equal
            // *          to the infinity-norm, and if TRANS = 'T' or 'C', CNORM(j)
            // *          must be greater than or equal to the 1-norm.
            // *
            // *          If NORMIN = 'N', CNORM is an output argument and CNORM(j)
            // *          returns the 1-norm of the offdiagonal part of the j-th column
            // *          of A.
            // *
            // *  INFO    (output) INTEGER
            // *          = 0:  successful exit
            // *          < 0:  if INFO = -k, the k-th argument had an illegal value
            // *
            // *  Further Details
            // *  ======= =======
            // *
            // *  A rough bound on x is computed; if that is less than overflow, DTRSV
            // *  is called, otherwise, specific code is used which checks for possible
            // *  overflow or divide-by-zero at every operation.
            // *
            // *  A columnwise scheme is used for solving A*x = b.  The basic algorithm
            // *  if A is lower triangular is
            // *
            // *       x[1:n] := b[1:n]
            // *       for j = 1, ..., n
            // *            x(j) := x(j) / A(j,j)
            // *            x[j+1:n] := x[j+1:n] - x(j) * A[j+1:n,j]
            // *       end
            // *
            // *  Define bounds on the components of x after j iterations of the loop:
            // *     M(j) = bound on x[1:j]
            // *     G(j) = bound on x[j+1:n]
            // *  Initially, let M(0) = 0 and G(0) = max{x(i), i=1,...,n}.
            // *
            // *  Then for iteration j+1 we have
            // *     M(j+1) <= G(j) / | A(j+1,j+1) |
            // *     G(j+1) <= G(j) + M(j+1) * | A[j+2:n,j+1] |
            // *            <= G(j) ( 1 + CNORM(j+1) / | A(j+1,j+1) | )
            // *
            // *  where CNORM(j+1) is greater than or equal to the infinity-norm of
            // *  column j+1 of A, not counting the diagonal.  Hence
            // *
            // *     G(j) <= G(0) product ( 1 + CNORM(i) / | A(i,i) | )
            // *                  1<=i<=j
            // *  and
            // *
            // *     |x(j)| <= ( G(0) / |A(j,j)| ) product ( 1 + CNORM(i) / |A(i,i)| )
            // *                                   1<=i< j
            // *
            // *  Since |x(j)| <= M(j), we use the Level 2 BLAS routine DTRSV if the
            // *  reciprocal of the largest M(j), j=1,..,n, is larger than
            // *  max(underflow, 1/overflow).
            // *
            // *  The bound on x(j) is also used to determine when a step in the
            // *  columnwise method can be performed without fear of overflow.  If
            // *  the computed bound is greater than a large constant, x is scaled to
            // *  prevent overflow, but if the bound overflows, x is set to 0, x(j) to
            // *  1, and scale to 0, and a non-trivial solution to A*x = 0 is found.
            // *
            // *  Similarly, a row-wise scheme is used to solve A'*x = b.  The basic
            // *  algorithm for A upper triangular is
            // *
            // *       for j = 1, ..., n
            // *            x(j) := ( b(j) - A[1:j-1,j]' * x[1:j-1] ) / A(j,j)
            // *       end
            // *
            // *  We simultaneously compute two bounds
            // *       G(j) = bound on ( b(i) - A[1:i-1,i]' * x[1:i-1] ), 1<=i<=j
            // *       M(j) = bound on x(i), 1<=i<=j
            // *
            // *  The initial values are G(0) = 0, M(0) = max{b(i), i=1,..,n}, and we
            // *  add the constraint G(j) >= G(j-1) and M(j) >= M(j-1) for j >= 1.
            // *  Then the bound on x(j) is
            // *
            // *       M(j) <= M(j-1) * ( 1 + CNORM(j) ) / | A(j,j) |
            // *
            // *            <= M(0) * product ( ( 1 + CNORM(i) ) / |A(i,i)| )
            // *                      1<=i<=j
            // *
            // *  and we can safely call DTRSV if 1/M(n) and 1/G(n) are both greater
            // *  than max(underflow, 1/overflow).
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
            //      INTRINSIC          ABS, MAX, MIN;
            // *     ..
            // *     .. Executable Statements ..
            // *

            #endregion


            #region Body
            
            INFO = 0;
            UPPER = this._lsame.Run(UPLO, "U");
            NOTRAN = this._lsame.Run(TRANS, "N");
            NOUNIT = this._lsame.Run(DIAG, "N");
            // *
            // *     Test the input parameters.
            // *
            if (!UPPER && !this._lsame.Run(UPLO, "L"))
            {
                INFO =  - 1;
            }
            else
            {
                if (!NOTRAN && !this._lsame.Run(TRANS, "T") && !this._lsame.Run(TRANS, "C"))
                {
                    INFO =  - 2;
                }
                else
                {
                    if (!NOUNIT && !this._lsame.Run(DIAG, "U"))
                    {
                        INFO =  - 3;
                    }
                    else
                    {
                        if (!this._lsame.Run(NORMIN, "Y") && !this._lsame.Run(NORMIN, "N"))
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
                                if (LDA < Math.Max(1, N))
                                {
                                    INFO =  - 7;
                                }
                            }
                        }
                    }
                }
            }
            if (INFO != 0)
            {
                this._xerbla.Run("DLATRS",  - INFO);
                return;
            }
            // *
            // *     Quick return if possible
            // *
            if (N == 0) return;
            // *
            // *     Determine machine dependent parameters to control overflow.
            // *
            SMLNUM = this._dlamch.Run("Safe minimum") / this._dlamch.Run("Precision");
            BIGNUM = ONE / SMLNUM;
            SCALE = ONE;
            // *
            if (this._lsame.Run(NORMIN, "N"))
            {
                // *
                // *        Compute the 1-norm of each column, not including the diagonal.
                // *
                if (UPPER)
                {
                    // *
                    // *           A is upper triangular.
                    // *
                    for (J = 1; J <= N; J++)
                    {
                        CNORM[J + o_cnorm] = this._dasum.Run(J - 1, A, 1+J * LDA + o_a, 1);
                    }
                }
                else
                {
                    // *
                    // *           A is lower triangular.
                    // *
                    for (J = 1; J <= N - 1; J++)
                    {
                        CNORM[J + o_cnorm] = this._dasum.Run(N - J, A, J + 1+J * LDA + o_a, 1);
                    }
                    CNORM[N + o_cnorm] = ZERO;
                }
            }
            // *
            // *     Scale the column norms by TSCAL if the maximum element in CNORM is
            // *     greater than BIGNUM.
            // *
            IMAX = this._idamax.Run(N, CNORM, offset_cnorm, 1);
            TMAX = CNORM[IMAX + o_cnorm];
            if (TMAX <= BIGNUM)
            {
                TSCAL = ONE;
            }
            else
            {
                TSCAL = ONE / (SMLNUM * TMAX);
                this._dscal.Run(N, TSCAL, ref CNORM, offset_cnorm, 1);
            }
            // *
            // *     Compute a bound on the computed solution vector to see if the
            // *     Level 2 BLAS routine DTRSV can be used.
            // *
            J = this._idamax.Run(N, X, offset_x, 1);
            XMAX = Math.Abs(X[J + o_x]);
            XBND = XMAX;
            if (NOTRAN)
            {
                // *
                // *        Compute the growth in A * x = b.
                // *
                if (UPPER)
                {
                    JFIRST = N;
                    JLAST = 1;
                    JINC =  - 1;
                }
                else
                {
                    JFIRST = 1;
                    JLAST = N;
                    JINC = 1;
                }
                // *
                if (TSCAL != ONE)
                {
                    GROW = ZERO;
                    goto LABEL50;
                }
                // *
                if (NOUNIT)
                {
                    // *
                    // *           A is non-unit triangular.
                    // *
                    // *           Compute GROW = 1/G(j) and XBND = 1/M(j).
                    // *           Initially, G(0) = max{x(i), i=1,...,n}.
                    // *
                    GROW = ONE / Math.Max(XBND, SMLNUM);
                    XBND = GROW;
                    for (J = JFIRST; (JINC >= 0) ? (J <= JLAST) : (J >= JLAST); J += JINC)
                    {
                        // *
                        // *              Exit the loop if the growth factor is too small.
                        // *
                        if (GROW <= SMLNUM) goto LABEL50;
                        // *
                        // *              M(j) = G(j-1) / abs(A(j,j))
                        // *
                        TJJ = Math.Abs(A[J+J * LDA + o_a]);
                        XBND = Math.Min(XBND, Math.Min(ONE, TJJ) * GROW);
                        if (TJJ + CNORM[J + o_cnorm] >= SMLNUM)
                        {
                            // *
                            // *                 G(j) = G(j-1)*( 1 + CNORM(j) / abs(A(j,j)) )
                            // *
                            GROW = GROW * (TJJ / (TJJ + CNORM[J + o_cnorm]));
                        }
                        else
                        {
                            // *
                            // *                 G(j) could overflow, set GROW to 0.
                            // *
                            GROW = ZERO;
                        }
                    }
                    GROW = XBND;
                }
                else
                {
                    // *
                    // *           A is unit triangular.
                    // *
                    // *           Compute GROW = 1/G(j), where G(0) = max{x(i), i=1,...,n}.
                    // *
                    GROW = Math.Min(ONE, ONE / Math.Max(XBND, SMLNUM));
                    for (J = JFIRST; (JINC >= 0) ? (J <= JLAST) : (J >= JLAST); J += JINC)
                    {
                        // *
                        // *              Exit the loop if the growth factor is too small.
                        // *
                        if (GROW <= SMLNUM) goto LABEL50;
                        // *
                        // *              G(j) = G(j-1)*( 1 + CNORM(j) )
                        // *
                        GROW = GROW * (ONE / (ONE + CNORM[J + o_cnorm]));
                    }
                }
            LABEL50:;
                // *
            }
            else
            {
                // *
                // *        Compute the growth in A' * x = b.
                // *
                if (UPPER)
                {
                    JFIRST = 1;
                    JLAST = N;
                    JINC = 1;
                }
                else
                {
                    JFIRST = N;
                    JLAST = 1;
                    JINC =  - 1;
                }
                // *
                if (TSCAL != ONE)
                {
                    GROW = ZERO;
                    goto LABEL80;
                }
                // *
                if (NOUNIT)
                {
                    // *
                    // *           A is non-unit triangular.
                    // *
                    // *           Compute GROW = 1/G(j) and XBND = 1/M(j).
                    // *           Initially, M(0) = max{x(i), i=1,...,n}.
                    // *
                    GROW = ONE / Math.Max(XBND, SMLNUM);
                    XBND = GROW;
                    for (J = JFIRST; (JINC >= 0) ? (J <= JLAST) : (J >= JLAST); J += JINC)
                    {
                        // *
                        // *              Exit the loop if the growth factor is too small.
                        // *
                        if (GROW <= SMLNUM) goto LABEL80;
                        // *
                        // *              G(j) = max( G(j-1), M(j-1)*( 1 + CNORM(j) ) )
                        // *
                        XJ = ONE + CNORM[J + o_cnorm];
                        GROW = Math.Min(GROW, XBND / XJ);
                        // *
                        // *              M(j) = M(j-1)*( 1 + CNORM(j) ) / abs(A(j,j))
                        // *
                        TJJ = Math.Abs(A[J+J * LDA + o_a]);
                        if (XJ > TJJ) XBND = XBND * (TJJ / XJ);
                    }
                    GROW = Math.Min(GROW, XBND);
                }
                else
                {
                    // *
                    // *           A is unit triangular.
                    // *
                    // *           Compute GROW = 1/G(j), where G(0) = max{x(i), i=1,...,n}.
                    // *
                    GROW = Math.Min(ONE, ONE / Math.Max(XBND, SMLNUM));
                    for (J = JFIRST; (JINC >= 0) ? (J <= JLAST) : (J >= JLAST); J += JINC)
                    {
                        // *
                        // *              Exit the loop if the growth factor is too small.
                        // *
                        if (GROW <= SMLNUM) goto LABEL80;
                        // *
                        // *              G(j) = ( 1 + CNORM(j) )*G(j-1)
                        // *
                        XJ = ONE + CNORM[J + o_cnorm];
                        GROW /= XJ;
                    }
                }
            LABEL80:;
            }
            // *
            if ((GROW * TSCAL) > SMLNUM)
            {
                // *
                // *        Use the Level 2 BLAS solve if the reciprocal of the bound on
                // *        elements of X is not too small.
                // *
                this._dtrsv.Run(UPLO, TRANS, DIAG, N, A, offset_a, LDA
                                , ref X, offset_x, 1);
            }
            else
            {
                // *
                // *        Use a Level 1 BLAS solve, scaling intermediate results.
                // *
                if (XMAX > BIGNUM)
                {
                    // *
                    // *           Scale X so that its components are less than or equal to
                    // *           BIGNUM in absolute value.
                    // *
                    SCALE = BIGNUM / XMAX;
                    this._dscal.Run(N, SCALE, ref X, offset_x, 1);
                    XMAX = BIGNUM;
                }
                // *
                if (NOTRAN)
                {
                    // *
                    // *           Solve A * x = b
                    // *
                    for (J = JFIRST; (JINC >= 0) ? (J <= JLAST) : (J >= JLAST); J += JINC)
                    {
                        // *
                        // *              Compute x(j) = b(j) / A(j,j), scaling x if necessary.
                        // *
                        XJ = Math.Abs(X[J + o_x]);
                        if (NOUNIT)
                        {
                            TJJS = A[J+J * LDA + o_a] * TSCAL;
                        }
                        else
                        {
                            TJJS = TSCAL;
                            if (TSCAL == ONE) goto LABEL100;
                        }
                        TJJ = Math.Abs(TJJS);
                        if (TJJ > SMLNUM)
                        {
                            // *
                            // *                    abs(A(j,j)) > SMLNUM:
                            // *
                            if (TJJ < ONE)
                            {
                                if (XJ > TJJ * BIGNUM)
                                {
                                    // *
                                    // *                          Scale x by 1/b(j).
                                    // *
                                    REC = ONE / XJ;
                                    this._dscal.Run(N, REC, ref X, offset_x, 1);
                                    SCALE *= REC;
                                    XMAX *= REC;
                                }
                            }
                            X[J + o_x] /= TJJS;
                            XJ = Math.Abs(X[J + o_x]);
                        }
                        else
                        {
                            if (TJJ > ZERO)
                            {
                                // *
                                // *                    0 < abs(A(j,j)) <= SMLNUM:
                                // *
                                if (XJ > TJJ * BIGNUM)
                                {
                                    // *
                                    // *                       Scale x by (1/abs(x(j)))*abs(A(j,j))*BIGNUM
                                    // *                       to avoid overflow when dividing by A(j,j).
                                    // *
                                    REC = (TJJ * BIGNUM) / XJ;
                                    if (CNORM[J + o_cnorm] > ONE)
                                    {
                                        // *
                                        // *                          Scale by 1/CNORM(j) to avoid overflow when
                                        // *                          multiplying x(j) times column j.
                                        // *
                                        REC /= CNORM[J + o_cnorm];
                                    }
                                    this._dscal.Run(N, REC, ref X, offset_x, 1);
                                    SCALE *= REC;
                                    XMAX *= REC;
                                }
                                X[J + o_x] /= TJJS;
                                XJ = Math.Abs(X[J + o_x]);
                            }
                            else
                            {
                                // *
                                // *                    A(j,j) = 0:  Set x(1:n) = 0, x(j) = 1, and
                                // *                    scale = 0, and compute a solution to A*x = 0.
                                // *
                                for (I = 1; I <= N; I++)
                                {
                                    X[I + o_x] = ZERO;
                                }
                                X[J + o_x] = ONE;
                                XJ = ONE;
                                SCALE = ZERO;
                                XMAX = ZERO;
                            }
                        }
                    LABEL100:;
                        // *
                        // *              Scale x if necessary to avoid overflow when adding a
                        // *              multiple of column j of A.
                        // *
                        if (XJ > ONE)
                        {
                            REC = ONE / XJ;
                            if (CNORM[J + o_cnorm] > (BIGNUM - XMAX) * REC)
                            {
                                // *
                                // *                    Scale x by 1/(2*abs(x(j))).
                                // *
                                REC *= HALF;
                                this._dscal.Run(N, REC, ref X, offset_x, 1);
                                SCALE *= REC;
                            }
                        }
                        else
                        {
                            if (XJ * CNORM[J + o_cnorm] > (BIGNUM - XMAX))
                            {
                                // *
                                // *                 Scale x by 1/2.
                                // *
                                this._dscal.Run(N, HALF, ref X, offset_x, 1);
                                SCALE *= HALF;
                            }
                        }
                        // *
                        if (UPPER)
                        {
                            if (J > 1)
                            {
                                // *
                                // *                    Compute the update
                                // *                       x(1:j-1) := x(1:j-1) - x(j) * A(1:j-1,j)
                                // *
                                this._daxpy.Run(J - 1,  - X[J + o_x] * TSCAL, A, 1+J * LDA + o_a, 1, ref X, offset_x, 1);
                                I = this._idamax.Run(J - 1, X, offset_x, 1);
                                XMAX = Math.Abs(X[I + o_x]);
                            }
                        }
                        else
                        {
                            if (J < N)
                            {
                                // *
                                // *                    Compute the update
                                // *                       x(j+1:n) := x(j+1:n) - x(j) * A(j+1:n,j)
                                // *
                                this._daxpy.Run(N - J,  - X[J + o_x] * TSCAL, A, J + 1+J * LDA + o_a, 1, ref X, J + 1 + o_x, 1);
                                I = J + this._idamax.Run(N - J, X, J + 1 + o_x, 1);
                                XMAX = Math.Abs(X[I + o_x]);
                            }
                        }
                    }
                    // *
                }
                else
                {
                    // *
                    // *           Solve A' * x = b
                    // *
                    for (J = JFIRST; (JINC >= 0) ? (J <= JLAST) : (J >= JLAST); J += JINC)
                    {
                        // *
                        // *              Compute x(j) = b(j) - sum A(k,j)*x(k).
                        // *                                    k<>j
                        // *
                        XJ = Math.Abs(X[J + o_x]);
                        USCAL = TSCAL;
                        REC = ONE / Math.Max(XMAX, ONE);
                        if (CNORM[J + o_cnorm] > (BIGNUM - XJ) * REC)
                        {
                            // *
                            // *                 If x(j) could overflow, scale x by 1/(2*XMAX).
                            // *
                            REC *= HALF;
                            if (NOUNIT)
                            {
                                TJJS = A[J+J * LDA + o_a] * TSCAL;
                            }
                            else
                            {
                                TJJS = TSCAL;
                            }
                            TJJ = Math.Abs(TJJS);
                            if (TJJ > ONE)
                            {
                                // *
                                // *                       Divide by A(j,j) when scaling x if A(j,j) > 1.
                                // *
                                REC = Math.Min(ONE, REC * TJJ);
                                USCAL /= TJJS;
                            }
                            if (REC < ONE)
                            {
                                this._dscal.Run(N, REC, ref X, offset_x, 1);
                                SCALE *= REC;
                                XMAX *= REC;
                            }
                        }
                        // *
                        SUMJ = ZERO;
                        if (USCAL == ONE)
                        {
                            // *
                            // *                 If the scaling needed for A in the dot product is 1,
                            // *                 call DDOT to perform the dot product.
                            // *
                            if (UPPER)
                            {
                                SUMJ = this._ddot.Run(J - 1, A, 1+J * LDA + o_a, 1, X, offset_x, 1);
                            }
                            else
                            {
                                if (J < N)
                                {
                                    SUMJ = this._ddot.Run(N - J, A, J + 1+J * LDA + o_a, 1, X, J + 1 + o_x, 1);
                                }
                            }
                        }
                        else
                        {
                            // *
                            // *                 Otherwise, use in-line code for the dot product.
                            // *
                            if (UPPER)
                            {
                                A_J = J * LDA + o_a;
                                for (I = 1; I <= J - 1; I++)
                                {
                                    SUMJ += (A[I + A_J] * USCAL) * X[I + o_x];
                                }
                            }
                            else
                            {
                                if (J < N)
                                {
                                    A_J = J * LDA + o_a;
                                    for (I = J + 1; I <= N; I++)
                                    {
                                        SUMJ += (A[I + A_J] * USCAL) * X[I + o_x];
                                    }
                                }
                            }
                        }
                        // *
                        if (USCAL == TSCAL)
                        {
                            // *
                            // *                 Compute x(j) := ( x(j) - sumj ) / A(j,j) if 1/A(j,j)
                            // *                 was not used to scale the dotproduct.
                            // *
                            X[J + o_x] -= SUMJ;
                            XJ = Math.Abs(X[J + o_x]);
                            if (NOUNIT)
                            {
                                TJJS = A[J+J * LDA + o_a] * TSCAL;
                            }
                            else
                            {
                                TJJS = TSCAL;
                                if (TSCAL == ONE) goto LABEL150;
                            }
                            // *
                            // *                    Compute x(j) = x(j) / A(j,j), scaling if necessary.
                            // *
                            TJJ = Math.Abs(TJJS);
                            if (TJJ > SMLNUM)
                            {
                                // *
                                // *                       abs(A(j,j)) > SMLNUM:
                                // *
                                if (TJJ < ONE)
                                {
                                    if (XJ > TJJ * BIGNUM)
                                    {
                                        // *
                                        // *                             Scale X by 1/abs(x(j)).
                                        // *
                                        REC = ONE / XJ;
                                        this._dscal.Run(N, REC, ref X, offset_x, 1);
                                        SCALE *= REC;
                                        XMAX *= REC;
                                    }
                                }
                                X[J + o_x] /= TJJS;
                            }
                            else
                            {
                                if (TJJ > ZERO)
                                {
                                    // *
                                    // *                       0 < abs(A(j,j)) <= SMLNUM:
                                    // *
                                    if (XJ > TJJ * BIGNUM)
                                    {
                                        // *
                                        // *                          Scale x by (1/abs(x(j)))*abs(A(j,j))*BIGNUM.
                                        // *
                                        REC = (TJJ * BIGNUM) / XJ;
                                        this._dscal.Run(N, REC, ref X, offset_x, 1);
                                        SCALE *= REC;
                                        XMAX *= REC;
                                    }
                                    X[J + o_x] /= TJJS;
                                }
                                else
                                {
                                    // *
                                    // *                       A(j,j) = 0:  Set x(1:n) = 0, x(j) = 1, and
                                    // *                       scale = 0, and compute a solution to A'*x = 0.
                                    // *
                                    for (I = 1; I <= N; I++)
                                    {
                                        X[I + o_x] = ZERO;
                                    }
                                    X[J + o_x] = ONE;
                                    SCALE = ZERO;
                                    XMAX = ZERO;
                                }
                            }
                        LABEL150:;
                        }
                        else
                        {
                            // *
                            // *                 Compute x(j) := x(j) / A(j,j)  - sumj if the dot
                            // *                 product has already been divided by 1/A(j,j).
                            // *
                            X[J + o_x] = X[J + o_x] / TJJS - SUMJ;
                        }
                        XMAX = Math.Max(XMAX, Math.Abs(X[J + o_x]));
                    }
                }
                SCALE /= TSCAL;
            }
            // *
            // *     Scale the column norms by 1/TSCAL for return.
            // *
            if (TSCAL != ONE)
            {
                this._dscal.Run(N, ONE / TSCAL, ref CNORM, offset_cnorm, 1);
            }
            // *
            return;
            // *
            // *     End of DLATRS
            // *

            #endregion

        }
    }
}
