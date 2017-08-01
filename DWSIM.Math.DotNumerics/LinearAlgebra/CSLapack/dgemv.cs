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
    /// Purpose
    /// =======
    /// 
    /// DGEMV  performs one of the matrix-vector operations
    /// 
    /// y := alpha*A*x + beta*y,   or   y := alpha*A'*x + beta*y,
    /// 
    /// where alpha and beta are scalars, x and y are vectors and A is an
    /// m by n matrix.
    /// 
    /// Parameters
    /// ==========
    /// 
    /// TRANS  - CHARACTER*1.
    /// On entry, TRANS specifies the operation to be performed as
    /// follows:
    /// 
    /// TRANS = 'N' or 'n'   y := alpha*A*x + beta*y.
    /// 
    /// TRANS = 'T' or 't'   y := alpha*A'*x + beta*y.
    /// 
    /// TRANS = 'C' or 'c'   y := alpha*A'*x + beta*y.
    /// 
    /// Unchanged on exit.
    /// 
    /// M      - INTEGER.
    /// On entry, M specifies the number of rows of the matrix A.
    /// M must be at least zero.
    /// Unchanged on exit.
    /// 
    /// N      - INTEGER.
    /// On entry, N specifies the number of columns of the matrix A.
    /// N must be at least zero.
    /// Unchanged on exit.
    /// 
    /// ALPHA  - DOUBLE PRECISION.
    /// On entry, ALPHA specifies the scalar alpha.
    /// Unchanged on exit.
    /// 
    /// A      - DOUBLE PRECISION array of DIMENSION ( LDA, n ).
    /// Before entry, the leading m by n part of the array A must
    /// contain the matrix of coefficients.
    /// Unchanged on exit.
    /// 
    /// LDA    - INTEGER.
    /// On entry, LDA specifies the first dimension of A as declared
    /// in the calling (sub) program. LDA must be at least
    /// max( 1, m ).
    /// Unchanged on exit.
    /// 
    /// X      - DOUBLE PRECISION array of DIMENSION at least
    /// ( 1 + ( n - 1 )*abs( INCX ) ) when TRANS = 'N' or 'n'
    /// and at least
    /// ( 1 + ( m - 1 )*abs( INCX ) ) otherwise.
    /// Before entry, the incremented array X must contain the
    /// vector x.
    /// Unchanged on exit.
    /// 
    /// INCX   - INTEGER.
    /// On entry, INCX specifies the increment for the elements of
    /// X. INCX must not be zero.
    /// Unchanged on exit.
    /// 
    /// BETA   - DOUBLE PRECISION.
    /// On entry, BETA specifies the scalar beta. When BETA is
    /// supplied as zero then Y need not be set on input.
    /// Unchanged on exit.
    /// 
    /// Y      - DOUBLE PRECISION array of DIMENSION at least
    /// ( 1 + ( m - 1 )*abs( INCY ) ) when TRANS = 'N' or 'n'
    /// and at least
    /// ( 1 + ( n - 1 )*abs( INCY ) ) otherwise.
    /// Before entry with BETA non-zero, the incremented array Y
    /// must contain the vector y. On exit, Y is overwritten by the
    /// updated vector y.
    /// 
    /// INCY   - INTEGER.
    /// On entry, INCY specifies the increment for the elements of
    /// Y. INCY must not be zero.
    /// Unchanged on exit.
    ///</summary>
    public class DGEMV
    {
    

        #region Dependencies
        
        LSAME _lsame; XERBLA _xerbla; 

        #endregion


        #region Variables
        
        const double ONE = 1.0E+0; const double ZERO = 0.0E+0; 

        #endregion

        public DGEMV(LSAME lsame, XERBLA xerbla)
        {
    

            #region Set Dependencies
            
            this._lsame = lsame; this._xerbla = xerbla; 

            #endregion

        }
    
        public DGEMV()
        {
    

            #region Dependencies (Initialization)
            
            LSAME lsame = new LSAME();
            XERBLA xerbla = new XERBLA();

            #endregion


            #region Set Dependencies
            
            this._lsame = lsame; this._xerbla = xerbla; 

            #endregion

        }
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DGEMV  performs one of the matrix-vector operations
        /// 
        /// y := alpha*A*x + beta*y,   or   y := alpha*A'*x + beta*y,
        /// 
        /// where alpha and beta are scalars, x and y are vectors and A is an
        /// m by n matrix.
        /// 
        /// Parameters
        /// ==========
        /// 
        /// TRANS  - CHARACTER*1.
        /// On entry, TRANS specifies the operation to be performed as
        /// follows:
        /// 
        /// TRANS = 'N' or 'n'   y := alpha*A*x + beta*y.
        /// 
        /// TRANS = 'T' or 't'   y := alpha*A'*x + beta*y.
        /// 
        /// TRANS = 'C' or 'c'   y := alpha*A'*x + beta*y.
        /// 
        /// Unchanged on exit.
        /// 
        /// M      - INTEGER.
        /// On entry, M specifies the number of rows of the matrix A.
        /// M must be at least zero.
        /// Unchanged on exit.
        /// 
        /// N      - INTEGER.
        /// On entry, N specifies the number of columns of the matrix A.
        /// N must be at least zero.
        /// Unchanged on exit.
        /// 
        /// ALPHA  - DOUBLE PRECISION.
        /// On entry, ALPHA specifies the scalar alpha.
        /// Unchanged on exit.
        /// 
        /// A      - DOUBLE PRECISION array of DIMENSION ( LDA, n ).
        /// Before entry, the leading m by n part of the array A must
        /// contain the matrix of coefficients.
        /// Unchanged on exit.
        /// 
        /// LDA    - INTEGER.
        /// On entry, LDA specifies the first dimension of A as declared
        /// in the calling (sub) program. LDA must be at least
        /// max( 1, m ).
        /// Unchanged on exit.
        /// 
        /// X      - DOUBLE PRECISION array of DIMENSION at least
        /// ( 1 + ( n - 1 )*abs( INCX ) ) when TRANS = 'N' or 'n'
        /// and at least
        /// ( 1 + ( m - 1 )*abs( INCX ) ) otherwise.
        /// Before entry, the incremented array X must contain the
        /// vector x.
        /// Unchanged on exit.
        /// 
        /// INCX   - INTEGER.
        /// On entry, INCX specifies the increment for the elements of
        /// X. INCX must not be zero.
        /// Unchanged on exit.
        /// 
        /// BETA   - DOUBLE PRECISION.
        /// On entry, BETA specifies the scalar beta. When BETA is
        /// supplied as zero then Y need not be set on input.
        /// Unchanged on exit.
        /// 
        /// Y      - DOUBLE PRECISION array of DIMENSION at least
        /// ( 1 + ( m - 1 )*abs( INCY ) ) when TRANS = 'N' or 'n'
        /// and at least
        /// ( 1 + ( n - 1 )*abs( INCY ) ) otherwise.
        /// Before entry with BETA non-zero, the incremented array Y
        /// must contain the vector y. On exit, Y is overwritten by the
        /// updated vector y.
        /// 
        /// INCY   - INTEGER.
        /// On entry, INCY specifies the increment for the elements of
        /// Y. INCY must not be zero.
        /// Unchanged on exit.
        ///</summary>
        /// <param name="TRANS">
        /// - CHARACTER*1.
        /// On entry, TRANS specifies the operation to be performed as
        /// follows:
        /// 
        /// TRANS = 'N' or 'n'   y := alpha*A*x + beta*y.
        /// 
        /// TRANS = 'T' or 't'   y := alpha*A'*x + beta*y.
        /// 
        /// TRANS = 'C' or 'c'   y := alpha*A'*x + beta*y.
        /// 
        /// Unchanged on exit.
        ///</param>
        /// <param name="M">
        /// - INTEGER.
        /// On entry, M specifies the number of rows of the matrix A.
        /// M must be at least zero.
        /// Unchanged on exit.
        ///</param>
        /// <param name="N">
        /// - INTEGER.
        /// On entry, N specifies the number of columns of the matrix A.
        /// N must be at least zero.
        /// Unchanged on exit.
        ///</param>
        /// <param name="ALPHA">
        /// - DOUBLE PRECISION.
        /// On entry, ALPHA specifies the scalar alpha.
        /// Unchanged on exit.
        ///</param>
        /// <param name="A">
        /// - DOUBLE PRECISION array of DIMENSION ( LDA, n ).
        /// Before entry, the leading m by n part of the array A must
        /// contain the matrix of coefficients.
        /// Unchanged on exit.
        ///</param>
        /// <param name="LDA">
        /// - INTEGER.
        /// On entry, LDA specifies the first dimension of A as declared
        /// in the calling (sub) program. LDA must be at least
        /// max( 1, m ).
        /// Unchanged on exit.
        ///</param>
        /// <param name="X">
        /// - DOUBLE PRECISION array of DIMENSION at least
        /// ( 1 + ( n - 1 )*abs( INCX ) ) when TRANS = 'N' or 'n'
        /// and at least
        /// ( 1 + ( m - 1 )*abs( INCX ) ) otherwise.
        /// Before entry, the incremented array X must contain the
        /// vector x.
        /// Unchanged on exit.
        ///</param>
        /// <param name="INCX">
        /// - INTEGER.
        /// On entry, INCX specifies the increment for the elements of
        /// X. INCX must not be zero.
        /// Unchanged on exit.
        ///</param>
        /// <param name="BETA">
        /// - DOUBLE PRECISION.
        /// On entry, BETA specifies the scalar beta. When BETA is
        /// supplied as zero then Y need not be set on input.
        /// Unchanged on exit.
        ///</param>
        /// <param name="Y">
        /// := alpha*A*x + beta*y,   or   y := alpha*A'*x + beta*y,
        ///</param>
        /// <param name="INCY">
        /// - INTEGER.
        /// On entry, INCY specifies the increment for the elements of
        /// Y. INCY must not be zero.
        /// Unchanged on exit.
        /// 
        ///</param>
        public void Run(string TRANS, int M, int N, double ALPHA, double[] A, int offset_a, int LDA
                         , double[] X, int offset_x, int INCX, double BETA, ref double[] Y, int offset_y, int INCY)
        {

            #region Variables
            
            double TEMP = 0; int I = 0; int INFO = 0; int IX = 0; int IY = 0; int J = 0; int JX = 0; int JY = 0; int KX = 0; 
            int KY = 0;int LENX = 0; int LENY = 0; 

            #endregion


            #region Implicit Variables
            
            int A_J = 0; 

            #endregion


            #region Array Index Correction
            
             int o_a = -1 - LDA + offset_a;  int o_x = -1 + offset_x;  int o_y = -1 + offset_y; 

            #endregion


            #region Strings
            
            TRANS = TRANS.Substring(0, 1);  

            #endregion


            #region Prolog
            
            // *     .. Scalar Arguments ..
            // *     .. Array Arguments ..
            // *     ..
            // *
            // *  Purpose
            // *  =======
            // *
            // *  DGEMV  performs one of the matrix-vector operations
            // *
            // *     y := alpha*A*x + beta*y,   or   y := alpha*A'*x + beta*y,
            // *
            // *  where alpha and beta are scalars, x and y are vectors and A is an
            // *  m by n matrix.
            // *
            // *  Parameters
            // *  ==========
            // *
            // *  TRANS  - CHARACTER*1.
            // *           On entry, TRANS specifies the operation to be performed as
            // *           follows:
            // *
            // *              TRANS = 'N' or 'n'   y := alpha*A*x + beta*y.
            // *
            // *              TRANS = 'T' or 't'   y := alpha*A'*x + beta*y.
            // *
            // *              TRANS = 'C' or 'c'   y := alpha*A'*x + beta*y.
            // *
            // *           Unchanged on exit.
            // *
            // *  M      - INTEGER.
            // *           On entry, M specifies the number of rows of the matrix A.
            // *           M must be at least zero.
            // *           Unchanged on exit.
            // *
            // *  N      - INTEGER.
            // *           On entry, N specifies the number of columns of the matrix A.
            // *           N must be at least zero.
            // *           Unchanged on exit.
            // *
            // *  ALPHA  - DOUBLE PRECISION.
            // *           On entry, ALPHA specifies the scalar alpha.
            // *           Unchanged on exit.
            // *
            // *  A      - DOUBLE PRECISION array of DIMENSION ( LDA, n ).
            // *           Before entry, the leading m by n part of the array A must
            // *           contain the matrix of coefficients.
            // *           Unchanged on exit.
            // *
            // *  LDA    - INTEGER.
            // *           On entry, LDA specifies the first dimension of A as declared
            // *           in the calling (sub) program. LDA must be at least
            // *           max( 1, m ).
            // *           Unchanged on exit.
            // *
            // *  X      - DOUBLE PRECISION array of DIMENSION at least
            // *           ( 1 + ( n - 1 )*abs( INCX ) ) when TRANS = 'N' or 'n'
            // *           and at least
            // *           ( 1 + ( m - 1 )*abs( INCX ) ) otherwise.
            // *           Before entry, the incremented array X must contain the
            // *           vector x.
            // *           Unchanged on exit.
            // *
            // *  INCX   - INTEGER.
            // *           On entry, INCX specifies the increment for the elements of
            // *           X. INCX must not be zero.
            // *           Unchanged on exit.
            // *
            // *  BETA   - DOUBLE PRECISION.
            // *           On entry, BETA specifies the scalar beta. When BETA is
            // *           supplied as zero then Y need not be set on input.
            // *           Unchanged on exit.
            // *
            // *  Y      - DOUBLE PRECISION array of DIMENSION at least
            // *           ( 1 + ( m - 1 )*abs( INCY ) ) when TRANS = 'N' or 'n'
            // *           and at least
            // *           ( 1 + ( n - 1 )*abs( INCY ) ) otherwise.
            // *           Before entry with BETA non-zero, the incremented array Y
            // *           must contain the vector y. On exit, Y is overwritten by the
            // *           updated vector y.
            // *
            // *  INCY   - INTEGER.
            // *           On entry, INCY specifies the increment for the elements of
            // *           Y. INCY must not be zero.
            // *           Unchanged on exit.
            // *
            // *
            // *  Level 2 Blas routine.
            // *
            // *  -- Written on 22-October-1986.
            // *     Jack Dongarra, Argonne National Lab.
            // *     Jeremy Du Croz, Nag Central Office.
            // *     Sven Hammarling, Nag Central Office.
            // *     Richard Hanson, Sandia National Labs.
            // *
            // *
            // *     .. Parameters ..
            // *     .. Local Scalars ..
            // *     .. External Functions ..
            // *     .. External Subroutines ..
            // *     .. Intrinsic Functions ..
            //      INTRINSIC          MAX;
            // *     ..
            // *     .. Executable Statements ..
            // *
            // *     Test the input parameters.
            // *

            #endregion


            #region Body
            
            INFO = 0;
            if (!this._lsame.Run(TRANS, "N") && !this._lsame.Run(TRANS, "T") && !this._lsame.Run(TRANS, "C"))
            {
                INFO = 1;
            }
            else
            {
                if (M < 0)
                {
                    INFO = 2;
                }
                else
                {
                    if (N < 0)
                    {
                        INFO = 3;
                    }
                    else
                    {
                        if (LDA < Math.Max(1, M))
                        {
                            INFO = 6;
                        }
                        else
                        {
                            if (INCX == 0)
                            {
                                INFO = 8;
                            }
                            else
                            {
                                if (INCY == 0)
                                {
                                    INFO = 11;
                                }
                            }
                        }
                    }
                }
            }
            if (INFO != 0)
            {
                this._xerbla.Run("DGEMV ", INFO);
                return;
            }
            // *
            // *     Quick return if possible.
            // *
            if ((M == 0) || (N == 0) || ((ALPHA == ZERO) && (BETA == ONE))) return;
            // *
            // *     Set  LENX  and  LENY, the lengths of the vectors x and y, and set
            // *     up the start points in  X  and  Y.
            // *
            if (this._lsame.Run(TRANS, "N"))
            {
                LENX = N;
                LENY = M;
            }
            else
            {
                LENX = M;
                LENY = N;
            }
            if (INCX > 0)
            {
                KX = 1;
            }
            else
            {
                KX = 1 - (LENX - 1) * INCX;
            }
            if (INCY > 0)
            {
                KY = 1;
            }
            else
            {
                KY = 1 - (LENY - 1) * INCY;
            }
            // *
            // *     Start the operations. In this version the elements of A are
            // *     accessed sequentially with one pass through A.
            // *
            // *     First form  y := beta*y.
            // *
            if (BETA != ONE)
            {
                if (INCY == 1)
                {
                    if (BETA == ZERO)
                    {
                        for (I = 1; I <= LENY; I++)
                        {
                            Y[I + o_y] = ZERO;
                        }
                    }
                    else
                    {
                        for (I = 1; I <= LENY; I++)
                        {
                            Y[I + o_y] *= BETA;
                        }
                    }
                }
                else
                {
                    IY = KY;
                    if (BETA == ZERO)
                    {
                        for (I = 1; I <= LENY; I++)
                        {
                            Y[IY + o_y] = ZERO;
                            IY += INCY;
                        }
                    }
                    else
                    {
                        for (I = 1; I <= LENY; I++)
                        {
                            Y[IY + o_y] *= BETA;
                            IY += INCY;
                        }
                    }
                }
            }
            if (ALPHA == ZERO) return;
            if (this._lsame.Run(TRANS, "N"))
            {
                // *
                // *        Form  y := alpha*A*x + y.
                // *
                JX = KX;
                if (INCY == 1)
                {
                    for (J = 1; J <= N; J++)
                    {
                        if (X[JX + o_x] != ZERO)
                        {
                            TEMP = ALPHA * X[JX + o_x];
                            A_J = J * LDA + o_a;
                            for (I = 1; I <= M; I++)
                            {
                                Y[I + o_y] += TEMP * A[I + A_J];
                            }
                        }
                        JX += INCX;
                    }
                }
                else
                {
                    for (J = 1; J <= N; J++)
                    {
                        if (X[JX + o_x] != ZERO)
                        {
                            TEMP = ALPHA * X[JX + o_x];
                            IY = KY;
                            A_J = J * LDA + o_a;
                            for (I = 1; I <= M; I++)
                            {
                                Y[IY + o_y] += TEMP * A[I + A_J];
                                IY += INCY;
                            }
                        }
                        JX += INCX;
                    }
                }
            }
            else
            {
                // *
                // *        Form  y := alpha*A'*x + y.
                // *
                JY = KY;
                if (INCX == 1)
                {
                    for (J = 1; J <= N; J++)
                    {
                        TEMP = ZERO;
                        A_J = J * LDA + o_a;
                        for (I = 1; I <= M; I++)
                        {
                            TEMP += A[I + A_J] * X[I + o_x];
                        }
                        Y[JY + o_y] += ALPHA * TEMP;
                        JY += INCY;
                    }
                }
                else
                {
                    for (J = 1; J <= N; J++)
                    {
                        TEMP = ZERO;
                        IX = KX;
                        A_J = J * LDA + o_a;
                        for (I = 1; I <= M; I++)
                        {
                            TEMP += A[I + A_J] * X[IX + o_x];
                            IX += INCX;
                        }
                        Y[JY + o_y] += ALPHA * TEMP;
                        JY += INCY;
                    }
                }
            }
            // *
            return;
            // *
            // *     End of DGEMV .
            // *

            #endregion

        }
    }
}
