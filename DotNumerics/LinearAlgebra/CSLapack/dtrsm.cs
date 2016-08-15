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
    /// DTRSM  solves one of the matrix equations
    /// 
    /// op( A )*X = alpha*B,   or   X*op( A ) = alpha*B,
    /// 
    /// where alpha is a scalar, X and B are m by n matrices, A is a unit, or
    /// non-unit,  upper or lower triangular matrix  and  op( A )  is one  of
    /// 
    /// op( A ) = A   or   op( A ) = A'.
    /// 
    /// The matrix X is overwritten on B.
    /// 
    ///</summary>
    public class DTRSM
    {
    

        #region Dependencies
        
        LSAME _lsame; XERBLA _xerbla; 

        #endregion


        #region Variables
        
        const double ONE = 1.0E+0; const double ZERO = 0.0E+0; 

        #endregion

        public DTRSM(LSAME lsame, XERBLA xerbla)
        {
    

            #region Set Dependencies
            
            this._lsame = lsame; this._xerbla = xerbla; 

            #endregion

        }
    
        public DTRSM()
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
        /// DTRSM  solves one of the matrix equations
        /// 
        /// op( A )*X = alpha*B,   or   X*op( A ) = alpha*B,
        /// 
        /// where alpha is a scalar, X and B are m by n matrices, A is a unit, or
        /// non-unit,  upper or lower triangular matrix  and  op( A )  is one  of
        /// 
        /// op( A ) = A   or   op( A ) = A'.
        /// 
        /// The matrix X is overwritten on B.
        /// 
        ///</summary>
        /// <param name="SIDE">
        /// - CHARACTER*1.
        /// On entry, SIDE specifies whether op( A ) appears on the left
        /// or right of X as follows:
        /// 
        /// SIDE = 'L' or 'l'   op( A )*X = alpha*B.
        /// 
        /// SIDE = 'R' or 'r'   X*op( A ) = alpha*B.
        /// 
        /// Unchanged on exit.
        ///</param>
        /// <param name="UPLO">
        /// - CHARACTER*1.
        /// On entry, UPLO specifies whether the matrix A is an upper or
        /// lower triangular matrix as follows:
        /// 
        /// UPLO = 'U' or 'u'   A is an upper triangular matrix.
        /// 
        /// UPLO = 'L' or 'l'   A is a lower triangular matrix.
        /// 
        /// Unchanged on exit.
        ///</param>
        /// <param name="TRANSA">
        /// - CHARACTER*1.
        /// On entry, TRANSA specifies the form of op( A ) to be used in
        /// the matrix multiplication as follows:
        /// 
        /// TRANSA = 'N' or 'n'   op( A ) = A.
        /// 
        /// TRANSA = 'T' or 't'   op( A ) = A'.
        /// 
        /// TRANSA = 'C' or 'c'   op( A ) = A'.
        /// 
        /// Unchanged on exit.
        ///</param>
        /// <param name="DIAG">
        /// - CHARACTER*1.
        /// On entry, DIAG specifies whether or not A is unit triangular
        /// as follows:
        /// 
        /// DIAG = 'U' or 'u'   A is assumed to be unit triangular.
        /// 
        /// DIAG = 'N' or 'n'   A is not assumed to be unit
        /// triangular.
        /// 
        /// Unchanged on exit.
        ///</param>
        /// <param name="M">
        /// - INTEGER.
        /// On entry, M specifies the number of rows of B. M must be at
        /// least zero.
        /// Unchanged on exit.
        ///</param>
        /// <param name="N">
        /// - INTEGER.
        /// On entry, N specifies the number of columns of B.  N must be
        /// at least zero.
        /// Unchanged on exit.
        ///</param>
        /// <param name="ALPHA">
        /// - DOUBLE PRECISION.
        /// On entry,  ALPHA specifies the scalar  alpha. When  alpha is
        /// zero then  A is not referenced and  B need not be set before
        /// entry.
        /// Unchanged on exit.
        ///</param>
        /// <param name="A">
        /// - DOUBLE PRECISION array of DIMENSION ( LDA, k ), where k is m
        /// when  SIDE = 'L' or 'l'  and is  n  when  SIDE = 'R' or 'r'.
        /// Before entry  with  UPLO = 'U' or 'u',  the  leading  k by k
        /// upper triangular part of the array  A must contain the upper
        /// triangular matrix  and the strictly lower triangular part of
        /// A is not referenced.
        /// Before entry  with  UPLO = 'L' or 'l',  the  leading  k by k
        /// lower triangular part of the array  A must contain the lower
        /// triangular matrix  and the strictly upper triangular part of
        /// A is not referenced.
        /// Note that when  DIAG = 'U' or 'u',  the diagonal elements of
        /// A  are not referenced either,  but are assumed to be  unity.
        /// Unchanged on exit.
        ///</param>
        /// <param name="LDA">
        /// - INTEGER.
        /// On entry, LDA specifies the first dimension of A as declared
        /// in the calling (sub) program.  When  SIDE = 'L' or 'l'  then
        /// LDA  must be at least  max( 1, m ),  when  SIDE = 'R' or 'r'
        /// then LDA must be at least max( 1, n ).
        /// Unchanged on exit.
        ///</param>
        /// <param name="B">
        /// - DOUBLE PRECISION array of DIMENSION ( LDB, n ).
        /// Before entry,  the leading  m by n part of the array  B must
        /// contain  the  right-hand  side  matrix  B,  and  on exit  is
        /// overwritten by the solution matrix  X.
        ///</param>
        /// <param name="LDB">
        /// - INTEGER.
        /// On entry, LDB specifies the first dimension of B as declared
        /// in  the  calling  (sub)  program.   LDB  must  be  at  least
        /// max( 1, m ).
        /// Unchanged on exit.
        /// 
        ///</param>
        public void Run(string SIDE, string UPLO, string TRANSA, string DIAG, int M, int N
                         , double ALPHA, double[] A, int offset_a, int LDA, ref double[] B, int offset_b, int LDB)
        {

            #region Variables
            
            double TEMP = 0; int I = 0; int INFO = 0; int J = 0; int K = 0; int NROWA = 0; bool LSIDE = false; 
            bool NOUNIT = false;bool UPPER = false; 

            #endregion


            #region Implicit Variables
            
            int B_J = 0; int A_K = 0; int A_I = 0; int B_K = 0; 

            #endregion


            #region Array Index Correction
            
             int o_a = -1 - LDA + offset_a;  int o_b = -1 - LDB + offset_b; 

            #endregion


            #region Strings
            
            SIDE = SIDE.Substring(0, 1);  UPLO = UPLO.Substring(0, 1);  TRANSA = TRANSA.Substring(0, 1);  
            DIAG = DIAG.Substring(0, 1); 

            #endregion


            #region Prolog
            
            // *     .. Scalar Arguments ..
            // *     ..
            // *     .. Array Arguments ..
            // *     ..
            // *
            // *  Purpose
            // *  =======
            // *
            // *  DTRSM  solves one of the matrix equations
            // *
            // *     op( A )*X = alpha*B,   or   X*op( A ) = alpha*B,
            // *
            // *  where alpha is a scalar, X and B are m by n matrices, A is a unit, or
            // *  non-unit,  upper or lower triangular matrix  and  op( A )  is one  of
            // *
            // *     op( A ) = A   or   op( A ) = A'.
            // *
            // *  The matrix X is overwritten on B.
            // *
            // *  Arguments
            // *  ==========
            // *
            // *  SIDE   - CHARACTER*1.
            // *           On entry, SIDE specifies whether op( A ) appears on the left
            // *           or right of X as follows:
            // *
            // *              SIDE = 'L' or 'l'   op( A )*X = alpha*B.
            // *
            // *              SIDE = 'R' or 'r'   X*op( A ) = alpha*B.
            // *
            // *           Unchanged on exit.
            // *
            // *  UPLO   - CHARACTER*1.
            // *           On entry, UPLO specifies whether the matrix A is an upper or
            // *           lower triangular matrix as follows:
            // *
            // *              UPLO = 'U' or 'u'   A is an upper triangular matrix.
            // *
            // *              UPLO = 'L' or 'l'   A is a lower triangular matrix.
            // *
            // *           Unchanged on exit.
            // *
            // *  TRANSA - CHARACTER*1.
            // *           On entry, TRANSA specifies the form of op( A ) to be used in
            // *           the matrix multiplication as follows:
            // *
            // *              TRANSA = 'N' or 'n'   op( A ) = A.
            // *
            // *              TRANSA = 'T' or 't'   op( A ) = A'.
            // *
            // *              TRANSA = 'C' or 'c'   op( A ) = A'.
            // *
            // *           Unchanged on exit.
            // *
            // *  DIAG   - CHARACTER*1.
            // *           On entry, DIAG specifies whether or not A is unit triangular
            // *           as follows:
            // *
            // *              DIAG = 'U' or 'u'   A is assumed to be unit triangular.
            // *
            // *              DIAG = 'N' or 'n'   A is not assumed to be unit
            // *                                  triangular.
            // *
            // *           Unchanged on exit.
            // *
            // *  M      - INTEGER.
            // *           On entry, M specifies the number of rows of B. M must be at
            // *           least zero.
            // *           Unchanged on exit.
            // *
            // *  N      - INTEGER.
            // *           On entry, N specifies the number of columns of B.  N must be
            // *           at least zero.
            // *           Unchanged on exit.
            // *
            // *  ALPHA  - DOUBLE PRECISION.
            // *           On entry,  ALPHA specifies the scalar  alpha. When  alpha is
            // *           zero then  A is not referenced and  B need not be set before
            // *           entry.
            // *           Unchanged on exit.
            // *
            // *  A      - DOUBLE PRECISION array of DIMENSION ( LDA, k ), where k is m
            // *           when  SIDE = 'L' or 'l'  and is  n  when  SIDE = 'R' or 'r'.
            // *           Before entry  with  UPLO = 'U' or 'u',  the  leading  k by k
            // *           upper triangular part of the array  A must contain the upper
            // *           triangular matrix  and the strictly lower triangular part of
            // *           A is not referenced.
            // *           Before entry  with  UPLO = 'L' or 'l',  the  leading  k by k
            // *           lower triangular part of the array  A must contain the lower
            // *           triangular matrix  and the strictly upper triangular part of
            // *           A is not referenced.
            // *           Note that when  DIAG = 'U' or 'u',  the diagonal elements of
            // *           A  are not referenced either,  but are assumed to be  unity.
            // *           Unchanged on exit.
            // *
            // *  LDA    - INTEGER.
            // *           On entry, LDA specifies the first dimension of A as declared
            // *           in the calling (sub) program.  When  SIDE = 'L' or 'l'  then
            // *           LDA  must be at least  max( 1, m ),  when  SIDE = 'R' or 'r'
            // *           then LDA must be at least max( 1, n ).
            // *           Unchanged on exit.
            // *
            // *  B      - DOUBLE PRECISION array of DIMENSION ( LDB, n ).
            // *           Before entry,  the leading  m by n part of the array  B must
            // *           contain  the  right-hand  side  matrix  B,  and  on exit  is
            // *           overwritten by the solution matrix  X.
            // *
            // *  LDB    - INTEGER.
            // *           On entry, LDB specifies the first dimension of B as declared
            // *           in  the  calling  (sub)  program.   LDB  must  be  at  least
            // *           max( 1, m ).
            // *           Unchanged on exit.
            // *
            // *
            // *  Level 3 Blas routine.
            // *
            // *
            // *  -- Written on 8-February-1989.
            // *     Jack Dongarra, Argonne National Laboratory.
            // *     Iain Duff, AERE Harwell.
            // *     Jeremy Du Croz, Numerical Algorithms Group Ltd.
            // *     Sven Hammarling, Numerical Algorithms Group Ltd.
            // *
            // *
            // *     .. External Functions ..
            // *     ..
            // *     .. External Subroutines ..
            // *     ..
            // *     .. Intrinsic Functions ..
            //      INTRINSIC MAX;
            // *     ..
            // *     .. Local Scalars ..
            // *     ..
            // *     .. Parameters ..
            // *     ..
            // *
            // *     Test the input parameters.
            // *

            #endregion


            #region Body
            
            LSIDE = this._lsame.Run(SIDE, "L");
            if (LSIDE)
            {
                NROWA = M;
            }
            else
            {
                NROWA = N;
            }
            NOUNIT = this._lsame.Run(DIAG, "N");
            UPPER = this._lsame.Run(UPLO, "U");
            // *
            INFO = 0;
            if ((!LSIDE) && (!this._lsame.Run(SIDE, "R")))
            {
                INFO = 1;
            }
            else
            {
                if ((!UPPER) && (!this._lsame.Run(UPLO, "L")))
                {
                    INFO = 2;
                }
                else
                {
                    if ((!this._lsame.Run(TRANSA, "N")) && (!this._lsame.Run(TRANSA, "T")) && (!this._lsame.Run(TRANSA, "C")))
                    {
                        INFO = 3;
                    }
                    else
                    {
                        if ((!this._lsame.Run(DIAG, "U")) && (!this._lsame.Run(DIAG, "N")))
                        {
                            INFO = 4;
                        }
                        else
                        {
                            if (M < 0)
                            {
                                INFO = 5;
                            }
                            else
                            {
                                if (N < 0)
                                {
                                    INFO = 6;
                                }
                                else
                                {
                                    if (LDA < Math.Max(1, NROWA))
                                    {
                                        INFO = 9;
                                    }
                                    else
                                    {
                                        if (LDB < Math.Max(1, M))
                                        {
                                            INFO = 11;
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
            if (INFO != 0)
            {
                this._xerbla.Run("DTRSM ", INFO);
                return;
            }
            // *
            // *     Quick return if possible.
            // *
            if (N == 0) return;
            // *
            // *     And when  alpha.eq.zero.
            // *
            if (ALPHA == ZERO)
            {
                for (J = 1; J <= N; J++)
                {
                    B_J = J * LDB + o_b;
                    for (I = 1; I <= M; I++)
                    {
                        B[I + B_J] = ZERO;
                    }
                }
                return;
            }
            // *
            // *     Start the operations.
            // *
            if (LSIDE)
            {
                if (this._lsame.Run(TRANSA, "N"))
                {
                    // *
                    // *           Form  B := alpha*inv( A )*B.
                    // *
                    if (UPPER)
                    {
                        for (J = 1; J <= N; J++)
                        {
                            if (ALPHA != ONE)
                            {
                                B_J = J * LDB + o_b;
                                for (I = 1; I <= M; I++)
                                {
                                    B[I + B_J] *= ALPHA;
                                }
                            }
                            for (K = M; K >= 1; K +=  - 1)
                            {
                                if (B[K+J * LDB + o_b] != ZERO)
                                {
                                    if (NOUNIT) B[K+J * LDB + o_b] /= A[K+K * LDA + o_a];
                                    B_J = J * LDB + o_b;
                                    A_K = K * LDA + o_a;
                                    for (I = 1; I <= K - 1; I++)
                                    {
                                        B[I + B_J] +=  - B[K+J * LDB + o_b] * A[I + A_K];
                                    }
                                }
                            }
                        }
                    }
                    else
                    {
                        for (J = 1; J <= N; J++)
                        {
                            if (ALPHA != ONE)
                            {
                                B_J = J * LDB + o_b;
                                for (I = 1; I <= M; I++)
                                {
                                    B[I + B_J] *= ALPHA;
                                }
                            }
                            for (K = 1; K <= M; K++)
                            {
                                if (B[K+J * LDB + o_b] != ZERO)
                                {
                                    if (NOUNIT) B[K+J * LDB + o_b] /= A[K+K * LDA + o_a];
                                    B_J = J * LDB + o_b;
                                    A_K = K * LDA + o_a;
                                    for (I = K + 1; I <= M; I++)
                                    {
                                        B[I + B_J] +=  - B[K+J * LDB + o_b] * A[I + A_K];
                                    }
                                }
                            }
                        }
                    }
                }
                else
                {
                    // *
                    // *           Form  B := alpha*inv( A' )*B.
                    // *
                    if (UPPER)
                    {
                        for (J = 1; J <= N; J++)
                        {
                            B_J = J * LDB + o_b;
                            for (I = 1; I <= M; I++)
                            {
                                TEMP = ALPHA * B[I + B_J];
                                A_I = I * LDA + o_a;
                                B_J = J * LDB + o_b;
                                for (K = 1; K <= I - 1; K++)
                                {
                                    TEMP +=  - A[K + A_I] * B[K + B_J];
                                }
                                if (NOUNIT) TEMP /= A[I+I * LDA + o_a];
                                B[I + B_J] = TEMP;
                            }
                        }
                    }
                    else
                    {
                        for (J = 1; J <= N; J++)
                        {
                            B_J = J * LDB + o_b;
                            for (I = M; I >= 1; I +=  - 1)
                            {
                                TEMP = ALPHA * B[I + B_J];
                                A_I = I * LDA + o_a;
                                B_J = J * LDB + o_b;
                                for (K = I + 1; K <= M; K++)
                                {
                                    TEMP +=  - A[K + A_I] * B[K + B_J];
                                }
                                if (NOUNIT) TEMP /= A[I+I * LDA + o_a];
                                B[I + B_J] = TEMP;
                            }
                        }
                    }
                }
            }
            else
            {
                if (this._lsame.Run(TRANSA, "N"))
                {
                    // *
                    // *           Form  B := alpha*B*inv( A ).
                    // *
                    if (UPPER)
                    {
                        for (J = 1; J <= N; J++)
                        {
                            if (ALPHA != ONE)
                            {
                                B_J = J * LDB + o_b;
                                for (I = 1; I <= M; I++)
                                {
                                    B[I + B_J] *= ALPHA;
                                }
                            }
                            for (K = 1; K <= J - 1; K++)
                            {
                                if (A[K+J * LDA + o_a] != ZERO)
                                {
                                    B_J = J * LDB + o_b;
                                    B_K = K * LDB + o_b;
                                    for (I = 1; I <= M; I++)
                                    {
                                        B[I + B_J] +=  - A[K+J * LDA + o_a] * B[I + B_K];
                                    }
                                }
                            }
                            if (NOUNIT)
                            {
                                TEMP = ONE / A[J+J * LDA + o_a];
                                B_J = J * LDB + o_b;
                                for (I = 1; I <= M; I++)
                                {
                                    B[I + B_J] *= TEMP;
                                }
                            }
                        }
                    }
                    else
                    {
                        for (J = N; J >= 1; J +=  - 1)
                        {
                            if (ALPHA != ONE)
                            {
                                B_J = J * LDB + o_b;
                                for (I = 1; I <= M; I++)
                                {
                                    B[I + B_J] *= ALPHA;
                                }
                            }
                            for (K = J + 1; K <= N; K++)
                            {
                                if (A[K+J * LDA + o_a] != ZERO)
                                {
                                    B_J = J * LDB + o_b;
                                    B_K = K * LDB + o_b;
                                    for (I = 1; I <= M; I++)
                                    {
                                        B[I + B_J] +=  - A[K+J * LDA + o_a] * B[I + B_K];
                                    }
                                }
                            }
                            if (NOUNIT)
                            {
                                TEMP = ONE / A[J+J * LDA + o_a];
                                B_J = J * LDB + o_b;
                                for (I = 1; I <= M; I++)
                                {
                                    B[I + B_J] *= TEMP;
                                }
                            }
                        }
                    }
                }
                else
                {
                    // *
                    // *           Form  B := alpha*B*inv( A' ).
                    // *
                    if (UPPER)
                    {
                        for (K = N; K >= 1; K +=  - 1)
                        {
                            if (NOUNIT)
                            {
                                TEMP = ONE / A[K+K * LDA + o_a];
                                B_K = K * LDB + o_b;
                                for (I = 1; I <= M; I++)
                                {
                                    B[I + B_K] *= TEMP;
                                }
                            }
                            for (J = 1; J <= K - 1; J++)
                            {
                                if (A[J+K * LDA + o_a] != ZERO)
                                {
                                    TEMP = A[J+K * LDA + o_a];
                                    B_J = J * LDB + o_b;
                                    B_K = K * LDB + o_b;
                                    for (I = 1; I <= M; I++)
                                    {
                                        B[I + B_J] +=  - TEMP * B[I + B_K];
                                    }
                                }
                            }
                            if (ALPHA != ONE)
                            {
                                B_K = K * LDB + o_b;
                                for (I = 1; I <= M; I++)
                                {
                                    B[I + B_K] *= ALPHA;
                                }
                            }
                        }
                    }
                    else
                    {
                        for (K = 1; K <= N; K++)
                        {
                            if (NOUNIT)
                            {
                                TEMP = ONE / A[K+K * LDA + o_a];
                                B_K = K * LDB + o_b;
                                for (I = 1; I <= M; I++)
                                {
                                    B[I + B_K] *= TEMP;
                                }
                            }
                            for (J = K + 1; J <= N; J++)
                            {
                                if (A[J+K * LDA + o_a] != ZERO)
                                {
                                    TEMP = A[J+K * LDA + o_a];
                                    B_J = J * LDB + o_b;
                                    B_K = K * LDB + o_b;
                                    for (I = 1; I <= M; I++)
                                    {
                                        B[I + B_J] +=  - TEMP * B[I + B_K];
                                    }
                                }
                            }
                            if (ALPHA != ONE)
                            {
                                B_K = K * LDB + o_b;
                                for (I = 1; I <= M; I++)
                                {
                                    B[I + B_K] *= ALPHA;
                                }
                            }
                        }
                    }
                }
            }
            // *
            return;
            // *
            // *     End of DTRSM .
            // *

            #endregion

        }
    }
}
