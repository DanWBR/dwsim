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
    /// DSYR2K  performs one of the symmetric rank 2k operations
    /// 
    /// C := alpha*A*B' + alpha*B*A' + beta*C,
    /// 
    /// or
    /// 
    /// C := alpha*A'*B + alpha*B'*A + beta*C,
    /// 
    /// where  alpha and beta  are scalars, C is an  n by n  symmetric matrix
    /// and  A and B  are  n by k  matrices  in the  first  case  and  k by n
    /// matrices in the second case.
    /// 
    ///</summary>
    public class DSYR2K
    {
    

        #region Dependencies
        
        LSAME _lsame; XERBLA _xerbla; 

        #endregion


        #region Variables
        
        const double ONE = 1.0E+0; const double ZERO = 0.0E+0; 

        #endregion

        public DSYR2K(LSAME lsame, XERBLA xerbla)
        {
    

            #region Set Dependencies
            
            this._lsame = lsame; this._xerbla = xerbla; 

            #endregion

        }
    
        public DSYR2K()
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
        /// DSYR2K  performs one of the symmetric rank 2k operations
        /// 
        /// C := alpha*A*B' + alpha*B*A' + beta*C,
        /// 
        /// or
        /// 
        /// C := alpha*A'*B + alpha*B'*A + beta*C,
        /// 
        /// where  alpha and beta  are scalars, C is an  n by n  symmetric matrix
        /// and  A and B  are  n by k  matrices  in the  first  case  and  k by n
        /// matrices in the second case.
        /// 
        ///</summary>
        /// <param name="UPLO">
        /// - CHARACTER*1.
        /// On  entry,   UPLO  specifies  whether  the  upper  or  lower
        /// triangular  part  of the  array  C  is to be  referenced  as
        /// follows:
        /// 
        /// UPLO = 'U' or 'u'   Only the  upper triangular part of  C
        /// is to be referenced.
        /// 
        /// UPLO = 'L' or 'l'   Only the  lower triangular part of  C
        /// is to be referenced.
        /// 
        /// Unchanged on exit.
        ///</param>
        /// <param name="TRANS">
        /// - CHARACTER*1.
        /// On entry,  TRANS  specifies the operation to be performed as
        /// follows:
        /// 
        /// TRANS = 'N' or 'n'   C := alpha*A*B' + alpha*B*A' +
        /// beta*C.
        /// 
        /// TRANS = 'T' or 't'   C := alpha*A'*B + alpha*B'*A +
        /// beta*C.
        /// 
        /// TRANS = 'C' or 'c'   C := alpha*A'*B + alpha*B'*A +
        /// beta*C.
        /// 
        /// Unchanged on exit.
        ///</param>
        /// <param name="N">
        /// - INTEGER.
        /// On entry,  N specifies the order of the matrix C.  N must be
        /// at least zero.
        /// Unchanged on exit.
        ///</param>
        /// <param name="K">
        /// - INTEGER.
        /// On entry with  TRANS = 'N' or 'n',  K  specifies  the number
        /// of  columns  of the  matrices  A and B,  and on  entry  with
        /// TRANS = 'T' or 't' or 'C' or 'c',  K  specifies  the  number
        /// of rows of the matrices  A and B.  K must be at least  zero.
        /// Unchanged on exit.
        ///</param>
        /// <param name="ALPHA">
        /// - DOUBLE PRECISION.
        /// On entry, ALPHA specifies the scalar alpha.
        /// Unchanged on exit.
        ///</param>
        /// <param name="A">
        /// - DOUBLE PRECISION array of DIMENSION ( LDA, ka ), where ka is
        /// k  when  TRANS = 'N' or 'n',  and is  n  otherwise.
        /// Before entry with  TRANS = 'N' or 'n',  the  leading  n by k
        /// part of the array  A  must contain the matrix  A,  otherwise
        /// the leading  k by n  part of the array  A  must contain  the
        /// matrix A.
        /// Unchanged on exit.
        ///</param>
        /// <param name="LDA">
        /// - INTEGER.
        /// On entry, LDA specifies the first dimension of A as declared
        /// in  the  calling  (sub)  program.   When  TRANS = 'N' or 'n'
        /// then  LDA must be at least  max( 1, n ), otherwise  LDA must
        /// be at least  max( 1, k ).
        /// Unchanged on exit.
        ///</param>
        /// <param name="B">
        /// - DOUBLE PRECISION array of DIMENSION ( LDB, kb ), where kb is
        /// k  when  TRANS = 'N' or 'n',  and is  n  otherwise.
        /// Before entry with  TRANS = 'N' or 'n',  the  leading  n by k
        /// part of the array  B  must contain the matrix  B,  otherwise
        /// the leading  k by n  part of the array  B  must contain  the
        /// matrix B.
        /// Unchanged on exit.
        ///</param>
        /// <param name="LDB">
        /// - INTEGER.
        /// On entry, LDB specifies the first dimension of B as declared
        /// in  the  calling  (sub)  program.   When  TRANS = 'N' or 'n'
        /// then  LDB must be at least  max( 1, n ), otherwise  LDB must
        /// be at least  max( 1, k ).
        /// Unchanged on exit.
        ///</param>
        /// <param name="BETA">
        /// - DOUBLE PRECISION.
        /// On entry, BETA specifies the scalar beta.
        /// Unchanged on exit.
        ///</param>
        /// <param name="C">
        /// := alpha*A*B' + alpha*B*A' + beta*C,
        ///</param>
        /// <param name="LDC">
        /// - INTEGER.
        /// On entry, LDC specifies the first dimension of C as declared
        /// in  the  calling  (sub)  program.   LDC  must  be  at  least
        /// max( 1, n ).
        /// Unchanged on exit.
        /// 
        ///</param>
        public void Run(string UPLO, string TRANS, int N, int K, double ALPHA, double[] A, int offset_a
                         , int LDA, double[] B, int offset_b, int LDB, double BETA, ref double[] C, int offset_c, int LDC)
        {

            #region Variables
            
            double TEMP1 = 0; double TEMP2 = 0; int I = 0; int INFO = 0; int J = 0; int L = 0; int NROWA = 0; bool UPPER = false; 

            #endregion


            #region Implicit Variables
            
            int C_J = 0; int A_L = 0; int B_L = 0; int A_I = 0; int B_J = 0; int B_I = 0; int A_J = 0; 

            #endregion


            #region Array Index Correction
            
             int o_a = -1 - LDA + offset_a;  int o_b = -1 - LDB + offset_b;  int o_c = -1 - LDC + offset_c; 

            #endregion


            #region Strings
            
            UPLO = UPLO.Substring(0, 1);  TRANS = TRANS.Substring(0, 1);  

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
            // *  DSYR2K  performs one of the symmetric rank 2k operations
            // *
            // *     C := alpha*A*B' + alpha*B*A' + beta*C,
            // *
            // *  or
            // *
            // *     C := alpha*A'*B + alpha*B'*A + beta*C,
            // *
            // *  where  alpha and beta  are scalars, C is an  n by n  symmetric matrix
            // *  and  A and B  are  n by k  matrices  in the  first  case  and  k by n
            // *  matrices in the second case.
            // *
            // *  Arguments
            // *  ==========
            // *
            // *  UPLO   - CHARACTER*1.
            // *           On  entry,   UPLO  specifies  whether  the  upper  or  lower
            // *           triangular  part  of the  array  C  is to be  referenced  as
            // *           follows:
            // *
            // *              UPLO = 'U' or 'u'   Only the  upper triangular part of  C
            // *                                  is to be referenced.
            // *
            // *              UPLO = 'L' or 'l'   Only the  lower triangular part of  C
            // *                                  is to be referenced.
            // *
            // *           Unchanged on exit.
            // *
            // *  TRANS  - CHARACTER*1.
            // *           On entry,  TRANS  specifies the operation to be performed as
            // *           follows:
            // *
            // *              TRANS = 'N' or 'n'   C := alpha*A*B' + alpha*B*A' +
            // *                                        beta*C.
            // *
            // *              TRANS = 'T' or 't'   C := alpha*A'*B + alpha*B'*A +
            // *                                        beta*C.
            // *
            // *              TRANS = 'C' or 'c'   C := alpha*A'*B + alpha*B'*A +
            // *                                        beta*C.
            // *
            // *           Unchanged on exit.
            // *
            // *  N      - INTEGER.
            // *           On entry,  N specifies the order of the matrix C.  N must be
            // *           at least zero.
            // *           Unchanged on exit.
            // *
            // *  K      - INTEGER.
            // *           On entry with  TRANS = 'N' or 'n',  K  specifies  the number
            // *           of  columns  of the  matrices  A and B,  and on  entry  with
            // *           TRANS = 'T' or 't' or 'C' or 'c',  K  specifies  the  number
            // *           of rows of the matrices  A and B.  K must be at least  zero.
            // *           Unchanged on exit.
            // *
            // *  ALPHA  - DOUBLE PRECISION.
            // *           On entry, ALPHA specifies the scalar alpha.
            // *           Unchanged on exit.
            // *
            // *  A      - DOUBLE PRECISION array of DIMENSION ( LDA, ka ), where ka is
            // *           k  when  TRANS = 'N' or 'n',  and is  n  otherwise.
            // *           Before entry with  TRANS = 'N' or 'n',  the  leading  n by k
            // *           part of the array  A  must contain the matrix  A,  otherwise
            // *           the leading  k by n  part of the array  A  must contain  the
            // *           matrix A.
            // *           Unchanged on exit.
            // *
            // *  LDA    - INTEGER.
            // *           On entry, LDA specifies the first dimension of A as declared
            // *           in  the  calling  (sub)  program.   When  TRANS = 'N' or 'n'
            // *           then  LDA must be at least  max( 1, n ), otherwise  LDA must
            // *           be at least  max( 1, k ).
            // *           Unchanged on exit.
            // *
            // *  B      - DOUBLE PRECISION array of DIMENSION ( LDB, kb ), where kb is
            // *           k  when  TRANS = 'N' or 'n',  and is  n  otherwise.
            // *           Before entry with  TRANS = 'N' or 'n',  the  leading  n by k
            // *           part of the array  B  must contain the matrix  B,  otherwise
            // *           the leading  k by n  part of the array  B  must contain  the
            // *           matrix B.
            // *           Unchanged on exit.
            // *
            // *  LDB    - INTEGER.
            // *           On entry, LDB specifies the first dimension of B as declared
            // *           in  the  calling  (sub)  program.   When  TRANS = 'N' or 'n'
            // *           then  LDB must be at least  max( 1, n ), otherwise  LDB must
            // *           be at least  max( 1, k ).
            // *           Unchanged on exit.
            // *
            // *  BETA   - DOUBLE PRECISION.
            // *           On entry, BETA specifies the scalar beta.
            // *           Unchanged on exit.
            // *
            // *  C      - DOUBLE PRECISION array of DIMENSION ( LDC, n ).
            // *           Before entry  with  UPLO = 'U' or 'u',  the leading  n by n
            // *           upper triangular part of the array C must contain the upper
            // *           triangular part  of the  symmetric matrix  and the strictly
            // *           lower triangular part of C is not referenced.  On exit, the
            // *           upper triangular part of the array  C is overwritten by the
            // *           upper triangular part of the updated matrix.
            // *           Before entry  with  UPLO = 'L' or 'l',  the leading  n by n
            // *           lower triangular part of the array C must contain the lower
            // *           triangular part  of the  symmetric matrix  and the strictly
            // *           upper triangular part of C is not referenced.  On exit, the
            // *           lower triangular part of the array  C is overwritten by the
            // *           lower triangular part of the updated matrix.
            // *
            // *  LDC    - INTEGER.
            // *           On entry, LDC specifies the first dimension of C as declared
            // *           in  the  calling  (sub)  program.   LDC  must  be  at  least
            // *           max( 1, n ).
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
            
            if (this._lsame.Run(TRANS, "N"))
            {
                NROWA = N;
            }
            else
            {
                NROWA = K;
            }
            UPPER = this._lsame.Run(UPLO, "U");
            // *
            INFO = 0;
            if ((!UPPER) && (!this._lsame.Run(UPLO, "L")))
            {
                INFO = 1;
            }
            else
            {
                if ((!this._lsame.Run(TRANS, "N")) && (!this._lsame.Run(TRANS, "T")) && (!this._lsame.Run(TRANS, "C")))
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
                        if (K < 0)
                        {
                            INFO = 4;
                        }
                        else
                        {
                            if (LDA < Math.Max(1, NROWA))
                            {
                                INFO = 7;
                            }
                            else
                            {
                                if (LDB < Math.Max(1, NROWA))
                                {
                                    INFO = 9;
                                }
                                else
                                {
                                    if (LDC < Math.Max(1, N))
                                    {
                                        INFO = 12;
                                    }
                                }
                            }
                        }
                    }
                }
            }
            if (INFO != 0)
            {
                this._xerbla.Run("DSYR2K", INFO);
                return;
            }
            // *
            // *     Quick return if possible.
            // *
            if ((N == 0) || (((ALPHA == ZERO) || (K == 0)) && (BETA == ONE))) return;
            // *
            // *     And when  alpha.eq.zero.
            // *
            if (ALPHA == ZERO)
            {
                if (UPPER)
                {
                    if (BETA == ZERO)
                    {
                        for (J = 1; J <= N; J++)
                        {
                            C_J = J * LDC + o_c;
                            for (I = 1; I <= J; I++)
                            {
                                C[I + C_J] = ZERO;
                            }
                        }
                    }
                    else
                    {
                        for (J = 1; J <= N; J++)
                        {
                            C_J = J * LDC + o_c;
                            for (I = 1; I <= J; I++)
                            {
                                C[I + C_J] *= BETA;
                            }
                        }
                    }
                }
                else
                {
                    if (BETA == ZERO)
                    {
                        for (J = 1; J <= N; J++)
                        {
                            C_J = J * LDC + o_c;
                            for (I = J; I <= N; I++)
                            {
                                C[I + C_J] = ZERO;
                            }
                        }
                    }
                    else
                    {
                        for (J = 1; J <= N; J++)
                        {
                            C_J = J * LDC + o_c;
                            for (I = J; I <= N; I++)
                            {
                                C[I + C_J] *= BETA;
                            }
                        }
                    }
                }
                return;
            }
            // *
            // *     Start the operations.
            // *
            if (this._lsame.Run(TRANS, "N"))
            {
                // *
                // *        Form  C := alpha*A*B' + alpha*B*A' + C.
                // *
                if (UPPER)
                {
                    for (J = 1; J <= N; J++)
                    {
                        if (BETA == ZERO)
                        {
                            C_J = J * LDC + o_c;
                            for (I = 1; I <= J; I++)
                            {
                                C[I + C_J] = ZERO;
                            }
                        }
                        else
                        {
                            if (BETA != ONE)
                            {
                                C_J = J * LDC + o_c;
                                for (I = 1; I <= J; I++)
                                {
                                    C[I + C_J] *= BETA;
                                }
                            }
                        }
                        for (L = 1; L <= K; L++)
                        {
                            if ((A[J+L * LDA + o_a] != ZERO) || (B[J+L * LDB + o_b] != ZERO))
                            {
                                TEMP1 = ALPHA * B[J+L * LDB + o_b];
                                TEMP2 = ALPHA * A[J+L * LDA + o_a];
                                C_J = J * LDC + o_c;
                                A_L = L * LDA + o_a;
                                B_L = L * LDB + o_b;
                                for (I = 1; I <= J; I++)
                                {
                                    C[I + C_J] += A[I + A_L] * TEMP1 + B[I + B_L] * TEMP2;
                                }
                            }
                        }
                    }
                }
                else
                {
                    for (J = 1; J <= N; J++)
                    {
                        if (BETA == ZERO)
                        {
                            C_J = J * LDC + o_c;
                            for (I = J; I <= N; I++)
                            {
                                C[I + C_J] = ZERO;
                            }
                        }
                        else
                        {
                            if (BETA != ONE)
                            {
                                C_J = J * LDC + o_c;
                                for (I = J; I <= N; I++)
                                {
                                    C[I + C_J] *= BETA;
                                }
                            }
                        }
                        for (L = 1; L <= K; L++)
                        {
                            if ((A[J+L * LDA + o_a] != ZERO) || (B[J+L * LDB + o_b] != ZERO))
                            {
                                TEMP1 = ALPHA * B[J+L * LDB + o_b];
                                TEMP2 = ALPHA * A[J+L * LDA + o_a];
                                C_J = J * LDC + o_c;
                                A_L = L * LDA + o_a;
                                B_L = L * LDB + o_b;
                                for (I = J; I <= N; I++)
                                {
                                    C[I + C_J] += A[I + A_L] * TEMP1 + B[I + B_L] * TEMP2;
                                }
                            }
                        }
                    }
                }
            }
            else
            {
                // *
                // *        Form  C := alpha*A'*B + alpha*B'*A + C.
                // *
                if (UPPER)
                {
                    for (J = 1; J <= N; J++)
                    {
                        for (I = 1; I <= J; I++)
                        {
                            TEMP1 = ZERO;
                            TEMP2 = ZERO;
                            A_I = I * LDA + o_a;
                            B_J = J * LDB + o_b;
                            B_I = I * LDB + o_b;
                            A_J = J * LDA + o_a;
                            for (L = 1; L <= K; L++)
                            {
                                TEMP1 += A[L + A_I] * B[L + B_J];
                                TEMP2 += B[L + B_I] * A[L + A_J];
                            }
                            if (BETA == ZERO)
                            {
                                C[I+J * LDC + o_c] = ALPHA * TEMP1 + ALPHA * TEMP2;
                            }
                            else
                            {
                                C[I+J * LDC + o_c] = BETA * C[I+J * LDC + o_c] + ALPHA * TEMP1 + ALPHA * TEMP2;
                            }
                        }
                    }
                }
                else
                {
                    for (J = 1; J <= N; J++)
                    {
                        for (I = J; I <= N; I++)
                        {
                            TEMP1 = ZERO;
                            TEMP2 = ZERO;
                            A_I = I * LDA + o_a;
                            B_J = J * LDB + o_b;
                            B_I = I * LDB + o_b;
                            A_J = J * LDA + o_a;
                            for (L = 1; L <= K; L++)
                            {
                                TEMP1 += A[L + A_I] * B[L + B_J];
                                TEMP2 += B[L + B_I] * A[L + A_J];
                            }
                            if (BETA == ZERO)
                            {
                                C[I+J * LDC + o_c] = ALPHA * TEMP1 + ALPHA * TEMP2;
                            }
                            else
                            {
                                C[I+J * LDC + o_c] = BETA * C[I+J * LDC + o_c] + ALPHA * TEMP1 + ALPHA * TEMP2;
                            }
                        }
                    }
                }
            }
            // *
            return;
            // *
            // *     End of DSYR2K.
            // *

            #endregion

        }
    }
}
