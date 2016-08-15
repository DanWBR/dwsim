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
    /// DTRTRS solves a triangular system of the form
    /// 
    /// A * X = B  or  A**T * X = B,
    /// 
    /// where A is a triangular matrix of order N, and B is an N-by-NRHS
    /// matrix.  A check is made to verify that A is nonsingular.
    /// 
    ///</summary>
    public class DTRTRS
    {
    

        #region Dependencies
        
        LSAME _lsame; DTRSM _dtrsm; XERBLA _xerbla; 

        #endregion


        #region Variables
        
        const double ZERO = 0.0E+0; const double ONE = 1.0E+0; 

        #endregion

        public DTRTRS(LSAME lsame, DTRSM dtrsm, XERBLA xerbla)
        {
    

            #region Set Dependencies
            
            this._lsame = lsame; this._dtrsm = dtrsm; this._xerbla = xerbla; 

            #endregion

        }
    
        public DTRTRS()
        {
    

            #region Dependencies (Initialization)
            
            LSAME lsame = new LSAME();
            XERBLA xerbla = new XERBLA();
            DTRSM dtrsm = new DTRSM(lsame, xerbla);

            #endregion


            #region Set Dependencies
            
            this._lsame = lsame; this._dtrsm = dtrsm; this._xerbla = xerbla; 

            #endregion

        }
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DTRTRS solves a triangular system of the form
        /// 
        /// A * X = B  or  A**T * X = B,
        /// 
        /// where A is a triangular matrix of order N, and B is an N-by-NRHS
        /// matrix.  A check is made to verify that A is nonsingular.
        /// 
        ///</summary>
        /// <param name="UPLO">
        /// (input) CHARACTER*1
        /// = 'U':  A is upper triangular;
        /// = 'L':  A is lower triangular.
        ///</param>
        /// <param name="TRANS">
        /// (input) CHARACTER*1
        /// Specifies the form of the system of equations:
        /// = 'N':  A * X = B  (No transpose)
        /// = 'T':  A**T * X = B  (Transpose)
        /// = 'C':  A**H * X = B  (Conjugate transpose = Transpose)
        ///</param>
        /// <param name="DIAG">
        /// (input) CHARACTER*1
        /// = 'N':  A is non-unit triangular;
        /// = 'U':  A is unit triangular.
        ///</param>
        /// <param name="N">
        /// (input) INTEGER
        /// The order of the matrix A.  N .GE. 0.
        ///</param>
        /// <param name="NRHS">
        /// (input) INTEGER
        /// The number of right hand sides, i.e., the number of columns
        /// of the matrix B.  NRHS .GE. 0.
        ///</param>
        /// <param name="A">
        /// * X = B  or  A**T * X = B,
        ///</param>
        /// <param name="LDA">
        /// (input) INTEGER
        /// The leading dimension of the array A.  LDA .GE. max(1,N).
        ///</param>
        /// <param name="B">
        /// (input/output) DOUBLE PRECISION array, dimension (LDB,NRHS)
        /// On entry, the right hand side matrix B.
        /// On exit, if INFO = 0, the solution matrix X.
        ///</param>
        /// <param name="LDB">
        /// (input) INTEGER
        /// The leading dimension of the array B.  LDB .GE. max(1,N).
        ///</param>
        /// <param name="INFO">
        /// (output) INTEGER
        /// = 0:  successful exit
        /// .LT. 0: if INFO = -i, the i-th argument had an illegal value
        /// .GT. 0: if INFO = i, the i-th diagonal element of A is zero,
        /// indicating that the matrix is singular and the solutions
        /// X have not been computed.
        ///</param>
        public void Run(string UPLO, string TRANS, string DIAG, int N, int NRHS, double[] A, int offset_a
                         , int LDA, ref double[] B, int offset_b, int LDB, ref int INFO)
        {

            #region Variables
            
            bool NOUNIT = false; 

            #endregion


            #region Array Index Correction
            
             int o_a = -1 - LDA + offset_a;  int o_b = -1 - LDB + offset_b; 

            #endregion


            #region Strings
            
            UPLO = UPLO.Substring(0, 1);  TRANS = TRANS.Substring(0, 1);  DIAG = DIAG.Substring(0, 1);  

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
            // *  DTRTRS solves a triangular system of the form
            // *
            // *     A * X = B  or  A**T * X = B,
            // *
            // *  where A is a triangular matrix of order N, and B is an N-by-NRHS
            // *  matrix.  A check is made to verify that A is nonsingular.
            // *
            // *  Arguments
            // *  =========
            // *
            // *  UPLO    (input) CHARACTER*1
            // *          = 'U':  A is upper triangular;
            // *          = 'L':  A is lower triangular.
            // *
            // *  TRANS   (input) CHARACTER*1
            // *          Specifies the form of the system of equations:
            // *          = 'N':  A * X = B  (No transpose)
            // *          = 'T':  A**T * X = B  (Transpose)
            // *          = 'C':  A**H * X = B  (Conjugate transpose = Transpose)
            // *
            // *  DIAG    (input) CHARACTER*1
            // *          = 'N':  A is non-unit triangular;
            // *          = 'U':  A is unit triangular.
            // *
            // *  N       (input) INTEGER
            // *          The order of the matrix A.  N >= 0.
            // *
            // *  NRHS    (input) INTEGER
            // *          The number of right hand sides, i.e., the number of columns
            // *          of the matrix B.  NRHS >= 0.
            // *
            // *  A       (input) DOUBLE PRECISION array, dimension (LDA,N)
            // *          The triangular matrix A.  If UPLO = 'U', the leading N-by-N
            // *          upper triangular part of the array A contains the upper
            // *          triangular matrix, and the strictly lower triangular part of
            // *          A is not referenced.  If UPLO = 'L', the leading N-by-N lower
            // *          triangular part of the array A contains the lower triangular
            // *          matrix, and the strictly upper triangular part of A is not
            // *          referenced.  If DIAG = 'U', the diagonal elements of A are
            // *          also not referenced and are assumed to be 1.
            // *
            // *  LDA     (input) INTEGER
            // *          The leading dimension of the array A.  LDA >= max(1,N).
            // *
            // *  B       (input/output) DOUBLE PRECISION array, dimension (LDB,NRHS)
            // *          On entry, the right hand side matrix B.
            // *          On exit, if INFO = 0, the solution matrix X.
            // *
            // *  LDB     (input) INTEGER
            // *          The leading dimension of the array B.  LDB >= max(1,N).
            // *
            // *  INFO    (output) INTEGER
            // *          = 0:  successful exit
            // *          < 0: if INFO = -i, the i-th argument had an illegal value
            // *          > 0: if INFO = i, the i-th diagonal element of A is zero,
            // *               indicating that the matrix is singular and the solutions
            // *               X have not been computed.
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
            //      INTRINSIC          MAX;
            // *     ..
            // *     .. Executable Statements ..
            // *
            // *     Test the input parameters.
            // *

            #endregion


            #region Body
            
            INFO = 0;
            NOUNIT = this._lsame.Run(DIAG, "N");
            if (!this._lsame.Run(UPLO, "U") && !this._lsame.Run(UPLO, "L"))
            {
                INFO =  - 1;
            }
            else
            {
                if (!this._lsame.Run(TRANS, "N") && !this._lsame.Run(TRANS, "T") && !this._lsame.Run(TRANS, "C"))
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
                        if (N < 0)
                        {
                            INFO =  - 4;
                        }
                        else
                        {
                            if (NRHS < 0)
                            {
                                INFO =  - 5;
                            }
                            else
                            {
                                if (LDA < Math.Max(1, N))
                                {
                                    INFO =  - 7;
                                }
                                else
                                {
                                    if (LDB < Math.Max(1, N))
                                    {
                                        INFO =  - 9;
                                    }
                                }
                            }
                        }
                    }
                }
            }
            if (INFO != 0)
            {
                this._xerbla.Run("DTRTRS",  - INFO);
                return;
            }
            // *
            // *     Quick return if possible
            // *
            if (N == 0) return;
            // *
            // *     Check for singularity.
            // *
            if (NOUNIT)
            {
                for (INFO = 1; INFO <= N; INFO++)
                {
                    if (A[INFO+INFO * LDA + o_a] == ZERO) return;
                }
            }
            INFO = 0;
            // *
            // *     Solve A * x = b  or  A' * x = b.
            // *
            this._dtrsm.Run("Left", UPLO, TRANS, DIAG, N, NRHS
                            , ONE, A, offset_a, LDA, ref B, offset_b, LDB);
            // *
            return;
            // *
            // *     End of DTRTRS
            // *

            #endregion

        }
    }
}
