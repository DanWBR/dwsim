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
    /// DTRTI2 computes the inverse of a real upper or lower triangular
    /// matrix.
    /// 
    /// This is the Level 2 BLAS version of the algorithm.
    /// 
    ///</summary>
    public class DTRTI2
    {
    

        #region Dependencies
        
        LSAME _lsame; DSCAL _dscal; DTRMV _dtrmv; XERBLA _xerbla; 

        #endregion


        #region Variables
        
        const double ONE = 1.0E+0; 

        #endregion

        public DTRTI2(LSAME lsame, DSCAL dscal, DTRMV dtrmv, XERBLA xerbla)
        {
    

            #region Set Dependencies
            
            this._lsame = lsame; this._dscal = dscal; this._dtrmv = dtrmv; this._xerbla = xerbla; 

            #endregion

        }
    
        public DTRTI2()
        {
    

            #region Dependencies (Initialization)
            
            LSAME lsame = new LSAME();
            DSCAL dscal = new DSCAL();
            XERBLA xerbla = new XERBLA();
            DTRMV dtrmv = new DTRMV(lsame, xerbla);

            #endregion


            #region Set Dependencies
            
            this._lsame = lsame; this._dscal = dscal; this._dtrmv = dtrmv; this._xerbla = xerbla; 

            #endregion

        }
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DTRTI2 computes the inverse of a real upper or lower triangular
        /// matrix.
        /// 
        /// This is the Level 2 BLAS version of the algorithm.
        /// 
        ///</summary>
        /// <param name="UPLO">
        /// (input) CHARACTER*1
        /// Specifies whether the matrix A is upper or lower triangular.
        /// = 'U':  Upper triangular
        /// = 'L':  Lower triangular
        ///</param>
        /// <param name="DIAG">
        /// (input) CHARACTER*1
        /// Specifies whether or not the matrix A is unit triangular.
        /// = 'N':  Non-unit triangular
        /// = 'U':  Unit triangular
        ///</param>
        /// <param name="N">
        /// (input) INTEGER
        /// The order of the matrix A.  N .GE. 0.
        ///</param>
        /// <param name="A">
        /// (input/output) DOUBLE PRECISION array, dimension (LDA,N)
        /// On entry, the triangular matrix A.  If UPLO = 'U', the
        /// leading n by n upper triangular part of the array A contains
        /// the upper triangular matrix, and the strictly lower
        /// triangular part of A is not referenced.  If UPLO = 'L', the
        /// leading n by n lower triangular part of the array A contains
        /// the lower triangular matrix, and the strictly upper
        /// triangular part of A is not referenced.  If DIAG = 'U', the
        /// diagonal elements of A are also not referenced and are
        /// assumed to be 1.
        /// 
        /// On exit, the (triangular) inverse of the original matrix, in
        /// the same storage format.
        ///</param>
        /// <param name="LDA">
        /// (input) INTEGER
        /// The leading dimension of the array A.  LDA .GE. max(1,N).
        ///</param>
        /// <param name="INFO">
        /// (output) INTEGER
        /// = 0: successful exit
        /// .LT. 0: if INFO = -k, the k-th argument had an illegal value
        ///</param>
        public void Run(string UPLO, string DIAG, int N, ref double[] A, int offset_a, int LDA, ref int INFO)
        {

            #region Variables
            
            bool NOUNIT = false; bool UPPER = false; int J = 0; double AJJ = 0; 

            #endregion


            #region Array Index Correction
            
             int o_a = -1 - LDA + offset_a; 

            #endregion


            #region Strings
            
            UPLO = UPLO.Substring(0, 1);  DIAG = DIAG.Substring(0, 1);  

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
            // *  DTRTI2 computes the inverse of a real upper or lower triangular
            // *  matrix.
            // *
            // *  This is the Level 2 BLAS version of the algorithm.
            // *
            // *  Arguments
            // *  =========
            // *
            // *  UPLO    (input) CHARACTER*1
            // *          Specifies whether the matrix A is upper or lower triangular.
            // *          = 'U':  Upper triangular
            // *          = 'L':  Lower triangular
            // *
            // *  DIAG    (input) CHARACTER*1
            // *          Specifies whether or not the matrix A is unit triangular.
            // *          = 'N':  Non-unit triangular
            // *          = 'U':  Unit triangular
            // *
            // *  N       (input) INTEGER
            // *          The order of the matrix A.  N >= 0.
            // *
            // *  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
            // *          On entry, the triangular matrix A.  If UPLO = 'U', the
            // *          leading n by n upper triangular part of the array A contains
            // *          the upper triangular matrix, and the strictly lower
            // *          triangular part of A is not referenced.  If UPLO = 'L', the
            // *          leading n by n lower triangular part of the array A contains
            // *          the lower triangular matrix, and the strictly upper
            // *          triangular part of A is not referenced.  If DIAG = 'U', the
            // *          diagonal elements of A are also not referenced and are
            // *          assumed to be 1.
            // *
            // *          On exit, the (triangular) inverse of the original matrix, in
            // *          the same storage format.
            // *
            // *  LDA     (input) INTEGER
            // *          The leading dimension of the array A.  LDA >= max(1,N).
            // *
            // *  INFO    (output) INTEGER
            // *          = 0: successful exit
            // *          < 0: if INFO = -k, the k-th argument had an illegal value
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
            UPPER = this._lsame.Run(UPLO, "U");
            NOUNIT = this._lsame.Run(DIAG, "N");
            if (!UPPER && !this._lsame.Run(UPLO, "L"))
            {
                INFO =  - 1;
            }
            else
            {
                if (!NOUNIT && !this._lsame.Run(DIAG, "U"))
                {
                    INFO =  - 2;
                }
                else
                {
                    if (N < 0)
                    {
                        INFO =  - 3;
                    }
                    else
                    {
                        if (LDA < Math.Max(1, N))
                        {
                            INFO =  - 5;
                        }
                    }
                }
            }
            if (INFO != 0)
            {
                this._xerbla.Run("DTRTI2",  - INFO);
                return;
            }
            // *
            if (UPPER)
            {
                // *
                // *        Compute inverse of upper triangular matrix.
                // *
                for (J = 1; J <= N; J++)
                {
                    if (NOUNIT)
                    {
                        A[J+J * LDA + o_a] = ONE / A[J+J * LDA + o_a];
                        AJJ =  - A[J+J * LDA + o_a];
                    }
                    else
                    {
                        AJJ =  - ONE;
                    }
                    // *
                    // *           Compute elements 1:j-1 of j-th column.
                    // *
                    this._dtrmv.Run("Upper", "No transpose", DIAG, J - 1, A, offset_a, LDA
                                    , ref A, 1+J * LDA + o_a, 1);
                    this._dscal.Run(J - 1, AJJ, ref A, 1+J * LDA + o_a, 1);
                }
            }
            else
            {
                // *
                // *        Compute inverse of lower triangular matrix.
                // *
                for (J = N; J >= 1; J +=  - 1)
                {
                    if (NOUNIT)
                    {
                        A[J+J * LDA + o_a] = ONE / A[J+J * LDA + o_a];
                        AJJ =  - A[J+J * LDA + o_a];
                    }
                    else
                    {
                        AJJ =  - ONE;
                    }
                    if (J < N)
                    {
                        // *
                        // *              Compute elements j+1:n of j-th column.
                        // *
                        this._dtrmv.Run("Lower", "No transpose", DIAG, N - J, A, J + 1+(J + 1) * LDA + o_a, LDA
                                        , ref A, J + 1+J * LDA + o_a, 1);
                        this._dscal.Run(N - J, AJJ, ref A, J + 1+J * LDA + o_a, 1);
                    }
                }
            }
            // *
            return;
            // *
            // *     End of DTRTI2
            // *

            #endregion

        }
    }
}
