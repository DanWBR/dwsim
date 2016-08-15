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
    /// DTRTRI computes the inverse of a real upper or lower triangular
    /// matrix A.
    /// 
    /// This is the Level 3 BLAS version of the algorithm.
    /// 
    ///</summary>
    public class DTRTRI
    {
    

        #region Dependencies
        
        LSAME _lsame; ILAENV _ilaenv; DTRMM _dtrmm; DTRSM _dtrsm; DTRTI2 _dtrti2; XERBLA _xerbla; 

        #endregion


        #region Variables
        
        const double ONE = 1.0E+0; const double ZERO = 0.0E+0; 

        #endregion

        public DTRTRI(LSAME lsame, ILAENV ilaenv, DTRMM dtrmm, DTRSM dtrsm, DTRTI2 dtrti2, XERBLA xerbla)
        {
    

            #region Set Dependencies
            
            this._lsame = lsame; this._ilaenv = ilaenv; this._dtrmm = dtrmm; this._dtrsm = dtrsm; this._dtrti2 = dtrti2; 
            this._xerbla = xerbla;

            #endregion

        }
    
        public DTRTRI()
        {
    

            #region Dependencies (Initialization)
            
            LSAME lsame = new LSAME();
            IEEECK ieeeck = new IEEECK();
            IPARMQ iparmq = new IPARMQ();
            XERBLA xerbla = new XERBLA();
            DSCAL dscal = new DSCAL();
            ILAENV ilaenv = new ILAENV(ieeeck, iparmq);
            DTRMM dtrmm = new DTRMM(lsame, xerbla);
            DTRSM dtrsm = new DTRSM(lsame, xerbla);
            DTRMV dtrmv = new DTRMV(lsame, xerbla);
            DTRTI2 dtrti2 = new DTRTI2(lsame, dscal, dtrmv, xerbla);

            #endregion


            #region Set Dependencies
            
            this._lsame = lsame; this._ilaenv = ilaenv; this._dtrmm = dtrmm; this._dtrsm = dtrsm; this._dtrti2 = dtrti2; 
            this._xerbla = xerbla;

            #endregion

        }
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DTRTRI computes the inverse of a real upper or lower triangular
        /// matrix A.
        /// 
        /// This is the Level 3 BLAS version of the algorithm.
        /// 
        ///</summary>
        /// <param name="UPLO">
        /// (input) CHARACTER*1
        /// = 'U':  A is upper triangular;
        /// = 'L':  A is lower triangular.
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
        /// <param name="A">
        /// (input/output) DOUBLE PRECISION array, dimension (LDA,N)
        /// On entry, the triangular matrix A.  If UPLO = 'U', the
        /// leading N-by-N upper triangular part of the array A contains
        /// the upper triangular matrix, and the strictly lower
        /// triangular part of A is not referenced.  If UPLO = 'L', the
        /// leading N-by-N lower triangular part of the array A contains
        /// the lower triangular matrix, and the strictly upper
        /// triangular part of A is not referenced.  If DIAG = 'U', the
        /// diagonal elements of A are also not referenced and are
        /// assumed to be 1.
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
        /// .LT. 0: if INFO = -i, the i-th argument had an illegal value
        /// .GT. 0: if INFO = i, A(i,i) is exactly zero.  The triangular
        /// matrix is singular and its inverse can not be computed.
        ///</param>
        public void Run(string UPLO, string DIAG, int N, ref double[] A, int offset_a, int LDA, ref int INFO)
        {

            #region Variables
            
            bool NOUNIT = false; bool UPPER = false; int J = 0; int JB = 0; int NB = 0; int NN = 0; 

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
            // *  DTRTRI computes the inverse of a real upper or lower triangular
            // *  matrix A.
            // *
            // *  This is the Level 3 BLAS version of the algorithm.
            // *
            // *  Arguments
            // *  =========
            // *
            // *  UPLO    (input) CHARACTER*1
            // *          = 'U':  A is upper triangular;
            // *          = 'L':  A is lower triangular.
            // *
            // *  DIAG    (input) CHARACTER*1
            // *          = 'N':  A is non-unit triangular;
            // *          = 'U':  A is unit triangular.
            // *
            // *  N       (input) INTEGER
            // *          The order of the matrix A.  N >= 0.
            // *
            // *  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
            // *          On entry, the triangular matrix A.  If UPLO = 'U', the
            // *          leading N-by-N upper triangular part of the array A contains
            // *          the upper triangular matrix, and the strictly lower
            // *          triangular part of A is not referenced.  If UPLO = 'L', the
            // *          leading N-by-N lower triangular part of the array A contains
            // *          the lower triangular matrix, and the strictly upper
            // *          triangular part of A is not referenced.  If DIAG = 'U', the
            // *          diagonal elements of A are also not referenced and are
            // *          assumed to be 1.
            // *          On exit, the (triangular) inverse of the original matrix, in
            // *          the same storage format.
            // *
            // *  LDA     (input) INTEGER
            // *          The leading dimension of the array A.  LDA >= max(1,N).
            // *
            // *  INFO    (output) INTEGER
            // *          = 0: successful exit
            // *          < 0: if INFO = -i, the i-th argument had an illegal value
            // *          > 0: if INFO = i, A(i,i) is exactly zero.  The triangular
            // *               matrix is singular and its inverse can not be computed.
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
            //      INTRINSIC          MAX, MIN;
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
                this._xerbla.Run("DTRTRI",  - INFO);
                return;
            }
            // *
            // *     Quick return if possible
            // *
            if (N == 0) return;
            // *
            // *     Check for singularity if non-unit.
            // *
            if (NOUNIT)
            {
                for (INFO = 1; INFO <= N; INFO++)
                {
                    if (A[INFO+INFO * LDA + o_a] == ZERO) return;
                }
                INFO = 0;
            }
            // *
            // *     Determine the block size for this environment.
            // *
            NB = this._ilaenv.Run(1, "DTRTRI", UPLO + DIAG, N,  - 1,  - 1,  - 1);
            if (NB <= 1 || NB >= N)
            {
                // *
                // *        Use unblocked code
                // *
                this._dtrti2.Run(UPLO, DIAG, N, ref A, offset_a, LDA, ref INFO);
            }
            else
            {
                // *
                // *        Use blocked code
                // *
                if (UPPER)
                {
                    // *
                    // *           Compute inverse of upper triangular matrix
                    // *
                    for (J = 1; (NB >= 0) ? (J <= N) : (J >= N); J += NB)
                    {
                        JB = Math.Min(NB, N - J + 1);
                        // *
                        // *              Compute rows 1:j-1 of current block column
                        // *
                        this._dtrmm.Run("Left", "Upper", "No transpose", DIAG, J - 1, JB
                                        , ONE, A, offset_a, LDA, ref A, 1+J * LDA + o_a, LDA);
                        this._dtrsm.Run("Right", "Upper", "No transpose", DIAG, J - 1, JB
                                        ,  - ONE, A, J+J * LDA + o_a, LDA, ref A, 1+J * LDA + o_a, LDA);
                        // *
                        // *              Compute inverse of current diagonal block
                        // *
                        this._dtrti2.Run("Upper", DIAG, JB, ref A, J+J * LDA + o_a, LDA, ref INFO);
                    }
                }
                else
                {
                    // *
                    // *           Compute inverse of lower triangular matrix
                    // *
                    NN = ((N - 1) / NB) * NB + 1;
                    for (J = NN; ( - NB >= 0) ? (J <= 1) : (J >= 1); J +=  - NB)
                    {
                        JB = Math.Min(NB, N - J + 1);
                        if (J + JB <= N)
                        {
                            // *
                            // *                 Compute rows j+jb:n of current block column
                            // *
                            this._dtrmm.Run("Left", "Lower", "No transpose", DIAG, N - J - JB + 1, JB
                                            , ONE, A, J + JB+(J + JB) * LDA + o_a, LDA, ref A, J + JB+J * LDA + o_a, LDA);
                            this._dtrsm.Run("Right", "Lower", "No transpose", DIAG, N - J - JB + 1, JB
                                            ,  - ONE, A, J+J * LDA + o_a, LDA, ref A, J + JB+J * LDA + o_a, LDA);
                        }
                        // *
                        // *              Compute inverse of current diagonal block
                        // *
                        this._dtrti2.Run("Lower", DIAG, JB, ref A, J+J * LDA + o_a, LDA, ref INFO);
                    }
                }
            }
            // *
            return;
            // *
            // *     End of DTRTRI
            // *

            #endregion

        }
    }
}
