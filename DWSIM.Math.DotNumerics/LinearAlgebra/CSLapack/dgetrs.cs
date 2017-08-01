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
    /// DGETRS solves a system of linear equations
    /// A * X = B  or  A' * X = B
    /// with a general N-by-N matrix A using the LU factorization computed
    /// by DGETRF.
    /// 
    ///</summary>
    public class DGETRS
    {
    

        #region Dependencies
        
        LSAME _lsame; DLASWP _dlaswp; DTRSM _dtrsm; XERBLA _xerbla; 

        #endregion


        #region Variables
        
        const double ONE = 1.0E+0; 

        #endregion

        public DGETRS(LSAME lsame, DLASWP dlaswp, DTRSM dtrsm, XERBLA xerbla)
        {
    

            #region Set Dependencies
            
            this._lsame = lsame; this._dlaswp = dlaswp; this._dtrsm = dtrsm; this._xerbla = xerbla; 

            #endregion

        }
    
        public DGETRS()
        {
    

            #region Dependencies (Initialization)
            
            LSAME lsame = new LSAME();
            DLASWP dlaswp = new DLASWP();
            XERBLA xerbla = new XERBLA();
            DTRSM dtrsm = new DTRSM(lsame, xerbla);

            #endregion


            #region Set Dependencies
            
            this._lsame = lsame; this._dlaswp = dlaswp; this._dtrsm = dtrsm; this._xerbla = xerbla; 

            #endregion

        }
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DGETRS solves a system of linear equations
        /// A * X = B  or  A' * X = B
        /// with a general N-by-N matrix A using the LU factorization computed
        /// by DGETRF.
        /// 
        ///</summary>
        /// <param name="TRANS">
        /// (input) CHARACTER*1
        /// Specifies the form of the system of equations:
        /// = 'N':  A * X = B  (No transpose)
        /// = 'T':  A'* X = B  (Transpose)
        /// = 'C':  A'* X = B  (Conjugate transpose = Transpose)
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
        /// (input) DOUBLE PRECISION array, dimension (LDA,N)
        /// The factors L and U from the factorization A = P*L*U
        /// as computed by DGETRF.
        ///</param>
        /// <param name="LDA">
        /// (input) INTEGER
        /// The leading dimension of the array A.  LDA .GE. max(1,N).
        ///</param>
        /// <param name="IPIV">
        /// (input) INTEGER array, dimension (N)
        /// The pivot indices from DGETRF; for 1.LE.i.LE.N, row i of the
        /// matrix was interchanged with row IPIV(i).
        ///</param>
        /// <param name="B">
        /// (input/output) DOUBLE PRECISION array, dimension (LDB,NRHS)
        /// On entry, the right hand side matrix B.
        /// On exit, the solution matrix X.
        ///</param>
        /// <param name="LDB">
        /// (input) INTEGER
        /// The leading dimension of the array B.  LDB .GE. max(1,N).
        ///</param>
        /// <param name="INFO">
        /// (output) INTEGER
        /// = 0:  successful exit
        /// .LT. 0:  if INFO = -i, the i-th argument had an illegal value
        ///</param>
        public void Run(string TRANS, int N, int NRHS, double[] A, int offset_a, int LDA, int[] IPIV, int offset_ipiv
                         , ref double[] B, int offset_b, int LDB, ref int INFO)
        {

            #region Variables
            
            bool NOTRAN = false; 

            #endregion


            #region Array Index Correction
            
             int o_a = -1 - LDA + offset_a;  int o_ipiv = -1 + offset_ipiv;  int o_b = -1 - LDB + offset_b; 

            #endregion


            #region Strings
            
            TRANS = TRANS.Substring(0, 1);  

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
            // *  DGETRS solves a system of linear equations
            // *     A * X = B  or  A' * X = B
            // *  with a general N-by-N matrix A using the LU factorization computed
            // *  by DGETRF.
            // *
            // *  Arguments
            // *  =========
            // *
            // *  TRANS   (input) CHARACTER*1
            // *          Specifies the form of the system of equations:
            // *          = 'N':  A * X = B  (No transpose)
            // *          = 'T':  A'* X = B  (Transpose)
            // *          = 'C':  A'* X = B  (Conjugate transpose = Transpose)
            // *
            // *  N       (input) INTEGER
            // *          The order of the matrix A.  N >= 0.
            // *
            // *  NRHS    (input) INTEGER
            // *          The number of right hand sides, i.e., the number of columns
            // *          of the matrix B.  NRHS >= 0.
            // *
            // *  A       (input) DOUBLE PRECISION array, dimension (LDA,N)
            // *          The factors L and U from the factorization A = P*L*U
            // *          as computed by DGETRF.
            // *
            // *  LDA     (input) INTEGER
            // *          The leading dimension of the array A.  LDA >= max(1,N).
            // *
            // *  IPIV    (input) INTEGER array, dimension (N)
            // *          The pivot indices from DGETRF; for 1<=i<=N, row i of the
            // *          matrix was interchanged with row IPIV(i).
            // *
            // *  B       (input/output) DOUBLE PRECISION array, dimension (LDB,NRHS)
            // *          On entry, the right hand side matrix B.
            // *          On exit, the solution matrix X.
            // *
            // *  LDB     (input) INTEGER
            // *          The leading dimension of the array B.  LDB >= max(1,N).
            // *
            // *  INFO    (output) INTEGER
            // *          = 0:  successful exit
            // *          < 0:  if INFO = -i, the i-th argument had an illegal value
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
            NOTRAN = this._lsame.Run(TRANS, "N");
            if (!NOTRAN && !this._lsame.Run(TRANS, "T") && !this._lsame.Run(TRANS, "C"))
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
                    if (NRHS < 0)
                    {
                        INFO =  - 3;
                    }
                    else
                    {
                        if (LDA < Math.Max(1, N))
                        {
                            INFO =  - 5;
                        }
                        else
                        {
                            if (LDB < Math.Max(1, N))
                            {
                                INFO =  - 8;
                            }
                        }
                    }
                }
            }
            if (INFO != 0)
            {
                this._xerbla.Run("DGETRS",  - INFO);
                return;
            }
            // *
            // *     Quick return if possible
            // *
            if (N == 0 || NRHS == 0) return;
            // *
            if (NOTRAN)
            {
                // *
                // *        Solve A * X = B.
                // *
                // *        Apply row interchanges to the right hand sides.
                // *
                this._dlaswp.Run(NRHS, ref B, offset_b, LDB, 1, N, IPIV, offset_ipiv
                                 , 1);
                // *
                // *        Solve L*X = B, overwriting B with X.
                // *
                this._dtrsm.Run("Left", "Lower", "No transpose", "Unit", N, NRHS
                                , ONE, A, offset_a, LDA, ref B, offset_b, LDB);
                // *
                // *        Solve U*X = B, overwriting B with X.
                // *
                this._dtrsm.Run("Left", "Upper", "No transpose", "Non-unit", N, NRHS
                                , ONE, A, offset_a, LDA, ref B, offset_b, LDB);
            }
            else
            {
                // *
                // *        Solve A' * X = B.
                // *
                // *        Solve U'*X = B, overwriting B with X.
                // *
                this._dtrsm.Run("Left", "Upper", "Transpose", "Non-unit", N, NRHS
                                , ONE, A, offset_a, LDA, ref B, offset_b, LDB);
                // *
                // *        Solve L'*X = B, overwriting B with X.
                // *
                this._dtrsm.Run("Left", "Lower", "Transpose", "Unit", N, NRHS
                                , ONE, A, offset_a, LDA, ref B, offset_b, LDB);
                // *
                // *        Apply row interchanges to the solution vectors.
                // *
                this._dlaswp.Run(NRHS, ref B, offset_b, LDB, 1, N, IPIV, offset_ipiv
                                 ,  - 1);
            }
            // *
            return;
            // *
            // *     End of DGETRS
            // *

            #endregion

        }
    }
}
