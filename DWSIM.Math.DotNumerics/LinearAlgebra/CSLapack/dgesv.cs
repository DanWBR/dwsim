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
    /// -- LAPACK driver routine (version 3.1) --
    /// Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
    /// November 2006
    /// Purpose
    /// =======
    /// 
    /// DGESV computes the solution to a real system of linear equations
    /// A * X = B,
    /// where A is an N-by-N matrix and X and B are N-by-NRHS matrices.
    /// 
    /// The LU decomposition with partial pivoting and row interchanges is
    /// used to factor A as
    /// A = P * L * U,
    /// where P is a permutation matrix, L is unit lower triangular, and U is
    /// upper triangular.  The factored form of A is then used to solve the
    /// system of equations A * X = B.
    /// 
    ///</summary>
    public class DGESV
    {
    

        #region Dependencies
        
        DGETRF _dgetrf; DGETRS _dgetrs; XERBLA _xerbla; 

        #endregion

        public DGESV(DGETRF dgetrf, DGETRS dgetrs, XERBLA xerbla)
        {
    

            #region Set Dependencies
            
            this._dgetrf = dgetrf; this._dgetrs = dgetrs; this._xerbla = xerbla; 

            #endregion

        }
    
        public DGESV()
        {
    

            #region Dependencies (Initialization)
            
            LSAME lsame = new LSAME();
            XERBLA xerbla = new XERBLA();
            DLAMC3 dlamc3 = new DLAMC3();
            IDAMAX idamax = new IDAMAX();
            DSCAL dscal = new DSCAL();
            DSWAP dswap = new DSWAP();
            DLASWP dlaswp = new DLASWP();
            IEEECK ieeeck = new IEEECK();
            IPARMQ iparmq = new IPARMQ();
            DGEMM dgemm = new DGEMM(lsame, xerbla);
            DLAMC1 dlamc1 = new DLAMC1(dlamc3);
            DLAMC4 dlamc4 = new DLAMC4(dlamc3);
            DLAMC5 dlamc5 = new DLAMC5(dlamc3);
            DLAMC2 dlamc2 = new DLAMC2(dlamc3, dlamc1, dlamc4, dlamc5);
            DLAMCH dlamch = new DLAMCH(lsame, dlamc2);
            DGER dger = new DGER(xerbla);
            DGETF2 dgetf2 = new DGETF2(dlamch, idamax, dger, dscal, dswap, xerbla);
            DTRSM dtrsm = new DTRSM(lsame, xerbla);
            ILAENV ilaenv = new ILAENV(ieeeck, iparmq);
            DGETRF dgetrf = new DGETRF(dgemm, dgetf2, dlaswp, dtrsm, xerbla, ilaenv);
            DGETRS dgetrs = new DGETRS(lsame, dlaswp, dtrsm, xerbla);

            #endregion


            #region Set Dependencies
            
            this._dgetrf = dgetrf; this._dgetrs = dgetrs; this._xerbla = xerbla; 

            #endregion

        }
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DGESV computes the solution to a real system of linear equations
        /// A * X = B,
        /// where A is an N-by-N matrix and X and B are N-by-NRHS matrices.
        /// 
        /// The LU decomposition with partial pivoting and row interchanges is
        /// used to factor A as
        /// A = P * L * U,
        /// where P is a permutation matrix, L is unit lower triangular, and U is
        /// upper triangular.  The factored form of A is then used to solve the
        /// system of equations A * X = B.
        /// 
        ///</summary>
        /// <param name="N">
        /// (input) INTEGER
        /// The number of linear equations, i.e., the order of the
        /// matrix A.  N .GE. 0.
        ///</param>
        /// <param name="NRHS">
        /// (input) INTEGER
        /// The number of right hand sides, i.e., the number of columns
        /// of the matrix B.  NRHS .GE. 0.
        ///</param>
        /// <param name="A">
        /// (input/output) DOUBLE PRECISION array, dimension (LDA,N)
        /// On entry, the N-by-N coefficient matrix A.
        /// On exit, the factors L and U from the factorization
        /// A = P*L*U; the unit diagonal elements of L are not stored.
        ///</param>
        /// <param name="LDA">
        /// (input) INTEGER
        /// The leading dimension of the array A.  LDA .GE. max(1,N).
        ///</param>
        /// <param name="IPIV">
        /// (output) INTEGER array, dimension (N)
        /// The pivot indices that define the permutation matrix P;
        /// row i of the matrix was interchanged with row IPIV(i).
        ///</param>
        /// <param name="B">
        /// (input/output) DOUBLE PRECISION array, dimension (LDB,NRHS)
        /// On entry, the N-by-NRHS matrix of right hand side matrix B.
        /// On exit, if INFO = 0, the N-by-NRHS solution matrix X.
        ///</param>
        /// <param name="LDB">
        /// (input) INTEGER
        /// The leading dimension of the array B.  LDB .GE. max(1,N).
        ///</param>
        /// <param name="INFO">
        /// (output) INTEGER
        /// = 0:  successful exit
        /// .LT. 0:  if INFO = -i, the i-th argument had an illegal value
        /// .GT. 0:  if INFO = i, U(i,i) is exactly zero.  The factorization
        /// has been completed, but the factor U is exactly
        /// singular, so the solution could not be computed.
        ///</param>
        public void Run(int N, int NRHS, ref double[] A, int offset_a, int LDA, ref int[] IPIV, int offset_ipiv, ref double[] B, int offset_b
                         , int LDB, ref int INFO)
        {

            #region Array Index Correction
            
             int o_a = -1 - LDA + offset_a;  int o_ipiv = -1 + offset_ipiv;  int o_b = -1 - LDB + offset_b; 

            #endregion


            #region Prolog
            
            // *
            // *  -- LAPACK driver routine (version 3.1) --
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
            // *  DGESV computes the solution to a real system of linear equations
            // *     A * X = B,
            // *  where A is an N-by-N matrix and X and B are N-by-NRHS matrices.
            // *
            // *  The LU decomposition with partial pivoting and row interchanges is
            // *  used to factor A as
            // *     A = P * L * U,
            // *  where P is a permutation matrix, L is unit lower triangular, and U is
            // *  upper triangular.  The factored form of A is then used to solve the
            // *  system of equations A * X = B.
            // *
            // *  Arguments
            // *  =========
            // *
            // *  N       (input) INTEGER
            // *          The number of linear equations, i.e., the order of the
            // *          matrix A.  N >= 0.
            // *
            // *  NRHS    (input) INTEGER
            // *          The number of right hand sides, i.e., the number of columns
            // *          of the matrix B.  NRHS >= 0.
            // *
            // *  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
            // *          On entry, the N-by-N coefficient matrix A.
            // *          On exit, the factors L and U from the factorization
            // *          A = P*L*U; the unit diagonal elements of L are not stored.
            // *
            // *  LDA     (input) INTEGER
            // *          The leading dimension of the array A.  LDA >= max(1,N).
            // *
            // *  IPIV    (output) INTEGER array, dimension (N)
            // *          The pivot indices that define the permutation matrix P;
            // *          row i of the matrix was interchanged with row IPIV(i).
            // *
            // *  B       (input/output) DOUBLE PRECISION array, dimension (LDB,NRHS)
            // *          On entry, the N-by-NRHS matrix of right hand side matrix B.
            // *          On exit, if INFO = 0, the N-by-NRHS solution matrix X.
            // *
            // *  LDB     (input) INTEGER
            // *          The leading dimension of the array B.  LDB >= max(1,N).
            // *
            // *  INFO    (output) INTEGER
            // *          = 0:  successful exit
            // *          < 0:  if INFO = -i, the i-th argument had an illegal value
            // *          > 0:  if INFO = i, U(i,i) is exactly zero.  The factorization
            // *                has been completed, but the factor U is exactly
            // *                singular, so the solution could not be computed.
            // *
            // *  =====================================================================
            // *
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
            if (N < 0)
            {
                INFO =  - 1;
            }
            else
            {
                if (NRHS < 0)
                {
                    INFO =  - 2;
                }
                else
                {
                    if (LDA < Math.Max(1, N))
                    {
                        INFO =  - 4;
                    }
                    else
                    {
                        if (LDB < Math.Max(1, N))
                        {
                            INFO =  - 7;
                        }
                    }
                }
            }
            if (INFO != 0)
            {
                this._xerbla.Run("DGESV ",  - INFO);
                return;
            }
            // *
            // *     Compute the LU factorization of A.
            // *
            this._dgetrf.Run(N, N, ref A, offset_a, LDA, ref IPIV, offset_ipiv, ref INFO);
            if (INFO == 0)
            {
                // *
                // *        Solve the system A*X = B, overwriting B with X.
                // *
                this._dgetrs.Run("No transpose", N, NRHS, A, offset_a, LDA, IPIV, offset_ipiv
                                 , ref B, offset_b, LDB, ref INFO);
            }
            return;
            // *
            // *     End of DGESV
            // *

            #endregion

        }
    }
}
