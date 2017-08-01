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
    /// DGETRF computes an LU factorization of a general M-by-N matrix A
    /// using partial pivoting with row interchanges.
    /// 
    /// The factorization has the form
    /// A = P * L * U
    /// where P is a permutation matrix, L is lower triangular with unit
    /// diagonal elements (lower trapezoidal if m .GT. n), and U is upper
    /// triangular (upper trapezoidal if m .LT. n).
    /// 
    /// This is the right-looking Level 3 BLAS version of the algorithm.
    /// 
    ///</summary>
    public class DGETRF
    {
    

        #region Dependencies
        
        DGEMM _dgemm; DGETF2 _dgetf2; DLASWP _dlaswp; DTRSM _dtrsm; XERBLA _xerbla; ILAENV _ilaenv; 

        #endregion


        #region Variables
        
        const double ONE = 1.0E+0; 

        #endregion

        public DGETRF(DGEMM dgemm, DGETF2 dgetf2, DLASWP dlaswp, DTRSM dtrsm, XERBLA xerbla, ILAENV ilaenv)
        {
    

            #region Set Dependencies
            
            this._dgemm = dgemm; this._dgetf2 = dgetf2; this._dlaswp = dlaswp; this._dtrsm = dtrsm; this._xerbla = xerbla; 
            this._ilaenv = ilaenv;

            #endregion

        }
    
        public DGETRF()
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

            #endregion


            #region Set Dependencies
            
            this._dgemm = dgemm; this._dgetf2 = dgetf2; this._dlaswp = dlaswp; this._dtrsm = dtrsm; this._xerbla = xerbla; 
            this._ilaenv = ilaenv;

            #endregion

        }
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DGETRF computes an LU factorization of a general M-by-N matrix A
        /// using partial pivoting with row interchanges.
        /// 
        /// The factorization has the form
        /// A = P * L * U
        /// where P is a permutation matrix, L is lower triangular with unit
        /// diagonal elements (lower trapezoidal if m .GT. n), and U is upper
        /// triangular (upper trapezoidal if m .LT. n).
        /// 
        /// This is the right-looking Level 3 BLAS version of the algorithm.
        /// 
        ///</summary>
        /// <param name="M">
        /// (input) INTEGER
        /// The number of rows of the matrix A.  M .GE. 0.
        ///</param>
        /// <param name="N">
        /// (input) INTEGER
        /// The number of columns of the matrix A.  N .GE. 0.
        ///</param>
        /// <param name="A">
        /// (input/output) DOUBLE PRECISION array, dimension (LDA,N)
        /// On entry, the M-by-N matrix to be factored.
        /// On exit, the factors L and U from the factorization
        /// A = P*L*U; the unit diagonal elements of L are not stored.
        ///</param>
        /// <param name="LDA">
        /// (input) INTEGER
        /// The leading dimension of the array A.  LDA .GE. max(1,M).
        ///</param>
        /// <param name="IPIV">
        /// (output) INTEGER array, dimension (min(M,N))
        /// The pivot indices; for 1 .LE. i .LE. min(M,N), row i of the
        /// matrix was interchanged with row IPIV(i).
        ///</param>
        /// <param name="INFO">
        /// (output) INTEGER
        /// = 0:  successful exit
        /// .LT. 0:  if INFO = -i, the i-th argument had an illegal value
        /// .GT. 0:  if INFO = i, U(i,i) is exactly zero. The factorization
        /// has been completed, but the factor U is exactly
        /// singular, and division by zero will occur if it is used
        /// to solve a system of equations.
        ///</param>
        public void Run(int M, int N, ref double[] A, int offset_a, int LDA, ref int[] IPIV, int offset_ipiv, ref int INFO)
        {

            #region Variables
            
            int I = 0; int IINFO = 0; int J = 0; int JB = 0; int NB = 0; 

            #endregion


            #region Array Index Correction
            
             int o_a = -1 - LDA + offset_a;  int o_ipiv = -1 + offset_ipiv; 

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
            // *  DGETRF computes an LU factorization of a general M-by-N matrix A
            // *  using partial pivoting with row interchanges.
            // *
            // *  The factorization has the form
            // *     A = P * L * U
            // *  where P is a permutation matrix, L is lower triangular with unit
            // *  diagonal elements (lower trapezoidal if m > n), and U is upper
            // *  triangular (upper trapezoidal if m < n).
            // *
            // *  This is the right-looking Level 3 BLAS version of the algorithm.
            // *
            // *  Arguments
            // *  =========
            // *
            // *  M       (input) INTEGER
            // *          The number of rows of the matrix A.  M >= 0.
            // *
            // *  N       (input) INTEGER
            // *          The number of columns of the matrix A.  N >= 0.
            // *
            // *  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
            // *          On entry, the M-by-N matrix to be factored.
            // *          On exit, the factors L and U from the factorization
            // *          A = P*L*U; the unit diagonal elements of L are not stored.
            // *
            // *  LDA     (input) INTEGER
            // *          The leading dimension of the array A.  LDA >= max(1,M).
            // *
            // *  IPIV    (output) INTEGER array, dimension (min(M,N))
            // *          The pivot indices; for 1 <= i <= min(M,N), row i of the
            // *          matrix was interchanged with row IPIV(i).
            // *
            // *  INFO    (output) INTEGER
            // *          = 0:  successful exit
            // *          < 0:  if INFO = -i, the i-th argument had an illegal value
            // *          > 0:  if INFO = i, U(i,i) is exactly zero. The factorization
            // *                has been completed, but the factor U is exactly
            // *                singular, and division by zero will occur if it is used
            // *                to solve a system of equations.
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
            // *     Test the input parameters.
            // *

            #endregion


            #region Body
            
            INFO = 0;
            if (M < 0)
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
                    if (LDA < Math.Max(1, M))
                    {
                        INFO =  - 4;
                    }
                }
            }
            if (INFO != 0)
            {
                this._xerbla.Run("DGETRF",  - INFO);
                return;
            }
            // *
            // *     Quick return if possible
            // *
            if (M == 0 || N == 0) return;
            // *
            // *     Determine the block size for this environment.
            // *
            NB = this._ilaenv.Run(1, "DGETRF", " ", M, N,  - 1,  - 1);
            if (NB <= 1 || NB >= Math.Min(M, N))
            {
                // *
                // *        Use unblocked code.
                // *
                this._dgetf2.Run(M, N, ref A, offset_a, LDA, ref IPIV, offset_ipiv, ref INFO);
            }
            else
            {
                // *
                // *        Use blocked code.
                // *
                for (J = 1; (NB >= 0) ? (J <= Math.Min(M, N)) : (J >= Math.Min(M, N)); J += NB)
                {
                    JB = Math.Min(Math.Min(M, N) - J + 1, NB);
                    // *
                    // *           Factor diagonal and subdiagonal blocks and test for exact
                    // *           singularity.
                    // *
                    this._dgetf2.Run(M - J + 1, JB, ref A, J+J * LDA + o_a, LDA, ref IPIV, J + o_ipiv, ref IINFO);
                    // *
                    // *           Adjust INFO and the pivot indices.
                    // *
                    if (INFO == 0 && IINFO > 0) INFO = IINFO + J - 1;
                    for (I = J; I <= Math.Min(M, J + JB - 1); I++)
                    {
                        IPIV[I + o_ipiv] = J - 1 + IPIV[I + o_ipiv];
                    }
                    // *
                    // *           Apply interchanges to columns 1:J-1.
                    // *
                    this._dlaswp.Run(J - 1, ref A, offset_a, LDA, J, J + JB - 1, IPIV, offset_ipiv
                                     , 1);
                    // *
                    if (J + JB <= N)
                    {
                        // *
                        // *              Apply interchanges to columns J+JB:N.
                        // *
                        this._dlaswp.Run(N - J - JB + 1, ref A, 1+(J + JB) * LDA + o_a, LDA, J, J + JB - 1, IPIV, offset_ipiv
                                         , 1);
                        // *
                        // *              Compute block row of U.
                        // *
                        this._dtrsm.Run("Left", "Lower", "No transpose", "Unit", JB, N - J - JB + 1
                                        , ONE, A, J+J * LDA + o_a, LDA, ref A, J+(J + JB) * LDA + o_a, LDA);
                        if (J + JB <= M)
                        {
                            // *
                            // *                 Update trailing submatrix.
                            // *
                            this._dgemm.Run("No transpose", "No transpose", M - J - JB + 1, N - J - JB + 1, JB,  - ONE
                                            , A, J + JB+J * LDA + o_a, LDA, A, J+(J + JB) * LDA + o_a, LDA, ONE, ref A, J + JB+(J + JB) * LDA + o_a
                                            , LDA);
                        }
                    }
                }
            }
            return;
            // *
            // *     End of DGETRF
            // *

            #endregion

        }
    }
}
