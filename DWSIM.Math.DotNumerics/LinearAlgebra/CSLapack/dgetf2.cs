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
    /// DGETF2 computes an LU factorization of a general m-by-n matrix A
    /// using partial pivoting with row interchanges.
    /// 
    /// The factorization has the form
    /// A = P * L * U
    /// where P is a permutation matrix, L is lower triangular with unit
    /// diagonal elements (lower trapezoidal if m .GT. n), and U is upper
    /// triangular (upper trapezoidal if m .LT. n).
    /// 
    /// This is the right-looking Level 2 BLAS version of the algorithm.
    /// 
    ///</summary>
    public class DGETF2
    {
    

        #region Dependencies
        
        DLAMCH _dlamch; IDAMAX _idamax; DGER _dger; DSCAL _dscal; DSWAP _dswap; XERBLA _xerbla; 

        #endregion


        #region Variables
        
        const double ONE = 1.0E+0; const double ZERO = 0.0E+0; 

        #endregion

        public DGETF2(DLAMCH dlamch, IDAMAX idamax, DGER dger, DSCAL dscal, DSWAP dswap, XERBLA xerbla)
        {
    

            #region Set Dependencies
            
            this._dlamch = dlamch; this._idamax = idamax; this._dger = dger; this._dscal = dscal; this._dswap = dswap; 
            this._xerbla = xerbla;

            #endregion

        }
    
        public DGETF2()
        {
    

            #region Dependencies (Initialization)
            
            LSAME lsame = new LSAME();
            DLAMC3 dlamc3 = new DLAMC3();
            IDAMAX idamax = new IDAMAX();
            XERBLA xerbla = new XERBLA();
            DSCAL dscal = new DSCAL();
            DSWAP dswap = new DSWAP();
            DLAMC1 dlamc1 = new DLAMC1(dlamc3);
            DLAMC4 dlamc4 = new DLAMC4(dlamc3);
            DLAMC5 dlamc5 = new DLAMC5(dlamc3);
            DLAMC2 dlamc2 = new DLAMC2(dlamc3, dlamc1, dlamc4, dlamc5);
            DLAMCH dlamch = new DLAMCH(lsame, dlamc2);
            DGER dger = new DGER(xerbla);

            #endregion


            #region Set Dependencies
            
            this._dlamch = dlamch; this._idamax = idamax; this._dger = dger; this._dscal = dscal; this._dswap = dswap; 
            this._xerbla = xerbla;

            #endregion

        }
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DGETF2 computes an LU factorization of a general m-by-n matrix A
        /// using partial pivoting with row interchanges.
        /// 
        /// The factorization has the form
        /// A = P * L * U
        /// where P is a permutation matrix, L is lower triangular with unit
        /// diagonal elements (lower trapezoidal if m .GT. n), and U is upper
        /// triangular (upper trapezoidal if m .LT. n).
        /// 
        /// This is the right-looking Level 2 BLAS version of the algorithm.
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
        /// On entry, the m by n matrix to be factored.
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
        /// = 0: successful exit
        /// .LT. 0: if INFO = -k, the k-th argument had an illegal value
        /// .GT. 0: if INFO = k, U(k,k) is exactly zero. The factorization
        /// has been completed, but the factor U is exactly
        /// singular, and division by zero will occur if it is used
        /// to solve a system of equations.
        ///</param>
        public void Run(int M, int N, ref double[] A, int offset_a, int LDA, ref int[] IPIV, int offset_ipiv, ref int INFO)
        {

            #region Variables
            
            double SFMIN = 0; int I = 0; int J = 0; int JP = 0; 

            #endregion


            #region Implicit Variables
            
            int A_J = 0; 

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
            // *  DGETF2 computes an LU factorization of a general m-by-n matrix A
            // *  using partial pivoting with row interchanges.
            // *
            // *  The factorization has the form
            // *     A = P * L * U
            // *  where P is a permutation matrix, L is lower triangular with unit
            // *  diagonal elements (lower trapezoidal if m > n), and U is upper
            // *  triangular (upper trapezoidal if m < n).
            // *
            // *  This is the right-looking Level 2 BLAS version of the algorithm.
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
            // *          On entry, the m by n matrix to be factored.
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
            // *          = 0: successful exit
            // *          < 0: if INFO = -k, the k-th argument had an illegal value
            // *          > 0: if INFO = k, U(k,k) is exactly zero. The factorization
            // *               has been completed, but the factor U is exactly
            // *               singular, and division by zero will occur if it is used
            // *               to solve a system of equations.
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
                this._xerbla.Run("DGETF2",  - INFO);
                return;
            }
            // *
            // *     Quick return if possible
            // *
            if (M == 0 || N == 0) return;
            // *
            // *     Compute machine safe minimum 
            // * 
            SFMIN = this._dlamch.Run("S");
            // *
            for (J = 1; J <= Math.Min(M, N); J++)
            {
                // *
                // *        Find pivot and test for singularity.
                // *
                JP = J - 1 + this._idamax.Run(M - J + 1, A, J+J * LDA + o_a, 1);
                IPIV[J + o_ipiv] = JP;
                if (A[JP+J * LDA + o_a] != ZERO)
                {
                    // *
                    // *           Apply the interchange to columns 1:N.
                    // *
                    if (JP != J) this._dswap.Run(N, ref A, J+1 * LDA + o_a, LDA, ref A, JP+1 * LDA + o_a, LDA);
                    // *
                    // *           Compute elements J+1:M of J-th column.
                    // *
                    if (J < M)
                    {
                        if (Math.Abs(A[J+J * LDA + o_a]) >= SFMIN)
                        {
                            this._dscal.Run(M - J, ONE / A[J+J * LDA + o_a], ref A, J + 1+J * LDA + o_a, 1);
                        }
                        else
                        {
                            A_J = J * LDA + o_a;
                            for (I = 1; I <= M - J; I++)
                            {
                                A[J + I + A_J] /= A[J+J * LDA + o_a];
                            }
                        }
                    }
                    // *
                }
                else
                {
                    if (INFO == 0)
                    {
                        // *
                        INFO = J;
                    }
                }
                // *
                if (J < Math.Min(M, N))
                {
                    // *
                    // *           Update trailing submatrix.
                    // *
                    this._dger.Run(M - J, N - J,  - ONE, A, J + 1+J * LDA + o_a, 1, A, J+(J + 1) * LDA + o_a
                                   , LDA, ref A, J + 1+(J + 1) * LDA + o_a, LDA);
                }
            }
            return;
            // *
            // *     End of DGETF2
            // *

            #endregion

        }
    }
}
