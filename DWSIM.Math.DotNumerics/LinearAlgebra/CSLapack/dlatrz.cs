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
    /// DLATRZ factors the M-by-(M+L) real upper trapezoidal matrix
    /// [ A1 A2 ] = [ A(1:M,1:M) A(1:M,N-L+1:N) ] as ( R  0 ) * Z, by means
    /// of orthogonal transformations.  Z is an (M+L)-by-(M+L) orthogonal
    /// matrix and, R and A1 are M-by-M upper triangular matrices.
    /// 
    ///</summary>
    public class DLATRZ
    {
    

        #region Dependencies
        
        DLARFG _dlarfg; DLARZ _dlarz; 

        #endregion


        #region Variables
        
        const double ZERO = 0.0E+0; 

        #endregion

        public DLATRZ(DLARFG dlarfg, DLARZ dlarz)
        {
    

            #region Set Dependencies
            
            this._dlarfg = dlarfg; this._dlarz = dlarz; 

            #endregion

        }
    
        public DLATRZ()
        {
    

            #region Dependencies (Initialization)
            
            LSAME lsame = new LSAME();
            DLAMC3 dlamc3 = new DLAMC3();
            DLAPY2 dlapy2 = new DLAPY2();
            DNRM2 dnrm2 = new DNRM2();
            DSCAL dscal = new DSCAL();
            DAXPY daxpy = new DAXPY();
            DCOPY dcopy = new DCOPY();
            XERBLA xerbla = new XERBLA();
            DLAMC1 dlamc1 = new DLAMC1(dlamc3);
            DLAMC4 dlamc4 = new DLAMC4(dlamc3);
            DLAMC5 dlamc5 = new DLAMC5(dlamc3);
            DLAMC2 dlamc2 = new DLAMC2(dlamc3, dlamc1, dlamc4, dlamc5);
            DLAMCH dlamch = new DLAMCH(lsame, dlamc2);
            DLARFG dlarfg = new DLARFG(dlamch, dlapy2, dnrm2, dscal);
            DGEMV dgemv = new DGEMV(lsame, xerbla);
            DGER dger = new DGER(xerbla);
            DLARZ dlarz = new DLARZ(daxpy, dcopy, dgemv, dger, lsame);

            #endregion


            #region Set Dependencies
            
            this._dlarfg = dlarfg; this._dlarz = dlarz; 

            #endregion

        }
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DLATRZ factors the M-by-(M+L) real upper trapezoidal matrix
        /// [ A1 A2 ] = [ A(1:M,1:M) A(1:M,N-L+1:N) ] as ( R  0 ) * Z, by means
        /// of orthogonal transformations.  Z is an (M+L)-by-(M+L) orthogonal
        /// matrix and, R and A1 are M-by-M upper triangular matrices.
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
        /// <param name="L">
        /// (input) INTEGER
        /// The number of columns of the matrix A containing the
        /// meaningful part of the Householder vectors. N-M .GE. L .GE. 0.
        ///</param>
        /// <param name="A">
        /// (input/output) DOUBLE PRECISION array, dimension (LDA,N)
        /// On entry, the leading M-by-N upper trapezoidal part of the
        /// array A must contain the matrix to be factorized.
        /// On exit, the leading M-by-M upper triangular part of A
        /// contains the upper triangular matrix R, and elements N-L+1 to
        /// N of the first M rows of A, with the array TAU, represent the
        /// orthogonal matrix Z as a product of M elementary reflectors.
        ///</param>
        /// <param name="LDA">
        /// (input) INTEGER
        /// The leading dimension of the array A.  LDA .GE. max(1,M).
        ///</param>
        /// <param name="TAU">
        /// (output) DOUBLE PRECISION array, dimension (M)
        /// The scalar factors of the elementary reflectors.
        ///</param>
        /// <param name="WORK">
        /// (workspace) DOUBLE PRECISION array, dimension (M)
        ///</param>
        public void Run(int M, int N, int L, ref double[] A, int offset_a, int LDA, ref double[] TAU, int offset_tau
                         , ref double[] WORK, int offset_work)
        {

            #region Variables
            
            int I = 0; 

            #endregion


            #region Array Index Correction
            
             int o_a = -1 - LDA + offset_a;  int o_tau = -1 + offset_tau;  int o_work = -1 + offset_work; 

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
            // *  DLATRZ factors the M-by-(M+L) real upper trapezoidal matrix
            // *  [ A1 A2 ] = [ A(1:M,1:M) A(1:M,N-L+1:N) ] as ( R  0 ) * Z, by means
            // *  of orthogonal transformations.  Z is an (M+L)-by-(M+L) orthogonal
            // *  matrix and, R and A1 are M-by-M upper triangular matrices.
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
            // *  L       (input) INTEGER
            // *          The number of columns of the matrix A containing the
            // *          meaningful part of the Householder vectors. N-M >= L >= 0.
            // *
            // *  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
            // *          On entry, the leading M-by-N upper trapezoidal part of the
            // *          array A must contain the matrix to be factorized.
            // *          On exit, the leading M-by-M upper triangular part of A
            // *          contains the upper triangular matrix R, and elements N-L+1 to
            // *          N of the first M rows of A, with the array TAU, represent the
            // *          orthogonal matrix Z as a product of M elementary reflectors.
            // *
            // *  LDA     (input) INTEGER
            // *          The leading dimension of the array A.  LDA >= max(1,M).
            // *
            // *  TAU     (output) DOUBLE PRECISION array, dimension (M)
            // *          The scalar factors of the elementary reflectors.
            // *
            // *  WORK    (workspace) DOUBLE PRECISION array, dimension (M)
            // *
            // *  Further Details
            // *  ===============
            // *
            // *  Based on contributions by
            // *    A. Petitet, Computer Science Dept., Univ. of Tenn., Knoxville, USA
            // *
            // *  The factorization is obtained by Householder's method.  The kth
            // *  transformation matrix, Z( k ), which is used to introduce zeros into
            // *  the ( m - k + 1 )th row of A, is given in the form
            // *
            // *     Z( k ) = ( I     0   ),
            // *              ( 0  T( k ) )
            // *
            // *  where
            // *
            // *     T( k ) = I - tau*u( k )*u( k )',   u( k ) = (   1    ),
            // *                                                 (   0    )
            // *                                                 ( z( k ) )
            // *
            // *  tau is a scalar and z( k ) is an l element vector. tau and z( k )
            // *  are chosen to annihilate the elements of the kth row of A2.
            // *
            // *  The scalar tau is returned in the kth element of TAU and the vector
            // *  u( k ) in the kth row of A2, such that the elements of z( k ) are
            // *  in  a( k, l + 1 ), ..., a( k, n ). The elements of R are returned in
            // *  the upper triangular part of A1.
            // *
            // *  Z is given by
            // *
            // *     Z =  Z( 1 ) * Z( 2 ) * ... * Z( m ).
            // *
            // *  =====================================================================
            // *
            // *     .. Parameters ..
            // *     ..
            // *     .. Local Scalars ..
            // *     ..
            // *     .. External Subroutines ..
            // *     ..
            // *     .. Executable Statements ..
            // *
            // *     Test the input arguments
            // *
            // *     Quick return if possible
            // *

            #endregion


            #region Body
            
            if (M == 0)
            {
                return;
            }
            else
            {
                if (M == N)
                {
                    for (I = 1; I <= N; I++)
                    {
                        TAU[I + o_tau] = ZERO;
                    }
                    return;
                }
            }
            // *
            for (I = M; I >= 1; I +=  - 1)
            {
                // *
                // *        Generate elementary reflector H(i) to annihilate
                // *        [ A(i,i) A(i,n-l+1:n) ]
                // *
                this._dlarfg.Run(L + 1, ref A[I+I * LDA + o_a], ref A, I+(N - L + 1) * LDA + o_a, LDA, ref TAU[I + o_tau]);
                // *
                // *        Apply H(i) to A(1:i-1,i:n) from the right
                // *
                this._dlarz.Run("Right", I - 1, N - I + 1, L, A, I+(N - L + 1) * LDA + o_a, LDA
                                , TAU[I + o_tau], ref A, 1+I * LDA + o_a, LDA, ref WORK, offset_work);
                // *
            }
            // *
            return;
            // *
            // *     End of DLATRZ
            // *

            #endregion

        }
    }
}
