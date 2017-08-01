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
    /// DLARZ applies a real elementary reflector H to a real M-by-N
    /// matrix C, from either the left or the right. H is represented in the
    /// form
    /// 
    /// H = I - tau * v * v'
    /// 
    /// where tau is a real scalar and v is a real vector.
    /// 
    /// If tau = 0, then H is taken to be the unit matrix.
    ///</summary>
    public class DLARZ
    {
    

        #region Dependencies
        
        DAXPY _daxpy; DCOPY _dcopy; DGEMV _dgemv; DGER _dger; LSAME _lsame; 

        #endregion


        #region Variables
        
        const double ONE = 1.0E+0; const double ZERO = 0.0E+0; 

        #endregion

        public DLARZ(DAXPY daxpy, DCOPY dcopy, DGEMV dgemv, DGER dger, LSAME lsame)
        {
    

            #region Set Dependencies
            
            this._daxpy = daxpy; this._dcopy = dcopy; this._dgemv = dgemv; this._dger = dger; this._lsame = lsame; 

            #endregion

        }
    
        public DLARZ()
        {
    

            #region Dependencies (Initialization)
            
            DAXPY daxpy = new DAXPY();
            DCOPY dcopy = new DCOPY();
            LSAME lsame = new LSAME();
            XERBLA xerbla = new XERBLA();
            DGEMV dgemv = new DGEMV(lsame, xerbla);
            DGER dger = new DGER(xerbla);

            #endregion


            #region Set Dependencies
            
            this._daxpy = daxpy; this._dcopy = dcopy; this._dgemv = dgemv; this._dger = dger; this._lsame = lsame; 

            #endregion

        }
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DLARZ applies a real elementary reflector H to a real M-by-N
        /// matrix C, from either the left or the right. H is represented in the
        /// form
        /// 
        /// H = I - tau * v * v'
        /// 
        /// where tau is a real scalar and v is a real vector.
        /// 
        /// If tau = 0, then H is taken to be the unit matrix.
        ///</summary>
        /// <param name="SIDE">
        /// (input) CHARACTER*1
        /// = 'L': form  H * C
        /// = 'R': form  C * H
        ///</param>
        /// <param name="M">
        /// (input) INTEGER
        /// The number of rows of the matrix C.
        ///</param>
        /// <param name="N">
        /// (input) INTEGER
        /// The number of columns of the matrix C.
        ///</param>
        /// <param name="L">
        /// (input) INTEGER
        /// The number of entries of the vector V containing
        /// the meaningful part of the Householder vectors.
        /// If SIDE = 'L', M .GE. L .GE. 0, if SIDE = 'R', N .GE. L .GE. 0.
        ///</param>
        /// <param name="V">
        /// (input) DOUBLE PRECISION array, dimension (1+(L-1)*abs(INCV))
        /// The vector v in the representation of H as returned by
        /// DTZRZF. V is not used if TAU = 0.
        ///</param>
        /// <param name="INCV">
        /// (input) INTEGER
        /// The increment between elements of v. INCV .LT..GT. 0.
        ///</param>
        /// <param name="TAU">
        /// (input) DOUBLE PRECISION
        /// The value tau in the representation of H.
        ///</param>
        /// <param name="C">
        /// (input/output) DOUBLE PRECISION array, dimension (LDC,N)
        /// On entry, the M-by-N matrix C.
        /// On exit, C is overwritten by the matrix H * C if SIDE = 'L',
        /// or C * H if SIDE = 'R'.
        ///</param>
        /// <param name="LDC">
        /// (input) INTEGER
        /// The leading dimension of the array C. LDC .GE. max(1,M).
        ///</param>
        /// <param name="WORK">
        /// (workspace) DOUBLE PRECISION array, dimension
        /// (N) if SIDE = 'L'
        /// or (M) if SIDE = 'R'
        ///</param>
        public void Run(string SIDE, int M, int N, int L, double[] V, int offset_v, int INCV
                         , double TAU, ref double[] C, int offset_c, int LDC, ref double[] WORK, int offset_work)
        {

            #region Array Index Correction
            
             int o_v = -1 + offset_v;  int o_c = -1 - LDC + offset_c;  int o_work = -1 + offset_work; 

            #endregion


            #region Strings
            
            SIDE = SIDE.Substring(0, 1);  

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
            // *  DLARZ applies a real elementary reflector H to a real M-by-N
            // *  matrix C, from either the left or the right. H is represented in the
            // *  form
            // *
            // *        H = I - tau * v * v'
            // *
            // *  where tau is a real scalar and v is a real vector.
            // *
            // *  If tau = 0, then H is taken to be the unit matrix.
            // *
            // *
            // *  H is a product of k elementary reflectors as returned by DTZRZF.
            // *
            // *  Arguments
            // *  =========
            // *
            // *  SIDE    (input) CHARACTER*1
            // *          = 'L': form  H * C
            // *          = 'R': form  C * H
            // *
            // *  M       (input) INTEGER
            // *          The number of rows of the matrix C.
            // *
            // *  N       (input) INTEGER
            // *          The number of columns of the matrix C.
            // *
            // *  L       (input) INTEGER
            // *          The number of entries of the vector V containing
            // *          the meaningful part of the Householder vectors.
            // *          If SIDE = 'L', M >= L >= 0, if SIDE = 'R', N >= L >= 0.
            // *
            // *  V       (input) DOUBLE PRECISION array, dimension (1+(L-1)*abs(INCV))
            // *          The vector v in the representation of H as returned by
            // *          DTZRZF. V is not used if TAU = 0.
            // *
            // *  INCV    (input) INTEGER
            // *          The increment between elements of v. INCV <> 0.
            // *
            // *  TAU     (input) DOUBLE PRECISION
            // *          The value tau in the representation of H.
            // *
            // *  C       (input/output) DOUBLE PRECISION array, dimension (LDC,N)
            // *          On entry, the M-by-N matrix C.
            // *          On exit, C is overwritten by the matrix H * C if SIDE = 'L',
            // *          or C * H if SIDE = 'R'.
            // *
            // *  LDC     (input) INTEGER
            // *          The leading dimension of the array C. LDC >= max(1,M).
            // *
            // *  WORK    (workspace) DOUBLE PRECISION array, dimension
            // *                         (N) if SIDE = 'L'
            // *                      or (M) if SIDE = 'R'
            // *
            // *  Further Details
            // *  ===============
            // *
            // *  Based on contributions by
            // *    A. Petitet, Computer Science Dept., Univ. of Tenn., Knoxville, USA
            // *
            // *  =====================================================================
            // *
            // *     .. Parameters ..
            // *     ..
            // *     .. External Subroutines ..
            // *     ..
            // *     .. External Functions ..
            // *     ..
            // *     .. Executable Statements ..
            // *

            #endregion


            #region Body
            
            if (this._lsame.Run(SIDE, "L"))
            {
                // *
                // *        Form  H * C
                // *
                if (TAU != ZERO)
                {
                    // *
                    // *           w( 1:n ) = C( 1, 1:n )
                    // *
                    this._dcopy.Run(N, C, offset_c, LDC, ref WORK, offset_work, 1);
                    // *
                    // *           w( 1:n ) = w( 1:n ) + C( m-l+1:m, 1:n )' * v( 1:l )
                    // *
                    this._dgemv.Run("Transpose", L, N, ONE, C, M - L + 1+1 * LDC + o_c, LDC
                                    , V, offset_v, INCV, ONE, ref WORK, offset_work, 1);
                    // *
                    // *           C( 1, 1:n ) = C( 1, 1:n ) - tau * w( 1:n )
                    // *
                    this._daxpy.Run(N,  - TAU, WORK, offset_work, 1, ref C, offset_c, LDC);
                    // *
                    // *           C( m-l+1:m, 1:n ) = C( m-l+1:m, 1:n ) - ...
                    // *                               tau * v( 1:l ) * w( 1:n )'
                    // *
                    this._dger.Run(L, N,  - TAU, V, offset_v, INCV, WORK, offset_work
                                   , 1, ref C, M - L + 1+1 * LDC + o_c, LDC);
                }
                // *
            }
            else
            {
                // *
                // *        Form  C * H
                // *
                if (TAU != ZERO)
                {
                    // *
                    // *           w( 1:m ) = C( 1:m, 1 )
                    // *
                    this._dcopy.Run(M, C, offset_c, 1, ref WORK, offset_work, 1);
                    // *
                    // *           w( 1:m ) = w( 1:m ) + C( 1:m, n-l+1:n, 1:n ) * v( 1:l )
                    // *
                    this._dgemv.Run("No transpose", M, L, ONE, C, 1+(N - L + 1) * LDC + o_c, LDC
                                    , V, offset_v, INCV, ONE, ref WORK, offset_work, 1);
                    // *
                    // *           C( 1:m, 1 ) = C( 1:m, 1 ) - tau * w( 1:m )
                    // *
                    this._daxpy.Run(M,  - TAU, WORK, offset_work, 1, ref C, offset_c, 1);
                    // *
                    // *           C( 1:m, n-l+1:n ) = C( 1:m, n-l+1:n ) - ...
                    // *                               tau * w( 1:m ) * v( 1:l )'
                    // *
                    this._dger.Run(M, L,  - TAU, WORK, offset_work, 1, V, offset_v
                                   , INCV, ref C, 1+(N - L + 1) * LDC + o_c, LDC);
                    // *
                }
                // *
            }
            // *
            return;
            // *
            // *     End of DLARZ
            // *

            #endregion

        }
    }
}
