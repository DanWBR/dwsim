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
    /// -- LAPACK auxiliary routine (version 3.1) --
    /// Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
    /// November 2006
    /// Purpose
    /// =======
    /// 
    /// Given two column vectors X and Y, let
    /// 
    /// A = ( X Y ).
    /// 
    /// The subroutine first computes the QR factorization of A = Q*R,
    /// and then computes the SVD of the 2-by-2 upper triangular matrix R.
    /// The smaller singular value of R is returned in SSMIN, which is used
    /// as the measurement of the linear dependency of the vectors X and Y.
    /// 
    ///</summary>
    public class DLAPLL
    {
    

        #region Dependencies
        
        DDOT _ddot; DAXPY _daxpy; DLARFG _dlarfg; DLAS2 _dlas2; 

        #endregion


        #region Variables
        
        const double ZERO = 0.0E+0; const double ONE = 1.0E+0; 

        #endregion

        public DLAPLL(DDOT ddot, DAXPY daxpy, DLARFG dlarfg, DLAS2 dlas2)
        {
    

            #region Set Dependencies
            
            this._ddot = ddot; this._daxpy = daxpy; this._dlarfg = dlarfg; this._dlas2 = dlas2; 

            #endregion

        }
    
        public DLAPLL()
        {
    

            #region Dependencies (Initialization)
            
            DDOT ddot = new DDOT();
            DAXPY daxpy = new DAXPY();
            LSAME lsame = new LSAME();
            DLAMC3 dlamc3 = new DLAMC3();
            DLAPY2 dlapy2 = new DLAPY2();
            DNRM2 dnrm2 = new DNRM2();
            DSCAL dscal = new DSCAL();
            DLAS2 dlas2 = new DLAS2();
            DLAMC1 dlamc1 = new DLAMC1(dlamc3);
            DLAMC4 dlamc4 = new DLAMC4(dlamc3);
            DLAMC5 dlamc5 = new DLAMC5(dlamc3);
            DLAMC2 dlamc2 = new DLAMC2(dlamc3, dlamc1, dlamc4, dlamc5);
            DLAMCH dlamch = new DLAMCH(lsame, dlamc2);
            DLARFG dlarfg = new DLARFG(dlamch, dlapy2, dnrm2, dscal);

            #endregion


            #region Set Dependencies
            
            this._ddot = ddot; this._daxpy = daxpy; this._dlarfg = dlarfg; this._dlas2 = dlas2; 

            #endregion

        }
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// Given two column vectors X and Y, let
        /// 
        /// A = ( X Y ).
        /// 
        /// The subroutine first computes the QR factorization of A = Q*R,
        /// and then computes the SVD of the 2-by-2 upper triangular matrix R.
        /// The smaller singular value of R is returned in SSMIN, which is used
        /// as the measurement of the linear dependency of the vectors X and Y.
        /// 
        ///</summary>
        /// <param name="N">
        /// (input) INTEGER
        /// The length of the vectors X and Y.
        ///</param>
        /// <param name="X">
        /// (input/output) DOUBLE PRECISION array,
        /// dimension (1+(N-1)*INCX)
        /// On entry, X contains the N-vector X.
        /// On exit, X is overwritten.
        ///</param>
        /// <param name="INCX">
        /// (input) INTEGER
        /// The increment between successive elements of X. INCX .GT. 0.
        ///</param>
        /// <param name="Y">
        /// (input/output) DOUBLE PRECISION array,
        /// dimension (1+(N-1)*INCY)
        /// On entry, Y contains the N-vector Y.
        /// On exit, Y is overwritten.
        ///</param>
        /// <param name="INCY">
        /// (input) INTEGER
        /// The increment between successive elements of Y. INCY .GT. 0.
        ///</param>
        /// <param name="SSMIN">
        /// (output) DOUBLE PRECISION
        /// The smallest singular value of the N-by-2 matrix A = ( X Y ).
        ///</param>
        public void Run(int N, ref double[] X, int offset_x, int INCX, ref double[] Y, int offset_y, int INCY, ref double SSMIN)
        {

            #region Variables
            
            double A11 = 0; double A12 = 0; double A22 = 0; double C = 0; double SSMAX = 0; double TAU = 0; 

            #endregion


            #region Array Index Correction
            
             int o_x = -1 + offset_x;  int o_y = -1 + offset_y; 

            #endregion


            #region Prolog
            
            // *
            // *  -- LAPACK auxiliary routine (version 3.1) --
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
            // *  Given two column vectors X and Y, let
            // *
            // *                       A = ( X Y ).
            // *
            // *  The subroutine first computes the QR factorization of A = Q*R,
            // *  and then computes the SVD of the 2-by-2 upper triangular matrix R.
            // *  The smaller singular value of R is returned in SSMIN, which is used
            // *  as the measurement of the linear dependency of the vectors X and Y.
            // *
            // *  Arguments
            // *  =========
            // *
            // *  N       (input) INTEGER
            // *          The length of the vectors X and Y.
            // *
            // *  X       (input/output) DOUBLE PRECISION array,
            // *                         dimension (1+(N-1)*INCX)
            // *          On entry, X contains the N-vector X.
            // *          On exit, X is overwritten.
            // *
            // *  INCX    (input) INTEGER
            // *          The increment between successive elements of X. INCX > 0.
            // *
            // *  Y       (input/output) DOUBLE PRECISION array,
            // *                         dimension (1+(N-1)*INCY)
            // *          On entry, Y contains the N-vector Y.
            // *          On exit, Y is overwritten.
            // *
            // *  INCY    (input) INTEGER
            // *          The increment between successive elements of Y. INCY > 0.
            // *
            // *  SSMIN   (output) DOUBLE PRECISION
            // *          The smallest singular value of the N-by-2 matrix A = ( X Y ).
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
            // *     .. Executable Statements ..
            // *
            // *     Quick return if possible
            // *

            #endregion


            #region Body
            
            if (N <= 1)
            {
                SSMIN = ZERO;
                return;
            }
            // *
            // *     Compute the QR factorization of the N-by-2 matrix ( X Y )
            // *
            this._dlarfg.Run(N, ref X[1 + o_x], ref X, 1 + INCX + o_x, INCX, ref TAU);
            A11 = X[1 + o_x];
            X[1 + o_x] = ONE;
            // *
            C =  - TAU * this._ddot.Run(N, X, offset_x, INCX, Y, offset_y, INCY);
            this._daxpy.Run(N, C, X, offset_x, INCX, ref Y, offset_y, INCY);
            // *
            this._dlarfg.Run(N - 1, ref Y[1 + INCY + o_y], ref Y, 1 + 2 * INCY + o_y, INCY, ref TAU);
            // *
            A12 = Y[1 + o_y];
            A22 = Y[1 + INCY + o_y];
            // *
            // *     Compute the SVD of 2-by-2 Upper triangular matrix.
            // *
            this._dlas2.Run(A11, A12, A22, ref SSMIN, ref SSMAX);
            // *
            return;
            // *
            // *     End of DLAPLL
            // *

            #endregion

        }
    }
}
