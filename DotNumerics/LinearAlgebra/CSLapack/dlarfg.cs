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
    /// DLARFG generates a real elementary reflector H of order n, such
    /// that
    /// 
    /// H * ( alpha ) = ( beta ),   H' * H = I.
    /// (   x   )   (   0  )
    /// 
    /// where alpha and beta are scalars, and x is an (n-1)-element real
    /// vector. H is represented in the form
    /// 
    /// H = I - tau * ( 1 ) * ( 1 v' ) ,
    /// ( v )
    /// 
    /// where tau is a real scalar and v is a real (n-1)-element
    /// vector.
    /// 
    /// If the elements of x are all zero, then tau = 0 and H is taken to be
    /// the unit matrix.
    /// 
    /// Otherwise  1 .LE. tau .LE. 2.
    /// 
    ///</summary>
    public class DLARFG
    {
    

        #region Dependencies
        
        DLAMCH _dlamch; DLAPY2 _dlapy2; DNRM2 _dnrm2; DSCAL _dscal; 

        #endregion


        #region Variables
        
        const double ONE = 1.0E+0; const double ZERO = 0.0E+0; 

        #endregion

        public DLARFG(DLAMCH dlamch, DLAPY2 dlapy2, DNRM2 dnrm2, DSCAL dscal)
        {
    

            #region Set Dependencies
            
            this._dlamch = dlamch; this._dlapy2 = dlapy2; this._dnrm2 = dnrm2; this._dscal = dscal; 

            #endregion

        }
    
        public DLARFG()
        {
    

            #region Dependencies (Initialization)
            
            LSAME lsame = new LSAME();
            DLAMC3 dlamc3 = new DLAMC3();
            DLAPY2 dlapy2 = new DLAPY2();
            DNRM2 dnrm2 = new DNRM2();
            DSCAL dscal = new DSCAL();
            DLAMC1 dlamc1 = new DLAMC1(dlamc3);
            DLAMC4 dlamc4 = new DLAMC4(dlamc3);
            DLAMC5 dlamc5 = new DLAMC5(dlamc3);
            DLAMC2 dlamc2 = new DLAMC2(dlamc3, dlamc1, dlamc4, dlamc5);
            DLAMCH dlamch = new DLAMCH(lsame, dlamc2);

            #endregion


            #region Set Dependencies
            
            this._dlamch = dlamch; this._dlapy2 = dlapy2; this._dnrm2 = dnrm2; this._dscal = dscal; 

            #endregion

        }
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DLARFG generates a real elementary reflector H of order n, such
        /// that
        /// 
        /// H * ( alpha ) = ( beta ),   H' * H = I.
        /// (   x   )   (   0  )
        /// 
        /// where alpha and beta are scalars, and x is an (n-1)-element real
        /// vector. H is represented in the form
        /// 
        /// H = I - tau * ( 1 ) * ( 1 v' ) ,
        /// ( v )
        /// 
        /// where tau is a real scalar and v is a real (n-1)-element
        /// vector.
        /// 
        /// If the elements of x are all zero, then tau = 0 and H is taken to be
        /// the unit matrix.
        /// 
        /// Otherwise  1 .LE. tau .LE. 2.
        /// 
        ///</summary>
        /// <param name="N">
        /// (input) INTEGER
        /// The order of the elementary reflector.
        ///</param>
        /// <param name="ALPHA">
        /// (input/output) DOUBLE PRECISION
        /// On entry, the value alpha.
        /// On exit, it is overwritten with the value beta.
        ///</param>
        /// <param name="X">
        /// (input/output) DOUBLE PRECISION array, dimension
        /// (1+(N-2)*abs(INCX))
        /// On entry, the vector x.
        /// On exit, it is overwritten with the vector v.
        ///</param>
        /// <param name="INCX">
        /// (input) INTEGER
        /// The increment between elements of X. INCX .GT. 0.
        ///</param>
        /// <param name="TAU">
        /// (output) DOUBLE PRECISION
        /// The value tau.
        ///</param>
        public void Run(int N, ref double ALPHA, ref double[] X, int offset_x, int INCX, ref double TAU)
        {

            #region Variables
            
            int J = 0; int KNT = 0; double BETA = 0; double RSAFMN = 0; double SAFMIN = 0; double XNORM = 0; 

            #endregion


            #region Array Index Correction
            
             int o_x = -1 + offset_x; 

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
            // *  DLARFG generates a real elementary reflector H of order n, such
            // *  that
            // *
            // *        H * ( alpha ) = ( beta ),   H' * H = I.
            // *            (   x   )   (   0  )
            // *
            // *  where alpha and beta are scalars, and x is an (n-1)-element real
            // *  vector. H is represented in the form
            // *
            // *        H = I - tau * ( 1 ) * ( 1 v' ) ,
            // *                      ( v )
            // *
            // *  where tau is a real scalar and v is a real (n-1)-element
            // *  vector.
            // *
            // *  If the elements of x are all zero, then tau = 0 and H is taken to be
            // *  the unit matrix.
            // *
            // *  Otherwise  1 <= tau <= 2.
            // *
            // *  Arguments
            // *  =========
            // *
            // *  N       (input) INTEGER
            // *          The order of the elementary reflector.
            // *
            // *  ALPHA   (input/output) DOUBLE PRECISION
            // *          On entry, the value alpha.
            // *          On exit, it is overwritten with the value beta.
            // *
            // *  X       (input/output) DOUBLE PRECISION array, dimension
            // *                         (1+(N-2)*abs(INCX))
            // *          On entry, the vector x.
            // *          On exit, it is overwritten with the vector v.
            // *
            // *  INCX    (input) INTEGER
            // *          The increment between elements of X. INCX > 0.
            // *
            // *  TAU     (output) DOUBLE PRECISION
            // *          The value tau.
            // *
            // *  =====================================================================
            // *
            // *     .. Parameters ..
            // *     ..
            // *     .. Local Scalars ..
            // *     ..
            // *     .. External Functions ..
            // *     ..
            // *     .. Intrinsic Functions ..
            //      INTRINSIC          ABS, SIGN;
            // *     ..
            // *     .. External Subroutines ..
            // *     ..
            // *     .. Executable Statements ..
            // *

            #endregion


            #region Body
            
            if (N <= 1)
            {
                TAU = ZERO;
                return;
            }
            // *
            XNORM = this._dnrm2.Run(N - 1, X, offset_x, INCX);
            // *
            if (XNORM == ZERO)
            {
                // *
                // *        H  =  I
                // *
                TAU = ZERO;
            }
            else
            {
                // *
                // *        general case
                // *
                BETA =  - FortranLib.Sign(this._dlapy2.Run(ALPHA, XNORM),ALPHA);
                SAFMIN = this._dlamch.Run("S") / this._dlamch.Run("E");
                if (Math.Abs(BETA) < SAFMIN)
                {
                    // *
                    // *           XNORM, BETA may be inaccurate; scale X and recompute them
                    // *
                    RSAFMN = ONE / SAFMIN;
                    KNT = 0;
                LABEL10:;
                    KNT += 1;
                    this._dscal.Run(N - 1, RSAFMN, ref X, offset_x, INCX);
                    BETA *= RSAFMN;
                    ALPHA *= RSAFMN;
                    if (Math.Abs(BETA) < SAFMIN) goto LABEL10;
                    // *
                    // *           New BETA is at most 1, at least SAFMIN
                    // *
                    XNORM = this._dnrm2.Run(N - 1, X, offset_x, INCX);
                    BETA =  - FortranLib.Sign(this._dlapy2.Run(ALPHA, XNORM),ALPHA);
                    TAU = (BETA - ALPHA) / BETA;
                    this._dscal.Run(N - 1, ONE / (ALPHA - BETA), ref X, offset_x, INCX);
                    // *
                    // *           If ALPHA is subnormal, it may lose relative accuracy
                    // *
                    ALPHA = BETA;
                    for (J = 1; J <= KNT; J++)
                    {
                        ALPHA *= SAFMIN;
                    }
                }
                else
                {
                    TAU = (BETA - ALPHA) / BETA;
                    this._dscal.Run(N - 1, ONE / (ALPHA - BETA), ref X, offset_x, INCX);
                    ALPHA = BETA;
                }
            }
            // *
            return;
            // *
            // *     End of DLARFG
            // *

            #endregion

        }
    }
}
