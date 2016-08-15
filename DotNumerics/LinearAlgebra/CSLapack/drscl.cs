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
    /// -- LAPACK auxiliary routine (version 3.0) --
    /// Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
    /// Courant Institute, Argonne National Lab, and Rice University
    /// September 30, 1994
    /// Purpose
    /// =======
    /// 
    /// DRSCL multiplies an n-element real vector x by the real scalar 1/a.
    /// This is done without overflow or underflow as long as
    /// the final result x/a does not overflow or underflow.
    /// 
    ///</summary>
    public class DRSCL
    {
    

        #region Dependencies
        
        DLAMCH _dlamch; DSCAL _dscal; DLABAD _dlabad; 

        #endregion


        #region Variables
        
        const double ONE = 1.0E+0; const double ZERO = 0.0E+0; 

        #endregion

        public DRSCL(DLAMCH dlamch, DSCAL dscal, DLABAD dlabad)
        {
    

            #region Set Dependencies
            
            this._dlamch = dlamch; this._dscal = dscal; this._dlabad = dlabad; 

            #endregion

        }
    
        public DRSCL()
        {
    

            #region Dependencies (Initialization)
            
            LSAME lsame = new LSAME();
            DLAMC3 dlamc3 = new DLAMC3();
            DSCAL dscal = new DSCAL();
            DLABAD dlabad = new DLABAD();
            DLAMC1 dlamc1 = new DLAMC1(dlamc3);
            DLAMC4 dlamc4 = new DLAMC4(dlamc3);
            DLAMC5 dlamc5 = new DLAMC5(dlamc3);
            DLAMC2 dlamc2 = new DLAMC2(dlamc3, dlamc1, dlamc4, dlamc5);
            DLAMCH dlamch = new DLAMCH(lsame, dlamc2);

            #endregion


            #region Set Dependencies
            
            this._dlamch = dlamch; this._dscal = dscal; this._dlabad = dlabad; 

            #endregion

        }
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DRSCL multiplies an n-element real vector x by the real scalar 1/a.
        /// This is done without overflow or underflow as long as
        /// the final result x/a does not overflow or underflow.
        /// 
        ///</summary>
        /// <param name="N">
        /// (input) INTEGER
        /// The number of components of the vector x.
        ///</param>
        /// <param name="SA">
        /// (input) DOUBLE PRECISION
        /// The scalar a which is used to divide each component of x.
        /// SA must be .GE. 0, or the subroutine will divide by zero.
        ///</param>
        /// <param name="SX">
        /// (input/output) DOUBLE PRECISION array, dimension
        /// (1+(N-1)*abs(INCX))
        /// The n-element vector x.
        ///</param>
        /// <param name="INCX">
        /// (input) INTEGER
        /// The increment between successive values of the vector SX.
        /// .GT. 0:  SX(1) = X(1) and SX(1+(i-1)*INCX) = x(i),     1.LT. i.LE. n
        ///</param>
        public void Run(int N, double SA, ref double[] SX, int offset_sx, int INCX)
        {

            #region Variables
            
            bool DONE = false; double BIGNUM = 0; double CDEN = 0; double CDEN1 = 0; double CNUM = 0; double CNUM1 = 0; 
            double MUL = 0;double SMLNUM = 0; 

            #endregion


            #region Array Index Correction
            
             int o_sx = -1 + offset_sx; 

            #endregion


            #region Prolog
            
            // *
            // *  -- LAPACK auxiliary routine (version 3.0) --
            // *     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
            // *     Courant Institute, Argonne National Lab, and Rice University
            // *     September 30, 1994
            // *
            // *     .. Scalar Arguments ..
            // *     ..
            // *     .. Array Arguments ..
            // *     ..
            // *
            // *  Purpose
            // *  =======
            // *
            // *  DRSCL multiplies an n-element real vector x by the real scalar 1/a.
            // *  This is done without overflow or underflow as long as
            // *  the final result x/a does not overflow or underflow.
            // *
            // *  Arguments
            // *  =========
            // *
            // *  N       (input) INTEGER
            // *          The number of components of the vector x.
            // *
            // *  SA      (input) DOUBLE PRECISION
            // *          The scalar a which is used to divide each component of x.
            // *          SA must be >= 0, or the subroutine will divide by zero.
            // *
            // *  SX      (input/output) DOUBLE PRECISION array, dimension
            // *                         (1+(N-1)*abs(INCX))
            // *          The n-element vector x.
            // *
            // *  INCX    (input) INTEGER
            // *          The increment between successive values of the vector SX.
            // *          > 0:  SX(1) = X(1) and SX(1+(i-1)*INCX) = x(i),     1< i<= n
            // *
            // * =====================================================================
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
            //      INTRINSIC          ABS;
            // *     ..
            // *     .. Executable Statements ..
            // *
            // *     Quick return if possible
            // *

            #endregion


            #region Body
            
            if (N <= 0) return;
            // *
            // *     Get machine parameters
            // *
            SMLNUM = this._dlamch.Run("S");
            BIGNUM = ONE / SMLNUM;
            this._dlabad.Run(ref SMLNUM, ref BIGNUM);
            // *
            // *     Initialize the denominator to SA and the numerator to 1.
            // *
            CDEN = SA;
            CNUM = ONE;
            // *
        LABEL10:;
            CDEN1 = CDEN * SMLNUM;
            CNUM1 = CNUM / BIGNUM;
            if (Math.Abs(CDEN1) > Math.Abs(CNUM) && CNUM != ZERO)
            {
                // *
                // *        Pre-multiply X by SMLNUM if CDEN is large compared to CNUM.
                // *
                MUL = SMLNUM;
                DONE = false;
                CDEN = CDEN1;
            }
            else
            {
                if (Math.Abs(CNUM1) > Math.Abs(CDEN))
                {
                    // *
                    // *        Pre-multiply X by BIGNUM if CDEN is small compared to CNUM.
                    // *
                    MUL = BIGNUM;
                    DONE = false;
                    CNUM = CNUM1;
                }
                else
                {
                    // *
                    // *        Multiply X by CNUM / CDEN and return.
                    // *
                    MUL = CNUM / CDEN;
                    DONE = true;
                }
            }
            // *
            // *     Scale the vector X by MUL
            // *
            this._dscal.Run(N, MUL, ref SX, offset_sx, INCX);
            // *
            if (!DONE) goto LABEL10;
            // *
            return;
            // *
            // *     End of DRSCL
            // *

            #endregion

        }
    }
}
