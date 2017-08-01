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
    /// DLABAD takes as input the values computed by DLAMCH for underflow and
    /// overflow, and returns the square root of each of these values if the
    /// log of LARGE is sufficiently large.  This subroutine is intended to
    /// identify machines with a large exponent range, such as the Crays, and
    /// redefine the underflow and overflow limits to be the square roots of
    /// the values computed by DLAMCH.  This subroutine is needed because
    /// DLAMCH does not compensate for poor arithmetic in the upper half of
    /// the exponent range, as is found on a Cray.
    /// 
    ///</summary>
    public class DLABAD
    {
    
        public DLABAD()
        {
    
        }
    
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DLABAD takes as input the values computed by DLAMCH for underflow and
        /// overflow, and returns the square root of each of these values if the
        /// log of LARGE is sufficiently large.  This subroutine is intended to
        /// identify machines with a large exponent range, such as the Crays, and
        /// redefine the underflow and overflow limits to be the square roots of
        /// the values computed by DLAMCH.  This subroutine is needed because
        /// DLAMCH does not compensate for poor arithmetic in the upper half of
        /// the exponent range, as is found on a Cray.
        /// 
        ///</summary>
        /// <param name="SMALL">
        /// (input/output) DOUBLE PRECISION
        /// On entry, the underflow threshold as computed by DLAMCH.
        /// On exit, if LOG10(LARGE) is sufficiently large, the square
        /// root of SMALL, otherwise unchanged.
        ///</param>
        /// <param name="LARGE">
        /// (input/output) DOUBLE PRECISION
        /// On entry, the overflow threshold as computed by DLAMCH.
        /// On exit, if LOG10(LARGE) is sufficiently large, the square
        /// root of LARGE, otherwise unchanged.
        ///</param>
        public void Run(ref double SMALL, ref double LARGE)
        {

            #region Prolog
            
            // *
            // *  -- LAPACK auxiliary routine (version 3.1) --
            // *     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
            // *     November 2006
            // *
            // *     .. Scalar Arguments ..
            // *     ..
            // *
            // *  Purpose
            // *  =======
            // *
            // *  DLABAD takes as input the values computed by DLAMCH for underflow and
            // *  overflow, and returns the square root of each of these values if the
            // *  log of LARGE is sufficiently large.  This subroutine is intended to
            // *  identify machines with a large exponent range, such as the Crays, and
            // *  redefine the underflow and overflow limits to be the square roots of
            // *  the values computed by DLAMCH.  This subroutine is needed because
            // *  DLAMCH does not compensate for poor arithmetic in the upper half of
            // *  the exponent range, as is found on a Cray.
            // *
            // *  Arguments
            // *  =========
            // *
            // *  SMALL   (input/output) DOUBLE PRECISION
            // *          On entry, the underflow threshold as computed by DLAMCH.
            // *          On exit, if LOG10(LARGE) is sufficiently large, the square
            // *          root of SMALL, otherwise unchanged.
            // *
            // *  LARGE   (input/output) DOUBLE PRECISION
            // *          On entry, the overflow threshold as computed by DLAMCH.
            // *          On exit, if LOG10(LARGE) is sufficiently large, the square
            // *          root of LARGE, otherwise unchanged.
            // *
            // *  =====================================================================
            // *
            // *     .. Intrinsic Functions ..
            //      INTRINSIC          LOG10, SQRT;
            // *     ..
            // *     .. Executable Statements ..
            // *
            // *     If it looks like we're on a Cray, take the square root of
            // *     SMALL and LARGE to avoid overflow and underflow problems.
            // *

            #endregion

            if (Math.Log10(LARGE) > 2000.0E0)
            {
                SMALL = Math.Sqrt(SMALL);
                LARGE = Math.Sqrt(LARGE);
            }
            // *
            return;
            // *
            // *     End of DLABAD
            // *
        }
    }
}
