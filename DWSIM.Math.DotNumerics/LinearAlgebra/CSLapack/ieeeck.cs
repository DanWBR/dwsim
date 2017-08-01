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
    /// IEEECK is called from the ILAENV to verify that Infinity and
    /// possibly NaN arithmetic is safe (i.e. will not trap).
    /// 
    ///</summary>
    public class IEEECK
    {
    
        public IEEECK()
        {
    
        }
    
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// IEEECK is called from the ILAENV to verify that Infinity and
        /// possibly NaN arithmetic is safe (i.e. will not trap).
        /// 
        ///</summary>
        /// <param name="ISPEC">
        /// (input) INTEGER
        /// Specifies whether to test just for inifinity arithmetic
        /// or whether to test for infinity and NaN arithmetic.
        /// = 0: Verify infinity arithmetic only.
        /// = 1: Verify infinity and NaN arithmetic.
        ///</param>
        /// <param name="ZERO">
        /// (input) REAL
        /// Must contain the value 0.0
        /// This is passed to prevent the compiler from optimizing
        /// away this code.
        ///</param>
        /// <param name="ONE">
        /// (input) REAL
        /// Must contain the value 1.0
        /// This is passed to prevent the compiler from optimizing
        /// away this code.
        ///</param>
        public int Run(int ISPEC, double ZERO, double ONE)
        {
        int ieeeck = 0;

            #region Variables
            
            double NAN1 = 0; double NAN2 = 0; double NAN3 = 0; double NAN4 = 0; double NAN5 = 0; double NAN6 = 0; 
            double NEGINF = 0;double NEGZRO = 0; double NEWZRO = 0; double POSINF = 0; 

            #endregion


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
            // *  IEEECK is called from the ILAENV to verify that Infinity and
            // *  possibly NaN arithmetic is safe (i.e. will not trap).
            // *
            // *  Arguments
            // *  =========
            // *
            // *  ISPEC   (input) INTEGER
            // *          Specifies whether to test just for inifinity arithmetic
            // *          or whether to test for infinity and NaN arithmetic.
            // *          = 0: Verify infinity arithmetic only.
            // *          = 1: Verify infinity and NaN arithmetic.
            // *
            // *  ZERO    (input) REAL
            // *          Must contain the value 0.0
            // *          This is passed to prevent the compiler from optimizing
            // *          away this code.
            // *
            // *  ONE     (input) REAL
            // *          Must contain the value 1.0
            // *          This is passed to prevent the compiler from optimizing
            // *          away this code.
            // *
            // *  RETURN VALUE:  INTEGER
            // *          = 0:  Arithmetic failed to produce the correct answers
            // *          = 1:  Arithmetic produced the correct answers
            // *
            // *     .. Local Scalars ..
            // *     ..
            // *     .. Executable Statements ..

            #endregion


            #region Body
            
            ieeeck = 1;
            // *
            POSINF = ONE / ZERO;
            if (POSINF <= ONE)
            {
                ieeeck = 0;
                return ieeeck;
            }
            // *
            NEGINF =  - ONE / ZERO;
            if (NEGINF >= ZERO)
            {
                ieeeck = 0;
                return ieeeck;
            }
            // *
            NEGZRO = ONE / (NEGINF + ONE);
            if (NEGZRO != ZERO)
            {
                ieeeck = 0;
                return ieeeck;
            }
            // *
            NEGINF = ONE / NEGZRO;
            if (NEGINF >= ZERO)
            {
                ieeeck = 0;
                return ieeeck;
            }
            // *
            NEWZRO = NEGZRO + ZERO;
            if (NEWZRO != ZERO)
            {
                ieeeck = 0;
                return ieeeck;
            }
            // *
            POSINF = ONE / NEWZRO;
            if (POSINF <= ONE)
            {
                ieeeck = 0;
                return ieeeck;
            }
            // *
            NEGINF *= POSINF;
            if (NEGINF >= ZERO)
            {
                ieeeck = 0;
                return ieeeck;
            }
            // *
            POSINF *= POSINF;
            if (POSINF <= ONE)
            {
                ieeeck = 0;
                return ieeeck;
            }
            // *
            // *
            // *
            // *
            // *     Return if we were only asked to check infinity arithmetic
            // *
            if (ISPEC == 0) return ieeeck;
            // *
            NAN1 = POSINF + NEGINF;
            // *
            NAN2 = POSINF / NEGINF;
            // *
            NAN3 = POSINF / POSINF;
            // *
            NAN4 = POSINF * ZERO;
            // *
            NAN5 = NEGINF * NEGZRO;
            // *
            NAN6 = NAN5 * 0.0;
            // *
            if (NAN1 == NAN1)
            {
                ieeeck = 0;
                return ieeeck;
            }
            // *
            if (NAN2 == NAN2)
            {
                ieeeck = 0;
                return ieeeck;
            }
            // *
            if (NAN3 == NAN3)
            {
                ieeeck = 0;
                return ieeeck;
            }
            // *
            if (NAN4 == NAN4)
            {
                ieeeck = 0;
                return ieeeck;
            }
            // *
            if (NAN5 == NAN5)
            {
                ieeeck = 0;
                return ieeeck;
            }
            // *
            if (NAN6 == NAN6)
            {
                ieeeck = 0;
                return ieeeck;
            }
            // *
            return ieeeck;

            #endregion

        }
    }
}
