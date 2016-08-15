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
    /// XERBLA  is an error handler for the LAPACK routines.
    /// It is called by an LAPACK routine if an input parameter has an
    /// invalid value.  A message is printed and execution stops.
    /// 
    /// Installers may consider modifying the STOP statement in order to
    /// call system-specific exception-handling facilities.
    /// 
    ///</summary>
    public class XERBLA
    {
    
        public XERBLA()
        {
    
        }
    
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// XERBLA  is an error handler for the LAPACK routines.
        /// It is called by an LAPACK routine if an input parameter has an
        /// invalid value.  A message is printed and execution stops.
        /// 
        /// Installers may consider modifying the STOP statement in order to
        /// call system-specific exception-handling facilities.
        /// 
        ///</summary>
        /// <param name="SRNAME">
        /// (input) CHARACTER*6
        /// The name of the routine which called XERBLA.
        ///</param>
        /// <param name="INFO">
        /// (input) INTEGER
        /// The position of the invalid parameter in the parameter list
        /// of the calling routine.
        ///</param>
        public void Run(string SRNAME, int INFO)
        {

            #region Strings
            
            SRNAME = SRNAME.Substring(0, 6);  

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
            // *  XERBLA  is an error handler for the LAPACK routines.
            // *  It is called by an LAPACK routine if an input parameter has an
            // *  invalid value.  A message is printed and execution stops.
            // *
            // *  Installers may consider modifying the STOP statement in order to
            // *  call system-specific exception-handling facilities.
            // *
            // *  Arguments
            // *  =========
            // *
            // *  SRNAME  (input) CHARACTER*6
            // *          The name of the routine which called XERBLA.
            // *
            // *  INFO    (input) INTEGER
            // *          The position of the invalid parameter in the parameter list
            // *          of the calling routine.
            // *
            // * =====================================================================
            // *
            // *     .. Executable Statements ..
            // *

            #endregion

            //ERROR-ERROR      WRITE( *, FMT = 9999 )SRNAME, INFO;
            // *
            return;
            // *
            // *
            // *     End of XERBLA
            // *
        }
    }
}
