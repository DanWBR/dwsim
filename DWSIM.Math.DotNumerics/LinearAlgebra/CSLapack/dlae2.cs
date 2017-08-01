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
    /// DLAE2  computes the eigenvalues of a 2-by-2 symmetric matrix
    /// [  A   B  ]
    /// [  B   C  ].
    /// On return, RT1 is the eigenvalue of larger absolute value, and RT2
    /// is the eigenvalue of smaller absolute value.
    /// 
    ///</summary>
    public class DLAE2
    {
    

        #region Variables
        
        const double ONE = 1.0E0; const double TWO = 2.0E0; const double ZERO = 0.0E0; const double HALF = 0.5E0; 

        #endregion

        public DLAE2()
        {
    
        }
    
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DLAE2  computes the eigenvalues of a 2-by-2 symmetric matrix
        /// [  A   B  ]
        /// [  B   C  ].
        /// On return, RT1 is the eigenvalue of larger absolute value, and RT2
        /// is the eigenvalue of smaller absolute value.
        /// 
        ///</summary>
        /// <param name="A">
        /// (input) DOUBLE PRECISION
        /// The (1,1) element of the 2-by-2 matrix.
        ///</param>
        /// <param name="B">
        /// (input) DOUBLE PRECISION
        /// The (1,2) and (2,1) elements of the 2-by-2 matrix.
        ///</param>
        /// <param name="C">
        /// (input) DOUBLE PRECISION
        /// The (2,2) element of the 2-by-2 matrix.
        ///</param>
        /// <param name="RT1">
        /// (output) DOUBLE PRECISION
        /// The eigenvalue of larger absolute value.
        ///</param>
        /// <param name="RT2">
        /// (output) DOUBLE PRECISION
        /// The eigenvalue of smaller absolute value.
        ///</param>
        public void Run(double A, double B, double C, ref double RT1, ref double RT2)
        {

            #region Variables
            
            double AB = 0; double ACMN = 0; double ACMX = 0; double ADF = 0; double DF = 0; double RT = 0; double SM = 0; 
            double TB = 0;

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
            // *  DLAE2  computes the eigenvalues of a 2-by-2 symmetric matrix
            // *     [  A   B  ]
            // *     [  B   C  ].
            // *  On return, RT1 is the eigenvalue of larger absolute value, and RT2
            // *  is the eigenvalue of smaller absolute value.
            // *
            // *  Arguments
            // *  =========
            // *
            // *  A       (input) DOUBLE PRECISION
            // *          The (1,1) element of the 2-by-2 matrix.
            // *
            // *  B       (input) DOUBLE PRECISION
            // *          The (1,2) and (2,1) elements of the 2-by-2 matrix.
            // *
            // *  C       (input) DOUBLE PRECISION
            // *          The (2,2) element of the 2-by-2 matrix.
            // *
            // *  RT1     (output) DOUBLE PRECISION
            // *          The eigenvalue of larger absolute value.
            // *
            // *  RT2     (output) DOUBLE PRECISION
            // *          The eigenvalue of smaller absolute value.
            // *
            // *  Further Details
            // *  ===============
            // *
            // *  RT1 is accurate to a few ulps barring over/underflow.
            // *
            // *  RT2 may be inaccurate if there is massive cancellation in the
            // *  determinant A*C-B*B; higher precision or correctly rounded or
            // *  correctly truncated arithmetic would be needed to compute RT2
            // *  accurately in all cases.
            // *
            // *  Overflow is possible only if RT1 is within a factor of 5 of overflow.
            // *  Underflow is harmless if the input data is 0 or exceeds
            // *     underflow_threshold / macheps.
            // *
            // * =====================================================================
            // *
            // *     .. Parameters ..
            // *     ..
            // *     .. Local Scalars ..
            // *     ..
            // *     .. Intrinsic Functions ..
            //      INTRINSIC          ABS, SQRT;
            // *     ..
            // *     .. Executable Statements ..
            // *
            // *     Compute the eigenvalues
            // *

            #endregion


            #region Body
            
            SM = A + C;
            DF = A - C;
            ADF = Math.Abs(DF);
            TB = B + B;
            AB = Math.Abs(TB);
            if (Math.Abs(A) > Math.Abs(C))
            {
                ACMX = A;
                ACMN = C;
            }
            else
            {
                ACMX = C;
                ACMN = A;
            }
            if (ADF > AB)
            {
                RT = ADF * Math.Sqrt(ONE + Math.Pow(AB / ADF,2));
            }
            else
            {
                if (ADF < AB)
                {
                    RT = AB * Math.Sqrt(ONE + Math.Pow(ADF / AB,2));
                }
                else
                {
                    // *
                    // *        Includes case AB=ADF=0
                    // *
                    RT = AB * Math.Sqrt(TWO);
                }
            }
            if (SM < ZERO)
            {
                RT1 = HALF * (SM - RT);
                // *
                // *        Order of execution important.
                // *        To get fully accurate smaller eigenvalue,
                // *        next line needs to be executed in higher precision.
                // *
                RT2 = (ACMX / RT1) * ACMN - (B / RT1) * B;
            }
            else
            {
                if (SM > ZERO)
                {
                    RT1 = HALF * (SM + RT);
                    // *
                    // *        Order of execution important.
                    // *        To get fully accurate smaller eigenvalue,
                    // *        next line needs to be executed in higher precision.
                    // *
                    RT2 = (ACMX / RT1) * ACMN - (B / RT1) * B;
                }
                else
                {
                    // *
                    // *        Includes case RT1 = RT2 = 0
                    // *
                    RT1 = HALF * RT;
                    RT2 =  - HALF * RT;
                }
            }
            return;
            // *
            // *     End of DLAE2
            // *

            #endregion

        }
    }
}
