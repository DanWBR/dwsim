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
    #region Interface

    public interface IDNRM2
    {
        double Run(int N, double[] X, int offset_x, int INCX);
    }

    #endregion

    public class DNRM2 : IDNRM2
    {
    

        #region Variables
        
        const double ONE = 1.0E+0; const double ZERO = 0.0E+0; 

        #endregion

        public DNRM2()
        {
    
        }
    
        public double Run(int N, double[] X, int offset_x, int INCX)
        {
        double dnrm2 = 0;

            #region Variables
            
            int IX = 0; double ABSXI = 0; double NORM = 0; double SCALE = 0; double SSQ = 0; 

            #endregion


            #region Array Index Correction
            
             int o_x = -1 + offset_x; 

            #endregion


            #region Prolog
            
            // *     .. Scalar Arguments ..
            // *     .. Array Arguments ..
            // *     ..
            // *
            // *  DNRM2 returns the euclidean norm of a vector via the function
            // *  name, so that
            // *
            // *     DNRM2 := sqrt( x'*x )
            // *
            // *
            // *
            // *  -- This version written on 25-October-1982.
            // *     Modified on 14-October-1993 to inline the call to DLASSQ.
            // *     Sven Hammarling, Nag Ltd.
            // *
            // *
            // *     .. Parameters ..
            // *     .. Local Scalars ..
            // *     .. Intrinsic Functions ..
            //      INTRINSIC             ABS, SQRT;
            // *     ..
            // *     .. Executable Statements ..

            #endregion


            #region Body
            
            if (N < 1 || INCX < 1)
            {
                NORM = ZERO;
            }
            else
            {
                if (N == 1)
                {
                    NORM = Math.Abs(X[1 + o_x]);
                }
                else
                {
                    SCALE = ZERO;
                    SSQ = ONE;
                    // *        The following loop is equivalent to this call to the LAPACK
                    // *        auxiliary routine:
                    // *        CALL DLASSQ( N, X, INCX, SCALE, SSQ )
                    // *
                    for (IX = 1; (INCX >= 0) ? (IX <= 1 + (N - 1) * INCX) : (IX >= 1 + (N - 1) * INCX); IX += INCX)
                    {
                        if (X[IX + o_x] != ZERO)
                        {
                            ABSXI = Math.Abs(X[IX + o_x]);
                            if (SCALE < ABSXI)
                            {
                                SSQ = ONE + SSQ * Math.Pow(SCALE / ABSXI,2);
                                SCALE = ABSXI;
                            }
                            else
                            {
                                SSQ += Math.Pow(ABSXI / SCALE,2);
                            }
                        }
                    }
                    NORM = SCALE * Math.Sqrt(SSQ);
                }
            }
            // *
            dnrm2 = NORM;
            return dnrm2;
            // *
            // *     End of DNRM2.
            // *

            #endregion

        }
    }
}
