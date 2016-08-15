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

namespace DotNumerics.Optimization.LBFGSB
{
    public class DNRM2
    {
    
        public DNRM2()
        {
    
        }
    
        /// <param name="N">
        /// is a positive integer input variable.
        ///</param>
        /// <param name="X">
        /// is an input array of length n.
        ///</param>
        /// <param name="INCX">
        /// is a positive integer variable that specifies the 
        /// stride of the vector.
        ///</param>
        public double Run(int N, double[] X, int offset_x, int INCX)
        {
        double dnrm2 = 0;

            #region Variables
            
            int I = 0; double SCALE = 0; 

            #endregion


            #region Array Index Correction
            
             int o_x = -1 + offset_x; 

            #endregion


            #region Prolog
            
            // c     **********
            // c
            // c     Function dnrm2
            // c
            // c     Given a vector x of length n, this function calculates the
            // c     Euclidean norm of x with stride incx.
            // c
            // c     The function statement is
            // c
            // c       double precision function dnrm2(n,x,incx)
            // c
            // c     where
            // c
            // c       n is a positive integer input variable.
            // c
            // c       x is an input array of length n.
            // c
            // c       incx is a positive integer variable that specifies the 
            // c         stride of the vector.
            // c
            // c     Subprograms called
            // c
            // c       FORTRAN-supplied ... abs, max, sqrt
            // c
            // c     MINPACK-2 Project. February 1991.
            // c     Argonne National Laboratory.
            // c     Brett M. Averick.
            // c
            // c     **********
            
            //	INTRINSIC ABS, MAX, SQRT;
            
            

            #endregion


            #region Body
            
            dnrm2 = 0.0E0;
            SCALE = 0.0E0;
            
            for (I = 1; (INCX >= 0) ? (I <= N) : (I >= N); I += INCX)
            {
                SCALE = Math.Max(SCALE, Math.Abs(X[I + o_x]));
            }
            
            if (SCALE == 0.0E0) return dnrm2;
            
            for (I = 1; (INCX >= 0) ? (I <= N) : (I >= N); I += INCX)
            {
                dnrm2 += Math.Pow(X[I + o_x] / SCALE,2);
            }
            
            dnrm2 = SCALE * Math.Sqrt(dnrm2);
            
            
            return dnrm2;
            
        return dnrm2;

            #endregion

        }
    }
    
    // c====================== The end of dnrm2 ===============================
}
