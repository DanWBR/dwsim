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
    /// Purpose
    /// =======
    /// 
    /// takes the sum of the absolute values.
    /// jack dongarra, linpack, 3/11/78.
    /// modified 3/93 to return if incx .le. 0.
    /// modified 12/3/93, array(1) declarations changed to array(*)
    ///</summary>
    public class DASUM
    {
    
        public DASUM()
        {
    
        }
    
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// takes the sum of the absolute values.
        /// jack dongarra, linpack, 3/11/78.
        /// modified 3/93 to return if incx .le. 0.
        /// modified 12/3/93, array(1) declarations changed to array(*)
        ///</summary>
        public double Run(int N, double[] DX, int offset_dx, int INCX)
        {
        double dasum = 0;

            #region Variables
            
            double DTEMP = 0; int I = 0; int M = 0; int MP1 = 0; int NINCX = 0; 

            #endregion


            #region Array Index Correction
            
             int o_dx = -1 + offset_dx; 

            #endregion


            #region Prolog
            
            // *     .. Scalar Arguments ..
            // *     ..
            // *     .. Array Arguments ..
            // *     ..
            // *
            // *  Purpose
            // *  =======
            // *
            // *     takes the sum of the absolute values.
            // *     jack dongarra, linpack, 3/11/78.
            // *     modified 3/93 to return if incx .le. 0.
            // *     modified 12/3/93, array(1) declarations changed to array(*)
            // *
            // *
            // *     .. Local Scalars ..
            // *     ..
            // *     .. Intrinsic Functions ..
            //      INTRINSIC DABS,MOD;
            // *     ..

            #endregion


            #region Body
            
            dasum = 0.0E0;
            DTEMP = 0.0E0;
            if (N <= 0 || INCX <= 0) return dasum;
            if (INCX == 1) goto LABEL20;
            // *
            // *        code for increment not equal to 1
            // *
            NINCX = N * INCX;
            for (I = 1; (INCX >= 0) ? (I <= NINCX) : (I >= NINCX); I += INCX)
            {
                DTEMP += Math.Abs(DX[I + o_dx]);
            }
            dasum = DTEMP;
            return dasum;
            // *
            // *        code for increment equal to 1
            // *
            // *
            // *        clean-up loop
            // *
        LABEL20:  M = FortranLib.Mod(N,6);
            if (M == 0) goto LABEL40;
            for (I = 1; I <= M; I++)
            {
                DTEMP += Math.Abs(DX[I + o_dx]);
            }
            if (N < 6) goto LABEL60;
        LABEL40:  MP1 = M + 1;
            for (I = MP1; I <= N; I += 6)
            {
                DTEMP += Math.Abs(DX[I + o_dx]) + Math.Abs(DX[I + 1 + o_dx]) + Math.Abs(DX[I + 2 + o_dx]) + Math.Abs(DX[I + 3 + o_dx]) + Math.Abs(DX[I + 4 + o_dx]) + Math.Abs(DX[I + 5 + o_dx]);
            }
        LABEL60:  dasum = DTEMP;
            return dasum;

            #endregion

        }
    }
}
