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
    /// *
    /// scales a vector by a constant.
    /// uses unrolled loops for increment equal to one.
    /// jack dongarra, linpack, 3/11/78.
    /// modified 3/93 to return if incx .le. 0.
    /// modified 12/3/93, array(1) declarations changed to array(*)
    ///</summary>
    public class DSCAL
    {
    
        public DSCAL()
        {
    
        }
    
        /// <summary>
        /// Purpose
        /// =======
        /// *
        /// scales a vector by a constant.
        /// uses unrolled loops for increment equal to one.
        /// jack dongarra, linpack, 3/11/78.
        /// modified 3/93 to return if incx .le. 0.
        /// modified 12/3/93, array(1) declarations changed to array(*)
        ///</summary>
        public void Run(int N, double DA, ref double[] DX, int offset_dx, int INCX)
        {

            #region Variables
            
            int I = 0; int M = 0; int MP1 = 0; int NINCX = 0; 

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
            // **
            // *     scales a vector by a constant.
            // *     uses unrolled loops for increment equal to one.
            // *     jack dongarra, linpack, 3/11/78.
            // *     modified 3/93 to return if incx .le. 0.
            // *     modified 12/3/93, array(1) declarations changed to array(*)
            // *
            // *
            // *     .. Local Scalars ..
            // *     ..
            // *     .. Intrinsic Functions ..
            //      INTRINSIC MOD;
            // *     ..

            #endregion


            #region Body
            
            if (N <= 0 || INCX <= 0) return;
            if (INCX == 1) goto LABEL20;
            // *
            // *        code for increment not equal to 1
            // *
            NINCX = N * INCX;
            for (I = 1; (INCX >= 0) ? (I <= NINCX) : (I >= NINCX); I += INCX)
            {
                DX[I + o_dx] *= DA;
            }
            return;
            // *
            // *        code for increment equal to 1
            // *
            // *
            // *        clean-up loop
            // *
        LABEL20:  M = FortranLib.Mod(N,5);
            if (M == 0) goto LABEL40;
            for (I = 1; I <= M; I++)
            {
                DX[I + o_dx] *= DA;
            }
            if (N < 5) return;
        LABEL40:  MP1 = M + 1;
            for (I = MP1; I <= N; I += 5)
            {
                DX[I + o_dx] *= DA;
                DX[I + 1 + o_dx] *= DA;
                DX[I + 2 + o_dx] *= DA;
                DX[I + 3 + o_dx] *= DA;
                DX[I + 4 + o_dx] *= DA;
            }
            return;

            #endregion

        }
    }
}
