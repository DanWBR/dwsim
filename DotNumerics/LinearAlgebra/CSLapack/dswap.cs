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
    /// interchanges two vectors.
    /// uses unrolled loops for increments equal one.
    /// jack dongarra, linpack, 3/11/78.
    /// modified 12/3/93, array(1) declarations changed to array(*)
    ///</summary>
    public class DSWAP
    {
    
        public DSWAP()
        {
    
        }
    
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// interchanges two vectors.
        /// uses unrolled loops for increments equal one.
        /// jack dongarra, linpack, 3/11/78.
        /// modified 12/3/93, array(1) declarations changed to array(*)
        ///</summary>
        public void Run(int N, ref double[] DX, int offset_dx, int INCX, ref double[] DY, int offset_dy, int INCY)
        {

            #region Variables
            
            double DTEMP = 0; int I = 0; int IX = 0; int IY = 0; int M = 0; int MP1 = 0; 

            #endregion


            #region Array Index Correction
            
             int o_dx = -1 + offset_dx;  int o_dy = -1 + offset_dy; 

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
            // *     interchanges two vectors.
            // *     uses unrolled loops for increments equal one.
            // *     jack dongarra, linpack, 3/11/78.
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
            
            if (N <= 0) return;
            if (INCX == 1 && INCY == 1) goto LABEL20;
            // *
            // *       code for unequal increments or equal increments not equal
            // *         to 1
            // *
            IX = 1;
            IY = 1;
            if (INCX < 0) IX = ( - N + 1) * INCX + 1;
            if (INCY < 0) IY = ( - N + 1) * INCY + 1;
            for (I = 1; I <= N; I++)
            {
                DTEMP = DX[IX + o_dx];
                DX[IX + o_dx] = DY[IY + o_dy];
                DY[IY + o_dy] = DTEMP;
                IX += INCX;
                IY += INCY;
            }
            return;
            // *
            // *       code for both increments equal to 1
            // *
            // *
            // *       clean-up loop
            // *
        LABEL20:  M = FortranLib.Mod(N,3);
            if (M == 0) goto LABEL40;
            for (I = 1; I <= M; I++)
            {
                DTEMP = DX[I + o_dx];
                DX[I + o_dx] = DY[I + o_dy];
                DY[I + o_dy] = DTEMP;
            }
            if (N < 3) return;
        LABEL40:  MP1 = M + 1;
            for (I = MP1; I <= N; I += 3)
            {
                DTEMP = DX[I + o_dx];
                DX[I + o_dx] = DY[I + o_dy];
                DY[I + o_dy] = DTEMP;
                DTEMP = DX[I + 1 + o_dx];
                DX[I + 1 + o_dx] = DY[I + 1 + o_dy];
                DY[I + 1 + o_dy] = DTEMP;
                DTEMP = DX[I + 2 + o_dx];
                DX[I + 2 + o_dx] = DY[I + 2 + o_dy];
                DY[I + 2 + o_dy] = DTEMP;
            }
            return;

            #endregion

        }
    }
}
