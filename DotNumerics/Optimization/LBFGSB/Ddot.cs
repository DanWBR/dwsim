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
    public class DDOT
    {
    
        public DDOT()
        {
    
        }
    
        public double Run(int N, double[] DX, int offset_dx, int INCX, double[] DY, int offset_dy, int INCY)
        {
        double ddot = 0;

            #region Variables
            
            double DTEMP = 0; int I = 0; int IX = 0; int IY = 0; int M = 0; int MP1 = 0; 

            #endregion


            #region Array Index Correction
            
             int o_dx = -1 + offset_dx;  int o_dy = -1 + offset_dy; 

            #endregion

            // c
            // c     forms the dot product of two vectors.
            // c     uses unrolled loops for increments equal to one.
            // c     jack dongarra, linpack, 3/11/78.
            // c
            // c

            #region Body
            
            ddot = 0.0E0;
            DTEMP = 0.0E0;
            if (N <= 0) return ddot;
            if (INCX == 1 && INCY == 1) goto LABEL20;
            // c
            // c        code for unequal increments or equal increments
            // c          not equal to 1
            // c
            IX = 1;
            IY = 1;
            if (INCX < 0) IX = ( - N + 1) * INCX + 1;
            if (INCY < 0) IY = ( - N + 1) * INCY + 1;
            for (I = 1; I <= N; I++)
            {
                DTEMP += DX[IX + o_dx] * DY[IY + o_dy];
                IX += INCX;
                IY += INCY;
            }
            ddot = DTEMP;
            return ddot;
            // c
            // c        code for both increments equal to 1
            // c
            // c
            // c        clean-up loop
            // c
        LABEL20:  M = FortranLib.Mod(N,5);
            if (M == 0) goto LABEL40;
            for (I = 1; I <= M; I++)
            {
                DTEMP += DX[I + o_dx] * DY[I + o_dy];
            }
            if (N < 5) goto LABEL60;
        LABEL40:  MP1 = M + 1;
            for (I = MP1; I <= N; I += 5)
            {
                DTEMP += DX[I + o_dx] * DY[I + o_dy] + DX[I + 1 + o_dx] * DY[I + 1 + o_dy] + DX[I + 2 + o_dx] * DY[I + 2 + o_dy] + DX[I + 3 + o_dx] * DY[I + 3 + o_dy] + DX[I + 4 + o_dx] * DY[I + 4 + o_dy];
            }
        LABEL60:  ddot = DTEMP;
            return ddot;

            #endregion

        }
    }
    
    // c====================== The end of ddot ================================
}
