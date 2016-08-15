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
    public class DROT
    {
    
        public DROT()
        {
    
        }
    
        public void Run(int N, ref double[] DX, int offset_dx, int INCX, ref double[] DY, int offset_dy, int INCY, double C
                         , double S)
        {

            #region Variables
            
            double DTEMP = 0; int I = 0; int IX = 0; int IY = 0; 

            #endregion


            #region Array Index Correction
            
             int o_dx = -1 + offset_dx;  int o_dy = -1 + offset_dy; 

            #endregion

            // c
            // c     applies a plane rotation.
            // c     jack dongarra, linpack, 3/11/78.
            // c     modified 12/3/93, array(1) declarations changed to array(*)
            // c
            // c

            #region Body
            
            if (N <= 0) return;
            if (INCX == 1 && INCY == 1) goto LABEL20;
            // c
            // c       code for unequal increments or equal increments not equal
            // c         to 1
            // c
            IX = 1;
            IY = 1;
            if (INCX < 0) IX = ( - N + 1) * INCX + 1;
            if (INCY < 0) IY = ( - N + 1) * INCY + 1;
            for (I = 1; I <= N; I++)
            {
                DTEMP = C * DX[IX + o_dx] + S * DY[IY + o_dy];
                DY[IY + o_dy] = C * DY[IY + o_dy] - S * DX[IX + o_dx];
                DX[IX + o_dx] = DTEMP;
                IX += INCX;
                IY += INCY;
            }
            return;
            // c
            // c       code for both increments equal to 1
            // c
        LABEL20:  
            for (I = 1; I <= N; I++)
            {
                DTEMP = C * DX[I + o_dx] + S * DY[I + o_dy];
                DY[I + o_dy] = C * DY[I + o_dy] - S * DX[I + o_dx];
                DX[I + o_dx] = DTEMP;
            }
            return;

            #endregion

        }
    }
}
