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
    /// finds the index of element having max. absolute value.
    /// jack dongarra, linpack, 3/11/78.
    /// modified 3/93 to return if incx .le. 0.
    /// modified 12/3/93, array(1) declarations changed to array(*)
    ///</summary>
    public class IDAMAX
    {
    
        public IDAMAX()
        {
    
        }
    
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// finds the index of element having max. absolute value.
        /// jack dongarra, linpack, 3/11/78.
        /// modified 3/93 to return if incx .le. 0.
        /// modified 12/3/93, array(1) declarations changed to array(*)
        ///</summary>
        public int Run(int N, double[] DX, int offset_dx, int INCX)
        {
        int idamax = 0;

            #region Variables
            
            double DMAX = 0; int I = 0; int IX = 0; 

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
            // *     finds the index of element having max. absolute value.
            // *     jack dongarra, linpack, 3/11/78.
            // *     modified 3/93 to return if incx .le. 0.
            // *     modified 12/3/93, array(1) declarations changed to array(*)
            // *
            // *
            // *     .. Local Scalars ..
            // *     ..
            // *     .. Intrinsic Functions ..
            //      INTRINSIC DABS;
            // *     ..

            #endregion


            #region Body
            
            idamax = 0;
            if (N < 1 || INCX <= 0) return idamax;
            idamax = 1;
            if (N == 1) return idamax;
            if (INCX == 1) goto LABEL20;
            // *
            // *        code for increment not equal to 1
            // *
            IX = 1;
            DMAX = Math.Abs(DX[1 + o_dx]);
            IX += INCX;
            for (I = 2; I <= N; I++)
            {
                if (Math.Abs(DX[IX + o_dx]) <= DMAX) goto LABEL5;
                idamax = I;
                DMAX = Math.Abs(DX[IX + o_dx]);
            LABEL5:  IX += INCX;
            }
            return idamax;
            // *
            // *        code for increment equal to 1
            // *
        LABEL20:  DMAX = Math.Abs(DX[1 + o_dx]);
            for (I = 2; I <= N; I++)
            {
                if (Math.Abs(DX[I + o_dx]) <= DMAX) goto LABEL30;
                idamax = I;
                DMAX = Math.Abs(DX[I + o_dx]);
            LABEL30:;
            }
            return idamax;

            #endregion

        }
    }
}
