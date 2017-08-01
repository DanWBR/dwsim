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

namespace DotNumerics.LinearAlgebra.CSEispack
{
    public class PYTHAG
    {
    
        public PYTHAG()
        {
    
        }
    
        public double Run(double A, double B)
        {
        double pythag = 0;

            #region Variables
            
            double P = 0; double R = 0; double S = 0; double T = 0; double U = 0; 

            #endregion

            // c
            // c     finds dsqrt(a**2+b**2) without overflow or destructive underflow
            // c
            P = Math.Max(Math.Abs(A), Math.Abs(B));
            if (P == 0.0E0) goto LABEL20;
            R = Math.Pow(Math.Min(Math.Abs(A), Math.Abs(B)) / P,2);
        LABEL10:;
            T = 4.0E0 + R;
            if (T == 4.0E0) goto LABEL20;
            S = R / T;
            U = 1.0E0 + 2.0E0 * S;
            P *= U;
            R *= Math.Pow(S / U,2);
            goto LABEL10;
        LABEL20:  pythag = P;
            return pythag;
        }
    }
}
