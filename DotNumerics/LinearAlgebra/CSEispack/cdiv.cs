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
    public class CDIV
    {
    
        public CDIV()
        {
    
        }
    
        public void Run(double AR, double AI, double BR, double BI, ref double CR, ref double CI)
        {

            #region Variables
            
            double S = 0; double ARS = 0; double AIS = 0; double BRS = 0; double BIS = 0; 

            #endregion

            // c
            // c     complex division, (cr,ci) = (ar,ai)/(br,bi)
            // c
            S = Math.Abs(BR) + Math.Abs(BI);
            ARS = AR / S;
            AIS = AI / S;
            BRS = BR / S;
            BIS = BI / S;
            S = Math.Pow(BRS,2) + Math.Pow(BIS,2);
            CR = (ARS * BRS + AIS * BIS) / S;
            CI = (AIS * BRS - ARS * BIS) / S;
            return;
        }
    }
}
