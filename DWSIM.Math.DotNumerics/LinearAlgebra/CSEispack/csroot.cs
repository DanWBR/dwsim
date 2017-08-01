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
    public class CSROOT
    {
    

        #region Dependencies
        
        PYTHAG _pythag; 

        #endregion

        public CSROOT(PYTHAG pythag)
        {
    

            #region Set Dependencies
            
            this._pythag = pythag; 

            #endregion

        }
    
        public CSROOT()
        {
    

            #region Dependencies (Initialization)
            
            PYTHAG pythag = new PYTHAG();

            #endregion


            #region Set Dependencies
            
            this._pythag = pythag; 

            #endregion

        }
        public void Run(double XR, double XI, ref double YR, ref double YI)
        {

            #region Variables
            
            double S = 0; double TR = 0; double TI = 0; 

            #endregion

            // c
            // c     (yr,yi) = complex dsqrt(xr,xi) 
            // c     branch chosen so that yr .ge. 0.0 and sign(yi) .eq. sign(xi)
            // c
            TR = XR;
            TI = XI;
            S = Math.Sqrt(0.5E0 * (this._pythag.Run(TR, TI) + Math.Abs(TR)));
            if (TR >= 0.0E0) YR = S;
            if (TI < 0.0E0) S =  - S;
            if (TR <= 0.0E0) YI = S;
            if (TR < 0.0E0) YR = 0.5E0 * (TI / YI);
            if (TR > 0.0E0) YI = 0.5E0 * (TI / YR);
            return;
        }
    }
}
