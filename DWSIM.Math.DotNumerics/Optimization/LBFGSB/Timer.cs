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
    public class TIMER
    {
    

        #region Variables
        
        double[] TARRAY = new double[2]; 

        #endregion

        public TIMER()
        {
    
        }
    
        /// <param name="TTIME">
        /// is an output variable which specifies the user time.
        ///</param>
        public void Run(ref double TTIME)
        {

            #region Variables
            
            double TEMP = 0; int offset_tarray = 0; int o_tarray = -1; double ETIME = 0; 

            #endregion


            #region Prolog
            
            // c     *********
            // c
            // c     Subroutine timer
            // c
            // c     This subroutine is used to determine user time. In a typical 
            // c     application, the user time for a code segment requires calls 
            // c     to subroutine timer to determine the initial and final time.
            // c
            // c     The subroutine statement is
            // c
            // c       subroutine timer(ttime)
            // c
            // c     where
            // c
            // c       ttime is an output variable which specifies the user time.
            // c
            // c     Argonne National Laboratory and University of Minnesota.
            // c     MINPACK-2 Project.
            // c
            // c     Modified October 1990 by Brett M. Averick.
            // c
            // c     **********
            
            // c     The first element of the array tarray specifies user time
            
            // c      temp = etime(tarray) 

            #endregion

            TEMP = 1;
            
            TTIME = Convert.ToDouble(TARRAY[1 + o_tarray]);
            
            return;
            
        }
    }
    
    // c====================== The end of timer ===============================
}
