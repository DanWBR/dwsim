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
    public class PRN1LB
    {
    
        public PRN1LB()
        {
    
        }
    
        public void Run(int N, int M, double[] L, int offset_l, double[] U, int offset_u, double[] X, int offset_x, int IPRINT
                         , int ITFILE, double EPSMCH)
        {

            #region Variables
            
            int I = 0; 

            #endregion


            #region Array Index Correction
            
             int o_l = -1 + offset_l;  int o_u = -1 + offset_u;  int o_x = -1 + offset_x; 

            #endregion


            #region Prolog
            
            
            
            // c     ************
            // c
            // c     Subroutine prn1lb
            // c
            // c     This subroutine prints the input data, initial point, upper and
            // c       lower bounds of each variable, machine precision, as well as 
            // c       the headings of the output.
            // c
            // c
            // c                           *  *  *
            // c
            // c     NEOS, November 1994. (Latest revision June 1996.)
            // c     Optimization Technology Center.
            // c     Argonne National Laboratory and Northwestern University.
            // c     Written by
            // c                        Ciyou Zhu
            // c     in collaboration with R.H. Byrd, P. Lu-Chen and J. Nocedal.
            // c
            // c
            // c     ************
            
            

            #endregion


            #region Body
            
            if (IPRINT >= 0)
            {
                //ERROR-ERROR         WRITE (6,7001) EPSMCH;
                //ERROR-ERROR         WRITE (6,*) 'N = ',N,'    M = ',M;
                if (IPRINT >= 1)
                {
                    //ERROR-ERROR            WRITE (ITFILE,2001) EPSMCH;
                    //ERROR-ERROR            WRITE (ITFILE,*)'N = ',N,'    M = ',M;
                    //ERROR-ERROR	    WRITE (ITFILE,9001);
                    if (IPRINT > 100)
                    {
                        //ERROR-ERROR               WRITE (6,1004) 'L =',(L(I),I = 1,N);
                        //ERROR-ERROR               WRITE (6,1004) 'X0 =',(X(I),I = 1,N);
                        //ERROR-ERROR               WRITE (6,1004) 'U =',(U(I),I = 1,N);
                    }
                }
            }
            
            
            return;
            

            #endregion

        }
    }
    
    // c======================= The end of prn1lb =============================
}
