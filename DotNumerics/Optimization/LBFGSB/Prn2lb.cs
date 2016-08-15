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
    public class PRN2LB
    {
    
        public PRN2LB()
        {
    
        }
    
        public void Run(int N, double[] X, int offset_x, double F, double[] G, int offset_g, int IPRINT, int ITFILE
                         , int ITER, int NFGV, int NACT, double SBGNRM, int NINT, ref BFGSWord WORD
                         , int IWORD, int IBACK, double STP, double XSTEP)
        {

            #region Variables
            
            int I = 0; int IMOD = 0; 

            #endregion


            #region Array Index Correction
            
             int o_x = -1 + offset_x;  int o_g = -1 + offset_g; 

            #endregion


            #region Prolog
            
            
            
            // c     ************
            // c
            // c     Subroutine prn2lb
            // c
            // c     This subroutine prints out new information after a successful
            // c       line search. 
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
            
            
            // c           'word' records the status of subspace solutions.

            #endregion


            #region Body

             if (IWORD == 0)
             {
                 // c                            the subspace minimization converged.
                 WORD = BFGSWord.con;
             }
             else
             {
                 if (IWORD == 1)
                 {
                     // c                          the subspace minimization stopped at a bound.
                     WORD = BFGSWord.bnd;
                 }
                 else
                 {
                     if (IWORD == 5)
                     {
                         // c                             the truncated Newton step has been used.
                         WORD = BFGSWord.tnt;
                     }
                     else
                     {
                         WORD = BFGSWord.aaa;
                     }
                 }
             }
            if (IPRINT >= 99)
            {
                //ERROR-ERROR         WRITE (6,*) 'LINE SEARCH',IBACK,' times; norm of step = ',XSTEP;
                //ERROR-ERROR         WRITE (6,2001) ITER,F,SBGNRM;
                if (IPRINT > 100)
                {
                    //ERROR-ERROR            WRITE (6,1004) 'X =',(X(I), I = 1, N);
                    //ERROR-ERROR            WRITE (6,1004) 'G =',(G(I), I = 1, N);
                }
            }
            else
            {
                if (IPRINT > 0)
                {
                    IMOD = FortranLib.Mod(ITER,IPRINT);
                    if (IMOD == 0) ;//ERROR-ERRORWRITE(6,2001)ITER,F,SBGNRM
                }
            }
            if (IPRINT >= 1) ;//ERROR-ERRORWRITE(ITFILE,3001)ITER,NFGV,NINT,NACT,WORD,IBACK,STP,XSTEP,SBGNRM,F
            
            
            return;
            

            #endregion

        }
    }
    
    // c======================= The end of prn2lb =============================
}
