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
    public class PRN3LB
    {
    
        public PRN3LB()
        {
    
        }

        public void Run(int N, double[] X, int offset_x, double F, BFGSTask TASK, int IPRINT, int INFO
                         , int ITFILE, int ITER, int NFGV, int NINTOL, int NSKIP, int NACT
                         , double SBGNRM, double TIME, int NINT, BFGSWord WORD, int IBACK, double STP
                         , double XSTEP, int K, double CACHYT, double SBTIME, double LNSCHT)
        {

            #region Variables
            
            int I = 0; 

            #endregion


            #region Array Index Correction
            
             int o_x = -1 + offset_x; 

            #endregion


            #region Prolog
            
            
            
            // c     ************
            // c
            // c     Subroutine prn3lb
            // c
            // c     This subroutine prints out information when either a built-in
            // c       convergence test is satisfied or when an error message is
            // c       generated.
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

            if (TASK == BFGSTask.ERROR) goto LABEL999;
            
            if (IPRINT >= 0)
            {
                //ERROR-ERROR         WRITE (6,3003);
                //ERROR-ERROR         WRITE (6,3004);
                //ERROR-ERROR         WRITE(6,3005) N,ITER,NFGV,NINTOL,NSKIP,NACT,SBGNRM,F;
                if (IPRINT >= 100)
                {
                    //ERROR-ERROR            WRITE (6,1004) 'X =',(X(I),I = 1,N);
                }
                if (IPRINT >= 1) ;//ERROR-ERRORWRITE(6,*)' F =',F
            }
        LABEL999:;
            if (IPRINT >= 0)
            {
                //ERROR-ERROR         WRITE (6,3009) TASK;
                if (INFO != 0)
                {
                    if (INFO ==  - 1) ;//ERROR-ERRORWRITE(6,9011)
                    if (INFO ==  - 2) ;//ERROR-ERRORWRITE(6,9012)
                    if (INFO ==  - 3) ;//ERROR-ERRORWRITE(6,9013)
                    if (INFO ==  - 4) ;//ERROR-ERRORWRITE(6,9014)
                    if (INFO ==  - 5) ;//ERROR-ERRORWRITE(6,9015)
                    if (INFO ==  - 6) ;//ERROR-ERRORWRITE(6,*)' Input nbd(',K,') is invalid.'
                    if (INFO ==  - 7) ;//ERROR-ERRORWRITE(6,*)' l(',K,') > u(',K,').  No feasible solution.'
                    if (INFO ==  - 8) ;//ERROR-ERRORWRITE(6,9018)
                    if (INFO ==  - 9) ;//ERROR-ERRORWRITE(6,9019)
                }
                if (IPRINT >= 1) ;//ERROR-ERRORWRITE(6,3007)CACHYT,SBTIME,LNSCHT
                //ERROR-ERROR         WRITE (6,3008) TIME;
                if (IPRINT >= 1)
                {
                    if (INFO ==  - 4 || INFO ==  - 9)
                    {
                        //ERROR-ERROR               WRITE (ITFILE,3002)ITER,NFGV,NINT,NACT,WORD,IBACK,STP,XSTEP;
                    }
                    //ERROR-ERROR            WRITE (ITFILE,3009) TASK;
                    if (INFO != 0)
                    {
                        if (INFO ==  - 1) ;//ERROR-ERRORWRITE(ITFILE,9011)
                        if (INFO ==  - 2) ;//ERROR-ERRORWRITE(ITFILE,9012)
                        if (INFO ==  - 3) ;//ERROR-ERRORWRITE(ITFILE,9013)
                        if (INFO ==  - 4) ;//ERROR-ERRORWRITE(ITFILE,9014)
                        if (INFO ==  - 5) ;//ERROR-ERRORWRITE(ITFILE,9015)
                        if (INFO ==  - 8) ;//ERROR-ERRORWRITE(ITFILE,9018)
                        if (INFO ==  - 9) ;//ERROR-ERRORWRITE(ITFILE,9019)
                    }
                    //ERROR-ERROR            WRITE (ITFILE,3008) TIME;
                }
            }
            
            
            return;
            

            #endregion

        }
    }
    
    // c======================= The end of prn3lb =============================
}
