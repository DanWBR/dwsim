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
    public class ACTIVE
    {
    

        #region Variables
        
        const double ZERO = 0.0E0; 

        #endregion

        public ACTIVE()
        {
    
        }
    
        /// <param name="IWHERE">
        /// is an integer array of dimension n.
        /// On entry iwhere is unspecified.
        /// On exit iwhere(i)=-1  if x(i) has no bounds
        /// 3   if l(i)=u(i)
        /// 0   otherwise.
        /// In cauchy, iwhere is given finer gradations.
        /// 
        /// 
        /// *  *  *
        ///</param>
        public void Run(int N, double[] L, int offset_l, double[] U, int offset_u, int[] NBD, int offset_nbd, ref double[] X, int offset_x, ref int[] IWHERE, int offset_iwhere
                         , int IPRINT, ref bool PRJCTD, ref bool CNSTND, ref bool BOXED)
        {

            #region Variables
            
            int NBDD = 0; int I = 0; 

            #endregion


            #region Array Index Correction
            
             int o_l = -1 + offset_l;  int o_u = -1 + offset_u;  int o_nbd = -1 + offset_nbd;  int o_x = -1 + offset_x; 
             int o_iwhere = -1 + offset_iwhere;

            #endregion


            #region Prolog
            
            
            
            // c     ************
            // c
            // c     Subroutine active
            // c
            // c     This subroutine initializes iwhere and projects the initial x to
            // c       the feasible set if necessary.
            // c
            // c     iwhere is an integer array of dimension n.
            // c       On entry iwhere is unspecified.
            // c       On exit iwhere(i)=-1  if x(i) has no bounds
            // c                         3   if l(i)=u(i)
            // c                         0   otherwise.
            // c       In cauchy, iwhere is given finer gradations.
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
            
            
            // c     Initialize nbdd, prjctd, cnstnd and boxed.
            

            #endregion


            #region Body
            
            NBDD = 0;
            PRJCTD = false;
            CNSTND = false;
            BOXED = true;
            
            // c     Project the initial x to the easible set if necessary.
            
            for (I = 1; I <= N; I++)
            {
                if (NBD[I + o_nbd] > 0)
                {
                    if (NBD[I + o_nbd] <= 2 && X[I + o_x] <= L[I + o_l])
                    {
                        if (X[I + o_x] < L[I + o_l])
                        {
                            PRJCTD = true;
                            X[I + o_x] = L[I + o_l];
                        }
                        NBDD += 1;
                    }
                    else
                    {
                        if (NBD[I + o_nbd] >= 2 && X[I + o_x] >= U[I + o_u])
                        {
                            if (X[I + o_x] > U[I + o_u])
                            {
                                PRJCTD = true;
                                X[I + o_x] = U[I + o_u];
                            }
                            NBDD += 1;
                        }
                    }
                }
            }
            
            // c     Initialize iwhere and assign values to cnstnd and boxed.
            
            for (I = 1; I <= N; I++)
            {
                if (NBD[I + o_nbd] != 2) BOXED = false;
                if (NBD[I + o_nbd] == 0)
                {
                    // c                                this variable is always free
                    IWHERE[I + o_iwhere] =  - 1;
                    
                    // c           otherwise set x(i)=mid(x(i), u(i), l(i)).
                }
                else
                {
                    CNSTND = true;
                    if (NBD[I + o_nbd] == 2 && U[I + o_u] - L[I + o_l] <= ZERO)
                    {
                        // c                   this variable is always fixed
                        IWHERE[I + o_iwhere] = 3;
                    }
                    else
                    {
                        IWHERE[I + o_iwhere] = 0;
                    }
                }
            }
            
            if (IPRINT >= 0)
            {
                if (PRJCTD) ;//ERROR-ERRORWRITE(6,*)'The initial X is infeasible.  Restart with its projection.'
                if (!CNSTND) ;//ERROR-ERRORWRITE(6,*)'This problem is unconstrained.'
            }
            
            if (IPRINT > 0) ;//ERROR-ERRORWRITE(6,1001)NBDD
            
            
            return;
            

            #endregion

        }
    }
    
    // c======================= The end of active =============================
}
