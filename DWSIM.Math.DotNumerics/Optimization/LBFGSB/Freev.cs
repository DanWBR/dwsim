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
    public class FREEV
    {
    
        public FREEV()
        {
    
        }
    
        /// <param name="INDEX">
        /// is an integer array of dimension n
        /// for i=1,...,nfree, index(i) are the indices of free variables
        /// for i=nfree+1,...,n, index(i) are the indices of bound variables
        /// On entry after the first iteration, index gives 
        /// the free variables at the previous iteration.
        /// On exit it gives the free variables based on the determination
        /// in cauchy using the array iwhere.
        ///</param>
        /// <param name="INDX2">
        /// is an integer array of dimension n
        /// On entry indx2 is unspecified.
        /// On exit with iter.GT.0, indx2 indicates which variables
        /// have changed status since the previous iteration.
        /// For i= 1,...,nenter, indx2(i) have changed from bound to free.
        /// For i= ileave+1,...,n, indx2(i) have changed from free to bound.
        /// 
        /// 
        /// *  *  *
        ///</param>
        /// <param name="CNSTND">
        /// is a logical variable indicating whether bounds are present
        ///</param>
        /// <param name="ITER">
        /// .GT. 0, and finds the index set of free and active variables
        ///</param>
        public void Run(int N, ref int NFREE, ref int[] INDEX, int offset_index, ref int NENTER, ref int ILEAVE, ref int[] INDX2, int offset_indx2
                         , int[] IWHERE, int offset_iwhere, ref bool WRK, bool UPDATD, bool CNSTND, int IPRINT, int ITER)
        {

            #region Variables
            
            int IACT = 0; int I = 0; int K = 0; 

            #endregion


            #region Array Index Correction
            
             int o_index = -1 + offset_index;  int o_indx2 = -1 + offset_indx2;  int o_iwhere = -1 + offset_iwhere; 

            #endregion


            #region Prolog
            
            
            
            // c     ************
            // c
            // c     Subroutine freev 
            // c
            // c     This subroutine counts the entering and leaving variables when
            // c       iter > 0, and finds the index set of free and active variables
            // c       at the GCP.
            // c
            // c     cnstnd is a logical variable indicating whether bounds are present
            // c
            // c     index is an integer array of dimension n
            // c       for i=1,...,nfree, index(i) are the indices of free variables
            // c       for i=nfree+1,...,n, index(i) are the indices of bound variables
            // c       On entry after the first iteration, index gives 
            // c         the free variables at the previous iteration.
            // c       On exit it gives the free variables based on the determination
            // c         in cauchy using the array iwhere.
            // c
            // c     indx2 is an integer array of dimension n
            // c       On entry indx2 is unspecified.
            // c       On exit with iter>0, indx2 indicates which variables
            // c          have changed status since the previous iteration.
            // c       For i= 1,...,nenter, indx2(i) have changed from bound to free.
            // c       For i= ileave+1,...,n, indx2(i) have changed from free to bound.
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
            
            NENTER = 0;
            ILEAVE = N + 1;
            if (ITER > 0 && CNSTND)
            {
                // c                           count the entering and leaving variables.
                for (I = 1; I <= NFREE; I++)
                {
                    K = INDEX[I + o_index];
                    if (IWHERE[K + o_iwhere] > 0)
                    {
                        ILEAVE -= 1;
                        INDX2[ILEAVE + o_indx2] = K;
                        if (IPRINT >= 100) ;//ERROR-ERRORWRITE(6,*)'Variable ',K,' leaves the set of free variables'
                    }
                }
                for (I = 1 + NFREE; I <= N; I++)
                {
                    K = INDEX[I + o_index];
                    if (IWHERE[K + o_iwhere] <= 0)
                    {
                        NENTER += 1;
                        INDX2[NENTER + o_indx2] = K;
                        if (IPRINT >= 100) ;//ERROR-ERRORWRITE(6,*)'Variable ',K,' enters the set of free variables'
                    }
                }
                if (IPRINT >= 99) ;//ERROR-ERRORWRITE(6,*)N+1-ILEAVE,' variables leave; ',NENTER,' variables enter'
            }
            WRK = (ILEAVE < N + 1) || (NENTER > 0) || UPDATD;
            
            // c     Find the index set of free and active variables at the GCP.
            
            NFREE = 0;
            IACT = N + 1;
            for (I = 1; I <= N; I++)
            {
                if (IWHERE[I + o_iwhere] <= 0)
                {
                    NFREE += 1;
                    INDEX[NFREE + o_index] = I;
                }
                else
                {
                    IACT -= 1;
                    INDEX[IACT + o_index] = I;
                }
            }
            if (IPRINT >= 99) ;//ERROR-ERRORWRITE(6,*)NFREE,' variables are free at GCP ',ITER+1
            
            return;
            

            #endregion

        }
    }
    
    // c======================= The end of freev ==============================
}
