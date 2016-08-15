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
    public class HPSOLB
    {
    
        public HPSOLB()
        {
    
        }
    
        /// <param name="N">
        /// is an integer variable.
        /// On entry n is the dimension of the arrays t and iorder.
        /// On exit n is unchanged.
        ///</param>
        /// <param name="T">
        /// is a double precision array of dimension n.
        /// On entry t stores the elements to be sorted,
        /// On exit t(n) stores the least elements of t, and t(1) to t(n-1)
        /// stores the remaining elements in the form of a heap.
        ///</param>
        /// <param name="IORDER">
        /// is an integer array of dimension n.
        /// On entry iorder(i) is the index of t(i).
        /// On exit iorder(i) is still the index of t(i), but iorder may be
        /// permuted in accordance with t.
        ///</param>
        /// <param name="IHEAP">
        /// is an integer variable specifying the task.
        /// On entry iheap should be set as follows:
        /// iheap .eq. 0 if t(1) to t(n) is not in the form of a heap,
        /// iheap .ne. 0 if otherwise.
        /// On exit iheap is unchanged.
        /// 
        ///</param>
        public void Run(int N, ref double[] T, int offset_t, ref int[] IORDER, int offset_iorder, int IHEAP)
        {

            #region Variables
            
            int I = 0; int J = 0; int K = 0; int INDXIN = 0; int INDXOU = 0; double DDUM = 0; double OUT = 0; 

            #endregion


            #region Array Index Correction
            
             int o_t = -1 + offset_t;  int o_iorder = -1 + offset_iorder; 

            #endregion


            #region Prolog
            
            
            // c     ************
            // c
            // c     Subroutine hpsolb 
            // c
            // c     This subroutine sorts out the least element of t, and puts the
            // c       remaining elements of t in a heap.
            // c 
            // c     n is an integer variable.
            // c       On entry n is the dimension of the arrays t and iorder.
            // c       On exit n is unchanged.
            // c
            // c     t is a double precision array of dimension n.
            // c       On entry t stores the elements to be sorted,
            // c       On exit t(n) stores the least elements of t, and t(1) to t(n-1)
            // c         stores the remaining elements in the form of a heap.
            // c
            // c     iorder is an integer array of dimension n.
            // c       On entry iorder(i) is the index of t(i).
            // c       On exit iorder(i) is still the index of t(i), but iorder may be
            // c         permuted in accordance with t.
            // c
            // c     iheap is an integer variable specifying the task.
            // c       On entry iheap should be set as follows:
            // c         iheap .eq. 0 if t(1) to t(n) is not in the form of a heap,
            // c         iheap .ne. 0 if otherwise.
            // c       On exit iheap is unchanged.
            // c
            // c
            // c     References:
            // c       Algorithm 232 of CACM (J. W. J. Williams): HEAPSORT.
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
            // c     ************
            
            

            #endregion


            #region Body
            
            if (IHEAP == 0)
            {
                
                // c        Rearrange the elements t(1) to t(n) to form a heap.
                
                for (K = 2; K <= N; K++)
                {
                    DDUM = T[K + o_t];
                    INDXIN = IORDER[K + o_iorder];
                    
                    // c           Add ddum to the heap.
                    I = K;
                LABEL10:;
                    if (I > 1)
                    {
                        J = I / 2;
                        if (DDUM < T[J + o_t])
                        {
                            T[I + o_t] = T[J + o_t];
                            IORDER[I + o_iorder] = IORDER[J + o_iorder];
                            I = J;
                            goto LABEL10;
                        }
                    }
                    T[I + o_t] = DDUM;
                    IORDER[I + o_iorder] = INDXIN;
                }
            }
            
            // c     Assign to 'out' the value of t(1), the least member of the heap,
            // c        and rearrange the remaining members to form a heap as
            // c        elements 1 to n-1 of t.
            
            if (N > 1)
            {
                I = 1;
                OUT = T[1 + o_t];
                INDXOU = IORDER[1 + o_iorder];
                DDUM = T[N + o_t];
                INDXIN = IORDER[N + o_iorder];
                
                // c        Restore the heap 
            LABEL30:;
                J = I + I;
                if (J <= N - 1)
                {
                    if (T[J + 1 + o_t] < T[J + o_t]) J += 1;
                    if (T[J + o_t] < DDUM)
                    {
                        T[I + o_t] = T[J + o_t];
                        IORDER[I + o_iorder] = IORDER[J + o_iorder];
                        I = J;
                        goto LABEL30;
                    }
                }
                T[I + o_t] = DDUM;
                IORDER[I + o_iorder] = INDXIN;
                
                // c     Put the least member in t(n). 
                
                T[N + o_t] = OUT;
                IORDER[N + o_iorder] = INDXOU;
            }
            
            return;
            

            #endregion

        }
    }
    
    // c====================== The end of hpsolb ==============================
}
