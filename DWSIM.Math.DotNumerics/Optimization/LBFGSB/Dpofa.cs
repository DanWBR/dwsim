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
    public class DPOFA
    {
    

        #region Dependencies
        
        DDOT _ddot; 

        #endregion

        public DPOFA(DDOT ddot)
        {
    

            #region Set Dependencies
            
            this._ddot = ddot; 

            #endregion

        }
    
        public DPOFA()
        {
    

            #region Dependencies (Initialization)
            
            DDOT ddot = new DDOT();

            #endregion


            #region Set Dependencies
            
            this._ddot = ddot; 

            #endregion

        }
        /// <param name="A">
        /// double precision(lda, n)
        /// the symmetric matrix to be factored.  only the
        /// diagonal and upper triangle are used.
        ///</param>
        /// <param name="LDA">
        /// integer
        /// the leading dimension of the array  a .
        ///</param>
        /// <param name="N">
        /// integer
        /// the order of the matrix  a .
        ///</param>
        /// <param name="INFO">
        /// integer
        /// = 0  for normal return.
        /// = k  signals an error condition.  the leading minor
        /// of order  k  is not positive definite.
        ///</param>
        public void Run(ref double[] A, int offset_a, int LDA, int N, ref int INFO)
        {

            #region Variables
            
            double T = 0; double S = 0; int J = 0; int JM1 = 0; int K = 0; 

            #endregion


            #region Implicit Variables
            
            int A_J = 0; 

            #endregion


            #region Array Index Correction
            
             int o_a = -1 - LDA + offset_a; 

            #endregion


            #region Prolog
            
            // c
            // c     dpofa factors a double precision symmetric positive definite
            // c     matrix.
            // c
            // c     dpofa is usually called by dpoco, but it can be called
            // c     directly with a saving in time if  rcond  is not needed.
            // c     (time for dpoco) = (1 + 18/n)*(time for dpofa) .
            // c
            // c     on entry
            // c
            // c        a       double precision(lda, n)
            // c                the symmetric matrix to be factored.  only the
            // c                diagonal and upper triangle are used.
            // c
            // c        lda     integer
            // c                the leading dimension of the array  a .
            // c
            // c        n       integer
            // c                the order of the matrix  a .
            // c
            // c     on return
            // c
            // c        a       an upper triangular matrix  r  so that  a = trans(r)*r
            // c                where  trans(r)  is the transpose.
            // c                the strict lower triangle is unaltered.
            // c                if  info .ne. 0 , the factorization is not complete.
            // c
            // c        info    integer
            // c                = 0  for normal return.
            // c                = k  signals an error condition.  the leading minor
            // c                     of order  k  is not positive definite.
            // c
            // c     linpack.  this version dated 08/14/78 .
            // c     cleve moler, university of new mexico, argonne national lab.
            // c
            // c     subroutines and functions
            // c
            // c     blas ddot
            // c     fortran sqrt
            // c
            // c     internal variables
            // c
            //	INTRINSIC SQRT;
            
            // c     begin block with ...exits to 40
            // c
            // c

            #endregion


            #region Body
            
            for (J = 1; J <= N; J++)
            {
                INFO = J;
                S = 0.0E0;
                JM1 = J - 1;
                if (JM1 < 1) goto LABEL20;
                A_J = J * LDA + o_a;
                for (K = 1; K <= JM1; K++)
                {
                    T = A[K + A_J] - this._ddot.Run(K - 1, A, 1+K * LDA + o_a, 1, A, 1+J * LDA + o_a, 1);
                    T /= A[K+K * LDA + o_a];
                    A[K + A_J] = T;
                    S += T * T;
                }
            LABEL20:;
                S = A[J+J * LDA + o_a] - S;
                // c     ......exit
                if (S <= 0.0E0) goto LABEL40;
                A[J+J * LDA + o_a] = Math.Sqrt(S);
            }
            INFO = 0;
        LABEL40:;
            return;

            #endregion

        }
    }
    
    // c====================== The end of dpofa ===============================
}
