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
    public class DTRSL
    {
    

        #region Dependencies
        
        DDOT _ddot; DAXPY _daxpy; 

        #endregion

        public DTRSL(DDOT ddot, DAXPY daxpy)
        {
    

            #region Set Dependencies
            
            this._ddot = ddot; this._daxpy = daxpy; 

            #endregion

        }
    
        public DTRSL()
        {
    

            #region Dependencies (Initialization)
            
            DDOT ddot = new DDOT();
            DAXPY daxpy = new DAXPY();

            #endregion


            #region Set Dependencies
            
            this._ddot = ddot; this._daxpy = daxpy; 

            #endregion

        }
        /// <param name="T">
        /// * x = b
        ///</param>
        /// <param name="LDT">
        /// integer
        /// ldt is the leading dimension of the array t.
        ///</param>
        /// <param name="N">
        /// integer
        /// n is the order of the system.
        ///</param>
        /// <param name="B">
        /// double precision(n).
        /// b contains the right hand side of the system.
        ///</param>
        /// <param name="JOB">
        /// integer
        /// job specifies what kind of system is to be solved.
        /// if job is
        /// 
        /// 00   solve t*x=b, t lower triangular,
        /// 01   solve t*x=b, t upper triangular,
        /// 10   solve trans(t)*x=b, t lower triangular,
        /// 11   solve trans(t)*x=b, t upper triangular.
        ///</param>
        /// <param name="INFO">
        /// integer
        /// info contains zero if the system is nonsingular.
        /// otherwise info contains the index of
        /// the first zero diagonal element of t.
        ///</param>
        public void Run(double[] T, int offset_t, int LDT, int N, ref double[] B, int offset_b, int JOB, ref int INFO)
        {

            #region Variables
            
            double TEMP = 0; int CASE = 0; int J = 0; int JJ = 0; 

            #endregion


            #region Array Index Correction
            
             int o_t = -1 - LDT + offset_t;  int o_b = -1 + offset_b; 

            #endregion


            #region Prolog
            
            // c
            // c
            // c     dtrsl solves systems of the form
            // c
            // c                   t * x = b
            // c     or
            // c                   trans(t) * x = b
            // c
            // c     where t is a triangular matrix of order n. here trans(t)
            // c     denotes the transpose of the matrix t.
            // c
            // c     on entry
            // c
            // c         t         double precision(ldt,n)
            // c                   t contains the matrix of the system. the zero
            // c                   elements of the matrix are not referenced, and
            // c                   the corresponding elements of the array can be
            // c                   used to store other information.
            // c
            // c         ldt       integer
            // c                   ldt is the leading dimension of the array t.
            // c
            // c         n         integer
            // c                   n is the order of the system.
            // c
            // c         b         double precision(n).
            // c                   b contains the right hand side of the system.
            // c
            // c         job       integer
            // c                   job specifies what kind of system is to be solved.
            // c                   if job is
            // c
            // c                        00   solve t*x=b, t lower triangular,
            // c                        01   solve t*x=b, t upper triangular,
            // c                        10   solve trans(t)*x=b, t lower triangular,
            // c                        11   solve trans(t)*x=b, t upper triangular.
            // c
            // c     on return
            // c
            // c         b         b contains the solution, if info .eq. 0.
            // c                   otherwise b is unaltered.
            // c
            // c         info      integer
            // c                   info contains zero if the system is nonsingular.
            // c                   otherwise info contains the index of
            // c                   the first zero diagonal element of t.
            // c
            // c     linpack. this version dated 08/14/78 .
            // c     g. w. stewart, university of maryland, argonne national lab.
            // c
            // c     subroutines and functions
            // c
            // c     blas daxpy,ddot
            // c     fortran mod
            // c
            // c     internal variables
            // c
            //	INTRINSIC MOD;
            // c
            // c     begin block permitting ...exits to 150
            // c
            // c        check for zero diagonal elements.
            // c

            #endregion


            #region Body
            
            for (INFO = 1; INFO <= N; INFO++)
            {
                // c     ......exit
                if (T[INFO+INFO * LDT + o_t] == 0.0E0) goto LABEL150;
            }
            INFO = 0;
            // c
            // c        determine the task and go to it.
            // c
            CASE = 1;
            if (FortranLib.Mod(JOB,10) != 0) CASE = 2;
            if (FortranLib.Mod(JOB,100) / 10 != 0) CASE += 2;
            switch (CASE)
            {
                case 1: goto LABEL20;
                case 2: goto LABEL50;
                case 3: goto LABEL80;
                case 4: goto LABEL110;
            }
            // c
            // c        solve t*x=b for t lower triangular
            // c
        LABEL20:;
            B[1 + o_b] /= T[1+1 * LDT + o_t];
            if (N < 2) goto LABEL40;
            for (J = 2; J <= N; J++)
            {
                TEMP =  - B[J - 1 + o_b];
                this._daxpy.Run(N - J + 1, TEMP, T, J+(J - 1) * LDT + o_t, 1, ref B, J + o_b, 1);
                B[J + o_b] /= T[J+J * LDT + o_t];
            }
        LABEL40:;
            goto LABEL140;
            // c
            // c        solve t*x=b for t upper triangular.
            // c
        LABEL50:;
            B[N + o_b] /= T[N+N * LDT + o_t];
            if (N < 2) goto LABEL70;
            for (JJ = 2; JJ <= N; JJ++)
            {
                J = N - JJ + 1;
                TEMP =  - B[J + 1 + o_b];
                this._daxpy.Run(J, TEMP, T, 1+(J + 1) * LDT + o_t, 1, ref B, 1 + o_b, 1);
                B[J + o_b] /= T[J+J * LDT + o_t];
            }
        LABEL70:;
            goto LABEL140;
            // c
            // c        solve trans(t)*x=b for t lower triangular.
            // c
        LABEL80:;
            B[N + o_b] /= T[N+N * LDT + o_t];
            if (N < 2) goto LABEL100;
            for (JJ = 2; JJ <= N; JJ++)
            {
                J = N - JJ + 1;
                B[J + o_b] -= this._ddot.Run(JJ - 1, T, J + 1+J * LDT + o_t, 1, B, J + 1 + o_b, 1);
                B[J + o_b] /= T[J+J * LDT + o_t];
            }
        LABEL100:;
            goto LABEL140;
            // c
            // c        solve trans(t)*x=b for t upper triangular.
            // c
        LABEL110:;
            B[1 + o_b] /= T[1+1 * LDT + o_t];
            if (N < 2) goto LABEL130;
            for (J = 2; J <= N; J++)
            {
                B[J + o_b] -= this._ddot.Run(J - 1, T, 1+J * LDT + o_t, 1, B, 1 + o_b, 1);
                B[J + o_b] /= T[J+J * LDT + o_t];
            }
        LABEL130:;
        LABEL140:;
        LABEL150:;
            return;

            #endregion

        }
    }
    
    // c====================== The end of dtrsl ===============================
    
}
