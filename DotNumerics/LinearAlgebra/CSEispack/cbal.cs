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
    public class CBAL
    {
    
        public CBAL()
        {
    
        }
    
        /// <param name="NM">
        /// must be set to the row dimension of two-dimensional
        /// array parameters as declared in the calling program
        /// dimension statement.
        ///</param>
        /// <param name="N">
        /// is the order of the matrix.
        ///</param>
        /// <param name="AR">
        /// and ai contain the real and imaginary parts,
        /// respectively, of the complex matrix to be balanced.
        ///</param>
        /// <param name="LOW">
        /// and igh are two integers such that ar(i,j) and ai(i,j)
        /// are equal to zero if
        /// (1) i is greater than j and
        /// (2) j=1,...,low-1 or i=igh+1,...,n.
        ///</param>
        /// <param name="SCALE">
        /// contains information determining the
        /// permutations and scaling factors used.
        ///</param>
        public void Run(int NM, int N, ref double[] AR, int offset_ar, ref double[] AI, int offset_ai, ref int LOW, ref int IGH
                         , ref double[] SCALE, int offset_scale)
        {

            #region Variables
            
            int I = 0; int J = 0; int K = 0; int L = 0; int M = 0; int JJ = 0; int IEXC = 0; double C = 0; double F = 0; 
            double G = 0;double R = 0; double S = 0; double B2 = 0; double RADIX = 0; bool NOCONV = false; 

            #endregion


            #region Implicit Variables
            
            int AR_J = 0; int AR_M = 0; int AI_J = 0; int AI_M = 0; int AR_I = 0; int AI_I = 0; 

            #endregion


            #region Array Index Correction
            
             int o_ar = -1 - NM + offset_ar;  int o_ai = -1 - NM + offset_ai;  int o_scale = -1 + offset_scale; 

            #endregion


            #region Prolog
            
            // c
            // c
            // c     this subroutine is a translation of the algol procedure
            // c     cbalance, which is a complex version of balance,
            // c     num. math. 13, 293-304(1969) by parlett and reinsch.
            // c     handbook for auto. comp., vol.ii-linear algebra, 315-326(1971).
            // c
            // c     this subroutine balances a complex matrix and isolates
            // c     eigenvalues whenever possible.
            // c
            // c     on input
            // c
            // c        nm must be set to the row dimension of two-dimensional
            // c          array parameters as declared in the calling program
            // c          dimension statement.
            // c
            // c        n is the order of the matrix.
            // c
            // c        ar and ai contain the real and imaginary parts,
            // c          respectively, of the complex matrix to be balanced.
            // c
            // c     on output
            // c
            // c        ar and ai contain the real and imaginary parts,
            // c          respectively, of the balanced matrix.
            // c
            // c        low and igh are two integers such that ar(i,j) and ai(i,j)
            // c          are equal to zero if
            // c           (1) i is greater than j and
            // c           (2) j=1,...,low-1 or i=igh+1,...,n.
            // c
            // c        scale contains information determining the
            // c           permutations and scaling factors used.
            // c
            // c     suppose that the principal submatrix in rows low through igh
            // c     has been balanced, that p(j) denotes the index interchanged
            // c     with j during the permutation step, and that the elements
            // c     of the diagonal matrix used are denoted by d(i,j).  then
            // c        scale(j) = p(j),    for j = 1,...,low-1
            // c                 = d(j,j)       j = low,...,igh
            // c                 = p(j)         j = igh+1,...,n.
            // c     the order in which the interchanges are made is n to igh+1,
            // c     then 1 to low-1.
            // c
            // c     note that 1 is returned for igh if igh is zero formally.
            // c
            // c     the algol procedure exc contained in cbalance appears in
            // c     cbal  in line.  (note that the algol roles of identifiers
            // c     k,l have been reversed.)
            // c
            // c     arithmetic is real throughout.
            // c
            // c     questions and comments should be directed to burton s. garbow,
            // c     mathematics and computer science div, argonne national laboratory
            // c
            // c     this version dated august 1983.
            // c
            // c     ------------------------------------------------------------------
            // c

            #endregion


            #region Body
            
            RADIX = 16.0E0;
            // c
            B2 = RADIX * RADIX;
            K = 1;
            L = N;
            goto LABEL100;
            // c     .......... in-line procedure for row and
            // c                column exchange ..........
        LABEL20:  SCALE[M + o_scale] = J;
            if (J == M) goto LABEL50;
            // c
            AR_J = J * NM + o_ar;
            AR_M = M * NM + o_ar;
            AI_J = J * NM + o_ai;
            AI_M = M * NM + o_ai;
            for (I = 1; I <= L; I++)
            {
                F = AR[I + AR_J];
                AR[I + AR_J] = AR[I + AR_M];
                AR[I + AR_M] = F;
                F = AI[I + AI_J];
                AI[I + AI_J] = AI[I + AI_M];
                AI[I + AI_M] = F;
            }
            // c
            for (I = K; I <= N; I++)
            {
                F = AR[J+I * NM + o_ar];
                AR[J+I * NM + o_ar] = AR[M+I * NM + o_ar];
                AR[M+I * NM + o_ar] = F;
                F = AI[J+I * NM + o_ai];
                AI[J+I * NM + o_ai] = AI[M+I * NM + o_ai];
                AI[M+I * NM + o_ai] = F;
            }
            // c
        LABEL50:
                switch (IEXC)
                {
                    case 1: goto LABEL80;
                    case 2: goto LABEL130;
                }
            // c     .......... search for rows isolating an eigenvalue
            // c                and push them down ..........
        LABEL80:  
            if (L == 1) goto LABEL280;
            L -= 1;
            // c     .......... for j=l step -1 until 1 do -- ..........
        LABEL100:  
            for (JJ = 1; JJ <= L; JJ++)
            {
                J = L + 1 - JJ;
                // c
                for (I = 1; I <= L; I++)
                {
                    if (I == J) goto LABEL110;
                    if (AR[J+I * NM + o_ar] != 0.0E0 || AI[J+I * NM + o_ai] != 0.0E0) goto LABEL120;
                LABEL110:;
                }
                // c
                M = L;
                IEXC = 1;
                goto LABEL20;
            LABEL120:;
            }
            // c
            goto LABEL140;
            // c     .......... search for columns isolating an eigenvalue
            // c                and push them left ..........
        LABEL130:  K += 1;
            // c
        LABEL140:  
            for (J = K; J <= L; J++)
            {
                // c
                for (I = K; I <= L; I++)
                {
                    if (I == J) goto LABEL150;
                    if (AR[I+J * NM + o_ar] != 0.0E0 || AI[I+J * NM + o_ai] != 0.0E0) goto LABEL170;
                LABEL150:;
                }
                // c
                M = K;
                IEXC = 2;
                goto LABEL20;
            LABEL170:;
            }
            // c     .......... now balance the submatrix in rows k to l ..........
            for (I = K; I <= L; I++)
            {
                SCALE[I + o_scale] = 1.0E0;
            }
            // c     .......... iterative loop for norm reduction ..........
        LABEL190:  NOCONV = false;
            // c
            for (I = K; I <= L; I++)
            {
                C = 0.0E0;
                R = 0.0E0;
                // c
                for (J = K; J <= L; J++)
                {
                    if (J == I) goto LABEL200;
                    C += Math.Abs(AR[J+I * NM + o_ar]) + Math.Abs(AI[J+I * NM + o_ai]);
                    R += Math.Abs(AR[I+J * NM + o_ar]) + Math.Abs(AI[I+J * NM + o_ai]);
                LABEL200:;
                }
                // c     .......... guard against zero c or r due to underflow ..........
                if (C == 0.0E0 || R == 0.0E0) goto LABEL270;
                G = R / RADIX;
                F = 1.0E0;
                S = C + R;
            LABEL210:  
                if (C >= G) goto LABEL220;
                F *= RADIX;
                C *= B2;
                goto LABEL210;
            LABEL220:  G = R * RADIX;
            LABEL230:  
                if (C < G) goto LABEL240;
                F /= RADIX;
                C /= B2;
                goto LABEL230;
                // c     .......... now balance ..........
            LABEL240:  
                if ((C + R) / F >= 0.95E0 * S) goto LABEL270;
                G = 1.0E0 / F;
                SCALE[I + o_scale] *= F;
                NOCONV = true;
                // c
                for (J = K; J <= N; J++)
                {
                    AR[I+J * NM + o_ar] *= G;
                    AI[I+J * NM + o_ai] *= G;
                }
                // c
                AR_I = I * NM + o_ar;
                AI_I = I * NM + o_ai;
                for (J = 1; J <= L; J++)
                {
                    AR[J + AR_I] *= F;
                    AI[J + AI_I] *= F;
                }
                // c
            LABEL270:;
            }
            // c
            if (NOCONV) goto LABEL190;
            // c
        LABEL280:  LOW = K;
            IGH = L;
            return;

            #endregion

        }
    }
}
