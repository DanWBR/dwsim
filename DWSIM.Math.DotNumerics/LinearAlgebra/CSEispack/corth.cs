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
    public class CORTH
    {
    

        #region Dependencies
        
        PYTHAG _pythag; 

        #endregion

        public CORTH(PYTHAG pythag)
        {
    

            #region Set Dependencies
            
            this._pythag = pythag; 

            #endregion

        }
    
        public CORTH()
        {
    

            #region Dependencies (Initialization)
            
            PYTHAG pythag = new PYTHAG();

            #endregion


            #region Set Dependencies
            
            this._pythag = pythag; 

            #endregion

        }
        /// <param name="NM">
        /// must be set to the row dimension of two-dimensional
        /// array parameters as declared in the calling program
        /// dimension statement.
        ///</param>
        /// <param name="N">
        /// is the order of the matrix.
        ///</param>
        /// <param name="LOW">
        /// and igh are integers determined by the balancing
        /// subroutine  cbal.  if  cbal  has not been used,
        /// set low=1, igh=n.
        ///</param>
        /// <param name="AR">
        /// and ai contain the real and imaginary parts,
        /// respectively, of the complex input matrix.
        ///</param>
        /// <param name="ORTR">
        /// and orti contain further information about the
        /// transformations.  only elements low through igh are used.
        ///</param>
        public void Run(int NM, int N, int LOW, int IGH, ref double[] AR, int offset_ar, ref double[] AI, int offset_ai
                         , ref double[] ORTR, int offset_ortr, ref double[] ORTI, int offset_orti)
        {

            #region Variables
            
            int I = 0; int J = 0; int M = 0; int II = 0; int JJ = 0; int LA = 0; int MP = 0; int KP1 = 0; double F = 0; 
            double G = 0;double H = 0; double FI = 0; double FR = 0; double SCALE = 0; 

            #endregion


            #region Implicit Variables
            
            int AR_J = 0; int AI_J = 0; 

            #endregion


            #region Array Index Correction
            
             int o_ar = -1 - NM + offset_ar;  int o_ai = -1 - NM + offset_ai;  int o_ortr = -1 + offset_ortr; 
             int o_orti = -1 + offset_orti;

            #endregion


            #region Prolog
            
            // c
            // c
            // c     this subroutine is a translation of a complex analogue of
            // c     the algol procedure orthes, num. math. 12, 349-368(1968)
            // c     by martin and wilkinson.
            // c     handbook for auto. comp., vol.ii-linear algebra, 339-358(1971).
            // c
            // c     given a complex general matrix, this subroutine
            // c     reduces a submatrix situated in rows and columns
            // c     low through igh to upper hessenberg form by
            // c     unitary similarity transformations.
            // c
            // c     on input
            // c
            // c        nm must be set to the row dimension of two-dimensional
            // c          array parameters as declared in the calling program
            // c          dimension statement.
            // c
            // c        n is the order of the matrix.
            // c
            // c        low and igh are integers determined by the balancing
            // c          subroutine  cbal.  if  cbal  has not been used,
            // c          set low=1, igh=n.
            // c
            // c        ar and ai contain the real and imaginary parts,
            // c          respectively, of the complex input matrix.
            // c
            // c     on output
            // c
            // c        ar and ai contain the real and imaginary parts,
            // c          respectively, of the hessenberg matrix.  information
            // c          about the unitary transformations used in the reduction
            // c          is stored in the remaining triangles under the
            // c          hessenberg matrix.
            // c
            // c        ortr and orti contain further information about the
            // c          transformations.  only elements low through igh are used.
            // c
            // c     calls pythag for  dsqrt(a*a + b*b) .
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
            
            LA = IGH - 1;
            KP1 = LOW + 1;
            if (LA < KP1) goto LABEL200;
            // c
            for (M = KP1; M <= LA; M++)
            {
                H = 0.0E0;
                ORTR[M + o_ortr] = 0.0E0;
                ORTI[M + o_orti] = 0.0E0;
                SCALE = 0.0E0;
                // c     .......... scale column (algol tol then not needed) ..........
                for (I = M; I <= IGH; I++)
                {
                    SCALE += Math.Abs(AR[I+(M - 1) * NM + o_ar]) + Math.Abs(AI[I+(M - 1) * NM + o_ai]);
                }
                // c
                if (SCALE == 0.0E0) goto LABEL180;
                MP = M + IGH;
                // c     .......... for i=igh step -1 until m do -- ..........
                for (II = M; II <= IGH; II++)
                {
                    I = MP - II;
                    ORTR[I + o_ortr] = AR[I+(M - 1) * NM + o_ar] / SCALE;
                    ORTI[I + o_orti] = AI[I+(M - 1) * NM + o_ai] / SCALE;
                    H += ORTR[I + o_ortr] * ORTR[I + o_ortr] + ORTI[I + o_orti] * ORTI[I + o_orti];
                }
                // c
                G = Math.Sqrt(H);
                F = this._pythag.Run(ORTR[M + o_ortr], ORTI[M + o_orti]);
                if (F == 0.0E0) goto LABEL103;
                H += F * G;
                G /= F;
                ORTR[M + o_ortr] = (1.0E0 + G) * ORTR[M + o_ortr];
                ORTI[M + o_orti] = (1.0E0 + G) * ORTI[M + o_orti];
                goto LABEL105;
                // c
            LABEL103:  ORTR[M + o_ortr] = G;
                AR[M+(M - 1) * NM + o_ar] = SCALE;
                // c     .......... form (i-(u*ut)/h) * a ..........
            LABEL105:  
                for (J = M; J <= N; J++)
                {
                    FR = 0.0E0;
                    FI = 0.0E0;
                    // c     .......... for i=igh step -1 until m do -- ..........
                    for (II = M; II <= IGH; II++)
                    {
                        I = MP - II;
                        FR += ORTR[I + o_ortr] * AR[I+J * NM + o_ar] + ORTI[I + o_orti] * AI[I+J * NM + o_ai];
                        FI += ORTR[I + o_ortr] * AI[I+J * NM + o_ai] - ORTI[I + o_orti] * AR[I+J * NM + o_ar];
                    }
                    // c
                    FR /= H;
                    FI /= H;
                    // c
                    AR_J = J * NM + o_ar;
                    AI_J = J * NM + o_ai;
                    for (I = M; I <= IGH; I++)
                    {
                        AR[I + AR_J] +=  - FR * ORTR[I + o_ortr] + FI * ORTI[I + o_orti];
                        AI[I + AI_J] +=  - FR * ORTI[I + o_orti] - FI * ORTR[I + o_ortr];
                    }
                    // c
                }
                // c     .......... form (i-(u*ut)/h)*a*(i-(u*ut)/h) ..........
                for (I = 1; I <= IGH; I++)
                {
                    FR = 0.0E0;
                    FI = 0.0E0;
                    // c     .......... for j=igh step -1 until m do -- ..........
                    for (JJ = M; JJ <= IGH; JJ++)
                    {
                        J = MP - JJ;
                        FR += ORTR[J + o_ortr] * AR[I+J * NM + o_ar] - ORTI[J + o_orti] * AI[I+J * NM + o_ai];
                        FI += ORTR[J + o_ortr] * AI[I+J * NM + o_ai] + ORTI[J + o_orti] * AR[I+J * NM + o_ar];
                    }
                    // c
                    FR /= H;
                    FI /= H;
                    // c
                    for (J = M; J <= IGH; J++)
                    {
                        AR[I+J * NM + o_ar] +=  - FR * ORTR[J + o_ortr] - FI * ORTI[J + o_orti];
                        AI[I+J * NM + o_ai] += FR * ORTI[J + o_orti] - FI * ORTR[J + o_ortr];
                    }
                    // c
                }
                // c
                ORTR[M + o_ortr] *= SCALE;
                ORTI[M + o_orti] *= SCALE;
                AR[M+(M - 1) * NM + o_ar] =  - G * AR[M+(M - 1) * NM + o_ar];
                AI[M+(M - 1) * NM + o_ai] =  - G * AI[M+(M - 1) * NM + o_ai];
            LABEL180:;
            }
            // c
        LABEL200:  return;

            #endregion

        }
    }
}
