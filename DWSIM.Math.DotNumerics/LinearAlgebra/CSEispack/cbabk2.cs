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
    public class CBABK2
    {
    
        public CBABK2()
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
        /// <param name="LOW">
        /// and igh are integers determined by  cbal.
        ///</param>
        /// <param name="SCALE">
        /// contains information determining the permutations
        /// and scaling factors used by  cbal.
        ///</param>
        /// <param name="M">
        /// is the number of eigenvectors to be back transformed.
        ///</param>
        /// <param name="ZR">
        /// and zi contain the real and imaginary parts,
        /// respectively, of the eigenvectors to be
        /// back transformed in their first m columns.
        ///</param>
        public void Run(int NM, int N, int LOW, int IGH, double[] SCALE, int offset_scale, int M
                         , ref double[] ZR, int offset_zr, ref double[] ZI, int offset_zi)
        {

            #region Variables
            
            int I = 0; int J = 0; int K = 0; int II = 0; double S = 0; 

            #endregion


            #region Array Index Correction
            
             int o_scale = -1 + offset_scale;  int o_zr = -1 - NM + offset_zr;  int o_zi = -1 - NM + offset_zi; 

            #endregion


            #region Prolog
            
            // c
            // c
            // c     this subroutine is a translation of the algol procedure
            // c     cbabk2, which is a complex version of balbak,
            // c     num. math. 13, 293-304(1969) by parlett and reinsch.
            // c     handbook for auto. comp., vol.ii-linear algebra, 315-326(1971).
            // c
            // c     this subroutine forms the eigenvectors of a complex general
            // c     matrix by back transforming those of the corresponding
            // c     balanced matrix determined by  cbal.
            // c
            // c     on input
            // c
            // c        nm must be set to the row dimension of two-dimensional
            // c          array parameters as declared in the calling program
            // c          dimension statement.
            // c
            // c        n is the order of the matrix.
            // c
            // c        low and igh are integers determined by  cbal.
            // c
            // c        scale contains information determining the permutations
            // c          and scaling factors used by  cbal.
            // c
            // c        m is the number of eigenvectors to be back transformed.
            // c
            // c        zr and zi contain the real and imaginary parts,
            // c          respectively, of the eigenvectors to be
            // c          back transformed in their first m columns.
            // c
            // c     on output
            // c
            // c        zr and zi contain the real and imaginary parts,
            // c          respectively, of the transformed eigenvectors
            // c          in their first m columns.
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
            
            if (M == 0) goto LABEL200;
            if (IGH == LOW) goto LABEL120;
            // c
            for (I = LOW; I <= IGH; I++)
            {
                S = SCALE[I + o_scale];
                // c     .......... left hand eigenvectors are back transformed
                // c                if the foregoing statement is replaced by
                // c                s=1.0d0/scale(i). ..........
                for (J = 1; J <= M; J++)
                {
                    ZR[I+J * NM + o_zr] *= S;
                    ZI[I+J * NM + o_zi] *= S;
                }
                // c
            }
            // c     .......... for i=low-1 step -1 until 1,
            // c                igh+1 step 1 until n do -- ..........
        LABEL120:  
            for (II = 1; II <= N; II++)
            {
                I = II;
                if (I >= LOW && I <= IGH) goto LABEL140;
                if (I < LOW) I = LOW - II;
                K = (int)SCALE[I + o_scale];
                if (K == I) goto LABEL140;
                // c
                for (J = 1; J <= M; J++)
                {
                    S = ZR[I+J * NM + o_zr];
                    ZR[I+J * NM + o_zr] = ZR[K+J * NM + o_zr];
                    ZR[K+J * NM + o_zr] = S;
                    S = ZI[I+J * NM + o_zi];
                    ZI[I+J * NM + o_zi] = ZI[K+J * NM + o_zi];
                    ZI[K+J * NM + o_zi] = S;
                }
                // c
            LABEL140:;
            }
            // c
        LABEL200:  return;

            #endregion

        }
    }
}
