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
    public class CG
    {
    

        #region Dependencies
        
        CBAL _cbal; CORTH _corth; COMQR _comqr; COMQR2 _comqr2; CBABK2 _cbabk2; 

        #endregion

        public CG(CBAL cbal, CORTH corth, COMQR comqr, COMQR2 comqr2, CBABK2 cbabk2)
        {
    

            #region Set Dependencies
            
            this._cbal = cbal; this._corth = corth; this._comqr = comqr; this._comqr2 = comqr2; this._cbabk2 = cbabk2; 

            #endregion

        }
    
        public CG()
        {
    

            #region Dependencies (Initialization)
            
            CBAL cbal = new CBAL();
            PYTHAG pythag = new PYTHAG();
            CDIV cdiv = new CDIV();
            CBABK2 cbabk2 = new CBABK2();
            CORTH corth = new CORTH(pythag);
            CSROOT csroot = new CSROOT(pythag);
            COMQR comqr = new COMQR(pythag, csroot, cdiv);
            COMQR2 comqr2 = new COMQR2(pythag, csroot, cdiv);

            #endregion


            #region Set Dependencies
            
            this._cbal = cbal; this._corth = corth; this._comqr = comqr; this._comqr2 = comqr2; this._cbabk2 = cbabk2; 

            #endregion

        }
        /// <param name="NM">
        /// must be set to the row dimension of the two-dimensional
        ///</param>
        /// <param name="N">
        /// is the order of the matrix  a=(ar,ai).
        ///</param>
        /// <param name="AR">
        /// and  ai  contain the real and imaginary parts,
        ///</param>
        /// <param name="WR">
        /// and  wi  contain the real and imaginary parts,
        ///</param>
        /// <param name="MATZ">
        /// is an integer variable set equal to zero if
        ///</param>
        /// <param name="ZR">
        /// and  zi  contain the real and imaginary parts,
        ///</param>
        /// <param name="IERR">
        /// is an integer output variable set equal to an error
        /// completion code described in the documentation for comqr
        /// and comqr2.  the normal completion code is zero.
        ///</param>
        public void Run(int NM, int N, ref double[] AR, int offset_ar, ref double[] AI, int offset_ai, ref double[] WR, int offset_wr, ref double[] WI, int offset_wi
                         , int MATZ, ref double[] ZR, int offset_zr, ref double[] ZI, int offset_zi, ref double[] FV1, int offset_fv1, ref double[] FV2, int offset_fv2, ref double[] FV3, int offset_fv3
                         , ref int IERR)
        {

            #region Variables
            
            int IS1 = 0; int IS2 = 0; 

            #endregion


            #region Array Index Correction
            
             int o_ar = -1 - NM + offset_ar;  int o_ai = -1 - NM + offset_ai;  int o_wr = -1 + offset_wr; 
             int o_wi = -1 + offset_wi; int o_zr = -1 - NM + offset_zr;  int o_zi = -1 - NM + offset_zi; 
             int o_fv1 = -1 + offset_fv1; int o_fv2 = -1 + offset_fv2;  int o_fv3 = -1 + offset_fv3; 

            #endregion


            #region Prolog
            
            // c
            // c
            // c     this subroutine calls the recommended sequence of
            // c     subroutines from the eigensystem subroutine package (eispack)
            // c     to find the eigenvalues and eigenvectors (if desired)
            // c     of a complex general matrix.
            // c
            // c     on input
            // c
            // c        nm  must be set to the row dimension of the two-dimensional
            // c        array parameters as declared in the calling program
            // c        dimension statement.
            // c
            // c        n  is the order of the matrix  a=(ar,ai).
            // c
            // c        ar  and  ai  contain the real and imaginary parts,
            // c        respectively, of the complex general matrix.
            // c
            // c        matz  is an integer variable set equal to zero if
            // c        only eigenvalues are desired.  otherwise it is set to
            // c        any non-zero integer for both eigenvalues and eigenvectors.
            // c
            // c     on output
            // c
            // c        wr  and  wi  contain the real and imaginary parts,
            // c        respectively, of the eigenvalues.
            // c
            // c        zr  and  zi  contain the real and imaginary parts,
            // c        respectively, of the eigenvectors if matz is not zero.
            // c
            // c        ierr  is an integer output variable set equal to an error
            // c           completion code described in the documentation for comqr
            // c           and comqr2.  the normal completion code is zero.
            // c
            // c        fv1, fv2, and  fv3  are temporary storage arrays.
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
            
            if (N <= NM) goto LABEL10;
            IERR = 10 * N;
            goto LABEL50;
            // c
        LABEL10:
                this._cbal.Run(NM, N, ref AR, offset_ar, ref AI, offset_ai, ref IS1, ref IS2
                               , ref FV1, offset_fv1);
            this._corth.Run(NM, N, IS1, IS2, ref AR, offset_ar, ref AI, offset_ai
                            , ref FV2, offset_fv2, ref FV3, offset_fv3);
            if (MATZ != 0) goto LABEL20;
            // c     .......... find eigenvalues only ..........
            this._comqr.Run(NM, N, IS1, IS2, ref AR, offset_ar, ref AI, offset_ai
                            , ref WR, offset_wr, ref WI, offset_wi, ref IERR);
            goto LABEL50;
            // c     .......... find both eigenvalues and eigenvectors ..........
        LABEL20:
                this._comqr2.Run(NM, N, IS1, IS2, ref FV2, offset_fv2, ref FV3, offset_fv3
                                 , ref AR, offset_ar, ref AI, offset_ai, ref WR, offset_wr, ref WI, offset_wi, ref ZR, offset_zr, ref ZI, offset_zi
                                 , ref IERR);
            if (IERR != 0) goto LABEL50;
            this._cbabk2.Run(NM, N, IS1, IS2, FV1, offset_fv1, N
                             , ref ZR, offset_zr, ref ZI, offset_zi);
        LABEL50:  return;

            #endregion

        }
    }
}
