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

namespace DotNumerics.LinearAlgebra.CSLapack
{
    /// <summary>
    /// -- LAPACK auxiliary routine (version 3.1) --
    /// Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
    /// November 2006
    /// Purpose
    /// =======
    /// 
    /// DLAHR2 reduces the first NB columns of A real general n-BY-(n-k+1)
    /// matrix A so that elements below the k-th subdiagonal are zero. The
    /// reduction is performed by an orthogonal similarity transformation
    /// Q' * A * Q. The routine returns the matrices V and T which determine
    /// Q as a block reflector I - V*T*V', and also the matrix Y = A * V * T.
    /// 
    /// This is an auxiliary routine called by DGEHRD.
    /// 
    ///</summary>
    public class DLAHR2
    {
    

        #region Dependencies
        
        DAXPY _daxpy; DCOPY _dcopy; DGEMM _dgemm; DGEMV _dgemv; DLACPY _dlacpy; DLARFG _dlarfg; DSCAL _dscal; DTRMM _dtrmm; 
        DTRMV _dtrmv;

        #endregion


        #region Variables
        
        const double ZERO = 0.0E+0; const double ONE = 1.0E+0; 

        #endregion

        public DLAHR2(DAXPY daxpy, DCOPY dcopy, DGEMM dgemm, DGEMV dgemv, DLACPY dlacpy, DLARFG dlarfg, DSCAL dscal, DTRMM dtrmm, DTRMV dtrmv)
        {
    

            #region Set Dependencies
            
            this._daxpy = daxpy; this._dcopy = dcopy; this._dgemm = dgemm; this._dgemv = dgemv; this._dlacpy = dlacpy; 
            this._dlarfg = dlarfg;this._dscal = dscal; this._dtrmm = dtrmm; this._dtrmv = dtrmv; 

            #endregion

        }
    
        public DLAHR2()
        {
    

            #region Dependencies (Initialization)
            
            DAXPY daxpy = new DAXPY();
            DCOPY dcopy = new DCOPY();
            LSAME lsame = new LSAME();
            XERBLA xerbla = new XERBLA();
            DLAMC3 dlamc3 = new DLAMC3();
            DLAPY2 dlapy2 = new DLAPY2();
            DNRM2 dnrm2 = new DNRM2();
            DSCAL dscal = new DSCAL();
            DGEMM dgemm = new DGEMM(lsame, xerbla);
            DGEMV dgemv = new DGEMV(lsame, xerbla);
            DLACPY dlacpy = new DLACPY(lsame);
            DLAMC1 dlamc1 = new DLAMC1(dlamc3);
            DLAMC4 dlamc4 = new DLAMC4(dlamc3);
            DLAMC5 dlamc5 = new DLAMC5(dlamc3);
            DLAMC2 dlamc2 = new DLAMC2(dlamc3, dlamc1, dlamc4, dlamc5);
            DLAMCH dlamch = new DLAMCH(lsame, dlamc2);
            DLARFG dlarfg = new DLARFG(dlamch, dlapy2, dnrm2, dscal);
            DTRMM dtrmm = new DTRMM(lsame, xerbla);
            DTRMV dtrmv = new DTRMV(lsame, xerbla);

            #endregion


            #region Set Dependencies
            
            this._daxpy = daxpy; this._dcopy = dcopy; this._dgemm = dgemm; this._dgemv = dgemv; this._dlacpy = dlacpy; 
            this._dlarfg = dlarfg;this._dscal = dscal; this._dtrmm = dtrmm; this._dtrmv = dtrmv; 

            #endregion

        }
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DLAHR2 reduces the first NB columns of A real general n-BY-(n-k+1)
        /// matrix A so that elements below the k-th subdiagonal are zero. The
        /// reduction is performed by an orthogonal similarity transformation
        /// Q' * A * Q. The routine returns the matrices V and T which determine
        /// Q as a block reflector I - V*T*V', and also the matrix Y = A * V * T.
        /// 
        /// This is an auxiliary routine called by DGEHRD.
        /// 
        ///</summary>
        /// <param name="N">
        /// (input) INTEGER
        /// The order of the matrix A.
        ///</param>
        /// <param name="K">
        /// (input) INTEGER
        /// The offset for the reduction. Elements below the k-th
        /// subdiagonal in the first NB columns are reduced to zero.
        /// K .LT. N.
        ///</param>
        /// <param name="NB">
        /// (input) INTEGER
        /// The number of columns to be reduced.
        ///</param>
        /// <param name="A">
        /// (input/output) DOUBLE PRECISION array, dimension (LDA,N-K+1)
        /// On entry, the n-by-(n-k+1) general matrix A.
        /// On exit, the elements on and above the k-th subdiagonal in
        /// the first NB columns are overwritten with the corresponding
        /// elements of the reduced matrix; the elements below the k-th
        /// subdiagonal, with the array TAU, represent the matrix Q as a
        /// product of elementary reflectors. The other columns of A are
        /// unchanged. See Further Details.
        ///</param>
        /// <param name="LDA">
        /// (input) INTEGER
        /// The leading dimension of the array A.  LDA .GE. max(1,N).
        ///</param>
        /// <param name="TAU">
        /// (output) DOUBLE PRECISION array, dimension (NB)
        /// The scalar factors of the elementary reflectors. See Further
        /// Details.
        ///</param>
        /// <param name="T">
        /// (output) DOUBLE PRECISION array, dimension (LDT,NB)
        /// The upper triangular matrix T.
        ///</param>
        /// <param name="LDT">
        /// (input) INTEGER
        /// The leading dimension of the array T.  LDT .GE. NB.
        ///</param>
        /// <param name="Y">
        /// (output) DOUBLE PRECISION array, dimension (LDY,NB)
        /// The n-by-nb matrix Y.
        ///</param>
        /// <param name="LDY">
        /// (input) INTEGER
        /// The leading dimension of the array Y. LDY .GE. N.
        ///</param>
        public void Run(int N, int K, int NB, ref double[] A, int offset_a, int LDA, ref double[] TAU, int offset_tau
                         , ref double[] T, int offset_t, int LDT, ref double[] Y, int offset_y, int LDY)
        {

            #region Variables
            
            int I = 0; double EI = 0; 

            #endregion


            #region Array Index Correction
            
             int o_a = -1 - LDA + offset_a;  int o_tau = -1 + offset_tau;  int o_t = -1 - LDT + offset_t; 
             int o_y = -1 - LDY + offset_y;

            #endregion


            #region Prolog
            
            // *
            // *  -- LAPACK auxiliary routine (version 3.1) --
            // *     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
            // *     November 2006
            // *
            // *     .. Scalar Arguments ..
            // *     ..
            // *     .. Array Arguments ..
            // *     ..
            // *
            // *  Purpose
            // *  =======
            // *
            // *  DLAHR2 reduces the first NB columns of A real general n-BY-(n-k+1)
            // *  matrix A so that elements below the k-th subdiagonal are zero. The
            // *  reduction is performed by an orthogonal similarity transformation
            // *  Q' * A * Q. The routine returns the matrices V and T which determine
            // *  Q as a block reflector I - V*T*V', and also the matrix Y = A * V * T.
            // *
            // *  This is an auxiliary routine called by DGEHRD.
            // *
            // *  Arguments
            // *  =========
            // *
            // *  N       (input) INTEGER
            // *          The order of the matrix A.
            // *
            // *  K       (input) INTEGER
            // *          The offset for the reduction. Elements below the k-th
            // *          subdiagonal in the first NB columns are reduced to zero.
            // *          K < N.
            // *
            // *  NB      (input) INTEGER
            // *          The number of columns to be reduced.
            // *
            // *  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N-K+1)
            // *          On entry, the n-by-(n-k+1) general matrix A.
            // *          On exit, the elements on and above the k-th subdiagonal in
            // *          the first NB columns are overwritten with the corresponding
            // *          elements of the reduced matrix; the elements below the k-th
            // *          subdiagonal, with the array TAU, represent the matrix Q as a
            // *          product of elementary reflectors. The other columns of A are
            // *          unchanged. See Further Details.
            // *
            // *  LDA     (input) INTEGER
            // *          The leading dimension of the array A.  LDA >= max(1,N).
            // *
            // *  TAU     (output) DOUBLE PRECISION array, dimension (NB)
            // *          The scalar factors of the elementary reflectors. See Further
            // *          Details.
            // *
            // *  T       (output) DOUBLE PRECISION array, dimension (LDT,NB)
            // *          The upper triangular matrix T.
            // *
            // *  LDT     (input) INTEGER
            // *          The leading dimension of the array T.  LDT >= NB.
            // *
            // *  Y       (output) DOUBLE PRECISION array, dimension (LDY,NB)
            // *          The n-by-nb matrix Y.
            // *
            // *  LDY     (input) INTEGER
            // *          The leading dimension of the array Y. LDY >= N.
            // *
            // *  Further Details
            // *  ===============
            // *
            // *  The matrix Q is represented as a product of nb elementary reflectors
            // *
            // *     Q = H(1) H(2) . . . H(nb).
            // *
            // *  Each H(i) has the form
            // *
            // *     H(i) = I - tau * v * v'
            // *
            // *  where tau is a real scalar, and v is a real vector with
            // *  v(1:i+k-1) = 0, v(i+k) = 1; v(i+k+1:n) is stored on exit in
            // *  A(i+k+1:n,i), and tau in TAU(i).
            // *
            // *  The elements of the vectors v together form the (n-k+1)-by-nb matrix
            // *  V which is needed, with T and Y, to apply the transformation to the
            // *  unreduced part of the matrix, using an update of the form:
            // *  A := (I - V*T*V') * (A - Y*V').
            // *
            // *  The contents of A on exit are illustrated by the following example
            // *  with n = 7, k = 3 and nb = 2:
            // *
            // *     ( a   a   a   a   a )
            // *     ( a   a   a   a   a )
            // *     ( a   a   a   a   a )
            // *     ( h   h   a   a   a )
            // *     ( v1  h   a   a   a )
            // *     ( v1  v2  a   a   a )
            // *     ( v1  v2  a   a   a )
            // *
            // *  where a denotes an element of the original matrix A, h denotes a
            // *  modified element of the upper Hessenberg matrix H, and vi denotes an
            // *  element of the vector defining H(i).
            // *
            // *  This file is a slight modification of LAPACK-3.0's DLAHRD
            // *  incorporating improvements proposed by Quintana-Orti and Van de
            // *  Gejin. Note that the entries of A(1:K,2:NB) differ from those
            // *  returned by the original LAPACK routine. This function is
            // *  not backward compatible with LAPACK3.0.
            // *
            // *  =====================================================================
            // *
            // *     .. Parameters ..
            // *     ..
            // *     .. Local Scalars ..
            // *     ..
            // *     .. External Subroutines ..
            // *     ..
            // *     .. Intrinsic Functions ..
            //      INTRINSIC          MIN;
            // *     ..
            // *     .. Executable Statements ..
            // *
            // *     Quick return if possible
            // *

            #endregion


            #region Body
            
            if (N <= 1) return;
            // *
            for (I = 1; I <= NB; I++)
            {
                if (I > 1)
                {
                    // *
                    // *           Update A(K+1:N,I)
                    // *
                    // *           Update I-th column of A - Y * V'
                    // *
                    this._dgemv.Run("NO TRANSPOSE", N - K, I - 1,  - ONE, Y, K + 1+1 * LDY + o_y, LDY
                                    , A, K + I - 1+1 * LDA + o_a, LDA, ONE, ref A, K + 1+I * LDA + o_a, 1);
                    // *
                    // *           Apply I - V * T' * V' to this column (call it b) from the
                    // *           left, using the last column of T as workspace
                    // *
                    // *           Let  V = ( V1 )   and   b = ( b1 )   (first I-1 rows)
                    // *                    ( V2 )             ( b2 )
                    // *
                    // *           where V1 is unit lower triangular
                    // *
                    // *           w := V1' * b1
                    // *
                    this._dcopy.Run(I - 1, A, K + 1+I * LDA + o_a, 1, ref T, 1+NB * LDT + o_t, 1);
                    this._dtrmv.Run("Lower", "Transpose", "UNIT", I - 1, A, K + 1+1 * LDA + o_a, LDA
                                    , ref T, 1+NB * LDT + o_t, 1);
                    // *
                    // *           w := w + V2'*b2
                    // *
                    this._dgemv.Run("Transpose", N - K - I + 1, I - 1, ONE, A, K + I+1 * LDA + o_a, LDA
                                    , A, K + I+I * LDA + o_a, 1, ONE, ref T, 1+NB * LDT + o_t, 1);
                    // *
                    // *           w := T'*w
                    // *
                    this._dtrmv.Run("Upper", "Transpose", "NON-UNIT", I - 1, T, offset_t, LDT
                                    , ref T, 1+NB * LDT + o_t, 1);
                    // *
                    // *           b2 := b2 - V2*w
                    // *
                    this._dgemv.Run("NO TRANSPOSE", N - K - I + 1, I - 1,  - ONE, A, K + I+1 * LDA + o_a, LDA
                                    , T, 1+NB * LDT + o_t, 1, ONE, ref A, K + I+I * LDA + o_a, 1);
                    // *
                    // *           b1 := b1 - V1*w
                    // *
                    this._dtrmv.Run("Lower", "NO TRANSPOSE", "UNIT", I - 1, A, K + 1+1 * LDA + o_a, LDA
                                    , ref T, 1+NB * LDT + o_t, 1);
                    this._daxpy.Run(I - 1,  - ONE, T, 1+NB * LDT + o_t, 1, ref A, K + 1+I * LDA + o_a, 1);
                    // *
                    A[K + I - 1+(I - 1) * LDA + o_a] = EI;
                }
                // *
                // *        Generate the elementary reflector H(I) to annihilate
                // *        A(K+I+1:N,I)
                // *
                this._dlarfg.Run(N - K - I + 1, ref A[K + I+I * LDA + o_a], ref A, Math.Min(K + I + 1, N)+I * LDA + o_a, 1, ref TAU[I + o_tau]);
                EI = A[K + I+I * LDA + o_a];
                A[K + I+I * LDA + o_a] = ONE;
                // *
                // *        Compute  Y(K+1:N,I)
                // *
                this._dgemv.Run("NO TRANSPOSE", N - K, N - K - I + 1, ONE, A, K + 1+(I + 1) * LDA + o_a, LDA
                                , A, K + I+I * LDA + o_a, 1, ZERO, ref Y, K + 1+I * LDY + o_y, 1);
                this._dgemv.Run("Transpose", N - K - I + 1, I - 1, ONE, A, K + I+1 * LDA + o_a, LDA
                                , A, K + I+I * LDA + o_a, 1, ZERO, ref T, 1+I * LDT + o_t, 1);
                this._dgemv.Run("NO TRANSPOSE", N - K, I - 1,  - ONE, Y, K + 1+1 * LDY + o_y, LDY
                                , T, 1+I * LDT + o_t, 1, ONE, ref Y, K + 1+I * LDY + o_y, 1);
                this._dscal.Run(N - K, TAU[I + o_tau], ref Y, K + 1+I * LDY + o_y, 1);
                // *
                // *        Compute T(1:I,I)
                // *
                this._dscal.Run(I - 1,  - TAU[I + o_tau], ref T, 1+I * LDT + o_t, 1);
                this._dtrmv.Run("Upper", "No Transpose", "NON-UNIT", I - 1, T, offset_t, LDT
                                , ref T, 1+I * LDT + o_t, 1);
                T[I+I * LDT + o_t] = TAU[I + o_tau];
                // *
            }
            A[K + NB+NB * LDA + o_a] = EI;
            // *
            // *     Compute Y(1:K,1:NB)
            // *
            this._dlacpy.Run("ALL", K, NB, A, 1+2 * LDA + o_a, LDA, ref Y, offset_y
                             , LDY);
            this._dtrmm.Run("RIGHT", "Lower", "NO TRANSPOSE", "UNIT", K, NB
                            , ONE, A, K + 1+1 * LDA + o_a, LDA, ref Y, offset_y, LDY);
            if (N > K + NB)
            {
                this._dgemm.Run("NO TRANSPOSE", "NO TRANSPOSE", K, NB, N - K - NB, ONE
                                , A, 1+(2 + NB) * LDA + o_a, LDA, A, K + 1 + NB+1 * LDA + o_a, LDA, ONE, ref Y, offset_y
                                , LDY);
            }
            this._dtrmm.Run("RIGHT", "Upper", "NO TRANSPOSE", "NON-UNIT", K, NB
                            , ONE, T, offset_t, LDT, ref Y, offset_y, LDY);
            // *
            return;
            // *
            // *     End of DLAHR2
            // *

            #endregion

        }
    }
}
