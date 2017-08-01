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
    /// DLAQP2 computes a QR factorization with column pivoting of
    /// the block A(OFFSET+1:M,1:N).
    /// The block A(1:OFFSET,1:N) is accordingly pivoted, but not factorized.
    /// 
    ///</summary>
    public class DLAQP2
    {
    

        #region Dependencies
        
        DLARF _dlarf; DLARFG _dlarfg; DSWAP _dswap; IDAMAX _idamax; DLAMCH _dlamch; DNRM2 _dnrm2; 

        #endregion


        #region Variables
        
        const double ZERO = 0.0E+0; const double ONE = 1.0E+0; 

        #endregion

        public DLAQP2(DLARF dlarf, DLARFG dlarfg, DSWAP dswap, IDAMAX idamax, DLAMCH dlamch, DNRM2 dnrm2)
        {
    

            #region Set Dependencies
            
            this._dlarf = dlarf; this._dlarfg = dlarfg; this._dswap = dswap; this._idamax = idamax; this._dlamch = dlamch; 
            this._dnrm2 = dnrm2;

            #endregion

        }
    
        public DLAQP2()
        {
    

            #region Dependencies (Initialization)
            
            LSAME lsame = new LSAME();
            XERBLA xerbla = new XERBLA();
            DLAMC3 dlamc3 = new DLAMC3();
            DLAPY2 dlapy2 = new DLAPY2();
            DNRM2 dnrm2 = new DNRM2();
            DSCAL dscal = new DSCAL();
            DSWAP dswap = new DSWAP();
            IDAMAX idamax = new IDAMAX();
            DGEMV dgemv = new DGEMV(lsame, xerbla);
            DGER dger = new DGER(xerbla);
            DLARF dlarf = new DLARF(dgemv, dger, lsame);
            DLAMC1 dlamc1 = new DLAMC1(dlamc3);
            DLAMC4 dlamc4 = new DLAMC4(dlamc3);
            DLAMC5 dlamc5 = new DLAMC5(dlamc3);
            DLAMC2 dlamc2 = new DLAMC2(dlamc3, dlamc1, dlamc4, dlamc5);
            DLAMCH dlamch = new DLAMCH(lsame, dlamc2);
            DLARFG dlarfg = new DLARFG(dlamch, dlapy2, dnrm2, dscal);

            #endregion


            #region Set Dependencies
            
            this._dlarf = dlarf; this._dlarfg = dlarfg; this._dswap = dswap; this._idamax = idamax; this._dlamch = dlamch; 
            this._dnrm2 = dnrm2;

            #endregion

        }
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DLAQP2 computes a QR factorization with column pivoting of
        /// the block A(OFFSET+1:M,1:N).
        /// The block A(1:OFFSET,1:N) is accordingly pivoted, but not factorized.
        /// 
        ///</summary>
        /// <param name="M">
        /// (input) INTEGER
        /// The number of rows of the matrix A. M .GE. 0.
        ///</param>
        /// <param name="N">
        /// (input) INTEGER
        /// The number of columns of the matrix A. N .GE. 0.
        ///</param>
        /// <param name="OFFSET">
        /// (input) INTEGER
        /// The number of rows of the matrix A that must be pivoted
        /// but no factorized. OFFSET .GE. 0.
        ///</param>
        /// <param name="A">
        /// (input/output) DOUBLE PRECISION array, dimension (LDA,N)
        /// On entry, the M-by-N matrix A.
        /// On exit, the upper triangle of block A(OFFSET+1:M,1:N) is 
        /// the triangular factor obtained; the elements in block
        /// A(OFFSET+1:M,1:N) below the diagonal, together with the
        /// array TAU, represent the orthogonal matrix Q as a product of
        /// elementary reflectors. Block A(1:OFFSET,1:N) has been
        /// accordingly pivoted, but no factorized.
        ///</param>
        /// <param name="LDA">
        /// (input) INTEGER
        /// The leading dimension of the array A. LDA .GE. max(1,M).
        ///</param>
        /// <param name="JPVT">
        /// (input/output) INTEGER array, dimension (N)
        /// On entry, if JPVT(i) .ne. 0, the i-th column of A is permuted
        /// to the front of A*P (a leading column); if JPVT(i) = 0,
        /// the i-th column of A is a free column.
        /// On exit, if JPVT(i) = k, then the i-th column of A*P
        /// was the k-th column of A.
        ///</param>
        /// <param name="TAU">
        /// (output) DOUBLE PRECISION array, dimension (min(M,N))
        /// The scalar factors of the elementary reflectors.
        ///</param>
        /// <param name="VN1">
        /// (input/output) DOUBLE PRECISION array, dimension (N)
        /// The vector with the partial column norms.
        ///</param>
        /// <param name="VN2">
        /// (input/output) DOUBLE PRECISION array, dimension (N)
        /// The vector with the exact column norms.
        ///</param>
        /// <param name="WORK">
        /// (workspace) DOUBLE PRECISION array, dimension (N)
        ///</param>
        public void Run(int M, int N, int OFFSET, ref double[] A, int offset_a, int LDA, ref int[] JPVT, int offset_jpvt
                         , ref double[] TAU, int offset_tau, ref double[] VN1, int offset_vn1, ref double[] VN2, int offset_vn2, ref double[] WORK, int offset_work)
        {

            #region Variables
            
            int I = 0; int ITEMP = 0; int J = 0; int MN = 0; int OFFPI = 0; int PVT = 0; double AII = 0; double TEMP = 0; 
            double TEMP2 = 0;double TOL3Z = 0; 

            #endregion


            #region Array Index Correction
            
             int o_a = -1 - LDA + offset_a;  int o_jpvt = -1 + offset_jpvt;  int o_tau = -1 + offset_tau; 
             int o_vn1 = -1 + offset_vn1; int o_vn2 = -1 + offset_vn2;  int o_work = -1 + offset_work; 

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
            // *  DLAQP2 computes a QR factorization with column pivoting of
            // *  the block A(OFFSET+1:M,1:N).
            // *  The block A(1:OFFSET,1:N) is accordingly pivoted, but not factorized.
            // *
            // *  Arguments
            // *  =========
            // *
            // *  M       (input) INTEGER
            // *          The number of rows of the matrix A. M >= 0.
            // *
            // *  N       (input) INTEGER
            // *          The number of columns of the matrix A. N >= 0.
            // *
            // *  OFFSET  (input) INTEGER
            // *          The number of rows of the matrix A that must be pivoted
            // *          but no factorized. OFFSET >= 0.
            // *
            // *  A       (input/output) DOUBLE PRECISION array, dimension (LDA,N)
            // *          On entry, the M-by-N matrix A.
            // *          On exit, the upper triangle of block A(OFFSET+1:M,1:N) is 
            // *          the triangular factor obtained; the elements in block
            // *          A(OFFSET+1:M,1:N) below the diagonal, together with the
            // *          array TAU, represent the orthogonal matrix Q as a product of
            // *          elementary reflectors. Block A(1:OFFSET,1:N) has been
            // *          accordingly pivoted, but no factorized.
            // *
            // *  LDA     (input) INTEGER
            // *          The leading dimension of the array A. LDA >= max(1,M).
            // *
            // *  JPVT    (input/output) INTEGER array, dimension (N)
            // *          On entry, if JPVT(i) .ne. 0, the i-th column of A is permuted
            // *          to the front of A*P (a leading column); if JPVT(i) = 0,
            // *          the i-th column of A is a free column.
            // *          On exit, if JPVT(i) = k, then the i-th column of A*P
            // *          was the k-th column of A.
            // *
            // *  TAU     (output) DOUBLE PRECISION array, dimension (min(M,N))
            // *          The scalar factors of the elementary reflectors.
            // *
            // *  VN1     (input/output) DOUBLE PRECISION array, dimension (N)
            // *          The vector with the partial column norms.
            // *
            // *  VN2     (input/output) DOUBLE PRECISION array, dimension (N)
            // *          The vector with the exact column norms.
            // *
            // *  WORK    (workspace) DOUBLE PRECISION array, dimension (N)
            // *
            // *  Further Details
            // *  ===============
            // *
            // *  Based on contributions by
            // *    G. Quintana-Orti, Depto. de Informatica, Universidad Jaime I, Spain
            // *    X. Sun, Computer Science Dept., Duke University, USA
            // *
            // *  Partial column norm updating strategy modified by
            // *    Z. Drmac and Z. Bujanovic, Dept. of Mathematics,
            // *    University of Zagreb, Croatia.
            // *    June 2006.
            // *  For more details see LAPACK Working Note 176.
            // *  =====================================================================
            // *
            // *     .. Parameters ..
            // *     ..
            // *     .. Local Scalars ..
            // *     ..
            // *     .. External Subroutines ..
            // *     ..
            // *     .. Intrinsic Functions ..
            //      INTRINSIC          ABS, MAX, MIN, SQRT;
            // *     ..
            // *     .. External Functions ..
            // *     ..
            // *     .. Executable Statements ..
            // *

            #endregion


            #region Body
            
            MN = Math.Min(M - OFFSET, N);
            TOL3Z = Math.Sqrt(this._dlamch.Run("Epsilon"));
            // *
            // *     Compute factorization.
            // *
            for (I = 1; I <= MN; I++)
            {
                // *
                OFFPI = OFFSET + I;
                // *
                // *        Determine ith pivot column and swap if necessary.
                // *
                PVT = (I - 1) + this._idamax.Run(N - I + 1, VN1, I + o_vn1, 1);
                // *
                if (PVT != I)
                {
                    this._dswap.Run(M, ref A, 1+PVT * LDA + o_a, 1, ref A, 1+I * LDA + o_a, 1);
                    ITEMP = JPVT[PVT + o_jpvt];
                    JPVT[PVT + o_jpvt] = JPVT[I + o_jpvt];
                    JPVT[I + o_jpvt] = ITEMP;
                    VN1[PVT + o_vn1] = VN1[I + o_vn1];
                    VN2[PVT + o_vn2] = VN2[I + o_vn2];
                }
                // *
                // *        Generate elementary reflector H(i).
                // *
                if (OFFPI < M)
                {
                    this._dlarfg.Run(M - OFFPI + 1, ref A[OFFPI+I * LDA + o_a], ref A, OFFPI + 1+I * LDA + o_a, 1, ref TAU[I + o_tau]);
                }
                else
                {
                    this._dlarfg.Run(1, ref A[M+I * LDA + o_a], ref A, M+I * LDA + o_a, 1, ref TAU[I + o_tau]);
                }
                // *
                if (I < N)
                {
                    // *
                    // *           Apply H(i)' to A(offset+i:m,i+1:n) from the left.
                    // *
                    AII = A[OFFPI+I * LDA + o_a];
                    A[OFFPI+I * LDA + o_a] = ONE;
                    this._dlarf.Run("Left", M - OFFPI + 1, N - I, A, OFFPI+I * LDA + o_a, 1, TAU[I + o_tau]
                                    , ref A, OFFPI+(I + 1) * LDA + o_a, LDA, ref WORK, 1 + o_work);
                    A[OFFPI+I * LDA + o_a] = AII;
                }
                // *
                // *        Update partial column norms.
                // *
                for (J = I + 1; J <= N; J++)
                {
                    if (VN1[J + o_vn1] != ZERO)
                    {
                        // *
                        // *              NOTE: The following 4 lines follow from the analysis in
                        // *              Lapack Working Note 176.
                        // *
                        TEMP = ONE - Math.Pow(Math.Abs(A[OFFPI+J * LDA + o_a]) / VN1[J + o_vn1],2);
                        TEMP = Math.Max(TEMP, ZERO);
                        TEMP2 = TEMP * Math.Pow(VN1[J + o_vn1] / VN2[J + o_vn2],2);
                        if (TEMP2 <= TOL3Z)
                        {
                            if (OFFPI < M)
                            {
                                VN1[J + o_vn1] = this._dnrm2.Run(M - OFFPI, A, OFFPI + 1+J * LDA + o_a, 1);
                                VN2[J + o_vn2] = VN1[J + o_vn1];
                            }
                            else
                            {
                                VN1[J + o_vn1] = ZERO;
                                VN2[J + o_vn2] = ZERO;
                            }
                        }
                        else
                        {
                            VN1[J + o_vn1] *= Math.Sqrt(TEMP);
                        }
                    }
                }
                // *
            }
            // *
            return;
            // *
            // *     End of DLAQP2
            // *

            #endregion

        }
    }
}
