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
    /// DLASD1 computes the SVD of an upper bidiagonal N-by-M matrix B,
    /// where N = NL + NR + 1 and M = N + SQRE. DLASD1 is called from DLASD0.
    /// 
    /// A related subroutine DLASD7 handles the case in which the singular
    /// values (and the singular vectors in factored form) are desired.
    /// 
    /// DLASD1 computes the SVD as follows:
    /// 
    /// ( D1(in)  0    0     0 )
    /// B = U(in) * (   Z1'   a   Z2'    b ) * VT(in)
    /// (   0     0   D2(in) 0 )
    /// 
    /// = U(out) * ( D(out) 0) * VT(out)
    /// 
    /// where Z' = (Z1' a Z2' b) = u' VT', and u is a vector of dimension M
    /// with ALPHA and BETA in the NL+1 and NL+2 th entries and zeros
    /// elsewhere; and the entry b is empty if SQRE = 0.
    /// 
    /// The left singular vectors of the original matrix are stored in U, and
    /// the transpose of the right singular vectors are stored in VT, and the
    /// singular values are in D.  The algorithm consists of three stages:
    /// 
    /// The first stage consists of deflating the size of the problem
    /// when there are multiple singular values or when there are zeros in
    /// the Z vector.  For each such occurence the dimension of the
    /// secular equation problem is reduced by one.  This stage is
    /// performed by the routine DLASD2.
    /// 
    /// The second stage consists of calculating the updated
    /// singular values. This is done by finding the square roots of the
    /// roots of the secular equation via the routine DLASD4 (as called
    /// by DLASD3). This routine also calculates the singular vectors of
    /// the current problem.
    /// 
    /// The final stage consists of computing the updated singular vectors
    /// directly using the updated singular values.  The singular vectors
    /// for the current problem are multiplied with the singular vectors
    /// from the overall problem.
    /// 
    ///</summary>
    public class DLASD1
    {
    

        #region Dependencies
        
        DLAMRG _dlamrg; DLASCL _dlascl; DLASD2 _dlasd2; DLASD3 _dlasd3; XERBLA _xerbla; 

        #endregion


        #region Variables
        
        const double ONE = 1.0E+0; const double ZERO = 0.0E+0; 

        #endregion

        public DLASD1(DLAMRG dlamrg, DLASCL dlascl, DLASD2 dlasd2, DLASD3 dlasd3, XERBLA xerbla)
        {
    

            #region Set Dependencies
            
            this._dlamrg = dlamrg; this._dlascl = dlascl; this._dlasd2 = dlasd2; this._dlasd3 = dlasd3; this._xerbla = xerbla; 

            #endregion

        }
    
        public DLASD1()
        {
    

            #region Dependencies (Initialization)
            
            DLAMRG dlamrg = new DLAMRG();
            LSAME lsame = new LSAME();
            DLAMC3 dlamc3 = new DLAMC3();
            XERBLA xerbla = new XERBLA();
            DLAPY2 dlapy2 = new DLAPY2();
            DCOPY dcopy = new DCOPY();
            DROT drot = new DROT();
            DNRM2 dnrm2 = new DNRM2();
            DLASD5 dlasd5 = new DLASD5();
            DLAMC1 dlamc1 = new DLAMC1(dlamc3);
            DLAMC4 dlamc4 = new DLAMC4(dlamc3);
            DLAMC5 dlamc5 = new DLAMC5(dlamc3);
            DLAMC2 dlamc2 = new DLAMC2(dlamc3, dlamc1, dlamc4, dlamc5);
            DLAMCH dlamch = new DLAMCH(lsame, dlamc2);
            DLASCL dlascl = new DLASCL(lsame, dlamch, xerbla);
            DLACPY dlacpy = new DLACPY(lsame);
            DLASET dlaset = new DLASET(lsame);
            DLASD2 dlasd2 = new DLASD2(dlamch, dlapy2, dcopy, dlacpy, dlamrg, dlaset, drot, xerbla);
            DGEMM dgemm = new DGEMM(lsame, xerbla);
            DLAED6 dlaed6 = new DLAED6(dlamch);
            DLASD4 dlasd4 = new DLASD4(dlaed6, dlasd5, dlamch);
            DLASD3 dlasd3 = new DLASD3(dlamc3, dnrm2, dcopy, dgemm, dlacpy, dlascl, dlasd4, xerbla);

            #endregion


            #region Set Dependencies
            
            this._dlamrg = dlamrg; this._dlascl = dlascl; this._dlasd2 = dlasd2; this._dlasd3 = dlasd3; this._xerbla = xerbla; 

            #endregion

        }
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DLASD1 computes the SVD of an upper bidiagonal N-by-M matrix B,
        /// where N = NL + NR + 1 and M = N + SQRE. DLASD1 is called from DLASD0.
        /// 
        /// A related subroutine DLASD7 handles the case in which the singular
        /// values (and the singular vectors in factored form) are desired.
        /// 
        /// DLASD1 computes the SVD as follows:
        /// 
        /// ( D1(in)  0    0     0 )
        /// B = U(in) * (   Z1'   a   Z2'    b ) * VT(in)
        /// (   0     0   D2(in) 0 )
        /// 
        /// = U(out) * ( D(out) 0) * VT(out)
        /// 
        /// where Z' = (Z1' a Z2' b) = u' VT', and u is a vector of dimension M
        /// with ALPHA and BETA in the NL+1 and NL+2 th entries and zeros
        /// elsewhere; and the entry b is empty if SQRE = 0.
        /// 
        /// The left singular vectors of the original matrix are stored in U, and
        /// the transpose of the right singular vectors are stored in VT, and the
        /// singular values are in D.  The algorithm consists of three stages:
        /// 
        /// The first stage consists of deflating the size of the problem
        /// when there are multiple singular values or when there are zeros in
        /// the Z vector.  For each such occurence the dimension of the
        /// secular equation problem is reduced by one.  This stage is
        /// performed by the routine DLASD2.
        /// 
        /// The second stage consists of calculating the updated
        /// singular values. This is done by finding the square roots of the
        /// roots of the secular equation via the routine DLASD4 (as called
        /// by DLASD3). This routine also calculates the singular vectors of
        /// the current problem.
        /// 
        /// The final stage consists of computing the updated singular vectors
        /// directly using the updated singular values.  The singular vectors
        /// for the current problem are multiplied with the singular vectors
        /// from the overall problem.
        /// 
        ///</summary>
        /// <param name="NL">
        /// (input) INTEGER
        /// The row dimension of the upper block.  NL .GE. 1.
        ///</param>
        /// <param name="NR">
        /// (input) INTEGER
        /// The row dimension of the lower block.  NR .GE. 1.
        ///</param>
        /// <param name="SQRE">
        /// (input) INTEGER
        /// = 0: the lower block is an NR-by-NR square matrix.
        /// = 1: the lower block is an NR-by-(NR+1) rectangular matrix.
        /// 
        /// The bidiagonal matrix has row dimension N = NL + NR + 1,
        /// and column dimension M = N + SQRE.
        ///</param>
        /// <param name="D">
        /// (input/output) DOUBLE PRECISION array,
        /// dimension (N = NL+NR+1).
        /// On entry D(1:NL,1:NL) contains the singular values of the
        /// upper block; and D(NL+2:N) contains the singular values of
        /// the lower block. On exit D(1:N) contains the singular values
        /// of the modified matrix.
        ///</param>
        /// <param name="ALPHA">
        /// (input/output) DOUBLE PRECISION
        /// Contains the diagonal element associated with the added row.
        ///</param>
        /// <param name="BETA">
        /// (input/output) DOUBLE PRECISION
        /// Contains the off-diagonal element associated with the added
        /// row.
        ///</param>
        /// <param name="U">
        /// (input/output) DOUBLE PRECISION array, dimension(LDU,N)
        /// On entry U(1:NL, 1:NL) contains the left singular vectors of
        /// the upper block; U(NL+2:N, NL+2:N) contains the left singular
        /// vectors of the lower block. On exit U contains the left
        /// singular vectors of the bidiagonal matrix.
        ///</param>
        /// <param name="LDU">
        /// (input) INTEGER
        /// The leading dimension of the array U.  LDU .GE. max( 1, N ).
        ///</param>
        /// <param name="VT">
        /// (input/output) DOUBLE PRECISION array, dimension(LDVT,M)
        /// where M = N + SQRE.
        /// On entry VT(1:NL+1, 1:NL+1)' contains the right singular
        /// vectors of the upper block; VT(NL+2:M, NL+2:M)' contains
        /// the right singular vectors of the lower block. On exit
        /// VT' contains the right singular vectors of the
        /// bidiagonal matrix.
        ///</param>
        /// <param name="LDVT">
        /// (input) INTEGER
        /// The leading dimension of the array VT.  LDVT .GE. max( 1, M ).
        ///</param>
        /// <param name="IDXQ">
        /// (output) INTEGER array, dimension(N)
        /// This contains the permutation which will reintegrate the
        /// subproblem just solved back into sorted order, i.e.
        /// D( IDXQ( I = 1, N ) ) will be in ascending order.
        ///</param>
        /// <param name="IWORK">
        /// (workspace) INTEGER array, dimension( 4 * N )
        ///</param>
        /// <param name="WORK">
        /// (workspace) DOUBLE PRECISION array, dimension( 3*M**2 + 2*M )
        ///</param>
        /// <param name="INFO">
        /// (output) INTEGER
        /// = 0:  successful exit.
        /// .LT. 0:  if INFO = -i, the i-th argument had an illegal value.
        /// .GT. 0:  if INFO = 1, an singular value did not converge
        ///</param>
        public void Run(int NL, int NR, int SQRE, ref double[] D, int offset_d, ref double ALPHA, ref double BETA
                         , ref double[] U, int offset_u, int LDU, ref double[] VT, int offset_vt, int LDVT, ref int[] IDXQ, int offset_idxq, ref int[] IWORK, int offset_iwork
                         , ref double[] WORK, int offset_work, ref int INFO)
        {

            #region Variables
            
            int COLTYP = 0; int I = 0; int IDX = 0; int IDXC = 0; int IDXP = 0; int IQ = 0; int ISIGMA = 0; int IU2 = 0; 
            int IVT2 = 0;int IZ = 0; int K = 0; int LDQ = 0; int LDU2 = 0; int LDVT2 = 0; int M = 0; int N = 0; int N1 = 0; 
            int N2 = 0;double ORGNRM = 0; 

            #endregion


            #region Array Index Correction
            
             int o_d = -1 + offset_d;  int o_u = -1 - LDU + offset_u;  int o_vt = -1 - LDVT + offset_vt; 
             int o_idxq = -1 + offset_idxq; int o_iwork = -1 + offset_iwork;  int o_work = -1 + offset_work; 

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
            // *  DLASD1 computes the SVD of an upper bidiagonal N-by-M matrix B,
            // *  where N = NL + NR + 1 and M = N + SQRE. DLASD1 is called from DLASD0.
            // *
            // *  A related subroutine DLASD7 handles the case in which the singular
            // *  values (and the singular vectors in factored form) are desired.
            // *
            // *  DLASD1 computes the SVD as follows:
            // *
            // *                ( D1(in)  0    0     0 )
            // *    B = U(in) * (   Z1'   a   Z2'    b ) * VT(in)
            // *                (   0     0   D2(in) 0 )
            // *
            // *      = U(out) * ( D(out) 0) * VT(out)
            // *
            // *  where Z' = (Z1' a Z2' b) = u' VT', and u is a vector of dimension M
            // *  with ALPHA and BETA in the NL+1 and NL+2 th entries and zeros
            // *  elsewhere; and the entry b is empty if SQRE = 0.
            // *
            // *  The left singular vectors of the original matrix are stored in U, and
            // *  the transpose of the right singular vectors are stored in VT, and the
            // *  singular values are in D.  The algorithm consists of three stages:
            // *
            // *     The first stage consists of deflating the size of the problem
            // *     when there are multiple singular values or when there are zeros in
            // *     the Z vector.  For each such occurence the dimension of the
            // *     secular equation problem is reduced by one.  This stage is
            // *     performed by the routine DLASD2.
            // *
            // *     The second stage consists of calculating the updated
            // *     singular values. This is done by finding the square roots of the
            // *     roots of the secular equation via the routine DLASD4 (as called
            // *     by DLASD3). This routine also calculates the singular vectors of
            // *     the current problem.
            // *
            // *     The final stage consists of computing the updated singular vectors
            // *     directly using the updated singular values.  The singular vectors
            // *     for the current problem are multiplied with the singular vectors
            // *     from the overall problem.
            // *
            // *  Arguments
            // *  =========
            // *
            // *  NL     (input) INTEGER
            // *         The row dimension of the upper block.  NL >= 1.
            // *
            // *  NR     (input) INTEGER
            // *         The row dimension of the lower block.  NR >= 1.
            // *
            // *  SQRE   (input) INTEGER
            // *         = 0: the lower block is an NR-by-NR square matrix.
            // *         = 1: the lower block is an NR-by-(NR+1) rectangular matrix.
            // *
            // *         The bidiagonal matrix has row dimension N = NL + NR + 1,
            // *         and column dimension M = N + SQRE.
            // *
            // *  D      (input/output) DOUBLE PRECISION array,
            // *                        dimension (N = NL+NR+1).
            // *         On entry D(1:NL,1:NL) contains the singular values of the
            // *         upper block; and D(NL+2:N) contains the singular values of
            // *         the lower block. On exit D(1:N) contains the singular values
            // *         of the modified matrix.
            // *
            // *  ALPHA  (input/output) DOUBLE PRECISION
            // *         Contains the diagonal element associated with the added row.
            // *
            // *  BETA   (input/output) DOUBLE PRECISION
            // *         Contains the off-diagonal element associated with the added
            // *         row.
            // *
            // *  U      (input/output) DOUBLE PRECISION array, dimension(LDU,N)
            // *         On entry U(1:NL, 1:NL) contains the left singular vectors of
            // *         the upper block; U(NL+2:N, NL+2:N) contains the left singular
            // *         vectors of the lower block. On exit U contains the left
            // *         singular vectors of the bidiagonal matrix.
            // *
            // *  LDU    (input) INTEGER
            // *         The leading dimension of the array U.  LDU >= max( 1, N ).
            // *
            // *  VT     (input/output) DOUBLE PRECISION array, dimension(LDVT,M)
            // *         where M = N + SQRE.
            // *         On entry VT(1:NL+1, 1:NL+1)' contains the right singular
            // *         vectors of the upper block; VT(NL+2:M, NL+2:M)' contains
            // *         the right singular vectors of the lower block. On exit
            // *         VT' contains the right singular vectors of the
            // *         bidiagonal matrix.
            // *
            // *  LDVT   (input) INTEGER
            // *         The leading dimension of the array VT.  LDVT >= max( 1, M ).
            // *
            // *  IDXQ  (output) INTEGER array, dimension(N)
            // *         This contains the permutation which will reintegrate the
            // *         subproblem just solved back into sorted order, i.e.
            // *         D( IDXQ( I = 1, N ) ) will be in ascending order.
            // *
            // *  IWORK  (workspace) INTEGER array, dimension( 4 * N )
            // *
            // *  WORK   (workspace) DOUBLE PRECISION array, dimension( 3*M**2 + 2*M )
            // *
            // *  INFO   (output) INTEGER
            // *          = 0:  successful exit.
            // *          < 0:  if INFO = -i, the i-th argument had an illegal value.
            // *          > 0:  if INFO = 1, an singular value did not converge
            // *
            // *  Further Details
            // *  ===============
            // *
            // *  Based on contributions by
            // *     Ming Gu and Huan Ren, Computer Science Division, University of
            // *     California at Berkeley, USA
            // *
            // *  =====================================================================
            // *
            // *     .. Parameters ..
            // *
            // *     ..
            // *     .. Local Scalars ..
            // *     ..
            // *     .. External Subroutines ..
            // *     ..
            // *     .. Intrinsic Functions ..
            //      INTRINSIC          ABS, MAX;
            // *     ..
            // *     .. Executable Statements ..
            // *
            // *     Test the input parameters.
            // *

            #endregion


            #region Body
            
            INFO = 0;
            // *
            if (NL < 1)
            {
                INFO =  - 1;
            }
            else
            {
                if (NR < 1)
                {
                    INFO =  - 2;
                }
                else
                {
                    if ((SQRE < 0) || (SQRE > 1))
                    {
                        INFO =  - 3;
                    }
                }
            }
            if (INFO != 0)
            {
                this._xerbla.Run("DLASD1",  - INFO);
                return;
            }
            // *
            N = NL + NR + 1;
            M = N + SQRE;
            // *
            // *     The following values are for bookkeeping purposes only.  They are
            // *     integer pointers which indicate the portion of the workspace
            // *     used by a particular array in DLASD2 and DLASD3.
            // *
            LDU2 = N;
            LDVT2 = M;
            // *
            IZ = 1;
            ISIGMA = IZ + M;
            IU2 = ISIGMA + N;
            IVT2 = IU2 + LDU2 * N;
            IQ = IVT2 + LDVT2 * M;
            // *
            IDX = 1;
            IDXC = IDX + N;
            COLTYP = IDXC + N;
            IDXP = COLTYP + N;
            // *
            // *     Scale.
            // *
            ORGNRM = Math.Max(Math.Abs(ALPHA), Math.Abs(BETA));
            D[NL + 1 + o_d] = ZERO;
            for (I = 1; I <= N; I++)
            {
                if (Math.Abs(D[I + o_d]) > ORGNRM)
                {
                    ORGNRM = Math.Abs(D[I + o_d]);
                }
            }
            this._dlascl.Run("G", 0, 0, ORGNRM, ONE, N
                             , 1, ref D, offset_d, N, ref INFO);
            ALPHA /= ORGNRM;
            BETA /= ORGNRM;
            // *
            // *     Deflate singular values.
            // *
            this._dlasd2.Run(NL, NR, SQRE, ref K, ref D, offset_d, ref WORK, IZ + o_work
                             , ALPHA, BETA, ref U, offset_u, LDU, ref VT, offset_vt, LDVT
                             , ref WORK, ISIGMA + o_work, ref WORK, IU2 + o_work, LDU2, ref WORK, IVT2 + o_work, LDVT2, ref IWORK, IDXP + o_iwork
                             , ref IWORK, IDX + o_iwork, ref IWORK, IDXC + o_iwork, ref IDXQ, offset_idxq, ref IWORK, COLTYP + o_iwork, ref INFO);
            // *
            // *     Solve Secular Equation and update singular vectors.
            // *
            LDQ = K;
            this._dlasd3.Run(NL, NR, SQRE, K, ref D, offset_d, ref WORK, IQ + o_work
                             , LDQ, ref WORK, ISIGMA + o_work, ref U, offset_u, LDU, WORK, IU2 + o_work, LDU2
                             , ref VT, offset_vt, LDVT, ref WORK, IVT2 + o_work, LDVT2, IWORK, IDXC + o_iwork, IWORK, COLTYP + o_iwork
                             , ref WORK, IZ + o_work, ref INFO);
            if (INFO != 0)
            {
                return;
            }
            // *
            // *     Unscale.
            // *
            this._dlascl.Run("G", 0, 0, ONE, ORGNRM, N
                             , 1, ref D, offset_d, N, ref INFO);
            // *
            // *     Prepare the IDXQ sorting permutation.
            // *
            N1 = K;
            N2 = N - K;
            this._dlamrg.Run(N1, N2, D, offset_d, 1,  - 1, ref IDXQ, offset_idxq);
            // *
            return;
            // *
            // *     End of DLASD1
            // *

            #endregion

        }
    }
}
