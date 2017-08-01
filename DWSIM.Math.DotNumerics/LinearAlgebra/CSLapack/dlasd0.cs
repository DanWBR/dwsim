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
    /// Using a divide and conquer approach, DLASD0 computes the singular
    /// value decomposition (SVD) of a real upper bidiagonal N-by-M
    /// matrix B with diagonal D and offdiagonal E, where M = N + SQRE.
    /// The algorithm computes orthogonal matrices U and VT such that
    /// B = U * S * VT. The singular values S are overwritten on D.
    /// 
    /// A related subroutine, DLASDA, computes only the singular values,
    /// and optionally, the singular vectors in compact form.
    /// 
    ///</summary>
    public class DLASD0
    {
    

        #region Dependencies
        
        DLASD1 _dlasd1; DLASDQ _dlasdq; DLASDT _dlasdt; XERBLA _xerbla; 

        #endregion

        public DLASD0(DLASD1 dlasd1, DLASDQ dlasdq, DLASDT dlasdt, XERBLA xerbla)
        {
    

            #region Set Dependencies
            
            this._dlasd1 = dlasd1; this._dlasdq = dlasdq; this._dlasdt = dlasdt; this._xerbla = xerbla; 

            #endregion

        }
    
        public DLASD0()
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
            DLAS2 dlas2 = new DLAS2();
            DLASQ5 dlasq5 = new DLASQ5();
            DLAZQ4 dlazq4 = new DLAZQ4();
            IEEECK ieeeck = new IEEECK();
            IPARMQ iparmq = new IPARMQ();
            DSCAL dscal = new DSCAL();
            DSWAP dswap = new DSWAP();
            DLASDT dlasdt = new DLASDT();
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
            DLASD1 dlasd1 = new DLASD1(dlamrg, dlascl, dlasd2, dlasd3, xerbla);
            DLARTG dlartg = new DLARTG(dlamch);
            DLASQ6 dlasq6 = new DLASQ6(dlamch);
            DLAZQ3 dlazq3 = new DLAZQ3(dlasq5, dlasq6, dlazq4, dlamch);
            DLASRT dlasrt = new DLASRT(lsame, xerbla);
            ILAENV ilaenv = new ILAENV(ieeeck, iparmq);
            DLASQ2 dlasq2 = new DLASQ2(dlazq3, dlasrt, xerbla, dlamch, ilaenv);
            DLASQ1 dlasq1 = new DLASQ1(dcopy, dlas2, dlascl, dlasq2, dlasrt, xerbla, dlamch);
            DLASR dlasr = new DLASR(lsame, xerbla);
            DLASV2 dlasv2 = new DLASV2(dlamch);
            DBDSQR dbdsqr = new DBDSQR(lsame, dlamch, dlartg, dlas2, dlasq1, dlasr, dlasv2, drot, dscal, dswap
                                       , xerbla);
            DLASDQ dlasdq = new DLASDQ(dbdsqr, dlartg, dlasr, dswap, xerbla, lsame);

            #endregion


            #region Set Dependencies
            
            this._dlasd1 = dlasd1; this._dlasdq = dlasdq; this._dlasdt = dlasdt; this._xerbla = xerbla; 

            #endregion

        }
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// Using a divide and conquer approach, DLASD0 computes the singular
        /// value decomposition (SVD) of a real upper bidiagonal N-by-M
        /// matrix B with diagonal D and offdiagonal E, where M = N + SQRE.
        /// The algorithm computes orthogonal matrices U and VT such that
        /// B = U * S * VT. The singular values S are overwritten on D.
        /// 
        /// A related subroutine, DLASDA, computes only the singular values,
        /// and optionally, the singular vectors in compact form.
        /// 
        ///</summary>
        /// <param name="N">
        /// (input) INTEGER
        /// On entry, the row dimension of the upper bidiagonal matrix.
        /// This is also the dimension of the main diagonal array D.
        ///</param>
        /// <param name="SQRE">
        /// (input) INTEGER
        /// Specifies the column dimension of the bidiagonal matrix.
        /// = 0: The bidiagonal matrix has column dimension M = N;
        /// = 1: The bidiagonal matrix has column dimension M = N+1;
        ///</param>
        /// <param name="D">
        /// (input/output) DOUBLE PRECISION array, dimension (N)
        /// On entry D contains the main diagonal of the bidiagonal
        /// matrix.
        /// On exit D, if INFO = 0, contains its singular values.
        ///</param>
        /// <param name="E">
        /// (input) DOUBLE PRECISION array, dimension (M-1)
        /// Contains the subdiagonal entries of the bidiagonal matrix.
        /// On exit, E has been destroyed.
        ///</param>
        /// <param name="U">
        /// (output) DOUBLE PRECISION array, dimension at least (LDQ, N)
        /// On exit, U contains the left singular vectors.
        ///</param>
        /// <param name="LDU">
        /// (input) INTEGER
        /// On entry, leading dimension of U.
        ///</param>
        /// <param name="VT">
        /// (output) DOUBLE PRECISION array, dimension at least (LDVT, M)
        /// On exit, VT' contains the right singular vectors.
        ///</param>
        /// <param name="LDVT">
        /// (input) INTEGER
        /// On entry, leading dimension of VT.
        ///</param>
        /// <param name="SMLSIZ">
        /// (input) INTEGER
        /// On entry, maximum size of the subproblems at the
        /// bottom of the computation tree.
        ///</param>
        /// <param name="IWORK">
        /// (workspace) INTEGER work array.
        /// Dimension must be at least (8 * N)
        ///</param>
        /// <param name="WORK">
        /// (workspace) DOUBLE PRECISION work array.
        /// Dimension must be at least (3 * M**2 + 2 * M)
        ///</param>
        /// <param name="INFO">
        /// (output) INTEGER
        /// = 0:  successful exit.
        /// .LT. 0:  if INFO = -i, the i-th argument had an illegal value.
        /// .GT. 0:  if INFO = 1, an singular value did not converge
        ///</param>
        public void Run(int N, int SQRE, ref double[] D, int offset_d, ref double[] E, int offset_e, ref double[] U, int offset_u, int LDU
                         , ref double[] VT, int offset_vt, int LDVT, int SMLSIZ, ref int[] IWORK, int offset_iwork, ref double[] WORK, int offset_work, ref int INFO)
        {

            #region Variables
            
            int I = 0; int I1 = 0; int IC = 0; int IDXQ = 0; int IDXQC = 0; int IM1 = 0; int INODE = 0; int ITEMP = 0; 
            int IWK = 0;int J = 0; int LF = 0; int LL = 0; int LVL = 0; int M = 0; int NCC = 0; int ND = 0; int NDB1 = 0; 
            int NDIML = 0;int NDIMR = 0; int NL = 0; int NLF = 0; int NLP1 = 0; int NLVL = 0; int NR = 0; int NRF = 0; 
            int NRP1 = 0;int SQREI = 0; double ALPHA = 0; double BETA = 0; 

            #endregion


            #region Array Index Correction
            
             int o_d = -1 + offset_d;  int o_e = -1 + offset_e;  int o_u = -1 - LDU + offset_u;  int o_vt = -1 - LDVT + offset_vt; 
             int o_iwork = -1 + offset_iwork; int o_work = -1 + offset_work; 

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
            // *  Using a divide and conquer approach, DLASD0 computes the singular
            // *  value decomposition (SVD) of a real upper bidiagonal N-by-M
            // *  matrix B with diagonal D and offdiagonal E, where M = N + SQRE.
            // *  The algorithm computes orthogonal matrices U and VT such that
            // *  B = U * S * VT. The singular values S are overwritten on D.
            // *
            // *  A related subroutine, DLASDA, computes only the singular values,
            // *  and optionally, the singular vectors in compact form.
            // *
            // *  Arguments
            // *  =========
            // *
            // *  N      (input) INTEGER
            // *         On entry, the row dimension of the upper bidiagonal matrix.
            // *         This is also the dimension of the main diagonal array D.
            // *
            // *  SQRE   (input) INTEGER
            // *         Specifies the column dimension of the bidiagonal matrix.
            // *         = 0: The bidiagonal matrix has column dimension M = N;
            // *         = 1: The bidiagonal matrix has column dimension M = N+1;
            // *
            // *  D      (input/output) DOUBLE PRECISION array, dimension (N)
            // *         On entry D contains the main diagonal of the bidiagonal
            // *         matrix.
            // *         On exit D, if INFO = 0, contains its singular values.
            // *
            // *  E      (input) DOUBLE PRECISION array, dimension (M-1)
            // *         Contains the subdiagonal entries of the bidiagonal matrix.
            // *         On exit, E has been destroyed.
            // *
            // *  U      (output) DOUBLE PRECISION array, dimension at least (LDQ, N)
            // *         On exit, U contains the left singular vectors.
            // *
            // *  LDU    (input) INTEGER
            // *         On entry, leading dimension of U.
            // *
            // *  VT     (output) DOUBLE PRECISION array, dimension at least (LDVT, M)
            // *         On exit, VT' contains the right singular vectors.
            // *
            // *  LDVT   (input) INTEGER
            // *         On entry, leading dimension of VT.
            // *
            // *  SMLSIZ (input) INTEGER
            // *         On entry, maximum size of the subproblems at the
            // *         bottom of the computation tree.
            // *
            // *  IWORK  (workspace) INTEGER work array.
            // *         Dimension must be at least (8 * N)
            // *
            // *  WORK   (workspace) DOUBLE PRECISION work array.
            // *         Dimension must be at least (3 * M**2 + 2 * M)
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
            // *     .. Local Scalars ..
            // *     ..
            // *     .. External Subroutines ..
            // *     ..
            // *     .. Executable Statements ..
            // *
            // *     Test the input parameters.
            // *

            #endregion


            #region Body
            
            INFO = 0;
            // *
            if (N < 0)
            {
                INFO =  - 1;
            }
            else
            {
                if ((SQRE < 0) || (SQRE > 1))
                {
                    INFO =  - 2;
                }
            }
            // *
            M = N + SQRE;
            // *
            if (LDU < N)
            {
                INFO =  - 6;
            }
            else
            {
                if (LDVT < M)
                {
                    INFO =  - 8;
                }
                else
                {
                    if (SMLSIZ < 3)
                    {
                        INFO =  - 9;
                    }
                }
            }
            if (INFO != 0)
            {
                this._xerbla.Run("DLASD0",  - INFO);
                return;
            }
            // *
            // *     If the input matrix is too small, call DLASDQ to find the SVD.
            // *
            if (N <= SMLSIZ)
            {
                this._dlasdq.Run("U", SQRE, N, M, N, 0
                                 , ref D, offset_d, ref E, offset_e, ref VT, offset_vt, LDVT, ref U, offset_u, LDU
                                 , ref U, offset_u, LDU, ref WORK, offset_work, ref INFO);
                return;
            }
            // *
            // *     Set up the computation tree.
            // *
            INODE = 1;
            NDIML = INODE + N;
            NDIMR = NDIML + N;
            IDXQ = NDIMR + N;
            IWK = IDXQ + N;
            this._dlasdt.Run(N, ref NLVL, ref ND, ref IWORK, INODE + o_iwork, ref IWORK, NDIML + o_iwork, ref IWORK, NDIMR + o_iwork
                             , SMLSIZ);
            // *
            // *     For the nodes on bottom level of the tree, solve
            // *     their subproblems by DLASDQ.
            // *
            NDB1 = (ND + 1) / 2;
            NCC = 0;
            for (I = NDB1; I <= ND; I++)
            {
                // *
                // *     IC : center row of each node
                // *     NL : number of rows of left  subproblem
                // *     NR : number of rows of right subproblem
                // *     NLF: starting row of the left   subproblem
                // *     NRF: starting row of the right  subproblem
                // *
                I1 = I - 1;
                IC = IWORK[INODE + I1 + o_iwork];
                NL = IWORK[NDIML + I1 + o_iwork];
                NLP1 = NL + 1;
                NR = IWORK[NDIMR + I1 + o_iwork];
                NRP1 = NR + 1;
                NLF = IC - NL;
                NRF = IC + 1;
                SQREI = 1;
                this._dlasdq.Run("U", SQREI, NL, NLP1, NL, NCC
                                 , ref D, NLF + o_d, ref E, NLF + o_e, ref VT, NLF+NLF * LDVT + o_vt, LDVT, ref U, NLF+NLF * LDU + o_u, LDU
                                 , ref U, NLF+NLF * LDU + o_u, LDU, ref WORK, offset_work, ref INFO);
                if (INFO != 0)
                {
                    return;
                }
                ITEMP = IDXQ + NLF - 2;
                for (J = 1; J <= NL; J++)
                {
                    IWORK[ITEMP + J + o_iwork] = J;
                }
                if (I == ND)
                {
                    SQREI = SQRE;
                }
                else
                {
                    SQREI = 1;
                }
                NRP1 = NR + SQREI;
                this._dlasdq.Run("U", SQREI, NR, NRP1, NR, NCC
                                 , ref D, NRF + o_d, ref E, NRF + o_e, ref VT, NRF+NRF * LDVT + o_vt, LDVT, ref U, NRF+NRF * LDU + o_u, LDU
                                 , ref U, NRF+NRF * LDU + o_u, LDU, ref WORK, offset_work, ref INFO);
                if (INFO != 0)
                {
                    return;
                }
                ITEMP = IDXQ + IC;
                for (J = 1; J <= NR; J++)
                {
                    IWORK[ITEMP + J - 1 + o_iwork] = J;
                }
            }
            // *
            // *     Now conquer each subproblem bottom-up.
            // *
            for (LVL = NLVL; LVL >= 1; LVL +=  - 1)
            {
                // *
                // *        Find the first node LF and last node LL on the
                // *        current level LVL.
                // *
                if (LVL == 1)
                {
                    LF = 1;
                    LL = 1;
                }
                else
                {
                    LF = (int)Math.Pow(2, LVL - 1);
                    LL = 2 * LF - 1;
                }
                for (I = LF; I <= LL; I++)
                {
                    IM1 = I - 1;
                    IC = IWORK[INODE + IM1 + o_iwork];
                    NL = IWORK[NDIML + IM1 + o_iwork];
                    NR = IWORK[NDIMR + IM1 + o_iwork];
                    NLF = IC - NL;
                    if ((SQRE == 0) && (I == LL))
                    {
                        SQREI = SQRE;
                    }
                    else
                    {
                        SQREI = 1;
                    }
                    IDXQC = IDXQ + NLF - 1;
                    ALPHA = D[IC + o_d];
                    BETA = E[IC + o_e];
                    this._dlasd1.Run(NL, NR, SQREI, ref D, NLF + o_d, ref ALPHA, ref BETA
                                     , ref U, NLF+NLF * LDU + o_u, LDU, ref VT, NLF+NLF * LDVT + o_vt, LDVT, ref IWORK, IDXQC + o_iwork, ref IWORK, IWK + o_iwork
                                     , ref WORK, offset_work, ref INFO);
                    if (INFO != 0)
                    {
                        return;
                    }
                }
            }
            // *
            return;
            // *
            // *     End of DLASD0
            // *

            #endregion

        }
    }
}
