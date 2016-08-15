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
    /// Using a divide and conquer approach, DLASDA computes the singular
    /// value decomposition (SVD) of a real upper bidiagonal N-by-M matrix
    /// B with diagonal D and offdiagonal E, where M = N + SQRE. The
    /// algorithm computes the singular values in the SVD B = U * S * VT.
    /// The orthogonal matrices U and VT are optionally computed in
    /// compact form.
    /// 
    /// A related subroutine, DLASD0, computes the singular values and
    /// the singular vectors in explicit form.
    /// 
    ///</summary>
    public class DLASDA
    {
    

        #region Dependencies
        
        DCOPY _dcopy; DLASD6 _dlasd6; DLASDQ _dlasdq; DLASDT _dlasdt; DLASET _dlaset; XERBLA _xerbla; 

        #endregion


        #region Variables
        
        const double ZERO = 0.0E+0; const double ONE = 1.0E+0; 

        #endregion

        public DLASDA(DCOPY dcopy, DLASD6 dlasd6, DLASDQ dlasdq, DLASDT dlasdt, DLASET dlaset, XERBLA xerbla)
        {
    

            #region Set Dependencies
            
            this._dcopy = dcopy; this._dlasd6 = dlasd6; this._dlasdq = dlasdq; this._dlasdt = dlasdt; this._dlaset = dlaset; 
            this._xerbla = xerbla;

            #endregion

        }
    
        public DLASDA()
        {
    

            #region Dependencies (Initialization)
            
            DCOPY dcopy = new DCOPY();
            DLAMRG dlamrg = new DLAMRG();
            LSAME lsame = new LSAME();
            DLAMC3 dlamc3 = new DLAMC3();
            XERBLA xerbla = new XERBLA();
            DROT drot = new DROT();
            DLAPY2 dlapy2 = new DLAPY2();
            DLASD5 dlasd5 = new DLASD5();
            DDOT ddot = new DDOT();
            DNRM2 dnrm2 = new DNRM2();
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
            DLASD7 dlasd7 = new DLASD7(dcopy, dlamrg, drot, xerbla, dlamch, dlapy2);
            DLAED6 dlaed6 = new DLAED6(dlamch);
            DLASD4 dlasd4 = new DLASD4(dlaed6, dlasd5, dlamch);
            DLASET dlaset = new DLASET(lsame);
            DLASD8 dlasd8 = new DLASD8(dcopy, dlascl, dlasd4, dlaset, xerbla, ddot, dlamc3, dnrm2);
            DLASD6 dlasd6 = new DLASD6(dcopy, dlamrg, dlascl, dlasd7, dlasd8, xerbla);
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
            
            this._dcopy = dcopy; this._dlasd6 = dlasd6; this._dlasdq = dlasdq; this._dlasdt = dlasdt; this._dlaset = dlaset; 
            this._xerbla = xerbla;

            #endregion

        }
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// Using a divide and conquer approach, DLASDA computes the singular
        /// value decomposition (SVD) of a real upper bidiagonal N-by-M matrix
        /// B with diagonal D and offdiagonal E, where M = N + SQRE. The
        /// algorithm computes the singular values in the SVD B = U * S * VT.
        /// The orthogonal matrices U and VT are optionally computed in
        /// compact form.
        /// 
        /// A related subroutine, DLASD0, computes the singular values and
        /// the singular vectors in explicit form.
        /// 
        ///</summary>
        /// <param name="ICOMPQ">
        /// (input) INTEGER
        /// Specifies whether singular vectors are to be computed
        /// in compact form, as follows
        /// = 0: Compute singular values only.
        /// = 1: Compute singular vectors of upper bidiagonal
        /// matrix in compact form.
        ///</param>
        /// <param name="SMLSIZ">
        /// (input) INTEGER
        /// The maximum size of the subproblems at the bottom of the
        /// computation tree.
        ///</param>
        /// <param name="N">
        /// (input) INTEGER
        /// The row dimension of the upper bidiagonal matrix. This is
        /// also the dimension of the main diagonal array D.
        ///</param>
        /// <param name="SQRE">
        /// (input) INTEGER
        /// Specifies the column dimension of the bidiagonal matrix.
        /// = 0: The bidiagonal matrix has column dimension M = N;
        /// = 1: The bidiagonal matrix has column dimension M = N + 1.
        ///</param>
        /// <param name="D">
        /// (input/output) DOUBLE PRECISION array, dimension ( N )
        /// On entry D contains the main diagonal of the bidiagonal
        /// matrix. On exit D, if INFO = 0, contains its singular values.
        ///</param>
        /// <param name="E">
        /// (input) DOUBLE PRECISION array, dimension ( M-1 )
        /// Contains the subdiagonal entries of the bidiagonal matrix.
        /// On exit, E has been destroyed.
        ///</param>
        /// <param name="U">
        /// (output) DOUBLE PRECISION array,
        /// dimension ( LDU, SMLSIZ ) if ICOMPQ = 1, and not referenced
        /// if ICOMPQ = 0. If ICOMPQ = 1, on exit, U contains the left
        /// singular vector matrices of all subproblems at the bottom
        /// level.
        ///</param>
        /// <param name="LDU">
        /// (input) INTEGER, LDU = .GT. N.
        /// The leading dimension of arrays U, VT, DIFL, DIFR, POLES,
        /// GIVNUM, and Z.
        ///</param>
        /// <param name="VT">
        /// (output) DOUBLE PRECISION array,
        /// dimension ( LDU, SMLSIZ+1 ) if ICOMPQ = 1, and not referenced
        /// if ICOMPQ = 0. If ICOMPQ = 1, on exit, VT' contains the right
        /// singular vector matrices of all subproblems at the bottom
        /// level.
        ///</param>
        /// <param name="K">
        /// (output) INTEGER array,
        /// dimension ( N ) if ICOMPQ = 1 and dimension 1 if ICOMPQ = 0.
        /// If ICOMPQ = 1, on exit, K(I) is the dimension of the I-th
        /// secular equation on the computation tree.
        ///</param>
        /// <param name="DIFL">
        /// (output) DOUBLE PRECISION array, dimension ( LDU, NLVL ),
        /// where NLVL = floor(log_2 (N/SMLSIZ))).
        ///</param>
        /// <param name="DIFR">
        /// (output) DOUBLE PRECISION array,
        /// dimension ( LDU, 2 * NLVL ) if ICOMPQ = 1 and
        /// dimension ( N ) if ICOMPQ = 0.
        /// If ICOMPQ = 1, on exit, DIFL(1:N, I) and DIFR(1:N, 2 * I - 1)
        /// record distances between singular values on the I-th
        /// level and singular values on the (I -1)-th level, and
        /// DIFR(1:N, 2 * I ) contains the normalizing factors for
        /// the right singular vector matrix. See DLASD8 for details.
        ///</param>
        /// <param name="Z">
        /// (output) DOUBLE PRECISION array,
        /// dimension ( LDU, NLVL ) if ICOMPQ = 1 and
        /// dimension ( N ) if ICOMPQ = 0.
        /// The first K elements of Z(1, I) contain the components of
        /// the deflation-adjusted updating row vector for subproblems
        /// on the I-th level.
        ///</param>
        /// <param name="POLES">
        /// (output) DOUBLE PRECISION array,
        /// dimension ( LDU, 2 * NLVL ) if ICOMPQ = 1, and not referenced
        /// if ICOMPQ = 0. If ICOMPQ = 1, on exit, POLES(1, 2*I - 1) and
        /// POLES(1, 2*I) contain  the new and old singular values
        /// involved in the secular equations on the I-th level.
        ///</param>
        /// <param name="GIVPTR">
        /// (output) INTEGER array,
        /// dimension ( N ) if ICOMPQ = 1, and not referenced if
        /// ICOMPQ = 0. If ICOMPQ = 1, on exit, GIVPTR( I ) records
        /// the number of Givens rotations performed on the I-th
        /// problem on the computation tree.
        ///</param>
        /// <param name="GIVCOL">
        /// (output) INTEGER array,
        /// dimension ( LDGCOL, 2 * NLVL ) if ICOMPQ = 1, and not
        /// referenced if ICOMPQ = 0. If ICOMPQ = 1, on exit, for each I,
        /// GIVCOL(1, 2 *I - 1) and GIVCOL(1, 2 *I) record the locations
        /// of Givens rotations performed on the I-th level on the
        /// computation tree.
        ///</param>
        /// <param name="LDGCOL">
        /// (input) INTEGER, LDGCOL = .GT. N.
        /// The leading dimension of arrays GIVCOL and PERM.
        ///</param>
        /// <param name="PERM">
        /// (output) INTEGER array,
        /// dimension ( LDGCOL, NLVL ) if ICOMPQ = 1, and not referenced
        /// if ICOMPQ = 0. If ICOMPQ = 1, on exit, PERM(1, I) records
        /// permutations done on the I-th level of the computation tree.
        ///</param>
        /// <param name="GIVNUM">
        /// (output) DOUBLE PRECISION array,
        /// dimension ( LDU,  2 * NLVL ) if ICOMPQ = 1, and not
        /// referenced if ICOMPQ = 0. If ICOMPQ = 1, on exit, for each I,
        /// GIVNUM(1, 2 *I - 1) and GIVNUM(1, 2 *I) record the C- and S-
        /// values of Givens rotations performed on the I-th level on
        /// the computation tree.
        ///</param>
        /// <param name="C">
        /// (output) DOUBLE PRECISION array,
        /// dimension ( N ) if ICOMPQ = 1, and dimension 1 if ICOMPQ = 0.
        /// If ICOMPQ = 1 and the I-th subproblem is not square, on exit,
        /// C( I ) contains the C-value of a Givens rotation related to
        /// the right null space of the I-th subproblem.
        ///</param>
        /// <param name="S">
        /// (output) DOUBLE PRECISION array, dimension ( N ) if
        /// ICOMPQ = 1, and dimension 1 if ICOMPQ = 0. If ICOMPQ = 1
        /// and the I-th subproblem is not square, on exit, S( I )
        /// contains the S-value of a Givens rotation related to
        /// the right null space of the I-th subproblem.
        ///</param>
        /// <param name="WORK">
        /// (workspace) DOUBLE PRECISION array, dimension
        /// (6 * N + (SMLSIZ + 1)*(SMLSIZ + 1)).
        ///</param>
        /// <param name="IWORK">
        /// (workspace) INTEGER array.
        /// Dimension must be at least (7 * N).
        ///</param>
        /// <param name="INFO">
        /// (output) INTEGER
        /// = 0:  successful exit.
        /// .LT. 0:  if INFO = -i, the i-th argument had an illegal value.
        /// .GT. 0:  if INFO = 1, an singular value did not converge
        ///</param>
        public void Run(int ICOMPQ, int SMLSIZ, int N, int SQRE, ref double[] D, int offset_d, ref double[] E, int offset_e
                         , ref double[] U, int offset_u, int LDU, ref double[] VT, int offset_vt, ref int[] K, int offset_k, ref double[] DIFL, int offset_difl, ref double[] DIFR, int offset_difr
                         , ref double[] Z, int offset_z, ref double[] POLES, int offset_poles, ref int[] GIVPTR, int offset_givptr, ref int[] GIVCOL, int offset_givcol, int LDGCOL, ref int[] PERM, int offset_perm
                         , ref double[] GIVNUM, int offset_givnum, ref double[] C, int offset_c, ref double[] S, int offset_s, ref double[] WORK, int offset_work, ref int[] IWORK, int offset_iwork, ref int INFO)
        {

            #region Variables
            
            int I = 0; int I1 = 0; int IC = 0; int IDXQ = 0; int IDXQI = 0; int IM1 = 0; int INODE = 0; int ITEMP = 0; 
            int IWK = 0;int J = 0; int LF = 0; int LL = 0; int LVL = 0; int LVL2 = 0; int M = 0; int NCC = 0; int ND = 0; 
            int NDB1 = 0;int NDIML = 0; int NDIMR = 0; int NL = 0; int NLF = 0; int NLP1 = 0; int NLVL = 0; int NR = 0; 
            int NRF = 0;int NRP1 = 0; int NRU = 0; int NWORK1 = 0; int NWORK2 = 0; int SMLSZP = 0; int SQREI = 0; int VF = 0; 
            int VFI = 0;int VL = 0; int VLI = 0; double ALPHA = 0; double BETA = 0; 

            #endregion


            #region Array Index Correction
            
             int o_d = -1 + offset_d;  int o_e = -1 + offset_e;  int o_u = -1 - LDU + offset_u;  int o_vt = -1 - LDU + offset_vt; 
             int o_k = -1 + offset_k; int o_difl = -1 - LDU + offset_difl;  int o_difr = -1 - LDU + offset_difr; 
             int o_z = -1 - LDU + offset_z; int o_poles = -1 - LDU + offset_poles;  int o_givptr = -1 + offset_givptr; 
             int o_givcol = -1 - LDGCOL + offset_givcol; int o_perm = -1 - LDGCOL + offset_perm; 
             int o_givnum = -1 - LDU + offset_givnum; int o_c = -1 + offset_c;  int o_s = -1 + offset_s; 
             int o_work = -1 + offset_work; int o_iwork = -1 + offset_iwork; 

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
            // *  Using a divide and conquer approach, DLASDA computes the singular
            // *  value decomposition (SVD) of a real upper bidiagonal N-by-M matrix
            // *  B with diagonal D and offdiagonal E, where M = N + SQRE. The
            // *  algorithm computes the singular values in the SVD B = U * S * VT.
            // *  The orthogonal matrices U and VT are optionally computed in
            // *  compact form.
            // *
            // *  A related subroutine, DLASD0, computes the singular values and
            // *  the singular vectors in explicit form.
            // *
            // *  Arguments
            // *  =========
            // *
            // *  ICOMPQ (input) INTEGER
            // *         Specifies whether singular vectors are to be computed
            // *         in compact form, as follows
            // *         = 0: Compute singular values only.
            // *         = 1: Compute singular vectors of upper bidiagonal
            // *              matrix in compact form.
            // *
            // *  SMLSIZ (input) INTEGER
            // *         The maximum size of the subproblems at the bottom of the
            // *         computation tree.
            // *
            // *  N      (input) INTEGER
            // *         The row dimension of the upper bidiagonal matrix. This is
            // *         also the dimension of the main diagonal array D.
            // *
            // *  SQRE   (input) INTEGER
            // *         Specifies the column dimension of the bidiagonal matrix.
            // *         = 0: The bidiagonal matrix has column dimension M = N;
            // *         = 1: The bidiagonal matrix has column dimension M = N + 1.
            // *
            // *  D      (input/output) DOUBLE PRECISION array, dimension ( N )
            // *         On entry D contains the main diagonal of the bidiagonal
            // *         matrix. On exit D, if INFO = 0, contains its singular values.
            // *
            // *  E      (input) DOUBLE PRECISION array, dimension ( M-1 )
            // *         Contains the subdiagonal entries of the bidiagonal matrix.
            // *         On exit, E has been destroyed.
            // *
            // *  U      (output) DOUBLE PRECISION array,
            // *         dimension ( LDU, SMLSIZ ) if ICOMPQ = 1, and not referenced
            // *         if ICOMPQ = 0. If ICOMPQ = 1, on exit, U contains the left
            // *         singular vector matrices of all subproblems at the bottom
            // *         level.
            // *
            // *  LDU    (input) INTEGER, LDU = > N.
            // *         The leading dimension of arrays U, VT, DIFL, DIFR, POLES,
            // *         GIVNUM, and Z.
            // *
            // *  VT     (output) DOUBLE PRECISION array,
            // *         dimension ( LDU, SMLSIZ+1 ) if ICOMPQ = 1, and not referenced
            // *         if ICOMPQ = 0. If ICOMPQ = 1, on exit, VT' contains the right
            // *         singular vector matrices of all subproblems at the bottom
            // *         level.
            // *
            // *  K      (output) INTEGER array,
            // *         dimension ( N ) if ICOMPQ = 1 and dimension 1 if ICOMPQ = 0.
            // *         If ICOMPQ = 1, on exit, K(I) is the dimension of the I-th
            // *         secular equation on the computation tree.
            // *
            // *  DIFL   (output) DOUBLE PRECISION array, dimension ( LDU, NLVL ),
            // *         where NLVL = floor(log_2 (N/SMLSIZ))).
            // *
            // *  DIFR   (output) DOUBLE PRECISION array,
            // *                  dimension ( LDU, 2 * NLVL ) if ICOMPQ = 1 and
            // *                  dimension ( N ) if ICOMPQ = 0.
            // *         If ICOMPQ = 1, on exit, DIFL(1:N, I) and DIFR(1:N, 2 * I - 1)
            // *         record distances between singular values on the I-th
            // *         level and singular values on the (I -1)-th level, and
            // *         DIFR(1:N, 2 * I ) contains the normalizing factors for
            // *         the right singular vector matrix. See DLASD8 for details.
            // *
            // *  Z      (output) DOUBLE PRECISION array,
            // *                  dimension ( LDU, NLVL ) if ICOMPQ = 1 and
            // *                  dimension ( N ) if ICOMPQ = 0.
            // *         The first K elements of Z(1, I) contain the components of
            // *         the deflation-adjusted updating row vector for subproblems
            // *         on the I-th level.
            // *
            // *  POLES  (output) DOUBLE PRECISION array,
            // *         dimension ( LDU, 2 * NLVL ) if ICOMPQ = 1, and not referenced
            // *         if ICOMPQ = 0. If ICOMPQ = 1, on exit, POLES(1, 2*I - 1) and
            // *         POLES(1, 2*I) contain  the new and old singular values
            // *         involved in the secular equations on the I-th level.
            // *
            // *  GIVPTR (output) INTEGER array,
            // *         dimension ( N ) if ICOMPQ = 1, and not referenced if
            // *         ICOMPQ = 0. If ICOMPQ = 1, on exit, GIVPTR( I ) records
            // *         the number of Givens rotations performed on the I-th
            // *         problem on the computation tree.
            // *
            // *  GIVCOL (output) INTEGER array,
            // *         dimension ( LDGCOL, 2 * NLVL ) if ICOMPQ = 1, and not
            // *         referenced if ICOMPQ = 0. If ICOMPQ = 1, on exit, for each I,
            // *         GIVCOL(1, 2 *I - 1) and GIVCOL(1, 2 *I) record the locations
            // *         of Givens rotations performed on the I-th level on the
            // *         computation tree.
            // *
            // *  LDGCOL (input) INTEGER, LDGCOL = > N.
            // *         The leading dimension of arrays GIVCOL and PERM.
            // *
            // *  PERM   (output) INTEGER array,
            // *         dimension ( LDGCOL, NLVL ) if ICOMPQ = 1, and not referenced
            // *         if ICOMPQ = 0. If ICOMPQ = 1, on exit, PERM(1, I) records
            // *         permutations done on the I-th level of the computation tree.
            // *
            // *  GIVNUM (output) DOUBLE PRECISION array,
            // *         dimension ( LDU,  2 * NLVL ) if ICOMPQ = 1, and not
            // *         referenced if ICOMPQ = 0. If ICOMPQ = 1, on exit, for each I,
            // *         GIVNUM(1, 2 *I - 1) and GIVNUM(1, 2 *I) record the C- and S-
            // *         values of Givens rotations performed on the I-th level on
            // *         the computation tree.
            // *
            // *  C      (output) DOUBLE PRECISION array,
            // *         dimension ( N ) if ICOMPQ = 1, and dimension 1 if ICOMPQ = 0.
            // *         If ICOMPQ = 1 and the I-th subproblem is not square, on exit,
            // *         C( I ) contains the C-value of a Givens rotation related to
            // *         the right null space of the I-th subproblem.
            // *
            // *  S      (output) DOUBLE PRECISION array, dimension ( N ) if
            // *         ICOMPQ = 1, and dimension 1 if ICOMPQ = 0. If ICOMPQ = 1
            // *         and the I-th subproblem is not square, on exit, S( I )
            // *         contains the S-value of a Givens rotation related to
            // *         the right null space of the I-th subproblem.
            // *
            // *  WORK   (workspace) DOUBLE PRECISION array, dimension
            // *         (6 * N + (SMLSIZ + 1)*(SMLSIZ + 1)).
            // *
            // *  IWORK  (workspace) INTEGER array.
            // *         Dimension must be at least (7 * N).
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
            // *     ..
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
            if ((ICOMPQ < 0) || (ICOMPQ > 1))
            {
                INFO =  - 1;
            }
            else
            {
                if (SMLSIZ < 3)
                {
                    INFO =  - 2;
                }
                else
                {
                    if (N < 0)
                    {
                        INFO =  - 3;
                    }
                    else
                    {
                        if ((SQRE < 0) || (SQRE > 1))
                        {
                            INFO =  - 4;
                        }
                        else
                        {
                            if (LDU < (N + SQRE))
                            {
                                INFO =  - 8;
                            }
                            else
                            {
                                if (LDGCOL < N)
                                {
                                    INFO =  - 17;
                                }
                            }
                        }
                    }
                }
            }
            if (INFO != 0)
            {
                this._xerbla.Run("DLASDA",  - INFO);
                return;
            }
            // *
            M = N + SQRE;
            // *
            // *     If the input matrix is too small, call DLASDQ to find the SVD.
            // *
            if (N <= SMLSIZ)
            {
                if (ICOMPQ == 0)
                {
                    this._dlasdq.Run("U", SQRE, N, 0, 0, 0
                                     , ref D, offset_d, ref E, offset_e, ref VT, offset_vt, LDU, ref U, offset_u, LDU
                                     , ref U, offset_u, LDU, ref WORK, offset_work, ref INFO);
                }
                else
                {
                    this._dlasdq.Run("U", SQRE, N, M, N, 0
                                     , ref D, offset_d, ref E, offset_e, ref VT, offset_vt, LDU, ref U, offset_u, LDU
                                     , ref U, offset_u, LDU, ref WORK, offset_work, ref INFO);
                }
                return;
            }
            // *
            // *     Book-keeping and  set up the computation tree.
            // *
            INODE = 1;
            NDIML = INODE + N;
            NDIMR = NDIML + N;
            IDXQ = NDIMR + N;
            IWK = IDXQ + N;
            // *
            NCC = 0;
            NRU = 0;
            // *
            SMLSZP = SMLSIZ + 1;
            VF = 1;
            VL = VF + M;
            NWORK1 = VL + M;
            NWORK2 = NWORK1 + SMLSZP * SMLSZP;
            // *
            this._dlasdt.Run(N, ref NLVL, ref ND, ref IWORK, INODE + o_iwork, ref IWORK, NDIML + o_iwork, ref IWORK, NDIMR + o_iwork
                             , SMLSIZ);
            // *
            // *     for the nodes on bottom level of the tree, solve
            // *     their subproblems by DLASDQ.
            // *
            NDB1 = (ND + 1) / 2;
            for (I = NDB1; I <= ND; I++)
            {
                // *
                // *        IC : center row of each node
                // *        NL : number of rows of left  subproblem
                // *        NR : number of rows of right subproblem
                // *        NLF: starting row of the left   subproblem
                // *        NRF: starting row of the right  subproblem
                // *
                I1 = I - 1;
                IC = IWORK[INODE + I1 + o_iwork];
                NL = IWORK[NDIML + I1 + o_iwork];
                NLP1 = NL + 1;
                NR = IWORK[NDIMR + I1 + o_iwork];
                NLF = IC - NL;
                NRF = IC + 1;
                IDXQI = IDXQ + NLF - 2;
                VFI = VF + NLF - 1;
                VLI = VL + NLF - 1;
                SQREI = 1;
                if (ICOMPQ == 0)
                {
                    this._dlaset.Run("A", NLP1, NLP1, ZERO, ONE, ref WORK, NWORK1 + o_work
                                     , SMLSZP);
                    this._dlasdq.Run("U", SQREI, NL, NLP1, NRU, NCC
                                     , ref D, NLF + o_d, ref E, NLF + o_e, ref WORK, NWORK1 + o_work, SMLSZP, ref WORK, NWORK2 + o_work, NL
                                     , ref WORK, NWORK2 + o_work, NL, ref WORK, NWORK2 + o_work, ref INFO);
                    ITEMP = NWORK1 + NL * SMLSZP;
                    this._dcopy.Run(NLP1, WORK, NWORK1 + o_work, 1, ref WORK, VFI + o_work, 1);
                    this._dcopy.Run(NLP1, WORK, ITEMP + o_work, 1, ref WORK, VLI + o_work, 1);
                }
                else
                {
                    this._dlaset.Run("A", NL, NL, ZERO, ONE, ref U, NLF+1 * LDU + o_u
                                     , LDU);
                    this._dlaset.Run("A", NLP1, NLP1, ZERO, ONE, ref VT, NLF+1 * LDU + o_vt
                                     , LDU);
                    this._dlasdq.Run("U", SQREI, NL, NLP1, NL, NCC
                                     , ref D, NLF + o_d, ref E, NLF + o_e, ref VT, NLF+1 * LDU + o_vt, LDU, ref U, NLF+1 * LDU + o_u, LDU
                                     , ref U, NLF+1 * LDU + o_u, LDU, ref WORK, NWORK1 + o_work, ref INFO);
                    this._dcopy.Run(NLP1, VT, NLF+1 * LDU + o_vt, 1, ref WORK, VFI + o_work, 1);
                    this._dcopy.Run(NLP1, VT, NLF+NLP1 * LDU + o_vt, 1, ref WORK, VLI + o_work, 1);
                }
                if (INFO != 0)
                {
                    return;
                }
                for (J = 1; J <= NL; J++)
                {
                    IWORK[IDXQI + J + o_iwork] = J;
                }
                if ((I == ND) && (SQRE == 0))
                {
                    SQREI = 0;
                }
                else
                {
                    SQREI = 1;
                }
                IDXQI += NLP1;
                VFI += NLP1;
                VLI += NLP1;
                NRP1 = NR + SQREI;
                if (ICOMPQ == 0)
                {
                    this._dlaset.Run("A", NRP1, NRP1, ZERO, ONE, ref WORK, NWORK1 + o_work
                                     , SMLSZP);
                    this._dlasdq.Run("U", SQREI, NR, NRP1, NRU, NCC
                                     , ref D, NRF + o_d, ref E, NRF + o_e, ref WORK, NWORK1 + o_work, SMLSZP, ref WORK, NWORK2 + o_work, NR
                                     , ref WORK, NWORK2 + o_work, NR, ref WORK, NWORK2 + o_work, ref INFO);
                    ITEMP = NWORK1 + (NRP1 - 1) * SMLSZP;
                    this._dcopy.Run(NRP1, WORK, NWORK1 + o_work, 1, ref WORK, VFI + o_work, 1);
                    this._dcopy.Run(NRP1, WORK, ITEMP + o_work, 1, ref WORK, VLI + o_work, 1);
                }
                else
                {
                    this._dlaset.Run("A", NR, NR, ZERO, ONE, ref U, NRF+1 * LDU + o_u
                                     , LDU);
                    this._dlaset.Run("A", NRP1, NRP1, ZERO, ONE, ref VT, NRF+1 * LDU + o_vt
                                     , LDU);
                    this._dlasdq.Run("U", SQREI, NR, NRP1, NR, NCC
                                     , ref D, NRF + o_d, ref E, NRF + o_e, ref VT, NRF+1 * LDU + o_vt, LDU, ref U, NRF+1 * LDU + o_u, LDU
                                     , ref U, NRF+1 * LDU + o_u, LDU, ref WORK, NWORK1 + o_work, ref INFO);
                    this._dcopy.Run(NRP1, VT, NRF+1 * LDU + o_vt, 1, ref WORK, VFI + o_work, 1);
                    this._dcopy.Run(NRP1, VT, NRF+NRP1 * LDU + o_vt, 1, ref WORK, VLI + o_work, 1);
                }
                if (INFO != 0)
                {
                    return;
                }
                for (J = 1; J <= NR; J++)
                {
                    IWORK[IDXQI + J + o_iwork] = J;
                }
            }
            // *
            // *     Now conquer each subproblem bottom-up.
            // *
            J = (int)Math.Pow(2, NLVL);
            for (LVL = NLVL; LVL >= 1; LVL +=  - 1)
            {
                LVL2 = LVL * 2 - 1;
                // *
                // *        Find the first node LF and last node LL on
                // *        the current level LVL.
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
                    NRF = IC + 1;
                    if (I == LL)
                    {
                        SQREI = SQRE;
                    }
                    else
                    {
                        SQREI = 1;
                    }
                    VFI = VF + NLF - 1;
                    VLI = VL + NLF - 1;
                    IDXQI = IDXQ + NLF - 1;
                    ALPHA = D[IC + o_d];
                    BETA = E[IC + o_e];
                    if (ICOMPQ == 0)
                    {
                        this._dlasd6.Run(ICOMPQ, NL, NR, SQREI, ref D, NLF + o_d, ref WORK, VFI + o_work
                                         , ref WORK, VLI + o_work, ref ALPHA, ref BETA, ref IWORK, IDXQI + o_iwork, ref PERM, offset_perm, ref GIVPTR[1 + o_givptr]
                                         , ref GIVCOL, offset_givcol, LDGCOL, ref GIVNUM, offset_givnum, LDU, ref POLES, offset_poles, ref DIFL, offset_difl
                                         , ref DIFR, offset_difr, ref Z, offset_z, ref K[1 + o_k], ref C[1 + o_c], ref S[1 + o_s], ref WORK, NWORK1 + o_work
                                         , ref IWORK, IWK + o_iwork, ref INFO);
                    }
                    else
                    {
                        J -= 1;
                        this._dlasd6.Run(ICOMPQ, NL, NR, SQREI, ref D, NLF + o_d, ref WORK, VFI + o_work
                                         , ref WORK, VLI + o_work, ref ALPHA, ref BETA, ref IWORK, IDXQI + o_iwork, ref PERM, NLF+LVL * LDGCOL + o_perm, ref GIVPTR[J + o_givptr]
                                         , ref GIVCOL, NLF+LVL2 * LDGCOL + o_givcol, LDGCOL, ref GIVNUM, NLF+LVL2 * LDU + o_givnum, LDU, ref POLES, NLF+LVL2 * LDU + o_poles, ref DIFL, NLF+LVL * LDU + o_difl
                                         , ref DIFR, NLF+LVL2 * LDU + o_difr, ref Z, NLF+LVL * LDU + o_z, ref K[J + o_k], ref C[J + o_c], ref S[J + o_s], ref WORK, NWORK1 + o_work
                                         , ref IWORK, IWK + o_iwork, ref INFO);
                    }
                    if (INFO != 0)
                    {
                        return;
                    }
                }
            }
            // *
            return;
            // *
            // *     End of DLASDA
            // *

            #endregion

        }
    }
}
