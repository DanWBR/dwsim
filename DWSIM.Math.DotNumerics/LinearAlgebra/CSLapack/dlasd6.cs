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
    /// DLASD6 computes the SVD of an updated upper bidiagonal matrix B
    /// obtained by merging two smaller ones by appending a row. This
    /// routine is used only for the problem which requires all singular
    /// values and optionally singular vector matrices in factored form.
    /// B is an N-by-M matrix with N = NL + NR + 1 and M = N + SQRE.
    /// A related subroutine, DLASD1, handles the case in which all singular
    /// values and singular vectors of the bidiagonal matrix are desired.
    /// 
    /// DLASD6 computes the SVD as follows:
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
    /// The singular values of B can be computed using D1, D2, the first
    /// components of all the right singular vectors of the lower block, and
    /// the last components of all the right singular vectors of the upper
    /// block. These components are stored and updated in VF and VL,
    /// respectively, in DLASD6. Hence U and VT are not explicitly
    /// referenced.
    /// 
    /// The singular values are stored in D. The algorithm consists of two
    /// stages:
    /// 
    /// The first stage consists of deflating the size of the problem
    /// when there are multiple singular values or if there is a zero
    /// in the Z vector. For each such occurence the dimension of the
    /// secular equation problem is reduced by one. This stage is
    /// performed by the routine DLASD7.
    /// 
    /// The second stage consists of calculating the updated
    /// singular values. This is done by finding the roots of the
    /// secular equation via the routine DLASD4 (as called by DLASD8).
    /// This routine also updates VF and VL and computes the distances
    /// between the updated singular values and the old singular
    /// values.
    /// 
    /// DLASD6 is called from DLASDA.
    /// 
    ///</summary>
    public class DLASD6
    {
    

        #region Dependencies
        
        DCOPY _dcopy; DLAMRG _dlamrg; DLASCL _dlascl; DLASD7 _dlasd7; DLASD8 _dlasd8; XERBLA _xerbla; 

        #endregion


        #region Variables
        
        const double ONE = 1.0E+0; const double ZERO = 0.0E+0; 

        #endregion

        public DLASD6(DCOPY dcopy, DLAMRG dlamrg, DLASCL dlascl, DLASD7 dlasd7, DLASD8 dlasd8, XERBLA xerbla)
        {
    

            #region Set Dependencies
            
            this._dcopy = dcopy; this._dlamrg = dlamrg; this._dlascl = dlascl; this._dlasd7 = dlasd7; this._dlasd8 = dlasd8; 
            this._xerbla = xerbla;

            #endregion

        }
    
        public DLASD6()
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

            #endregion


            #region Set Dependencies
            
            this._dcopy = dcopy; this._dlamrg = dlamrg; this._dlascl = dlascl; this._dlasd7 = dlasd7; this._dlasd8 = dlasd8; 
            this._xerbla = xerbla;

            #endregion

        }
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DLASD6 computes the SVD of an updated upper bidiagonal matrix B
        /// obtained by merging two smaller ones by appending a row. This
        /// routine is used only for the problem which requires all singular
        /// values and optionally singular vector matrices in factored form.
        /// B is an N-by-M matrix with N = NL + NR + 1 and M = N + SQRE.
        /// A related subroutine, DLASD1, handles the case in which all singular
        /// values and singular vectors of the bidiagonal matrix are desired.
        /// 
        /// DLASD6 computes the SVD as follows:
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
        /// The singular values of B can be computed using D1, D2, the first
        /// components of all the right singular vectors of the lower block, and
        /// the last components of all the right singular vectors of the upper
        /// block. These components are stored and updated in VF and VL,
        /// respectively, in DLASD6. Hence U and VT are not explicitly
        /// referenced.
        /// 
        /// The singular values are stored in D. The algorithm consists of two
        /// stages:
        /// 
        /// The first stage consists of deflating the size of the problem
        /// when there are multiple singular values or if there is a zero
        /// in the Z vector. For each such occurence the dimension of the
        /// secular equation problem is reduced by one. This stage is
        /// performed by the routine DLASD7.
        /// 
        /// The second stage consists of calculating the updated
        /// singular values. This is done by finding the roots of the
        /// secular equation via the routine DLASD4 (as called by DLASD8).
        /// This routine also updates VF and VL and computes the distances
        /// between the updated singular values and the old singular
        /// values.
        /// 
        /// DLASD6 is called from DLASDA.
        /// 
        ///</summary>
        /// <param name="ICOMPQ">
        /// (input) INTEGER
        /// Specifies whether singular vectors are to be computed in
        /// factored form:
        /// = 0: Compute singular values only.
        /// = 1: Compute singular vectors in factored form as well.
        ///</param>
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
        /// (input/output) DOUBLE PRECISION array, dimension ( NL+NR+1 ).
        /// On entry D(1:NL,1:NL) contains the singular values of the
        /// upper block, and D(NL+2:N) contains the singular values
        /// of the lower block. On exit D(1:N) contains the singular
        /// values of the modified matrix.
        ///</param>
        /// <param name="VF">
        /// (input/output) DOUBLE PRECISION array, dimension ( M )
        /// On entry, VF(1:NL+1) contains the first components of all
        /// right singular vectors of the upper block; and VF(NL+2:M)
        /// contains the first components of all right singular vectors
        /// of the lower block. On exit, VF contains the first components
        /// of all right singular vectors of the bidiagonal matrix.
        ///</param>
        /// <param name="VL">
        /// (input/output) DOUBLE PRECISION array, dimension ( M )
        /// On entry, VL(1:NL+1) contains the  last components of all
        /// right singular vectors of the upper block; and VL(NL+2:M)
        /// contains the last components of all right singular vectors of
        /// the lower block. On exit, VL contains the last components of
        /// all right singular vectors of the bidiagonal matrix.
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
        /// <param name="IDXQ">
        /// (output) INTEGER array, dimension ( N )
        /// This contains the permutation which will reintegrate the
        /// subproblem just solved back into sorted order, i.e.
        /// D( IDXQ( I = 1, N ) ) will be in ascending order.
        ///</param>
        /// <param name="PERM">
        /// (output) INTEGER array, dimension ( N )
        /// The permutations (from deflation and sorting) to be applied
        /// to each block. Not referenced if ICOMPQ = 0.
        ///</param>
        /// <param name="GIVPTR">
        /// (output) INTEGER
        /// The number of Givens rotations which took place in this
        /// subproblem. Not referenced if ICOMPQ = 0.
        ///</param>
        /// <param name="GIVCOL">
        /// (output) INTEGER array, dimension ( LDGCOL, 2 )
        /// Each pair of numbers indicates a pair of columns to take place
        /// in a Givens rotation. Not referenced if ICOMPQ = 0.
        ///</param>
        /// <param name="LDGCOL">
        /// (input) INTEGER
        /// leading dimension of GIVCOL, must be at least N.
        ///</param>
        /// <param name="GIVNUM">
        /// (output) DOUBLE PRECISION array, dimension ( LDGNUM, 2 )
        /// Each number indicates the C or S value to be used in the
        /// corresponding Givens rotation. Not referenced if ICOMPQ = 0.
        ///</param>
        /// <param name="LDGNUM">
        /// (input) INTEGER
        /// The leading dimension of GIVNUM and POLES, must be at least N.
        ///</param>
        /// <param name="POLES">
        /// (output) DOUBLE PRECISION array, dimension ( LDGNUM, 2 )
        /// On exit, POLES(1,*) is an array containing the new singular
        /// values obtained from solving the secular equation, and
        /// POLES(2,*) is an array containing the poles in the secular
        /// equation. Not referenced if ICOMPQ = 0.
        ///</param>
        /// <param name="DIFL">
        /// (output) DOUBLE PRECISION array, dimension ( N )
        /// On exit, DIFL(I) is the distance between I-th updated
        /// (undeflated) singular value and the I-th (undeflated) old
        /// singular value.
        ///</param>
        /// <param name="DIFR">
        /// (output) DOUBLE PRECISION array,
        /// dimension ( LDGNUM, 2 ) if ICOMPQ = 1 and
        /// dimension ( N ) if ICOMPQ = 0.
        /// On exit, DIFR(I, 1) is the distance between I-th updated
        /// (undeflated) singular value and the I+1-th (undeflated) old
        /// singular value.
        /// 
        /// If ICOMPQ = 1, DIFR(1:K,2) is an array containing the
        /// normalizing factors for the right singular vector matrix.
        /// 
        /// See DLASD8 for details on DIFL and DIFR.
        ///</param>
        /// <param name="Z">
        /// (output) DOUBLE PRECISION array, dimension ( M )
        /// The first elements of this array contain the components
        /// of the deflation-adjusted updating row vector.
        ///</param>
        /// <param name="K">
        /// (output) INTEGER
        /// Contains the dimension of the non-deflated matrix,
        /// This is the order of the related secular equation. 1 .LE. K .LE.N.
        ///</param>
        /// <param name="C">
        /// (output) DOUBLE PRECISION
        /// C contains garbage if SQRE =0 and the C-value of a Givens
        /// rotation related to the right null space if SQRE = 1.
        ///</param>
        /// <param name="S">
        /// (output) DOUBLE PRECISION
        /// S contains garbage if SQRE =0 and the S-value of a Givens
        /// rotation related to the right null space if SQRE = 1.
        ///</param>
        /// <param name="WORK">
        /// (workspace) DOUBLE PRECISION array, dimension ( 4 * M )
        ///</param>
        /// <param name="IWORK">
        /// (workspace) INTEGER array, dimension ( 3 * N )
        ///</param>
        /// <param name="INFO">
        /// (output) INTEGER
        /// = 0:  successful exit.
        /// .LT. 0:  if INFO = -i, the i-th argument had an illegal value.
        /// .GT. 0:  if INFO = 1, an singular value did not converge
        ///</param>
        public void Run(int ICOMPQ, int NL, int NR, int SQRE, ref double[] D, int offset_d, ref double[] VF, int offset_vf
                         , ref double[] VL, int offset_vl, ref double ALPHA, ref double BETA, ref int[] IDXQ, int offset_idxq, ref int[] PERM, int offset_perm, ref int GIVPTR
                         , ref int[] GIVCOL, int offset_givcol, int LDGCOL, ref double[] GIVNUM, int offset_givnum, int LDGNUM, ref double[] POLES, int offset_poles, ref double[] DIFL, int offset_difl
                         , ref double[] DIFR, int offset_difr, ref double[] Z, int offset_z, ref int K, ref double C, ref double S, ref double[] WORK, int offset_work
                         , ref int[] IWORK, int offset_iwork, ref int INFO)
        {

            #region Variables
            
            int I = 0; int IDX = 0; int IDXC = 0; int IDXP = 0; int ISIGMA = 0; int IVFW = 0; int IVLW = 0; int IW = 0; int M = 0; 
            int N = 0;int N1 = 0; int N2 = 0; double ORGNRM = 0; 

            #endregion


            #region Array Index Correction
            
             int o_d = -1 + offset_d;  int o_vf = -1 + offset_vf;  int o_vl = -1 + offset_vl;  int o_idxq = -1 + offset_idxq; 
             int o_perm = -1 + offset_perm; int o_givcol = -1 - LDGCOL + offset_givcol; 
             int o_givnum = -1 - LDGNUM + offset_givnum; int o_poles = -1 - LDGNUM + offset_poles;  int o_difl = -1 + offset_difl; 
             int o_difr = -1 + offset_difr; int o_z = -1 + offset_z;  int o_work = -1 + offset_work; 
             int o_iwork = -1 + offset_iwork;

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
            // *  DLASD6 computes the SVD of an updated upper bidiagonal matrix B
            // *  obtained by merging two smaller ones by appending a row. This
            // *  routine is used only for the problem which requires all singular
            // *  values and optionally singular vector matrices in factored form.
            // *  B is an N-by-M matrix with N = NL + NR + 1 and M = N + SQRE.
            // *  A related subroutine, DLASD1, handles the case in which all singular
            // *  values and singular vectors of the bidiagonal matrix are desired.
            // *
            // *  DLASD6 computes the SVD as follows:
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
            // *  The singular values of B can be computed using D1, D2, the first
            // *  components of all the right singular vectors of the lower block, and
            // *  the last components of all the right singular vectors of the upper
            // *  block. These components are stored and updated in VF and VL,
            // *  respectively, in DLASD6. Hence U and VT are not explicitly
            // *  referenced.
            // *
            // *  The singular values are stored in D. The algorithm consists of two
            // *  stages:
            // *
            // *        The first stage consists of deflating the size of the problem
            // *        when there are multiple singular values or if there is a zero
            // *        in the Z vector. For each such occurence the dimension of the
            // *        secular equation problem is reduced by one. This stage is
            // *        performed by the routine DLASD7.
            // *
            // *        The second stage consists of calculating the updated
            // *        singular values. This is done by finding the roots of the
            // *        secular equation via the routine DLASD4 (as called by DLASD8).
            // *        This routine also updates VF and VL and computes the distances
            // *        between the updated singular values and the old singular
            // *        values.
            // *
            // *  DLASD6 is called from DLASDA.
            // *
            // *  Arguments
            // *  =========
            // *
            // *  ICOMPQ (input) INTEGER
            // *         Specifies whether singular vectors are to be computed in
            // *         factored form:
            // *         = 0: Compute singular values only.
            // *         = 1: Compute singular vectors in factored form as well.
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
            // *  D      (input/output) DOUBLE PRECISION array, dimension ( NL+NR+1 ).
            // *         On entry D(1:NL,1:NL) contains the singular values of the
            // *         upper block, and D(NL+2:N) contains the singular values
            // *         of the lower block. On exit D(1:N) contains the singular
            // *         values of the modified matrix.
            // *
            // *  VF     (input/output) DOUBLE PRECISION array, dimension ( M )
            // *         On entry, VF(1:NL+1) contains the first components of all
            // *         right singular vectors of the upper block; and VF(NL+2:M)
            // *         contains the first components of all right singular vectors
            // *         of the lower block. On exit, VF contains the first components
            // *         of all right singular vectors of the bidiagonal matrix.
            // *
            // *  VL     (input/output) DOUBLE PRECISION array, dimension ( M )
            // *         On entry, VL(1:NL+1) contains the  last components of all
            // *         right singular vectors of the upper block; and VL(NL+2:M)
            // *         contains the last components of all right singular vectors of
            // *         the lower block. On exit, VL contains the last components of
            // *         all right singular vectors of the bidiagonal matrix.
            // *
            // *  ALPHA  (input/output) DOUBLE PRECISION
            // *         Contains the diagonal element associated with the added row.
            // *
            // *  BETA   (input/output) DOUBLE PRECISION
            // *         Contains the off-diagonal element associated with the added
            // *         row.
            // *
            // *  IDXQ   (output) INTEGER array, dimension ( N )
            // *         This contains the permutation which will reintegrate the
            // *         subproblem just solved back into sorted order, i.e.
            // *         D( IDXQ( I = 1, N ) ) will be in ascending order.
            // *
            // *  PERM   (output) INTEGER array, dimension ( N )
            // *         The permutations (from deflation and sorting) to be applied
            // *         to each block. Not referenced if ICOMPQ = 0.
            // *
            // *  GIVPTR (output) INTEGER
            // *         The number of Givens rotations which took place in this
            // *         subproblem. Not referenced if ICOMPQ = 0.
            // *
            // *  GIVCOL (output) INTEGER array, dimension ( LDGCOL, 2 )
            // *         Each pair of numbers indicates a pair of columns to take place
            // *         in a Givens rotation. Not referenced if ICOMPQ = 0.
            // *
            // *  LDGCOL (input) INTEGER
            // *         leading dimension of GIVCOL, must be at least N.
            // *
            // *  GIVNUM (output) DOUBLE PRECISION array, dimension ( LDGNUM, 2 )
            // *         Each number indicates the C or S value to be used in the
            // *         corresponding Givens rotation. Not referenced if ICOMPQ = 0.
            // *
            // *  LDGNUM (input) INTEGER
            // *         The leading dimension of GIVNUM and POLES, must be at least N.
            // *
            // *  POLES  (output) DOUBLE PRECISION array, dimension ( LDGNUM, 2 )
            // *         On exit, POLES(1,*) is an array containing the new singular
            // *         values obtained from solving the secular equation, and
            // *         POLES(2,*) is an array containing the poles in the secular
            // *         equation. Not referenced if ICOMPQ = 0.
            // *
            // *  DIFL   (output) DOUBLE PRECISION array, dimension ( N )
            // *         On exit, DIFL(I) is the distance between I-th updated
            // *         (undeflated) singular value and the I-th (undeflated) old
            // *         singular value.
            // *
            // *  DIFR   (output) DOUBLE PRECISION array,
            // *                  dimension ( LDGNUM, 2 ) if ICOMPQ = 1 and
            // *                  dimension ( N ) if ICOMPQ = 0.
            // *         On exit, DIFR(I, 1) is the distance between I-th updated
            // *         (undeflated) singular value and the I+1-th (undeflated) old
            // *         singular value.
            // *
            // *         If ICOMPQ = 1, DIFR(1:K,2) is an array containing the
            // *         normalizing factors for the right singular vector matrix.
            // *
            // *         See DLASD8 for details on DIFL and DIFR.
            // *
            // *  Z      (output) DOUBLE PRECISION array, dimension ( M )
            // *         The first elements of this array contain the components
            // *         of the deflation-adjusted updating row vector.
            // *
            // *  K      (output) INTEGER
            // *         Contains the dimension of the non-deflated matrix,
            // *         This is the order of the related secular equation. 1 <= K <=N.
            // *
            // *  C      (output) DOUBLE PRECISION
            // *         C contains garbage if SQRE =0 and the C-value of a Givens
            // *         rotation related to the right null space if SQRE = 1.
            // *
            // *  S      (output) DOUBLE PRECISION
            // *         S contains garbage if SQRE =0 and the S-value of a Givens
            // *         rotation related to the right null space if SQRE = 1.
            // *
            // *  WORK   (workspace) DOUBLE PRECISION array, dimension ( 4 * M )
            // *
            // *  IWORK  (workspace) INTEGER array, dimension ( 3 * N )
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
            N = NL + NR + 1;
            M = N + SQRE;
            // *
            if ((ICOMPQ < 0) || (ICOMPQ > 1))
            {
                INFO =  - 1;
            }
            else
            {
                if (NL < 1)
                {
                    INFO =  - 2;
                }
                else
                {
                    if (NR < 1)
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
                            if (LDGCOL < N)
                            {
                                INFO =  - 14;
                            }
                            else
                            {
                                if (LDGNUM < N)
                                {
                                    INFO =  - 16;
                                }
                            }
                        }
                    }
                }
            }
            if (INFO != 0)
            {
                this._xerbla.Run("DLASD6",  - INFO);
                return;
            }
            // *
            // *     The following values are for bookkeeping purposes only.  They are
            // *     integer pointers which indicate the portion of the workspace
            // *     used by a particular array in DLASD7 and DLASD8.
            // *
            ISIGMA = 1;
            IW = ISIGMA + N;
            IVFW = IW + M;
            IVLW = IVFW + M;
            // *
            IDX = 1;
            IDXC = IDX + N;
            IDXP = IDXC + N;
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
            // *     Sort and Deflate singular values.
            // *
            this._dlasd7.Run(ICOMPQ, NL, NR, SQRE, ref K, ref D, offset_d
                             , ref Z, offset_z, ref WORK, IW + o_work, ref VF, offset_vf, ref WORK, IVFW + o_work, ref VL, offset_vl, ref WORK, IVLW + o_work
                             , ALPHA, BETA, ref WORK, ISIGMA + o_work, ref IWORK, IDX + o_iwork, ref IWORK, IDXP + o_iwork, ref IDXQ, offset_idxq
                             , ref PERM, offset_perm, ref GIVPTR, ref GIVCOL, offset_givcol, LDGCOL, ref GIVNUM, offset_givnum, LDGNUM
                             , ref C, ref S, ref INFO);
            // *
            // *     Solve Secular Equation, compute DIFL, DIFR, and update VF, VL.
            // *
            this._dlasd8.Run(ICOMPQ, K, ref D, offset_d, ref Z, offset_z, ref VF, offset_vf, ref VL, offset_vl
                             , ref DIFL, offset_difl, ref DIFR, offset_difr, LDGNUM, ref WORK, ISIGMA + o_work, ref WORK, IW + o_work, ref INFO);
            // *
            // *     Save the poles if ICOMPQ = 1.
            // *
            if (ICOMPQ == 1)
            {
                this._dcopy.Run(K, D, offset_d, 1, ref POLES, 1+1 * LDGNUM + o_poles, 1);
                this._dcopy.Run(K, WORK, ISIGMA + o_work, 1, ref POLES, 1+2 * LDGNUM + o_poles, 1);
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
            // *     End of DLASD6
            // *

            #endregion

        }
    }
}
