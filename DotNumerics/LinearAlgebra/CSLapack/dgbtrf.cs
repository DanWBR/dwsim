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
    /// -- LAPACK routine (version 3.1) --
    /// Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
    /// November 2006
    /// Purpose
    /// =======
    /// 
    /// DGBTRF computes an LU factorization of a real m-by-n band matrix A
    /// using partial pivoting with row interchanges.
    /// 
    /// This is the blocked version of the algorithm, calling Level 3 BLAS.
    /// 
    ///</summary>
    public class DGBTRF
    {
    

        #region Dependencies
        
        IDAMAX _idamax; ILAENV _ilaenv; DCOPY _dcopy; DGBTF2 _dgbtf2; DGEMM _dgemm; DGER _dger; DLASWP _dlaswp; DSCAL _dscal; 
        DSWAP _dswap;DTRSM _dtrsm; XERBLA _xerbla; 

        #endregion


        #region Variables
        
        const double ONE = 1.0E+0; const double ZERO = 0.0E+0; const int NBMAX = 64; const int LDWORK = NBMAX + 1; 
        double[] WORK13 = new double[LDWORK * NBMAX];double[] WORK31 = new double[LDWORK * NBMAX]; 

        #endregion

        public DGBTRF(IDAMAX idamax, ILAENV ilaenv, DCOPY dcopy, DGBTF2 dgbtf2, DGEMM dgemm, DGER dger, DLASWP dlaswp, DSCAL dscal, DSWAP dswap, DTRSM dtrsm
                      , XERBLA xerbla)
        {
    

            #region Set Dependencies
            
            this._idamax = idamax; this._ilaenv = ilaenv; this._dcopy = dcopy; this._dgbtf2 = dgbtf2; this._dgemm = dgemm; 
            this._dger = dger;this._dlaswp = dlaswp; this._dscal = dscal; this._dswap = dswap; this._dtrsm = dtrsm; 
            this._xerbla = xerbla;

            #endregion

        }
    
        public DGBTRF()
        {
    

            #region Dependencies (Initialization)
            
            IDAMAX idamax = new IDAMAX();
            IEEECK ieeeck = new IEEECK();
            IPARMQ iparmq = new IPARMQ();
            DCOPY dcopy = new DCOPY();
            XERBLA xerbla = new XERBLA();
            DSCAL dscal = new DSCAL();
            DSWAP dswap = new DSWAP();
            LSAME lsame = new LSAME();
            DLASWP dlaswp = new DLASWP();
            ILAENV ilaenv = new ILAENV(ieeeck, iparmq);
            DGER dger = new DGER(xerbla);
            DGBTF2 dgbtf2 = new DGBTF2(idamax, dger, dscal, dswap, xerbla);
            DGEMM dgemm = new DGEMM(lsame, xerbla);
            DTRSM dtrsm = new DTRSM(lsame, xerbla);

            #endregion


            #region Set Dependencies
            
            this._idamax = idamax; this._ilaenv = ilaenv; this._dcopy = dcopy; this._dgbtf2 = dgbtf2; this._dgemm = dgemm; 
            this._dger = dger;this._dlaswp = dlaswp; this._dscal = dscal; this._dswap = dswap; this._dtrsm = dtrsm; 
            this._xerbla = xerbla;

            #endregion

        }
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DGBTRF computes an LU factorization of a real m-by-n band matrix A
        /// using partial pivoting with row interchanges.
        /// 
        /// This is the blocked version of the algorithm, calling Level 3 BLAS.
        /// 
        ///</summary>
        /// <param name="M">
        /// (input) INTEGER
        /// The number of rows of the matrix A.  M .GE. 0.
        ///</param>
        /// <param name="N">
        /// (input) INTEGER
        /// The number of columns of the matrix A.  N .GE. 0.
        ///</param>
        /// <param name="KL">
        /// (input) INTEGER
        /// The number of subdiagonals within the band of A.  KL .GE. 0.
        ///</param>
        /// <param name="KU">
        /// (input) INTEGER
        /// The number of superdiagonals within the band of A.  KU .GE. 0.
        ///</param>
        /// <param name="AB">
        /// (input/output) DOUBLE PRECISION array, dimension (LDAB,N)
        /// On entry, the matrix A in band storage, in rows KL+1 to
        /// 2*KL+KU+1; rows 1 to KL of the array need not be set.
        /// The j-th column of A is stored in the j-th column of the
        /// array AB as follows:
        /// AB(kl+ku+1+i-j,j) = A(i,j) for max(1,j-ku).LE.i.LE.min(m,j+kl)
        /// 
        /// On exit, details of the factorization: U is stored as an
        /// upper triangular band matrix with KL+KU superdiagonals in
        /// rows 1 to KL+KU+1, and the multipliers used during the
        /// factorization are stored in rows KL+KU+2 to 2*KL+KU+1.
        /// See below for further details.
        ///</param>
        /// <param name="LDAB">
        /// (input) INTEGER
        /// The leading dimension of the array AB.  LDAB .GE. 2*KL+KU+1.
        ///</param>
        /// <param name="IPIV">
        /// (output) INTEGER array, dimension (min(M,N))
        /// The pivot indices; for 1 .LE. i .LE. min(M,N), row i of the
        /// matrix was interchanged with row IPIV(i).
        ///</param>
        /// <param name="INFO">
        /// (output) INTEGER
        /// = 0: successful exit
        /// .LT. 0: if INFO = -i, the i-th argument had an illegal value
        /// .GT. 0: if INFO = +i, U(i,i) is exactly zero. The factorization
        /// has been completed, but the factor U is exactly
        /// singular, and division by zero will occur if it is used
        /// to solve a system of equations.
        ///</param>
        public void Run(int M, int N, int KL, int KU, ref double[] AB, int offset_ab, int LDAB
                         , ref int[] IPIV, int offset_ipiv, ref int INFO)
        {

            #region Variables
            
            int I = 0; int I2 = 0; int I3 = 0; int II = 0; int IP = 0; int J = 0; int J2 = 0; int J3 = 0; int JB = 0; int JJ = 0; 
            int JM = 0;int JP = 0; int JU = 0; int K2 = 0; int KM = 0; int KV = 0; int NB = 0; int NW = 0; double TEMP = 0; 
            int offset_work13 = 0; int o_work13 = -1 - LDWORK;int offset_work31 = 0; int o_work31 = -1 - LDWORK; 

            #endregion


            #region Implicit Variables
            
            int WORK13_J = 0; int WORK31_J = 0; int AB_J = 0; int AB_0 = 0; int WORK13_JJ = 0; int AB_1 = 0; int AB_2 = 0; 

            #endregion


            #region Array Index Correction
            
             int o_ab = -1 - LDAB + offset_ab;  int o_ipiv = -1 + offset_ipiv; 

            #endregion


            #region Prolog
            
            // *
            // *  -- LAPACK routine (version 3.1) --
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
            // *  DGBTRF computes an LU factorization of a real m-by-n band matrix A
            // *  using partial pivoting with row interchanges.
            // *
            // *  This is the blocked version of the algorithm, calling Level 3 BLAS.
            // *
            // *  Arguments
            // *  =========
            // *
            // *  M       (input) INTEGER
            // *          The number of rows of the matrix A.  M >= 0.
            // *
            // *  N       (input) INTEGER
            // *          The number of columns of the matrix A.  N >= 0.
            // *
            // *  KL      (input) INTEGER
            // *          The number of subdiagonals within the band of A.  KL >= 0.
            // *
            // *  KU      (input) INTEGER
            // *          The number of superdiagonals within the band of A.  KU >= 0.
            // *
            // *  AB      (input/output) DOUBLE PRECISION array, dimension (LDAB,N)
            // *          On entry, the matrix A in band storage, in rows KL+1 to
            // *          2*KL+KU+1; rows 1 to KL of the array need not be set.
            // *          The j-th column of A is stored in the j-th column of the
            // *          array AB as follows:
            // *          AB(kl+ku+1+i-j,j) = A(i,j) for max(1,j-ku)<=i<=min(m,j+kl)
            // *
            // *          On exit, details of the factorization: U is stored as an
            // *          upper triangular band matrix with KL+KU superdiagonals in
            // *          rows 1 to KL+KU+1, and the multipliers used during the
            // *          factorization are stored in rows KL+KU+2 to 2*KL+KU+1.
            // *          See below for further details.
            // *
            // *  LDAB    (input) INTEGER
            // *          The leading dimension of the array AB.  LDAB >= 2*KL+KU+1.
            // *
            // *  IPIV    (output) INTEGER array, dimension (min(M,N))
            // *          The pivot indices; for 1 <= i <= min(M,N), row i of the
            // *          matrix was interchanged with row IPIV(i).
            // *
            // *  INFO    (output) INTEGER
            // *          = 0: successful exit
            // *          < 0: if INFO = -i, the i-th argument had an illegal value
            // *          > 0: if INFO = +i, U(i,i) is exactly zero. The factorization
            // *               has been completed, but the factor U is exactly
            // *               singular, and division by zero will occur if it is used
            // *               to solve a system of equations.
            // *
            // *  Further Details
            // *  ===============
            // *
            // *  The band storage scheme is illustrated by the following example, when
            // *  M = N = 6, KL = 2, KU = 1:
            // *
            // *  On entry:                       On exit:
            // *
            // *      *    *    *    +    +    +       *    *    *   u14  u25  u36
            // *      *    *    +    +    +    +       *    *   u13  u24  u35  u46
            // *      *   a12  a23  a34  a45  a56      *   u12  u23  u34  u45  u56
            // *     a11  a22  a33  a44  a55  a66     u11  u22  u33  u44  u55  u66
            // *     a21  a32  a43  a54  a65   *      m21  m32  m43  m54  m65   *
            // *     a31  a42  a53  a64   *    *      m31  m42  m53  m64   *    *
            // *
            // *  Array elements marked * are not used by the routine; elements marked
            // *  + need not be set on entry, but are required by the routine to store
            // *  elements of U because of fill-in resulting from the row interchanges.
            // *
            // *  =====================================================================
            // *
            // *     .. Parameters ..
            // *     ..
            // *     .. Local Scalars ..
            // *     ..
            // *     .. Local Arrays ..
            // *     ..
            // *     .. External Functions ..
            // *     ..
            // *     .. External Subroutines ..
            // *     ..
            // *     .. Intrinsic Functions ..
            //      INTRINSIC          MAX, MIN;
            // *     ..
            // *     .. Executable Statements ..
            // *
            // *     KV is the number of superdiagonals in the factor U, allowing for
            // *     fill-in
            // *

            #endregion


            #region Body
            
            KV = KU + KL;
            // *
            // *     Test the input parameters.
            // *
            INFO = 0;
            if (M < 0)
            {
                INFO =  - 1;
            }
            else
            {
                if (N < 0)
                {
                    INFO =  - 2;
                }
                else
                {
                    if (KL < 0)
                    {
                        INFO =  - 3;
                    }
                    else
                    {
                        if (KU < 0)
                        {
                            INFO =  - 4;
                        }
                        else
                        {
                            if (LDAB < KL + KV + 1)
                            {
                                INFO =  - 6;
                            }
                        }
                    }
                }
            }
            if (INFO != 0)
            {
                this._xerbla.Run("DGBTRF",  - INFO);
                return;
            }
            // *
            // *     Quick return if possible
            // *
            if (M == 0 || N == 0) return;
            // *
            // *     Determine the block size for this environment
            // *
            NB = this._ilaenv.Run(1, "DGBTRF", " ", M, N, KL, KU);
            // *
            // *     The block size must not exceed the limit set by the size of the
            // *     local arrays WORK13 and WORK31.
            // *
            NB = Math.Min(NB, NBMAX);
            // *
            if (NB <= 1 || NB > KL)
            {
                // *
                // *        Use unblocked code
                // *
                this._dgbtf2.Run(M, N, KL, KU, ref AB, offset_ab, LDAB
                                 , ref IPIV, offset_ipiv, ref INFO);
            }
            else
            {
                // *
                // *        Use blocked code
                // *
                // *        Zero the superdiagonal elements of the work array WORK13
                // *
                for (J = 1; J <= NB; J++)
                {
                    WORK13_J = J * LDWORK + o_work13;
                    for (I = 1; I <= J - 1; I++)
                    {
                        WORK13[I + WORK13_J] = ZERO;
                    }
                }
                // *
                // *        Zero the subdiagonal elements of the work array WORK31
                // *
                for (J = 1; J <= NB; J++)
                {
                    WORK31_J = J * LDWORK + o_work31;
                    for (I = J + 1; I <= NB; I++)
                    {
                        WORK31[I + WORK31_J] = ZERO;
                    }
                }
                // *
                // *        Gaussian elimination with partial pivoting
                // *
                // *        Set fill-in elements in columns KU+2 to KV to zero
                // *
                for (J = KU + 2; J <= Math.Min(KV, N); J++)
                {
                    AB_J = J * LDAB + o_ab;
                    for (I = KV - J + 2; I <= KL; I++)
                    {
                        AB[I + AB_J] = ZERO;
                    }
                }
                // *
                // *        JU is the index of the last column affected by the current
                // *        stage of the factorization
                // *
                JU = 1;
                // *
                for (J = 1; (NB >= 0) ? (J <= Math.Min(M, N)) : (J >= Math.Min(M, N)); J += NB)
                {
                    JB = Math.Min(NB, Math.Min(M, N) - J + 1);
                    // *
                    // *           The active part of the matrix is partitioned
                    // *
                    // *              A11   A12   A13
                    // *              A21   A22   A23
                    // *              A31   A32   A33
                    // *
                    // *           Here A11, A21 and A31 denote the current block of JB columns
                    // *           which is about to be factorized. The number of rows in the
                    // *           partitioning are JB, I2, I3 respectively, and the numbers
                    // *           of columns are JB, J2, J3. The superdiagonal elements of A13
                    // *           and the subdiagonal elements of A31 lie outside the band.
                    // *
                    I2 = Math.Min(KL - JB, M - J - JB + 1);
                    I3 = Math.Min(JB, M - J - KL + 1);
                    // *
                    // *           J2 and J3 are computed after JU has been updated.
                    // *
                    // *           Factorize the current block of JB columns
                    // *
                    for (JJ = J; JJ <= J + JB - 1; JJ++)
                    {
                        // *
                        // *              Set fill-in elements in column JJ+KV to zero
                        // *
                        if (JJ + KV <= N)
                        {
                            AB_0 = (JJ + KV) * LDAB + o_ab;
                            for (I = 1; I <= KL; I++)
                            {
                                AB[I + AB_0] = ZERO;
                            }
                        }
                        // *
                        // *              Find pivot and test for singularity. KM is the number of
                        // *              subdiagonal elements in the current column.
                        // *
                        KM = Math.Min(KL, M - JJ);
                        JP = this._idamax.Run(KM + 1, AB, KV + 1+JJ * LDAB + o_ab, 1);
                        IPIV[JJ + o_ipiv] = JP + JJ - J;
                        if (AB[KV + JP+JJ * LDAB + o_ab] != ZERO)
                        {
                            JU = Math.Max(JU, Math.Min(JJ + KU + JP - 1, N));
                            if (JP != 1)
                            {
                                // *
                                // *                    Apply interchange to columns J to J+JB-1
                                // *
                                if (JP + JJ - 1 < J + KL)
                                {
                                    // *
                                    this._dswap.Run(JB, ref AB, KV + 1 + JJ - J+J * LDAB + o_ab, LDAB - 1, ref AB, KV + JP + JJ - J+J * LDAB + o_ab, LDAB - 1);
                                }
                                else
                                {
                                    // *
                                    // *                       The interchange affects columns J to JJ-1 of A31
                                    // *                       which are stored in the work array WORK31
                                    // *
                                    this._dswap.Run(JJ - J, ref AB, KV + 1 + JJ - J+J * LDAB + o_ab, LDAB - 1, ref WORK31, JP + JJ - J - KL+1 * LDWORK + o_work31, LDWORK);
                                    this._dswap.Run(J + JB - JJ, ref AB, KV + 1+JJ * LDAB + o_ab, LDAB - 1, ref AB, KV + JP+JJ * LDAB + o_ab, LDAB - 1);
                                }
                            }
                            // *
                            // *                 Compute multipliers
                            // *
                            this._dscal.Run(KM, ONE / AB[KV + 1+JJ * LDAB + o_ab], ref AB, KV + 2+JJ * LDAB + o_ab, 1);
                            // *
                            // *                 Update trailing submatrix within the band and within
                            // *                 the current block. JM is the index of the last column
                            // *                 which needs to be updated.
                            // *
                            JM = Math.Min(JU, J + JB - 1);
                            if (JM > JJ)
                            {
                                this._dger.Run(KM, JM - JJ,  - ONE, AB, KV + 2+JJ * LDAB + o_ab, 1, AB, KV+(JJ + 1) * LDAB + o_ab
                                               , LDAB - 1, ref AB, KV + 1+(JJ + 1) * LDAB + o_ab, LDAB - 1);
                            }
                        }
                        else
                        {
                            // *
                            // *                 If pivot is zero, set INFO to the index of the pivot
                            // *                 unless a zero pivot has already been found.
                            // *
                            if (INFO == 0) INFO = JJ;
                        }
                        // *
                        // *              Copy current column of A31 into the work array WORK31
                        // *
                        NW = Math.Min(JJ - J + 1, I3);
                        if (NW > 0) this._dcopy.Run(NW, AB, KV + KL + 1 - JJ + J+JJ * LDAB + o_ab, 1, ref WORK31, 1+(JJ - J + 1) * LDWORK + o_work31, 1);
                    }
                    if (J + JB <= N)
                    {
                        // *
                        // *              Apply the row interchanges to the other blocks.
                        // *
                        J2 = Math.Min(JU - J + 1, KV) - JB;
                        J3 = Math.Max(0, JU - J - KV + 1);
                        // *
                        // *              Use DLASWP to apply the row interchanges to A12, A22, and
                        // *              A32.
                        // *
                        this._dlaswp.Run(J2, ref AB, KV + 1 - JB+(J + JB) * LDAB + o_ab, LDAB - 1, 1, JB, IPIV, J + o_ipiv
                                         , 1);
                        // *
                        // *              Adjust the pivot indices.
                        // *
                        for (I = J; I <= J + JB - 1; I++)
                        {
                            IPIV[I + o_ipiv] += J - 1;
                        }
                        // *
                        // *              Apply the row interchanges to A13, A23, and A33
                        // *              columnwise.
                        // *
                        K2 = J - 1 + JB + J2;
                        for (I = 1; I <= J3; I++)
                        {
                            JJ = K2 + I;
                            for (II = J + I - 1; II <= J + JB - 1; II++)
                            {
                                IP = IPIV[II + o_ipiv];
                                if (IP != II)
                                {
                                    TEMP = AB[KV + 1 + II - JJ+JJ * LDAB + o_ab];
                                    AB[KV + 1 + II - JJ+JJ * LDAB + o_ab] = AB[KV + 1 + IP - JJ+JJ * LDAB + o_ab];
                                    AB[KV + 1 + IP - JJ+JJ * LDAB + o_ab] = TEMP;
                                }
                            }
                        }
                        // *
                        // *              Update the relevant part of the trailing submatrix
                        // *
                        if (J2 > 0)
                        {
                            // *
                            // *                 Update A12
                            // *
                            this._dtrsm.Run("Left", "Lower", "No transpose", "Unit", JB, J2
                                            , ONE, AB, KV + 1+J * LDAB + o_ab, LDAB - 1, ref AB, KV + 1 - JB+(J + JB) * LDAB + o_ab, LDAB - 1);
                            // *
                            if (I2 > 0)
                            {
                                // *
                                // *                    Update A22
                                // *
                                this._dgemm.Run("No transpose", "No transpose", I2, J2, JB,  - ONE
                                                , AB, KV + 1 + JB+J * LDAB + o_ab, LDAB - 1, AB, KV + 1 - JB+(J + JB) * LDAB + o_ab, LDAB - 1, ONE, ref AB, KV + 1+(J + JB) * LDAB + o_ab
                                                , LDAB - 1);
                            }
                            // *
                            if (I3 > 0)
                            {
                                // *
                                // *                    Update A32
                                // *
                                this._dgemm.Run("No transpose", "No transpose", I3, J2, JB,  - ONE
                                                , WORK31, offset_work31, LDWORK, AB, KV + 1 - JB+(J + JB) * LDAB + o_ab, LDAB - 1, ONE, ref AB, KV + KL + 1 - JB+(J + JB) * LDAB + o_ab
                                                , LDAB - 1);
                            }
                        }
                        // *
                        if (J3 > 0)
                        {
                            // *
                            // *                 Copy the lower triangle of A13 into the work array
                            // *                 WORK13
                            // *
                            for (JJ = 1; JJ <= J3; JJ++)
                            {
                                WORK13_JJ = JJ * LDWORK + o_work13;
                                AB_1 = (JJ + J + KV - 1) * LDAB + o_ab;
                                for (II = JJ; II <= JB; II++)
                                {
                                    WORK13[II + WORK13_JJ] = AB[II - JJ + 1 + AB_1];
                                }
                            }
                            // *
                            // *                 Update A13 in the work array
                            // *
                            this._dtrsm.Run("Left", "Lower", "No transpose", "Unit", JB, J3
                                            , ONE, AB, KV + 1+J * LDAB + o_ab, LDAB - 1, ref WORK13, offset_work13, LDWORK);
                            // *
                            if (I2 > 0)
                            {
                                // *
                                // *                    Update A23
                                // *
                                this._dgemm.Run("No transpose", "No transpose", I2, J3, JB,  - ONE
                                                , AB, KV + 1 + JB+J * LDAB + o_ab, LDAB - 1, WORK13, offset_work13, LDWORK, ONE, ref AB, 1 + JB+(J + KV) * LDAB + o_ab
                                                , LDAB - 1);
                            }
                            // *
                            if (I3 > 0)
                            {
                                // *
                                // *                    Update A33
                                // *
                                this._dgemm.Run("No transpose", "No transpose", I3, J3, JB,  - ONE
                                                , WORK31, offset_work31, LDWORK, WORK13, offset_work13, LDWORK, ONE, ref AB, 1 + KL+(J + KV) * LDAB + o_ab
                                                , LDAB - 1);
                            }
                            // *
                            // *                 Copy the lower triangle of A13 back into place
                            // *
                            for (JJ = 1; JJ <= J3; JJ++)
                            {
                                AB_2 = (JJ + J + KV - 1) * LDAB + o_ab;
                                WORK13_JJ = JJ * LDWORK + o_work13;
                                for (II = JJ; II <= JB; II++)
                                {
                                    AB[II - JJ + 1 + AB_2] = WORK13[II + WORK13_JJ];
                                }
                            }
                        }
                    }
                    else
                    {
                        // *
                        // *              Adjust the pivot indices.
                        // *
                        for (I = J; I <= J + JB - 1; I++)
                        {
                            IPIV[I + o_ipiv] += J - 1;
                        }
                    }
                    // *
                    // *           Partially undo the interchanges in the current block to
                    // *           restore the upper triangular form of A31 and copy the upper
                    // *           triangle of A31 back into place
                    // *
                    for (JJ = J + JB - 1; JJ >= J; JJ +=  - 1)
                    {
                        JP = IPIV[JJ + o_ipiv] - JJ + 1;
                        if (JP != 1)
                        {
                            // *
                            // *                 Apply interchange to columns J to JJ-1
                            // *
                            if (JP + JJ - 1 < J + KL)
                            {
                                // *
                                // *                    The interchange does not affect A31
                                // *
                                this._dswap.Run(JJ - J, ref AB, KV + 1 + JJ - J+J * LDAB + o_ab, LDAB - 1, ref AB, KV + JP + JJ - J+J * LDAB + o_ab, LDAB - 1);
                            }
                            else
                            {
                                // *
                                // *                    The interchange does affect A31
                                // *
                                this._dswap.Run(JJ - J, ref AB, KV + 1 + JJ - J+J * LDAB + o_ab, LDAB - 1, ref WORK31, JP + JJ - J - KL+1 * LDWORK + o_work31, LDWORK);
                            }
                        }
                        // *
                        // *              Copy the current column of A31 back into place
                        // *
                        NW = Math.Min(I3, JJ - J + 1);
                        if (NW > 0) this._dcopy.Run(NW, WORK31, 1+(JJ - J + 1) * LDWORK + o_work31, 1, ref AB, KV + KL + 1 - JJ + J+JJ * LDAB + o_ab, 1);
                    }
                }
            }
            // *
            return;
            // *
            // *     End of DGBTRF
            // *

            #endregion

        }
    }
}
