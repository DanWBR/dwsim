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
    /// DSBTRD reduces a real symmetric band matrix A to symmetric
    /// tridiagonal form T by an orthogonal similarity transformation:
    /// Q**T * A * Q = T.
    /// 
    ///</summary>
    public class DSBTRD
    {
    

        #region Dependencies
        
        DLAR2V _dlar2v; DLARGV _dlargv; DLARTG _dlartg; DLARTV _dlartv; DLASET _dlaset; DROT _drot; XERBLA _xerbla; LSAME _lsame; 

        #endregion


        #region Variables
        
        const double ZERO = 0.0E+0; const double ONE = 1.0E+0; 

        #endregion

        public DSBTRD(DLAR2V dlar2v, DLARGV dlargv, DLARTG dlartg, DLARTV dlartv, DLASET dlaset, DROT drot, XERBLA xerbla, LSAME lsame)
        {
    

            #region Set Dependencies
            
            this._dlar2v = dlar2v; this._dlargv = dlargv; this._dlartg = dlartg; this._dlartv = dlartv; this._dlaset = dlaset; 
            this._drot = drot;this._xerbla = xerbla; this._lsame = lsame; 

            #endregion

        }
    
        public DSBTRD()
        {
    

            #region Dependencies (Initialization)
            
            DLAR2V dlar2v = new DLAR2V();
            DLARGV dlargv = new DLARGV();
            LSAME lsame = new LSAME();
            DLAMC3 dlamc3 = new DLAMC3();
            DLARTV dlartv = new DLARTV();
            DROT drot = new DROT();
            XERBLA xerbla = new XERBLA();
            DLAMC1 dlamc1 = new DLAMC1(dlamc3);
            DLAMC4 dlamc4 = new DLAMC4(dlamc3);
            DLAMC5 dlamc5 = new DLAMC5(dlamc3);
            DLAMC2 dlamc2 = new DLAMC2(dlamc3, dlamc1, dlamc4, dlamc5);
            DLAMCH dlamch = new DLAMCH(lsame, dlamc2);
            DLARTG dlartg = new DLARTG(dlamch);
            DLASET dlaset = new DLASET(lsame);

            #endregion


            #region Set Dependencies
            
            this._dlar2v = dlar2v; this._dlargv = dlargv; this._dlartg = dlartg; this._dlartv = dlartv; this._dlaset = dlaset; 
            this._drot = drot;this._xerbla = xerbla; this._lsame = lsame; 

            #endregion

        }
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DSBTRD reduces a real symmetric band matrix A to symmetric
        /// tridiagonal form T by an orthogonal similarity transformation:
        /// Q**T * A * Q = T.
        /// 
        ///</summary>
        /// <param name="VECT">
        /// (input) CHARACTER*1
        /// = 'N':  do not form Q;
        /// = 'V':  form Q;
        /// = 'U':  update a matrix X, by forming X*Q.
        ///</param>
        /// <param name="UPLO">
        /// (input) CHARACTER*1
        /// = 'U':  Upper triangle of A is stored;
        /// = 'L':  Lower triangle of A is stored.
        ///</param>
        /// <param name="N">
        /// (input) INTEGER
        /// The order of the matrix A.  N .GE. 0.
        ///</param>
        /// <param name="KD">
        /// (input) INTEGER
        /// The number of superdiagonals of the matrix A if UPLO = 'U',
        /// or the number of subdiagonals if UPLO = 'L'.  KD .GE. 0.
        ///</param>
        /// <param name="AB">
        /// (input/output) DOUBLE PRECISION array, dimension (LDAB,N)
        /// On entry, the upper or lower triangle of the symmetric band
        /// matrix A, stored in the first KD+1 rows of the array.  The
        /// j-th column of A is stored in the j-th column of the array AB
        /// as follows:
        /// if UPLO = 'U', AB(kd+1+i-j,j) = A(i,j) for max(1,j-kd).LE.i.LE.j;
        /// if UPLO = 'L', AB(1+i-j,j)    = A(i,j) for j.LE.i.LE.min(n,j+kd).
        /// On exit, the diagonal elements of AB are overwritten by the
        /// diagonal elements of the tridiagonal matrix T; if KD .GT. 0, the
        /// elements on the first superdiagonal (if UPLO = 'U') or the
        /// first subdiagonal (if UPLO = 'L') are overwritten by the
        /// off-diagonal elements of T; the rest of AB is overwritten by
        /// values generated during the reduction.
        ///</param>
        /// <param name="LDAB">
        /// (input) INTEGER
        /// The leading dimension of the array AB.  LDAB .GE. KD+1.
        ///</param>
        /// <param name="D">
        /// (output) DOUBLE PRECISION array, dimension (N)
        /// The diagonal elements of the tridiagonal matrix T.
        ///</param>
        /// <param name="E">
        /// (output) DOUBLE PRECISION array, dimension (N-1)
        /// The off-diagonal elements of the tridiagonal matrix T:
        /// E(i) = T(i,i+1) if UPLO = 'U'; E(i) = T(i+1,i) if UPLO = 'L'.
        ///</param>
        /// <param name="Q">
        /// (input/output) DOUBLE PRECISION array, dimension (LDQ,N)
        /// On entry, if VECT = 'U', then Q must contain an N-by-N
        /// matrix X; if VECT = 'N' or 'V', then Q need not be set.
        /// 
        /// On exit:
        /// if VECT = 'V', Q contains the N-by-N orthogonal matrix Q;
        /// if VECT = 'U', Q contains the product X*Q;
        /// if VECT = 'N', the array Q is not referenced.
        ///</param>
        /// <param name="LDQ">
        /// (input) INTEGER
        /// The leading dimension of the array Q.
        /// LDQ .GE. 1, and LDQ .GE. N if VECT = 'V' or 'U'.
        ///</param>
        /// <param name="WORK">
        /// (workspace) DOUBLE PRECISION array, dimension (N)
        ///</param>
        /// <param name="INFO">
        /// (output) INTEGER
        /// = 0:  successful exit
        /// .LT. 0:  if INFO = -i, the i-th argument had an illegal value
        ///</param>
        public void Run(string VECT, string UPLO, int N, int KD, ref double[] AB, int offset_ab, int LDAB
                         , ref double[] D, int offset_d, ref double[] E, int offset_e, ref double[] Q, int offset_q, int LDQ, ref double[] WORK, int offset_work, ref int INFO)
        {

            #region Variables
            
            bool INITQ = false; bool UPPER = false; bool WANTQ = false; int I = 0; int I2 = 0; int IBL = 0; int INCA = 0; 
            int INCX = 0;int IQAEND = 0; int IQB = 0; int IQEND = 0; int J = 0; int J1 = 0; int J1END = 0; int J1INC = 0; 
            int J2 = 0;int JEND = 0; int JIN = 0; int JINC = 0; int K = 0; int KD1 = 0; int KDM1 = 0; int KDN = 0; int L = 0; 
            int LAST = 0;int LEND = 0; int NQ = 0; int NR = 0; int NRT = 0; double TEMP = 0; 

            #endregion


            #region Array Index Correction
            
             int o_ab = -1 - LDAB + offset_ab;  int o_d = -1 + offset_d;  int o_e = -1 + offset_e;  int o_q = -1 - LDQ + offset_q; 
             int o_work = -1 + offset_work;

            #endregion


            #region Strings
            
            VECT = VECT.Substring(0, 1);  UPLO = UPLO.Substring(0, 1);  

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
            // *  DSBTRD reduces a real symmetric band matrix A to symmetric
            // *  tridiagonal form T by an orthogonal similarity transformation:
            // *  Q**T * A * Q = T.
            // *
            // *  Arguments
            // *  =========
            // *
            // *  VECT    (input) CHARACTER*1
            // *          = 'N':  do not form Q;
            // *          = 'V':  form Q;
            // *          = 'U':  update a matrix X, by forming X*Q.
            // *
            // *  UPLO    (input) CHARACTER*1
            // *          = 'U':  Upper triangle of A is stored;
            // *          = 'L':  Lower triangle of A is stored.
            // *
            // *  N       (input) INTEGER
            // *          The order of the matrix A.  N >= 0.
            // *
            // *  KD      (input) INTEGER
            // *          The number of superdiagonals of the matrix A if UPLO = 'U',
            // *          or the number of subdiagonals if UPLO = 'L'.  KD >= 0.
            // *
            // *  AB      (input/output) DOUBLE PRECISION array, dimension (LDAB,N)
            // *          On entry, the upper or lower triangle of the symmetric band
            // *          matrix A, stored in the first KD+1 rows of the array.  The
            // *          j-th column of A is stored in the j-th column of the array AB
            // *          as follows:
            // *          if UPLO = 'U', AB(kd+1+i-j,j) = A(i,j) for max(1,j-kd)<=i<=j;
            // *          if UPLO = 'L', AB(1+i-j,j)    = A(i,j) for j<=i<=min(n,j+kd).
            // *          On exit, the diagonal elements of AB are overwritten by the
            // *          diagonal elements of the tridiagonal matrix T; if KD > 0, the
            // *          elements on the first superdiagonal (if UPLO = 'U') or the
            // *          first subdiagonal (if UPLO = 'L') are overwritten by the
            // *          off-diagonal elements of T; the rest of AB is overwritten by
            // *          values generated during the reduction.
            // *
            // *  LDAB    (input) INTEGER
            // *          The leading dimension of the array AB.  LDAB >= KD+1.
            // *
            // *  D       (output) DOUBLE PRECISION array, dimension (N)
            // *          The diagonal elements of the tridiagonal matrix T.
            // *
            // *  E       (output) DOUBLE PRECISION array, dimension (N-1)
            // *          The off-diagonal elements of the tridiagonal matrix T:
            // *          E(i) = T(i,i+1) if UPLO = 'U'; E(i) = T(i+1,i) if UPLO = 'L'.
            // *
            // *  Q       (input/output) DOUBLE PRECISION array, dimension (LDQ,N)
            // *          On entry, if VECT = 'U', then Q must contain an N-by-N
            // *          matrix X; if VECT = 'N' or 'V', then Q need not be set.
            // *
            // *          On exit:
            // *          if VECT = 'V', Q contains the N-by-N orthogonal matrix Q;
            // *          if VECT = 'U', Q contains the product X*Q;
            // *          if VECT = 'N', the array Q is not referenced.
            // *
            // *  LDQ     (input) INTEGER
            // *          The leading dimension of the array Q.
            // *          LDQ >= 1, and LDQ >= N if VECT = 'V' or 'U'.
            // *
            // *  WORK    (workspace) DOUBLE PRECISION array, dimension (N)
            // *
            // *  INFO    (output) INTEGER
            // *          = 0:  successful exit
            // *          < 0:  if INFO = -i, the i-th argument had an illegal value
            // *
            // *  Further Details
            // *  ===============
            // *
            // *  Modified by Linda Kaufman, Bell Labs.
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
            //      INTRINSIC          MAX, MIN;
            // *     ..
            // *     .. External Functions ..
            // *     ..
            // *     .. Executable Statements ..
            // *
            // *     Test the input parameters
            // *

            #endregion


            #region Body
            
            INITQ = this._lsame.Run(VECT, "V");
            WANTQ = INITQ || this._lsame.Run(VECT, "U");
            UPPER = this._lsame.Run(UPLO, "U");
            KD1 = KD + 1;
            KDM1 = KD - 1;
            INCX = LDAB - 1;
            IQEND = 1;
            // *
            INFO = 0;
            if (!WANTQ && !this._lsame.Run(VECT, "N"))
            {
                INFO =  - 1;
            }
            else
            {
                if (!UPPER && !this._lsame.Run(UPLO, "L"))
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
                        if (KD < 0)
                        {
                            INFO =  - 4;
                        }
                        else
                        {
                            if (LDAB < KD1)
                            {
                                INFO =  - 6;
                            }
                            else
                            {
                                if (LDQ < Math.Max(1, N) && WANTQ)
                                {
                                    INFO =  - 10;
                                }
                            }
                        }
                    }
                }
            }
            if (INFO != 0)
            {
                this._xerbla.Run("DSBTRD",  - INFO);
                return;
            }
            // *
            // *     Quick return if possible
            // *
            if (N == 0) return;
            // *
            // *     Initialize Q to the unit matrix, if needed
            // *
            if (INITQ)
            {
                this._dlaset.Run("Full", N, N, ZERO, ONE, ref Q, offset_q
                                 , LDQ);
            }
            // *
            // *     Wherever possible, plane rotations are generated and applied in
            // *     vector operations of length NR over the index set J1:J2:KD1.
            // *
            // *     The cosines and sines of the plane rotations are stored in the
            // *     arrays D and WORK.
            // *
            INCA = KD1 * LDAB;
            KDN = Math.Min(N - 1, KD);
            if (UPPER)
            {
                // *
                if (KD > 1)
                {
                    // *
                    // *           Reduce to tridiagonal form, working with upper triangle
                    // *
                    NR = 0;
                    J1 = KDN + 2;
                    J2 = 1;
                    // *
                    for (I = 1; I <= N - 2; I++)
                    {
                        // *
                        // *              Reduce i-th row of matrix to tridiagonal form
                        // *
                        for (K = KDN + 1; K >= 2; K +=  - 1)
                        {
                            J1 += KDN;
                            J2 += KDN;
                            // *
                            if (NR > 0)
                            {
                                // *
                                // *                    generate plane rotations to annihilate nonzero
                                // *                    elements which have been created outside the band
                                // *
                                this._dlargv.Run(NR, ref AB, 1+(J1 - 1) * LDAB + o_ab, INCA, ref WORK, J1 + o_work, KD1, ref D, J1 + o_d
                                                 , KD1);
                                // *
                                // *                    apply rotations from the right
                                // *
                                // *
                                // *                    Dependent on the the number of diagonals either
                                // *                    DLARTV or DROT is used
                                // *
                                if (NR >= 2 * KD - 1)
                                {
                                    for (L = 1; L <= KD - 1; L++)
                                    {
                                        this._dlartv.Run(NR, ref AB, L + 1+(J1 - 1) * LDAB + o_ab, INCA, ref AB, L+J1 * LDAB + o_ab, INCA, D, J1 + o_d
                                                         , WORK, J1 + o_work, KD1);
                                    }
                                    // *
                                }
                                else
                                {
                                    JEND = J1 + (NR - 1) * KD1;
                                    for (JINC = J1; (KD1 >= 0) ? (JINC <= JEND) : (JINC >= JEND); JINC += KD1)
                                    {
                                        this._drot.Run(KDM1, ref AB, 2+(JINC - 1) * LDAB + o_ab, 1, ref AB, 1+JINC * LDAB + o_ab, 1, D[JINC + o_d]
                                                       , WORK[JINC + o_work]);
                                    }
                                }
                            }
                            // *
                            // *
                            if (K > 2)
                            {
                                if (K <= N - I + 1)
                                {
                                    // *
                                    // *                       generate plane rotation to annihilate a(i,i+k-1)
                                    // *                       within the band
                                    // *
                                    this._dlartg.Run(AB[KD - K + 3+(I + K - 2) * LDAB + o_ab], AB[KD - K + 2+(I + K - 1) * LDAB + o_ab], ref D[I + K - 1 + o_d], ref WORK[I + K - 1 + o_work], ref TEMP);
                                    AB[KD - K + 3+(I + K - 2) * LDAB + o_ab] = TEMP;
                                    // *
                                    // *                       apply rotation from the right
                                    // *
                                    this._drot.Run(K - 3, ref AB, KD - K + 4+(I + K - 2) * LDAB + o_ab, 1, ref AB, KD - K + 3+(I + K - 1) * LDAB + o_ab, 1, D[I + K - 1 + o_d]
                                                   , WORK[I + K - 1 + o_work]);
                                }
                                NR += 1;
                                J1 +=  - KDN - 1;
                            }
                            // *
                            // *                 apply plane rotations from both sides to diagonal
                            // *                 blocks
                            // *
                            if (NR > 0)
                            {
                                this._dlar2v.Run(NR, ref AB, KD1+(J1 - 1) * LDAB + o_ab, ref AB, KD1+J1 * LDAB + o_ab, ref AB, KD+J1 * LDAB + o_ab, INCA, D, J1 + o_d
                                                 , WORK, J1 + o_work, KD1);
                            }
                            // *
                            // *                 apply plane rotations from the left
                            // *
                            if (NR > 0)
                            {
                                if (2 * KD - 1 < NR)
                                {
                                    // *
                                    // *                    Dependent on the the number of diagonals either
                                    // *                    DLARTV or DROT is used
                                    // *
                                    for (L = 1; L <= KD - 1; L++)
                                    {
                                        if (J2 + L > N)
                                        {
                                            NRT = NR - 1;
                                        }
                                        else
                                        {
                                            NRT = NR;
                                        }
                                        if (NRT > 0)
                                        {
                                            this._dlartv.Run(NRT, ref AB, KD - L+(J1 + L) * LDAB + o_ab, INCA, ref AB, KD - L + 1+(J1 + L) * LDAB + o_ab, INCA, D, J1 + o_d
                                                             , WORK, J1 + o_work, KD1);
                                        }
                                    }
                                }
                                else
                                {
                                    J1END = J1 + KD1 * (NR - 2);
                                    if (J1END >= J1)
                                    {
                                        for (JIN = J1; (KD1 >= 0) ? (JIN <= J1END) : (JIN >= J1END); JIN += KD1)
                                        {
                                            this._drot.Run(KD - 1, ref AB, KD - 1+(JIN + 1) * LDAB + o_ab, INCX, ref AB, KD+(JIN + 1) * LDAB + o_ab, INCX, D[JIN + o_d]
                                                           , WORK[JIN + o_work]);
                                        }
                                    }
                                    LEND = Math.Min(KDM1, N - J2);
                                    LAST = J1END + KD1;
                                    if (LEND > 0)
                                    {
                                        this._drot.Run(LEND, ref AB, KD - 1+(LAST + 1) * LDAB + o_ab, INCX, ref AB, KD+(LAST + 1) * LDAB + o_ab, INCX, D[LAST + o_d]
                                                       , WORK[LAST + o_work]);
                                    }
                                }
                            }
                            // *
                            if (WANTQ)
                            {
                                // *
                                // *                    accumulate product of plane rotations in Q
                                // *
                                if (INITQ)
                                {
                                    // *
                                    // *                 take advantage of the fact that Q was
                                    // *                 initially the Identity matrix
                                    // *
                                    IQEND = Math.Max(IQEND, J2);
                                    I2 = Math.Max(0, K - 3);
                                    IQAEND = 1 + I * KD;
                                    if (K == 2) IQAEND += KD;
                                    IQAEND = Math.Min(IQAEND, IQEND);
                                    for (J = J1; (KD1 >= 0) ? (J <= J2) : (J >= J2); J += KD1)
                                    {
                                        IBL = I - I2 / KDM1;
                                        I2 += 1;
                                        IQB = Math.Max(1, J - IBL);
                                        NQ = 1 + IQAEND - IQB;
                                        IQAEND = Math.Min(IQAEND + KD, IQEND);
                                        this._drot.Run(NQ, ref Q, IQB+(J - 1) * LDQ + o_q, 1, ref Q, IQB+J * LDQ + o_q, 1, D[J + o_d]
                                                       , WORK[J + o_work]);
                                    }
                                }
                                else
                                {
                                    // *
                                    for (J = J1; (KD1 >= 0) ? (J <= J2) : (J >= J2); J += KD1)
                                    {
                                        this._drot.Run(N, ref Q, 1+(J - 1) * LDQ + o_q, 1, ref Q, 1+J * LDQ + o_q, 1, D[J + o_d]
                                                       , WORK[J + o_work]);
                                    }
                                }
                                // *
                            }
                            // *
                            if (J2 + KDN > N)
                            {
                                // *
                                // *                    adjust J2 to keep within the bounds of the matrix
                                // *
                                NR -= 1;
                                J2 +=  - KDN - 1;
                            }
                            // *
                            for (J = J1; (KD1 >= 0) ? (J <= J2) : (J >= J2); J += KD1)
                            {
                                // *
                                // *                    create nonzero element a(j-1,j+kd) outside the band
                                // *                    and store it in WORK
                                // *
                                WORK[J + KD + o_work] = WORK[J + o_work] * AB[1+(J + KD) * LDAB + o_ab];
                                AB[1+(J + KD) * LDAB + o_ab] *= D[J + o_d];
                            }
                        }
                    }
                }
                // *
                if (KD > 0)
                {
                    // *
                    // *           copy off-diagonal elements to E
                    // *
                    for (I = 1; I <= N - 1; I++)
                    {
                        E[I + o_e] = AB[KD+(I + 1) * LDAB + o_ab];
                    }
                }
                else
                {
                    // *
                    // *           set E to zero if original matrix was diagonal
                    // *
                    for (I = 1; I <= N - 1; I++)
                    {
                        E[I + o_e] = ZERO;
                    }
                }
                // *
                // *        copy diagonal elements to D
                // *
                for (I = 1; I <= N; I++)
                {
                    D[I + o_d] = AB[KD1+I * LDAB + o_ab];
                }
                // *
            }
            else
            {
                // *
                if (KD > 1)
                {
                    // *
                    // *           Reduce to tridiagonal form, working with lower triangle
                    // *
                    NR = 0;
                    J1 = KDN + 2;
                    J2 = 1;
                    // *
                    for (I = 1; I <= N - 2; I++)
                    {
                        // *
                        // *              Reduce i-th column of matrix to tridiagonal form
                        // *
                        for (K = KDN + 1; K >= 2; K +=  - 1)
                        {
                            J1 += KDN;
                            J2 += KDN;
                            // *
                            if (NR > 0)
                            {
                                // *
                                // *                    generate plane rotations to annihilate nonzero
                                // *                    elements which have been created outside the band
                                // *
                                this._dlargv.Run(NR, ref AB, KD1+(J1 - KD1) * LDAB + o_ab, INCA, ref WORK, J1 + o_work, KD1, ref D, J1 + o_d
                                                 , KD1);
                                // *
                                // *                    apply plane rotations from one side
                                // *
                                // *
                                // *                    Dependent on the the number of diagonals either
                                // *                    DLARTV or DROT is used
                                // *
                                if (NR > 2 * KD - 1)
                                {
                                    for (L = 1; L <= KD - 1; L++)
                                    {
                                        this._dlartv.Run(NR, ref AB, KD1 - L+(J1 - KD1 + L) * LDAB + o_ab, INCA, ref AB, KD1 - L + 1+(J1 - KD1 + L) * LDAB + o_ab, INCA, D, J1 + o_d
                                                         , WORK, J1 + o_work, KD1);
                                    }
                                }
                                else
                                {
                                    JEND = J1 + KD1 * (NR - 1);
                                    for (JINC = J1; (KD1 >= 0) ? (JINC <= JEND) : (JINC >= JEND); JINC += KD1)
                                    {
                                        this._drot.Run(KDM1, ref AB, KD+(JINC - KD) * LDAB + o_ab, INCX, ref AB, KD1+(JINC - KD) * LDAB + o_ab, INCX, D[JINC + o_d]
                                                       , WORK[JINC + o_work]);
                                    }
                                }
                                // *
                            }
                            // *
                            if (K > 2)
                            {
                                if (K <= N - I + 1)
                                {
                                    // *
                                    // *                       generate plane rotation to annihilate a(i+k-1,i)
                                    // *                       within the band
                                    // *
                                    this._dlartg.Run(AB[K - 1+I * LDAB + o_ab], AB[K+I * LDAB + o_ab], ref D[I + K - 1 + o_d], ref WORK[I + K - 1 + o_work], ref TEMP);
                                    AB[K - 1+I * LDAB + o_ab] = TEMP;
                                    // *
                                    // *                       apply rotation from the left
                                    // *
                                    this._drot.Run(K - 3, ref AB, K - 2+(I + 1) * LDAB + o_ab, LDAB - 1, ref AB, K - 1+(I + 1) * LDAB + o_ab, LDAB - 1, D[I + K - 1 + o_d]
                                                   , WORK[I + K - 1 + o_work]);
                                }
                                NR += 1;
                                J1 +=  - KDN - 1;
                            }
                            // *
                            // *                 apply plane rotations from both sides to diagonal
                            // *                 blocks
                            // *
                            if (NR > 0)
                            {
                                this._dlar2v.Run(NR, ref AB, 1+(J1 - 1) * LDAB + o_ab, ref AB, 1+J1 * LDAB + o_ab, ref AB, 2+(J1 - 1) * LDAB + o_ab, INCA, D, J1 + o_d
                                                 , WORK, J1 + o_work, KD1);
                            }
                            // *
                            // *                 apply plane rotations from the right
                            // *
                            // *
                            // *                    Dependent on the the number of diagonals either
                            // *                    DLARTV or DROT is used
                            // *
                            if (NR > 0)
                            {
                                if (NR > 2 * KD - 1)
                                {
                                    for (L = 1; L <= KD - 1; L++)
                                    {
                                        if (J2 + L > N)
                                        {
                                            NRT = NR - 1;
                                        }
                                        else
                                        {
                                            NRT = NR;
                                        }
                                        if (NRT > 0)
                                        {
                                            this._dlartv.Run(NRT, ref AB, L + 2+(J1 - 1) * LDAB + o_ab, INCA, ref AB, L + 1+J1 * LDAB + o_ab, INCA, D, J1 + o_d
                                                             , WORK, J1 + o_work, KD1);
                                        }
                                    }
                                }
                                else
                                {
                                    J1END = J1 + KD1 * (NR - 2);
                                    if (J1END >= J1)
                                    {
                                        for (J1INC = J1; (KD1 >= 0) ? (J1INC <= J1END) : (J1INC >= J1END); J1INC += KD1)
                                        {
                                            this._drot.Run(KDM1, ref AB, 3+(J1INC - 1) * LDAB + o_ab, 1, ref AB, 2+J1INC * LDAB + o_ab, 1, D[J1INC + o_d]
                                                           , WORK[J1INC + o_work]);
                                        }
                                    }
                                    LEND = Math.Min(KDM1, N - J2);
                                    LAST = J1END + KD1;
                                    if (LEND > 0)
                                    {
                                        this._drot.Run(LEND, ref AB, 3+(LAST - 1) * LDAB + o_ab, 1, ref AB, 2+LAST * LDAB + o_ab, 1, D[LAST + o_d]
                                                       , WORK[LAST + o_work]);
                                    }
                                }
                            }
                            // *
                            // *
                            // *
                            if (WANTQ)
                            {
                                // *
                                // *                    accumulate product of plane rotations in Q
                                // *
                                if (INITQ)
                                {
                                    // *
                                    // *                 take advantage of the fact that Q was
                                    // *                 initially the Identity matrix
                                    // *
                                    IQEND = Math.Max(IQEND, J2);
                                    I2 = Math.Max(0, K - 3);
                                    IQAEND = 1 + I * KD;
                                    if (K == 2) IQAEND += KD;
                                    IQAEND = Math.Min(IQAEND, IQEND);
                                    for (J = J1; (KD1 >= 0) ? (J <= J2) : (J >= J2); J += KD1)
                                    {
                                        IBL = I - I2 / KDM1;
                                        I2 += 1;
                                        IQB = Math.Max(1, J - IBL);
                                        NQ = 1 + IQAEND - IQB;
                                        IQAEND = Math.Min(IQAEND + KD, IQEND);
                                        this._drot.Run(NQ, ref Q, IQB+(J - 1) * LDQ + o_q, 1, ref Q, IQB+J * LDQ + o_q, 1, D[J + o_d]
                                                       , WORK[J + o_work]);
                                    }
                                }
                                else
                                {
                                    // *
                                    for (J = J1; (KD1 >= 0) ? (J <= J2) : (J >= J2); J += KD1)
                                    {
                                        this._drot.Run(N, ref Q, 1+(J - 1) * LDQ + o_q, 1, ref Q, 1+J * LDQ + o_q, 1, D[J + o_d]
                                                       , WORK[J + o_work]);
                                    }
                                }
                            }
                            // *
                            if (J2 + KDN > N)
                            {
                                // *
                                // *                    adjust J2 to keep within the bounds of the matrix
                                // *
                                NR -= 1;
                                J2 +=  - KDN - 1;
                            }
                            // *
                            for (J = J1; (KD1 >= 0) ? (J <= J2) : (J >= J2); J += KD1)
                            {
                                // *
                                // *                    create nonzero element a(j+kd,j-1) outside the
                                // *                    band and store it in WORK
                                // *
                                WORK[J + KD + o_work] = WORK[J + o_work] * AB[KD1+J * LDAB + o_ab];
                                AB[KD1+J * LDAB + o_ab] *= D[J + o_d];
                            }
                        }
                    }
                }
                // *
                if (KD > 0)
                {
                    // *
                    // *           copy off-diagonal elements to E
                    // *
                    for (I = 1; I <= N - 1; I++)
                    {
                        E[I + o_e] = AB[2+I * LDAB + o_ab];
                    }
                }
                else
                {
                    // *
                    // *           set E to zero if original matrix was diagonal
                    // *
                    for (I = 1; I <= N - 1; I++)
                    {
                        E[I + o_e] = ZERO;
                    }
                }
                // *
                // *        copy diagonal elements to D
                // *
                for (I = 1; I <= N; I++)
                {
                    D[I + o_d] = AB[1+I * LDAB + o_ab];
                }
            }
            // *
            return;
            // *
            // *     End of DSBTRD
            // *

            #endregion

        }
    }
}
