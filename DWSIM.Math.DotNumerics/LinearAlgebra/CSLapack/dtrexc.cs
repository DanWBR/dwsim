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
    /// DTREXC reorders the real Schur factorization of a real matrix
    /// A = Q*T*Q**T, so that the diagonal block of T with row index IFST is
    /// moved to row ILST.
    /// 
    /// The real Schur form T is reordered by an orthogonal similarity
    /// transformation Z**T*T*Z, and optionally the matrix Q of Schur vectors
    /// is updated by postmultiplying it with Z.
    /// 
    /// T must be in Schur canonical form (as returned by DHSEQR), that is,
    /// block upper triangular with 1-by-1 and 2-by-2 diagonal blocks; each
    /// 2-by-2 diagonal block has its diagonal elements equal and its
    /// off-diagonal elements of opposite sign.
    /// 
    ///</summary>
    public class DTREXC
    {
    

        #region Dependencies
        
        LSAME _lsame; DLAEXC _dlaexc; XERBLA _xerbla; 

        #endregion


        #region Variables
        
        const double ZERO = 0.0E+0; 

        #endregion

        public DTREXC(LSAME lsame, DLAEXC dlaexc, XERBLA xerbla)
        {
    

            #region Set Dependencies
            
            this._lsame = lsame; this._dlaexc = dlaexc; this._xerbla = xerbla; 

            #endregion

        }
    
        public DTREXC()
        {
    

            #region Dependencies (Initialization)
            
            LSAME lsame = new LSAME();
            DLAMC3 dlamc3 = new DLAMC3();
            DLASSQ dlassq = new DLASSQ();
            DLAPY2 dlapy2 = new DLAPY2();
            DNRM2 dnrm2 = new DNRM2();
            DSCAL dscal = new DSCAL();
            XERBLA xerbla = new XERBLA();
            IDAMAX idamax = new IDAMAX();
            DCOPY dcopy = new DCOPY();
            DSWAP dswap = new DSWAP();
            DROT drot = new DROT();
            DLAMC1 dlamc1 = new DLAMC1(dlamc3);
            DLAMC4 dlamc4 = new DLAMC4(dlamc3);
            DLAMC5 dlamc5 = new DLAMC5(dlamc3);
            DLAMC2 dlamc2 = new DLAMC2(dlamc3, dlamc1, dlamc4, dlamc5);
            DLAMCH dlamch = new DLAMCH(lsame, dlamc2);
            DLANGE dlange = new DLANGE(dlassq, lsame);
            DLACPY dlacpy = new DLACPY(lsame);
            DLANV2 dlanv2 = new DLANV2(dlamch, dlapy2);
            DLARFG dlarfg = new DLARFG(dlamch, dlapy2, dnrm2, dscal);
            DGEMV dgemv = new DGEMV(lsame, xerbla);
            DGER dger = new DGER(xerbla);
            DLARFX dlarfx = new DLARFX(lsame, dgemv, dger);
            DLARTG dlartg = new DLARTG(dlamch);
            DLASY2 dlasy2 = new DLASY2(idamax, dlamch, dcopy, dswap);
            DLAEXC dlaexc = new DLAEXC(dlamch, dlange, dlacpy, dlanv2, dlarfg, dlarfx, dlartg, dlasy2, drot);

            #endregion


            #region Set Dependencies
            
            this._lsame = lsame; this._dlaexc = dlaexc; this._xerbla = xerbla; 

            #endregion

        }
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// DTREXC reorders the real Schur factorization of a real matrix
        /// A = Q*T*Q**T, so that the diagonal block of T with row index IFST is
        /// moved to row ILST.
        /// 
        /// The real Schur form T is reordered by an orthogonal similarity
        /// transformation Z**T*T*Z, and optionally the matrix Q of Schur vectors
        /// is updated by postmultiplying it with Z.
        /// 
        /// T must be in Schur canonical form (as returned by DHSEQR), that is,
        /// block upper triangular with 1-by-1 and 2-by-2 diagonal blocks; each
        /// 2-by-2 diagonal block has its diagonal elements equal and its
        /// off-diagonal elements of opposite sign.
        /// 
        ///</summary>
        /// <param name="COMPQ">
        /// (input) CHARACTER*1
        /// = 'V':  update the matrix Q of Schur vectors;
        /// = 'N':  do not update Q.
        ///</param>
        /// <param name="N">
        /// (input) INTEGER
        /// The order of the matrix T. N .GE. 0.
        ///</param>
        /// <param name="T">
        /// must be in Schur canonical form (as returned by DHSEQR), that is,
        ///</param>
        /// <param name="LDT">
        /// (input) INTEGER
        /// The leading dimension of the array T. LDT .GE. max(1,N).
        ///</param>
        /// <param name="Q">
        /// (input/output) DOUBLE PRECISION array, dimension (LDQ,N)
        /// On entry, if COMPQ = 'V', the matrix Q of Schur vectors.
        /// On exit, if COMPQ = 'V', Q has been postmultiplied by the
        /// orthogonal transformation matrix Z which reorders T.
        /// If COMPQ = 'N', Q is not referenced.
        ///</param>
        /// <param name="LDQ">
        /// (input) INTEGER
        /// The leading dimension of the array Q.  LDQ .GE. max(1,N).
        ///</param>
        /// <param name="IFST">
        /// (input/output) INTEGER
        ///</param>
        /// <param name="ILST">
        /// (input/output) INTEGER
        /// Specify the reordering of the diagonal blocks of T.
        /// The block with row index IFST is moved to row ILST, by a
        /// sequence of transpositions between adjacent blocks.
        /// On exit, if IFST pointed on entry to the second row of a
        /// 2-by-2 block, it is changed to point to the first row; ILST
        /// always points to the first row of the block in its final
        /// position (which may differ from its input value by +1 or -1).
        /// 1 .LE. IFST .LE. N; 1 .LE. ILST .LE. N.
        ///</param>
        /// <param name="WORK">
        /// (workspace) DOUBLE PRECISION array, dimension (N)
        ///</param>
        /// <param name="INFO">
        /// (output) INTEGER
        /// = 0:  successful exit
        /// .LT. 0:  if INFO = -i, the i-th argument had an illegal value
        /// = 1:  two adjacent blocks were too close to swap (the problem
        /// is very ill-conditioned); T may have been partially
        /// reordered, and ILST points to the first row of the
        /// current position of the block being moved.
        ///</param>
        public void Run(string COMPQ, int N, ref double[] T, int offset_t, int LDT, ref double[] Q, int offset_q, int LDQ
                         , ref int IFST, ref int ILST, ref double[] WORK, int offset_work, ref int INFO)
        {

            #region Variables
            
            bool WANTQ = false; int HERE = 0; int NBF = 0; int NBL = 0; int NBNEXT = 0; 

            #endregion


            #region Array Index Correction
            
             int o_t = -1 - LDT + offset_t;  int o_q = -1 - LDQ + offset_q;  int o_work = -1 + offset_work; 

            #endregion


            #region Strings
            
            COMPQ = COMPQ.Substring(0, 1);  

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
            // *  DTREXC reorders the real Schur factorization of a real matrix
            // *  A = Q*T*Q**T, so that the diagonal block of T with row index IFST is
            // *  moved to row ILST.
            // *
            // *  The real Schur form T is reordered by an orthogonal similarity
            // *  transformation Z**T*T*Z, and optionally the matrix Q of Schur vectors
            // *  is updated by postmultiplying it with Z.
            // *
            // *  T must be in Schur canonical form (as returned by DHSEQR), that is,
            // *  block upper triangular with 1-by-1 and 2-by-2 diagonal blocks; each
            // *  2-by-2 diagonal block has its diagonal elements equal and its
            // *  off-diagonal elements of opposite sign.
            // *
            // *  Arguments
            // *  =========
            // *
            // *  COMPQ   (input) CHARACTER*1
            // *          = 'V':  update the matrix Q of Schur vectors;
            // *          = 'N':  do not update Q.
            // *
            // *  N       (input) INTEGER
            // *          The order of the matrix T. N >= 0.
            // *
            // *  T       (input/output) DOUBLE PRECISION array, dimension (LDT,N)
            // *          On entry, the upper quasi-triangular matrix T, in Schur
            // *          Schur canonical form.
            // *          On exit, the reordered upper quasi-triangular matrix, again
            // *          in Schur canonical form.
            // *
            // *  LDT     (input) INTEGER
            // *          The leading dimension of the array T. LDT >= max(1,N).
            // *
            // *  Q       (input/output) DOUBLE PRECISION array, dimension (LDQ,N)
            // *          On entry, if COMPQ = 'V', the matrix Q of Schur vectors.
            // *          On exit, if COMPQ = 'V', Q has been postmultiplied by the
            // *          orthogonal transformation matrix Z which reorders T.
            // *          If COMPQ = 'N', Q is not referenced.
            // *
            // *  LDQ     (input) INTEGER
            // *          The leading dimension of the array Q.  LDQ >= max(1,N).
            // *
            // *  IFST    (input/output) INTEGER
            // *  ILST    (input/output) INTEGER
            // *          Specify the reordering of the diagonal blocks of T.
            // *          The block with row index IFST is moved to row ILST, by a
            // *          sequence of transpositions between adjacent blocks.
            // *          On exit, if IFST pointed on entry to the second row of a
            // *          2-by-2 block, it is changed to point to the first row; ILST
            // *          always points to the first row of the block in its final
            // *          position (which may differ from its input value by +1 or -1).
            // *          1 <= IFST <= N; 1 <= ILST <= N.
            // *
            // *  WORK    (workspace) DOUBLE PRECISION array, dimension (N)
            // *
            // *  INFO    (output) INTEGER
            // *          = 0:  successful exit
            // *          < 0:  if INFO = -i, the i-th argument had an illegal value
            // *          = 1:  two adjacent blocks were too close to swap (the problem
            // *                is very ill-conditioned); T may have been partially
            // *                reordered, and ILST points to the first row of the
            // *                current position of the block being moved.
            // *
            // *  =====================================================================
            // *
            // *     .. Parameters ..
            // *     ..
            // *     .. Local Scalars ..
            // *     ..
            // *     .. External Functions ..
            // *     ..
            // *     .. External Subroutines ..
            // *     ..
            // *     .. Intrinsic Functions ..
            //      INTRINSIC          MAX;
            // *     ..
            // *     .. Executable Statements ..
            // *
            // *     Decode and test the input arguments.
            // *

            #endregion


            #region Body
            
            INFO = 0;
            WANTQ = this._lsame.Run(COMPQ, "V");
            if (!WANTQ && !this._lsame.Run(COMPQ, "N"))
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
                    if (LDT < Math.Max(1, N))
                    {
                        INFO =  - 4;
                    }
                    else
                    {
                        if (LDQ < 1 || (WANTQ && LDQ < Math.Max(1, N)))
                        {
                            INFO =  - 6;
                        }
                        else
                        {
                            if (IFST < 1 || IFST > N)
                            {
                                INFO =  - 7;
                            }
                            else
                            {
                                if (ILST < 1 || ILST > N)
                                {
                                    INFO =  - 8;
                                }
                            }
                        }
                    }
                }
            }
            if (INFO != 0)
            {
                this._xerbla.Run("DTREXC",  - INFO);
                return;
            }
            // *
            // *     Quick return if possible
            // *
            if (N <= 1) return;
            // *
            // *     Determine the first row of specified block
            // *     and find out it is 1 by 1 or 2 by 2.
            // *
            if (IFST > 1)
            {
                if (T[IFST+(IFST - 1) * LDT + o_t] != ZERO) IFST -= 1;
            }
            NBF = 1;
            if (IFST < N)
            {
                if (T[IFST + 1+IFST * LDT + o_t] != ZERO) NBF = 2;
            }
            // *
            // *     Determine the first row of the final block
            // *     and find out it is 1 by 1 or 2 by 2.
            // *
            if (ILST > 1)
            {
                if (T[ILST+(ILST - 1) * LDT + o_t] != ZERO) ILST -= 1;
            }
            NBL = 1;
            if (ILST < N)
            {
                if (T[ILST + 1+ILST * LDT + o_t] != ZERO) NBL = 2;
            }
            // *
            if (IFST == ILST) return;
            // *
            if (IFST < ILST)
            {
                // *
                // *        Update ILST
                // *
                if (NBF == 2 && NBL == 1) ILST -= 1;
                if (NBF == 1 && NBL == 2) ILST += 1;
                // *
                HERE = IFST;
                // *
            LABEL10:;
                // *
                // *        Swap block with next one below
                // *
                if (NBF == 1 || NBF == 2)
                {
                    // *
                    // *           Current block either 1 by 1 or 2 by 2
                    // *
                    NBNEXT = 1;
                    if (HERE + NBF + 1 <= N)
                    {
                        if (T[HERE + NBF + 1+(HERE + NBF) * LDT + o_t] != ZERO) NBNEXT = 2;
                    }
                    this._dlaexc.Run(WANTQ, N, ref T, offset_t, LDT, ref Q, offset_q, LDQ
                                     , HERE, NBF, NBNEXT, ref WORK, offset_work, ref INFO);
                    if (INFO != 0)
                    {
                        ILST = HERE;
                        return;
                    }
                    HERE += NBNEXT;
                    // *
                    // *           Test if 2 by 2 block breaks into two 1 by 1 blocks
                    // *
                    if (NBF == 2)
                    {
                        if (T[HERE + 1+HERE * LDT + o_t] == ZERO) NBF = 3;
                    }
                    // *
                }
                else
                {
                    // *
                    // *           Current block consists of two 1 by 1 blocks each of which
                    // *           must be swapped individually
                    // *
                    NBNEXT = 1;
                    if (HERE + 3 <= N)
                    {
                        if (T[HERE + 3+(HERE + 2) * LDT + o_t] != ZERO) NBNEXT = 2;
                    }
                    this._dlaexc.Run(WANTQ, N, ref T, offset_t, LDT, ref Q, offset_q, LDQ
                                     , HERE + 1, 1, NBNEXT, ref WORK, offset_work, ref INFO);
                    if (INFO != 0)
                    {
                        ILST = HERE;
                        return;
                    }
                    if (NBNEXT == 1)
                    {
                        // *
                        // *              Swap two 1 by 1 blocks, no problems possible
                        // *
                        this._dlaexc.Run(WANTQ, N, ref T, offset_t, LDT, ref Q, offset_q, LDQ
                                         , HERE, 1, NBNEXT, ref WORK, offset_work, ref INFO);
                        HERE += 1;
                    }
                    else
                    {
                        // *
                        // *              Recompute NBNEXT in case 2 by 2 split
                        // *
                        if (T[HERE + 2+(HERE + 1) * LDT + o_t] == ZERO) NBNEXT = 1;
                        if (NBNEXT == 2)
                        {
                            // *
                            // *                 2 by 2 Block did not split
                            // *
                            this._dlaexc.Run(WANTQ, N, ref T, offset_t, LDT, ref Q, offset_q, LDQ
                                             , HERE, 1, NBNEXT, ref WORK, offset_work, ref INFO);
                            if (INFO != 0)
                            {
                                ILST = HERE;
                                return;
                            }
                            HERE += 2;
                        }
                        else
                        {
                            // *
                            // *                 2 by 2 Block did split
                            // *
                            this._dlaexc.Run(WANTQ, N, ref T, offset_t, LDT, ref Q, offset_q, LDQ
                                             , HERE, 1, 1, ref WORK, offset_work, ref INFO);
                            this._dlaexc.Run(WANTQ, N, ref T, offset_t, LDT, ref Q, offset_q, LDQ
                                             , HERE + 1, 1, 1, ref WORK, offset_work, ref INFO);
                            HERE += 2;
                        }
                    }
                }
                if (HERE < ILST) goto LABEL10;
                // *
            }
            else
            {
                // *
                HERE = IFST;
            LABEL20:;
                // *
                // *        Swap block with next one above
                // *
                if (NBF == 1 || NBF == 2)
                {
                    // *
                    // *           Current block either 1 by 1 or 2 by 2
                    // *
                    NBNEXT = 1;
                    if (HERE >= 3)
                    {
                        if (T[HERE - 1+(HERE - 2) * LDT + o_t] != ZERO) NBNEXT = 2;
                    }
                    this._dlaexc.Run(WANTQ, N, ref T, offset_t, LDT, ref Q, offset_q, LDQ
                                     , HERE - NBNEXT, NBNEXT, NBF, ref WORK, offset_work, ref INFO);
                    if (INFO != 0)
                    {
                        ILST = HERE;
                        return;
                    }
                    HERE -= NBNEXT;
                    // *
                    // *           Test if 2 by 2 block breaks into two 1 by 1 blocks
                    // *
                    if (NBF == 2)
                    {
                        if (T[HERE + 1+HERE * LDT + o_t] == ZERO) NBF = 3;
                    }
                    // *
                }
                else
                {
                    // *
                    // *           Current block consists of two 1 by 1 blocks each of which
                    // *           must be swapped individually
                    // *
                    NBNEXT = 1;
                    if (HERE >= 3)
                    {
                        if (T[HERE - 1+(HERE - 2) * LDT + o_t] != ZERO) NBNEXT = 2;
                    }
                    this._dlaexc.Run(WANTQ, N, ref T, offset_t, LDT, ref Q, offset_q, LDQ
                                     , HERE - NBNEXT, NBNEXT, 1, ref WORK, offset_work, ref INFO);
                    if (INFO != 0)
                    {
                        ILST = HERE;
                        return;
                    }
                    if (NBNEXT == 1)
                    {
                        // *
                        // *              Swap two 1 by 1 blocks, no problems possible
                        // *
                        this._dlaexc.Run(WANTQ, N, ref T, offset_t, LDT, ref Q, offset_q, LDQ
                                         , HERE, NBNEXT, 1, ref WORK, offset_work, ref INFO);
                        HERE -= 1;
                    }
                    else
                    {
                        // *
                        // *              Recompute NBNEXT in case 2 by 2 split
                        // *
                        if (T[HERE+(HERE - 1) * LDT + o_t] == ZERO) NBNEXT = 1;
                        if (NBNEXT == 2)
                        {
                            // *
                            // *                 2 by 2 Block did not split
                            // *
                            this._dlaexc.Run(WANTQ, N, ref T, offset_t, LDT, ref Q, offset_q, LDQ
                                             , HERE - 1, 2, 1, ref WORK, offset_work, ref INFO);
                            if (INFO != 0)
                            {
                                ILST = HERE;
                                return;
                            }
                            HERE -= 2;
                        }
                        else
                        {
                            // *
                            // *                 2 by 2 Block did split
                            // *
                            this._dlaexc.Run(WANTQ, N, ref T, offset_t, LDT, ref Q, offset_q, LDQ
                                             , HERE, 1, 1, ref WORK, offset_work, ref INFO);
                            this._dlaexc.Run(WANTQ, N, ref T, offset_t, LDT, ref Q, offset_q, LDQ
                                             , HERE - 1, 1, 1, ref WORK, offset_work, ref INFO);
                            HERE -= 2;
                        }
                    }
                }
                if (HERE > ILST) goto LABEL20;
            }
            ILST = HERE;
            // *
            return;
            // *
            // *     End of DTREXC
            // *

            #endregion

        }
    }
}
