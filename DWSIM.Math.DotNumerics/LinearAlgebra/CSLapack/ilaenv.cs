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
    /// -- LAPACK auxiliary routine (version 3.1.1) --
    /// Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
    /// January 2007
    /// Purpose
    /// =======
    /// 
    /// ILAENV is called from the LAPACK routines to choose problem-dependent
    /// parameters for the local environment.  See ISPEC for a description of
    /// the parameters.
    /// 
    /// ILAENV returns an INTEGER
    /// if ILAENV .GE. 0: ILAENV returns the value of the parameter specified by ISPEC
    /// if ILAENV .LT. 0:  if ILAENV = -k, the k-th argument had an illegal value.
    /// 
    /// This version provides a set of parameters which should give good,
    /// but not optimal, performance on many of the currently available
    /// computers.  Users are encouraged to modify this subroutine to set
    /// the tuning parameters for their particular machine using the option
    /// and problem size information in the arguments.
    /// 
    /// This routine will not function correctly if it is converted to all
    /// lower case.  Converting it to all upper case is allowed.
    /// 
    ///</summary>
    public class ILAENV
    {
    

        #region Dependencies
        
        IEEECK _ieeeck; IPARMQ _iparmq; 

        #endregion

        public ILAENV(IEEECK ieeeck, IPARMQ iparmq)
        {
    

            #region Set Dependencies
            
            this._ieeeck = ieeeck; this._iparmq = iparmq; 

            #endregion

        }
    
        public ILAENV()
        {
    

            #region Dependencies (Initialization)
            
            IEEECK ieeeck = new IEEECK();
            IPARMQ iparmq = new IPARMQ();

            #endregion


            #region Set Dependencies
            
            this._ieeeck = ieeeck; this._iparmq = iparmq; 

            #endregion

        }
        /// <summary>
        /// Purpose
        /// =======
        /// 
        /// ILAENV is called from the LAPACK routines to choose problem-dependent
        /// parameters for the local environment.  See ISPEC for a description of
        /// the parameters.
        /// 
        /// ILAENV returns an INTEGER
        /// if ILAENV .GE. 0: ILAENV returns the value of the parameter specified by ISPEC
        /// if ILAENV .LT. 0:  if ILAENV = -k, the k-th argument had an illegal value.
        /// 
        /// This version provides a set of parameters which should give good,
        /// but not optimal, performance on many of the currently available
        /// computers.  Users are encouraged to modify this subroutine to set
        /// the tuning parameters for their particular machine using the option
        /// and problem size information in the arguments.
        /// 
        /// This routine will not function correctly if it is converted to all
        /// lower case.  Converting it to all upper case is allowed.
        /// 
        ///</summary>
        /// <param name="ISPEC">
        /// (input) INTEGER
        /// Specifies the parameter to be returned as the value of
        /// ILAENV.
        /// = 1: the optimal blocksize; if this value is 1, an unblocked
        /// algorithm will give the best performance.
        /// = 2: the minimum block size for which the block routine
        /// should be used; if the usable block size is less than
        /// this value, an unblocked routine should be used.
        /// = 3: the crossover point (in a block routine, for N less
        /// than this value, an unblocked routine should be used)
        /// = 4: the number of shifts, used in the nonsymmetric
        /// eigenvalue routines (DEPRECATED)
        /// = 5: the minimum column dimension for blocking to be used;
        /// rectangular blocks must have dimension at least k by m,
        /// where k is given by ILAENV(2,...) and m by ILAENV(5,...)
        /// = 6: the crossover point for the SVD (when reducing an m by n
        /// matrix to bidiagonal form, if max(m,n)/min(m,n) exceeds
        /// this value, a QR factorization is used first to reduce
        /// the matrix to a triangular form.)
        /// = 7: the number of processors
        /// = 8: the crossover point for the multishift QR method
        /// for nonsymmetric eigenvalue problems (DEPRECATED)
        /// = 9: maximum size of the subproblems at the bottom of the
        /// computation tree in the divide-and-conquer algorithm
        /// (used by xGELSD and xGESDD)
        /// =10: ieee NaN arithmetic can be trusted not to trap
        /// =11: infinity arithmetic can be trusted not to trap
        /// 12 .LE. ISPEC .LE. 16:
        /// xHSEQR or one of its subroutines,
        /// see IPARMQ for detailed explanation
        ///</param>
        /// <param name="NAME">
        /// (input) CHARACTER*(*)
        /// The name of the calling subroutine, in either upper case or
        /// lower case.
        ///</param>
        /// <param name="OPTS">
        /// (input) CHARACTER*(*)
        /// The character options to the subroutine NAME, concatenated
        /// into a single character string.  For example, UPLO = 'U',
        /// TRANS = 'T', and DIAG = 'N' for a triangular routine would
        /// be specified as OPTS = 'UTN'.
        ///</param>
        /// <param name="N1">
        /// (input) INTEGER
        ///</param>
        /// <param name="N2">
        /// (input) INTEGER
        ///</param>
        /// <param name="N3">
        /// (input) INTEGER
        ///</param>
        /// <param name="N4">
        /// (input) INTEGER
        /// Problem dimensions for the subroutine NAME; these may not all
        /// be required.
        ///</param>
        public int Run(int ISPEC, string NAME, string OPTS, int N1, int N2, int N3
                        , int N4)
        {
        int ilaenv = 0;

            #region Variables
            
            int I = 0; int IC = 0; int IZ = 0; int NB = 0; int NBMIN = 0; int NX = 0; bool CNAME = false; bool SNAME = false; 
            string C1 = new string(' ', 1);string C2 = new string(' ', 2); string C4 = new string(' ', 2); 
            string C3 = new string(' ', 3);string SUBNAM = new string(' ', 6); 

            #endregion


            #region Prolog
            
            // *
            // *  -- LAPACK auxiliary routine (version 3.1.1) --
            // *     Univ. of Tennessee, Univ. of California Berkeley and NAG Ltd..
            // *     January 2007
            // *
            // *     .. Scalar Arguments ..
            // *     ..
            // *
            // *  Purpose
            // *  =======
            // *
            // *  ILAENV is called from the LAPACK routines to choose problem-dependent
            // *  parameters for the local environment.  See ISPEC for a description of
            // *  the parameters.
            // *
            // *  ILAENV returns an INTEGER
            // *  if ILAENV >= 0: ILAENV returns the value of the parameter specified by ISPEC
            // *  if ILAENV < 0:  if ILAENV = -k, the k-th argument had an illegal value.
            // *
            // *  This version provides a set of parameters which should give good,
            // *  but not optimal, performance on many of the currently available
            // *  computers.  Users are encouraged to modify this subroutine to set
            // *  the tuning parameters for their particular machine using the option
            // *  and problem size information in the arguments.
            // *
            // *  This routine will not function correctly if it is converted to all
            // *  lower case.  Converting it to all upper case is allowed.
            // *
            // *  Arguments
            // *  =========
            // *
            // *  ISPEC   (input) INTEGER
            // *          Specifies the parameter to be returned as the value of
            // *          ILAENV.
            // *          = 1: the optimal blocksize; if this value is 1, an unblocked
            // *               algorithm will give the best performance.
            // *          = 2: the minimum block size for which the block routine
            // *               should be used; if the usable block size is less than
            // *               this value, an unblocked routine should be used.
            // *          = 3: the crossover point (in a block routine, for N less
            // *               than this value, an unblocked routine should be used)
            // *          = 4: the number of shifts, used in the nonsymmetric
            // *               eigenvalue routines (DEPRECATED)
            // *          = 5: the minimum column dimension for blocking to be used;
            // *               rectangular blocks must have dimension at least k by m,
            // *               where k is given by ILAENV(2,...) and m by ILAENV(5,...)
            // *          = 6: the crossover point for the SVD (when reducing an m by n
            // *               matrix to bidiagonal form, if max(m,n)/min(m,n) exceeds
            // *               this value, a QR factorization is used first to reduce
            // *               the matrix to a triangular form.)
            // *          = 7: the number of processors
            // *          = 8: the crossover point for the multishift QR method
            // *               for nonsymmetric eigenvalue problems (DEPRECATED)
            // *          = 9: maximum size of the subproblems at the bottom of the
            // *               computation tree in the divide-and-conquer algorithm
            // *               (used by xGELSD and xGESDD)
            // *          =10: ieee NaN arithmetic can be trusted not to trap
            // *          =11: infinity arithmetic can be trusted not to trap
            // *          12 <= ISPEC <= 16:
            // *               xHSEQR or one of its subroutines,
            // *               see IPARMQ for detailed explanation
            // *
            // *  NAME    (input) CHARACTER*(*)
            // *          The name of the calling subroutine, in either upper case or
            // *          lower case.
            // *
            // *  OPTS    (input) CHARACTER*(*)
            // *          The character options to the subroutine NAME, concatenated
            // *          into a single character string.  For example, UPLO = 'U',
            // *          TRANS = 'T', and DIAG = 'N' for a triangular routine would
            // *          be specified as OPTS = 'UTN'.
            // *
            // *  N1      (input) INTEGER
            // *  N2      (input) INTEGER
            // *  N3      (input) INTEGER
            // *  N4      (input) INTEGER
            // *          Problem dimensions for the subroutine NAME; these may not all
            // *          be required.
            // *
            // *  Further Details
            // *  ===============
            // *
            // *  The following conventions have been used when calling ILAENV from the
            // *  LAPACK routines:
            // *  1)  OPTS is a concatenation of all of the character options to
            // *      subroutine NAME, in the same order that they appear in the
            // *      argument list for NAME, even if they are not used in determining
            // *      the value of the parameter specified by ISPEC.
            // *  2)  The problem dimensions N1, N2, N3, N4 are specified in the order
            // *      that they appear in the argument list for NAME.  N1 is used
            // *      first, N2 second, and so on, and unused problem dimensions are
            // *      passed a value of -1.
            // *  3)  The parameter value returned by ILAENV is checked for validity in
            // *      the calling subroutine.  For example, ILAENV is used to retrieve
            // *      the optimal blocksize for STRTRI as follows:
            // *
            // *      NB = ILAENV( 1, 'STRTRI', UPLO // DIAG, N, -1, -1, -1 )
            // *      IF( NB.LE.1 ) NB = MAX( 1, N )
            // *
            // *  =====================================================================
            // *
            // *     .. Local Scalars ..
            // *     ..
            // *     .. Intrinsic Functions ..
            //      INTRINSIC          CHAR, ICHAR, INT, MIN, REAL;
            // *     ..
            // *     .. External Functions ..
            // *     ..
            // *     .. Executable Statements ..
            // *

            #endregion


            #region Body
            
            switch (ISPEC)
            {
                case 1: goto LABEL10;
                case 2: goto LABEL10;
                case 3: goto LABEL10;
                case 4: goto LABEL80;
                case 5: goto LABEL90;
                case 6: goto LABEL100;
                case 7: goto LABEL110;
                case 8: goto LABEL120;
                case 9: goto LABEL130;
                case 10: goto LABEL140;
                case 11: goto LABEL150;
                case 12: goto LABEL160;
                case 13: goto LABEL160;
                case 14: goto LABEL160;
                case 15: goto LABEL160;
                case 16: goto LABEL160;
            }
            // *
            // *     Invalid value for ISPEC
            // *
            ilaenv =  - 1;
            return ilaenv;
            // *
        LABEL10:;
            // *
            // *     Convert NAME to upper case if the first character is lower case.
            // *
            ilaenv = 1;
            FortranLib.Copy(ref SUBNAM , NAME);
            IC = Convert.ToInt32(Convert.ToChar(FortranLib.Substring(SUBNAM, 1, 1)));
            IZ = Convert.ToInt32('Z');
            if (IZ == 90 || IZ == 122)
            {
                // *
                // *        ASCII character set
                // *
                if (IC >= 97 && IC <= 122)
                {
                    FortranLib.Copy(ref SUBNAM, 1, 1, Convert.ToChar(IC - 32));
                    for (I = 2; I <= 6; I++)
                    {
                        IC = Convert.ToInt32(Convert.ToChar(FortranLib.Substring(SUBNAM, I, I)));
                        if (IC >= 97 && IC <= 122) FortranLib.Copy(ref SUBNAM, I, I, Convert.ToChar(IC - 32));
                    }
                }
                // *
            }
            else
            {
                if (IZ == 233 || IZ == 169)
                {
                    // *
                    // *        EBCDIC character set
                    // *
                    if ((IC >= 129 && IC <= 137) || (IC >= 145 && IC <= 153) || (IC >= 162 && IC <= 169))
                    {
                        FortranLib.Copy(ref SUBNAM, 1, 1, Convert.ToChar(IC + 64));
                        for (I = 2; I <= 6; I++)
                        {
                            IC = Convert.ToInt32(Convert.ToChar(FortranLib.Substring(SUBNAM, I, I)));
                            if ((IC >= 129 && IC <= 137) || (IC >= 145 && IC <= 153) || (IC >= 162 && IC <= 169)) FortranLib.Copy(ref SUBNAM, I, I, Convert.ToChar(IC + 64));
                        }
                    }
                    // *
                }
                else
                {
                    if (IZ == 218 || IZ == 250)
                    {
                        // *
                        // *        Prime machines:  ASCII+128
                        // *
                        if (IC >= 225 && IC <= 250)
                        {
                            FortranLib.Copy(ref SUBNAM, 1, 1, Convert.ToChar(IC - 32));
                            for (I = 2; I <= 6; I++)
                            {
                                IC = Convert.ToInt32(Convert.ToChar(FortranLib.Substring(SUBNAM, I, I)));
                                if (IC >= 225 && IC <= 250) FortranLib.Copy(ref SUBNAM, I, I, Convert.ToChar(IC - 32));
                            }
                        }
                    }
                }
            }
            // *
            FortranLib.Copy(ref C1 , FortranLib.Substring(SUBNAM, 1, 1));
            SNAME = C1 == "S" || C1 == "D";
            CNAME = C1 == "C" || C1 == "Z";
            if (!(CNAME || SNAME)) return ilaenv;
            FortranLib.Copy(ref C2 , FortranLib.Substring(SUBNAM, 2, 3));
            FortranLib.Copy(ref C3 , FortranLib.Substring(SUBNAM, 4, 6));
            FortranLib.Copy(ref C4 , FortranLib.Substring(C3, 2, 3));
            // *
            switch (ISPEC)
            {
                case 1: goto LABEL50;
                case 2: goto LABEL60;
                case 3: goto LABEL70;
            }
            // *
        LABEL50:;
            // *
            // *     ISPEC = 1:  block size
            // *
            // *     In these examples, separate code is provided for setting NB for
            // *     real and complex.  We assume that NB will take the same value in
            // *     single or double precision.
            // *
            NB = 1;
            // *
            if (C2 == "GE")
            {
                if (C3 == "TRF")
                {
                    if (SNAME)
                    {
                        NB = 64;
                    }
                    else
                    {
                        NB = 64;
                    }
                }
                else
                {
                    if (C3 == "QRF" || C3 == "RQF" || C3 == "LQF" || C3 == "QLF")
                    {
                        if (SNAME)
                        {
                            NB = 32;
                        }
                        else
                        {
                            NB = 32;
                        }
                    }
                    else
                    {
                        if (C3 == "HRD")
                        {
                            if (SNAME)
                            {
                                NB = 32;
                            }
                            else
                            {
                                NB = 32;
                            }
                        }
                        else
                        {
                            if (C3 == "BRD")
                            {
                                if (SNAME)
                                {
                                    NB = 32;
                                }
                                else
                                {
                                    NB = 32;
                                }
                            }
                            else
                            {
                                if (C3 == "TRI")
                                {
                                    if (SNAME)
                                    {
                                        NB = 64;
                                    }
                                    else
                                    {
                                        NB = 64;
                                    }
                                }
                            }
                        }
                    }
                }
            }
            else
            {
                if (C2 == "PO")
                {
                    if (C3 == "TRF")
                    {
                        if (SNAME)
                        {
                            NB = 64;
                        }
                        else
                        {
                            NB = 64;
                        }
                    }
                }
                else
                {
                    if (C2 == "SY")
                    {
                        if (C3 == "TRF")
                        {
                            if (SNAME)
                            {
                                NB = 64;
                            }
                            else
                            {
                                NB = 64;
                            }
                        }
                        else
                        {
                            if (SNAME && C3 == "TRD")
                            {
                                NB = 32;
                            }
                            else
                            {
                                if (SNAME && C3 == "GST")
                                {
                                    NB = 64;
                                }
                            }
                        }
                    }
                    else
                    {
                        if (CNAME && C2 == "HE")
                        {
                            if (C3 == "TRF")
                            {
                                NB = 64;
                            }
                            else
                            {
                                if (C3 == "TRD")
                                {
                                    NB = 32;
                                }
                                else
                                {
                                    if (C3 == "GST")
                                    {
                                        NB = 64;
                                    }
                                }
                            }
                        }
                        else
                        {
                            if (SNAME && C2 == "OR")
                            {
                                if (FortranLib.Substring(C3, 1, 1) == "G")
                                {
                                    if (C4 == "QR" || C4 == "RQ" || C4 == "LQ" || C4 == "QL" || C4 == "HR" || C4 == "TR" || C4 == "BR")
                                    {
                                        NB = 32;
                                    }
                                }
                                else
                                {
                                    if (FortranLib.Substring(C3, 1, 1) == "M")
                                    {
                                        if (C4 == "QR" || C4 == "RQ" || C4 == "LQ" || C4 == "QL" || C4 == "HR" || C4 == "TR" || C4 == "BR")
                                        {
                                            NB = 32;
                                        }
                                    }
                                }
                            }
                            else
                            {
                                if (CNAME && C2 == "UN")
                                {
                                    if (FortranLib.Substring(C3, 1, 1) == "G")
                                    {
                                        if (C4 == "QR" || C4 == "RQ" || C4 == "LQ" || C4 == "QL" || C4 == "HR" || C4 == "TR" || C4 == "BR")
                                        {
                                            NB = 32;
                                        }
                                    }
                                    else
                                    {
                                        if (FortranLib.Substring(C3, 1, 1) == "M")
                                        {
                                            if (C4 == "QR" || C4 == "RQ" || C4 == "LQ" || C4 == "QL" || C4 == "HR" || C4 == "TR" || C4 == "BR")
                                            {
                                                NB = 32;
                                            }
                                        }
                                    }
                                }
                                else
                                {
                                    if (C2 == "GB")
                                    {
                                        if (C3 == "TRF")
                                        {
                                            if (SNAME)
                                            {
                                                if (N4 <= 64)
                                                {
                                                    NB = 1;
                                                }
                                                else
                                                {
                                                    NB = 32;
                                                }
                                            }
                                            else
                                            {
                                                if (N4 <= 64)
                                                {
                                                    NB = 1;
                                                }
                                                else
                                                {
                                                    NB = 32;
                                                }
                                            }
                                        }
                                    }
                                    else
                                    {
                                        if (C2 == "PB")
                                        {
                                            if (C3 == "TRF")
                                            {
                                                if (SNAME)
                                                {
                                                    if (N2 <= 64)
                                                    {
                                                        NB = 1;
                                                    }
                                                    else
                                                    {
                                                        NB = 32;
                                                    }
                                                }
                                                else
                                                {
                                                    if (N2 <= 64)
                                                    {
                                                        NB = 1;
                                                    }
                                                    else
                                                    {
                                                        NB = 32;
                                                    }
                                                }
                                            }
                                        }
                                        else
                                        {
                                            if (C2 == "TR")
                                            {
                                                if (C3 == "TRI")
                                                {
                                                    if (SNAME)
                                                    {
                                                        NB = 64;
                                                    }
                                                    else
                                                    {
                                                        NB = 64;
                                                    }
                                                }
                                            }
                                            else
                                            {
                                                if (C2 == "LA")
                                                {
                                                    if (C3 == "UUM")
                                                    {
                                                        if (SNAME)
                                                        {
                                                            NB = 64;
                                                        }
                                                        else
                                                        {
                                                            NB = 64;
                                                        }
                                                    }
                                                }
                                                else
                                                {
                                                    if (SNAME && C2 == "ST")
                                                    {
                                                        if (C3 == "EBZ")
                                                        {
                                                            NB = 1;
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
            ilaenv = NB;
            return ilaenv;
            // *
        LABEL60:;
            // *
            // *     ISPEC = 2:  minimum block size
            // *
            NBMIN = 2;
            if (C2 == "GE")
            {
                if (C3 == "QRF" || C3 == "RQF" || C3 == "LQF" || C3 == "QLF")
                {
                    if (SNAME)
                    {
                        NBMIN = 2;
                    }
                    else
                    {
                        NBMIN = 2;
                    }
                }
                else
                {
                    if (C3 == "HRD")
                    {
                        if (SNAME)
                        {
                            NBMIN = 2;
                        }
                        else
                        {
                            NBMIN = 2;
                        }
                    }
                    else
                    {
                        if (C3 == "BRD")
                        {
                            if (SNAME)
                            {
                                NBMIN = 2;
                            }
                            else
                            {
                                NBMIN = 2;
                            }
                        }
                        else
                        {
                            if (C3 == "TRI")
                            {
                                if (SNAME)
                                {
                                    NBMIN = 2;
                                }
                                else
                                {
                                    NBMIN = 2;
                                }
                            }
                        }
                    }
                }
            }
            else
            {
                if (C2 == "SY")
                {
                    if (C3 == "TRF")
                    {
                        if (SNAME)
                        {
                            NBMIN = 8;
                        }
                        else
                        {
                            NBMIN = 8;
                        }
                    }
                    else
                    {
                        if (SNAME && C3 == "TRD")
                        {
                            NBMIN = 2;
                        }
                    }
                }
                else
                {
                    if (CNAME && C2 == "HE")
                    {
                        if (C3 == "TRD")
                        {
                            NBMIN = 2;
                        }
                    }
                    else
                    {
                        if (SNAME && C2 == "OR")
                        {
                            if (FortranLib.Substring(C3, 1, 1) == "G")
                            {
                                if (C4 == "QR" || C4 == "RQ" || C4 == "LQ" || C4 == "QL" || C4 == "HR" || C4 == "TR" || C4 == "BR")
                                {
                                    NBMIN = 2;
                                }
                            }
                            else
                            {
                                if (FortranLib.Substring(C3, 1, 1) == "M")
                                {
                                    if (C4 == "QR" || C4 == "RQ" || C4 == "LQ" || C4 == "QL" || C4 == "HR" || C4 == "TR" || C4 == "BR")
                                    {
                                        NBMIN = 2;
                                    }
                                }
                            }
                        }
                        else
                        {
                            if (CNAME && C2 == "UN")
                            {
                                if (FortranLib.Substring(C3, 1, 1) == "G")
                                {
                                    if (C4 == "QR" || C4 == "RQ" || C4 == "LQ" || C4 == "QL" || C4 == "HR" || C4 == "TR" || C4 == "BR")
                                    {
                                        NBMIN = 2;
                                    }
                                }
                                else
                                {
                                    if (FortranLib.Substring(C3, 1, 1) == "M")
                                    {
                                        if (C4 == "QR" || C4 == "RQ" || C4 == "LQ" || C4 == "QL" || C4 == "HR" || C4 == "TR" || C4 == "BR")
                                        {
                                            NBMIN = 2;
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
            ilaenv = NBMIN;
            return ilaenv;
            // *
        LABEL70:;
            // *
            // *     ISPEC = 3:  crossover point
            // *
            NX = 0;
            if (C2 == "GE")
            {
                if (C3 == "QRF" || C3 == "RQF" || C3 == "LQF" || C3 == "QLF")
                {
                    if (SNAME)
                    {
                        NX = 128;
                    }
                    else
                    {
                        NX = 128;
                    }
                }
                else
                {
                    if (C3 == "HRD")
                    {
                        if (SNAME)
                        {
                            NX = 128;
                        }
                        else
                        {
                            NX = 128;
                        }
                    }
                    else
                    {
                        if (C3 == "BRD")
                        {
                            if (SNAME)
                            {
                                NX = 128;
                            }
                            else
                            {
                                NX = 128;
                            }
                        }
                    }
                }
            }
            else
            {
                if (C2 == "SY")
                {
                    if (SNAME && C3 == "TRD")
                    {
                        NX = 32;
                    }
                }
                else
                {
                    if (CNAME && C2 == "HE")
                    {
                        if (C3 == "TRD")
                        {
                            NX = 32;
                        }
                    }
                    else
                    {
                        if (SNAME && C2 == "OR")
                        {
                            if (FortranLib.Substring(C3, 1, 1) == "G")
                            {
                                if (C4 == "QR" || C4 == "RQ" || C4 == "LQ" || C4 == "QL" || C4 == "HR" || C4 == "TR" || C4 == "BR")
                                {
                                    NX = 128;
                                }
                            }
                        }
                        else
                        {
                            if (CNAME && C2 == "UN")
                            {
                                if (FortranLib.Substring(C3, 1, 1) == "G")
                                {
                                    if (C4 == "QR" || C4 == "RQ" || C4 == "LQ" || C4 == "QL" || C4 == "HR" || C4 == "TR" || C4 == "BR")
                                    {
                                        NX = 128;
                                    }
                                }
                            }
                        }
                    }
                }
            }
            ilaenv = NX;
            return ilaenv;
            // *
        LABEL80:;
            // *
            // *     ISPEC = 4:  number of shifts (used by xHSEQR)
            // *
            ilaenv = 6;
            return ilaenv;
            // *
        LABEL90:;
            // *
            // *     ISPEC = 5:  minimum column dimension (not used)
            // *
            ilaenv = 2;
            return ilaenv;
            // *
        LABEL100:;
            // *
            // *     ISPEC = 6:  crossover point for SVD (used by xGELSS and xGESVD)
            // *
            ilaenv = Convert.ToInt32(Math.Truncate(Convert.ToSingle(Math.Min(N1, N2)) * 1.6E0));
            return ilaenv;
            // *
        LABEL110:;
            // *
            // *     ISPEC = 7:  number of processors (not used)
            // *
            ilaenv = 1;
            return ilaenv;
            // *
        LABEL120:;
            // *
            // *     ISPEC = 8:  crossover point for multishift (used by xHSEQR)
            // *
            ilaenv = 50;
            return ilaenv;
            // *
        LABEL130:;
            // *
            // *     ISPEC = 9:  maximum size of the subproblems at the bottom of the
            // *                 computation tree in the divide-and-conquer algorithm
            // *                 (used by xGELSD and xGESDD)
            // *
            ilaenv = 25;
            return ilaenv;
            // *
        LABEL140:;
            // *
            // *     ISPEC = 10: ieee NaN arithmetic can be trusted not to trap
            // *
            // *     ILAENV = 0
            ilaenv = 1;
            if (ilaenv == 1)
            {
                ilaenv = this._ieeeck.Run(0, 0.0, 1.0);
            }
            return ilaenv;
            // *
        LABEL150:;
            // *
            // *     ISPEC = 11: infinity arithmetic can be trusted not to trap
            // *
            // *     ILAENV = 0
            ilaenv = 1;
            if (ilaenv == 1)
            {
                ilaenv = this._ieeeck.Run(1, 0.0, 1.0);
            }
            return ilaenv;
            // *
        LABEL160:;
            // *
            // *     12 <= ISPEC <= 16: xHSEQR or one of its subroutines. 
            // *
            ilaenv = this._iparmq.Run(ISPEC, NAME, OPTS, N1, N2, N3, N4);
            return ilaenv;
            // *
            // *     End of ILAENV
            // *

            #endregion

        }
    }
}
