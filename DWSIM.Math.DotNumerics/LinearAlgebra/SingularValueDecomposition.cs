#region Copyright © 2009, De Santiago-Castillo JA. All rights reserved.

//Copyright © 2009 Jose Antonio De Santiago-Castillo 
//E-mail:JAntonioDeSantiago@gmail.com
//Web: www.DotNumerics.com
//
#endregion

using System;
using System.Collections.Generic;
using System.Text;
using System.ComponentModel;
using DotNumerics.LinearAlgebra.CSLapack;


namespace DotNumerics.LinearAlgebra
{
    /// <summary>
    /// Computes the singular value decomposition (SVD) of a real
    /// M-by-N matrix A.
    /// </summary>
    /// <remarks>
    /// The SVD is written
    /// 
    /// A = U * S * transpose(V)
    /// 
    /// where S is an M-by-N matrix which is zero except for its
    /// min(m,n) diagonal elements, U is an M-by-M orthogonal matrix, and
    /// V is an N-by-N orthogonal matrix.  The diagonal elements of S
    /// are the singular values of A; they are real and non-negative, and
    /// are returned in descending order.  The first min(m,n) columns of
    /// U and V are the left and right singular vectors of A.
    /// 
    /// Note that the routine returns V**T, not V.
    /// </remarks>
    public sealed class SingularValueDecomposition 
    {

        #region Fields

        DGESVD _dgesvd;

        #endregion

        /// <summary>
        /// Initializes a new instance of the SingularValues class.
        /// </summary>
        public SingularValueDecomposition()
        {

            //MeDGESVD = new DGESVD();

        }

        #region Public Metods

        /// <summary>
        ///Computes the singular value decomposition (SVD) of a real
        /// M-by-N matrix A.
        /// The SVD is written
        /// A = U * S * transpose(V)
        /// </summary>
        /// <param name="A">The A matrix.</param>
        /// <param name="S">The diagonal elements of S are the singular values of A.</param>
        public void ComputeSVD(Matrix A, out Matrix S)
        {

            Vector singularValues;

            S = new Matrix(A.RowCount, A.ColumnCount);// A is MxN, S is  MxN

            this.ComputeSVD(A, out singularValues);

            for (int i = 0; i < singularValues.Length; i++)
            {
                S[i, i] = singularValues[i];
            }
        }

        /// <summary>
        ///Computes the singular value decomposition (SVD) of a real
        /// M-by-N matrix A.
        /// The SVD is written
        /// A = U * S * transpose(V)
        /// </summary>
        /// <param name="A">The A matrix.</param>
        /// <param name="S">A vector of singular values.</param>
        public void ComputeSVD(Matrix A, out Vector S)
        {

            if (this._dgesvd == null) this._dgesvd = new DGESVD();

            Matrix ACopy = A.Clone();
            double[] ACopyData = ACopy.Data;
            S = new Vector(Math.Min(A.RowCount, A.ColumnCount));// (output) DOUBLE PRECISION array, dimension (min(M,N))
            double[] SingularValuesData = S.Data;
            Matrix U = new Matrix(1, 1); // A is MxN, U is  MxM, como aqui no se requiere no importa
            double[] UData = U.Data;
            Matrix VT = new Matrix(1, 1);// A is MxN, V is  NxN, como aqui no se requiere no importa
            double[] VTData = VT.Data;

            double[] Work = new double[1];
            int LWork = -1;
            int Info = 0;

            //Calculamos LWORK 
            this._dgesvd.Run("N", "N", A.RowCount, A.ColumnCount, ref ACopyData, 0, A.RowCount, ref SingularValuesData, 0, ref UData, 0, U.RowCount, ref VTData, 0, VT.RowCount, ref Work, 0, LWork, ref Info);

            LWork = Convert.ToInt32(Work[0]);
            if (LWork > 0)
            {
                Work = new double[LWork];
                _dgesvd.Run("N", "N", A.RowCount, A.ColumnCount, ref ACopyData, 0, A.RowCount, ref SingularValuesData, 0, ref UData, 0, U.RowCount, ref VTData, 0, VT.RowCount, ref Work, 0, LWork, ref Info);
            }
            else
            {
                //Error
            }

            #region Error
            // <param name="INFO">
            // (output) INTEGER
            // = 0:  successful exit.
            // .LT. 0:  if INFO = -i, the i-th argument had an illegal value.
            // .GT. 0:  if DBDSQR did not converge, INFO specifies how many
            // superdiagonals of an intermediate bidiagonal form B
            // did not converge to zero. See the description of WORK
            // above for details.

            if (Info < 0)
            {
                string infoSTg = Math.Abs(Info).ToString();
                throw new ArgumentException("the " + infoSTg + " -th argument had an illegal value");
            }
            else if (Info > 0)
            {
                string infoSTg = Math.Abs(Info).ToString();
                throw new Exception("DBDSQR did not converge.");
            }

            #endregion

        }


        /// <summary>
        ///Computes the singular value decomposition (SVD) of a real
        /// M-by-N matrix A.
        /// The SVD is written
        /// A = U * S * transpose(V)
        /// </summary>
        /// <param name="A">The A matrix.</param>
        /// <param name="S">The diagonal elements of S are the singular values of A.</param>
        /// <param name="U">The U matrix, U is an M-by-M orthogonal matrix</param>
        /// <param name="VT">the transpose(V), V is an N-by-N orthogonal matrix.</param>
        public void ComputeSVD(Matrix A, out Matrix S, out Matrix U, out Matrix VT)
        {

            if (this._dgesvd == null) this._dgesvd = new DGESVD();

            Matrix ACopy = A.Clone();
            double[] ACopyData = ACopy.Data;
            S = new Matrix(A.RowCount, A.ColumnCount);// A is MxN, S is  MxN
            double[] SingularValuesData = new double[Math.Min(A.RowCount, A.ColumnCount)];
            U = new Matrix(A.RowCount, A.RowCount); // A is MxN, U is  MxM
            double[] UData = U.Data;
            VT = new Matrix(A.ColumnCount, A.ColumnCount);// A is MxN, V is  NxN
            double[] VTData = VT.Data;

            double[] Work = new double[1];
            int LWork = -1;
            int Info = 0;

            //Calculamos LWORK 
            this._dgesvd.Run("A", "A", A.RowCount, A.ColumnCount, ref ACopyData, 0, A.RowCount, ref SingularValuesData, 0, ref UData, 0, U.RowCount, ref VTData, 0, VT.RowCount, ref Work, 0, LWork, ref Info);

            LWork = Convert.ToInt32(Work[0]);
            if (LWork > 0)
            {
                Work = new double[LWork];
                _dgesvd.Run("A", "A", A.RowCount, A.ColumnCount, ref ACopyData, 0, A.RowCount, ref SingularValuesData, 0, ref UData, 0, U.RowCount, ref VTData, 0, VT.RowCount, ref Work, 0, LWork, ref Info);
            }
            else
            {
                //Error
            }

            #region Error
            // <param name="INFO">
            // (output) INTEGER
            // = 0:  successful exit.
            // .LT. 0:  if INFO = -i, the i-th argument had an illegal value.
            // .GT. 0:  if DBDSQR did not converge, INFO specifies how many
            // superdiagonals of an intermediate bidiagonal form B
            // did not converge to zero. See the description of WORK
            // above for details.

            if (Info < 0)
            {
                string infoSTg = Math.Abs(Info).ToString();
                throw new ArgumentException("the " + infoSTg + " -th argument had an illegal value");
            }
            else if (Info > 0)
            {
                string infoSTg = Math.Abs(Info).ToString();
                throw new Exception("DBDSQR did not converge.");
            }

            #endregion

            for (int i = 0; i < SingularValuesData.Length; i++)
            {
                S[i, i] = SingularValuesData[i];
            }

        }

        #endregion

    }
}
