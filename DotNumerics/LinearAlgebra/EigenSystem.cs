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
using DotNumerics.LinearAlgebra.CSEispack;
using DotNumerics.FortranLibrary;

namespace DotNumerics.LinearAlgebra
{
    /// <summary>
    /// Computes the eigenvalues and the eigenvectors of a square matrix.
    /// </summary>
    public sealed class EigenSystem
    {

        #region Fields

        DGEEV _dgeev;
        DSYEV _dsyev;
        DSBEV _dsbev;
        CG _cg;

        #endregion


        /// <summary>
        /// Initializes a new instance of the EigenSystem class.
        /// </summary>
        public EigenSystem()
        {

        }

        #region General matrix 


        /// <summary>
        /// Computes the eigenvalues for an N-by-N real nonsymmetric matrix A.
        /// </summary>
        /// <param name="A">N-by-N real nonsymmetric matrix A.</param>
        /// <returns>The eigenvalues.</returns>
        public ComplexMatrix GetEigenvalues(Matrix A)
        {

            if (this._dgeev == null) this._dgeev = new DGEEV();

            this.CheckDimensions(A);


            Matrix ACopy = A.Clone();
            double[] ACopyData = ACopy.Data;
            Matrix RealEVectors = new Matrix(1, 1);  
            double[] EigenVectsData = RealEVectors.Data;
            ComplexMatrix EigenVals = new ComplexMatrix(A.RowCount, 1);

            double[] REigVal = new double[A.RowCount];
            double[] IEigVal = new double[A.RowCount];

            //double[] EigenValsData = EigenVals.Data;
            int Info = 0;

            double[] VL = new double[A.RowCount];

            double[] Work = new double[1];
            int LWork = -1;

            //Calculamos LWORK 
            _dgeev.Run("N", "N", A.RowCount, ref ACopyData, 0, ACopy.RowCount, ref REigVal, 0, ref IEigVal, 0, ref VL, 0, 1, ref  EigenVectsData, 0, A.RowCount, ref Work, 0, LWork, ref Info);

            LWork = Convert.ToInt32(Work[0]);
            if (LWork > 0)
            {
                Work = new double[LWork];
                _dgeev.Run("N", "N", A.RowCount, ref ACopyData, 0, ACopy.RowCount, ref REigVal, 0, ref IEigVal, 0, ref VL, 0, 1, ref  EigenVectsData, 0, A.RowCount, ref Work, 0, LWork, ref Info);
            }
            else
            {

                //Error
            }


            #region Error
            //= 0:  successful exit
            //.LT. 0:  if INFO = -i, the i-th argument had an illegal value.
            //.GT. 0:  if INFO = i, the QR algorithm failed to compute all the
            // eigenvalues, and no eigenvectors have been computed;
            // elements i+1:N of WR and WI contain eigenvalues which
            // have converged.

            if (Info < 0)
            {
                string infoSTg = Math.Abs(Info).ToString();
                throw new ArgumentException("the " + infoSTg + " -th argument had an illegal value");
            }
            else if (Info > 0)
            {
                string infoSTg = Math.Abs(Info).ToString();
                throw new Exception("The QR algorithm failed to compute all the eigenvalues.");
            }

            #endregion


            for (int i = 0; i < EigenVals.RowCount; i++)
            {
                EigenVals[i, 0] = new Complex(REigVal[i], IEigVal[i]);
            }


            return EigenVals;

        }

        /// <summary>
        /// Computes for an N-by-N real nonsymmetric matrix A, the
        /// eigenvalues and eigenvectors.
        /// </summary>
        /// <param name="A">N-by-N real nonsymmetric matrix A.</param>
        /// <param name="EigenVectors">The eigenvectors.</param>
        /// <returns>The eigenvalues.</returns>
        public ComplexMatrix GetEigenvalues(Matrix A, out ComplexMatrix EigenVectors)
        {

            if (this._dgeev == null) this._dgeev = new DGEEV();

            this.CheckDimensions(A);


            Matrix ACopy = A.Clone();
            double[] ACopyData = ACopy.Data;
            EigenVectors = new ComplexMatrix(A.RowCount, A.ColumnCount);
            Matrix RealEVectors = new Matrix(A.RowCount, A.ColumnCount);
            double[] EigenVectsData = RealEVectors.Data;
            ComplexMatrix EigenVals = new ComplexMatrix(A.RowCount, 1);

            double[] REigVal = new double[A.RowCount];
            double[] IEigVal = new double[A.RowCount];

            //double[] EigenValsData = EigenVals.Data;
            int Info = 0;

            double[] VL = new double[A.RowCount];

            double[] Work = new double[1];
            int LWork = -1;

            //Calculamos LWORK 
            _dgeev.Run("N", "V", A.RowCount, ref ACopyData, 0, ACopy.RowCount, ref REigVal, 0, ref IEigVal, 0, ref VL, 0, 1, ref  EigenVectsData, 0, A.RowCount, ref Work, 0, LWork, ref Info);

            LWork = Convert.ToInt32(Work[0]);
            if (LWork > 0)
            {
                Work = new double[LWork];
                _dgeev.Run("N", "V", A.RowCount, ref ACopyData, 0, ACopy.RowCount, ref REigVal, 0, ref IEigVal, 0, ref VL, 0, 1, ref  EigenVectsData, 0, A.RowCount, ref Work, 0, LWork, ref Info);
            }
            else
            {

                //Error
            }


            #region Error
            //= 0:  successful exit
            //.LT. 0:  if INFO = -i, the i-th argument had an illegal value.
            //.GT. 0:  if INFO = i, the QR algorithm failed to compute all the
            // eigenvalues, and no eigenvectors have been computed;
            // elements i+1:N of WR and WI contain eigenvalues which
            // have converged.

            if (Info < 0)
            {
                string infoSTg = Math.Abs(Info).ToString();
                throw new ArgumentException("the " + infoSTg + " -th argument had an illegal value");
            }
            else if (Info > 0)
            {
                string infoSTg = Math.Abs(Info).ToString();
                throw new Exception("The QR algorithm failed to compute all the eigenvalues.");
            }

            #endregion


            for (int i = 0; i < EigenVals.RowCount; i++)
            {
                EigenVals[i, 0] = new Complex(REigVal[i], IEigVal[i]);
            }

            for (int i = 0; i < EigenVals.RowCount; i++)
            {
                if (EigenVals[i, 0].Imaginary == 0.0)
                {
                    for (int j = 0; j < EigenVectors.RowCount; j++)
                    {
                        EigenVectors[j, i] = new Complex(RealEVectors[j, i], 0.0);
                    }
                }
                else
                {
                    if (EigenVals[i, 0].Imaginary > 0.0)
                    {
                        for (int j = 0; j < EigenVectors.RowCount; j++)
                        {
                            EigenVectors[j, i] = new Complex(RealEVectors[j, i], RealEVectors[j, i + 1]);
                        }
                    }
                    else
                    {
                        for (int j = 0; j < EigenVectors.RowCount; j++)
                        {
                            EigenVectors[j, i] = new Complex(RealEVectors[j, i - 1], -RealEVectors[j, i]);

                        }
                    }
                }
            }

            return EigenVals;
        }

        #endregion


        #region SymmetricMatrix 

        /// <summary>
        /// Computes all eigenvalues of a real symmetric matrix A.
        /// </summary>
        /// <param name="A">The real symmetric matrix A.</param>
        /// <returns>The eigenvalues.</returns>
        public Matrix GetEigenvalues(SymmetricMatrix A)
        {
            if (this._dsyev == null) this._dsyev = new DSYEV();


            this.CheckDimensions(A);

            Matrix EigenVects = new Matrix(A.RowCount, A.ColumnCount, A.Data);
            double[] EigenVectsData = EigenVects.Data;
            Matrix EigenVals = new Matrix(A.RowCount, 1);
            double[] EigenValsData = EigenVals.Data;
            int Info = 0;

            double[] Work = new double[1];

            int LWork = -1;
            //Calculamos LWORK ideal

            _dsyev.Run("N", "U", A.RowCount, ref EigenVectsData, 0, A.RowCount, ref EigenValsData, 0, ref Work, 0, LWork, ref Info);

            LWork = Convert.ToInt32(Work[0]);
            if (LWork > 0)
            {
                Work = new double[LWork];
                _dsyev.Run("N", "U", A.RowCount, ref EigenVectsData, 0, A.RowCount, ref EigenValsData, 0, ref Work, 0, LWork, ref Info);
            }
            else
            {

                //Error
            }

            #region Error
            /// = 0:  successful exit
            /// .LT. 0:  if INFO = -i, the i-th argument had an illegal value
            /// .GT. 0:  if INFO = i, the algorithm failed to converge; i
            /// off-diagonal elements of an intermediate tridiagonal
            /// form did not converge to zero.

            if (Info < 0)
            {
                string infoSTg = Math.Abs(Info).ToString();
                throw new ArgumentException("the " + infoSTg + " -th argument had an illegal value");
            }
            else if (Info > 0)
            {
                string infoSTg = Math.Abs(Info).ToString();
                throw new Exception("The algorithm failed to converge.");
            }

            #endregion


            return EigenVals;
        }

        /// <summary>
        /// Computes all eigenvalues and eigenvectors of a of a real symmetric matrix A.
        /// </summary>
        /// <param name="A">The real symmetric matrix A.</param>
        /// <param name="EigenVects">The eigenvectors.</param>
        /// <returns>The eigenvalues.</returns>
        public Matrix GetEigenvalues(SymmetricMatrix A, out Matrix EigenVects)
        {

            if (this._dsyev == null) this._dsyev = new DSYEV();
            this.CheckDimensions(A);


            EigenVects = new Matrix(A.RowCount, A.ColumnCount, A.Data);
            double[] EigenVectsData = EigenVects.Data;
            Matrix EigenVals = new Matrix(A.RowCount, 1);
            double[] EigenValsData = EigenVals.Data;
            int Info = 0;

            double[] Work = new double[1];

            int LWork = -1;
            //Calculamos LWORK ideal

            _dsyev.Run("V", "U", A.RowCount, ref EigenVectsData, 0, A.RowCount, ref EigenValsData, 0, ref Work, 0, LWork, ref Info);

            LWork = Convert.ToInt32(Work[0]);
            if (LWork > 0)
            {
                Work = new double[LWork];
                _dsyev.Run("V", "U", A.RowCount, ref EigenVectsData, 0, A.RowCount, ref EigenValsData, 0, ref Work, 0, LWork, ref Info);
            }
            else
            {

                //Error
            }

            #region Error
            /// = 0:  successful exit
            /// .LT. 0:  if INFO = -i, the i-th argument had an illegal value
            /// .GT. 0:  if INFO = i, the algorithm failed to converge; i
            /// off-diagonal elements of an intermediate tridiagonal
            /// form did not converge to zero.

            if (Info < 0)
            {
                string infoSTg = Math.Abs(Info).ToString();
                throw new ArgumentException("the " + infoSTg + " -th argument had an illegal value");
            }
            else if (Info > 0)
            {
                string infoSTg = Math.Abs(Info).ToString();
                throw new Exception("The algorithm failed to converge.");
            }

            #endregion


            return EigenVals;
        }

        #endregion


        #region SymmetricBandMatrix

        /// <summary>
        ///Computes all the eigenvalues of
        /// a real symmetric band matrix A.
        /// </summary>
        /// <param name="A">The real symmetric band matrix A.</param>
        /// <returns>The eigenvalues.</returns>
        public Matrix GetEigenvalues(SymmetricBandMatrix A)
        {
            if (this._dsbev == null) this._dsbev = new DSBEV();

            this.CheckDimensions(A);

            Matrix SymmetricBand = A.GetSymmetricBandPackedMatrix();
            double[] SymmetricBandData = SymmetricBand.Data;
            Matrix EigenVects = new Matrix(1, 1);  //Se pone (1,1) pues nos e usaran
            double[] EigenVectsData = EigenVects.Data;
            Matrix EigenVals = new Matrix(A.RowCount, 1);
            double[] EigenValsData = EigenVals.Data;
            int Info = 0;

            double[] Work = new double[3 * A.RowCount - 2];


            _dsbev.Run("N", "U", A.RowCount, A.UpperBandWidth, ref SymmetricBandData, 0, SymmetricBand.RowCount, ref EigenValsData, 0, ref EigenVectsData, 0, A.RowCount, ref Work, 0, ref Info);


            #region Error
            /// = 0:  successful exit
            /// .LT. 0:  if INFO = -i, the i-th argument had an illegal value
            /// .GT. 0:  if INFO = i, the algorithm failed to converge; i
            /// off-diagonal elements of an intermediate tridiagonal
            /// form did not converge to zero.

            if (Info < 0)
            {
                string infoSTg = Math.Abs(Info).ToString();
                throw new ArgumentException("the " + infoSTg + " -th argument had an illegal value");
            }
            else if (Info > 0)
            {
                string infoSTg = Math.Abs(Info).ToString();
                throw new Exception("The algorithm failed to converge.");
            }

            #endregion


            return EigenVals;
        }

        /// <summary>
        ///Computes all the eigenvalues and eigenvectors of
        /// a real symmetric band matrix A.
        /// </summary>
        /// <param name="A">The real symmetric band matrix A.</param>
        /// <param name="EigenVects">The eigenvectors.</param>
        /// <returns>The eigenvalues.</returns>
        public Matrix GetEigenvalues(SymmetricBandMatrix A, out Matrix EigenVects)
        {
            if (this._dsbev == null) this._dsbev = new DSBEV();
            this.CheckDimensions(A);

            Matrix SymmetricBand = A.GetSymmetricBandPackedMatrix();
            double[] SymmetricBandData = SymmetricBand.Data;
            EigenVects = new Matrix(A.RowCount, A.ColumnCount);
            double[] EigenVectsData = EigenVects.Data;
            Matrix EigenVals = new Matrix(A.RowCount, 1);
            double[] EigenValsData = EigenVals.Data;
            int Info = 0;

            double[] Work = new double[3 * A.RowCount - 2];


            _dsbev.Run("V", "U", A.RowCount, A.UpperBandWidth, ref SymmetricBandData, 0, SymmetricBand.RowCount, ref EigenValsData, 0, ref EigenVectsData, 0, A.RowCount, ref Work, 0, ref Info);


            #region Error
            /// = 0:  successful exit
            /// .LT. 0:  if INFO = -i, the i-th argument had an illegal value
            /// .GT. 0:  if INFO = i, the algorithm failed to converge; i
            /// off-diagonal elements of an intermediate tridiagonal
            /// form did not converge to zero.

            if (Info < 0)
            {
                string infoSTg = Math.Abs(Info).ToString();
                throw new ArgumentException("the " + infoSTg + " -th argument had an illegal value");
            }
            else if (Info > 0)
            {
                string infoSTg = Math.Abs(Info).ToString();
                throw new Exception("The algorithm failed to converge.");
            }

            #endregion


            return EigenVals;
        }

        #endregion


        #region Complex General matrix

        /// <summary>
        /// Computes the eigenvalues for an complex general matrix A.
        /// </summary>
        /// <param name="A">The complex general matrix A.</param>
        /// <returns>The eigenvalues.</returns>
        public ComplexMatrix GetEigenvalues(ComplexMatrix A)
        {
            //Fortran Ejemplo
            //CG(NM,N,AR,AI,WR,WI,1,ZR,ZI,SCALE,ORTR,ORTI,ERROR)
            //C
            //C     THIS DRIVER TESTS  EISPACK  FOR THE CLASS OF COMPLEX GENERAL
            //C     MATRICES SUMMARIZING THE FIGURES OF MERIT FOR ALL PATHS.
            //C
            //C     THIS DRIVER IS CATALOGUED AS  EISPDRV4(CGSUMARY).
            //C
            //C     THE DIMENSION OF  AR,AI,ZR,ZI,ASAVER,ASAVEI,RM1,  AND  RM2 SHOULD
            //C     BE  NM  BY  NM.
            //C     THE DIMENSION OF  WR,WI,WR1,WI1,SELECT,SLHOLD,INT,SCALE,ORTR,ORTI,
            //C     RV1  AND  RV2  SHOULD BE  NM.
            //C     THE DIMENSION OF  ARHOLD  AND  AIHOLD  SHOULD BE  NM  BY  NM.
            //C     HERE NM = 20.


            if (this._cg == null) this._cg = new CG();

            this.CheckDimensions(A);

            Matrix AReal = A.GetReal();
            double[] ARealData = AReal.Data;
            Matrix AImag = A.GetImag();
            double[] AImagData = AImag.Data;

            ComplexMatrix EigenVals = new ComplexMatrix(A.RowCount, 1);
            Matrix RealEigenVals = new Matrix(A.RowCount, 1);
            double[] RealEigenValsData = RealEigenVals.Data;
            Matrix ImagEigenVals = new Matrix(A.RowCount, 1);
            double[] ImagEigenValsData = ImagEigenVals.Data;

            ComplexMatrix EigenVectors = new ComplexMatrix(1, 1);   
            Matrix RealEigVect = new Matrix(A.RowCount);
            double[] RealEigVectData = RealEigVect.Data;
            Matrix ImagEigVect = new Matrix(A.RowCount);
            double[] ImagEigVectData = ImagEigVect.Data;

            double[] SCALE = new double[A.RowCount];
            double[] ORTR = new double[A.RowCount];
            double[] ORTI = new double[A.RowCount];

            int Info = 0;

            int matz = 0; //No se calculan los eigenvectores

            _cg.Run(A.RowCount, A.RowCount, ref ARealData, 0, ref AImagData, 0, ref RealEigenValsData, 0, ref ImagEigenValsData, 0,
                matz, ref RealEigVectData, 0, ref ImagEigVectData, 0, ref SCALE, 0, ref ORTR, 0, ref ORTI, 0, ref Info);


            #region Error
            /// is set to
            /// zero       for normal return,
            /// j          if the limit of 30*n iterations is exhausted
            /// while the j-th eigenvalue is being sought.

            if (Info != 0)
            {
                throw new ArgumentException("The limit of 30*n iterations is exhausted");
            }

            #endregion



            EigenVals.SetReal(RealEigenVals);
            EigenVals.SetImag(ImagEigenVals);

            EigenVectors.SetReal(RealEigVect);
            EigenVectors.SetImag(ImagEigVect);

            return EigenVals;
        }

        /// <summary>
        /// Computes the eigenvalues and eigenvectors for an complex general matrix A.
        /// </summary>
        /// <param name="A">The complex general matrix A.</param>
        /// <param name="EigenVectors">The eigenvectors.</param>
        /// <returns>The eigenvalues.</returns>
        public ComplexMatrix GetEigenvalues(ComplexMatrix A, out ComplexMatrix EigenVectors)
        {
            //Fortran Ejemplo
            //CG(NM,N,AR,AI,WR,WI,1,ZR,ZI,SCALE,ORTR,ORTI,ERROR)
            //C
            //C     THIS DRIVER TESTS  EISPACK  FOR THE CLASS OF COMPLEX GENERAL
            //C     MATRICES SUMMARIZING THE FIGURES OF MERIT FOR ALL PATHS.
            //C
            //C     THIS DRIVER IS CATALOGUED AS  EISPDRV4(CGSUMARY).
            //C
            //C     THE DIMENSION OF  AR,AI,ZR,ZI,ASAVER,ASAVEI,RM1,  AND  RM2 SHOULD
            //C     BE  NM  BY  NM.
            //C     THE DIMENSION OF  WR,WI,WR1,WI1,SELECT,SLHOLD,INT,SCALE,ORTR,ORTI,
            //C     RV1  AND  RV2  SHOULD BE  NM.
            //C     THE DIMENSION OF  ARHOLD  AND  AIHOLD  SHOULD BE  NM  BY  NM.
            //C     HERE NM = 20.

            if (this._cg == null) this._cg = new CG();

            this.CheckDimensions(A);

            Matrix AReal = A.GetReal();
            double[] ARealData = AReal.Data;
            Matrix AImag = A.GetImag();
            double[] AImagData = AImag.Data;

            ComplexMatrix EigenVals = new ComplexMatrix(A.RowCount, 1);
            Matrix RealEigenVals = new Matrix(A.RowCount, 1);
            double[] RealEigenValsData = RealEigenVals.Data;
            Matrix ImagEigenVals = new Matrix(A.RowCount, 1);
            double[] ImagEigenValsData = ImagEigenVals.Data;

            EigenVectors = new ComplexMatrix(A.RowCount, A.ColumnCount);
            Matrix RealEigVect = new Matrix(A.RowCount);
            double[] RealEigVectData = RealEigVect.Data;
            Matrix ImagEigVect = new Matrix(A.RowCount);
            double[] ImagEigVectData = ImagEigVect.Data;

            double[] SCALE = new double[A.RowCount];
            double[] ORTR = new double[A.RowCount];
            double[] ORTI = new double[A.RowCount];

            int Info = 0;
            int matz = 1; //Se calculan los eigenvalores y los eigenvectores
            _cg.Run(A.RowCount, A.RowCount, ref ARealData, 0, ref AImagData, 0, ref RealEigenValsData, 0, ref ImagEigenValsData, 0,
                matz, ref RealEigVectData, 0, ref ImagEigVectData, 0, ref SCALE, 0, ref ORTR, 0, ref ORTI, 0, ref Info);


            #region Error
            /// is set to
            /// zero       for normal return,
            /// j          if the limit of 30*n iterations is exhausted
            /// while the j-th eigenvalue is being sought.

            if (Info != 0)
            {
                throw new ArgumentException("The limit of 30*n iterations is exhausted");
            }

            #endregion



            EigenVals.SetReal(RealEigenVals);
            EigenVals.SetImag(ImagEigenVals);

            EigenVectors.SetReal(RealEigVect);
            EigenVectors.SetImag(ImagEigVect);

            return EigenVals;
        }



        #endregion


        #region Private methods

        private void CheckDimensions(BaseMatrix matrixA)
        {
            if (matrixA.IsSquare != true)
            {
                throw new System.ArgumentException("Matrix A is not a square matrix.");
            }

        }

        private void CheckDimensions(ComplexMatrix matrixA)
        {
            if (matrixA.IsSquare != true)
            {
                throw new System.ArgumentException("Matrix A is not a square matrix.");
            }

        }

        #endregion


    }
}
