#region Copyright © 2009, De Santiago-Castillo JA. All rights reserved.

//Copyright © 2009 Jose Antonio De Santiago-Castillo 
//E-mail:JAntonioDeSantiago@gmail.com
//Web: www.DotNumerics.com
//
#endregion

using System;
using System.Collections.Generic;
using System.Text;
using System.Diagnostics;
using System.IO;
using DotNumerics.FortranLibrary;

namespace DotNumerics.LinearAlgebra
{
    /// <summary>
    /// Represents a Complex Vector.
    /// </summary>
    [DebuggerDisplay(": {Type} , Length : {Length}", Name = "vector")]
    [DebuggerTypeProxy(typeof(VectorComplexDebuggerDisplay))]
    public class ComplexVector
    {

        #region Fields
        /// <summary>
        /// Los datos del vector
        /// </summary>
        [DebuggerBrowsable(DebuggerBrowsableState.Never)]
        protected Complex[] _Data;

        /// <summary>
        /// El tipo de vector.
        /// </summary>
        [DebuggerBrowsable(DebuggerBrowsableState.Never)]
        protected VectorType _Type = VectorType.Column;

        #endregion

        #region Constructor

        /// <summary>
        /// Initializes a new instance of the VectorComplex class of the given size.
        /// </summary>
        /// <param name="length">The vector length</param>
        public ComplexVector(int length) : this(VectorType.Column, length) { }

        /// <summary>
        /// Initializes a new instance of the Vector class of the given size and type.
        /// </summary>
        /// <param name="type">The vector type</param>
        /// <param name="length">length">The vector length</param>
        public ComplexVector(VectorType type, int length)
        {
            if (length < 1) throw new System.ArgumentException("length < 1");
            this._Type = type;
            this._Data = new Complex[length];
        }

        /// <summary>
        /// Initializes a new instance of the Vector class that contains elements 
        /// copied from the specified array.
        /// </summary>
        /// <param name="data">The array whose elements are copied to the vector.</param>
        public ComplexVector(Complex[] data) : this(VectorType.Column, data) { }

        /// <summary>
        /// Initializes a new instance of the Vector class that contains elements
        /// copied from the specified array.
        /// </summary>
        /// <param name="type">The vector type</param>
        /// <param name="data">The array whose elements are copied to the vector.</param>
        public ComplexVector(VectorType type, Complex[] data)
        {
            if (data.Length < 1) throw new System.ArgumentException("data.Length < 1");
            this._Type = type;
            this._Data = new Complex[data.Length];

            data.CopyTo(this._Data, 0);

            //for (int i = 0; i < data.Length; i++)
            //{
            //    this.MeData[i] = data[i];
            //}
        }

        #endregion

        #region Public Properties

        /// <summary>
        /// Los datos del vector
        /// </summary>
        [DebuggerBrowsable(DebuggerBrowsableState.Never)]
        internal Complex[] Data
        {
            get { return this._Data; }
        }

        /// <summary>
        /// Returns the number of rows.
        /// </summary>
        [DebuggerBrowsable(DebuggerBrowsableState.Never)]
        public int Length
        {
            get { return this._Data.Length; }
        }

        public VectorType Type
        {
            get { return this._Type; }
            set { this._Type = value; }
        }

        /// <summary>
        /// Gets or sets the element at the specified index.
        /// </summary>
        /// <param name="index">The zero-based index of the element to get or set.</param>
        /// <returns>The element at the specified index.</returns>
        public virtual Complex this[int index]
        {
            get
            {
                return this._Data[index];
            }
            set
            {
                this._Data[index] = value;
            }
        }

        #endregion

        #region Operators

        /// <summary>
        /// Vector addition.
        /// </summary>
        /// <param name="A">The left side vector of the addition operator.</param>
        /// <param name="B">The right side vector of the addition operator.</param>
        /// <returns>A vector that represents the result of the addition.</returns>
        public static ComplexVector operator +(ComplexVector A, ComplexVector B)
        {
            return A.Add(B);
        }


        /// <summary>
        /// Unary minus.
        /// </summary>
        /// <param name="v">The vector.</param>
        /// <returns>Vector r[i] = -this[i]</returns>
        public static ComplexVector operator -(ComplexVector v)
        {
            return v.UnaryMinus();
        }


        /// <summary>
        /// Vector subtraction.
        /// </summary>
        /// <param name="A"> The left side vector of the subtraction operator.</param>
        /// <param name="B">The right side vector of the subtraction operator.</param>
        /// <returns>A vector that represents the result of the vector subtraction.</returns>
        public static ComplexVector operator -(ComplexVector A, ComplexVector B)
        {
            return A.Subtract(B);
        }

        /// <summary>
        /// Scalar-Vector multiplication.
        /// </summary>
        /// <param name="c"> The left side complex number of the multiplication operator.</param>
        /// <param name="A">The right side vector of the multiplication operator.</param>
        /// <returns>A vector that represents the result of the multiplication.</returns>
        public static ComplexVector operator *(Complex c, ComplexVector A)
        {
            return A.Multiply(c);
        }

        /// <summary>
        /// Vector-Scalar multiplication.
        /// </summary>
        /// <param name="A">The left side vector of the multiplication operator.</param>
        /// <param name="c"> The right side complex number of the multiplication operator.</param>
        /// <returns>A vector that represents the result of the multiplication.</returns>
        public static ComplexVector operator *(ComplexVector A, Complex c)
        {
            return A.Multiply(c);
        }

        ///// <summary>Vector multiplication.</summary>
        //public static Complex operator *(VectorComplex A, VectorComplex B)
        //{
        //    if (A.Type != VectorType.Row || B.Type != VectorType.Column  || B.Length != A.Length)
        //    {
        //        throw new System.ArgumentException("Vector dimensions or type are not valid.");
        //    }

        //    Complex C = new Complex(0.0, 0.0);

        //    Complex[] AData = A.Data;
        //    Complex[] BData = B.Data;

        //    for (int i = 0; i < AData.Length; i++)
        //    {
        //        C += AData[i] * BData[i];
        //    }

        //    return C;
        //}

        ///// <summary>
        ///// The dot product
        ///// </summary>
        ///// <param name="A"></param>
        ///// <returns>The dot product of A.</returns>
        //public static double DotProduct(Vector A )
        //{
        //    double C = 0.0;
        //    double[] AData = A.Data;
        //    for (int i = 0; i < AData.Length; i++)
        //    {
        //        C += AData[i] * AData[i];
        //    }
        //    return C;
        //}

        /// <summary>
        /// Transposed vector.
        /// </summary>
        /// <returns></returns>
        public ComplexVector Transpose()
        {
            ComplexVector AT = new ComplexVector(this._Data);

            if (this._Type == VectorType.Column) AT.Type = VectorType.Row;
            else AT.Type = VectorType.Column;

            return AT;
        }

        ///// <summary>Matrix- Vector multiplication.</summary>
        //public static MatrixComplex operator *(MatrixComplex A, VectorComplex B)
        //{

        //    int BRows;
        //    int BColumns;

        //    if (B.Type == VectorType.Column)
        //    {
        //        BColumns = 1;
        //        BRows = B.Length;
        //    }
        //    else
        //    {
        //        BColumns = B.Length;
        //        BRows = 1;
        //    }



        //    if (BRows != A.Columns)
        //    {
        //        throw new System.ArgumentException("Matrix dimensions are not valid.");
        //    }

        //    MatrixComplex C = new MatrixComplex(A.Rows, BColumns);

        //    Complex[] AData = A.Data;
        //    Complex[] BData = B.Data;
        //    Complex[] CData = C.Data;

        //    int ARows = A.Rows;
        //    int AColumns = A.Columns;


        //    Complex Sum;
        //    for (int j = 0; j < BColumns; j++)
        //    {
        //        for (int i = 0; i < ARows; i++)
        //        {
        //            Sum = new Complex(0.0, 0.0);
        //            for (int k = 0; k < AColumns; k++)
        //            {
        //                Sum += AData[i + k * ARows] * BData[k + j * BRows];
        //            }
        //            CData[i + j * ARows] = Sum;
        //        }
        //    }
        //    return C;
        //}


        ///// <summary>Matrix- Vector multiplication.</summary>
        //public static MatrixComplex operator *(VectorComplex A, MatrixComplex B)
        //{

        //    int ARows;
        //    int AColumns;

        //    if (A.Type == VectorType.Column)
        //    {
        //        AColumns = 1;
        //        ARows = A.Length;
        //    }
        //    else
        //    {
        //        AColumns = A.Length;
        //        ARows = 1;
        //    }



        //    if (B.Rows != AColumns)
        //    {
        //        throw new System.ArgumentException("Matrix dimensions are not valid.");
        //    }

        //    MatrixComplex C = new MatrixComplex(ARows, B.Columns);

        //    Complex[] AData = A.Data;
        //    Complex[] BData = B.Data;
        //    Complex[] CData = C.Data;

        //    int BRows = B.Rows;
        //    int BColumns = B.Columns;


        //    Complex Sum;
        //    for (int j = 0; j < BColumns; j++)
        //    {
        //        for (int i = 0; i < ARows; i++)
        //        {
        //            Sum = new Complex(0.0, 0.0);
        //            for (int k = 0; k < AColumns; k++)
        //            {
        //                Sum += AData[i + k * ARows] * BData[k + j * BRows];
        //            }
        //            CData[i + j * ARows] = Sum;
        //        }
        //    }
        //    return C;
        //}

        #endregion


        /// <summary>
        /// Implicit Vector to Matrix conversion.
        /// </summary>
        /// <param name="V">The Vector</param>
        /// <returns>The Matrix.</returns>
        public static implicit operator ComplexMatrix(ComplexVector V)
        {
            ComplexMatrix NewMatrix;
            if (V.Type == VectorType.Column)
            {
                NewMatrix = new ComplexMatrix(V.Length, 1, V.Data); 
            }
            else
            {
                NewMatrix = new ComplexMatrix(1, V.Length, V.Data);
            }
            return NewMatrix;
        }

        #region Public Methods


        #region Add

        /// <summary>
        /// Add a complex number to all elements of this vector.
        /// </summary>
        /// <param name="c">The complex number.</param>
        /// <returns>
        /// VectorComplex r[i] = this[i] + c
        /// </returns>
        public ComplexVector Add(Complex c)
        {
            ComplexVector v = new ComplexVector(this._Type, this._Data.Length);
            Complex[] vData = v.Data;
            for (int i = 0; i < vData.Length; i++)
            {
                vData[i] = this._Data[i] + c;
            }

            return v;
        }

        /// <summary>
        /// In place add a scalar to all elements of this vector.
        /// </summary>
        /// <param name="c">The complex number.</param>
        public void AddInplace(Complex c)
        {
            for (int i = 0; i < this._Data.Length; i++)
            {
                this._Data[i] += c;
            }
        }


        /// <summary>
        /// Add a VectorComplex.
        /// </summary>
        /// <param name="B">The vector B.</param>
        /// <returns>
        /// VectorComplex r[i] = this[i] + B[i]
        /// </returns>
        public ComplexVector Add(ComplexVector B)
        {
            if (B.Type != this.Type || B.Length != this.Length)
            {
                throw new System.ArgumentException("Vector dimensions or type are not valid.");
            }

            ComplexVector r = new ComplexVector(this._Type, this.Length);
            Complex[] rData = r.Data;
            for (int i = 0; i < rData.Length; i++)
            {
                rData[i] = this._Data[i] + B[i];
            }

            return r;
        }

        /// <summary>
        /// In place add a VectorComplex.
        /// </summary>
        /// <param name="B">The vector B.</param>
        public void AddInplace(ComplexVector B)
        {
            if (B.Type != this.Type || B.Length != this.Length)
            {
                throw new System.ArgumentException("Vector dimensions or type are not valid.");
            }

            for (int i = 0; i < this._Data.Length; i++)
            {
                this._Data[i] += B[i];
            }

        }

        #endregion



        #region Subtract

        /// <summary>
        /// Subtract a scalar to all elements of this vector.
        /// </summary>
        /// <param name="c">The complex number.</param>
        /// <returns>
        /// VectorComplex r[i] = this[i] - c
        /// </returns>
        public ComplexVector Subtract(Complex c)
        {
            ComplexVector v = new ComplexVector(this._Type, this.Length);
            Complex[] vData = v.Data;
            for (int i = 0; i < vData.Length; i++)
            {
                vData[i] = this._Data[i] - c;
            }

            return v;
        }

        /// <summary>
        /// In place subtract a scalar to all elements of this vector.
        /// </summary>
        /// <param name="c">The complex number.</param>
        public void SubtractInplace(Complex c)
        {
            for (int i = 0; i < this._Data.Length; i++)
            {
                this._Data[i] -= c;
            }
        }


        /// <summary>
        /// Subtract a VectorComplex.
        /// </summary>
        /// <param name="B">The vector B.</param>
        /// <returns>
        /// VectorComplex r[i] = this[i] - B[i]
        /// </returns>
        public ComplexVector Subtract(ComplexVector B)
        {
            if (B.Type != this.Type || B.Length != this.Length)
            {
                throw new System.ArgumentException("Vector dimensions or type are not valid.");
            }

            ComplexVector r = new ComplexVector(this._Type, this.Length);
            Complex[] rData = r.Data;
            for (int i = 0; i < rData.Length; i++)
            {
                rData[i] = this._Data[i] - B[i];
            }

            return r;
        }

        /// <summary>
        /// In place add a VectorComplex.
        /// </summary>
        /// <param name="B">The vector B.</param>
        public void SubtractInplace(ComplexVector B)
        {
            if (B.Type != this.Type || B.Length != this.Length)
            {
                throw new System.ArgumentException("Vector dimensions or type are not valid.");
            }

            for (int i = 0; i < this._Data.Length; i++)
            {
                this._Data[i] -= B[i];
            }

        }

        #endregion


        #region Multiply


        /// <summary>
        /// Multiply a scalar to all elements of this vector.
        /// </summary>
        /// <param name="c">The complex number.</param>
        /// <returns>
        /// VectorComplex r[i] = this[i] * c
        /// </returns>
        public ComplexVector Multiply(Complex c)
        {
            ComplexVector v = new ComplexVector(this._Type, this.Length);
            Complex[] vData = v.Data;
            for (int i = 0; i < vData.Length; i++)
            {
                vData[i] = this._Data[i] * c;
            }

            return v;
        }


        /// <summary>
        /// In place multiply this vector with a scalar.
        /// </summary>
        /// <param name="scalar">The scalar </param>
        public void MultiplyInplace(Complex scalar)
        {
            for (int i = 0; i < this._Data.Length; i++)
            {
                this._Data[i] *= scalar;
            }
        }

        #endregion


        #region UnaryMinus

        /// <summary>
        /// Unary minus this vector.
        /// </summary>
        /// <returns>
        /// Vector r[i] = -this[i]
        /// </returns>
        public ComplexVector UnaryMinus()
        {
            ComplexVector v = new ComplexVector(this._Type, this.Length);
            Complex[] vData = v.Data;
            for (int i = 0; i < vData.Length; i++)
            {
                vData[i] -= this._Data[i];
            }

            return v;
        }

        /// <summary>
        /// In place unary minus of this vector.
        /// </summary>
        public void UnaryMinusInplace()
        {
            for (int i = 0; i < this._Data.Length; i++)
            {
                this._Data[i] = -this._Data[i];
            }
        }


        #endregion

        #region Conjugate

        /// <summary>
        /// Conjugate this vector.
        /// </summary>
        /// <returns>
        /// Vector r[i] = Real(this[i]) - Imaginary(this[i])
        /// </returns>
        public ComplexVector Conjugate()
        {
            ComplexVector conjVect= new ComplexVector(this._Data.Length);

            Complex[] v = conjVect.Data;
            for (int i = 0; i < v.Length; i++)
            {
                v[i] = this._Data[i].Conjugate;
            }

            return conjVect;
        }

        /// <summary>
        /// In place conjugation of this vector.
        /// </summary>
        public void ConjugateInplace()
        {
            for (int i = 0; i < this._Data.Length; i++)
            {
                this._Data[i] = this._Data[i].Conjugate;
            }
        }


        #endregion


        /// <summary>
        /// Calculate the 1-norm of the vector.
        /// </summary>
        /// <returns>
        /// r = sum(abs(this[i]))
        /// </returns>
        public double Norm1()
        {
            double sum = 0;
            for (int i = 0; i < this._Data.Length; i++)
            {
                sum += this._Data[i].Modulus;
            }

            return sum;
        }


        ///// <summary>
        ///// Calculate the norm of the vector
        ///// </summary>
        ///// <returns>The norm</returns>
        //public double Norm()
        //{
        //    double norm = Vector.DotProduct(this);
        //    norm = Math.Sqrt(norm);
        //    return norm;
        //}


        /// <summary>
        /// Returns the equivalent string representation of the vector.
        /// </summary>
        /// <returns>The string representation of the vector.</returns>
        public string VectorToString()
        {
            using (StringWriter writer = new StringWriter())
            {
                if (this._Type == VectorType.Column)
                {
                    for (int i = 0; i < this._Data.Length; i++)
                    {
                        writer.Write(this._Data[i]);
                        if (i < this._Data.Length - 1) writer.WriteLine();
                    }
                }
                else if (this._Type == VectorType.Row)
                {
                    for (int i = 0; i < this._Data.Length; i++)
                    {
                        if (i < this._Data.Length - 1)
                            writer.Write(this._Data[i] + ", ");
                        else
                            writer.Write(this._Data[i]);
                    }
                }
                return writer.ToString();
            }
        }


        #endregion

    }
}
