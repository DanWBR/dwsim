Namespace DWSIM.MathEx.MatrixOps

    '/*************************************************************************
    'Copyright (c) 1992-2007 The University of Tennessee.  All rights reserved.

    'Contributors:
    '    * Sergey Bochkanov (ALGLIB project). Translation from FORTRAN to
    '      pseudocode.

    'See subroutines comments for additional copyrights.

    'Redistribution and use in source and binary forms, with or without
    'modification, are permitted provided that the following conditions are
    'met:

    '- Redistributions of source code must retain the above copyright
    '  notice, this list of conditions and the following disclaimer.

    '- Redistributions in binary form must reproduce the above copyright
    '  notice, this list of conditions and the following disclaimer listed
    '  in this license in the documentation and/or other materials
    '  provided with the distribution.

    '- Neither the name of the copyright holders nor the names of its
    '  contributors may be used to endorse or promote products derived from
    '  this software without specific prior written permission.

    'THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    '"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    'LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
    'A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
    'OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
    'SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
    'LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
    'DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
    'THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
    '(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
    'OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
    '*************************************************************************/

    Public Class Determinant

        '/*************************************************************************
        'Calculation of the determinant of a general matrix

        'Input parameters:
        '    A       -   matrix, array[0..N-1, 0..N-1]
        '    N       -   size of matrix A.

        'Result: determinant of matrix A.

        '  -- ALGLIB --
        '     Copyright 2005 by Bochkanov Sergey
        '*************************************************************************/

        Public Shared Function rmatrixdet(ByVal a As Double(,), ByVal n As Integer) As Double
            Dim pivots As Integer() = New Integer(0) {}
            Dim a2 = DirectCast(a.Clone, Double(,))
            DWSIM.MathEx.SysLin.lu.rmatrixlu(a2, n, n, pivots)
            Return DWSIM.MathEx.MatrixOps.Determinant.rmatrixludet(a2, pivots, n)
        End Function

        '/*************************************************************************
        'Determinant calculation of the matrix given by its LU decomposition.

        'Input parameters:
        '    A       -   LU decomposition of the matrix (output of
        '                RMatrixLU subroutine).
        '    Pivots  -   table of permutations which were made during
        '                the LU decomposition.
        '                Output of RMatrixLU subroutine.
        '    N       -   size of matrix A.

        'Result: matrix determinant.

        '  -- ALGLIB --
        '     Copyright 2005 by Bochkanov Sergey
        '*************************************************************************/

        Public Shared Function rmatrixludet(ByRef a As Double(,), ByRef pivots As Integer(), ByVal n As Integer) As Double
            Dim num As Double = 0
            Dim index As Integer = 0
            Dim num3 As Integer = 0
            num = 1
            num3 = 1
            index = 0
            Do While (index <= (n - 1))
                num = (num * a(index, index))
                If (pivots(index) <> index) Then
                    num3 = -num3
                End If
                index += 1
            Loop
            Return (num * num3)
        End Function

        Public Shared Function determinant(ByVal a As Double(,), ByVal n As Integer) As Double
            Dim pivots As Integer() = New Integer(0) {}
            a = DirectCast(a.Clone, Double(,))
            DWSIM.MathEx.SysLin.lu.ludecomposition(a, n, n, pivots)
            Return DWSIM.MathEx.MatrixOps.Determinant.determinantlu(a, pivots, n)
        End Function

        Public Shared Function determinantlu(ByRef a As Double(,), ByRef pivots As Integer(), ByVal n As Integer) As Double
            Dim num As Double = 0
            Dim index As Integer = 0
            Dim num3 As Integer = 0
            num = 1
            num3 = 1
            index = 1
            Do While (index <= n)
                num = (num * a(index, index))
                If (pivots(index) <> index) Then
                    num3 = -num3
                End If
                index += 1
            Loop
            Return (num * num3)
        End Function

    End Class

    Public Class Inverse

        '/*************************************************************************
        'Inversion of a general matrix.

        'Input parameters:
        '    A   -   matrix. Array whose indexes range within [0..N-1, 0..N-1].
        '    N   -   size of matrix A.

        'Output parameters:
        '    A   -   inverse of matrix A.
        '            Array whose indexes range within [0..N-1, 0..N-1].

        'Result:
        '    True, if the matrix is not singular.
        '    False, if the matrix is singular.

        '  -- ALGLIB --
        '     Copyright 2005 by Bochkanov Sergey
        '*************************************************************************/
        Public Shared Function rmatrixinverse(ByRef a As Double(,), ByVal n As Integer) As Boolean
            Dim pivots As Integer() = New Integer() {}
            DWSIM.MathEx.SysLin.lu.rmatrixlu(a, n, n, pivots)
            Return rmatrixluinverse(a, pivots, n)
        End Function

        '/*************************************************************************
        'Inversion of a matrix given by its LU decomposition.

        'Input parameters:
        '    A       -   LU decomposition of the matrix (output of RMatrixLU subroutine).
        '    Pivots  -   table of permutations which were made during the LU decomposition
        '                (the output of RMatrixLU subroutine).
        '    N       -   size of matrix A.

        'Output parameters:
        '    A       -   inverse of matrix A.
        '                Array whose indexes range within [0..N-1, 0..N-1].

        'Result:
        '    True, if the matrix is not singular.
        '    False, if the matrix is singular.

        '  -- LAPACK routine (version 3.0) --
        '     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
        '     Courant Institute, Argonne National Lab, and Rice University
        '     February 29, 1992
        '*************************************************************************/

        Public Shared Function rmatrixluinverse(ByRef a As Double(,), ByRef pivots As Integer(), ByVal n As Integer) As Boolean
            Dim flag As Boolean = False
            Dim numArray As Double() = New Double() {}
            Dim index As Integer = 0
            Dim num2 As Integer = 0
            Dim num3 As Integer = 0
            Dim num4 As Double = 0
            Dim num5 As Integer = 0
            flag = True
            If (n <> 0) Then
                numArray = New Double(((n - 1) + 1)) {}
                If Not TRInverse.rmatrixtrinverse(a, n, True, False) Then
                    Return False
                End If
                num2 = (n - 1)
                Do While (num2 >= 0)
                    index = (num2 + 1)
                    Do While (index <= (n - 1))
                        numArray(index) = a(index, num2)
                        a(index, num2) = 0
                        index += 1
                    Loop
                    If (num2 < (n - 1)) Then
                        index = 0
                        Do While (index <= (n - 1))
                            num4 = 0
                            num5 = (num2 + 1)
                            Do While (num5 <= (n - 1))
                                num4 = (num4 + (a(index, num5) * numArray(num5)))
                                num5 += 1
                            Loop
                            a(index, num2) = (a(index, num2) - num4)
                            index += 1
                        Loop
                    End If
                    num2 -= 1
                Loop
                num2 = (n - 2)
                Do While (num2 >= 0)
                    num3 = pivots(num2)
                    If (num3 <> num2) Then
                        num5 = 0
                        Do While (num5 <= (n - 1))
                            numArray(num5) = a(num5, num2)
                            num5 += 1
                        Loop
                        num5 = 0
                        Do While (num5 <= (n - 1))
                            a(num5, num2) = a(num5, num3)
                            num5 += 1
                        Loop
                        num5 = 0
                        Do While (num5 <= (n - 1))
                            a(num5, num3) = numArray(num5)
                            num5 += 1
                        Loop
                    End If
                    num2 -= 1
                Loop
            End If
            Return flag
        End Function

        Public Shared Function inverse(ByRef a As Double(,), ByVal n As Integer) As Boolean
            Dim pivots As Integer() = New Integer() {}
            DWSIM.MathEx.SysLin.lu.ludecomposition(a, n, n, pivots)
            Return DWSIM.MathEx.MatrixOps.Inverse.inverselu(a, pivots, n)
        End Function

        Public Shared Function inverselu(ByRef a As Double(,), ByRef pivots As Integer(), ByVal n As Integer) As Boolean
            Dim flag As Boolean = False
            Dim numArray As Double() = New Double() {}
            Dim index As Integer = 0
            Dim num2 As Integer = 0
            Dim num3 As Integer = 0
            Dim num4 As Integer = 0
            Dim num5 As Double = 0
            Dim num6 As Integer = 0
            flag = True
            If (n <> 0) Then
                numArray = New Double((n + 1)) {}
                If Not TRInverse.invtriangular(a, n, True, False) Then
                    Return False
                End If
                num2 = n
                Do While (num2 >= 1)
                    index = (num2 + 1)
                    Do While (index <= n)
                        numArray(index) = a(index, num2)
                        a(index, num2) = 0
                        index += 1
                    Loop
                    If (num2 < n) Then
                        num4 = (num2 + 1)
                        index = 1
                        Do While (index <= n)
                            num5 = 0
                            num6 = num4
                            Do While (num6 <= n)
                                num5 = (num5 + (a(index, num6) * numArray(num6)))
                                num6 += 1
                            Loop
                            a(index, num2) = (a(index, num2) - num5)
                            index += 1
                        Loop
                    End If
                    num2 -= 1
                Loop
                num2 = (n - 1)
                Do While (num2 >= 1)
                    num3 = pivots(num2)
                    If (num3 <> num2) Then
                        num6 = 1
                        Do While (num6 <= n)
                            numArray(num6) = a(num6, num2)
                            num6 += 1
                        Loop
                        num6 = 1
                        Do While (num6 <= n)
                            a(num6, num2) = a(num6, num3)
                            num6 += 1
                        Loop
                        num6 = 1
                        Do While (num6 <= n)
                            a(num6, num3) = numArray(num6)
                            num6 += 1
                        Loop
                    End If
                    num2 -= 1
                Loop
            End If
            Return flag
        End Function

    End Class

    Public Class TRInverse

        '/*************************************************************************
        '    Triangular matrix inversion

        '    The subroutine inverts the following types of matrices:
        '        * upper triangular
        '        * upper triangular with unit diagonal
        '        * lower triangular
        '        * lower triangular with unit diagonal

        '    In case of an upper (lower) triangular matrix,  the  inverse  matrix  will
        '    also be upper (lower) triangular, and after the end of the algorithm,  the
        '    inverse matrix replaces the source matrix. The elements  below (above) the
        '    main diagonal are not changed by the algorithm.

        '    If  the matrix  has a unit diagonal, the inverse matrix also  has  a  unit
        '    diagonal, and the diagonal elements are not passed to the algorithm.

        '    Input parameters:
        '        A       -   matrix.
        '                    Array whose indexes range within [0..N-1, 0..N-1].
        '        N       -   size of matrix A.
        '        IsUpper -   True, if the matrix is upper triangular.
        '        IsUnitTriangular
        '                -   True, if the matrix has a unit diagonal.

        '    Output parameters:
        '        A       -   inverse matrix (if the problem is not degenerate).

        '    Result:
        '        True, if the matrix is not singular.
        '        False, if the matrix is singular.

        '      -- LAPACK routine (version 3.0) --
        '         Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
        '         Courant Institute, Argonne National Lab, and Rice University
        '         February 29, 1992
        '    *************************************************************************/

        Public Shared Function rmatrixtrinverse(ByRef a As Double(,), ByVal n As Integer, ByVal isupper As Boolean, ByVal isunittriangular As Boolean) As Boolean
            Dim flag As Boolean = False
            Dim flag2 As Boolean = False
            Dim index As Integer = 0
            Dim num2 As Integer = 0
            Dim num3 As Double = 0
            Dim num4 As Double = 0
            Dim numArray As Double() = New Double() {}
            Dim num5 As Integer = 0
            flag = True
            numArray = New Double(((n - 1) + 1)) {}
            flag2 = Not isunittriangular
            If isupper Then
                num2 = 0
                Do While (num2 <= (n - 1))
                    If flag2 Then
                        If (a(num2, num2) = 0) Then
                            Return False
                        End If
                        a(num2, num2) = (1 / a(num2, num2))
                        num4 = -a(num2, num2)
                    Else
                        num4 = -1
                    End If
                    If (num2 > 0) Then
                        num5 = 0
                        Do While (num5 <= (num2 - 1))
                            numArray(num5) = a(num5, num2)
                            num5 += 1
                        Loop
                        index = 0
                        Do While (index <= (num2 - 1))
                            If (index < (num2 - 1)) Then
                                num3 = 0
                                num5 = (index + 1)
                                Do While (num5 <= (num2 - 1))
                                    num3 = (num3 + (a(index, num5) * numArray(num5)))
                                    num5 += 1
                                Loop
                            Else
                                num3 = 0
                            End If
                            If flag2 Then
                                a(index, num2) = (num3 + (a(index, index) * numArray(index)))
                            Else
                                a(index, num2) = (num3 + numArray(index))
                            End If
                            index += 1
                        Loop
                        num5 = 0
                        Do While (num5 <= (num2 - 1))
                            a(num5, num2) = (num4 * a(num5, num2))
                            num5 += 1
                        Loop
                    End If
                    num2 += 1
                Loop
                Return flag
            End If
            num2 = (n - 1)
            Do While (num2 >= 0)
                If flag2 Then
                    If (a(num2, num2) = 0) Then
                        Return False
                    End If
                    a(num2, num2) = (1 / a(num2, num2))
                    num4 = -a(num2, num2)
                Else
                    num4 = -1
                End If
                If (num2 < (n - 1)) Then
                    num5 = (num2 + 1)
                    Do While (num5 <= (n - 1))
                        numArray(num5) = a(num5, num2)
                        num5 += 1
                    Loop
                    index = (num2 + 1)
                    Do While (index <= (n - 1))
                        If (index > (num2 + 1)) Then
                            num3 = 0
                            num5 = (num2 + 1)
                            Do While (num5 <= (index - 1))
                                num3 = (num3 + (a(index, num5) * numArray(num5)))
                                num5 += 1
                            Loop
                        Else
                            num3 = 0
                        End If
                        If flag2 Then
                            a(index, num2) = (num3 + (a(index, index) * numArray(index)))
                        Else
                            a(index, num2) = (num3 + numArray(index))
                        End If
                        index += 1
                    Loop
                    num5 = (num2 + 1)
                    Do While (num5 <= (n - 1))
                        a(num5, num2) = (num4 * a(num5, num2))
                        num5 += 1
                    Loop
                End If
                num2 -= 1
            Loop
            Return flag
        End Function

        Public Shared Function invtriangular(ByRef a As Double(,), ByVal n As Integer, ByVal isupper As Boolean, ByVal isunittriangular As Boolean) As Boolean
            Dim flag As Boolean = False
            Dim flag2 As Boolean = False
            Dim index As Integer = 0
            Dim num2 As Integer = 0
            Dim num3 As Integer = 0
            Dim num4 As Integer = 0
            Dim num5 As Double = 0
            Dim num6 As Double = 0
            Dim numArray As Double() = New Double() {}
            Dim num7 As Integer = 0
            flag = True
            numArray = New Double((n + 1)) {}
            flag2 = Not isunittriangular
            If isupper Then
                num2 = 1
                Do While (num2 <= n)
                    If flag2 Then
                        If (a(num2, num2) = 0) Then
                            Return False
                        End If
                        a(num2, num2) = (1 / a(num2, num2))
                        num6 = -a(num2, num2)
                    Else
                        num6 = -1
                    End If
                    If (num2 > 1) Then
                        num3 = (num2 - 1)
                        num7 = 1
                        Do While (num7 <= num3)
                            numArray(num7) = a(num7, num2)
                            num7 += 1
                        Loop
                        index = 1
                        Do While (index <= (num2 - 1))
                            If (index < (num2 - 1)) Then
                                num5 = 0
                                num7 = (index + 1)
                                Do While (num7 <= num3)
                                    num5 = (num5 + (a(index, num7) * numArray(num7)))
                                    num7 += 1
                                Loop
                            Else
                                num5 = 0
                            End If
                            If flag2 Then
                                a(index, num2) = (num5 + (a(index, index) * numArray(index)))
                            Else
                                a(index, num2) = (num5 + numArray(index))
                            End If
                            index += 1
                        Loop
                        num7 = 1
                        Do While (num7 <= num3)
                            a(num7, num2) = (num6 * a(num7, num2))
                            num7 += 1
                        Loop
                    End If
                    num2 += 1
                Loop
                Return flag
            End If
            num2 = n
            Do While (num2 >= 1)
                If flag2 Then
                    If (a(num2, num2) = 0) Then
                        Return False
                    End If
                    a(num2, num2) = (1 / a(num2, num2))
                    num6 = -a(num2, num2)
                Else
                    num6 = -1
                End If
                If (num2 < n) Then
                    num4 = (num2 + 1)
                    num7 = num4
                    Do While (num7 <= n)
                        numArray(num7) = a(num7, num2)
                        num7 += 1
                    Loop
                    index = (num2 + 1)
                    Do While (index <= n)
                        If (index > (num2 + 1)) Then
                            num5 = 0
                            num7 = num4
                            Do While (num7 <= (index - 1))
                                num5 = (num5 + (a(index, num7) * numArray(num7)))
                                num7 += 1
                            Loop
                        Else
                            num5 = 0
                        End If
                        If flag2 Then
                            a(index, num2) = (num5 + (a(index, index) * numArray(index)))
                        Else
                            a(index, num2) = (num5 + numArray(index))
                        End If
                        index += 1
                    Loop
                    num7 = num4
                    Do While (num7 <= n)
                        a(num7, num2) = (num6 * a(num7, num2))
                        num7 += 1
                    Loop
                End If
                num2 -= 1
            Loop
            Return flag
        End Function

    End Class

End Namespace