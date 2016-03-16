Option Strict Off
Option Explicit On 

Imports System.Math

Namespace DWSIM.MathEx.MatrixOps

    Public Class MatLib
        Private Shared Sub Find_R_C(ByVal Mat(,) As Double, ByRef Row As Integer, ByRef Col As Integer)
            Row = Mat.GetUpperBound(0)
            Col = Mat.GetUpperBound(1)
        End Sub

#Region "Add Matrices"
        ''''''''''''''''''''''''''''''''''''''''''''''''''''''''
        ' Add two matrices, their dimensions should be compatible!
        ' Function returns the summation or errors due to
        ' dimensions incompatibility
        ' Example:
        '  Check Main Form !!
        ''''''''''''''''''''''''''''''''''''''''''''''''''''''''
        Public Shared Function Add(ByVal Mat1(,) As Double, ByVal Mat2(,) As Double) As Double(,)
            Dim sol(,) As Double
            Dim i, j As Integer
            Dim Rows1, Cols1 As Integer
            Dim Rows2, Cols2 As Integer

            On Error GoTo Error_Handler

            Find_R_C(Mat1, Rows1, Cols1)
            Find_R_C(Mat2, Rows2, Cols2)

            If Rows1 <> Rows2 Or Cols1 <> Cols2 Then
                GoTo Error_Dimension
            End If

            ReDim sol(Rows1, Cols1)
            For i = 0 To Rows1
                For j = 0 To Cols1
                    sol(i, j) = Mat1(i, j) + Mat2(i, j)
                Next j
            Next i

            Return sol

Error_Dimension:
            Err.Raise("5005", , "Dimensions of the two matrices do not match !")

Error_Handler:
            If Err.Number = 5005 Then
                Err.Raise("5005", , "Dimensions of the two matrices do not match !")
            Else
                Err.Raise("5022", , "One or both of the matrices are null, this operation cannot be done !!")
            End If

            Return Nothing

        End Function
#End Region

#Region "Subtract Matrices"
        ''''''''''''''''''''''''''''''''''''''''''''''''''''''''
        ' Subtracts two matrices from each other, their
        ' dimensions should be compatible!
        ' Function returns the solution or errors due to
        ' dimensions incompatibility
        ' Example:
        '  Check Main Form !!
        ''''''''''''''''''''''''''''''''''''''''''''''''''''''''
        Public Shared Function Subtract(ByVal Mat1(,) As Double, ByVal Mat2(,) As Double) As Double(,)
            Dim i, j As Integer
            Dim sol(,) As Double
            Dim Rows1, Cols1 As Integer
            Dim Rows2, Cols2 As Integer

            On Error GoTo Error_Handler

            Find_R_C(Mat1, Rows1, Cols1)
            Find_R_C(Mat2, Rows2, Cols2)

            If Rows1 <> Rows2 Or Cols1 <> Cols2 Then
                GoTo Error_Dimension
            End If

            ReDim sol(Rows1, Cols1)

            For i = 0 To Rows1
                For j = 0 To Cols1
                    sol(i, j) = Mat1(i, j) - Mat2(i, j)
                Next j
            Next i

            Return sol

Error_Dimension:
            Err.Raise("5007", , "Dimensions of the two matrices do not match !")

Error_Handler:
            If Err.Number = 5007 Then
                Err.Raise("5007", , "Dimensions of the two matrices do not match !")
            Else
                Err.Raise("5022", , "One or both of the matrices are null, this operation cannot be done !!")
            End If
            Return Nothing

        End Function

#End Region

#Region "Multiply Matrices"
        '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
        ' Multiply two matrices, their dimensions should be compatible!
        ' Function returns the solution or errors due to
        ' dimensions incompatibility
        ' Example:
        '  Check Main Form !!
        '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
        Public Shared Function Multiply(ByVal Mat1(,) As Double, ByVal Mat2(,) As Double) As Double(,)
            Dim l, i, j As Integer
            Dim OptiString As String
            Dim sol(,) As Double, MulAdd As Double
            Dim Rows1, Cols1 As Integer
            Dim Rows2, Cols2 As Integer

            On Error GoTo Error_Handler

            MulAdd = 0

            Find_R_C(Mat1, Rows1, Cols1)
            Find_R_C(Mat2, Rows2, Cols2)

            If Cols1 <> Rows2 Then
                GoTo Error_Dimension
            End If

            ReDim sol(Rows1, Cols2)

            For i = 0 To Rows1
                For j = 0 To Cols2
                    For l = 0 To Cols1
                        MulAdd = MulAdd + Mat1(i, l) * Mat2(l, j)
                    Next l
                    sol(i, j) = MulAdd
                    MulAdd = 0
                Next j
            Next i

            Return sol

Error_Dimension:
            Err.Raise("5009", , "Dimensions of the two matrices not suitable for multiplication !")

Error_Handler:
            If Err.Number = 5009 Then
                Err.Raise("5009", , "Dimensions of the two matrices not suitable for multiplication !")
            Else
                Err.Raise("5022", , "One or both of the matrices are null, this operation cannot be done !!")
            End If
            Return Nothing

        End Function

#End Region

#Region "Determinant of a Matrix"
        '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
        ' Determinant of a matrix should be (nxn)
        ' Function returns the solution or errors due to
        ' dimensions incompatibility
        ' Example:
        '  Check Main Form !!
        '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
        Public Shared Function Det(ByVal Mat(,) As Double) As Double
            Dim DArray(,) As Double, S As Integer
            Dim k, k1, i, j As Integer
            Dim save, ArrayK As Double
            Dim M1 As String
            Dim Rows, Cols As Integer

            On Error GoTo Error_Handler

            Find_R_C(Mat, Rows, Cols)

            If Rows <> Cols Then GoTo Error_Dimension

            S = Rows
            Det = 1
            DArray = Mat.Clone()

            For k = 0 To S
                If DArray(k, k) = 0 Then
                    j = k
                    Do While ((j < S) And (DArray(k, j) = 0))
                        j = j + 1
                    Loop
                    If DArray(k, j) = 0 Then
                        Det = 0
                        Exit Function
                    Else
                        For i = k To S
                            save = DArray(i, j)
                            DArray(i, j) = DArray(i, k)
                            DArray(i, k) = save
                        Next i
                    End If

                    Det = -Det
                End If
                ArrayK = DArray(k, k)
                Det = Det * ArrayK
                If k < S Then
                    k1 = k + 1
                    For i = k1 To S
                        For j = k1 To S
                            DArray(i, j) = DArray(i, j) - DArray(i, k) * (DArray(k, j) / ArrayK)
                        Next j
                    Next i
                End If
            Next

            Exit Function

Error_Dimension:
            Err.Raise("5011", , "Matrix should be a square matrix !")

Error_Handler:
            If Err.Number = 5011 Then
                Err.Raise("5011", , "Matrix should be a square matrix !")
            Else
                Err.Raise("5022", , "In order to do this operation values must be assigned to the matrix !!")
            End If
        End Function

#End Region

#Region "Inverse of a Matrix"
        '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
        ' Inverse of a matrix, should be (nxn) and det(Mat)<>0
        ' Function returns the solution or errors due to
        ' dimensions incompatibility
        ' Example:
        '  Check Main Form !!
        '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
        Public Shared Function Inv(ByVal Mat(,) As Double) As Double(,)
            Dim AI(,) As Double, AIN As Double, AF As Double, _
                Mat1(,) As Double
            Dim LL As Integer, LLM As Integer, L1 As Integer, _
                L2 As Integer, LC As Integer, LCA As Integer, _
                LCB As Integer, i As Integer, j As Integer
            Dim Rows, Cols As Integer

            On Error GoTo Error_Handler

            Find_R_C(Mat, Rows, Cols)
            If Rows <> Cols Then GoTo Error_Dimension

            If Det(Mat) = 0 Then GoTo Error_Zero

            LL = Rows
            LLM = Cols
            Mat1 = Mat.Clone()
            ReDim AI(LL, LL)

            For L2 = 0 To LL
                For L1 = 0 To LL
                    AI(L1, L2) = 0
                Next
                AI(L2, L2) = 1
            Next

            For LC = 0 To LL
                If Abs(Mat1(LC, LC)) < 0.0000000001 Then
                    For LCA = LC + 1 To LL
                        If LCA = LC Then GoTo 1090
                        If Abs(Mat1(LC, LCA)) > 0.0000000001 Then
                            For LCB = 0 To LL
                                Mat1(LCB, LC) = Mat1(LCB, LC) + Mat1(LCB, LCA)
                                AI(LCB, LC) = AI(LCB, LC) + AI(LCB, LCA)
                            Next
                            GoTo 1100
                        End If
1090:               Next
                End If

1100:
                AIN = 1 / Mat1(LC, LC)
                For LCA = 0 To LL
                    Mat1(LCA, LC) = AIN * Mat1(LCA, LC)
                    AI(LCA, LC) = AIN * AI(LCA, LC)
                Next

                For LCA = 0 To LL
                    If LCA = LC Then GoTo 1150
                    AF = Mat1(LC, LCA)
                    For LCB = 0 To LL
                        Mat1(LCB, LCA) = Mat1(LCB, LCA) - AF * Mat1(LCB, LC)
                        AI(LCB, LCA) = AI(LCB, LCA) - AF * AI(LCB, LC)
                    Next
1150:           Next

            Next

            Return AI

Error_Zero:
            Err.Raise("5012", , "Determinent equals zero, inverse can't be found !")

Error_Dimension:
            Err.Raise("5014", , "Matrix should be a square matrix !")

Error_Handler:
            If Err.Number = 5012 Then
                Err.Raise("5012", , "Determinent equals zero, inverse can't be found !")
            ElseIf Err.Number = 5014 Then
                Err.Raise("5014", , "Matrix should be a square matrix !")
            End If

            Return Nothing

        End Function

#End Region

#Region "Multiply Vectors"
        '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
        ' Multiply two vectors, dimensions should be (3x1)
        ' Function returns the solution or errors due to
        ' dimensions incompatibility
        ' Example:
        '  Check Main Form !!
        '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
        Public Shared Function MultiplyVectors(ByVal Mat1(,) As Double, ByVal Mat2(,) As Double) As Double(,)
            Dim i, j, k As Double
            Dim sol(2, 0) As Double
            Dim Rows1, Cols1 As Integer
            Dim Rows2, Cols2 As Integer

            On Error GoTo Error_Handler

            Find_R_C(Mat1, Rows1, Cols1)
            Find_R_C(Mat2, Rows2, Cols2)

            If Rows1 <> 2 Or Cols1 <> 0 Then
                GoTo Error_Dimension
            End If

            If Rows2 <> 2 Or Cols2 <> 0 Then
                GoTo Error_Dimension
            End If

            i = Mat1(1, 0) * Mat2(2, 0) - Mat1(2, 0) * Mat2(1, 0)
            j = Mat1(2, 0) * Mat2(0, 0) - Mat1(0, 0) * Mat2(2, 0)
            k = Mat1(0, 0) * Mat2(1, 0) - Mat1(1, 0) * Mat2(0, 0)

            sol(0, 0) = i : sol(1, 0) = j : sol(2, 0) = k

            Return sol

Error_Dimension:
            Err.Raise("5016", , "Dimension should be (2 x 0) for both matrices in order to do cross multiplication !")

Error_Handler:

            If Err.Number = 5016 Then
                Err.Raise("5016", , "Dimension should be (2 x 0) for both matrices in order to do cross multiplication !")
            Else
                Err.Raise("5022", , "One or both of the matrices are null, this operation cannot be done !!")
            End If

            Return Nothing

        End Function

#End Region

#Region "Magnitude of a Vector"

        '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
        ' Magnitude of a Vector, vector should be (3x1)
        ' Function returns the solution or errors due to
        ' dimensions incompatibility
        ' Example:
        '  Check Main Form !!
        '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
        Public Shared Function VectorMagnitude(ByVal Mat(,) As Double) As Double

            Dim Rows, Cols As Integer

            On Error GoTo Error_Handler

            Find_R_C(Mat, Rows, Cols)

            If Rows <> 2 Or Cols <> 0 Then
                GoTo Error_Dimension
            End If

            Return Sqrt(Mat(0, 0) * Mat(0, 0) + Mat(1, 0) * Mat(1, 0) + Mat(2, 0) * Mat(2, 0))

Error_Dimension:
            Err.Raise("5018", , "Dimension of the matrix should be (2 x 0) in order to find the vector's norm !")

Error_Handler:
            If Err.Number = 5018 Then
                Err.Raise("5018", , "Dimension of the matrix should be (2 x 0) in order to find the vector's magnitude !")
            Else
                Err.Raise("5022", , "In order to do this operation values must be assigned to the matrix !!")
            End If

        End Function
#End Region

#Region "Transpose of a Matrix"
        '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
        ' Transpose of a matrix
        ' Function returns the solution or errors
        ' Example:
        '  Check Main Form !!
        '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
        Public Shared Function Transpose(ByVal Mat(,) As Double) As Double(,)
            Dim Tr_Mat(,) As Double
            Dim i, j, Rows, Cols As Integer

            On Error GoTo Error_Handler

            Find_R_C(Mat, Rows, Cols)

            ReDim Tr_Mat(Cols, Rows)

            For i = 0 To Cols
                For j = 0 To Rows
                    Tr_Mat(j, i) = Mat(i, j)
                Next j
            Next i

            Return Tr_Mat

Error_Handler:
            Err.Raise("5028", , "In order to do this operation values must be assigned to the matrix !!")

        End Function
#End Region

#Region "Multiply a matrix or a vector with a scalar quantity"

        '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
        ' Multiply a matrix or a vector with a scalar quantity
        ' Function returns the solution or errors
        ' Example:
        '  Check Main Form !!
        '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
        Public Shared Function ScalarMultiply(ByVal Value As Double, ByVal Mat(,) As Double) As Double(,)
            Dim i, j, Rows, Cols As Integer
            Dim sol(,) As Double

            On Error GoTo Error_Handler

            Find_R_C(Mat, Rows, Cols)
            ReDim sol(Rows, Cols)

            For i = 0 To Rows
                For j = 0 To Cols
                    sol(i, j) = Mat(i, j) * Value
                Next j
            Next i

            Return (sol)

Error_Handler:
            Err.Raise("5022", , "Matrix was not assigned")
        End Function

#End Region

#Region "Divide a matrix or a vector with a scalar quantity"
        '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
        ' Divide matrix elements or a vector by a scalar quantity
        ' Function returns the solution or errors
        ' Example:
        '  Check Main Form !!
        '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
        Public Shared Function ScalarDivide(ByVal Value As Double, ByVal Mat(,) As Double) As Double(,)
            Dim i, j, Rows, Cols As Integer
            Dim sol(,) As Double

            On Error GoTo Error_Handler

            Find_R_C(Mat, Rows, Cols)
            ReDim sol(Rows, Cols)

            For i = 0 To Rows
                For j = 0 To Cols
                    sol(i, j) = Mat(i, j) / Value
                Next j
            Next i

            Return sol

            Exit Function

Error_Handler:
            Err.Raise("5022", , "Matrix was not assigned")
        End Function

#End Region


#Region "Print Matrix"

        '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
        ' Print a matrix to multitext text box
        ' Function returns the solution or errors
        ' Example:
        '  Check Main Form !!
        '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
        Public Shared Function PrintMat(ByVal Mat(,) As Double) As String
            Dim N_Rows As Integer, N_Columns, k As Integer, _
                i As Integer, j As Integer, m As Integer
            Dim StrElem As String, StrLen As Long, _
                Greatest() As Integer, LarString As String = ""
            Dim OptiString As String, sol As String

            Find_R_C(Mat, N_Rows, N_Columns)

            sol = ""
            OptiString = ""

            ReDim Greatest(N_Columns)

            For i = 0 To N_Rows
                For j = 0 To N_Columns
                    If i = 0 Then
                        Greatest(j) = 0
                        For m = 0 To N_Rows
                            StrElem = Format$(Mat(m, j), "0.0000")
                            StrLen = Len(StrElem)
                            If Greatest(j) < StrLen Then
                                Greatest(j) = StrLen
                                LarString = StrElem
                            End If
                        Next m
                        If Mid$(LarString, 1, 1) = "-" Then Greatest(j) = Greatest(j) + 1
                    End If
                    StrElem = Format$(Mat(i, j), "0.0000")
                    If Mid$(StrElem, 1, 1) = "-" Then
                        StrLen = Len(StrElem)
                        If Greatest(j) >= StrLen Then
                            For k = 1 To (Greatest(j) - StrLen)
                                OptiString = OptiString & "  "
                            Next k
                            OptiString = OptiString & " "
                        End If
                    Else
                        StrLen = Len(StrElem)
                        If Greatest(j) > StrLen Then
                            For k = 1 To (Greatest(j) - StrLen)
                                OptiString = OptiString & "  "
                            Next k
                        End If
                    End If
                    OptiString = OptiString & "  " & Format$(Mat(i, j), "0.0000")
                Next j
                If i <> N_Rows Then
                    sol = sol & OptiString & vbCrLf
                    OptiString = ""
                End If
                sol = sol & OptiString
                OptiString = ""
            Next i

            PrintMat = sol

            Exit Function
        End Function
#End Region

    End Class

End Namespace
