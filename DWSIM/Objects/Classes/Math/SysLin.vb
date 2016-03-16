Namespace DWSIM.MathEx.SysLin

    Public Class rsolve

        '/*************************************************************************
        '    Solving a system of linear equations with a system matrix given by its
        '    LU decomposition.

        '    The algorithm solves a system of linear equations whose matrix is given by
        '    its LU decomposition. In case of a singular matrix, the algorithm  returns
        '    False.

        '    The algorithm solves systems with a square matrix only.

        '    Input parameters:
        '        A       -   LU decomposition of a system matrix in compact  form  (the
        '                    result of the RMatrixLU subroutine).
        '        Pivots  -   row permutation table (the result of a
        '                    RMatrixLU subroutine).
        '        B       -   right side of a system.
        '                    Array whose index ranges within [0..N-1].
        '        N       -   size of matrix A.

        '    Output parameters:
        '        X       -   solution of a system.
        '                    Array whose index ranges within [0..N-1].

        '    Result:
        '        True, if the matrix is not singular.
        '        False, if the matrux is singular. In this case, X doesn't contain a
        '    solution.

        '      -- ALGLIB --
        '         Copyright 2005-2008 by Bochkanov Sergey
        '*************************************************************************/

        ' Methods

        Public Shared Function rmatrixlusolve(ByRef a As Double(,), ByRef pivots As Integer(), ByVal b As Double(), ByVal n As Integer, ByRef x As Double()) As Boolean
            Dim result As Boolean = False
            Dim y As Double() = New Double(0) {}
            Dim i As Integer = 0
            Dim v As Double = 0
            Dim i_ As Integer = 0
            b = DirectCast(b.Clone, Double())
            y = New Double(((n - 1) + 1)) {}
            x = New Double(((n - 1) + 1)) {}
            result = True
            i = 0
            Do While (i <= (n - 1))
                If (a(i, i) = 0) Then
                    Return False
                End If
                i += 1
            Loop
            i = 0
            Do While (i <= (n - 1))
                If (pivots(i) <> i) Then
                    v = b(i)
                    b(i) = b(pivots(i))
                    b(pivots(i)) = v
                End If
                i += 1
            Loop
            y(0) = b(0)
            i = 1
            Do While (i <= (n - 1))
                v = 0
                i_ = 0
                Do While (i_ <= (i - 1))
                    v = (v + (a(i, i_) * y(i_)))
                    i_ += 1
                Loop
                y(i) = (b(i) - v)
                i += 1
            Loop
            x((n - 1)) = (y((n - 1)) / a((n - 1), (n - 1)))
            i = (n - 2)
            Do While (i >= 0)
                v = 0
                i_ = (i + 1)
                Do While (i_ <= (n - 1))
                    v = (v + (a(i, i_) * x(i_)))
                    i_ += 1
                Loop
                x(i) = ((y(i) - v) / a(i, i))
                i -= 1
            Loop
            Return result
        End Function

        '/*************************************************************************
        '   Solving a system of linear equations.

        '   The algorithm solves a system of linear equations by using the
        '   LU decomposition. The algorithm solves systems with a square matrix only.

        '   Input parameters:
        '       A   -   system matrix.
        '               Array whose indexes range within [0..N-1, 0..N-1].
        '       B   -   right side of a system.
        '               Array whose indexes range within [0..N-1].
        '       N   -   size of matrix A.

        '   Output parameters:
        '       X   -   solution of a system.
        '               Array whose index ranges within [0..N-1].

        '   Result:
        '       True, if the matrix is not singular.
        '       False, if the matrix is singular. In this case, X doesn't contain a
        '   solution.

        '   -- ALGLIB --
        '   Copyright 2005-2008 by Bochkanov Sergey
        '*************************************************************************/

        Public Shared Function rmatrixsolve(ByVal a As Double(,), ByVal b As Double(), ByVal n As Integer, ByRef x As Double()) As Boolean
            Dim pivots As Integer() = New Integer(0) {}
            a = DirectCast(a.Clone, Double(,))
            b = DirectCast(b.Clone, Double())
            lu.rmatrixlu(a, n, n, pivots)
            Return rsolve.rmatrixlusolve(a, pivots, b, n, x)
        End Function

        Public Shared Function solvesystem(ByVal a As Double(,), ByVal b As Double(), ByVal n As Integer, ByRef x As Double()) As Boolean
            Dim pivots As Integer() = New Integer(0) {}
            a = DirectCast(a.Clone, Double(,))
            b = DirectCast(b.Clone, Double())
            lu.ludecomposition(a, n, n, pivots)
            Return rsolve.solvesystemlu(a, pivots, b, n, x)
        End Function

        Public Shared Function solvesystemlu(ByRef a As Double(,), ByRef pivots As Integer(), ByVal b As Double(), ByVal n As Integer, ByRef x As Double()) As Boolean
            Dim result As Boolean = False
            Dim y As Double() = New Double(0) {}
            Dim i As Integer = 0
            Dim v As Double = 0
            Dim ip1 As Integer = 0
            Dim im1 As Integer = 0
            Dim i_ As Integer = 0
            b = DirectCast(b.Clone, Double())
            y = New Double(n + 1) {}
            x = New Double(n + 1) {}
            result = True
            i = 1
            Do While (i <= n)
                If (a(i, i) = 0) Then
                    Return False
                End If
                i += 1
            Loop
            i = 1
            Do While (i <= n)
                If (pivots(i) <> i) Then
                    v = b(i)
                    b(i) = b(pivots(i))
                    b(pivots(i)) = v
                End If
                i += 1
            Loop
            y(1) = b(1)
            i = 2
            Do While (i <= n)
                im1 = (i - 1)
                v = 0
                i_ = 1
                Do While (i_ <= im1)
                    v = (v + (a(i, i_) * y(i_)))
                    i_ += 1
                Loop
                y(i) = (b(i) - v)
                i += 1
            Loop
            x(n) = (y(n) / a(n, n))
            i = (n - 1)
            Do While (i >= 1)
                ip1 = (i + 1)
                v = 0
                i_ = ip1
                Do While (i_ <= n)
                    v = (v + (a(i, i_) * x(i_)))
                    i_ += 1
                Loop
                x(i) = ((y(i) - v) / a(i, i))
                i -= 1
            Loop
            Return result
        End Function


    End Class

    Public Class lu

        '/*************************************************************************
        '    LU decomposition of a general matrix of size MxN

        '    The subroutine calculates the LU decomposition of a rectangular general
        '    matrix with partial pivoting (with row permutations).

        '    Input parameters:
        '        A   -   matrix A whose indexes range within [0..M-1, 0..N-1].
        '        M   -   number of rows in matrix A.
        '        N   -   number of columns in matrix A.

        '    Output parameters:
        '        A   -   matrices L and U in compact form (see below).
        '                Array whose indexes range within [0..M-1, 0..N-1].
        '        Pivots - permutation matrix in compact form (see below).
        '                Array whose index ranges within [0..Min(M-1,N-1)].

        '    Matrix A is represented as A = P * L * U, where P is a permutation matrix,
        '    matrix L - lower triangular (or lower trapezoid, if M>N) matrix,
        '    U - upper triangular (or upper trapezoid, if M<N) matrix.

        '    Let M be equal to 4 and N be equal to 3:

        '                       (  1          )    ( U11 U12 U13  )
        '    A = P1 * P2 * P3 * ( L21  1      )  * (     U22 U23  )
        '                       ( L31 L32  1  )    (         U33  )
        '                       ( L41 L42 L43 )

        '    Matrix L has size MxMin(M,N), matrix U has size Min(M,N)xN, matrix P(i) is
        '    a permutation of the identity matrix of size MxM with numbers I and Pivots[I].

        '    The algorithm returns array Pivots and the following matrix which replaces
        '    matrix A and contains matrices L and U in compact form (the example applies
        '    to M=4, N=3).

        '     ( U11 U12 U13 )
        '     ( L21 U22 U23 )
        '     ( L31 L32 U33 )
        '     ( L41 L42 L43 )

        '    As we can see, the unit diagonal isn't stored.

        '      -- LAPACK routine (version 3.0) --
        '         Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
        '         Courant Institute, Argonne National Lab, and Rice University
        '         June 30, 1992
        '    *************************************************************************/

        ' Methods

        Public Shared Sub ludecomposition(ByRef a As Double(,), ByVal m As Integer, ByVal n As Integer, ByRef pivots As Integer())
            Dim i As Integer = 0
            Dim j As Integer = 0
            Dim jp As Integer = 0
            Dim t1 As Double() = New Double(0) {}
            Dim s As Double = 0
            Dim i_ As Integer = 0
            pivots = New Integer((Math.Min(m, n) + 1)) {}
            t1 = New Double((Math.Max(m, n) + 1)) {}
            If Not ((m = 0) Or (n = 0)) Then
                j = 1
                Do While (j <= Math.Min(m, n))
                    jp = j
                    i = (j + 1)
                    Do While (i <= m)
                        If (Math.Abs(a(i, j)) > Math.Abs(a(jp, j))) Then
                            jp = i
                        End If
                        i += 1
                    Loop
                    pivots(j) = jp
                    If (a(jp, j) <> 0) Then
                        If (jp <> j) Then
                            i_ = 1
                            Do While (i_ <= n)
                                t1(i_) = a(j, i_)
                                i_ += 1
                            Loop
                            i_ = 1
                            Do While (i_ <= n)
                                a(j, i_) = a(jp, i_)
                                i_ += 1
                            Loop
                            i_ = 1
                            Do While (i_ <= n)
                                a(jp, i_) = t1(i_)
                                i_ += 1
                            Loop
                        End If
                        If (j < m) Then
                            jp = (j + 1)
                            s = (1 / a(j, j))
                            i_ = jp
                            Do While (i_ <= m)
                                a(i_, j) = (s * a(i_, j))
                                i_ += 1
                            Loop
                        End If
                    End If
                    If (j < Math.Min(m, n)) Then
                        jp = (j + 1)
                        i = (j + 1)
                        Do While (i <= m)
                            s = a(i, j)
                            i_ = jp
                            Do While (i_ <= n)
                                a(i, i_) = (a(i, i_) - (s * a(j, i_)))
                                i_ += 1
                            Loop
                            i += 1
                        Loop
                    End If
                    j += 1
                Loop
            End If
        End Sub

        Public Shared Sub ludecompositionunpacked(ByVal a As Double(,), ByVal m As Integer, ByVal n As Integer, ByRef l As Double(,), ByRef u As Double(,), ByRef pivots As Integer())
            Dim i As Integer = 0
            Dim j As Integer = 0
            Dim minmn As Integer = 0
            a = DirectCast(a.Clone, Double(,))
            If Not ((m = 0) Or (n = 0)) Then
                minmn = Math.Min(m, n)
                l = New Double((m + 1), (minmn + 1)) {}
                u = New Double((minmn + 1), (n + 1)) {}
                lu.ludecomposition(a, m, n, pivots)
                i = 1
                Do While (i <= m)
                    j = 1
                    Do While (j <= minmn)
                        If (j > i) Then
                            l(i, j) = 0
                        End If
                        If (j = i) Then
                            l(i, j) = 1
                        End If
                        If (j < i) Then
                            l(i, j) = a(i, j)
                        End If
                        j += 1
                    Loop
                    i += 1
                Loop
                i = 1
                Do While (i <= minmn)
                    j = 1
                    Do While (j <= n)
                        If (j < i) Then
                            u(i, j) = 0
                        End If
                        If (j >= i) Then
                            u(i, j) = a(i, j)
                        End If
                        j += 1
                    Loop
                    i += 1
                Loop
            End If
        End Sub

        Public Shared Sub rmatrixlu(ByRef a As Double(,), ByVal m As Integer, ByVal n As Integer, ByRef pivots As Integer())
            Dim b(,) As Double = New Double(0, 0) {}
            Dim t As Double() = New Double(0) {}
            Dim bp As Integer() = New Integer(0) {}
            Dim minmn As Integer = 0
            Dim i As Integer = 0
            Dim ip As Integer = 0
            Dim j As Integer = 0
            Dim j1 As Integer = 0
            Dim j2 As Integer = 0
            Dim cb As Integer = 0
            Dim nb As Integer = 0
            Dim v As Double = 0
            Dim i_ As Integer = 0
            Dim i1_ As Integer = 0
            nb = 8
            If (((n <= 1) Or (Math.Min(m, n) <= nb)) Or (nb = 1)) Then
                lu.rmatrixlu2(a, m, n, pivots)
            Else
                b = New Double(((m - 1) + 1), ((nb - 1) + 1)) {}
                t = New Double(((n - 1) + 1)) {}
                pivots = New Integer(((Math.Min(m, n) - 1) + 1)) {}
                minmn = Math.Min(m, n)
                j1 = 0
                j2 = (Math.Min(minmn, nb) - 1)
                Do While (j1 < minmn)
                    cb = ((j2 - j1) + 1)
                    i = j1
                    Do While (i <= (m - 1))
                        i1_ = j1
                        i_ = 0
                        Do While (i_ <= (cb - 1))
                            b((i - j1), i_) = a(i, (i_ + i1_))
                            i_ += 1
                        Loop
                        i += 1
                    Loop
                    lu.rmatrixlu2(b, (m - j1), cb, bp)
                    i = j1
                    Do While (i <= (m - 1))
                        i1_ = -j1
                        i_ = j1
                        Do While (i_ <= j2)
                            a(i, i_) = b((i - j1), (i_ + i1_))
                            i_ += 1
                        Loop
                        i += 1
                    Loop
                    i = 0
                    Do While (i <= (cb - 1))
                        ip = bp(i)
                        pivots((j1 + i)) = (j1 + ip)
                        If (bp(i) <> i) Then
                            If (j1 <> 0) Then
                                i_ = 0
                                Do While (i_ <= (j1 - 1))
                                    t(i_) = a((j1 + i), i_)
                                    i_ += 1
                                Loop
                                i_ = 0
                                Do While (i_ <= (j1 - 1))
                                    a((j1 + i), i_) = a((j1 + ip), i_)
                                    i_ += 1
                                Loop
                                i_ = 0
                                Do While (i_ <= (j1 - 1))
                                    a((j1 + ip), i_) = t(i_)
                                    i_ += 1
                                Loop
                            End If
                            If (j2 < (n - 1)) Then
                                i_ = (j2 + 1)
                                Do While (i_ <= (n - 1))
                                    t(i_) = a((j1 + i), i_)
                                    i_ += 1
                                Loop
                                i_ = (j2 + 1)
                                Do While (i_ <= (n - 1))
                                    a((j1 + i), i_) = a((j1 + ip), i_)
                                    i_ += 1
                                Loop
                                i_ = (j2 + 1)
                                Do While (i_ <= (n - 1))
                                    a((j1 + ip), i_) = t(i_)
                                    i_ += 1
                                Loop
                            End If
                        End If
                        i += 1
                    Loop
                    If (j2 < (n - 1)) Then
                        i = (j1 + 1)
                        Do While (i <= j2)
                            j = j1
                            Do While (j <= (i - 1))
                                v = a(i, j)
                                i_ = (j2 + 1)
                                Do While (i_ <= (n - 1))
                                    a(i, i_) = (a(i, i_) - (v * a(j, i_)))
                                    i_ += 1
                                Loop
                                j += 1
                            Loop
                            i += 1
                        Loop
                    End If
                    If (j2 < (n - 1)) Then
                        i = (j2 + 1)
                        Do While (i <= (m - 1))
                            j = j1
                            Do While (j <= j2)
                                v = a(i, j)
                                i_ = (j2 + 1)
                                Do While (i_ <= (n - 1))
                                    a(i, i_) = (a(i, i_) - (v * a(j, i_)))
                                    i_ += 1
                                Loop
                                j += 1
                            Loop
                            i += 1
                        Loop
                    End If
                    j1 = (j2 + 1)
                    j2 = (Math.Min(minmn, (j1 + nb)) - 1)
                Loop
            End If
        End Sub

        Private Shared Sub rmatrixlu2(ByRef a As Double(,), ByVal m As Integer, ByVal n As Integer, ByRef pivots As Integer())
            Dim i As Integer = 0
            Dim j As Integer = 0
            Dim jp As Integer = 0
            Dim t1 As Double() = New Double(0) {}
            Dim s As Double = 0
            Dim i_ As Integer = 0
            pivots = New Integer((Math.Min(CInt((m - 1)), CInt((n - 1))) + 1)) {}
            t1 = New Double((Math.Max(CInt((m - 1)), CInt((n - 1))) + 1)) {}
            If Not ((m = 0) Or (n = 0)) Then
                j = 0
                Do While (j <= Math.Min(CInt((m - 1)), CInt((n - 1))))
                    jp = j
                    i = (j + 1)
                    Do While (i <= (m - 1))
                        If (Math.Abs(a(i, j)) > Math.Abs(a(jp, j))) Then
                            jp = i
                        End If
                        i += 1
                    Loop
                    pivots(j) = jp
                    If (a(jp, j) <> 0) Then
                        If (jp <> j) Then
                            i_ = 0
                            Do While (i_ <= (n - 1))
                                t1(i_) = a(j, i_)
                                i_ += 1
                            Loop
                            i_ = 0
                            Do While (i_ <= (n - 1))
                                a(j, i_) = a(jp, i_)
                                i_ += 1
                            Loop
                            i_ = 0
                            Do While (i_ <= (n - 1))
                                a(jp, i_) = t1(i_)
                                i_ += 1
                            Loop
                        End If
                        If (j < m) Then
                            jp = (j + 1)
                            s = (1 / a(j, j))
                            i_ = jp
                            Do While (i_ <= (m - 1))
                                a(i_, j) = (s * a(i_, j))
                                i_ += 1
                            Loop
                        End If
                    End If
                    If (j < (Math.Min(m, n) - 1)) Then
                        jp = (j + 1)
                        i = (j + 1)
                        Do While (i <= (m - 1))
                            s = a(i, j)
                            i_ = jp
                            Do While (i_ <= (n - 1))
                                a(i, i_) = (a(i, i_) - (s * a(j, i_)))
                                i_ += 1
                            Loop
                            i += 1
                        Loop
                    End If
                    j += 1
                Loop
            End If
        End Sub

        ' Fields
        Public Const lunb As Integer = 8

    End Class

    Public Class yves

        'Author: Yves Vander Haeghen (Yves.VanderHaeghen@UGent.be)
        'Version: 1.0
        'VersionDate": 13 june 2003

        'Class of helper functions for simple algebra operations on 1 and 2 dimensional single arrays
        'Although speed is not essential, we try to avoid recreating and reallocating output arrays 
        'on every call as this could slow things down a lot. This means that usually the output arrays MUST
        'be allocated and passed to the functions, except when they are passed on by reference.
        'All matrices are supposedly ordered ROW x COLUMN

        'August 2003: Added non-linear optimization (Nelder-Mead simplex algorithm)

        Enum NormOrder As Integer
            AbsoluteValue = 1
            Euclidean = 2
            Max = 16
        End Enum

        'Defines for NMS algorithm
        Private Const NMSMAX = 30000
        Private Const NMSTINY = 0.000000001
        Private Const NMSTOL = 1.0E-23 'Machine precision?

        'Helper function for NMS algorithm
        Private Shared Sub Swap(ByRef sA As Single, ByRef sB As Single)
            Dim sTemp As Single
            sTemp = sA
            sA = sB
            sB = sTemp
        End Sub

        'Prototype for the function to be optimized
        Delegate Function SolveNonLinearError(ByVal sX() As Single) As Single

        Public Shared Function SolveNonLinear(ByVal sX(,) As Single, _
                                              ByVal sY() As Single, _
                                              ByVal lNrIterations As Long, _
                                              ByVal ErrorFunction As SolveNonLinearError) As Boolean
            'Minimize a function of iNrDim dimensions using the Nelder-Mead
            'simplex algorythm (NMS). sX is a (iNrDim + 1) by iNrDim matrix
            'initialized with a starting simplex. sY is a iNrDim vector with
            'function values at the simplex points.
            Dim iNrDims As Integer
            Dim iNrPts As Integer, iLo As Integer, iHi As Integer
            Dim i As Integer, j As Integer, iNHi As Integer
            Dim sSum() As Single, sYSave As Single, sYTry As Single
            Dim sRTol As Single, iDisplayCounter As Integer

            SolveNonLinear = False

            iNrDims = sX.GetUpperBound(1) + 1
            iNrPts = iNrDims + 1
            ReDim sSum(iNrDims - 1)
            lNrIterations = 0
            iDisplayCounter = 0
            Sum(sX, sSum)
            Do
                'Rank vertices of simplex by function value
                iLo = 0
                If sY(0) > sY(1) Then
                    iNHi = 1
                    iHi = 0
                Else
                    iNHi = 0
                    iHi = 1
                End If

                For i = 0 To iNrPts - 1
                    If sY(i) <= sY(iLo) Then iLo = i
                    If sY(i) > sY(iHi) Then
                        iNHi = iHi
                        iHi = i
                    ElseIf (sY(i) > sY(iNHi)) And (i <> iHi) Then
                        iNHi = i
                    End If
                Next i

                'TEST
                'Debug.Print "Highest vertex: " & iHi & ", next " & iNHi
                'Debug.Print "Lowest vertex: " & iLo
                iDisplayCounter = iDisplayCounter + 1
                sRTol = 2.0# * Math.Abs(sY(iHi) - sY(iLo)) / (Math.Abs(sY(iHi)) + Math.Abs(sY(iLo)) + NMSTINY)
                If iDisplayCounter Mod 200 = 0 Then
                    Console.WriteLine("Iteration " & lNrIterations & ", best solution has error: " & sY(iLo))
                End If

                'Convergence criterium
                If sRTol < NMSTOL Then
                    Swap(sY(0), sY(iLo))
                    For i = 0 To iNrDims - 1
                        Swap(sX(0, i), sX(iLo, i))
                    Next i

                    Dim sCoef(iNrDims - 1) As Single
                    Console.WriteLine("Convergence after " & lNrIterations & " iterations, with error " & sY(iLo))
                    GetMatrixRow(sX, sCoef, iLo)
                    Console.WriteLine("Parameters are " & ToString(sCoef))

                    Exit Do
                End If

                If lNrIterations > NMSMAX Then
                    'Do not raise error, result is useful most of the time!
                    'NMSErrorType = NMSTooManyIterations
                    'GoTo ErrorHandler
                    Exit Function
                End If
                lNrIterations = lNrIterations + 2

                sYTry = SolveNonLinearAdjustSimplex(sX, sY, sSum, iNrDims, iHi, -1.0#, ErrorFunction)

                If sYTry < sY(iLo) Then
                    sYTry = SolveNonLinearAdjustSimplex(sX, sY, sSum, iNrDims, iHi, 2.0#, ErrorFunction)
                ElseIf sYTry > sY(iNHi) Then
                    sYSave = sY(iHi)
                    sYTry = SolveNonLinearAdjustSimplex(sX, sY, sSum, iNrDims, iHi, 0.5, ErrorFunction)
                    If sYTry >= sYSave Then
                        For i = 0 To iNrPts - 1
                            If i <> iLo Then
                                For j = 0 To iNrDims - 1
                                    sX(i, j) = 0.5 * (sX(i, j) + sX(iLo, j))
                                    sSum(j) = sX(i, j)
                                Next j
                                sY(i) = ErrorFunction(sSum)
                            End If
                        Next i
                        lNrIterations = lNrIterations + iNrDims
                        Sum(sX, sSum)
                    Else
                        lNrIterations = lNrIterations - 1
                    End If
                End If
            Loop While True
            SolveNonLinear = True
        End Function

        Private Shared Function SolveNonLinearAdjustSimplex(ByVal sX(,) As Single, _
                                      ByVal sY() As Single, _
                                      ByVal sSum() As Single, _
                                      ByVal iNrDims As Integer, _
                                      ByVal iHi As Integer, _
                                      ByVal sFactor As Single, _
                                      ByVal ErrorFunction As SolveNonLinearError) As Single
            Dim i As Integer, sFactor1 As Single, sFactor2 As Single
            Dim sYTry As Single, sXTry(iNrDims - 1) As Single

            'Debug.Print "Try adjustment simplex with factor " & sFactor
            sFactor1 = (1.0# - sFactor) / iNrDims
            sFactor2 = sFactor1 - sFactor
            For i = 0 To iNrDims - 1
                sXTry(i) = sSum(i) * sFactor1 - sX(iHi, i) * sFactor2
            Next i
            sYTry = ErrorFunction(sXTry)
            'Console.WriteLine("Proposed vertex " & ToString(sXTry) & "Value " & sYTry)

            If sYTry < sY(iHi) Then
                sY(iHi) = sYTry
                For i = 0 To iNrDims - 1
                    sSum(i) = sSum(i) + sXTry(i) - sX(iHi, i)
                    sX(iHi, i) = sXTry(i)
                Next i
                'DisplayMatrix "New simplex", sX()
            Else
                'Debug.Print "Vertex rejected"
            End If
            SolveNonLinearAdjustSimplex = sYTry
        End Function

        Public Shared Sub SolveNonLinearTest(ByVal iNrDims As Integer)

            Dim sCoef() As Single, iVertexNr As Integer
            Dim sSimplex(,) As Single, iNrVertices As Integer
            Dim sSimplexVal() As Single, lNrIterations As Long
            Dim i As Integer

            Randomize()
            iNrVertices = iNrDims + 1
            ReDim sCoef(iNrDims - 1)
            ReDim sSimplex(iNrVertices - 1, iNrDims - 1)
            ReDim sSimplexVal(iNrVertices - 1)
            For iVertexNr = 0 To iNrVertices - 1
                For i = 0 To iNrDims - 1
                    If iVertexNr > 0 And i = iVertexNr - 1 Then
                        sCoef(i) = 1.0 * Rnd()
                    Else
                        sCoef(i) = 0.0#
                    End If
                Next i

                'Put in simplex and compute function value
                SetMatrixRow(sSimplex, sCoef, iVertexNr)
                sSimplexVal(iVertexNr) = SolveNonLinearTestError(sCoef)

            Next iVertexNr

            'Optimize
            SolveNonLinear(sSimplex, sSimplexVal, lNrIterations, AddressOf SolveNonLinearTestError)
        End Sub

        Public Shared Function SolveNonLinearTestError(ByVal sCoef() As Single) As Single
            'Return the error
            Dim i, iNrDims As Integer, sError As Single = 100
            iNrDims = sCoef.GetUpperBound(0) + 1

            For i = 0 To iNrDims - 1
                If i Mod 2 = 0 Then
                    sError += sCoef(i) * i
                Else
                    sError -= sCoef(i) * i
                End If
            Next i
            Return Math.Abs(sError)
        End Function

        Public Overloads Shared Function Solve(ByVal sA(,) As Single, ByVal sX(,) As Single, ByVal sY(,) As Single) As Boolean
            'Solve A.X = Y, FOR every column of Y!!!
            'This is useful because we only have to decompose A once, 
            'and then use this decomposition to compute X = inv(A).Y for every column of Y
            'The results are stored in the corresponding columns of X
            'See overloaded Solve for general explanation about the solver.
            Dim sU(,) As Single = New Single(,) {}, sW() As Single = New Single() {}, sV(,) As Single = New Single(,) {}, i As Integer
            Dim strError As String = ""

            If SVDDecomposition(sA, sU, sW, sV, strError) = False Then
                MsgBox("Algebra.Solve: SVD gives error '" & strError & "'", MsgBoxStyle.Critical + MsgBoxStyle.OkOnly)
                Return False
            End If

            SVDRemoveSingularValues(sW, 0.0001)

            'Run though every column of sY, compute the result, and store it in the corresponding column of sX.
            Dim iNrEquationSets As Integer = sY.GetUpperBound(1) + 1
            Dim iNrVariables As Integer = sA.GetUpperBound(1) + 1
            Dim iNrEquationsPerSet As Integer = sA.GetUpperBound(0) + 1
            Dim sXCol(iNrVariables - 1), sYCol(iNrEquationsPerSet - 1) As Single
            For i = 0 To iNrEquationSets - 1
                GetMatrixColumn(sY, sYCol, i)
                Solve(sA, sXCol, sYCol)
                SetMatrixColumn(sX, sXCol, i)
            Next
        End Function

        Public Overloads Shared Function Solve(ByVal sA(,) As Single, ByVal sX() As Single, ByVal sY() As Single) As Boolean
            'Solve the set of linear equations represented by A.x = y.
            'The number of equations can be larger than the number of variables (overdetermined):
            'i.e. the number of rows in A > number of cols in A. In that case the solution is 
            'a solution in the least-squares sense.
            'This routine uses singular value decomposition, translated from "Numerical recipes in C"
            Dim sU(,) As Single = New Single(,) {}, sW() As Single = New Single() {}, sV(,) As Single = New Single(,) {}
            Dim strError As String = ""

            Console.WriteLine("Solving linear set of equations A.x = y with A" & _
              vbNewLine & yves.ToString(sA) & _
              vbNewLine & "y" & _
              vbNewLine & yves.ToString(sY))

            If SVDDecomposition(sA, sU, sW, sV, strError) = False Then
                'MsgBox("Algebra.Solve: SVD gives error '" & strError & "'", MsgBoxStyle.Critical + MsgBoxStyle.OkOnly)
                Return False
            End If

            SVDRemoveSingularValues(sW, 0.0001)

            'Compute pseudo-inverse multiplied with sY
            SVDInvert(sU, sW, sV, sY, sX)
            Return True
        End Function

        Private Shared Sub SVDRemoveSingularValues(ByVal sW() As Single, ByVal sThresholdFactor As Single)
            'Set singular values to zero by compairing them to
            'the highest value in w. 
            Dim iNrVariables As Integer = sW.GetUpperBound(0) + 1
            Dim i As Integer, sWMax As Single = 0.0

            For i = 0 To iNrVariables - 1
                If sW(i) > sWMax Then sWMax = sW(i)
            Next i
            Dim sThreshold As Single = sThresholdFactor * sWMax
            For i = 0 To iNrVariables - 1
                If sW(i) < sThreshold Then sW(i) = 0.0
            Next i
        End Sub

        Private Shared Sub SVDInvert(ByVal sU(,) As Single, _
                                     ByVal sW() As Single, _
                                     ByVal sV(,) As Single, _
                                     ByVal sY() As Single, _
                                     ByVal sX() As Single)
            'Computes Y = inv(A).Y using the SVD decomposition of A = U.W.Vt
            Dim jj, j, i, m, n As Integer
            Dim s As Single

            m = sU.GetUpperBound(0) + 1
            n = sU.GetUpperBound(1) + 1

            Dim tmp(n - 1) As Single
            For j = 1 To n
                s = 0.0
                If sW(j - 1) <> 0.0 Then
                    For i = 1 To m
                        s = s + sU(i - 1, j - 1) * sY(i - 1)
                    Next i
                    s = s / sW(j - 1)
                End If

                tmp(j - 1) = s
            Next j

            For j = 1 To n
                s = 0.0
                For jj = 1 To n
                    s = s + sV(j - 1, jj - 1) * tmp(jj - 1)
                Next jj
                sX(j - 1) = s
            Next j
        End Sub

        Private Shared Function SVDDecomposition(ByVal sA(,) As Single, _
                                                 ByRef sU(,) As Single, _
                                                 ByRef sW() As Single, _
                                                 ByRef sV(,) As Single, _
                                                 ByVal strError As String) As Boolean

            'Compute the singular value decomposition of
            'an m sx n matrix A: A = U.W.Vt
            'None of the byref matrices must be allocated here.
            'If something goes wrong it returns false with a message in strError
            Dim Flag As Boolean, i As Integer, its As Integer
            Dim j As Integer, jj As Integer, k As Integer
            Dim l As Integer, nm As Integer
            Dim c As Single, f As Single, h As Single, s As Single
            Dim sX As Single, sY As Single, sz As Single, rv1() As Single
            Dim anorm As Single, g As Single, hhscale As Single
            'Extra variables for VBasic.
            Dim sTemp1 As Single, n As Integer, m As Integer

            m = sA.GetUpperBound(0) + 1
            n = sA.GetUpperBound(1) + 1

            If m < n Then
                strError = "Not enough rows in A (underdetermined system)"
                Return False
            End If

            ReDim sU(m - 1, n - 1)
            ReDim sW(n - 1)
            ReDim sV(n - 1, n - 1)
            ReDim rv1(n - 1)

            'Copy the matrix A in U.
            Array.Copy(sA, sU, sA.Length)

            'Householder reduction to bidiagonal form
            anorm = 0.0#
            For i = 1 To n
                l = i + 1
                rv1(i - 1) = hhscale * g
                g = 0.0#
                s = 0.0#
                hhscale = 0.0#
                If i <= m Then
                    For k = i To m
                        hhscale = hhscale + Math.Abs(sU(k - 1, i - 1))
                    Next k

                    If hhscale <> 0.0# Then
                        For k = i To m
                            sU(k - 1, i - 1) = sU(k - 1, i - 1) / hhscale
                            s = s + sU(k - 1, i - 1) * sU(k - 1, i - 1)
                        Next k

                        f = sU(i - 1, i - 1)
                        If f >= 0 Then
                            g = -Math.Sqrt(s)
                        Else
                            g = Math.Sqrt(s)
                        End If

                        h = f * g - s
                        sU(i - 1, i - 1) = f - g
                        If i <> n Then
                            For j = l To n
                                s = 0.0#
                                For k = i To m
                                    s = s + sU(k - 1, i - 1) * sU(k - 1, j - 1)
                                Next k
                                f = s / h
                                For k = i To m
                                    sU(k - 1, j - 1) = sU(k - 1, j - 1) + f * sU(k - 1, i - 1)
                                Next k
                            Next j
                        End If

                        For k = i To m
                            sU(k - 1, i - 1) = sU(k - 1, i - 1) * hhscale
                        Next k

                    End If
                End If

                sW(i - 1) = hhscale * g
                g = 0.0#
                s = 0.0#
                hhscale = 0.0#
                If i <= m And i <> n Then
                    For k = l To n
                        hhscale = hhscale + Math.Abs(sU(i - 1, k - 1))
                    Next k

                    If hhscale <> 0.0# Then
                        For k = l To n
                            sU(i - 1, k - 1) = sU(i - 1, k - 1) / hhscale
                            s = s + sU(i - 1, k - 1) * sU(i - 1, k - 1)
                        Next k

                        f = sU(i - 1, l - 1)
                        If f >= 0 Then
                            g = -Math.Sqrt(s)
                        Else
                            g = Math.Sqrt(s)
                        End If
                        h = f * g - s
                        sU(i - 1, l - 1) = f - g

                        For k = l To n
                            rv1(k - 1) = sU(i - 1, k - 1) / h
                        Next k

                        If i <> m Then
                            For j = l To m
                                s = 0.0#
                                For k = l To n
                                    s = s + sU(j - 1, k - 1) * sU(i - 1, k - 1)
                                Next k

                                For k = l To n
                                    sU(j - 1, k - 1) = sU(j - 1, k - 1) + s * rv1(k - 1)
                                Next k
                            Next j
                        End If

                        For k = l To n
                            sU(i - 1, k - 1) = sU(i - 1, k - 1) * hhscale
                        Next k

                    End If
                End If

                sTemp1 = Math.Abs(sW(i - 1)) + Math.Abs(rv1(i - 1))
                If anorm < sTemp1 Then anorm = sTemp1
            Next i
            'Call DisplayMatrix("Bidiagonal form", a())

            'Accumulation of right-hand transformations
            For i = n To 1 Step -1
                If i < n Then
                    If g <> 0.0# Then
                        For j = l To n
                            sV(j - 1, i - 1) = (sU(i - 1, j - 1) / sU(i - 1, l - 1)) / g
                        Next j

                        For j = l To n
                            s = 0.0#
                            For k = l To n
                                s = s + sU(i - 1, k - 1) * sV(k - 1, j - 1)
                            Next k

                            For k = l To n
                                sV(k - 1, j - 1) = sV(k - 1, j - 1) + s * sV(k - 1, i - 1)
                            Next k
                        Next j
                    End If

                    For j = l To n
                        sV(i - 1, j - 1) = 0.0#
                        sV(j - 1, i - 1) = 0.0#
                    Next j

                End If

                sV(i - 1, i - 1) = 1.0#
                g = rv1(i - 1)
                l = i
            Next i

            'Accumulation of left-hand transformations
            For i = n To 1 Step -1
                l = i + 1
                g = sW(i - 1)
                If i < n Then
                    For j = l To n
                        sU(i - 1, j - 1) = 0.0#
                    Next j
                End If

                If g <> 0.0# Then
                    g = 1.0# / g
                    If i <> n Then
                        For j = l To n
                            s = 0.0#
                            For k = l To m
                                s = s + sU(k - 1, i - 1) * sU(k - 1, j - 1)
                            Next k

                            f = (s / sU(i - 1, i - 1)) * g
                            For k = i To m
                                sU(k - 1, j - 1) = sU(k - 1, j - 1) + f * sU(k - 1, i - 1)
                            Next k
                        Next j
                    End If

                    For j = i To m
                        sU(j - 1, i - 1) = sU(j - 1, i - 1) * g
                    Next j
                Else
                    For j = i To m
                        sU(j - 1, i - 1) = 0.0#
                    Next j
                End If

                sU(i - 1, i - 1) = sU(i - 1, i - 1) + 1.0#
            Next i

            'Diagonalization of the bidiagonal form (QR algorythm)
            For k = n To 1 Step -1
                For its = 1 To 30
                    'Debug.Print "Iteration " & its
                    Flag = True
                    For l = k To 1 Step -1
                        nm = l - 1
                        If Math.Abs(rv1(l - 1)) + anorm = anorm Then
                            Flag = False
                            Exit For
                        End If

                        If Math.Abs(sW(nm - 1)) + anorm = anorm Then
                            Exit For
                        End If
                    Next l

                    If Flag = True Then
                        c = 0.0#
                        s = 1.0#
                        For i = l To k
                            f = s * rv1(i - 1)
                            If (Math.Abs(f) + anorm) <> anorm Then
                                g = sW(i - 1)
                                h = Pythagoras(f, g)
                                sW(i - 1) = h
                                h = 1.0# / h
                                c = g * h
                                s = (-f * h)
                                For j = 1 To m
                                    sY = sU(j - 1, nm - 1)
                                    sz = sU(j - 1, i - 1)
                                    sU(j - 1, nm - 1) = sY * c + sz * s
                                    sU(j - 1, i - 1) = sz * c - sY * s
                                Next j
                            End If
                        Next i
                    End If
                    sz = sW(k - 1)

                    'Test for convergence
                    If l = k Then
                        If sz < 0.0# Then
                            sW(k - 1) = -sz
                            For j = 1 To n
                                sV(j - 1, k - 1) = -sV(j - 1, k - 1)
                            Next j
                        End If
                        Exit For
                    End If

                    If its = 30 Then
                        strError = "Too many iterations"
                        Return False
                    End If

                    sX = sW(l - 1)
                    nm = k - 1
                    sY = sW(nm - 1)
                    g = rv1(nm - 1)
                    h = rv1(k - 1)
                    f = ((sY - sz) * (sY + sz) + (g - h) * (g + h)) / (2.0# * h * sY)
                    g = Pythagoras(f, 1.0#)
                    If f > 0.0# Then
                        f = ((sX - sz) * (sX + sz) + h * ((sY / (f + Math.Abs(g))) - h)) / sX
                    Else
                        f = ((sX - sz) * (sX + sz) + h * ((sY / (f - Math.Abs(g))) - h)) / sX
                    End If

                    c = 1.0#
                    s = 1.0#
                    For j = l To nm
                        i = j + 1
                        g = rv1(i - 1)
                        sY = sW(i - 1)
                        h = s * g
                        g = c * g
                        sz = Pythagoras(f, h)
                        rv1(j - 1) = sz
                        c = f / sz
                        s = h / sz
                        f = sX * c + g * s
                        g = g * c - sX * s
                        h = sY * s
                        sY = sY * c
                        For jj = 1 To n
                            sX = sV(jj - 1, j - 1)
                            sz = sV(jj - 1, i - 1)
                            sV(jj - 1, j - 1) = sX * c + sz * s
                            sV(jj - 1, i - 1) = sz * c - sX * s
                        Next jj
                        sz = Pythagoras(f, h)
                        sW(j - 1) = sz
                        If sz <> 0.0# Then
                            sz = 1.0# / sz
                            c = f * sz
                            s = h * sz
                        End If
                        f = c * g + s * sY
                        sX = c * sY - s * g
                        For jj = 1 To m
                            sY = sU(jj - 1, j - 1)
                            sz = sU(jj - 1, i - 1)
                            sU(jj - 1, j - 1) = sY * c + sz * s
                            sU(jj - 1, i - 1) = sz * c - sY * s
                        Next jj
                    Next j
                    rv1(l - 1) = 0.0#
                    rv1(k - 1) = f
                    sW(k - 1) = sX
                Next its
            Next k
            Return True
        End Function

        Private Shared Function Pythagoras(ByVal a As Single, ByVal b As Single) As Single
            Dim at As Single, bt As Single, ct As Single

            at = Math.Abs(a)
            bt = Math.Abs(b)
            If at > bt Then
                ct = bt / at
                Pythagoras = at * Math.Sqrt(1.0# + ct * ct)
            Else
                If bt = 0.0# Then
                    'Means a is also 0
                    Pythagoras = 0.0#
                Else
                    ct = at / bt
                    Pythagoras = bt * Math.Sqrt(1.0# + ct * ct)
                End If
            End If
        End Function

        Public Overloads Shared Sub Add(ByVal sV1() As Single, ByVal sV2() As Single, ByVal sR() As Single)
            Dim i, iHiCol As Integer
            iHiCol = sV1.GetUpperBound(0)
            For i = 0 To iHiCol
                sR(i) = sV1(i) + sV2(i)
            Next
        End Sub

        Public Overloads Shared Sub Add(ByVal sM1(,) As Single, ByVal sM2(,) As Single, ByVal sMR(,) As Single)
            Dim i, j, iHiRow, iHiCol As Integer
            GetBounds(sM1, iHiRow, iHiCol)
            For j = 0 To iHiCol
                For i = 0 To iHiRow
                    sMR(i, j) = sM1(i, j) + sM2(i, j)
                Next i
            Next j
        End Sub

        Public Overloads Shared Sub Subtract(ByVal sV1() As Single, ByVal sV2() As Single, ByVal sR() As Single)
            Dim i As Integer, iHiCol As Integer
            iHiCol = sV1.GetUpperBound(0)
            For i = 0 To iHiCol
                sR(i) = sV1(i) - sV2(i)
            Next
        End Sub

        Public Overloads Shared Sub Subtract(ByVal sM1(,) As Single, ByVal sM2(,) As Single, ByVal sMR(,) As Single)
            Dim i, j, iHiRow, iHiCol As Integer
            GetBounds(sM1, iHiRow, iHiCol)
            For j = 0 To iHiCol
                For i = 0 To iHiRow
                    sMR(i, j) = sM1(i, j) - sM2(i, j)
                Next i
            Next j
        End Sub

        Public Overloads Shared Function Norm(ByVal sV1() As Single) As Single
            Return Norm(sV1, NormOrder.Euclidean)
        End Function

        Public Overloads Shared Function Norm(ByVal sV1() As Single, ByVal iOrder As NormOrder) As Single
            'Compute norm of given order
            Dim i As Integer, sNorm As Single = 0.0, iHiCol As Integer
            iHiCol = sV1.GetUpperBound(0)
            Select Case iOrder
                Case NormOrder.AbsoluteValue
                    For i = 0 To iHiCol
                        sNorm += Math.Abs(sV1(i))
                    Next
                Case NormOrder.Euclidean
                    For i = 0 To iHiCol
                        sNorm += sV1(i) ^ 2
                    Next
                    sNorm = sNorm ^ 0.5
                Case NormOrder.Max
                    sNorm = 0
                    For i = 0 To iHiCol
                        Dim sTemp As Single = Math.Abs(sV1(i))
                        If sTemp > sNorm Then sNorm = sTemp
                    Next
            End Select
            Return sNorm
        End Function

        Public Overloads Shared Sub Mean(ByVal sM(,) As Single, ByVal sV() As Single)
            'Compute columnwise mean
            Dim i, iHiCol As Integer
            iHiCol = sV.GetUpperBound(0)
            Sum(sM, sV)
            For i = 0 To iHiCol
                sV(i) = sV(i) / (iHiCol + 1)
            Next i
        End Sub

        Public Overloads Shared Function Mean(ByVal sV() As Single) As Single
            'Compute average of a vector
            Dim sMean As Single
            sMean = Sum(sV)
            sMean /= sV.GetLength(0)
            Return sMean
        End Function


        Public Overloads Shared Sub Sum(ByVal sM(,) As Single, ByVal sV() As Single)
            'Compute columnwise sum of matrix
            Dim i, j, iHiRow, iHiCol As Integer
            GetBounds(sM, iHiRow, iHiCol)

            For j = 0 To iHiCol
                sV(j) = 0.0
                For i = 0 To iHiRow
                    sV(j) += sM(i, j)
                Next i
            Next j
        End Sub

        Public Overloads Shared Function Sum(ByVal sV() As Single) As Single
            'Compute sum of elements of vector
            Dim sSum As Single = 0
            Dim i, iHiCol As Integer
            iHiCol = sV.GetUpperBound(0)

            For i = 0 To iHiCol
                sSum = sSum + sV(i)
            Next i
            Return sSum
        End Function

        Public Overloads Shared Function Max(ByVal sV() As Single, ByRef iPos As Integer) As Single
            'Find max of a vector
            Dim i As Integer, sMax As Single = 0.0
            Dim iHiCol As Integer
            iHiCol = sV.GetUpperBound(0)

            For i = 0 To iHiCol
                If sV(i) > sMax Then
                    iPos = i
                    sMax = sV(i)
                End If
            Next i
            Return sMax
        End Function

        Public Overloads Shared Function Max(ByVal sV() As Single) As Single
            Dim iPos As Integer
            Return Max(sV, iPos)
        End Function

        Public Overloads Shared Function Max(ByVal sM(,) As Single) As Single
            'Find max of a matrix
            Dim i, j As Integer
            Return Max(sM, i, j)
        End Function

        Public Overloads Shared Function Max(ByVal sM(,) As Single, ByRef iCol As Integer, ByRef iRow As Integer) As Single
            'Find max of a matrix
            Dim sMAx As Single = 0
            Dim i, j, iHiRow, iHiCol As Integer
            GetBounds(sM, iHiRow, iHiCol)
            For j = 0 To iHiCol
                For i = 0 To iHiRow
                    If sM(i, j) > sMAx Then
                        iCol = j
                        iRow = i
                        sMAx = sM(i, j)
                    End If
                Next i
            Next j
            Return sMAx
        End Function

        Public Overloads Shared Function Scale(ByVal sX As Single, ByVal sOffset As Single, ByVal sScale As Single) As Single
            'Scale a scalar with an offset. For vectors and matrices this would lead to too many 
            'different versions, so use Subtract to have an offset.
            Return (sX - sOffset) * sScale
        End Function

        Public Overloads Shared Sub Scale(ByVal sScale As Single, _
                                          ByVal sV2() As Single, _
                                          ByVal sY() As Single)
            'Scale elements of vector V2 using the scalar sScale
            Dim i As Integer, iHiRow As Integer
            iHiRow = UBound(sV2)
            For i = 0 To iHiRow
                sY(i) = sScale * sV2(i)
            Next i
        End Sub

        Public Overloads Shared Sub Scale(ByVal sV1() As Single, _
                                          ByVal sV2() As Single, _
                                          ByVal sY() As Single)
            'Scale elements of vector V2 using the elements of V1
            Dim i As Integer, iHiRow As Integer
            iHiRow = UBound(sV2)
            For i = 0 To iHiRow
                sY(i) = sV1(i) * sV2(i)
            Next i
        End Sub

        Public Overloads Shared Sub Scale(ByVal sScale As Single, _
                                        ByVal sB(,) As Single, _
                                        ByVal sY(,) As Single)
            'Scale elements of matrix sB using  sScale
            Dim i, j, iHiRow, iHiCol As Integer
            GetBounds(sB, iHiRow, iHiCol)
            For i = 0 To iHiRow
                For j = 0 To iHiCol
                    sY(i, j) = sScale * sB(i, j)
                Next j
            Next i
        End Sub

        Public Overloads Shared Sub Scale(ByVal sA(,) As Single, _
                                        ByVal sB(,) As Single, _
                                        ByVal sY(,) As Single)
            'Scale elements of matrix sB using the corresponding elements of sA
            Dim i, j, iHiRow, iHiCol As Integer
            GetBounds(sB, iHiRow, iHiCol)
            For i = 0 To iHiRow
                For j = 0 To iHiCol
                    sY(i, j) = sA(i, j) * sB(i, j)
                Next j
            Next i
        End Sub

        Public Overloads Shared Sub Scale(ByVal sRowScales() As Single, _
                                          ByVal sB(,) As Single, _
                                          ByVal sY(,) As Single)
            'Scale elements of matrix sB using the corresponding elements of sRowScales, per ROW
            Dim i, j, iHiRow, iHiCol As Integer
            GetBounds(sB, iHiRow, iHiCol)
            For i = 0 To iHiRow
                For j = 0 To iHiCol
                    sY(i, j) = sRowScales(i) * sB(i, j)
                Next j
            Next i
        End Sub

        Public Overloads Shared Sub Scale(ByVal sB(,) As Single, _
                                          ByVal sColScales() As Single, _
                                          ByVal sY(,) As Single)
            'Scale elements of matrix sB using the corresponding elements of sColScales, per col
            Dim i, j, iHiRow, iHiCol As Integer
            GetBounds(sB, iHiRow, iHiCol)
            For i = 0 To iHiRow
                For j = 0 To iHiCol
                    sY(i, j) = sColScales(j) * sB(i, j)
                Next j
            Next i
        End Sub

        Public Overloads Shared Sub Product(ByVal sA(,) As Single, _
                         ByVal sB(,) As Single, _
                         ByVal sC(,) As Single)
            'Compute A * B and store in C. 
            'Raise a fatal run-time error if any errors (no return value)!
            Dim i, j, k, iAHiRow, iAHiCol As Integer
            GetBounds(sA, iAHiRow, iAHiCol)
            Dim iBHiRow, iBHiCol As Integer
            GetBounds(sB, iBHiRow, iBHiCol)
            Dim iCHiRow, iCHiCol As Integer
            GetBounds(sC, iCHiRow, iCHiCol)

            If (((iAHiCol) <> (iBHiRow)) Or _
                ((iAHiRow) <> (iCHiRow)) Or _
                ((iBHiCol) <> (iCHiCol))) Then
                MsgBox("Algebra.Product: Incompatible matrix dimensions", MsgBoxStyle.OkOnly + MsgBoxStyle.Critical)
            End If

            For i = 0 To iCHiRow
                For j = 0 To iCHiCol
                    sC(i, j) = 0.0
                    For k = 0 To iAHiCol
                        sC(i, j) += sA(i, k) * sB(k, j)
                    Next k
                Next j
            Next i
        End Sub

        Public Overloads Shared Function Product(ByVal sV1() As Single, ByVal sV2() As Single) As Single
            'Return the scalar product of two vectors.
            Dim i As Integer, iHiRow As Integer, sResult As Single

            iHiRow = UBound(sV1)
            For i = 0 To iHiRow
                sResult = sResult + sV1(i) * sV2(i)
            Next i
            Return sResult
        End Function

        Public Overloads Shared Sub Product(ByVal sM() As Single, _
                                            ByVal sX() As Single, _
                                            ByVal sY(,) As Single)
            'Multiply a vector times a vector (Y = M.Y), by interpreting the vector M as a columnmatrix,
            'and X as a rowmatrix. Result is a matrix
            Dim sA(0, sM.GetUpperBound(0)) As Single, sB(sX.GetUpperBound(0), 0) As Single

            SetMatrixColumn(sA, sM, 0)
            SetMatrixRow(sB, sX, 0)
            Product(sA, sB, sY)
        End Sub

        Public Overloads Shared Sub Product(ByVal sM(,) As Single, _
                                            ByVal sX() As Single, _
                                            ByVal sY() As Single)
            'Multiply a matrix times a vector (y = M.x), by interpreting the vector X as a columnmatrix.
            Dim sB(sX.GetUpperBound(0), 0), sC(sM.GetUpperBound(0), 0) As Single

            SetMatrixColumn(sB, sX, 0)
            Product(sM, sB, sC)
            GetMatrixColumn(sC, sY, 0)
        End Sub

        Public Overloads Shared Sub Product(ByVal sX() As Single, _
                                            ByVal sM(,) As Single, _
                                            ByVal sY() As Single)
            'Multiply a vector with a matrix (y = x.M), by interpreting the vector X as a rowmatrix.
            Dim iHiCol As Integer = sX.GetUpperBound(0)
            Dim sB(0, iHiCol), sC(0, iHiCol) As Single

            SetMatrixRow(sB, sX, 0)
            Product(sM, sB, sC)
            GetMatrixRow(sC, sY, 0)
        End Sub

        Public Shared Sub SubMatrix(ByVal sA(,) As Single, _
                             ByVal sB(,) As Single, _
                             ByVal iRow As Integer, _
                             ByVal iCol As Integer)
            'Extract submatrix of the dimensions of B using row and col
            'as start values in sA. sA and sB can be mixed one and zero-
            'based, but iRow and iCol are interpreted according to sA
            Dim i, j, iHiRow, iHiCol As Integer
            GetBounds(sB, iHiRow, iHiCol)
            For i = 0 To iHiRow
                For j = 0 To iHiCol
                    sB(i, j) = sA(i + iRow, j + iCol)
                Next j
            Next i
        End Sub
        Public Overloads Shared Sub GetMatrixColumn(ByVal sM(,) As Single, _
                                ByVal sV() As Single, _
                                ByVal iCol As Integer)
            GetMatrixColumn(sM, sV, iCol, 0)
        End Sub

        Public Overloads Shared Sub GetMatrixColumn(ByVal sM(,) As Single, _
                                ByVal sV() As Single, _
                                ByVal iCol As Integer, _
                                ByVal iStartRow As Integer)
            'Fill vector with matrix col
            Dim i, iHiCol As Integer
            iHiCol = sV.GetUpperBound(0)
            For i = 0 To iHiCol
                sV(i) = sM(i + iStartRow, iCol)
            Next i
        End Sub

        Public Overloads Shared Sub GetMatrixRow(ByVal sM(,) As Single, _
                              ByVal sV() As Single, _
                              ByVal iRow As Integer)
            GetMatrixRow(sM, sV, iRow, 0)
        End Sub

        Public Overloads Shared Sub GetMatrixRow(ByVal sM(,) As Single, _
                                ByVal sV() As Single, _
                                ByVal iRow As Integer, _
                                ByVal iStartCol As Integer)
            'Fill vector with matrix row. 
            Dim i, iHiCol As Integer
            iHiCol = sV.GetUpperBound(0)
            For i = 0 To iHiCol
                sV(i) = sM(iRow, i + iStartCol)
            Next i
        End Sub

        Public Overloads Shared Sub SetMatrixColumn(ByVal sM(,) As Single, _
                                  ByVal sV() As Single, _
                                  ByVal iCol As Integer, _
                                  ByVal iStartRow As Integer)
            'Fill matrix col with vector
            Dim i, iHiCol As Integer
            iHiCol = sV.GetUpperBound(0)
            For i = 0 To iHiCol
                sM(i + iStartRow, iCol) = sV(i)
            Next i
        End Sub

        Public Overloads Shared Sub SetMatrixColumn(ByVal sM(,) As Single, _
                                   ByVal sV() As Single, _
                                   ByVal iCol As Integer)
            SetMatrixColumn(sM, sV, iCol, 0)
        End Sub

        Public Overloads Shared Sub SetMatrixRow(ByVal sM(,) As Single, _
                              ByVal sV() As Single, _
                              ByVal iRow As Integer)
            SetMatrixRow(sM, sV, iRow, 0)
        End Sub

        Public Overloads Shared Sub SetMatrixRow(ByVal sM(,) As Single, _
                                ByVal sV() As Single, _
                                ByVal iRow As Integer, _
                                ByVal iStartCol As Integer)
            Dim i, iHiCol As Integer
            iHiCol = sV.GetUpperBound(0)
            For i = 0 To iHiCol
                sM(iRow, i + iStartCol) = sV(i)
            Next i
        End Sub

        Public Shared Sub MatrixToVector(ByVal sM(,) As Single, _
                                         ByVal sV() As Single)
            'Put all elements of a matrix into a vector
            Dim i, j, iHiRow, iHiCol, k As Integer

            GetBounds(sM, iHiRow, iHiCol)
            k = 0
            For i = 0 To iHiRow
                For j = 0 To iHiCol
                    sV(k) = sM(i, j)
                    k += 1
                Next
            Next
        End Sub

        Public Shared Sub VectorToMatrix(ByVal sV() As Single, _
                                         ByVal sM(,) As Single)
            'Put all elements of a vector into a vector. Use the shape of the matrix
            Dim i, j, iHiRow, iHiCol, k As Integer

            GetBounds(sM, iHiRow, iHiCol)
            k = 0
            For i = 0 To iHiRow
                For j = 0 To iHiCol
                    sM(i, j) = sV(k)
                    k += 1
                Next
            Next
        End Sub

        Public Shared Sub Transpose(ByVal sA(,) As Single, ByVal sAt(,) As Single)
            'Transpose matrix A and put result in At. Output has
            'same base as input. Input arguments must be different!
            Dim i, j, iHiRow, iHiCol As Integer
            GetBounds(sA, iHiRow, iHiCol)

            For i = 0 To iHiRow
                For j = 0 To iHiCol
                    sAt(j, i) = sA(i, j)
                Next j
            Next i
        End Sub

        Public Shared Function Load(ByVal strFile As String, ByRef sM(,) As Single) As Boolean
            'Read a tex file with a matrix or vector stored separated by spaces and
            'newlines. sM will be redimensioned as necessary and must be
            'a dynamic array. Redimensioning can only affect the last dimension!
            'When a vector is read in the matrix will be of size n x 1, and can easily 
            'be converted to a vector
            Dim iNrCols As Integer
            Dim iRowNr As Integer, iColNr As Integer
            Dim sMt(,) As Single = New Single(,) {}, strText As String, strTextItems() As String

            Try
                FileOpen(5, strFile, OpenMode.Input, OpenAccess.Read)
            Catch e As Exception
                MsgBox("Algebra.Load:" & e.Message, MsgBoxStyle.OkOnly + MsgBoxStyle.Critical)
                Return False
            End Try

            iRowNr = 0
            iNrCols = 0
            Do While Not EOF(5)
                'Read first line to count number of columns
                strText = Trim(LineInput(5))

                If strText.Length > 0 Then
                    strText = strText.Replace("  ", " ") 'Make sure no 2 spaces are in the string ...
                    strText = strText.Replace("   ", " ") 'Make sure no 3 spaces are in the string ...
                    strTextItems = strText.Split()

                    'Redimension the array if the nr of cols is known, i.e. after
                    'reading the first line.
                    If iRowNr = 0 Then
                        iNrCols = (strTextItems.GetUpperBound(0) + 1)
                        ReDim sMt(iNrCols - 1, 0)
                    Else
                        ReDim Preserve sMt(iNrCols - 1, iRowNr)
                    End If

                    'Read values into transposed matrix
                    For iColNr = 0 To iNrCols - 1
                        'sMt(iColNr, iRowNr) = CSng(strTextItems(iColNr))
                        sMt(iColNr, iRowNr) = Val(strTextItems(iColNr))
                    Next
                    iRowNr += 1
                End If
            Loop

            'close file
            FileClose(5)

            'Transpose matrix to output format
            ReDim sM(iRowNr - 1, iNrCols - 1)
            Transpose(sMt, sM)
            Return True
        End Function

        Public Overloads Shared Sub Save(ByVal strFile As String, _
                            ByVal sM(,) As Single)
            Save(strFile, sM, 16, 2)
        End Sub

        Public Overloads Shared Sub Save(ByVal strFile As String, _
                              ByVal sM(,) As Single, _
                              ByVal iPrecBeforeDec As Integer, _
                              ByVal iPrecAfterDec As Integer)
            'Save a matrix to file.
            Dim strF As String = ""
            Dim i, j, iHiRow, iHiCol As Integer

            If iPrecAfterDec = -1 Then
                strF = "0."
            Else
                For i = 1 To iPrecAfterDec
                    strF = strF & "0"
                Next
                strF = strF & "."
            End If

            For i = 1 To iPrecBeforeDec
                strF = strF & "#"
            Next

            If System.IO.File.Exists(strFile) Then System.IO.File.Delete(strFile)

            Try
                FileOpen(5, strFile, OpenMode.Output, OpenAccess.Write)
                GetBounds(sM, iHiRow, iHiCol)
                For i = 0 To iHiRow
                    For j = 0 To iHiCol - 1
                        Print(5, Format(sM(i, j), strF), SPC(1))
                    Next j
                    PrintLine(5, SPC(1), Format(sM(i, iHiCol), strF))
                Next i
                FileClose(5)
            Catch e As Exception
                MsgBox("Algebra.Save (file = " & strFile & "):" & e.Message, MsgBoxStyle.OkOnly + MsgBoxStyle.Critical)
            End Try
        End Sub

        Private Shared Sub GetBounds(ByVal sM(,) As Single, _
                                     ByRef iHiRow As Integer, _
                                     ByRef iHiCol As Integer)
            iHiRow = sM.GetUpperBound(0)
            iHiCol = sM.GetUpperBound(1)
        End Sub

        Public Overloads Shared Function ToString(ByVal sM(,) As Single) As String
            Dim strText As String = vbNewLine
            Dim i, j, iHiRow, iHiCol As Integer
            GetBounds(sM, iHiRow, iHiCol)
            For i = 0 To iHiRow
                For j = 0 To iHiCol - 1
                    strText = strText & sM(i, j).ToString & " "
                Next j
                strText = strText & sM(i, iHiCol).ToString & vbNewLine
            Next i
            Return strText
        End Function

        Public Overloads Shared Function ToString(ByVal sV() As Single) As String
            Dim strText As String = ""
            Dim i, iHiCol As Integer
            iHiCol = sV.GetUpperBound(0)
            For i = 0 To iHiCol - 1
                strText = strText & sV(i).ToString & " "
            Next i
            strText = vbNewLine & strText & sV(iHiCol).ToString
            Return strText
        End Function
    End Class


End Namespace



