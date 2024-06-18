Namespace MathEx

    Public Class LMFit

        Public Enum FitType
            SecondDegreePoly = 0
            ThirdDegreePoly = 1
            FourthDegreePoly = 2
            Linear = 3
            FifthDegreePoly = 4
            SixthDegreePoly = 5
            Pvap = 6
            Cp = 7
            LiqVisc = 8
            HVap = 9
            LiqDens = 10
        End Enum

        Private _x, _y As Double()
        Private sum As Double
        Private its As Integer = 0

        Public Function GetCoeffs(ByVal x As Double(), ByVal y As Double(), ByVal inest As Double(), ByVal fittype As FitType,
                                ByVal epsg As Double, ByVal epsf As Double, ByVal epsx As Double, ByVal maxits As Integer) As Tuple(Of Double(), String, Double, Integer)

            Dim lmsolve As New MathEx.LM.levenbergmarquardt
            Select Case fittype
                Case LMFit.FitType.SecondDegreePoly
                    lmsolve.DefineFuncGradDelegate(AddressOf fvsdp)
                Case LMFit.FitType.ThirdDegreePoly
                    lmsolve.DefineFuncGradDelegate(AddressOf fvstp)
                Case LMFit.FitType.FourthDegreePoly
                    lmsolve.DefineFuncGradDelegate(AddressOf fvftp)
                Case LMFit.FitType.Linear
                    lmsolve.DefineFuncGradDelegate(AddressOf fvlin)
                Case LMFit.FitType.FifthDegreePoly
                    lmsolve.DefineFuncGradDelegate(AddressOf fvfdp)
                Case LMFit.FitType.SixthDegreePoly
                    lmsolve.DefineFuncGradDelegate(AddressOf fvxdp)
                Case LMFit.FitType.Pvap
                    lmsolve.DefineFuncGradDelegate(AddressOf fvpvap)
                Case LMFit.FitType.Cp
                    lmsolve.DefineFuncGradDelegate(AddressOf fvcp)
                Case LMFit.FitType.LiqVisc
                    lmsolve.DefineFuncGradDelegate(AddressOf fvlvisc)
                Case LMFit.FitType.HVap
                    lmsolve.DefineFuncGradDelegate(AddressOf fvhvap)
                Case LMFit.FitType.LiqDens
                    lmsolve.DefineFuncGradDelegate(AddressOf fvliqdens)
            End Select

            Dim newc(UBound(inest) + 1) As Double
            Dim i As Integer = 1
            Do
                newc(i) = inest(i - 1)
                i = i + 1
            Loop Until i = UBound(inest) + 2

            Me._x = x
            Me._y = y

            Dim info As Integer = 56

            its = 0
            lmsolve.levenbergmarquardtminimize(inest.Length, _x.Length, newc, epsg, epsf, epsx, maxits, info)

            Dim coeffs(UBound(inest)) As Double

            i = 0
            Do
                coeffs(i) = newc(i + 1)
                i = i + 1
            Loop Until i = UBound(inest) + 1

            Return New Tuple(Of Double(), String, Double, Integer)(coeffs, GetInfo(info), sum, its)

        End Function

        Private Function GetInfo(code As Integer)

            Select Case code
                Case -1
                    Return "Wrong parameters were specified"
                Case 0
                    Return "Interrupted by user"
                Case 1
                    Return "Relative decrease of sum of function values squares (real and predicted on the base  of extrapolation) is less or equal EpsF"
                Case 2
                    Return "Relative change of solution Is less Or equal EpsX."
                Case 3
                    Return "Conditions (1) And (2) are fulfilled."
                Case 4
                    Return "Cosine of the angle between vector of function values and each of the Jacobian columns is less or equal EpsG by absolute value."
                Case 5
                    Return "Number of iterations exceeds MaxIts."
                Case 6
                    Return "EpsF Is too small. It is impossible to get a better result."
                Case 7
                    Return "EpsX Is too small. It Is impossible to get a better result."
                Case 8
                    Return "EpsG Is too small. Vector of functions is orthogonal to Jacobian columns with near-machine precision."
                Case Else
                    Return ""
            End Select

        End Function

        Public Sub fvsdp(ByRef x As Double(), ByRef fvec As Double(), ByRef fjac As Double(,), ByRef iflag As Integer)

            If Double.IsNaN(x(1)) Or Double.IsNegativeInfinity(x(1)) Or Double.IsPositiveInfinity(x(1)) Then iflag = -1
            If Double.IsNaN(fvec(1)) Or Double.IsNegativeInfinity(fvec(1)) Or Double.IsPositiveInfinity(fvec(1)) Then iflag = -1

            'A + B * T + C * T ^ 2
            Dim i As Integer
            If iflag = 1 Then
                sum = 0.0#
                i = 1
                Do
                    fvec(i) = -_y(i - 1) + (x(1) + x(2) * _x(i - 1) + x(3) * _x(i - 1) ^ 2)
                    sum += (fvec(i)) ^ 2
                    i = i + 1
                Loop Until i = UBound(_y) + 2
            ElseIf iflag = 2 Then
                i = 1
                Do
                    'A + B * T + C * T ^ 2
                    fjac(i, 1) = 1
                    fjac(i, 2) = _x(i - 1)
                    fjac(i, 3) = _x(i - 1) ^ 2
                    i = i + 1
                Loop Until i = UBound(_y) + 2
            End If

            its += 1

        End Sub

        Public Sub fvstp(ByRef x As Double(), ByRef fvec As Double(), ByRef fjac As Double(,), ByRef iflag As Integer)

            If Double.IsNaN(x(1)) Or Double.IsNegativeInfinity(x(1)) Or Double.IsPositiveInfinity(x(1)) Then iflag = -1
            If Double.IsNaN(fvec(1)) Or Double.IsNegativeInfinity(fvec(1)) Or Double.IsPositiveInfinity(fvec(1)) Then iflag = -1

            'A + B * T + C * T ^ 2 + D * T ^ 3
            Dim i As Integer
            If iflag = 1 Then
                sum = 0.0#
                i = 1
                Do
                    fvec(i) = -_y(i - 1) + (x(1) + x(2) * _x(i - 1) + x(3) * _x(i - 1) ^ 2 + x(4) * _x(i - 1) ^ 3)
                    sum += (fvec(i)) ^ 2
                    i = i + 1
                Loop Until i = UBound(_y) + 2
            ElseIf iflag = 2 Then
                i = 1
                Do
                    'A + B * T + C * T ^ 2 + D * T ^ 3
                    fjac(i, 1) = 1
                    fjac(i, 2) = _x(i - 1)
                    fjac(i, 3) = _x(i - 1) ^ 2
                    fjac(i, 4) = _x(i - 1) ^ 3
                    i = i + 1
                Loop Until i = UBound(_y) + 2
            End If

            its += 1

        End Sub

        Public Sub fvftp(ByRef x As Double(), ByRef fvec As Double(), ByRef fjac As Double(,), ByRef iflag As Integer)

            If Double.IsNaN(x(1)) Or Double.IsNegativeInfinity(x(1)) Or Double.IsPositiveInfinity(x(1)) Then iflag = -1
            If Double.IsNaN(fvec(1)) Or Double.IsNegativeInfinity(fvec(1)) Or Double.IsPositiveInfinity(fvec(1)) Then iflag = -1

            'A + B * T + C * T ^ 2 + D * T ^ 3 + E * T ^ 4
            Dim i As Integer
            If iflag = 1 Then
                sum = 0.0#
                i = 1
                Do
                    fvec(i) = -_y(i - 1) + (x(1) + x(2) * _x(i - 1) + x(3) * _x(i - 1) ^ 2 + x(4) * _x(i - 1) ^ 3 + x(5) * _x(i - 1) ^ 4)
                    sum += (fvec(i)) ^ 2
                    i = i + 1
                Loop Until i = UBound(_y) + 2
            ElseIf iflag = 2 Then
                i = 1
                Do
                    'A + B * T + C * T ^ 2 + D * T ^ 3 + E * T ^ 4
                    fjac(i, 1) = 1
                    fjac(i, 2) = _x(i - 1)
                    fjac(i, 3) = _x(i - 1) ^ 2
                    fjac(i, 4) = _x(i - 1) ^ 3
                    fjac(i, 5) = _x(i - 1) ^ 4
                    i = i + 1
                Loop Until i = UBound(_y) + 2
            End If

            its += 1

        End Sub

        Public Sub fvfdp(ByRef x As Double(), ByRef fvec As Double(), ByRef fjac As Double(,), ByRef iflag As Integer)

            If Double.IsNaN(x(1)) Or Double.IsNegativeInfinity(x(1)) Or Double.IsPositiveInfinity(x(1)) Then iflag = -1
            If Double.IsNaN(fvec(1)) Or Double.IsNegativeInfinity(fvec(1)) Or Double.IsPositiveInfinity(fvec(1)) Then iflag = -1

            'A + B * T + C * T ^ 2 + D * T ^ 3 + E * T ^ 4 + F * T ^ 5
            Dim i As Integer
            If iflag = 1 Then
                sum = 0.0#
                i = 1
                Do
                    fvec(i) = -_y(i - 1) + (x(1) + x(2) * _x(i - 1) + x(3) * _x(i - 1) ^ 2 + x(4) * _x(i - 1) ^ 3 + x(5) * _x(i - 1) ^ 4 + x(6) * _x(i - 1) ^ 5)
                    sum += (fvec(i)) ^ 2
                    i = i + 1
                Loop Until i = UBound(_y) + 2
            ElseIf iflag = 2 Then
                i = 1
                Do
                    'A + B * T + C * T ^ 2 + D * T ^ 3 + E * T ^ 4 + F * T ^ 5
                    fjac(i, 1) = 1
                    fjac(i, 2) = _x(i - 1)
                    fjac(i, 3) = _x(i - 1) ^ 2
                    fjac(i, 4) = _x(i - 1) ^ 3
                    fjac(i, 5) = _x(i - 1) ^ 4
                    fjac(i, 6) = _x(i - 1) ^ 5
                    i = i + 1
                Loop Until i = UBound(_y) + 2
            End If

            its += 1

        End Sub

        Public Sub fvxdp(ByRef x As Double(), ByRef fvec As Double(), ByRef fjac As Double(,), ByRef iflag As Integer)

            If Double.IsNaN(x(1)) Or Double.IsNegativeInfinity(x(1)) Or Double.IsPositiveInfinity(x(1)) Then iflag = -1
            If Double.IsNaN(fvec(1)) Or Double.IsNegativeInfinity(fvec(1)) Or Double.IsPositiveInfinity(fvec(1)) Then iflag = -1

            'A + B * T + C * T ^ 2 + D * T ^ 3 + E * T ^ 4 + F * T ^ 5 + G * T ^ 6
            Dim i As Integer
            If iflag = 1 Then
                sum = 0.0#
                i = 1
                Do
                    fvec(i) = -_y(i - 1) + (x(1) + x(2) * _x(i - 1) + x(3) * _x(i - 1) ^ 2 + x(4) * _x(i - 1) ^ 3 + x(5) * _x(i - 1) ^ 4 + x(6) * _x(i - 1) ^ 5 + x(7) * _x(i - 1) ^ 6)
                    sum += (fvec(i)) ^ 2
                    i = i + 1
                Loop Until i = UBound(_y) + 2
            ElseIf iflag = 2 Then
                i = 1
                Do
                    'A + B * T + C * T ^ 2 + D * T ^ 3 + E * T ^ 4 + F * T ^ 5 + G * T ^ 6
                    fjac(i, 1) = 1
                    fjac(i, 2) = _x(i - 1)
                    fjac(i, 3) = _x(i - 1) ^ 2
                    fjac(i, 4) = _x(i - 1) ^ 3
                    fjac(i, 5) = _x(i - 1) ^ 4
                    fjac(i, 6) = _x(i - 1) ^ 5
                    fjac(i, 7) = _x(i - 1) ^ 6
                    i = i + 1
                Loop Until i = UBound(_y) + 2
            End If

            its += 1

        End Sub

        Public Sub fvlin(ByRef x As Double(), ByRef fvec As Double(), ByRef fjac As Double(,), ByRef iflag As Integer)

            If Double.IsNaN(x(1)) Or Double.IsNegativeInfinity(x(1)) Or Double.IsPositiveInfinity(x(1)) Then iflag = -1
            If Double.IsNaN(fvec(1)) Or Double.IsNegativeInfinity(fvec(1)) Or Double.IsPositiveInfinity(fvec(1)) Then iflag = -1

            'A + B * T
            Dim i As Integer
            If iflag = 1 Then
                sum = 0.0#
                i = 1
                Do
                    fvec(i) = -_y(i - 1) + (x(1) + x(2) * _x(i - 1))
                    sum += (fvec(i)) ^ 2
                    i = i + 1
                Loop Until i = UBound(_y) + 2
            ElseIf iflag = 2 Then
                i = 1
                Do
                    'A + B * T
                    fjac(i, 1) = 1
                    fjac(i, 2) = _x(i - 1)
                    i = i + 1
                Loop Until i = UBound(_y) + 2
            End If

            its += 1

        End Sub

        Public Sub fvpvap(ByRef x As Double(), ByRef fvec As Double(), ByRef fjac As Double(,), ByRef iflag As Integer)

            If Double.IsNaN(x(1)) Or Double.IsNegativeInfinity(x(1)) Or Double.IsPositiveInfinity(x(1)) Then iflag = -1
            If Double.IsNaN(fvec(1)) Or Double.IsNegativeInfinity(fvec(1)) Or Double.IsPositiveInfinity(fvec(1)) Then iflag = -1

            sum = 0.0#
            Dim i As Integer
            If iflag = 1 Then
                i = 1
                Do
                    fvec(i) = -_y(i - 1) + (Math.Exp(x(1) + x(2) / _x(i - 1) + x(3) * Math.Log(_x(i - 1)) + x(4) * _x(i - 1) ^ x(5)))
                    sum += (fvec(i)) ^ 2
                    i = i + 1
                Loop Until i = UBound(_y) + 2
            ElseIf iflag = 2 Then
                Dim fval As Double = 0
                i = 1
                Do
                    'Math.Exp(A + B / T + C * Math.Log(T) + D * T ^ E)
                    fval = (Math.Exp(x(1) + x(2) / _x(i - 1) + x(3) * Math.Log(_x(i - 1)) + x(4) * _x(i - 1) ^ x(5)))
                    fjac(i, 1) = fval
                    fjac(i, 2) = fval * 1 / _x(i - 1)
                    fjac(i, 3) = fval * Math.Log(_x(i - 1))
                    fjac(i, 4) = fval * _x(i - 1) ^ x(5)
                    fjac(i, 5) = fval * x(5) * _x(i - 1) ^ x(5) * Math.Log(_x(i - 1))
                    i = i + 1
                Loop Until i = UBound(_y) + 2
            End If

            its += 1

        End Sub

        Public Sub fvcp(ByRef x As Double(), ByRef fvec As Double(), ByRef fjac As Double(,), ByRef iflag As Integer)

            If Double.IsNaN(x(1)) Or Double.IsNegativeInfinity(x(1)) Or Double.IsPositiveInfinity(x(1)) Then iflag = -1
            If Double.IsNaN(fvec(1)) Or Double.IsNegativeInfinity(fvec(1)) Or Double.IsPositiveInfinity(fvec(1)) Then iflag = -1

            'A + B * T + C * T ^ 2 + D * T ^ 3 + E * T ^ 4
            sum = 0.0#
            Dim i As Integer
            If iflag = 1 Then
                i = 1
                Do
                    fvec(i) = -_y(i - 1) + (x(1) + x(2) * _x(i - 1) + x(3) * _x(i - 1) ^ 2 + x(4) * _x(i - 1) ^ 3 + x(5) * _x(i - 1) ^ 4)
                    sum += (fvec(i)) ^ 2
                    i = i + 1
                Loop Until i = UBound(_y) + 2
            ElseIf iflag = 2 Then
                i = 1
                Do
                    'A + B * T + C * T ^ 2 + D * T ^ 3 + E * T ^ 4
                    fjac(i, 1) = 1
                    fjac(i, 2) = _x(i - 1)
                    fjac(i, 3) = _x(i - 1) ^ 2
                    fjac(i, 4) = _x(i - 1) ^ 3
                    fjac(i, 5) = _x(i - 1) ^ 4
                    i = i + 1
                Loop Until i = UBound(_y) + 2
            End If

            its += 1

        End Sub

        Public Sub fvlvisc(ByRef x As Double(), ByRef fvec As Double(), ByRef fjac As Double(,), ByRef iflag As Integer)

            If Double.IsNaN(x(1)) Or Double.IsNegativeInfinity(x(1)) Or Double.IsPositiveInfinity(x(1)) Then iflag = -1
            If Double.IsNaN(fvec(1)) Or Double.IsNegativeInfinity(fvec(1)) Or Double.IsPositiveInfinity(fvec(1)) Then iflag = -1

            sum = 0
            Dim i As Integer
            If iflag = 1 Then
                i = 1
                Do
                    fvec(i) = -_y(i - 1) + (Math.Exp(x(1) + x(2) / _x(i - 1) + x(3) * Math.Log(_x(i - 1)) + x(4) * _x(i - 1) ^ x(5)))
                    sum += (fvec(i)) ^ 2
                    i = i + 1
                Loop Until i = UBound(_y) + 2
            ElseIf iflag = 2 Then
                Dim fval As Double = 0
                i = 1
                Do
                    'Math.Exp(A + B / T + C * Math.Log(T) + D * T ^ E)
                    fval = (Math.Exp(x(1) + x(2) / _x(i - 1) + x(3) * Math.Log(_x(i - 1)) + x(4) * _x(i - 1) ^ x(5)))
                    fjac(i, 1) = fval
                    fjac(i, 2) = fval * 1 / _x(i - 1)
                    fjac(i, 3) = fval * Math.Log(_x(i - 1))
                    fjac(i, 4) = fval * _x(i - 1) ^ x(5)
                    fjac(i, 5) = fval * x(5) * _x(i - 1) ^ x(5) * Math.Log(_x(i - 1))
                    i = i + 1
                Loop Until i = UBound(_y) + 2
            End If

            its += 1

        End Sub

        Public Sub fvhvap(ByRef x As Double(), ByRef fvec As Double(), ByRef fjac As Double(,), ByRef iflag As Integer)

            If Double.IsNaN(x(1)) Or Double.IsNegativeInfinity(x(1)) Or Double.IsPositiveInfinity(x(1)) Then iflag = -1
            If Double.IsNaN(fvec(1)) Or Double.IsNegativeInfinity(fvec(1)) Or Double.IsPositiveInfinity(fvec(1)) Then iflag = -1

            'A * (1 - Tr) ^ (B + C * Tr + D * Tr ^ 2)
            sum = 0.0#
            Dim i As Integer
            If iflag = 1 Then
                i = 1
                Do
                    fvec(i) = -_y(i - 1) + (x(1) * (1 - _x(i - 1)) ^ (x(2) + x(3) * _x(i - 1) + x(4) * _x(i - 1) ^ 2))
                    sum += (fvec(i)) ^ 2
                    i = i + 1
                Loop Until i = UBound(_y) + 2
            ElseIf iflag = 2 Then
                i = 1
                Do
                    Dim fval As Double = 0
                    'A * (1 - Tr) ^ (B + C * Tr + D * Tr ^ 2)
                    fval = (x(1) * (1 - _x(i - 1)) ^ (x(2) + x(3) * _x(i - 1) + x(4) * _x(i - 1) ^ 2))
                    fjac(i, 1) = fval
                    fjac(i, 2) = fval
                    fjac(i, 3) = fval * _x(i - 1)
                    fjac(i, 4) = fval * _x(i - 1) ^ 2
                    i = i + 1
                Loop Until i = UBound(_y) + 2
            End If

            its += 1

        End Sub

        Public Sub fvliqdens(ByRef x As Double(), ByRef fvec As Double(), ByRef fjac As Double(,), ByRef iflag As Integer)

            If Double.IsNaN(x(1)) Or Double.IsNegativeInfinity(x(1)) Or Double.IsPositiveInfinity(x(1)) Then iflag = -1
            If Double.IsNaN(fvec(1)) Or Double.IsNegativeInfinity(fvec(1)) Or Double.IsPositiveInfinity(fvec(1)) Then iflag = -1

            'a / b^[1 + (1 - t/c)^d]
            sum = 0.0#
            Dim i As Integer
            If iflag = 1 Then
                i = 1
                Do
                    fvec(i) = -_y(i - 1) + (x(1) / x(2) ^ (1 + (1 - _x(i - 1) / x(3)) ^ x(4)))
                    sum += (fvec(i)) ^ 2
                    i = i + 1
                Loop Until i = UBound(_y) + 2
            ElseIf iflag = 2 Then
                i = 1
                Do
                    'a / b^[1 + (1 - t/c)^d]
                    fjac(i, 1) = 1 / x(2) ^ (1 + (1 - _x(i - 1) / x(3)) ^ x(4))
                    fjac(i, 2) = -(x(1) * (x(3) - _x(i - 1)) ^ x(4) + x(1) * x(3) ^ x(4)) / (x(2) ^ (((x(3) - _x(i - 1)) ^ x(4) + 2 * x(3) ^ x(4)) / x(3) ^ x(4)) * x(3) ^ x(4))
                    fjac(i, 3) = x(1) * Math.Log(x(2)) * x(4) * (x(3) - _x(i - 1)) ^ x(4) * _x(i - 1) / (x(2) ^ (((x(3) - _x(i - 1)) ^ x(4) + x(3) ^ x(4)) / x(3) ^ x(4)) * x(3) ^ (x(4) + 1) * _x(i - 1) - x(2) ^ (((x(3) - _x(i - 1)) ^ x(4) + x(3) ^ x(4)) / x(3) ^ x(4)) * x(3) ^ (x(4) + 2))
                    fjac(i, 4) = -(x(1) * Math.Log(x(2)) * Math.Log(x(3) - _x(i - 1)) - x(1) * Math.Log(x(2)) * Math.Log(x(3))) * (x(3) - _x(i - 1)) ^ x(4) / (x(2) ^ (((x(3) - _x(i - 1)) ^ x(4) + x(3) ^ x(4)) / x(3) ^ x(4)) * x(3) ^ x(4))
                    fjac(i, 5) = 0
                    i = i + 1
                Loop Until i = UBound(_y) + 2
            End If

            its += 1

        End Sub

        'Generic Function Implementation

        Private FunctionPointer As Func(Of Double(), Double, Double)

        Public Function GetCoeffs(x As Double(), y As Double(), inest As Double(),
                             epsg As Double, maxits As Integer, fp As Func(Of Double(), Double, Double)) As Object

            Dim lmsolve As New MathEx.LM.levenbergmarquardt()

            FunctionPointer = fp

            lmsolve.DefineFuncGradDelegate(AddressOf fgeneric)

            Dim newc(UBound(inest) + 1) As Double
            Dim i As Integer = 1
            Do
                newc(i) = inest(i - 1)
                i = i + 1
            Loop Until i = UBound(inest) + 2

            _x = x
            _y = y

            Dim info As Integer = 56

            its = 0
            lmsolve.levenbergmarquardtminimize(inest.Length, _x.Length, newc, epsg, epsg, epsg, maxits, info)

            Dim coeffs(UBound(inest)) As Double

            i = 0
            Do
                coeffs(i) = newc(i + 1)
                i = i + 1
            Loop Until i = UBound(inest) + 1

            Dim ycalc = _x.Select(Function(xval) FunctionPointer.Invoke(newc, xval)).ToList()

            Dim ymean = y.Sum / y.Count
            Dim SST = y.Select(Function(yval) (yval - ymean) ^ 2).Sum

            Dim errors As New List(Of Double)
            Dim errors2 As New List(Of Double)
            For i = 0 To y.Count - 1
                errors.Add((y(i) - ycalc(i)) / y(i) * 100.0)
                errors2.Add((y(i) - ycalc(i)) ^ 2)
            Next

            Dim R2 = 1.0 - errors2.Sum / SST

            Return New Object() {coeffs, info, sum, its, ycalc, errors, R2}

        End Function

        Public Sub fgeneric(ByRef x As Double(), ByRef fvec As Double(), ByRef fjac As Double(,), ByRef iflag As Integer)

            If Double.IsNaN(x(1)) Or Double.IsNegativeInfinity(x(1)) Or Double.IsPositiveInfinity(x(1)) Then iflag = -1
            If Double.IsNaN(fvec(1)) Or Double.IsNegativeInfinity(fvec(1)) Or Double.IsPositiveInfinity(fvec(1)) Then iflag = -1

            sum = 0.0#
            Dim i As Integer
            If iflag = 1 Then
                i = 1
                Do
                    fvec(i) = -_y(i - 1) + FunctionPointer.Invoke(x, _x(i - 1))
                    sum += (fvec(i)) ^ 2
                    i = i + 1
                Loop Until i = UBound(_y) + 2
            ElseIf iflag = 2 Then
                i = 1
                Do
                    Dim grad = FunctionGradient(x, _x(i - 1))
                    For j = 1 To x.Length - 1
                        fjac(i, j) = grad(j)
                    Next
                    i = i + 1
                Loop Until i = UBound(_y) + 2
            End If

            its += 1

        End Sub

        Private Function FunctionGradient(ByVal x() As Double, xval As Double) As Double()

            Dim epsilon As Double = 0.1

            Dim f1, f2 As Double
            Dim g(x.Length - 1), x1(x.Length - 1), x2(x.Length - 1) As Double
            Dim j, k As Integer

            For j = 1 To x.Length - 1
                For k = 1 To x.Length - 1
                    x1(k) = x(k)
                    x2(k) = x(k)
                Next
                If x(j) <> 0.0# Then
                    x1(j) = x(j) * (1.0# + epsilon)
                    x2(j) = x(j) * (1.0# - epsilon)
                Else
                    x1(j) = x(j) + epsilon
                    x2(j) = x(j) - epsilon
                End If
                f1 = FunctionPointer.Invoke(x1, xval)
                f2 = FunctionPointer.Invoke(x2, xval)
                g(j) = (f2 - f1) / (x2(j) - x1(j))
            Next

            Return g

        End Function


    End Class

End Namespace
