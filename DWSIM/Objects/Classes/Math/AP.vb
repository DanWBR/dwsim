Namespace DWSIM.MathEx.AP

    <Runtime.InteropServices.StructLayout(Runtime.InteropServices.LayoutKind.Sequential)> _
    Public Structure Complex
        Public x As Double
        Public y As Double
        Public Sub New(ByVal _x As Double)
            Me.x = _x
            Me.y = 0
        End Sub

        Public Sub New(ByVal _x As Double, ByVal _y As Double)
            Me.x = _x
            Me.y = _y
        End Sub

        Public Shared Widening Operator CType(ByVal _x As Double) As Complex
            Return New Complex(_x)
        End Operator

        Public Shared Operator =(ByVal lhs As Complex, ByVal rhs As Complex) As Boolean
            Return ((lhs.x = rhs.x) And (lhs.y = rhs.y))
        End Operator

        Public Shared Operator <>(ByVal lhs As Complex, ByVal rhs As Complex) As Boolean
            Return ((lhs.x <> rhs.x) Or (lhs.y <> rhs.y))
        End Operator

        Public Shared Operator +(ByVal lhs As Complex) As Complex
            Return lhs
        End Operator

        Public Shared Operator -(ByVal lhs As Complex) As Complex
            Return New Complex(-lhs.x, -lhs.y)
        End Operator

        Public Shared Operator +(ByVal lhs As Complex, ByVal rhs As Complex) As Complex
            Return New Complex((lhs.x + rhs.x), (lhs.y + rhs.y))
        End Operator

        Public Shared Operator -(ByVal lhs As Complex, ByVal rhs As Complex) As Complex
            Return New Complex((lhs.x - rhs.x), (lhs.y - rhs.y))
        End Operator

        Public Shared Operator *(ByVal lhs As Complex, ByVal rhs As Complex) As Complex
            Return New Complex(((lhs.x * rhs.x) - (lhs.y * rhs.y)), ((lhs.x * rhs.y) + (lhs.y * rhs.x)))
        End Operator

        Public Shared Operator /(ByVal lhs As Complex, ByVal rhs As Complex) As Complex
            Dim complex As Complex
            Dim num As Double
            Dim num2 As Double
            If (Math.Abs(rhs.y) < Math.Abs(rhs.x)) Then
                num = (rhs.y / rhs.x)
                num2 = (rhs.x + (rhs.y * num))
                complex.x = ((lhs.x + (lhs.y * num)) / num2)
                complex.y = ((lhs.y - (lhs.x * num)) / num2)
                Return complex
            End If
            num = (rhs.x / rhs.y)
            num2 = (rhs.y + (rhs.x * num))
            complex.x = ((lhs.y + (lhs.x * num)) / num2)
            complex.y = ((-lhs.x + (lhs.y * num)) / num2)
            Return complex
        End Operator
    End Structure

    Public Class MathEx
        ' Methods
        Public Shared Function AbsComplex(ByVal z As Complex) As Double
            Dim num2 As Double = Math.Abs(z.x)
            Dim num3 As Double = Math.Abs(z.y)
            Dim num As Double = IIf((num2 > num3), num2, num3)
            Dim num4 As Double = IIf((num2 < num3), num2, num3)
            If (num4 = 0) Then
                Return num
            End If
            Dim num5 As Double = (num4 / num)
            Return (num * Math.Sqrt((1 + (num5 * num5))))
        End Function

        Public Shared Function Conj(ByVal z As Complex) As Complex
            Return New Complex(z.x, -z.y)
        End Function

        Public Shared Function CSqr(ByVal z As Complex) As Complex
            Return New Complex(((z.x * z.x) - (z.y * z.y)), ((2 * z.x) * z.y))
        End Function

        Public Shared Function RandomInteger(ByVal N As Integer) As Integer
            Return MathEx.RndObject.Next(N)
        End Function

        Public Shared Function RandomReal() As Double
            Return MathEx.RndObject.NextDouble
        End Function

        Public Shared Function Sqr(ByVal X As Double) As Double
            Return (X * X)
        End Function

        ' Fields
        Public Const MachineEpsilon As Double = 0.0000000000000005
        Public Const MaxRealNumber As Double = 1.0E+300
        Public Const MinRealNumber As Double = 1.0E-300
        Private Shared RndObject As Random = New Random
    End Class

End Namespace


