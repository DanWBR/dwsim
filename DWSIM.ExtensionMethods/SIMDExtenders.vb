Imports DWSIM.GlobalSettings
Imports DWSIM.MathOps

Public Module SIMDExtenders

    ''' <summary>
    ''' Computes the exponential of each vector element.
    ''' </summary>
    ''' <param name="vector"></param>
    ''' <returns></returns>
    ''' <remarks></remarks>
    <System.Runtime.CompilerServices.Extension()> Public Function ExpY(vector As Double()) As Double()

        Dim vector2(vector.Length - 1) As Double

        If Settings.UseSIMDExtensions Then
            Yeppp.Math.Exp_V64f_V64f(vector, 0, vector2, 0, vector.Length)
        Else
            For i As Integer = 0 To vector.Length - 1
                vector2(i) = Math.Exp(vector(i))
            Next
        End If

        Return vector2

    End Function

    ''' <summary>
    ''' Computes the exponent of each vector element.
    ''' </summary>
    ''' <param name="vector"></param>
    ''' <returns></returns>
    ''' <remarks></remarks>
    <System.Runtime.CompilerServices.Extension()> Public Function PowY(vector As Double(), exponent As Double) As Double()

        Dim vector2(vector.Length - 1) As Double

        For i As Integer = 0 To vector.Length - 1
            vector2(i) = Math.Pow(vector(i), exponent)
        Next

        Return vector2

    End Function

    ''' <summary>
    ''' Computes the natural logarithm of each vector element.
    ''' </summary>
    ''' <param name="vector"></param>
    ''' <returns></returns>
    ''' <remarks></remarks>
    <System.Runtime.CompilerServices.Extension()> Public Function LogY(vector As Double()) As Double()

        Dim vector2(vector.Length - 1) As Double

        If Settings.UseSIMDExtensions Then
            Yeppp.Math.Log_V64f_V64f(vector, 0, vector2, 0, vector.Length)
        Else
            For i As Integer = 0 To vector.Length - 1
                vector2(i) = Math.Log(vector(i))
            Next
        End If

        Return vector2

    End Function

    ''' <summary>
    ''' Computes the absolute value of each vector element.
    ''' </summary>
    ''' <param name="vector"></param>
    ''' <returns></returns>
    ''' <remarks></remarks>
    <System.Runtime.CompilerServices.Extension()> Public Function AbsY(vector As Double()) As Double()

        Dim vector2(vector.Length - 1) As Double

        For i As Integer = 0 To vector.Length - 1
            vector2(i) = Math.Abs(vector(i))
        Next

        Return vector2

    End Function

    ''' <summary>
    ''' Returns the smallest element in the vector.
    ''' </summary>
    ''' <param name="vector"></param>
    ''' <returns></returns>
    ''' <remarks></remarks>
    <System.Runtime.CompilerServices.Extension()> Public Function MinY(vector As Double()) As Double

        If Settings.UseSIMDExtensions Then
            Return Yeppp.Core.Min_V64f_S64f(vector, 0, vector.Length)
        Else
            Return vector.Min
        End If

    End Function


    ''' <summary>
    ''' Returns the smallest non-zero element in the vector.
    ''' </summary>
    ''' <param name="vector"></param>
    ''' <returns></returns>
    ''' <remarks></remarks>
    <System.Runtime.CompilerServices.Extension()> Public Function MinY_NonZero(vector As Double()) As Double

        Dim vector2 = vector.Where(Function(x) x > 0).ToArray()

        Return vector2.Min

    End Function

    ''' <summary>
    ''' Returns the biggest element in the vector.
    ''' </summary>
    ''' <param name="vector"></param>
    ''' <returns></returns>
    ''' <remarks></remarks>
    <System.Runtime.CompilerServices.Extension()> Public Function MaxY(vector As Double()) As Double

        If Settings.UseSIMDExtensions Then
            Return Yeppp.Core.Max_V64f_S64f(vector, 0, vector.Length)
        Else
            Return vector.Max
        End If

    End Function

    <System.Runtime.CompilerServices.Extension()> Public Function MaxY_NonZero(vector As Double(), refvec As Double()) As Double

        Dim mult = vector.MultiplyY(refvec)
        Dim r0 = refvec.Where(Function(x) x > 0).ToArray()

        Dim vector2 = mult.Where(Function(x) x > 0).ToArray()

        Dim val = vector2.Max
        Dim m0 = r0.ToList()(vector2.ToList().IndexOf(val))

        Return val / m0

    End Function

    <System.Runtime.CompilerServices.Extension()> Public Function MinY_NonZero(vector As Double(), refvec As Double()) As Double

        Dim mult = vector.MultiplyY(refvec)
        Dim r0 = refvec.Where(Function(x) x > 0).ToArray()

        Dim vector2 = mult.Where(Function(x) x > 0).ToArray()

        Dim val = vector2.Min
        Dim m0 = r0.ToList()(vector2.ToList().IndexOf(val))

        Return val / m0

    End Function

    ''' <summary>
    ''' Sum of the vector elements.
    ''' </summary>
    ''' <param name="vector"></param>
    ''' <returns></returns>
    ''' <remarks></remarks>
    <System.Runtime.CompilerServices.Extension()> Public Function SumY(vector As Double()) As Double

        If Settings.UseSIMDExtensions Then
            Return Yeppp.Core.Sum_V64f_S64f(vector, 0, vector.Length)
        Else
            Return vector.Sum
        End If

    End Function

    ''' <summary>
    ''' Absolute sum of the vector elements
    ''' </summary>
    ''' <param name="vector"></param>
    ''' <returns></returns>
    ''' <remarks></remarks>
    <System.Runtime.CompilerServices.Extension()> Public Function AbsSumY(vector As Double()) As Double

        If Settings.UseSIMDExtensions Then
            Return Yeppp.Core.SumAbs_V64f_S64f(vector, 0, vector.Length)
        Else
            Return MathEx.Common.AbsSum(vector)
        End If

    End Function

    ''' <summary>
    ''' Absolute square sum of vector elements.
    ''' </summary>
    ''' <param name="vector"></param>
    ''' <returns></returns>
    ''' <remarks></remarks>
    <System.Runtime.CompilerServices.Extension()> Public Function AbsSqrSumY(vector As Double()) As Double

        If Settings.UseSIMDExtensions Then
            Return Yeppp.Core.SumSquares_V64f_S64f(vector, 0, vector.Length)
        Else
            Return MathEx.Common.SumSqr(vector)
        End If

    End Function

    ''' <summary>
    ''' Negates the elements of a vector.
    ''' </summary>
    ''' <param name="vector"></param>
    ''' <returns></returns>
    ''' <remarks></remarks>
    <System.Runtime.CompilerServices.Extension()> Public Function NegateY(vector As Double()) As Double()

        Dim vector0(vector.Length - 1) As Double

        If Settings.UseSIMDExtensions Then
            Yeppp.Core.Negate_V64f_V64f(vector, 0, vector0, 0, vector.Length)
        Else
            For i As Integer = 0 To vector.Length - 1
                vector0(i) = -vector(i)
            Next
        End If

        Return vector0

    End Function

    ''' <summary>
    ''' Multiplies vector elements.
    ''' </summary>
    ''' <param name="vector1"></param>
    ''' <param name="vector2"></param>
    ''' <returns></returns>
    ''' <remarks></remarks>
    <System.Runtime.CompilerServices.Extension()> Public Function MultiplyY(vector1 As Double(), vector2 As Double()) As Double()

        Dim vector0(vector1.Length - 1) As Double

        If Settings.UseSIMDExtensions Then
            Yeppp.Core.Multiply_V64fV64f_V64f(vector1, 0, vector2, 0, vector0, 0, vector1.Length)
        Else
            For i As Integer = 0 To vector1.Length - 1
                vector0(i) = vector1(i) * vector2(i)
            Next
        End If

        Return vector0

    End Function

    ''' <summary>
    ''' Divides vector elements.
    ''' </summary>
    ''' <param name="vector"></param>
    ''' <param name="vector2"></param>
    ''' <returns></returns>
    ''' <remarks></remarks>
    <System.Runtime.CompilerServices.Extension()> Public Function DivideY(vector As Double(), vector2 As Double()) As Double()

        Dim vector0(vector.Length - 1) As Double

        If Settings.UseSIMDExtensions Then
            Dim invvector2(vector.Length - 1) As Double
            For i As Integer = 0 To vector2.Length - 1
                invvector2(i) = 1 / vector2(i)
            Next
            Yeppp.Core.Multiply_V64fV64f_V64f(vector, 0, invvector2, 0, vector0, 0, vector.Length)
        Else
            For i As Integer = 0 To vector.Length - 1
                vector0(i) = vector(i) / vector2(i)
            Next
        End If


        Return vector0

    End Function

    ''' <summary>
    ''' Subtracts vector elements.
    ''' </summary>
    ''' <param name="vector"></param>
    ''' <param name="vector2"></param>
    ''' <returns></returns>
    ''' <remarks></remarks>
    <System.Runtime.CompilerServices.Extension()> Public Function SubtractY(vector As Double(), vector2 As Double()) As Double()

        Dim vector0(vector.Length - 1) As Double

        If Settings.UseSIMDExtensions Then
            Yeppp.Core.Subtract_V64fV64f_V64f(vector, 0, vector2, 0, vector0, 0, vector.Length)
        Else
            For i As Integer = 0 To vector.Length - 1
                vector0(i) = vector(i) - vector2(i)
            Next
        End If

        Return vector0

    End Function

    <System.Runtime.CompilerServices.Extension()> Public Function SubtractInversesY(vector As Double(), vector2 As Double()) As Double()

        Dim vector0(vector.Length - 1) As Double
        Dim invvector1(vector.Length - 1), invvector2(vector.Length - 1) As Double

        For i As Integer = 0 To vector.Length - 1
            invvector1(i) = 1 / vector(i)
            invvector2(i) = 1 / vector2(i)
        Next

        If Settings.UseSIMDExtensions Then
            Yeppp.Core.Subtract_V64fV64f_V64f(invvector1, 0, invvector2, 0, vector0, 0, vector.Length)
        Else
            For i As Integer = 0 To vector.Length - 1
                vector0(i) = invvector1(i) - invvector2(i)
            Next
        End If

        Return vector0

    End Function

    <System.Runtime.CompilerServices.Extension()> Public Function SubtractInverseY(vector As Double(), vector2 As Double()) As Double()

        Dim vector0(vector.Length - 1) As Double

        If Settings.UseSIMDExtensions Then
            Dim invvector2(vector.Length - 1) As Double
            For i As Integer = 0 To vector.Length - 1
                invvector2(i) = 1 / vector2(i)
            Next
            Yeppp.Core.Subtract_V64fV64f_V64f(vector, 0, invvector2, 0, vector0, 0, vector.Length)
        Else
            For i As Integer = 0 To vector.Length - 1
                vector0(i) = vector(i) - 1 / vector2(i)
            Next
        End If

        Return vector0

    End Function

    ''' <summary>
    ''' Multiplies vector elements by a constant.
    ''' </summary>
    ''' <param name="vector"></param>
    ''' <returns></returns>
    ''' <remarks></remarks>
    <System.Runtime.CompilerServices.Extension()> Public Function MultiplyConstY(vector As Double(), constant As Double) As Double()

        Dim vector0(vector.Length - 1) As Double

        If Settings.UseSIMDExtensions Then
            Yeppp.Core.Multiply_V64fS64f_V64f(vector, 0, constant, vector0, 0, vector.Length)
        Else
            For i As Integer = 0 To vector.Length - 1
                vector0(i) = vector(i) * constant
            Next
        End If


        Return vector0

    End Function

    ''' <summary>
    ''' Normalizes a vector (sum = 1).
    ''' </summary>
    ''' <param name="vector"></param>
    ''' <returns></returns>
    ''' <remarks></remarks>
    <System.Runtime.CompilerServices.Extension()> Public Function NormalizeY(vector As Double()) As Double()

        Dim vector0(vector.Length - 1) As Double
        Dim sum As Double = vector.SumY

        If sum = 0.0# Then sum = 1.0# 'to handle null vectors

        If Settings.UseSIMDExtensions Then
            Yeppp.Core.Multiply_V64fS64f_V64f(vector, 0, 1 / sum, vector0, 0, vector.Length)
        Else
            For i As Integer = 0 To vector.Length - 1
                vector0(i) = vector(i) / sum
            Next
        End If

        Return vector0

    End Function

    ''' <summary>
    ''' Adds vector elements.
    ''' </summary>
    ''' <param name="vector"></param>
    ''' <param name="vector2"></param>
    ''' <returns></returns>
    ''' <remarks></remarks>
    <System.Runtime.CompilerServices.Extension()> Public Function AddY(vector As Double(), vector2 As Double()) As Double()

        Dim vector0 As Double() = vector.Clone()

        If Settings.UseSIMDExtensions Then
            Yeppp.Core.Add_IV64fV64f_IV64f(vector0, 0, vector2, 0, vector.Length)
        Else
            For i As Integer = 0 To vector.Length - 1
                vector0(i) = vector(i) + vector2(i)
            Next
        End If

        Return vector0

    End Function

    ''' <summary>
    ''' Adds a constant value to vector elements.
    ''' </summary>
    ''' <param name="vector"></param>
    ''' <returns></returns>
    ''' <remarks></remarks>
    <System.Runtime.CompilerServices.Extension()> Public Function AddConstY(vector As Double(), constant As Double) As Double()

        Dim vector0 As Double() = vector.Clone()

        If Settings.UseSIMDExtensions Then
            Yeppp.Core.Add_V64fS64f_V64f(vector, 0, constant, vector0, 0, vector.Length)
        Else
            For i As Integer = 0 To vector.Length - 1
                vector0(i) = vector(i) + constant
            Next
        End If

        Return vector0

    End Function

    ''' <summary>
    ''' Replace NaNs and Infinities with zero.
    ''' </summary>
    ''' <param name="vector"></param>
    ''' <returns></returns>
    ''' <remarks></remarks>
    <System.Runtime.CompilerServices.Extension()> Public Function ReplaceInvalidsWithZeroes(vector As Double()) As Double()

        Dim vector0 As Double() = vector.Clone()

        For i As Integer = 0 To vector.Length - 1
            If Double.IsNaN(vector(i)) Or Double.IsInfinity(vector(i)) Then
                vector0(i) = 0.0
            End If
        Next

        Return vector0

    End Function

    <System.Runtime.CompilerServices.Extension()> Public Function HasNaN(vector As Double()) As Double

        Return vector.Any(Function(v) Double.IsNaN(v))

    End Function

    <System.Runtime.CompilerServices.Extension()> Public Function HasInf(vector As Double()) As Double

        Return vector.Any(Function(v) Double.IsInfinity(v))

    End Function


    <System.Runtime.CompilerServices.Extension()> Public Function HasNegative(vector As Double()) As Double

        Return vector.Any(Function(v) v < 0.0)

    End Function

End Module
