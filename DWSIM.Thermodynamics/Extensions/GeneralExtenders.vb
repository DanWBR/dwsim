Imports System.Windows.Forms
Imports System.Globalization

Module Extensions

    <System.Runtime.CompilerServices.Extension()> _
    Public Function ToString(sourcearray As String(), ci As CultureInfo) As String

        Dim sb As String = ""

        If Not sourcearray Is Nothing Then
            If sourcearray.Length > 0 Then

                For Each obj As Object In sourcearray
                    If TypeOf obj Is Double Then
                        sb += Double.Parse(obj).ToString(ci) + ","
                    Else
                        sb += obj.ToString + ","
                    End If
                Next

                sb = sb.Remove(sb.Length - 1)

            End If
        End If

        Return sb

    End Function

    <System.Runtime.CompilerServices.Extension()> _
    Public Function ToArray(ByVal text As String, ci As CultureInfo, arraytype As Type) As Array

        If Not text Is Nothing Then
            Dim values() As String = text.Split(",")
            Dim myarr As New ArrayList

            For Each s As String In values
                If Double.TryParse(s, New Double) Then
                    myarr.Add(Double.Parse(s, ci))
                Else
                    myarr.Add(s)
                End If
            Next

            Return myarr.ToArray(arraytype)
        Else
            Return New ArrayList().ToArray(arraytype)
        End If

    End Function

    <System.Runtime.CompilerServices.Extension()> _
    Public Function GetUnits(control As System.Windows.Forms.GridItem) As String
        If control.Value.ToString().Split(" ").Length > 1 Then
            Return control.Value.ToString.Substring(control.Value.ToString.IndexOf(" "c) + 1, control.Value.ToString.Length - control.Value.ToString.IndexOf(" "c) - 1)
        Else
            Return ""
        End If
    End Function

    <System.Runtime.CompilerServices.Extension()> _
    Public Function GetValue(control As System.Windows.Forms.GridItem) As Double
        Dim istring As Object
        If control.Value.ToString().Split(" ").Length > 1 Then
            istring = control.Value.ToString().Split(" ")(0)
            If Double.TryParse(istring.ToString, New Double) Then
                Return Convert.ToDouble(istring)
            Else
                Return Double.NaN
            End If
        ElseIf control.Value.ToString().Split(" ").Length = 1 Then
            istring = control.Value
            If Double.TryParse(istring.ToString, New Double) Then
                Return Convert.ToDouble(control.Value)
            Else
                Return Double.NaN
            End If
        Else
            Return Double.NaN
        End If
    End Function

    <System.Runtime.CompilerServices.Extension()> _
    Public Function ToArrayString(vector As Double()) As String

        Dim retstr As String = "{ "
        For Each d In vector
            retstr += d.ToString + ", "
        Next
        retstr.TrimEnd(",")
        retstr += "}"

        Return retstr

    End Function

    <System.Runtime.CompilerServices.Extension()>
    Public Function ToArrayString(vector As Double(), ByVal ci As System.Globalization.CultureInfo) As String

        If vector.Length > 1 Then

            Dim retstr As String = "{"
            For Each d As Double In vector
                retstr += d.ToString(ci) + "; "
            Next
            retstr = retstr.TrimEnd(New Char() {";"c, " "c})
            retstr += "}"

            Return retstr

        ElseIf vector.Length > 0 Then

            Return vector(0).ToString(ci)

        Else

            Return ""

        End If

    End Function

    <System.Runtime.CompilerServices.Extension()>
    Public Function ToDoubleArray(text As String, ByVal ci As System.Globalization.CultureInfo) As Double()

        Dim numbers As String() = text.Trim(New Char() {"{"c, "}"c}).Split(";"c)

        Dim doubles As New List(Of Double)

        For Each n As String In numbers
            If n <> "" Then doubles.Add(Convert.ToDouble(n, ci))
        Next

        Return doubles.ToArray

    End Function

    <System.Runtime.CompilerServices.Extension()> _
    Public Function ToArrayString(vector As String()) As String

        Dim retstr As String = "{ "
        For Each s In vector
            retstr += s + ", "
        Next
        retstr.TrimEnd(",")
        retstr += "}"

        Return retstr

    End Function

    <System.Runtime.CompilerServices.Extension()> _
    Public Function ToArrayString(vector As Object()) As String

        Dim retstr As String = "{ "
        For Each d In vector
            retstr += d.ToString + ", "
        Next
        retstr.TrimEnd(",")
        retstr += "}"

        Return retstr

    End Function

    <System.Runtime.CompilerServices.Extension()> _
    Public Function ToArrayString(vector As Array) As String

        Dim retstr As String = "{ "
        For Each d In vector
            retstr += d.ToString + ", "
        Next
        retstr.TrimEnd(",")
        retstr += "}"

        Return retstr

    End Function

    <System.Runtime.CompilerServices.Extension()> _
    Public Function IsValid(d As Double) As Boolean
        If Double.IsNaN(d) Or Double.IsInfinity(d) Then Return False Else Return True
    End Function

    <System.Runtime.CompilerServices.Extension()> _
    Public Function IsValid(d As Nullable(Of Double)) As Boolean
        If Double.IsNaN(d.GetValueOrDefault) Or Double.IsInfinity(d.GetValueOrDefault) Then Return False Else Return True
    End Function

    <System.Runtime.CompilerServices.Extension()> _
    Public Function IsPositive(d As Double) As Boolean
        If d.IsValid() Then
            If d > 0.0# Then Return True Else Return False
        Else
            Throw New ArgumentException("invalid double")
        End If
    End Function

    <System.Runtime.CompilerServices.Extension()> _
    Public Function IsPositive(d As Nullable(Of Double)) As Boolean
        If d.GetValueOrDefault.IsValid() Then
            If d.GetValueOrDefault > 0.0# Then Return True Else Return False
        Else
            Throw New ArgumentException("invalid double")
        End If
    End Function

    <System.Runtime.CompilerServices.Extension()> _
    Public Function IsNegative(d As Double) As Boolean
        If d.IsValid() Then
            If d < 0.0# Then Return True Else Return False
        Else
            Throw New ArgumentException("invalid double")
        End If
    End Function

    <System.Runtime.CompilerServices.Extension()> _
    Public Function IsNegative(d As Nullable(Of Double)) As Boolean
        If d.GetValueOrDefault.IsValid() Then
            If d.GetValueOrDefault < 0.0# Then Return True Else Return False
        Else
            Throw New ArgumentException("invalid double")
        End If
    End Function

    ''' <summary>
    ''' Alternative implementation for the Exponential (Exp) function.
    ''' </summary>
    ''' <param name="val"></param>
    ''' <returns></returns>
    ''' <remarks></remarks>
    <System.Runtime.CompilerServices.Extension()> Public Function ExpY(val As Double) As Double
        Dim tmp As Long = CLng(1512775 * val + 1072632447)
        Return BitConverter.Int64BitsToDouble(tmp << 32)
    End Function


    ''' <summary>
    ''' Converts a two-dimensional array to a jagged array.
    ''' </summary>
    ''' <typeparam name="T"></typeparam>
    ''' <param name="twoDimensionalArray"></param>
    ''' <returns></returns>
    ''' <remarks></remarks>
    <System.Runtime.CompilerServices.Extension> Public Function ToJaggedArray(Of T)(twoDimensionalArray As T(,)) As T()()

        Dim rowsFirstIndex As Integer = twoDimensionalArray.GetLowerBound(0)
        Dim rowsLastIndex As Integer = twoDimensionalArray.GetUpperBound(0)
        Dim numberOfRows As Integer = rowsLastIndex + 1

        Dim columnsFirstIndex As Integer = twoDimensionalArray.GetLowerBound(1)
        Dim columnsLastIndex As Integer = twoDimensionalArray.GetUpperBound(1)
        Dim numberOfColumns As Integer = columnsLastIndex + 1

        Dim jaggedArray As T()() = New T(numberOfRows - 1)() {}
        For i As Integer = rowsFirstIndex To rowsLastIndex
            jaggedArray(i) = New T(numberOfColumns - 1) {}

            For j As Integer = columnsFirstIndex To columnsLastIndex
                jaggedArray(i)(j) = twoDimensionalArray(i, j)
            Next
        Next
        Return jaggedArray

    End Function

    ''' <summary>
    ''' Converts a jagged array to a two-dimensional array.
    ''' </summary>
    ''' <typeparam name="T"></typeparam>
    ''' <param name="jaggedArray"></param>
    ''' <returns></returns>
    ''' <remarks></remarks>
    <System.Runtime.CompilerServices.Extension> Public Function FromJaggedArray(Of T)(jaggedArray As T()()) As T(,)

        Dim rowsFirstIndex As Integer = jaggedArray.GetLowerBound(0)
        Dim rowsLastIndex As Integer = jaggedArray.GetUpperBound(0)
        Dim numberOfRows As Integer = rowsLastIndex + 1

        Dim columnsFirstIndex As Integer = jaggedArray(0).GetLowerBound(0)
        Dim columnsLastIndex As Integer = jaggedArray(0).GetUpperBound(0)
        Dim numberOfColumns As Integer = columnsLastIndex + 1

        Dim twoDimensionalArray As T(,) = New T(numberOfRows - 1, numberOfColumns - 1) {}
        For i As Integer = rowsFirstIndex To rowsLastIndex
            For j As Integer = columnsFirstIndex To columnsLastIndex
                twoDimensionalArray(i, j) = jaggedArray(i)(j)
            Next
        Next
        Return twoDimensionalArray

    End Function

End Module
