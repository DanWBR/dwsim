Module Extensions

    <System.Runtime.CompilerServices.Extension()>
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
                retstr += d.ToString("G6", ci) + "; "
            Next
            retstr = retstr.TrimEnd(New Char() {";"c, " "c})
            retstr += "}"

            Return retstr

        ElseIf vector.Length > 0 Then

            Return vector(0).ToString("G6", ci)

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

        Dim retstr As String = ""
        For Each s In vector
            retstr += s + ", "
        Next
        retstr = retstr.Trim().TrimEnd(",")

        Return retstr

    End Function

    <System.Runtime.CompilerServices.Extension()> _
    Public Function ToArrayString(vector As Object()) As String

        Dim retstr As String = ""
        For Each d In vector
            retstr += d.ToString + ", "
        Next
        retstr = retstr.Trim().TrimEnd(",")

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
