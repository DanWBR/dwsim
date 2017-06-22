Public Module ConvertExtensions

    <System.Runtime.CompilerServices.Extension()>
    Public Function ConvertFromSI(d As Double, units As String) As Double

        Return SystemsOfUnits.Converter.ConvertFromSI(units, d)

    End Function

    <System.Runtime.CompilerServices.Extension()>
    Public Function ConvertToSI(d As Double, units As String) As Double

        Return SystemsOfUnits.Converter.ConvertToSI(units, d)

    End Function

    <System.Runtime.CompilerServices.Extension()>
    Public Function ConvertUnits(d As Double, fromunits As String, tounits As String) As Double

        Return SystemsOfUnits.Converter.ConvertFromSI(tounits, SystemsOfUnits.Converter.ConvertToSI(fromunits, d))

    End Function

    <System.Runtime.CompilerServices.Extension()>
    Public Function ConvertFromSI(vector As Double(), units As String) As Double()



        Dim newvector As Double() = DirectCast(vector.Clone, Double())

        For i As Integer = 0 To vector.Length - 1
            newvector(i) = SystemsOfUnits.Converter.ConvertFromSI(units, vector(i))
        Next

        Return newvector

    End Function

    <System.Runtime.CompilerServices.Extension()>
    Public Function ConvertToSI(vector As Double(), units As String) As Double()



        Dim newvector As Double() = DirectCast(vector.Clone, Double())

        For i As Integer = 0 To vector.Length - 1
            newvector(i) = SystemsOfUnits.Converter.ConvertToSI(units, vector(i))
        Next

        Return newvector

    End Function

    <System.Runtime.CompilerServices.Extension()>
    Public Function ConvertUnits(vector As Double(), fromunits As String, tounits As String) As Double()



        Dim newvector As Double() = DirectCast(vector.Clone, Double())

        For i As Integer = 0 To vector.Length - 1
            newvector(i) = SystemsOfUnits.Converter.ConvertFromSI(tounits, SystemsOfUnits.Converter.ConvertToSI(fromunits, vector(i)))
        Next

        Return newvector

    End Function

    <System.Runtime.CompilerServices.Extension()>
    Public Function ConvertFromSI(vector As List(Of Double), units As String) As List(Of Double)

        Dim newvector As New List(Of Double)

        For i As Integer = 0 To vector.Count - 1
            newvector.Add(SystemsOfUnits.Converter.ConvertFromSI(units, vector(i)))
        Next

        Return newvector

    End Function

    <System.Runtime.CompilerServices.Extension()>
    Public Function ConvertToSI(vector As List(Of Double), units As String) As List(Of Double)



        Dim newvector As New List(Of Double)

        For i As Integer = 0 To vector.Count - 1
            newvector.Add(SystemsOfUnits.Converter.ConvertToSI(units, vector(i)))
        Next

        Return newvector

    End Function

    <System.Runtime.CompilerServices.Extension()>
    Public Function ConvertUnits(vector As List(Of Double), fromunits As String, tounits As String) As List(Of Double)



        Dim newvector As New List(Of Double)

        For i As Integer = 0 To vector.Count - 1
            newvector.Add(SystemsOfUnits.Converter.ConvertFromSI(tounits, SystemsOfUnits.Converter.ConvertToSI(fromunits, vector(i))))
        Next

        Return newvector

    End Function

End Module