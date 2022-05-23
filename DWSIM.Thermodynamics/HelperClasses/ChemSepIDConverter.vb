Imports FileHelpers

Namespace ChemSepHelper

    <DelimitedRecord(";")> <IgnoreFirst()> <System.Serializable()> _
    Public Class ChemSepNameIDPair

        Implements ICloneable

        Public ID As Integer = -1
        Public ChemSepName As String = ""
        Public DWSIMName As String = ""

        Public Function Clone() As Object Implements System.ICloneable.Clone

            Dim newclass As New ChemSepNameIDPair
            With newclass
                .ID = Me.ID
                .ChemSepName = Me.ChemSepName
                .DWSIMName = Me.DWSIMName
            End With
            Return newclass
        End Function

    End Class

    Public Class ChemSepIDConverter

        Private _ids As System.Collections.Generic.Dictionary(Of Integer, ChemSepNameIDPair)

        Private CS_IDs As New Dictionary(Of String, String)

        Public ReadOnly Property IDs() As System.Collections.Generic.Dictionary(Of Integer, ChemSepNameIDPair)
            Get
                Return _ids
            End Get
        End Property

        Sub New()

            _ids = New System.Collections.Generic.Dictionary(Of Integer, ChemSepNameIDPair)

            Dim pathsep As Char = System.IO.Path.DirectorySeparatorChar

            Dim csid As ChemSepNameIDPair
            Dim csidc() As ChemSepNameIDPair
            Dim fh1 As New FileHelperEngine(Of ChemSepNameIDPair)
            Using filestr As IO.Stream = System.Reflection.Assembly.GetAssembly(Me.GetType).GetManifestResourceStream("DWSIM.Thermodynamics.csid.dat")
                Using t As New IO.StreamReader(filestr)
                    csidc = fh1.ReadStream(t)
                End Using
            End Using

            For Each csid In csidc
                Me.IDs.Add(csid.ID, csid.Clone)
            Next

            csid = Nothing
            csidc = Nothing
            fh1 = Nothing

            Using filestr As IO.Stream = System.Reflection.Assembly.GetAssembly(Me.GetType).GetManifestResourceStream("DWSIM.Thermodynamics.ChemSep_IDs.txt")
                Using t As New IO.StreamReader(filestr)
                    While Not t.EndOfStream
                        Dim line = t.ReadLine().Split(vbTab)
                        CS_IDs.Add(line(0), line(1))
                    End While
                End Using
            End Using

        End Sub

        Public Function GetCSName(ByVal id As String)

            If Me.CS_IDs.ContainsKey(id) Then
                Return Me.CS_IDs(id)
            ElseIf Me.IDs.ContainsKey(id) Then
                Return Me.IDs(id).ChemSepName
            Else
                Return id
            End If

        End Function

        Public Function GetDWSIMName(ByVal id As String)

            If Me.IDs.ContainsKey(id) Then
                Return Me.IDs(id).DWSIMName
            Else
                Return id
            End If

        End Function

    End Class

End Namespace
