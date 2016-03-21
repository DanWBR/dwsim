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
            With fh1
                csidc = .ReadFile(My.Application.Info.DirectoryPath & pathsep & "data" & pathsep & "csid.dat")
            End With

            For Each csid In csidc
                Me.IDs.Add(csid.ID, csid.Clone)
            Next

            csid = Nothing
            csidc = Nothing
            fh1 = Nothing

        End Sub

        Public Function GetCSName(ByVal id As String)

            If Me.IDs.ContainsKey(id) Then
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
