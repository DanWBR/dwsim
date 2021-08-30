Imports System.IO
Imports System.Reflection

Public Class FormConfigCoolPropIncompMixture

    Public pp As PropertyPackages.CoolPropIncompressibleMixturePropertyPackage

    Private fluidlist As New Dictionary(Of String, String)

    Private Sub FormConfigCoolPropIncompMixture_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        Dim contents As String = ""
        Using filestr As Stream = Assembly.GetAssembly(Me.GetType).GetManifestResourceStream("DWSIM.Thermodynamics.CoolPropIncompMixtures.txt")
            Using t As New StreamReader(filestr)
                contents = t.ReadToEnd()
            End Using
        End Using

        For Each l As String In contents.Split(New Char() {vbLf, vbCr, vbCrLf})
            If l <> "" Then fluidlist.Add(l.Split(vbTab)(0), l.Split(vbTab)(0) & " (" & l.Split(vbTab)(1) & ")")
        Next

        For Each item In fluidlist
            cbSoluteCP.Items.Add(item.Value)
        Next

        Try
            cbSoluteCP.SelectedItem = fluidlist(pp.SoluteName)
        Catch ex As Exception

        End Try

        For Each item In pp.Flowsheet.SelectedCompounds.Keys
            cbSoluteDWSIM.Items.Add(item)
        Next

        Try
            cbSoluteDWSIM.SelectedItem = pp.SoluteCompound
        Catch ex As Exception

        End Try

        For Each item In pp.Flowsheet.SelectedCompounds.Keys
            cbSolvent.Items.Add(item)
        Next

        Try
            cbSolvent.SelectedItem = pp.SolventCompound
        Catch ex As Exception

        End Try

    End Sub

    Private Sub cbSolvent_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbSolvent.SelectedIndexChanged
        pp.SolventCompound = cbSolvent.SelectedItem
    End Sub

    Private Sub cbSoluteDWSIM_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbSoluteDWSIM.SelectedIndexChanged
        pp.SoluteCompound = cbSoluteDWSIM.SelectedItem
    End Sub

    Private Sub cbSoluteCP_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbSoluteCP.SelectedIndexChanged
        pp.SoluteName = fluidlist.Where(Function(x) x.Value = cbSoluteCP.SelectedItem).FirstOrDefault.Key
    End Sub

End Class