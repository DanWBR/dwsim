Imports System.IO
Imports System.Reflection

Public Class FormConfigCoolPropIncompFluid

    Public pp As PropertyPackages.CoolPropIncompressiblePurePropertyPackage

    Private fluidlist As New Dictionary(Of String, String)

    Private Sub FormConfigCoolPropIncompFluid_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        ExtensionMethods.ChangeDefaultFont(Me)

        Dim contents As String = ""
        Using filestr As Stream = Assembly.GetAssembly(Me.GetType).GetManifestResourceStream("DWSIM.Thermodynamics.CoolPropIncompPure.txt")
            Using t As New StreamReader(filestr)
                contents = t.ReadToEnd()
            End Using
        End Using
        For Each l As String In contents.Split(New Char() {vbLf, vbCr, vbCrLf})
            If l <> "" Then fluidlist.Add(l.Split(vbTab)(0), l.Split(vbTab)(0) & " (" & l.Split(vbTab)(1) & ")")
        Next
        For Each item In fluidlist
            cbIncompFluid.Items.Add(item.Value)
        Next
        Try
            cbIncompFluid.SelectedItem = fluidlist(pp.FluidName)
        Catch ex As Exception

        End Try

    End Sub

    Private Sub cbIncompFluid_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbIncompFluid.SelectedIndexChanged
        pp.FluidName = fluidlist.Where(Function(x) x.Value = cbIncompFluid.SelectedItem).FirstOrDefault.Key
    End Sub

End Class