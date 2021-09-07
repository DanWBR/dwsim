Public Class FormConfigPropertyPackageBase

    Inherits System.Windows.Forms.Form

    Public _pp As PropertyPackages.PropertyPackage
    Public _form As Interfaces.IFlowsheet
    Public _comps As Dictionary(Of String, Interfaces.ICompoundConstantProperties)

    Private Sub FormConfigPropertyPackageBase_Load(sender As Object, e As EventArgs) Handles MyBase.Load

    End Sub

    Private Sub FormConfigPropertyPackageBase_Shown(sender As Object, e As EventArgs) Handles Me.Shown

        ExtensionMethods.ChangeDefaultFont(Me)

    End Sub
End Class