Public Class FormConfigPropertyPackageBase

    Inherits WeifenLuo.WinFormsUI.Docking.DockContent

    Public _pp As PropertyPackages.PropertyPackage
    Public _form As Interfaces.IFlowsheet
    Public _comps As Dictionary(Of String, Interfaces.ICompoundConstantProperties)

    Private Sub FormConfigPropertyPackageBase_Load(sender As Object, e As EventArgs) Handles MyBase.Shown

        Me.TabText = Me.Text

    End Sub

End Class