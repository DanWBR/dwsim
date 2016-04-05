Public Class EditingForm

    Inherits WeifenLuo.WinFormsUI.Docking.DockContent

    Public Property DefaultLabelWidth As Integer = 150
    Public Property DefaultRowHeight As Integer = 28
    Public Property DefaultEditorWidth As Integer = 100
    Public Property DefaultUnitsWidth As Integer = 50

    Public ReadOnly Property TotalWidth As Integer
        Get
            Return DefaultLabelWidth + DefaultEditorWidth + DefaultUnitsWidth + 6 * DefaultPadding.All
        End Get
    End Property

    Private Sub EditingForm_Load(sender As Object, e As EventArgs) Handles MyBase.Load

    End Sub

End Class