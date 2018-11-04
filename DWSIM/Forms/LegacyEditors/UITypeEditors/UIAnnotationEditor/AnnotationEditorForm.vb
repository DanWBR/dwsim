Public Class AnnotationEditorForm

    Inherits System.Windows.Forms.Form

    Public rtftext As String = ""

    Private Sub AnnotationEditorForm_Shown(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Shown
        Me.RichTextBoxExtended1.rtb1.Rtf = rtftext
    End Sub

    Private Sub AnnotationEditorForm_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load

    End Sub
End Class