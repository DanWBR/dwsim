Imports System.Windows.Forms

Public Module DropDownWidth

    <System.Runtime.CompilerServices.Extension()>
    Public Sub SetDropDownMaxWidth(ByVal myCombo As ComboBox)

        Dim maxWidth As Integer, temp As Integer = 0
        maxWidth = myCombo.Width

        Try
            For Each obj In myCombo.Items
                temp = TextRenderer.MeasureText(obj.ToString(), myCombo.Font).Width
                If temp > maxWidth Then maxWidth = temp
            Next
        Catch ex As Exception
        End Try

        If maxWidth > 0 Then myCombo.DropDownWidth = maxWidth

    End Sub

End Module
