Imports System.Windows.Forms

Public Module DropDownWidth

    <System.Runtime.CompilerServices.Extension()>
    Public Sub SetDropDownMaxWidth(ByVal myCombo As ComboBox)

        Dim maxWidth As Integer = 0, temp As Integer = 0

        For Each obj In myCombo.Items
            temp = TextRenderer.MeasureText(obj.ToString(), myCombo.Font).Width

            If temp > maxWidth Then
                maxWidth = temp
            End If
        Next

        myCombo.DropDownWidth = maxWidth

    End Sub

End Module
