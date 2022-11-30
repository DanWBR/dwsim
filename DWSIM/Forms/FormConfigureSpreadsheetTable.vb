Imports DWSIM.Drawing.SkiaSharp.GraphicObjects.Tables

Public Class FormConfigureSpreadsheetTable

    Public Table As SpreadsheetTableGraphic

    Private Sub FormConfigureSpreadsheetTable_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        ExtensionMethods.ChangeDefaultFont(Me)

        TextBox1.Text = Table.SpreadsheetCellRange

    End Sub

    Private Sub TextBox1_TextChanged(sender As Object, e As EventArgs) Handles TextBox1.TextChanged

        Table.SpreadsheetCellRange = TextBox1.Text

    End Sub

    Private Sub FormConfigureSpreadsheetTable_Shown(sender As Object, e As EventArgs) Handles Me.Shown
        FormMain.TranslateFormFunction?.Invoke(Me)
    End Sub
End Class