Public Class FormConfigureSpreadsheetTable

    Public Table As GraphicObjects.SpreadsheetTableGraphic

    Private Sub FormConfigureSpreadsheetTable_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        TextBox1.Text = Table.SpreadsheetCellRange

    End Sub

    Private Sub TextBox1_TextChanged(sender As Object, e As EventArgs) Handles TextBox1.TextChanged

        Table.SpreadsheetCellRange = TextBox1.Text

    End Sub

End Class