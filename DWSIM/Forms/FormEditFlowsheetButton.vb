Imports System.Linq

Public Class FormEditFlowsheetButton

    Public ButtonObject As Drawing.SkiaSharp.GraphicObjects.Shapes.ButtonGraphic

    Public Flowsheet As Interfaces.IFlowsheet

    Private Sub FormEditFlowsheetButton_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        ChangeDefaultFont()

        tbDisplayText.Text = ButtonObject.Text

        Try
            cbFontSize.SelectedItem = ButtonObject.FontSize.ToString()
        Catch ex As Exception
        End Try

        cbScriptToRun.Items.Clear()
        cbScriptToRun.Items.AddRange(Flowsheet.Scripts.Values.Select(Function(s) s.Title).ToArray())

        Try
            cbScriptToRun.SelectedItem = ButtonObject.SelectedScript
        Catch ex As Exception
        End Try

    End Sub

    Private Sub tbDisplayText_TextChanged(sender As Object, e As EventArgs) Handles tbDisplayText.TextChanged
        ButtonObject.Text = tbDisplayText.Text
    End Sub

    Private Sub cbFontSize_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbFontSize.SelectedIndexChanged
        ButtonObject.FontSize = cbFontSize.SelectedItem
    End Sub

    Private Sub cbScriptToRun_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbScriptToRun.SelectedIndexChanged
        ButtonObject.SelectedScript = cbScriptToRun.SelectedItem
    End Sub

End Class