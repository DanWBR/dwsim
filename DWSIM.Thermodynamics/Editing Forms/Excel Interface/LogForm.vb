Imports System.IO


Public Class LogForm

    <System.Runtime.InteropServices.DllImport("kernel32.dll")> _
    Private Shared Function AttachConsole(dwProcessId As Integer) As Boolean
    End Function
    Private Const ATTACH_PARENT_PROCESS As Integer = -1

    <System.Runtime.InteropServices.DllImport("kernel32.dll", SetLastError:=True)> _
    Friend Shared Function FreeConsole() As Integer
    End Function

    Private Sub ToolStripButton1_Click(sender As Object, e As EventArgs) Handles ToolStripButton1.Click
        TextBox1.Text = ""
    End Sub

    Private Sub ToolStripButton2_Click(sender As Object, e As EventArgs) Handles ToolStripButton2.Click
        Clipboard.SetText(TextBox1.Text)
    End Sub

    Private Sub LogForm_FormClosing(sender As Object, e As FormClosingEventArgs) Handles Me.FormClosing
        AttachConsole(ATTACH_PARENT_PROCESS)
        Dim standardOutput As New StreamWriter(Console.OpenStandardOutput())
        standardOutput.AutoFlush = True
        Console.SetOut(standardOutput)
    End Sub

    Private Sub LogForm_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        If Not Settings.ExcelMode Then Text = "Console Output"
    End Sub
End Class