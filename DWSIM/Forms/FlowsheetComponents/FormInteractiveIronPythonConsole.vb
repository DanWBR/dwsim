Imports System.Windows.Forms.Integration

Public Class FormInteractiveIronPythonConsole

    Inherits WeifenLuo.WinFormsUI.Docking.DockContent

    Public Property Flowsheet As Interfaces.IFlowsheet

    Private Sub FormInteractiveIronPythonConsole_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        ExtensionMethods.ChangeDefaultFont(Me)

        Dim ctrlHost = New ElementHost()
        ctrlHost.Dock = DockStyle.Fill
        Me.Panel1.Controls.Add(ctrlHost)
        Dim wpfCtrl = New IronPythonConsole.PythonConsoleWindow()
        wpfCtrl.InitializeComponent()
        ctrlHost.Child = wpfCtrl

    End Sub

End Class