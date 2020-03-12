Public Class FormDynamicsManager

    Inherits WeifenLuo.WinFormsUI.Docking.DockContent

    Public Flowsheet As FormFlowsheet

    Private Sub FormDynamicsManager_Load(sender As Object, e As EventArgs) Handles MyBase.Load

    End Sub

    Private Sub CheckBox1_CheckedChanged(sender As Object, e As EventArgs) Handles chkDynamics.CheckedChanged
        If chkDynamics.Checked Then
            chkDynamics.Text = DWSIM.App.GetLocalString("Deactivate")
            lblStatus.Text = DWSIM.App.GetLocalString("DynEnabled")
            Flowsheet.DynamicMode = True
            chkDynamics.ForeColor = Color.White
            chkDynamics.BackColor = Color.DarkGreen
        Else
            chkDynamics.Text = DWSIM.App.GetLocalString("Activate")
            lblStatus.Text = DWSIM.App.GetLocalString("DynDisabled")
            Flowsheet.DynamicMode = False
            chkDynamics.ForeColor = Color.White
            chkDynamics.BackColor = Color.DarkRed
        End If
    End Sub

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
        Flowsheet.FormIntegratorControls.Show(Flowsheet.GetDockPanel)
        'Flowsheet.dckPanel.SaveAsXml("C:\Users\Daniel\Desktop\layout.xml")
    End Sub

End Class