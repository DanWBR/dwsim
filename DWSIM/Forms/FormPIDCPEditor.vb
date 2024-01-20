Public Class FormPIDCPEditor

    Public PID As PIDController

    Private Sub FormPIDCPEditor_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        ExtensionMethods.ChangeDefaultFont(Me)

        UpdateInfo()

    End Sub

    Public Sub UpdateInfo()

        If PID IsNot Nothing Then

            Text = PID.GraphicObject.Tag

            chkActive.Checked = PID.Active

            chkAuto.Checked = Not PID.ManualOverride

            tbSP.Text = PID.AdjustValue.ToString(PID.GetFlowsheet.FlowsheetOptions.NumberFormat)

            tbPV.Text = PID.PVValue.ToString(PID.GetFlowsheet.FlowsheetOptions.NumberFormat)

            tbMV.Text = PID.MVValue.ToString(PID.GetFlowsheet.FlowsheetOptions.NumberFormat)

            tbMV.ReadOnly = chkAuto.Checked

        End If

    End Sub


    Private Sub tbSP_KeyDown(sender As Object, e As KeyEventArgs) Handles tbSP.KeyDown

        If e.KeyCode = Keys.Enter Then
            Try
                PID.AdjustValue = tbSP.Text.ToDoubleFromCurrent
                PID.SPValue = PID.AdjustValue
                Close()
            Catch ex As Exception
                MessageBox.Show(DWSIM.App.GetLocalString("Erro"), ex.Message, MessageBoxButtons.OK, MessageBoxIcon.Error)
            End Try
        End If

    End Sub

    Private Sub tbMV_KeyDown(sender As Object, e As KeyEventArgs) Handles tbMV.KeyDown

        If e.KeyCode = Keys.Enter And Not tbMV.ReadOnly Then
            Try
                PID.MVValue = tbMV.Text.ToDoubleFromCurrent
                Close()
            Catch ex As Exception
                MessageBox.Show(DWSIM.App.GetLocalString("Erro"), ex.Message, MessageBoxButtons.OK, MessageBoxIcon.Error)
            End Try
        End If

    End Sub

    Private Sub CheckBox1_CheckedChanged(sender As Object, e As EventArgs) Handles chkAuto.CheckedChanged

        If PID Is Nothing Then Exit Sub

        PID.ManualOverride = Not chkAuto.Checked

        tbMV.ReadOnly = chkAuto.Checked

        If chkAuto.Checked Then
            chkAuto.ForeColor = Color.White
            chkAuto.BackColor = Color.Green
            chkAuto.Text = "AUTO"
        Else
            chkAuto.ForeColor = Color.White
            chkAuto.BackColor = Color.Blue
            chkAuto.Text = "MANUAL"
        End If

    End Sub

    Private Sub chkActive_CheckedChanged(sender As Object, e As EventArgs) Handles chkActive.CheckedChanged

        If PID Is Nothing Then Exit Sub

        PID.Active = chkActive.Checked

        If chkActive.Checked Then

            chkActive.ForeColor = Color.White
            chkActive.BackColor = Color.Green
            chkActive.Text = "ON"
            chkAuto.Enabled = True
            tbMV.Enabled = True
            tbPV.Enabled = True
            tbSP.Enabled = True

        Else

            chkActive.ForeColor = Color.White
            chkActive.BackColor = Color.Red
            chkActive.Text = "OFF"
            chkAuto.Enabled = False
            tbMV.Enabled = False
            tbPV.Enabled = False
            tbSP.Enabled = False

        End If

    End Sub

End Class