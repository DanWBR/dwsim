Public Class FormPIDCPEditor

    Public PID As PIDController

    Private Sub FormPIDCPEditor_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        Text = PID.GraphicObject.Tag

        chkAuto.Checked = Not PID.ManualOverride

        tbSP.Text = PID.SPValue.ToString(PID.GetFlowsheet.FlowsheetOptions.NumberFormat)

        tbPV.Text = PID.PVValue.ToString(PID.GetFlowsheet.FlowsheetOptions.NumberFormat)

        tbMV.Text = PID.MVValue.ToString(PID.GetFlowsheet.FlowsheetOptions.NumberFormat)

        tbMV.ReadOnly = chkAuto.Checked

    End Sub

    Private Sub tbSP_KeyDown(sender As Object, e As KeyEventArgs) Handles tbSP.KeyDown

        If e.KeyCode = Keys.Enter Then
            Try
                PID.AdjustValue = tbSP.Text.ToDoubleFromCurrent
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

        PID.ManualOverride = Not chkAuto.Checked

        tbMV.ReadOnly = chkAuto.Checked

        If chkAuto.Checked Then
            chkAuto.ForeColor = Color.White
            chkAuto.BackColor = Color.Green
            chkAuto.Text = "AUTO"
        Else
            chkAuto.ForeColor = Color.Black
            chkAuto.BackColor = Color.Red
            chkAuto.Text = "MANUAL"
        End If

    End Sub

End Class