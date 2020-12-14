Imports DWSIM.SharedClasses
Imports Microsoft.Win32
Imports CapeOpen

Public Class FlashAlgorithmConfig

    Inherits System.Windows.Forms.Form

    Public Property Settings As Dictionary(Of Interfaces.Enums.FlashSetting, String)

    Dim ci As Globalization.CultureInfo

    Private _loaded As Boolean = False
    Public _esname As String = ""

    Public Property ExcelMode As Boolean = False

    Private Sub FlashAlgorithmConfig_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        If Not ExcelMode Then

        Else

            Me.Text = "Flash Algorithm Settings"

        End If

        ci = Globalization.CultureInfo.InvariantCulture

        chkReplaceFlashPT.Checked = Settings(Interfaces.Enums.FlashSetting.Replace_PTFlash)
        chkValidateEqCalc.Checked = Settings(Interfaces.Enums.FlashSetting.ValidateEquilibriumCalc)
        tbFlashValidationTolerance.Text = Double.Parse(Settings(Interfaces.Enums.FlashSetting.ValidationGibbsTolerance), ci).ToString

        tbPHExtMaxIt.Text = Integer.Parse(Settings(Interfaces.Enums.FlashSetting.PHFlash_Maximum_Number_Of_External_Iterations), ci).ToString
        tbPHExtMaxTol.Text = Double.Parse(Settings(Interfaces.Enums.FlashSetting.PHFlash_External_Loop_Tolerance), ci).ToString
        tbPHIntMaxIt.Text = Integer.Parse(Settings(Interfaces.Enums.FlashSetting.PHFlash_Maximum_Number_Of_Internal_Iterations), ci).ToString
        tbPHintMaxTol.Text = Double.Parse(Settings(Interfaces.Enums.FlashSetting.PHFlash_Internal_Loop_Tolerance), ci).ToString
        tbPTExtMaxIt.Text = Integer.Parse(Settings(Interfaces.Enums.FlashSetting.PTFlash_Maximum_Number_Of_External_Iterations), ci).ToString
        tbPTExtTol.Text = Double.Parse(Settings(Interfaces.Enums.FlashSetting.PTFlash_External_Loop_Tolerance), ci).ToString
        tbPTintMaxIt.Text = Integer.Parse(Settings(Interfaces.Enums.FlashSetting.PTFlash_Maximum_Number_Of_Internal_Iterations), ci).ToString
        tbPTIntTol.Text = Double.Parse(Settings(Interfaces.Enums.FlashSetting.PTFlash_Internal_Loop_Tolerance), ci).ToString

        tbPV_DampingFactor.Text = Double.Parse(Settings(Interfaces.Enums.FlashSetting.PVFlash_FixedDampingFactor), ci).ToString
        tbPV_EpsilonT.Text = Double.Parse(Settings(Interfaces.Enums.FlashSetting.PVFlash_TemperatureDerivativeEpsilon), ci).ToString
        tbPV_MaxDT.Text = Double.Parse(Settings(Interfaces.Enums.FlashSetting.PVFlash_MaximumTemperatureChange), ci).ToString

        tbPH_MaxDT.Text = Double.Parse(Settings(Interfaces.Enums.FlashSetting.PHFlash_MaximumTemperatureChange), ci).ToString

        tbPT_DampingFactor.Text = Double.Parse(Settings(Interfaces.Enums.FlashSetting.PTFlash_DampingFactor), ci).ToString

        chkFastModeNL.Checked = Settings(Interfaces.Enums.FlashSetting.NL_FastMode)

        chkImmiscible.Checked = Settings(Interfaces.Enums.FlashSetting.ImmiscibleWaterOption)

        Select Case Settings(Interfaces.Enums.FlashSetting.ForceEquilibriumCalculationType)
            Case "Default"
                cbFlashType.SelectedIndex = 0
            Case "VLE"
                cbFlashType.SelectedIndex = 1
            Case "VLLE"
                cbFlashType.SelectedIndex = 2
            Case "SVLE"
                cbFlashType.SelectedIndex = 3
            Case "SVLLE"
                cbFlashType.SelectedIndex = 4
        End Select

        _loaded = True

    End Sub

    Private Sub FlashAlgorithmConfig_FormClosing(sender As Object, e As FormClosingEventArgs) Handles Me.FormClosing

        Try

            If Not ExcelMode Then

                Settings(Interfaces.Enums.FlashSetting.Replace_PTFlash) = chkReplaceFlashPT.Checked
                Settings(Interfaces.Enums.FlashSetting.ValidateEquilibriumCalc) = chkValidateEqCalc.Checked

                Settings(Interfaces.Enums.FlashSetting.ValidationGibbsTolerance) = Double.Parse(tbFlashValidationTolerance.Text).ToString(ci)

            End If

            If tbPHExtMaxIt.Text <> "" Then Settings(Interfaces.Enums.FlashSetting.PHFlash_Maximum_Number_Of_External_Iterations) = Integer.Parse(tbPHExtMaxIt.Text).ToString(ci)
            If tbPHExtMaxTol.Text <> "" Then Settings(Interfaces.Enums.FlashSetting.PHFlash_External_Loop_Tolerance) = tbPHExtMaxTol.Text.ToDoubleFromCurrent().ToString(ci)
            If tbPHIntMaxIt.Text <> "" Then Settings(Interfaces.Enums.FlashSetting.PHFlash_Maximum_Number_Of_Internal_Iterations) = Integer.Parse(tbPHIntMaxIt.Text).ToString(ci)
            If tbPHintMaxTol.Text <> "" Then Settings(Interfaces.Enums.FlashSetting.PHFlash_Internal_Loop_Tolerance) = tbPHintMaxTol.Text.ToDoubleFromCurrent().ToString(ci)
            If tbPTExtMaxIt.Text <> "" Then Settings(Interfaces.Enums.FlashSetting.PTFlash_Maximum_Number_Of_External_Iterations) = Integer.Parse(tbPTExtMaxIt.Text).ToString(ci)
            If tbPTExtTol.Text <> "" Then Settings(Interfaces.Enums.FlashSetting.PTFlash_External_Loop_Tolerance) = tbPTExtTol.Text.ToDoubleFromCurrent().ToString(ci)
            If tbPTintMaxIt.Text <> "" Then Settings(Interfaces.Enums.FlashSetting.PTFlash_Maximum_Number_Of_Internal_Iterations) = Integer.Parse(tbPTintMaxIt.Text).ToString(ci)
            If tbPTIntTol.Text <> "" Then Settings(Interfaces.Enums.FlashSetting.PTFlash_Internal_Loop_Tolerance) = tbPTIntTol.Text.ToDoubleFromCurrent().ToString(ci)

            If tbPV_MaxDT.Text <> "" Then Settings(Interfaces.Enums.FlashSetting.PVFlash_MaximumTemperatureChange) = tbPV_MaxDT.Text.ToDoubleFromCurrent().ToString(ci)
            If tbPV_DampingFactor.Text <> "" Then Settings(Interfaces.Enums.FlashSetting.PVFlash_FixedDampingFactor) = tbPV_DampingFactor.Text.ToDoubleFromCurrent().ToString(ci)
            If tbPV_EpsilonT.Text <> "" Then Settings(Interfaces.Enums.FlashSetting.PVFlash_TemperatureDerivativeEpsilon) = tbPV_EpsilonT.Text.ToDoubleFromCurrent().ToString(ci)

            If tbPH_MaxDT.Text <> "" Then Settings(Interfaces.Enums.FlashSetting.PHFlash_MaximumTemperatureChange) = tbPH_MaxDT.Text.ToDoubleFromCurrent().ToString(ci)

            If tbPT_DampingFactor.Text <> "" Then Settings(Interfaces.Enums.FlashSetting.PTFlash_DampingFactor) = tbPT_DampingFactor.Text.ToDoubleFromCurrent().ToString(ci)

            Settings(Interfaces.Enums.FlashSetting.NL_FastMode) = chkFastModeNL.Checked

            Select Case cbFlashType.SelectedIndex
                Case 0
                    Settings(Interfaces.Enums.FlashSetting.ForceEquilibriumCalculationType) = "Default"
                Case 1
                    Settings(Interfaces.Enums.FlashSetting.ForceEquilibriumCalculationType) = "VLE"
                Case 2
                    Settings(Interfaces.Enums.FlashSetting.ForceEquilibriumCalculationType) = "VLLE"
                Case 3
                    Settings(Interfaces.Enums.FlashSetting.ForceEquilibriumCalculationType) = "SVLE"
                Case 4
                    Settings(Interfaces.Enums.FlashSetting.ForceEquilibriumCalculationType) = "SVLLE"
            End Select

            Settings(Interfaces.Enums.FlashSetting.ImmiscibleWaterOption) = chkImmiscible.Checked

        Catch ex As Exception

            MessageBox.Show("Error parsing input. Some settings may not have been updated.", "Error", MessageBoxButtons.OK, MessageBoxIcon.Error)

        End Try

    End Sub

    Private Sub cbFlashType_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbFlashType.SelectedIndexChanged
        If cbFlashType.SelectedIndex = 2 Then
            chkImmiscible.Enabled = True
        Else
            chkImmiscible.Enabled = False
        End If
    End Sub
End Class