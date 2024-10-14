Imports DWSIM.SharedClasses
Imports Microsoft.Win32
Imports CapeOpen
Imports DWSIM.Interfaces

Public Class FlashAlgorithmConfig

    Inherits System.Windows.Forms.Form

    Public Property Settings As Dictionary(Of Interfaces.Enums.FlashSetting, String)

    Public Property PropPack As PropertyPackages.PropertyPackage

    Dim ci As Globalization.CultureInfo

    Private _loaded As Boolean = False
    Public _esname As String = ""

    Public Property ExcelMode As Boolean = False

    Private Sub FlashAlgorithmConfig_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        ExtensionMethods.ChangeDefaultFont(Me)

        If Not ExcelMode Then

        Else

            Me.Text = "Flash Algorithm Settings"

        End If

        ci = Globalization.CultureInfo.InvariantCulture

        chkValidateEqCalc.Checked = Settings(Interfaces.Enums.FlashSetting.ValidateEquilibriumCalc)

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

        chkHandleSolids.Checked = Settings(Interfaces.Enums.FlashSetting.HandleSolidsInDefaultEqCalcMode)

        chkDoPhaseId.Checked = Settings(Interfaces.Enums.FlashSetting.UsePhaseIdentificationAlgorithm)

        chkCalcBubbleDew.Checked = Settings(Interfaces.Enums.FlashSetting.CalculateBubbleAndDewPoints)

        CheckBoxInterpTempPHFlash.Checked = Settings(Interfaces.Enums.FlashSetting.PHFlash_Use_Interpolated_Result_In_Oscillating_Temperature_Cases)

        chkIdealPVFlash.Checked = Settings(Interfaces.Enums.FlashSetting.PVFlash_TryIdealCalcOnFailure)

        chkDisplayCompWarning.Checked = PropPack.DisplayMissingCompoundPropertiesWarning

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
            Case "NoFlash"
                cbFlashType.SelectedIndex = 5
        End Select

        cbFSMethod.SelectedIndex = Settings(Interfaces.Enums.FlashSetting.FailSafeCalculationMode)

        cbFlashApproach.SelectedIndex = PropPack.FlashCalculationApproach

        'external solvers

        If PropPack.Flowsheet IsNot Nothing Then

            cbExternalSolver.Items.Clear()
            cbExternalSolver.Items.Add("")
            For Each solver In PropPack.Flowsheet.ExternalSolvers.Values
                If solver.Category = Interfaces.Enums.ExternalSolverCategory.NonLinearMinimization Then
                    cbExternalSolver.Items.Add(solver.DisplayText)
                End If
            Next
            Dim esolver = Settings(Interfaces.Enums.FlashSetting.GibbsMinimizationExternalSolver)
            Dim selectedsolver = PropPack.Flowsheet.ExternalSolvers.Values.Where(Function(s) s.ID = esolver).FirstOrDefault()
            If selectedsolver IsNot Nothing Then
                cbExternalSolver.SelectedItem = selectedsolver.DisplayText
            Else
                cbExternalSolver.SelectedIndex = 0
            End If

            If PropPack.FlashCalculationApproach = PropertyPackages.PropertyPackage.FlashCalculationApproachType.GibbsMinimization Then
                cbExternalSolver.Enabled = True
            Else
                cbExternalSolver.Enabled = False
            End If

            If selectedsolver IsNot Nothing Then
                If TryCast(selectedsolver, IExternalSolverConfiguration) IsNot Nothing Then
                    btnConfigExtSolver.Enabled = True
                Else
                    btnConfigExtSolver.Enabled = False
                End If
            End If

        Else

            cbExternalSolver.Enabled = False

        End If

        cbExternalSolver.SetDropDownMaxWidth()


        _loaded = True

    End Sub

    Public Sub FlashAlgorithmConfig_FormClosing(sender As Object, e As FormClosingEventArgs) Handles Me.FormClosing

        Try

            If Not ExcelMode Then

                Settings(Interfaces.Enums.FlashSetting.ValidateEquilibriumCalc) = chkValidateEqCalc.Checked

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
                Case 5
                    Settings(Interfaces.Enums.FlashSetting.ForceEquilibriumCalculationType) = "NoFlash"
            End Select

            Settings(Interfaces.Enums.FlashSetting.FailSafeCalculationMode) = cbFSMethod.SelectedIndex

            Settings(Interfaces.Enums.FlashSetting.ImmiscibleWaterOption) = chkImmiscible.Checked

            Settings(Interfaces.Enums.FlashSetting.HandleSolidsInDefaultEqCalcMode) = chkHandleSolids.Checked

            Settings(Interfaces.Enums.FlashSetting.UsePhaseIdentificationAlgorithm) = chkDoPhaseId.Checked

            Settings(Interfaces.Enums.FlashSetting.CalculateBubbleAndDewPoints) = chkCalcBubbleDew.Checked

            Settings(Interfaces.Enums.FlashSetting.PHFlash_Use_Interpolated_Result_In_Oscillating_Temperature_Cases) = CheckBoxInterpTempPHFlash.Checked

            Settings(Interfaces.Enums.FlashSetting.PVFlash_TryIdealCalcOnFailure) = chkIdealPVFlash.Checked

            PropPack.FlashCalculationApproach = cbFlashApproach.SelectedIndex

            PropPack.DisplayMissingCompoundPropertiesWarning = chkDisplayCompWarning.Checked

            If cbExternalSolver.Enabled And PropPack.Flowsheet IsNot Nothing Then
                Dim selectedsolver = PropPack.Flowsheet.ExternalSolvers.Values.Where(
              Function(s) s.DisplayText = cbExternalSolver.SelectedItem.ToString()).FirstOrDefault()
                If selectedsolver IsNot Nothing Then
                    Settings(Interfaces.Enums.FlashSetting.GibbsMinimizationExternalSolver) = selectedsolver.ID
                Else
                    Settings(Interfaces.Enums.FlashSetting.GibbsMinimizationExternalSolver) = ""
                End If
            End If

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
        If cbFlashType.SelectedIndex = 0 Then
            chkHandleSolids.Enabled = True
        Else
            chkHandleSolids.Enabled = False
        End If
        If cbFlashApproach.SelectedIndex = 0 Then
            gpNL.Enabled = True
        Else
            gpNL.Enabled = False
        End If
        If cbFlashApproach.SelectedIndex = 1 Then
            Select Case cbFlashType.SelectedIndex
                Case 3, 4
                    MessageBox.Show("Inside-Out doesn't support solid calculations. Please select another method.", "Error")
                    cbFlashApproach.SelectedIndex = 0
            End Select
        End If
    End Sub

    Private Sub cbFlashApproach_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbFlashApproach.SelectedIndexChanged
        If cbFlashApproach.SelectedIndex = 0 Then
            gpNL.Enabled = True
        Else
            gpNL.Enabled = False
        End If
        If cbFlashApproach.SelectedIndex = 1 Then
            Select Case cbFlashType.SelectedIndex
                Case 3, 4
                    MessageBox.Show("Inside-Out doesn't support solid calculations. Please select another method.", "Error")
                    cbFlashApproach.SelectedIndex = 0
            End Select
        End If
        If cbFlashApproach.SelectedIndex = 2 Then
            cbExternalSolver.Enabled = True
        Else
            cbExternalSolver.Enabled = False
        End If

    End Sub

    Private Sub cbExternalSolver_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbExternalSolver.SelectedIndexChanged
        If cbExternalSolver.Enabled And PropPack.Flowsheet IsNot Nothing Then
            Dim selectedsolver = PropPack.Flowsheet.ExternalSolvers.Values.Where(
          Function(s) s.DisplayText = cbExternalSolver.SelectedItem.ToString()).FirstOrDefault()
            If selectedsolver IsNot Nothing Then
                If TryCast(selectedsolver, IExternalSolverConfiguration) IsNot Nothing Then
                    btnConfigExtSolver.Enabled = True
                Else
                    btnConfigExtSolver.Enabled = False
                End If
            Else
                btnConfigExtSolver.Enabled = False
            End If
        End If
    End Sub

    Private Sub btnConfigExtSolver_Click(sender As Object, e As EventArgs) Handles btnConfigExtSolver.Click
        Dim selectedsolver = PropPack.Flowsheet.ExternalSolvers.Values.Where(
          Function(s) s.DisplayText = cbExternalSolver.SelectedItem.ToString()).FirstOrDefault()
        If TryCast(selectedsolver, IExternalSolverConfiguration) IsNot Nothing Then
            Settings(Interfaces.Enums.FlashSetting.GibbsMinimizationExternalSolverConfigData) =
                DirectCast(selectedsolver, IExternalSolverConfiguration).Edit(Settings(Interfaces.Enums.FlashSetting.GibbsMinimizationExternalSolverConfigData))
        End If
    End Sub

End Class