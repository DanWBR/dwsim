Public Class FlashAlgorithmConfig

    Inherits WeifenLuo.WinFormsUI.Docking.DockContent

    Public Property Settings As Dictionary(Of Interfaces.Enums.FlashSetting, String)

    Public Property AvailableCompounds As List(Of String)

    Public FlashAlgo As Interfaces.IFlashAlgorithm

    Dim ci As Globalization.CultureInfo

    Private Sub FlashAlgorithmConfig_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        Select Case FlashAlgo.AlgoType
            Case Interfaces.Enums.FlashMethod.Default_Algorithm, Interfaces.Enums.FlashMethod.Nested_Loops_VLE
                TabControl1.TabPages.Remove(TabPageGM)
                TabControl1.TabPages.Remove(TabPageIO)
                TabControl1.TabPages.Remove(TabPageVLLE)
            Case Interfaces.Enums.FlashMethod.Nested_Loops_VLLE, Interfaces.Enums.FlashMethod.Nested_Loops_Immiscible_VLLE
                TabControl1.TabPages.Remove(TabPageGM)
                TabControl1.TabPages.Remove(TabPageIO)
            Case Interfaces.Enums.FlashMethod.Gibbs_Minimization_VLE
                TabControl1.TabPages.Remove(TabPageNL)
                TabControl1.TabPages.Remove(TabPageIO)
                TabControl1.TabPages.Remove(TabPageVLLE)
            Case Interfaces.Enums.FlashMethod.Gibbs_Minimization_VLLE
                TabControl1.TabPages.Remove(TabPageNL)
                TabControl1.TabPages.Remove(TabPageIO)
            Case Interfaces.Enums.FlashMethod.Inside_Out_VLE
                TabControl1.TabPages.Remove(TabPageGM)
                TabControl1.TabPages.Remove(TabPageNL)
                TabControl1.TabPages.Remove(TabPageVLLE)
            Case Interfaces.Enums.FlashMethod.Inside_Out_VLLE
                TabControl1.TabPages.Remove(TabPageGM)
                TabControl1.TabPages.Remove(TabPageNL)
            Case Interfaces.Enums.FlashMethod.Nested_Loops_SLE_Eutectic, Interfaces.Enums.FlashMethod.Nested_Loops_SLE_SolidSolution, Interfaces.Enums.FlashMethod.Simple_LLE
                TabControl1.TabPages.Remove(TabPageGM)
                TabControl1.TabPages.Remove(TabPageNL)
                TabControl1.TabPages.Remove(TabPageIO)
                TabControl1.TabPages.Remove(TabPageVLLE)
        End Select

        ci = Globalization.CultureInfo.InvariantCulture

        chkReplaceFlashPT.Checked = Settings(Interfaces.Enums.FlashSetting.Replace_PTFlash)
        chkValidateEqCalc.Checked = Settings(Interfaces.Enums.FlashSetting.ValidateEquilibriumCalc)
        tbFlashValidationTolerance.Text = Double.Parse(Settings(Interfaces.Enums.FlashSetting.ValidationGibbsTolerance), ci).ToString
        chkDoPhaseId.Checked = Settings(Interfaces.Enums.FlashSetting.UsePhaseIdentificationAlgorithm)
        chkCalcBubbleDew.Checked = Settings(Interfaces.Enums.FlashSetting.CalculateBubbleAndDewPoints)

        tbPHExtMaxIt.Text = Integer.Parse(Settings(Interfaces.Enums.FlashSetting.PHFlash_Maximum_Number_Of_External_Iterations), ci).ToString
        tbPHExtMaxTol.Text = Double.Parse(Settings(Interfaces.Enums.FlashSetting.PHFlash_External_Loop_Tolerance), ci).ToString
        tbPHIntMaxIt.Text = Integer.Parse(Settings(Interfaces.Enums.FlashSetting.PHFlash_Maximum_Number_Of_Internal_Iterations), ci).ToString
        tbPHintMaxTol.Text = Double.Parse(Settings(Interfaces.Enums.FlashSetting.PHFlash_Internal_Loop_Tolerance), ci).ToString
        tbPTExtMaxIt.Text = Integer.Parse(Settings(Interfaces.Enums.FlashSetting.PTFlash_Maximum_Number_Of_External_Iterations), ci).ToString
        tbPTExtTol.Text = Double.Parse(Settings(Interfaces.Enums.FlashSetting.PTFlash_External_Loop_Tolerance), ci).ToString
        tbPTintMaxIt.Text = Integer.Parse(Settings(Interfaces.Enums.FlashSetting.PTFlash_Maximum_Number_Of_Internal_Iterations), ci).ToString
        tbPTIntTol.Text = Double.Parse(Settings(Interfaces.Enums.FlashSetting.PTFlash_Internal_Loop_Tolerance), ci).ToString

        chkFastModeNL.Checked = Settings(Interfaces.Enums.FlashSetting.NL_FastMode)

        chkUseBroydenIO.Checked = Settings(Interfaces.Enums.FlashSetting.IO_FastMode)

        Dim minmethods As String() = [Enum].GetNames(New PropertyPackages.Auxiliary.FlashAlgorithms.GibbsMinimization3P().Solver.GetType)
        cbMinMethodGM.Items.Clear()
        cbMinMethodGM.Items.AddRange(minmethods)

        cbMinMethodGM.SelectedItem = Settings(Interfaces.Enums.FlashSetting.GM_OptimizationMethod)

        Select Case Settings(Interfaces.Enums.FlashSetting.ThreePhaseFlashStabTestSeverity)
            Case 0
                rbLow.Checked = True
            Case 1
                rbMedium.Checked = True
            Case 2
                rbHigh.Checked = True
        End Select

        SetupKeyCompounds()

    End Sub

    Private Sub FlashAlgorithmConfig_FormClosing(sender As Object, e As FormClosingEventArgs) Handles Me.FormClosing

        Try

            Settings(Interfaces.Enums.FlashSetting.Replace_PTFlash) = chkReplaceFlashPT.Checked
            Settings(Interfaces.Enums.FlashSetting.ValidateEquilibriumCalc) = chkValidateEqCalc.Checked
            Settings(Interfaces.Enums.FlashSetting.UsePhaseIdentificationAlgorithm) = chkDoPhaseId.Checked
            Settings(Interfaces.Enums.FlashSetting.CalculateBubbleAndDewPoints) = chkCalcBubbleDew.Checked

            Settings(Interfaces.Enums.FlashSetting.ValidationGibbsTolerance) = Double.Parse(tbFlashValidationTolerance.Text).ToString(ci)

            Settings(Interfaces.Enums.FlashSetting.PHFlash_Maximum_Number_Of_External_Iterations) = Integer.Parse(tbPHExtMaxIt.Text).ToString(ci)
            Settings(Interfaces.Enums.FlashSetting.PHFlash_External_Loop_Tolerance) = Double.Parse(tbPHExtMaxTol.Text).ToString(ci)
            Settings(Interfaces.Enums.FlashSetting.PHFlash_Maximum_Number_Of_Internal_Iterations) = Integer.Parse(tbPHIntMaxIt.Text).ToString(ci)
            Settings(Interfaces.Enums.FlashSetting.PHFlash_Internal_Loop_Tolerance) = Double.Parse(tbPHintMaxTol.Text).ToString(ci)
            Settings(Interfaces.Enums.FlashSetting.PTFlash_Maximum_Number_Of_External_Iterations) = Integer.Parse(tbPTExtMaxIt.Text).ToString(ci)
            Settings(Interfaces.Enums.FlashSetting.PTFlash_External_Loop_Tolerance) = Double.Parse(tbPTExtTol.Text).ToString(ci)
            Settings(Interfaces.Enums.FlashSetting.PTFlash_Maximum_Number_Of_Internal_Iterations) = Integer.Parse(tbPTintMaxIt.Text).ToString(ci)
            Settings(Interfaces.Enums.FlashSetting.PTFlash_Internal_Loop_Tolerance) = Double.Parse(tbPTIntTol.Text).ToString(ci)

            Settings(Interfaces.Enums.FlashSetting.NL_FastMode) = chkFastModeNL.Checked

            Settings(Interfaces.Enums.FlashSetting.IO_FastMode) = chkUseBroydenIO.Checked

            Settings(Interfaces.Enums.FlashSetting.GM_OptimizationMethod) = cbMinMethodGM.SelectedItem

            If rbLow.Checked Then Settings(Interfaces.Enums.FlashSetting.ThreePhaseFlashStabTestSeverity) = 0
            If rbMedium.Checked Then Settings(Interfaces.Enums.FlashSetting.ThreePhaseFlashStabTestSeverity) = 1
            If rbHigh.Checked Then Settings(Interfaces.Enums.FlashSetting.ThreePhaseFlashStabTestSeverity) = 2

            Dim comps As String = ""

            For Each lvi As ListViewItem In lvKeyComp.Items
                If lvi.Checked Then comps += lvi.Text + ","
            Next

            Settings(Interfaces.Enums.FlashSetting.ThreePhaseFlashStabTestCompIds) = comps

        Catch ex As Exception

            MessageBox.Show("Error parsing input. Some settings may not have been updated.", "Error", MessageBoxButtons.OK, MessageBoxIcon.Error)

        End Try

    End Sub

    Private Sub SetupKeyCompounds()

        Dim selected As Array

        selected = Settings(Interfaces.Enums.FlashSetting.ThreePhaseFlashStabTestCompIds).ToArray(ci, Type.GetType("System.String"))

        Me.lvKeyComp.Items.Clear()

        For i As Integer = 0 To AvailableCompounds.Count - 1
            With Me.lvKeyComp.Items.Add(AvailableCompounds(i))
                For Each s As String In selected
                    If s = AvailableCompounds(i) Then
                        .Checked = True
                        Exit For
                    End If
                Next
            End With
        Next

    End Sub

End Class