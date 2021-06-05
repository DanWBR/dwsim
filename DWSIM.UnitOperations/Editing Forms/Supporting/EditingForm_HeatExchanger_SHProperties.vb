Imports DWSIM.UnitOperations.UnitOperations
Imports cv = DWSIM.SharedClasses.SystemsOfUnits.Converter

Public Class EditingForm_HeatExchanger_SHProperties

    Public hx As UnitOperations.HeatExchanger

    Dim su As SharedClasses.SystemsOfUnits.Units
    Dim nf As String = ""

    Private Sub STHXEditorForm_FormClosing(ByVal sender As Object, ByVal e As System.Windows.Forms.FormClosingEventArgs) Handles Me.FormClosing

        With hx.STProperties

            .Shell_BaffleCut = tbBaffleCut.Text.ToDoubleFromCurrent()
            .Shell_BaffleSpacing = cv.ConvertToSI(su.thickness, tbBaffleSpacing.Text.ToDoubleFromCurrent) * 1000
            .Shell_NumberOfPasses = tbNumberOfShellPasses.Text.ToDoubleFromCurrent
            .Tube_NumberPerShell = tbNumberOfTubesPerShell.Text.ToDoubleFromCurrent
            .Shell_Di = cv.ConvertToSI(su.diameter, tbShellDi.Text.ToDoubleFromCurrent) * 1000
            .Shell_Fouling = cv.ConvertToSI(su.foulingfactor, tbShellFoulingFactor.Text.ToDoubleFromCurrent)
            .Shell_NumberOfShellsInSeries = tbShellsInSeries.Text.ToDoubleFromCurrent
            .Tube_De = cv.ConvertToSI(su.diameter, tbTubeDe.Text.ToDoubleFromCurrent) * 1000
            .Tube_Di = cv.ConvertToSI(su.diameter, tbTubeDi.Text.ToDoubleFromCurrent) * 1000
            .Tube_Roughness = cv.ConvertToSI(su.diameter, tbTubeRoughness.Text.ToDoubleFromCurrent) * 1000
            .Tube_Fouling = cv.ConvertToSI(su.foulingfactor, tbTubeFoulingFactor.Text.ToDoubleFromCurrent)
            .Tube_Length = cv.ConvertToSI(su.distance, tbTubeLength.Text.ToDoubleFromCurrent)
            .Tube_PassesPerShell = tbTubePassesPerShell.Text.ToDoubleFromCurrent
            .Tube_Pitch = cv.ConvertToSI(su.thickness, tbTubePitch.Text.ToDoubleFromCurrent) * 1000
            .Tube_ThermalConductivity = cv.ConvertToSI(su.thermalConductivity, tbTubeThermalCond.Text.ToDoubleFromCurrent)
            .Shell_BaffleOrientation = cbBaffleOrientation.SelectedIndex
            .Shell_BaffleType = cbBaffleType.SelectedIndex
            .Tube_Layout = cbTubeLayout.SelectedIndex

            If rbCold.Checked Then
                .Tube_Fluid = 0
                .Shell_Fluid = 1
            Else
                .Tube_Fluid = 1
                .Shell_Fluid = 0
            End If

        End With

    End Sub

    Private Sub STHXEditorForm_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load

        su = hx.FlowSheet.FlowsheetOptions.SelectedUnitSystem
        nf = hx.FlowSheet.FlowsheetOptions.NumberFormat

        With hx.STProperties

            tbBaffleCut.Text = Format(.Shell_BaffleCut, nf)
            tbBaffleSpacing.Text = Format(cv.ConvertFromSI(su.thickness, .Shell_BaffleSpacing / 1000.0), nf)
            tbNumberOfShellPasses.Text = .Shell_NumberOfPasses
            tbNumberOfTubesPerShell.Text = .Tube_NumberPerShell
            tbShellDi.Text = Format(cv.ConvertFromSI(su.diameter, .Shell_Di / 1000.0), nf)
            tbShellFoulingFactor.Text = cv.ConvertFromSI(su.foulingfactor, .Shell_Fouling)
            tbShellsInSeries.Text = .Shell_NumberOfShellsInSeries
            tbTubeDe.Text = Format(cv.ConvertFromSI(su.diameter, .Tube_De / 1000.0), nf)
            tbTubeDi.Text = Format(cv.ConvertFromSI(su.diameter, .Tube_Di / 1000.0), nf)
            tbTubeFoulingFactor.Text = cv.ConvertFromSI(su.foulingfactor, .Tube_Fouling)
            tbTubeLength.Text = Format(cv.ConvertFromSI(su.distance, .Tube_Length), nf)
            tbTubePassesPerShell.Text = .Tube_PassesPerShell
            tbTubePitch.Text = Format(cv.ConvertFromSI(su.thickness, .Tube_Pitch / 1000.0), nf)
            tbTubeRoughness.Text = cv.ConvertFromSI(su.diameter, .Tube_Roughness / 1000.0)
            tbTubeThermalCond.Text = cv.ConvertFromSI(su.thermalConductivity, .Tube_ThermalConductivity)
            cbBaffleOrientation.SelectedIndex = .Shell_BaffleOrientation
            cbBaffleType.SelectedIndex = .Shell_BaffleType
            cbTubeLayout.SelectedIndex = .Tube_Layout
            If .Tube_Fluid = 0 Then
                rbCold.Checked = True
            Else
                rbCold.Checked = False
            End If

        End With

        lbTubeDe.Text = su.diameter
        lbuTubeDi.Text = su.diameter
        lbTubeRoughness.Text = su.diameter
        lbuShellDi.Text = su.diameter
        lbuBaffleCut.Text = "%"
        lbuBaffleSpacing.Text = su.thickness
        lbuShellFoulingFactor.Text = su.foulingfactor
        lbuTubeFoulingFactor.Text = su.foulingfactor
        lbuTubeLength.Text = su.distance
        lbuTubePitch.Text = su.thickness
        lbuTubeThermalCond.Text = su.thermalConductivity

    End Sub

End Class