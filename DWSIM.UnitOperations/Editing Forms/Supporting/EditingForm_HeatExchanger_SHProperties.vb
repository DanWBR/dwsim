Imports DWSIM.UnitOperations.UnitOperations
Imports cv = DWSIM.SharedClasses.SystemsOfUnits.Converter

Public Class EditingForm_HeatExchanger_SHProperties

    Public hx As UnitOperations.HeatExchanger

    Dim su As SharedClasses.SystemsOfUnits.Units
    Dim nf As String = ""

    Private Sub STHXEditorForm_FormClosing(ByVal sender As Object, ByVal e As System.Windows.Forms.FormClosingEventArgs) Handles Me.FormClosing

        With hx.STProperties

            .Shell_BaffleCut = tbBaffleCut.Text
            .Shell_BaffleSpacing = cv.ConvertToSI(su.thickness, tbBaffleSpacing.Text)
            .Shell_NumberOfPasses = tbNumberOfShellPasses.Text
            .Tube_NumberPerShell = tbNumberOfTubesPerShell.Text
            .Shell_Di = cv.ConvertToSI(su.diameter, tbShellDi.Text)
            .Shell_Fouling = cv.ConvertToSI(su.foulingfactor, tbShellFoulingFactor.Text)
            .Shell_NumberOfShellsInSeries = tbShellsInSeries.Text
            .Tube_De = cv.ConvertToSI(su.diameter, tbTubeDe.Text)
            .Tube_Di = cv.ConvertToSI(su.diameter, tbTubeDi.Text)
            .Tube_Roughness = cv.ConvertToSI(su.diameter, tbTubeRoughness.Text)
            .Tube_Fouling = cv.ConvertToSI(su.foulingfactor, tbTubeFoulingFactor.Text)
            .Tube_Length = cv.ConvertToSI(su.distance, tbTubeLength.Text)
            .Tube_PassesPerShell = tbTubePassesPerShell.Text
            .Tube_Pitch = cv.ConvertToSI(su.thickness, tbTubePitch.Text)
            .Tube_ThermalConductivity = cv.ConvertToSI(su.thermalConductivity, tbTubeThermalCond.Text)
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
            tbBaffleSpacing.Text = Format(cv.ConvertFromSI(su.thickness, .Shell_BaffleSpacing), nf)
            tbNumberOfShellPasses.Text = .Shell_NumberOfPasses
            tbNumberOfTubesPerShell.Text = .Tube_NumberPerShell
            tbShellDi.Text = Format(cv.ConvertFromSI(su.diameter, .Shell_Di), nf)
            tbShellFoulingFactor.Text = cv.ConvertFromSI(su.foulingfactor, .Shell_Fouling)
            tbShellsInSeries.Text = .Shell_NumberOfShellsInSeries
            tbTubeDe.Text = Format(cv.ConvertFromSI(su.diameter, .Tube_De), nf)
            tbTubeDi.Text = Format(cv.ConvertFromSI(su.diameter, .Tube_Di), nf)
            tbTubeFoulingFactor.Text = cv.ConvertFromSI(su.foulingfactor, .Tube_Fouling)
            tbTubeLength.Text = Format(cv.ConvertFromSI(su.distance, .Tube_Length), nf)
            tbTubePassesPerShell.Text = .Tube_PassesPerShell
            tbTubePitch.Text = Format(cv.ConvertFromSI(su.thickness, .Tube_Pitch), nf)
            tbTubeRoughness.Text = cv.ConvertFromSI(su.diameter, .Tube_Roughness)
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