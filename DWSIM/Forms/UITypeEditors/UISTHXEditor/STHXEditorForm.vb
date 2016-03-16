Public Class STHXEditorForm

    Public props As DWSIM.SimulationObjects.UnitOperations.Auxiliary.HeatExchanger.STHXProperties

    Dim su As DWSIM.SystemsOfUnits.Units = My.Application.ActiveSimulation.Options.SelectedUnitSystem
    Dim cv As New DWSIM.SystemsOfUnits.Converter
    Dim nf As String = My.Application.ActiveSimulation.Options.NumberFormat

    Private Sub STHXEditorForm_FormClosing(ByVal sender As Object, ByVal e As System.Windows.Forms.FormClosingEventArgs) Handles Me.FormClosing

        With props
            .Shell_BaffleCut = tbBaffleCut.Text
            .Shell_BaffleSpacing = Converter.ConvertToSI(su.thickness, tbBaffleSpacing.Text)
            .Shell_NumberOfPasses = tbNumberOfShellPasses.Text
            .Tube_NumberPerShell = tbNumberOfTubesPerShell.Text
            .Shell_Di = Converter.ConvertToSI(su.diameter, tbShellDi.Text)
            .Shell_Fouling = Converter.ConvertToSI(su.foulingfactor, tbShellFoulingFactor.Text)
            .Shell_NumberOfShellsInSeries = tbShellsInSeries.Text
            .Tube_De = Converter.ConvertToSI(su.diameter, tbTubeDe.Text)
            .Tube_Di = Converter.ConvertToSI(su.diameter, tbTubeDi.Text)
            .Tube_Roughness = Converter.ConvertToSI(su.diameter, tbTubeRoughness.Text)
            .Tube_Fouling = Converter.ConvertToSI(su.foulingfactor, tbTubeFoulingFactor.Text)
            .Tube_Length = Converter.ConvertToSI(su.distance, tbTubeLength.Text)
            .Tube_PassesPerShell = tbTubePassesPerShell.Text
            .Tube_Pitch = Converter.ConvertToSI(su.thickness, tbTubePitch.Text)
            .Tube_ThermalConductivity = Converter.ConvertToSI(su.thermalConductivity, tbTubeThermalCond.Text)
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

        With props
            tbBaffleCut.Text = Format(.Shell_BaffleCut, nf)
            tbBaffleSpacing.Text = Format(Converter.ConvertFromSI(su.thickness, .Shell_BaffleSpacing), nf)
            tbNumberOfShellPasses.Text = .Shell_NumberOfPasses
            tbNumberOfTubesPerShell.Text = .Tube_NumberPerShell
            tbShellDi.Text = Format(Converter.ConvertFromSI(su.diameter, .Shell_Di), nf)
            tbShellFoulingFactor.Text = Converter.ConvertFromSI(su.foulingfactor, .Shell_Fouling)
            tbShellsInSeries.Text = .Shell_NumberOfShellsInSeries
            tbTubeDe.Text = Format(Converter.ConvertFromSI(su.diameter, .Tube_De), nf)
            tbTubeDi.Text = Format(Converter.ConvertFromSI(su.diameter, .Tube_Di), nf)
            tbTubeFoulingFactor.Text = Converter.ConvertFromSI(su.foulingfactor, .Tube_Fouling)
            tbTubeLength.Text = Format(Converter.ConvertFromSI(su.distance, .Tube_Length), nf)
            tbTubePassesPerShell.Text = .Tube_PassesPerShell
            tbTubePitch.Text = Format(Converter.ConvertFromSI(su.thickness, .Tube_Pitch), nf)
            tbTubeRoughness.Text = Converter.ConvertFromSI(su.diameter, .Tube_Roughness)
            tbTubeThermalCond.Text = Converter.ConvertFromSI(su.thermalConductivity, .Tube_ThermalConductivity)
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