Imports System.Windows.Forms
Imports DWSIM.Interfaces.Enums.GraphicObjects
Imports DWSIM.SharedClasses.UnitOperations
Imports su = DWSIM.SharedClasses.SystemsOfUnits
Imports DWSIM.UnitOperations.UnitOperations
Imports System.Drawing
Imports DWSIM.UnitOperations.UnitOperations.Column
Imports DWSIM.UnitOperations.UnitOperations.Auxiliary.SepOps

Public Class EditingForm_Column

    Inherits WeifenLuo.WinFormsUI.Docking.DockContent

    Public Property SimObject As UnitOperations.Column

    Public Loaded As Boolean = False

    Dim units As SharedClasses.SystemsOfUnits.Units
    Dim nf As String

    Private Sub EditingForm_HeaterCooler_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        Me.ShowHint = GlobalSettings.Settings.DefaultEditFormLocation

        UpdateInfo()

    End Sub

    Sub UpdateInfo()

        units = SimObject.FlowSheet.FlowsheetOptions.SelectedUnitSystem
        nf = SimObject.FlowSheet.FlowsheetOptions.NumberFormat

        Loaded = False

        With SimObject

            'first block

            chkActive.Checked = .GraphicObject.Active

            Me.Text = .GetDisplayName() & ": " & .GraphicObject.Tag

            lblTag.Text = .GraphicObject.Tag
            If .Calculated Then
                lblStatus.Text = .FlowSheet.GetTranslatedString("Calculado") & " (" & .LastUpdated.ToString & ")"
                lblStatus.ForeColor = Drawing.Color.Blue
            Else
                If Not .GraphicObject.Active Then
                    lblStatus.Text = .FlowSheet.GetTranslatedString("Inativo")
                    lblStatus.ForeColor = Drawing.Color.Gray
                ElseIf .ErrorMessage <> "" Then
                    If .ErrorMessage.Length > 50 Then
                        lblStatus.Text = .FlowSheet.GetTranslatedString("Erro") & " (" & .ErrorMessage.Substring(50) & "...)"
                    Else
                        lblStatus.Text = .FlowSheet.GetTranslatedString("Erro") & " (" & .ErrorMessage & ")"
                    End If
                    lblStatus.ForeColor = Drawing.Color.Red
                Else
                    lblStatus.Text = .FlowSheet.GetTranslatedString("NoCalculado")
                    lblStatus.ForeColor = Drawing.Color.Black
                End If
            End If

            lblConnectedTo.Text = ""

            If .IsSpecAttached Then lblConnectedTo.Text = .FlowSheet.SimulationObjects(.AttachedSpecId).GraphicObject.Tag
            If .IsAdjustAttached Then lblConnectedTo.Text = .FlowSheet.SimulationObjects(.AttachedAdjustId).GraphicObject.Tag

            If TypeOf SimObject Is DistillationColumn Then
            ElseIf TypeOf SimObject Is AbsorptionColumn Then
                TabControl1.TabPages.Remove(TabCondenser)
                TabControl1.TabPages.Remove(TabReboiler)
            ElseIf TypeOf SimObject Is RefluxedAbsorber Then
                TabControl1.TabPages.Remove(TabReboiler)
            ElseIf TypeOf SimObject Is ReboiledAbsorber Then
                TabControl1.TabPages.Remove(TabCondenser)
            End If

            If .SolvingMethod = 0 Then
                TabControl2.TabPages.Remove(TabSolverIO)
                TabControl2.TabPages.Remove(TabSolverNS)
            ElseIf .SolvingMethod = 1 Then
                TabControl2.TabPages.Remove(TabSolverBP)
                TabControl2.TabPages.Remove(TabSolverIO)
            Else
                TabControl2.TabPages.Remove(TabSolverBP)
                TabControl2.TabPages.Remove(TabSolverNS)
            End If

            'parameters

            tbNStages.Text = .NumberOfStages

            If TypeOf SimObject Is AbsorptionColumn Then cbAbsorberMode.SelectedIndex = DirectCast(SimObject, AbsorptionColumn).OperationMode Else cbAbsorberMode.Enabled = False
            cbSolvingMethod.SelectedIndex = .SolvingMethod
            tbMaxIt.Text = .MaxIterations
            tbConvTol.Text = .ExternalLoopTolerance

            cbCondType.SelectedIndex = .CondenserType
            tbCondPressure.Text = su.Converter.ConvertFromSI(units.pressure, .CondenserPressure).ToString(nf)
            tbCondPDrop.Text = su.Converter.ConvertFromSI(units.deltaP, .CondenserDeltaP).ToString(nf)
            cbCondSpec.SelectedIndex = .Specs("C").SType
            Dim cunits As String() = {}
            Select Case .Specs("C").SType
                Case ColumnSpec.SpecType.Component_Fraction
                    cunits = New String() {"M", "We"}
                Case ColumnSpec.SpecType.Component_Mass_Flow_Rate
                    cunits = New String() {"g/s", "lbm/h", "kg/s", "kg/h", "kg/d", "kg/min", "lb/min", "lb/s"}
                Case ColumnSpec.SpecType.Component_Molar_Flow_Rate
                    cunits = New String() {"mol/s", "lbmol/h", "mol/h", "mol/d", "kmol/s", "kmol/h", "kmol/d", "m3/d @ BR", "m3/d @ NC", "m3/d @ CNTP", "m3/d @ SC", "m3/d @ 0 C, 1 atm", "m3/d @ 15.56 C, 1 atm", "m3/d @ 20 C, 1 atm", "ft3/d @ 60 F, 14.7 psia", "ft3/d @ 0 C, 1 atm"}
                Case ColumnSpec.SpecType.Component_Recovery
                    cunits = New String() {"% M/M", "% W/W"}
                Case ColumnSpec.SpecType.Heat_Duty
                    cunits = New String() {"kW", "kcal/h", "BTU/h", "BTU/s", "cal/s", "HP", "kJ/h", "kJ/d", "MW", "W"}
                Case ColumnSpec.SpecType.Product_Mass_Flow_Rate
                    cunits = New String() {"g/s", "lbm/h", "kg/s", "kg/h", "kg/d", "kg/min", "lb/min", "lb/s"}
                Case ColumnSpec.SpecType.Product_Molar_Flow_Rate
                    cunits = New String() {"mol/s", "lbmol/h", "mol/h", "mol/d", "kmol/s", "kmol/h", "kmol/d", "m3/d @ BR", "m3/d @ NC", "m3/d @ CNTP", "m3/d @ SC", "m3/d @ 0 C, 1 atm", "m3/d @ 15.56 C, 1 atm", "m3/d @ 20 C, 1 atm", "ft3/d @ 60 F, 14.7 psia", "ft3/d @ 0 C, 1 atm"}
                Case ColumnSpec.SpecType.Stream_Ratio
                    cunits = New String() {""}
                Case ColumnSpec.SpecType.Temperature
                    cunits = New String() {"K", "R", "C", "F"}
            End Select
            cbCondSpecUnits.Items.Clear()
            cbCondSpecUnits.Items.AddRange(cunits)
            cbCondSpecUnits.SelectedItem = .Specs("C").SpecUnit
            tbCondSpec.Text = su.Converter.ConvertFromSI(.Specs("C").SpecUnit, .Specs("C").SpecValue).ToString(nf)
            cbCondVapFlowUnits.Items.Clear()
            cbCondVapFlowUnits.Items.AddRange(New String() {"mol/s", "lbmol/h", "mol/h", "mol/d", "kmol/s", "kmol/h", "kmol/d", "m3/d @ BR", "m3/d @ NC", "m3/d @ CNTP", "m3/d @ SC", "m3/d @ 0 C, 1 atm", "m3/d @ 15.56 C, 1 atm", "m3/d @ 20 C, 1 atm", "ft3/d @ 60 F, 14.7 psia", "ft3/d @ 0 C, 1 atm"})
            cbCondVapFlowUnits.SelectedItem = .VaporFlowRateUnit
            tbCondVapFlow.Text = su.Converter.ConvertFromSI(.VaporFlowRateUnit, .VaporFlowRate).ToString(nf)

            tbRebPressure.Text = su.Converter.ConvertFromSI(units.pressure, .ReboilerPressure).ToString(nf)
            cbRebSpec.SelectedIndex = .Specs("R").SType
            Dim runits As String() = {}
            Select Case .Specs("R").SType
                Case ColumnSpec.SpecType.Component_Fraction
                    runits = New String() {"M", "We"}
                Case ColumnSpec.SpecType.Component_Mass_Flow_Rate
                    runits = New String() {"g/s", "lbm/h", "kg/s", "kg/h", "kg/d", "kg/min", "lb/min", "lb/s"}
                Case ColumnSpec.SpecType.Component_Molar_Flow_Rate
                    runits = New String() {"mol/s", "lbmol/h", "mol/h", "mol/d", "kmol/s", "kmol/h", "kmol/d", "m3/d @ BR", "m3/d @ NC", "m3/d @ CNTP", "m3/d @ SC", "m3/d @ 0 C, 1 atm", "m3/d @ 15.56 C, 1 atm", "m3/d @ 20 C, 1 atm", "ft3/d @ 60 F, 14.7 psia", "ft3/d @ 0 C, 1 atm"}
                Case ColumnSpec.SpecType.Component_Recovery
                    runits = New String() {"% M/M", "% W/W"}
                Case ColumnSpec.SpecType.Heat_Duty
                    runits = New String() {"kW", "kcal/h", "BTU/h", "BTU/s", "cal/s", "HP", "kJ/h", "kJ/d", "MW", "W"}
                Case ColumnSpec.SpecType.Product_Mass_Flow_Rate
                    runits = New String() {"g/s", "lbm/h", "kg/s", "kg/h", "kg/d", "kg/min", "lb/min", "lb/s"}
                Case ColumnSpec.SpecType.Product_Molar_Flow_Rate
                    runits = New String() {"mol/s", "lbmol/h", "mol/h", "mol/d", "kmol/s", "kmol/h", "kmol/d", "m3/d @ BR", "m3/d @ NC", "m3/d @ CNTP", "m3/d @ SC", "m3/d @ 0 C, 1 atm", "m3/d @ 15.56 C, 1 atm", "m3/d @ 20 C, 1 atm", "ft3/d @ 60 F, 14.7 psia", "ft3/d @ 0 C, 1 atm"}
                Case ColumnSpec.SpecType.Stream_Ratio
                    runits = New String() {""}
                Case ColumnSpec.SpecType.Temperature
                    runits = New String() {"K", "R", "C", "F"}
            End Select
            cbRebSpecUnits.Items.Clear()
            cbRebSpecUnits.Items.AddRange(cunits)
            cbRebSpecUnits.SelectedItem = .Specs("R").SpecUnit
            tbRebSpecValue.Text = su.Converter.ConvertFromSI(.Specs("R").SpecUnit, .Specs("R").SpecValue).ToString(nf)

            chkUseIE_T.Checked = .UseTemperatureEstimates
            chkUseIE_LF.Checked = .UseLiquidFlowEstimates
            chkUseIE_VF.Checked = .UseVaporFlowEstimates
            chkUseIE_C.Checked = .UseCompositionEstimates

            tbBPStopAtIter.Text = .StopAtIterationNumber
            tbNSMaximumDeltaT.Text = su.Converter.ConvertFromSI(units.deltaT, .SC_MaximumTemperatureChange).ToString(nf)
            tbNSNumericalDerivativeStep.Text = .SC_NumericalDerivativeStep
            chkNSJacobian.Checked = .StoreAndReuseJacobian
            chkNSUseDampingFactor.Checked = .UseDampingFactor
            chkNSUseNewton.Checked = .UseNewtonUpdate

            tbIONumericalDerivativeStep.Text = .IO_NumericalDerivativeStep
            tbIOTempPerturbation.Text = su.Converter.ConvertFromSI(units.deltaT, .IO_ExtLoop_DeltaT).ToString(nf)
            tbIOMinDamping.Text = .IO_DampingFactorMin
            tbIOMaxDamping.Text = .IO_DampingFactorMax
            chkIOAdjustSb.Checked = .AdjustSb
            chkIOAverageKb.Checked = .KbjWeightedAverage
            chkIOJacobian.Checked = .StoreAndReuseJacobian
            chkIONewton.Checked = .UseNewtonUpdate
            chkIOUseDampingFactor.Checked = .UseDampingFactor

            'tabs

            TabStages.Controls.Clear()
            Dim seditor As New EditingForm_Column_Stages With {.dc = Me.SimObject}
            seditor.Dock = DockStyle.Fill
            TabStages.Controls.Add(seditor)

            TabConnections.Controls.Clear()
            Dim ceditor As New EditingForm_Column_Connections With {.dc = Me.SimObject}
            ceditor.Dock = DockStyle.Fill
            TabConnections.Controls.Add(ceditor)

            TabInitialEstimates.Controls.Clear()
            Dim ieditor As New EditingForm_Column_InitialEstimates With {.dc = Me.SimObject}
            ieditor.Dock = DockStyle.Fill
            TabInitialEstimates.Controls.Add(ieditor)

            TabResults.Controls.Clear()
            Dim reditor As New EditingForm_Column_Results With {.dc = Me.SimObject}
            reditor.Dock = DockStyle.Fill
            TabResults.Controls.Add(reditor)

            'results

            gridResults.Rows.Clear()
            Select Case .ColumnType
                Case ColType.DistillationColumn
                    gridResults.Rows.Add(New Object() {.FlowSheet.GetTranslatedString("DCCondenserDuty"), su.Converter.ConvertFromSI(units.heatflow, .CondenserDuty).ToString(nf), units.deltaP})
                    gridResults.Rows.Add(New Object() {.FlowSheet.GetTranslatedString("DCReboilerDuty"), su.Converter.ConvertFromSI(units.heatflow, .ReboilerDuty).ToString(nf), units.deltaT})
                Case ColType.AbsorptionColumn
                Case ColType.ReboiledAbsorber
                    gridResults.Rows.Add(New Object() {.FlowSheet.GetTranslatedString("DCReboilerDuty"), su.Converter.ConvertFromSI(units.heatflow, .ReboilerDuty).ToString(nf), units.deltaT})
                Case ColType.RefluxedAbsorber
                    gridResults.Rows.Add(New Object() {.FlowSheet.GetTranslatedString("DCCondenserDuty"), su.Converter.ConvertFromSI(units.heatflow, .CondenserDuty).ToString(nf), units.deltaP})
            End Select

            'property package

            Dim proppacks As String() = .FlowSheet.PropertyPackages.Values.Select(Function(m) m.Tag).ToArray
            cbPropPack.Items.Clear()
            cbPropPack.Items.AddRange(proppacks)
            cbPropPack.SelectedItem = .PropertyPackage.Tag

            Dim flashalgos As String() = .FlowSheet.FlowsheetOptions.FlashAlgorithms.Select(Function(x) x.Tag).ToArray
            cbFlashAlg.Items.Clear()
            cbFlashAlg.Items.AddRange(flashalgos)
            cbFlashAlg.SelectedItem = .PreferredFlashAlgorithmTag

        End With

        Loaded = True

    End Sub

    Private Sub btnConfigurePP_Click(sender As Object, e As EventArgs) Handles btnConfigurePP.Click
        SimObject.FlowSheet.PropertyPackages.Values.Where(Function(x) x.Tag = cbPropPack.SelectedItem.ToString).SingleOrDefault.DisplayEditingForm()
    End Sub

    Private Sub btnConfigureFlashAlg_Click(sender As Object, e As EventArgs) Handles btnConfigureFlashAlg.Click

        Thermodynamics.Calculator.ConfigureFlashInstance(SimObject, cbFlashAlg.SelectedItem.ToString)

    End Sub

    Private Sub lblTag_TextChanged(sender As Object, e As EventArgs) Handles lblTag.TextChanged
        If Loaded Then SimObject.GraphicObject.Tag = lblTag.Text
        Me.Text = SimObject.GetDisplayName() & ": " & SimObject.GraphicObject.Tag
    End Sub

    Sub RequestCalc()

        SimObject.FlowSheet.RequestCalculation(SimObject)

    End Sub

    Private Sub cbPropPack_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbPropPack.SelectedIndexChanged
        If Loaded Then
            SimObject.PropertyPackage = SimObject.FlowSheet.PropertyPackages.Values.Where(Function(x) x.Tag = cbPropPack.SelectedItem.ToString).SingleOrDefault
            RequestCalc()
        End If
    End Sub

    Private Sub cbFlashAlg_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbFlashAlg.SelectedIndexChanged
        If Loaded Then
            SimObject.PreferredFlashAlgorithmTag = cbFlashAlg.SelectedItem.ToString
            RequestCalc()
        End If
    End Sub

    Private Sub chkActive_CheckedChanged(sender As Object, e As EventArgs) Handles chkActive.CheckedChanged
        If Loaded Then SimObject.GraphicObject.Active = chkActive.Checked
    End Sub

    Private Sub tb_TextChanged(sender As Object, e As EventArgs)

        Dim tbox = DirectCast(sender, TextBox)

        If Double.TryParse(tbox.Text, New Double()) Then
            tbox.ForeColor = Drawing.Color.Blue
        Else
            tbox.ForeColor = Drawing.Color.Red
        End If

    End Sub

    Private Sub TextBoxKeyDown(sender As Object, e As KeyEventArgs) Handles tbNStages.KeyDown

        If e.KeyCode = Keys.Enter And Loaded And DirectCast(sender, TextBox).ForeColor = Drawing.Color.Blue Then

            UpdateProps(sender)

            DirectCast(sender, TextBox).SelectAll()

        End If

    End Sub

    Sub UpdateProps(sender As Object)

        'If sender Is tbOutletTemperature Then SimObject.OutletTemperature = su.Converter.ConvertToSI(cbTemp.SelectedItem.ToString, tbOutletTemperature.Text)
        'If sender Is tbOutletPressure Then SimObject.OutletPressure = su.Converter.ConvertToSI(cbPressure.SelectedItem.ToString, tbOutletPressure.Text)

        RequestCalc()

    End Sub

    Private Sub cbTemp_SelectedIndexChanged(sender As Object, e As EventArgs)

        'If Loaded Then
        '    Try
        '        If sender Is cbTemp Then
        '            tbOutletTemperature.Text = su.Converter.Convert(cbTemp.SelectedItem.ToString, units.temperature, Double.Parse(tbOutletTemperature.Text)).ToString(nf)
        '            cbTemp.SelectedItem = units.temperature
        '            UpdateProps(tbOutletTemperature)
        '        ElseIf sender Is cbPressure Then
        '            tbOutletPressure.Text = su.Converter.Convert(cbPressure.SelectedItem.ToString, units.pressure, Double.Parse(tbOutletPressure.Text)).ToString(nf)
        '            cbPressure.SelectedItem = units.pressure
        '            UpdateProps(tbOutletPressure)
        '        End If
        '    Catch ex As Exception
        '        SimObject.FlowSheet.ShowMessage(ex.Message.ToString, Interfaces.IFlowsheet.MessageType.GeneralError)
        '    End Try
        'End If

    End Sub

End Class