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
            ElseIf .SolvingMethod = 2 Then
                TabControl2.TabPages.Remove(TabSolverBP)
                TabControl2.TabPages.Remove(TabSolverNS)
            Else
                TabControl2.TabPages.Remove(TabSolverIO)
                TabControl2.TabPages.Remove(TabSolverBP)
                TabControl2.TabPages.Remove(TabSolverNS)
            End If

            'parameters

            cbCondPressureUnits.Items.Clear()
            cbCondPressureUnits.Items.AddRange(units.GetUnitSet(Interfaces.Enums.UnitOfMeasure.pressure).ToArray)
            cbCondPressureUnits.SelectedItem = units.pressure

            cbRebPressure.Items.Clear()
            cbRebPressure.Items.AddRange(units.GetUnitSet(Interfaces.Enums.UnitOfMeasure.pressure).ToArray)
            cbRebPressure.SelectedItem = units.pressure

            cbCondPDropUnits.Items.Clear()
            cbCondPDropUnits.Items.AddRange(units.GetUnitSet(Interfaces.Enums.UnitOfMeasure.deltaP).ToArray)
            cbCondPDropUnits.SelectedItem = units.deltaP

            cbIOTempPerturbationUnits.Items.Clear()
            cbIOTempPerturbationUnits.Items.AddRange(units.GetUnitSet(Interfaces.Enums.UnitOfMeasure.deltaT).ToArray)
            cbIOTempPerturbationUnits.SelectedItem = units.deltaT

            cbNSMaximumDeltaT.Items.Clear()
            cbNSMaximumDeltaT.Items.AddRange(units.GetUnitSet(Interfaces.Enums.UnitOfMeasure.deltaT).ToArray)
            cbNSMaximumDeltaT.SelectedItem = units.deltaT

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

            InitialEstimatesPanel.Controls.Clear()
            Dim ieditor As New EditingForm_Column_InitialEstimates With {.dc = Me.SimObject}
            ieditor.Dock = DockStyle.Fill
            InitialEstimatesPanel.Controls.Add(ieditor)

            'results

            gridResults.Rows.Clear()
            Select Case .ColumnType
                Case ColType.DistillationColumn
                    gridResults.Rows.Add(New Object() {.FlowSheet.GetTranslatedString("DCCondenserDuty"), su.Converter.ConvertFromSI(units.heatflow, .CondenserDuty).ToString(nf), units.heatflow})
                    gridResults.Rows.Add(New Object() {.FlowSheet.GetTranslatedString("DCReboilerDuty"), su.Converter.ConvertFromSI(units.heatflow, .ReboilerDuty).ToString(nf), units.heatflow})
                Case ColType.AbsorptionColumn
                Case ColType.ReboiledAbsorber
                    gridResults.Rows.Add(New Object() {.FlowSheet.GetTranslatedString("DCReboilerDuty"), su.Converter.ConvertFromSI(units.heatflow, .ReboilerDuty).ToString(nf), units.heatflow})
                Case ColType.RefluxedAbsorber
                    gridResults.Rows.Add(New Object() {.FlowSheet.GetTranslatedString("DCCondenserDuty"), su.Converter.ConvertFromSI(units.heatflow, .CondenserDuty).ToString(nf), units.heatflow})
            End Select
            gridResults.Rows.Add(New Object() {.FlowSheet.GetTranslatedString("DCILIts"), .ic, ""})
            gridResults.Rows.Add(New Object() {.FlowSheet.GetTranslatedString("DCELIts"), .ec, ""})

            btnResults.Enabled = .x0.Count > 0

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

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles btnResults.Click

        Dim d As New WeifenLuo.WinFormsUI.Docking.DockContent()

        Dim reditor As New EditingForm_Column_Results With {.dc = Me.SimObject}
        reditor.Dock = DockStyle.Fill
        d.Text = SimObject.GetDisplayName() & ": " & SimObject.GraphicObject.Tag
        d.TabText = d.Text
        d.Controls.Add(reditor)
        d.ShowHint = WeifenLuo.WinFormsUI.Docking.DockState.Document
        SimObject.FlowSheet.DisplayForm(d)

    End Sub

    Private Sub tbNStages_TextChanged(sender As Object, e As KeyEventArgs) Handles tbNStages.KeyDown

        If Loaded And e.KeyCode = Keys.Enter Then

            SimObject.NumberOfStages = tbNStages.Text

            Dim ne As Integer = SimObject.NumberOfStages

            Dim nep As Integer = SimObject.Stages.Count

            Dim dif As Integer = ne - nep

            If dif < 0 Then
                SimObject.Stages.RemoveRange(nep + dif - 1, -dif)
                With SimObject.InitialEstimates
                    .LiqCompositions.RemoveRange(nep + dif - 1, -dif)
                    .VapCompositions.RemoveRange(nep + dif - 1, -dif)
                    .LiqMolarFlows.RemoveRange(nep + dif - 1, -dif)
                    .VapMolarFlows.RemoveRange(nep + dif - 1, -dif)
                    .StageTemps.RemoveRange(nep + dif - 1, -dif)
                End With
            ElseIf dif > 0 Then
                Dim i As Integer
                For i = 1 To dif
                    SimObject.Stages.Insert(SimObject.Stages.Count - 1, New Stage(Guid.NewGuid().ToString))
                    SimObject.Stages(SimObject.Stages.Count - 2).Name = SimObject.FlowSheet.GetTranslatedString("DCStage") & "_" & SimObject.Stages.Count - 2
                    With SimObject.InitialEstimates
                        Dim d As New Dictionary(Of String, Parameter)
                        For Each cp In SimObject.FlowSheet.SelectedCompounds.Values
                            d.Add(cp.Name, New Parameter)
                        Next
                        .LiqCompositions.Insert(.LiqCompositions.Count - 1, d)
                        .VapCompositions.Insert(.VapCompositions.Count - 1, d)
                        .LiqMolarFlows.Insert(.LiqMolarFlows.Count - 1, New Parameter)
                        .VapMolarFlows.Insert(.VapMolarFlows.Count - 1, New Parameter)
                        .StageTemps.Insert(.StageTemps.Count - 1, New Parameter)
                    End With
                Next
            End If

            UpdateInfo()

        End If

    End Sub

    Private Sub tbMaxIt_TextChanged(sender As Object, e As KeyEventArgs) Handles tbMaxIt.KeyDown

        If Loaded And e.KeyCode = Keys.Enter Then

            SimObject.MaxIterations = tbMaxIt.Text
            RequestCalc()

        End If

    End Sub

    Private Sub tbConvTol_TextChanged(sender As Object, e As KeyEventArgs) Handles tbConvTol.KeyDown

        If Loaded And e.KeyCode = Keys.Enter Then

            SimObject.ExternalLoopTolerance = tbMaxIt.Text
            SimObject.InternalLoopTolerance = tbMaxIt.Text
            RequestCalc()

        End If

    End Sub

    Private Sub cbAbsorberMode_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbAbsorberMode.SelectedIndexChanged

        If Loaded Then

            DirectCast(SimObject, AbsorptionColumn).OperationMode = cbAbsorberMode.SelectedIndex

        End If

    End Sub

    Private Sub cbSolvingMethod_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbSolvingMethod.SelectedIndexChanged

        If Loaded Then

            SimObject.SolvingMethod = cbSolvingMethod.SelectedIndex

            If SimObject.SolvingMethod = 0 Then
                If Not TabControl2.TabPages.Contains(TabSolverBP) Then TabControl2.TabPages.Add(TabSolverBP)
                If TabControl2.TabPages.Contains(TabSolverIO) Then TabControl2.TabPages.Remove(TabSolverIO)
                If TabControl2.TabPages.Contains(TabSolverNS) Then TabControl2.TabPages.Remove(TabSolverNS)
            ElseIf SimObject.SolvingMethod = 1 Then
                If Not TabControl2.TabPages.Contains(TabSolverNS) Then TabControl2.TabPages.Add(TabSolverNS)
                If TabControl2.TabPages.Contains(TabSolverBP) Then TabControl2.TabPages.Remove(TabSolverBP)
                If TabControl2.TabPages.Contains(TabSolverIO) Then TabControl2.TabPages.Remove(TabSolverIO)
            ElseIf SimObject.SolvingMethod = 2 Then
                If Not TabControl2.TabPages.Contains(TabSolverIO) Then TabControl2.TabPages.Add(TabSolverIO)
                If TabControl2.TabPages.Contains(TabSolverBP) Then TabControl2.TabPages.Remove(TabSolverBP)
                If TabControl2.TabPages.Contains(TabSolverNS) Then TabControl2.TabPages.Remove(TabSolverNS)
            Else
                If TabControl2.TabPages.Contains(TabSolverIO) Then TabControl2.TabPages.Remove(TabSolverIO)
                If TabControl2.TabPages.Contains(TabSolverBP) Then TabControl2.TabPages.Remove(TabSolverBP)
                If TabControl2.TabPages.Contains(TabSolverNS) Then TabControl2.TabPages.Remove(TabSolverNS)
            End If

        End If

    End Sub

    Private Sub cbCondType_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbCondType.SelectedIndexChanged

        SimObject.CondenserType = cbCondType.SelectedIndex

        If SimObject.CondenserType = condtype.Partial_Condenser Then
            tbCondVapFlow.Enabled = True
            cbCondVapFlowUnits.Enabled = True
        Else
            tbCondVapFlow.Enabled = False
            cbCondVapFlowUnits.Enabled = False
        End If

    End Sub

    Private Sub cbCondSpec_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbCondSpec.SelectedIndexChanged

        If Loaded Then

            SimObject.Specs("C").SType = cbCondSpec.SelectedIndex
            Dim cunits As String() = {}
            Select Case SimObject.Specs("C").SType
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
            cbCondSpecUnits.SelectedItem = Nothing

        End If

    End Sub

    Private Sub tbCondPressure_TextChanged(sender As Object, e As KeyEventArgs) Handles tbCondPressure.KeyDown

        If Loaded And e.KeyCode = Keys.Enter Then

            SimObject.CondenserPressure = su.Converter.ConvertToSI(units.pressure, tbCondPressure.Text)

            RequestCalc()

        End If

    End Sub

    Private Sub tbCondPDrop_TextChanged(sender As Object, e As KeyEventArgs) Handles tbCondPDrop.KeyDown

        If Loaded And e.KeyCode = Keys.Enter Then

            SimObject.CondenserDeltaP = su.Converter.ConvertToSI(units.deltaP, tbCondPDrop.Text)

            RequestCalc()

        End If

    End Sub

    Private Sub tbCondSpec_TextChanged(sender As Object, e As KeyEventArgs) Handles tbCondSpec.KeyDown

        If Loaded And e.KeyCode = Keys.Enter Then

            SimObject.Specs("C").SpecValue = su.Converter.ConvertToSI(SimObject.Specs("C").SpecUnit, tbCondSpec.Text)

            RequestCalc()

        End If

    End Sub

    Private Sub cbCondSpecUnits_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbCondSpecUnits.SelectedIndexChanged

        If Loaded Then

            SimObject.Specs("C").SpecUnit = cbCondSpecUnits.SelectedItem.ToString

            RequestCalc()

        End If

    End Sub

    Private Sub tbCondVapFlow_TextChanged(sender As Object, e As KeyEventArgs) Handles tbCondVapFlow.KeyDown

        If Loaded And e.KeyCode = Keys.Enter Then

            SimObject.VaporFlowRate = su.Converter.ConvertToSI(SimObject.VaporFlowRateUnit, tbCondVapFlow.Text)

            RequestCalc()

        End If

    End Sub

    Private Sub cbCondVapFlowUnits_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbCondVapFlowUnits.SelectedIndexChanged

        If Loaded Then

            SimObject.VaporFlowRateUnit = cbCondVapFlowUnits.SelectedItem.ToString

            RequestCalc()

        End If

    End Sub

    Private Sub tbRebPressure_TextChanged(sender As Object, e As KeyEventArgs) Handles tbRebPressure.KeyDown

        If Loaded And e.KeyCode = Keys.Enter Then

            SimObject.ReboilerPressure = su.Converter.ConvertToSI(units.pressure, tbRebPressure.Text)

            RequestCalc()

        End If

    End Sub

    Private Sub cbRebSpec_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbRebSpec.SelectedIndexChanged

        If Loaded Then

            SimObject.Specs("R").SType = cbRebSpec.SelectedIndex
            Dim cunits As String() = {}
            Select Case SimObject.Specs("R").SType
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
            cbRebSpec.Items.Clear()
            cbRebSpec.Items.AddRange(cunits)
            cbRebSpec.SelectedItem = Nothing

        End If

    End Sub

    Private Sub tbRebSpecValue_TextChanged(sender As Object, e As KeyEventArgs) Handles tbRebSpecValue.KeyDown

        If Loaded And e.KeyCode = Keys.Enter Then

            SimObject.Specs("R").SpecValue = su.Converter.ConvertToSI(SimObject.Specs("R").SpecUnit, tbRebSpecValue.Text)

            RequestCalc()

        End If

    End Sub

    Private Sub cbRebSpecUnits_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbRebSpecUnits.SelectedIndexChanged

        If Loaded Then

            SimObject.Specs("R").SpecUnit = cbRebSpecUnits.SelectedItem.ToString

            RequestCalc()

        End If

    End Sub

    Private Sub chkUseIE_T_CheckedChanged(sender As Object, e As EventArgs) Handles chkUseIE_T.CheckedChanged
        SimObject.UseTemperatureEstimates = chkUseIE_T.Checked
    End Sub

    Private Sub chkUseIE_LF_CheckedChanged(sender As Object, e As EventArgs) Handles chkUseIE_LF.CheckedChanged
        SimObject.UseLiquidFlowEstimates = chkUseIE_LF.Checked
    End Sub

    Private Sub chkUseIE_VF_CheckedChanged(sender As Object, e As EventArgs) Handles chkUseIE_VF.CheckedChanged
        SimObject.UseVaporFlowEstimates = chkUseIE_VF.Checked
    End Sub

    Private Sub chkUseIE_C_CheckedChanged(sender As Object, e As EventArgs) Handles chkUseIE_C.CheckedChanged
        SimObject.UseCompositionEstimates = chkUseIE_C.Checked
    End Sub

    Private Sub tbBPStopAtIter_TextChanged(sender As Object, e As KeyEventArgs) Handles tbBPStopAtIter.KeyDown
        If Loaded And e.KeyCode = Keys.Enter Then
            SimObject.StopAtIterationNumber = tbBPStopAtIter.Text
        End If
    End Sub

    Private Sub tbNSNumericalDerivativeStep_TextChanged(sender As Object, e As KeyEventArgs) Handles tbNSNumericalDerivativeStep.KeyDown
        If Loaded And e.KeyCode = Keys.Enter Then
            SimObject.SC_NumericalDerivativeStep = tbNSNumericalDerivativeStep.Text
        End If
    End Sub

    Private Sub tbIONumericalDerivativeStep_TextChanged(sender As Object, e As KeyEventArgs) Handles tbIONumericalDerivativeStep.KeyDown
        If Loaded And e.KeyCode = Keys.Enter Then
            SimObject.IO_NumericalDerivativeStep = tbIONumericalDerivativeStep.Text
        End If
    End Sub

    Private Sub tbNSMaximumDeltaT_TextChanged(sender As Object, e As KeyEventArgs) Handles tbNSMaximumDeltaT.KeyDown
        If Loaded And e.KeyCode = Keys.Enter Then
            SimObject.SC_MaximumTemperatureChange = su.Converter.ConvertToSI(units.temperature, tbNSMaximumDeltaT.Text)
        End If
    End Sub

    Private Sub chkNSUseDampingFactor_CheckedChanged(sender As Object, e As EventArgs) Handles chkNSUseDampingFactor.CheckedChanged
        SimObject.UseDampingFactor = chkNSUseDampingFactor.Checked
    End Sub

    Private Sub chkNSUseNewton_CheckedChanged(sender As Object, e As EventArgs) Handles chkNSUseNewton.CheckedChanged
        SimObject.UseNewtonUpdate = chkNSUseNewton.Checked
    End Sub

    Private Sub chkNSJacobian_CheckedChanged(sender As Object, e As EventArgs) Handles chkNSJacobian.CheckedChanged
        SimObject.StoreAndReuseJacobian = chkNSJacobian.Checked
    End Sub

    Private Sub tbIOTempPerturbation_TextChanged(sender As Object, e As KeyEventArgs) Handles tbIOTempPerturbation.KeyDown
        If Loaded And e.KeyCode = Keys.Enter Then
            SimObject.IO_ExtLoop_DeltaT = su.Converter.ConvertToSI(units.temperature, tbIOTempPerturbation.Text)
        End If
    End Sub

    Private Sub tbIOMinDamping_TextChanged(sender As Object, e As KeyEventArgs) Handles tbIOMinDamping.KeyDown
        If Loaded And e.KeyCode = Keys.Enter Then
            SimObject.IO_DampingFactorMin = tbIOMinDamping.Text
        End If
    End Sub

    Private Sub tbIOMaxDamping_TextChanged(sender As Object, e As KeyEventArgs) Handles tbIOMaxDamping.KeyDown
        If Loaded And e.KeyCode = Keys.Enter Then
            SimObject.IO_DampingFactorMax = tbIOMaxDamping.Text
        End If
    End Sub

    Private Sub chkIOUseDampingFactor_CheckedChanged(sender As Object, e As EventArgs) Handles chkIOUseDampingFactor.CheckedChanged
        SimObject.UseDampingFactor = chkIOUseDampingFactor.Checked
    End Sub

    Private Sub chkIONewton_CheckedChanged(sender As Object, e As EventArgs) Handles chkIONewton.CheckedChanged
        SimObject.UseNewtonUpdate = chkIONewton.Checked
    End Sub

    Private Sub chkIOJacobian_CheckedChanged(sender As Object, e As EventArgs) Handles chkIOJacobian.CheckedChanged
        SimObject.StoreAndReuseJacobian = chkIOJacobian.Checked
    End Sub

    Private Sub chkIOAdjustSb_CheckedChanged(sender As Object, e As EventArgs) Handles chkIOAdjustSb.CheckedChanged
        SimObject.AdjustSb = chkIOAdjustSb.Checked
    End Sub

    Private Sub chkIOAverageKb_CheckedChanged(sender As Object, e As EventArgs) Handles chkIOAverageKb.CheckedChanged
        SimObject.KbjWeightedAverage = chkIOAverageKb.Checked
    End Sub
End Class