Imports System.Windows.Forms
Imports DWSIM.Interfaces.Enums.GraphicObjects
Imports DWSIM.SharedClasses.UnitOperations
Imports su = DWSIM.SharedClasses.SystemsOfUnits
Imports DWSIM.UnitOperations.UnitOperations
Imports System.Drawing
Imports DWSIM.UnitOperations.UnitOperations.Column
Imports DWSIM.UnitOperations.UnitOperations.Auxiliary.SepOps

Public Class EditingForm_Column

    Inherits SharedClasses.ObjectEditorForm

    Public Property SimObject As UnitOperations.Column

    Public Loaded As Boolean = False

    Dim units As SharedClasses.SystemsOfUnits.Units
    Dim nf As String

    Friend tab1, tab2 As Integer

    Private Sub EditingForm_HeaterCooler_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        Me.ShowHint = GlobalSettings.Settings.DefaultEditFormLocation

        UpdateInfo()

    End Sub

    Sub UpdateInfo()

        units = SimObject.FlowSheet.FlowsheetOptions.SelectedUnitSystem
        nf = SimObject.FlowSheet.FlowsheetOptions.NumberFormat

        Loaded = False

        If Host.Items.Where(Function(x) x.Name.Contains(SimObject.GraphicObject.Tag)).Count > 0 Then
            If InspReportBar Is Nothing Then
                InspReportBar = New SharedClasses.InspectorReportBar
                InspReportBar.Dock = DockStyle.Bottom
                AddHandler InspReportBar.Button1.Click, Sub()
                                                            Dim iwindow As New Inspector.Window2
                                                            iwindow.SelectedObject = SimObject
                                                            iwindow.Show(DockPanel)
                                                        End Sub
                Me.Controls.Add(InspReportBar)
                InspReportBar.BringToFront()
            End If
        Else
            If InspReportBar IsNot Nothing Then
                Me.Controls.Remove(InspReportBar)
                InspReportBar = Nothing
            End If
        End If

        With SimObject

            'first block

            chkActive.Checked = .GraphicObject.Active

            ToolTip1.SetToolTip(chkActive, .FlowSheet.GetTranslatedString("AtivoInativo"))

            Me.Text = .GraphicObject.Tag & " (" & .GetDisplayName() & ")"

            lblTag.Text = .GraphicObject.Tag
            If .Calculated Then
                lblStatus.Text = .FlowSheet.GetTranslatedString("Calculado") & " (" & .LastUpdated.ToString & ")"
                lblStatus.ForeColor = System.Drawing.Color.Blue
            Else
                If Not .GraphicObject.Active Then
                    lblStatus.Text = .FlowSheet.GetTranslatedString("Inativo")
                    lblStatus.ForeColor = System.Drawing.Color.Gray
                ElseIf .ErrorMessage <> "" Then
                    lblStatus.Text = .FlowSheet.GetTranslatedString("Erro")
                    lblStatus.ForeColor = System.Drawing.Color.Red
                Else
                    lblStatus.Text = .FlowSheet.GetTranslatedString("NoCalculado")
                    lblStatus.ForeColor = System.Drawing.Color.Black
                End If
            End If

            lblConnectedTo.Text = ""

            If .IsSpecAttached Then lblConnectedTo.Text = .FlowSheet.SimulationObjects(.AttachedSpecId).GraphicObject.Tag
            If .IsAdjustAttached Then lblConnectedTo.Text = .FlowSheet.SimulationObjects(.AttachedAdjustId).GraphicObject.Tag

            If TypeOf SimObject Is DistillationColumn Then
                chkNoCondenser.Checked = DirectCast(SimObject, DistillationColumn).ReboiledAbsorber
                chkNoReboiler.Checked = DirectCast(SimObject, DistillationColumn).RefluxedAbsorber
            ElseIf TypeOf SimObject Is AbsorptionColumn Then
                TabContainerSpecification.TabPages.Remove(TabCondenser)
                TabContainerSpecification.TabPages.Remove(TabReboiler)
            ElseIf TypeOf SimObject Is RefluxedAbsorber Then
                TabContainerSpecification.TabPages.Remove(TabReboiler)
            ElseIf TypeOf SimObject Is ReboiledAbsorber Then
                TabContainerSpecification.TabPages.Remove(TabCondenser)
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

            tbNStages.Text = .NumberOfStages

            If TypeOf SimObject Is AbsorptionColumn Then cbAbsorberMode.SelectedIndex = DirectCast(SimObject, AbsorptionColumn).OperationMode Else cbAbsorberMode.Enabled = False

            tbMaxIt.Text = .MaxIterations
            tbConvTol.Text = .ExternalLoopTolerance.ToString("R")

            cbCondType.SelectedIndex = .CondenserType
            tbCondPressure.Text = su.Converter.ConvertFromSI(units.pressure, .CondenserPressure).ToString(nf)
            tbCondPDrop.Text = su.Converter.ConvertFromSI(units.deltaP, .CondenserDeltaP).ToString(nf)
            cbCondSpec.SelectedIndex = .Specs("C").SType
            Dim cunits As String() = {}
            Select Case .Specs("C").SType
                Case ColumnSpec.SpecType.Component_Fraction
                    cunits = New String() {"Molar", "Mass"}
                    cbCondComp.Enabled = True
                Case ColumnSpec.SpecType.Component_Mass_Flow_Rate
                    cunits = New String() {"g/s", "lbm/h", "kg/s", "kg/h", "kg/d", "kg/min", "lb/min", "lb/s"}
                    cbCondComp.Enabled = True
                Case ColumnSpec.SpecType.Component_Molar_Flow_Rate
                    cunits = New String() {"mol/s", "lbmol/h", "mol/h", "mol/d", "kmol/s", "kmol/h", "kmol/d", "m3/d @ BR", "m3/d @ NC", "m3/d @ CNTP", "m3/d @ SC", "m3/d @ 0 C, 1 atm", "m3/d @ 15.56 C, 1 atm", "m3/d @ 20 C, 1 atm", "ft3/d @ 60 F, 14.7 psia", "ft3/d @ 0 C, 1 atm"}
                    cbCondComp.Enabled = True
                Case ColumnSpec.SpecType.Component_Recovery
                    cunits = New String() {"% M/M", "% W/W"}
                    cbCondComp.Enabled = True
                Case ColumnSpec.SpecType.Heat_Duty
                    cunits = New String() {"kW", "kcal/h", "BTU/h", "BTU/s", "cal/s", "HP", "kJ/h", "kJ/d", "MW", "W"}
                    cbCondComp.Enabled = False
                Case ColumnSpec.SpecType.Product_Mass_Flow_Rate
                    cunits = New String() {"g/s", "lbm/h", "kg/s", "kg/h", "kg/d", "kg/min", "lb/min", "lb/s"}
                    cbCondComp.Enabled = False
                Case ColumnSpec.SpecType.Product_Molar_Flow_Rate
                    cunits = New String() {"mol/s", "lbmol/h", "mol/h", "mol/d", "kmol/s", "kmol/h", "kmol/d", "m3/d @ BR", "m3/d @ NC", "m3/d @ CNTP", "m3/d @ SC", "m3/d @ 0 C, 1 atm", "m3/d @ 15.56 C, 1 atm", "m3/d @ 20 C, 1 atm", "ft3/d @ 60 F, 14.7 psia", "ft3/d @ 0 C, 1 atm"}
                    cbCondComp.Enabled = False
                Case ColumnSpec.SpecType.Stream_Ratio
                    cunits = New String() {""}
                    cbCondComp.Enabled = False
                Case ColumnSpec.SpecType.Temperature
                    cunits = New String() {"K", "R", "C", "F"}
                    cbCondComp.Enabled = False
            End Select
            cbCondSpecUnits.Items.Clear()
            cbCondSpecUnits.Items.AddRange(cunits)
            cbCondSpecUnits.SelectedItem = .Specs("C").SpecUnit
            tbCondSpec.Text = .Specs("C").SpecValue.ToString(nf)
            cbCondVapFlowUnits.Items.Clear()
            cbCondVapFlowUnits.Items.AddRange(New String() {"mol/s", "lbmol/h", "mol/h", "mol/d", "kmol/s", "kmol/h", "kmol/d", "m3/d @ BR", "m3/d @ NC", "m3/d @ CNTP", "m3/d @ SC", "m3/d @ 0 C, 1 atm", "m3/d @ 15.56 C, 1 atm", "m3/d @ 20 C, 1 atm", "ft3/d @ 60 F, 14.7 psia", "ft3/d @ 0 C, 1 atm"})
            cbCondVapFlowUnits.SelectedItem = .VaporFlowRateUnit
            tbCondVapFlow.Text = su.Converter.ConvertFromSI(.VaporFlowRateUnit, .VaporFlowRate).ToString(nf)

            tbRebPressure.Text = su.Converter.ConvertFromSI(units.pressure, .ReboilerPressure).ToString(nf)
            cbRebSpec.SelectedIndex = .Specs("R").SType
            Dim runits As String() = {}
            Select Case .Specs("R").SType
                Case ColumnSpec.SpecType.Component_Fraction
                    runits = New String() {"Molar", "Mass"}
                    cbRebComp.Enabled = True
                Case ColumnSpec.SpecType.Component_Mass_Flow_Rate
                    runits = New String() {"g/s", "lbm/h", "kg/s", "kg/h", "kg/d", "kg/min", "lb/min", "lb/s"}
                    cbRebComp.Enabled = True
                Case ColumnSpec.SpecType.Component_Molar_Flow_Rate
                    runits = New String() {"mol/s", "lbmol/h", "mol/h", "mol/d", "kmol/s", "kmol/h", "kmol/d", "m3/d @ BR", "m3/d @ NC", "m3/d @ CNTP", "m3/d @ SC", "m3/d @ 0 C, 1 atm", "m3/d @ 15.56 C, 1 atm", "m3/d @ 20 C, 1 atm", "ft3/d @ 60 F, 14.7 psia", "ft3/d @ 0 C, 1 atm"}
                    cbRebComp.Enabled = True
                Case ColumnSpec.SpecType.Component_Recovery
                    runits = New String() {"% M/M", "% W/W"}
                    cbRebComp.Enabled = True
                Case ColumnSpec.SpecType.Heat_Duty
                    runits = New String() {"kW", "kcal/h", "BTU/h", "BTU/s", "cal/s", "HP", "kJ/h", "kJ/d", "MW", "W"}
                    cbRebComp.Enabled = False
                Case ColumnSpec.SpecType.Product_Mass_Flow_Rate
                    runits = New String() {"g/s", "lbm/h", "kg/s", "kg/h", "kg/d", "kg/min", "lb/min", "lb/s"}
                    cbRebComp.Enabled = False
                Case ColumnSpec.SpecType.Product_Molar_Flow_Rate
                    runits = New String() {"mol/s", "lbmol/h", "mol/h", "mol/d", "kmol/s", "kmol/h", "kmol/d", "m3/d @ BR", "m3/d @ NC", "m3/d @ CNTP", "m3/d @ SC", "m3/d @ 0 C, 1 atm", "m3/d @ 15.56 C, 1 atm", "m3/d @ 20 C, 1 atm", "ft3/d @ 60 F, 14.7 psia", "ft3/d @ 0 C, 1 atm"}
                    cbRebComp.Enabled = False
                Case ColumnSpec.SpecType.Stream_Ratio
                    runits = New String() {""}
                    cbRebComp.Enabled = False
                Case ColumnSpec.SpecType.Temperature
                    runits = New String() {"K", "R", "C", "F"}
                    cbRebComp.Enabled = False
            End Select
            cbRebSpecUnits.Items.Clear()
            cbRebSpecUnits.Items.AddRange(runits)
            cbRebSpecUnits.SelectedItem = .Specs("R").SpecUnit
            tbRebSpecValue.Text = .Specs("R").SpecValue.ToString(nf)

            cbCondComp.Items.Clear()
            cbCondComp.Items.AddRange(SimObject.FlowSheet.SelectedCompounds.Values.Select(Function(x) x.Name).ToArray)

            cbRebComp.Items.Clear()
            cbRebComp.Items.AddRange(SimObject.FlowSheet.SelectedCompounds.Values.Select(Function(x) x.Name).ToArray)

            If cbCondComp.Items.Contains(.Specs("C").ComponentID) Then cbCondComp.SelectedItem = .Specs("C").ComponentID
            If cbRebComp.Items.Contains(.Specs("R").ComponentID) Then cbRebComp.SelectedItem = .Specs("R").ComponentID

            chkUseIE_T.Checked = .UseTemperatureEstimates
            chkUseIE_LF.Checked = .UseLiquidFlowEstimates
            chkUseIE_VF.Checked = .UseVaporFlowEstimates
            chkUseIE_C.Checked = .UseCompositionEstimates

            'tabs

            TabStages.Controls.Clear()
            Dim seditor As New EditingForm_Column_Stages With {.dc = Me.SimObject}
            seditor.Dock = DockStyle.Fill
            TabStages.Controls.Add(seditor)

            TabConnections.Controls.Clear()
            Dim ceditor As New EditingForm_Column_Connections_New With {.rc = Me.SimObject, .ownerform = Me}
            ceditor.Dock = DockStyle.Fill
            TabConnections.Controls.Add(ceditor)
            ceditor.UpdateInfo()

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
                    gridResults.Rows.Add(New Object() { .FlowSheet.GetTranslatedString("CondenserSpecValue"), su.Converter.ConvertFromSI(.Specs("C").SpecUnit, .Specs("C").SpecValue).ToString(nf), .Specs("C").SpecUnit})
                    gridResults.Rows.Add(New Object() { .FlowSheet.GetTranslatedString("CondenserCalcValue"), su.Converter.ConvertFromSI(.Specs("C").SpecUnit, .Specs("C").CalculatedValue).ToString(nf), .Specs("C").SpecUnit})
                    gridResults.Rows.Add(New Object() { .FlowSheet.GetTranslatedString("ReboilerSpecValue"), su.Converter.ConvertFromSI(.Specs("R").SpecUnit, .Specs("R").SpecValue).ToString(nf), .Specs("R").SpecUnit})
                    gridResults.Rows.Add(New Object() { .FlowSheet.GetTranslatedString("ReboilerCalcValue"), su.Converter.ConvertFromSI(.Specs("R").SpecUnit, .Specs("R").CalculatedValue).ToString(nf), .Specs("R").SpecUnit})
                Case ColType.AbsorptionColumn
                Case ColType.ReboiledAbsorber
                    gridResults.Rows.Add(New Object() {.FlowSheet.GetTranslatedString("DCReboilerDuty"), su.Converter.ConvertFromSI(units.heatflow, .ReboilerDuty).ToString(nf), units.heatflow})
                Case ColType.RefluxedAbsorber
                    gridResults.Rows.Add(New Object() {.FlowSheet.GetTranslatedString("DCCondenserDuty"), su.Converter.ConvertFromSI(units.heatflow, .CondenserDuty).ToString(nf), units.heatflow})
            End Select
            gridResults.Rows.Add(New Object() {.FlowSheet.GetTranslatedString("DCILIts"), .ic, ""})

            btnResults.Enabled = .x0.Count > 0

            'property package

            Dim proppacks As String() = .FlowSheet.PropertyPackages.Values.Select(Function(m) m.Tag).ToArray
            cbPropPack.Items.Clear()
            cbPropPack.Items.AddRange(proppacks)
            cbPropPack.SelectedItem = .PropertyPackage?.Tag

        End With

        Loaded = True

    End Sub

    Private Sub btnConfigurePP_Click(sender As Object, e As EventArgs) Handles btnConfigurePP.Click
        SimObject.FlowSheet.PropertyPackages.Values.Where(Function(x) x.Tag = cbPropPack.SelectedItem.ToString).SingleOrDefault.DisplayGroupedEditingForm()
    End Sub

    Private Sub lblTag_TextChanged(sender As Object, e As EventArgs) Handles lblTag.TextChanged
        If Loaded Then ToolTipChangeTag.Show("Press ENTER to commit changes.", lblTag, New System.Drawing.Point(0, lblTag.Height + 3), 3000)

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

    Private Sub chkActive_CheckedChanged(sender As Object, e As EventArgs) Handles chkActive.CheckedChanged
        If Loaded Then SimObject.GraphicObject.Active = chkActive.Checked
    End Sub

    Private Sub tb_TextChanged(sender As Object, e As EventArgs)

        Dim tbox = DirectCast(sender, TextBox)

        If tbox.Text.IsValidDoubleExpression Then
            tbox.ForeColor = System.Drawing.Color.Blue
        Else
            tbox.ForeColor = System.Drawing.Color.Red
        End If

    End Sub

    Private Sub TextBoxKeyDown(sender As Object, e As KeyEventArgs) Handles tbNStages.KeyDown

        If e.KeyCode = Keys.Enter And Loaded And DirectCast(sender, TextBox).ForeColor = System.Drawing.Color.Blue Then

            UpdateProps(sender)

            DirectCast(sender, TextBox).SelectAll()

        End If

    End Sub

    Sub UpdateProps(sender As Object)

         RequestCalc()

    End Sub

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles btnResults.Click

        Dim reditor As New EditingForm_Column_Results With {.dc = Me.SimObject}
        reditor.Text = SimObject.GetDisplayName() & ": " & SimObject.GraphicObject.Tag
        reditor.TabText = reditor.Text
        reditor.ShowHint = WeifenLuo.WinFormsUI.Docking.DockState.Document
        SimObject.FlowSheet.DisplayForm(reditor)

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

            SimObject.MaxIterations = Convert.ToInt32(Double.Parse(tbMaxIt.Text))

        End If

    End Sub

    Private Sub tbConvTol_TextChanged(sender As Object, e As KeyEventArgs) Handles tbConvTol.KeyDown

        If Loaded And e.KeyCode = Keys.Enter Then

            SimObject.ExternalLoopTolerance = Double.Parse(tbConvTol.Text)
            SimObject.InternalLoopTolerance = Double.Parse(tbConvTol.Text)

        End If

    End Sub

    Private Sub cbAbsorberMode_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbAbsorberMode.SelectedIndexChanged

        If Loaded Then

            DirectCast(SimObject, AbsorptionColumn).OperationMode = cbAbsorberMode.SelectedIndex

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

        SimObject.Specs("C").SType = cbCondSpec.SelectedIndex
            Dim cunits As String() = {}
            Select Case SimObject.Specs("C").SType
                Case ColumnSpec.SpecType.Component_Fraction
                cunits = New String() {"Molar", "Mass"}
                cbCondComp.Enabled = True
                Case ColumnSpec.SpecType.Component_Mass_Flow_Rate
                    cunits = New String() {"g/s", "lbm/h", "kg/s", "kg/h", "kg/d", "kg/min", "lb/min", "lb/s"}
                    cbCondComp.Enabled = True
                Case ColumnSpec.SpecType.Component_Molar_Flow_Rate
                    cunits = New String() {"mol/s", "lbmol/h", "mol/h", "mol/d", "kmol/s", "kmol/h", "kmol/d", "m3/d @ BR", "m3/d @ NC", "m3/d @ CNTP", "m3/d @ SC", "m3/d @ 0 C, 1 atm", "m3/d @ 15.56 C, 1 atm", "m3/d @ 20 C, 1 atm", "ft3/d @ 60 F, 14.7 psia", "ft3/d @ 0 C, 1 atm"}
                    cbCondComp.Enabled = True
                Case ColumnSpec.SpecType.Component_Recovery
                    cunits = New String() {"% M/M", "% W/W"}
                    cbCondComp.Enabled = True
                Case ColumnSpec.SpecType.Heat_Duty
                    cunits = New String() {"kW", "kcal/h", "BTU/h", "BTU/s", "cal/s", "HP", "kJ/h", "kJ/d", "MW", "W"}
                    cbCondComp.Enabled = False
                Case ColumnSpec.SpecType.Product_Mass_Flow_Rate
                    cunits = New String() {"g/s", "lbm/h", "kg/s", "kg/h", "kg/d", "kg/min", "lb/min", "lb/s"}
                    cbCondComp.Enabled = False
                Case ColumnSpec.SpecType.Product_Molar_Flow_Rate
                    cunits = New String() {"mol/s", "lbmol/h", "mol/h", "mol/d", "kmol/s", "kmol/h", "kmol/d", "m3/d @ BR", "m3/d @ NC", "m3/d @ CNTP", "m3/d @ SC", "m3/d @ 0 C, 1 atm", "m3/d @ 15.56 C, 1 atm", "m3/d @ 20 C, 1 atm", "ft3/d @ 60 F, 14.7 psia", "ft3/d @ 0 C, 1 atm"}
                    cbCondComp.Enabled = False
                Case ColumnSpec.SpecType.Stream_Ratio
                    cunits = New String() {""}
                    cbCondComp.Enabled = False
                Case ColumnSpec.SpecType.Temperature
                    cunits = New String() {"K", "R", "C", "F"}
                    cbCondComp.Enabled = False
            End Select
            cbCondSpecUnits.Items.Clear()
            cbCondSpecUnits.Items.AddRange(cunits)
            cbCondSpecUnits.SelectedItem = Nothing

    End Sub

    Private Sub tbCondPressure_TextChanged(sender As Object, e As KeyEventArgs) Handles tbCondPressure.KeyDown

        If Loaded And e.KeyCode = Keys.Enter Then

            SimObject.CondenserPressure = su.Converter.ConvertToSI(units.pressure, tbCondPressure.Text)
            SimObject.Stages(0).P = SimObject.CondenserPressure

            UpdateInfo()
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

            SimObject.Specs("C").SpecValue = Double.Parse(tbCondSpec.Text)

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

            SimObject.VaporFlowRate = su.Converter.ConvertToSI(SimObject.VaporFlowRateUnit, tbCondVapFlow.Text.ParseExpressionToDouble)

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

            SimObject.ReboilerPressure = su.Converter.ConvertToSI(units.pressure, tbRebPressure.Text.ParseExpressionToDouble)
            SimObject.Stages(SimObject.Stages.Count - 1).P = SimObject.ReboilerPressure

            RequestCalc()

        End If

    End Sub

    Private Sub cbRebSpec_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbRebSpec.SelectedIndexChanged

            SimObject.Specs("R").SType = cbRebSpec.SelectedIndex
            Dim cunits As String() = {}
            Select Case SimObject.Specs("R").SType
                Case ColumnSpec.SpecType.Component_Fraction
                cunits = New String() {"Molar", "Mass"}
                cbRebComp.Enabled = True
                Case ColumnSpec.SpecType.Component_Mass_Flow_Rate
                    cunits = New String() {"g/s", "lbm/h", "kg/s", "kg/h", "kg/d", "kg/min", "lb/min", "lb/s"}
                    cbRebComp.Enabled = True
                Case ColumnSpec.SpecType.Component_Molar_Flow_Rate
                    cunits = New String() {"mol/s", "lbmol/h", "mol/h", "mol/d", "kmol/s", "kmol/h", "kmol/d", "m3/d @ BR", "m3/d @ NC", "m3/d @ CNTP", "m3/d @ SC", "m3/d @ 0 C, 1 atm", "m3/d @ 15.56 C, 1 atm", "m3/d @ 20 C, 1 atm", "ft3/d @ 60 F, 14.7 psia", "ft3/d @ 0 C, 1 atm"}
                    cbRebComp.Enabled = True
                Case ColumnSpec.SpecType.Component_Recovery
                    cunits = New String() {"% M/M", "% W/W"}
                    cbRebComp.Enabled = True
                Case ColumnSpec.SpecType.Heat_Duty
                    cunits = New String() {"kW", "kcal/h", "BTU/h", "BTU/s", "cal/s", "HP", "kJ/h", "kJ/d", "MW", "W"}
                    cbRebComp.Enabled = False
                Case ColumnSpec.SpecType.Product_Mass_Flow_Rate
                    cunits = New String() {"g/s", "lbm/h", "kg/s", "kg/h", "kg/d", "kg/min", "lb/min", "lb/s"}
                    cbRebComp.Enabled = False
                Case ColumnSpec.SpecType.Product_Molar_Flow_Rate
                    cunits = New String() {"mol/s", "lbmol/h", "mol/h", "mol/d", "kmol/s", "kmol/h", "kmol/d", "m3/d @ BR", "m3/d @ NC", "m3/d @ CNTP", "m3/d @ SC", "m3/d @ 0 C, 1 atm", "m3/d @ 15.56 C, 1 atm", "m3/d @ 20 C, 1 atm", "ft3/d @ 60 F, 14.7 psia", "ft3/d @ 0 C, 1 atm"}
                    cbRebComp.Enabled = False
                Case ColumnSpec.SpecType.Stream_Ratio
                    cunits = New String() {""}
                    cbRebComp.Enabled = False
                Case ColumnSpec.SpecType.Temperature
                    cunits = New String() {"K", "R", "C", "F"}
                    cbRebComp.Enabled = False
            End Select
            cbRebSpecUnits.Items.Clear()
            cbRebSpecUnits.Items.AddRange(cunits)
            cbRebSpecUnits.SelectedItem = Nothing

    End Sub

    Private Sub tbRebSpecValue_TextChanged(sender As Object, e As KeyEventArgs) Handles tbRebSpecValue.KeyDown

        If Loaded And e.KeyCode = Keys.Enter Then

            SimObject.Specs("R").SpecValue = Double.Parse(tbRebSpecValue.Text.ParseExpressionToDouble)

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

    Private Sub cbCondComp_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbCondComp.SelectedIndexChanged
        If Loaded Then
            Me.SimObject.Specs("C").ComponentID = cbCondComp.SelectedItem.ToString
        End If
    End Sub

    Private Sub cbRebComp_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbRebComp.SelectedIndexChanged
        If Loaded Then
            Me.SimObject.Specs("R").ComponentID = cbRebComp.SelectedItem.ToString
        End If
    End Sub

    Private Sub tbNStages_TextChanged(sender As Object, e As EventArgs) Handles tbNStages.TextChanged, tbCondPDrop.TextChanged, tbCondPressure.TextChanged, tbCondSpec.TextChanged, tbCondVapFlow.TextChanged,
                                                                                tbConvTol.TextChanged, tbMaxIt.TextChanged, tbNStages.TextChanged, tbRebPressure.TextChanged, tbRebSpecValue.TextChanged
        Dim tbox = DirectCast(sender, TextBox)

        If Loaded Then
            ToolTip1.ToolTipTitle = SimObject.FlowSheet.GetTranslatedString("Informao")
            ToolTip1.ToolTipIcon = ToolTipIcon.Info
            ToolTip1.Show(SimObject.FlowSheet.GetTranslatedString("CommitChanges"), tbox, 0, tbox.Height + 4, 2000)
        End If

    End Sub

    Private Sub chkNoCondenser_CheckedChanged(sender As Object, e As EventArgs) Handles chkNoCondenser.CheckedChanged
        PanelCondenser.Enabled = Not chkNoCondenser.Checked
        If TypeOf SimObject Is DistillationColumn Then
            DirectCast(SimObject, DistillationColumn).ReboiledAbsorber = chkNoCondenser.Checked
        End If
    End Sub

    Private Sub chkNoReboiler_CheckedChanged(sender As Object, e As EventArgs) Handles chkNoReboiler.CheckedChanged
        PanelReboiler.Enabled = Not chkNoReboiler.Checked
        If TypeOf SimObject Is DistillationColumn Then
            DirectCast(SimObject, DistillationColumn).RefluxedAbsorber = chkNoReboiler.Checked
        End If
    End Sub

    Private Sub lblTag_KeyPress(sender As Object, e As KeyEventArgs) Handles lblTag.KeyUp

        If e.KeyCode = Keys.Enter Then

            If Loaded Then SimObject.GraphicObject.Tag = lblTag.Text
            If Loaded Then SimObject.FlowSheet.UpdateOpenEditForms()
            Me.Text = SimObject.GraphicObject.Tag & " (" & SimObject.GetDisplayName() & ")"
            DirectCast(SimObject.FlowSheet, Interfaces.IFlowsheetGUI).UpdateInterface()

        End If

    End Sub

End Class