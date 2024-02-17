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

    Dim reditor As EditingForm_Column_Results
    Dim fr, fr2 As ReportViewer

    Dim units As SharedClasses.SystemsOfUnits.Units
    Dim nf As String

    Friend tab1, tab2 As Integer

    Private Sub EditingForm_HeaterCooler_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        Me.ShowHint = GlobalSettings.Settings.DefaultEditFormLocation

        UpdateInfo()

        ChangeDefaultFont()

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

            chkCreateConvReport.Checked = .CreateSolverConvergengeReport

            btnViewReport.Enabled = (.ColumnSolverConvergenceReport <> "")

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

            'solving method

            cbSolvingMethod.Enabled = True
            LabelSM.Enabled = True
            cbSolvingMethod.Items.Clear()
            If TypeOf SimObject Is DistillationColumn Then
                cbSolvingMethod.Items.Add("Wang-Henke (Bubble Point)")
                cbSolvingMethod.Items.Add("Napthali-Sandholm (Simultaneous Correction)")
                cbSolvingMethod.Items.Add("Modified Wang-Henke (Bubble Point)")
            ElseIf TypeOf SimObject Is AbsorptionColumn Then
                cbSolvingMethod.Items.Add("Burningham-Otto (Sum Rates)")
                cbSolvingMethod.Items.Add("Napthali-Sandholm (Simultaneous Correction)")
                If .SolvingMethodName = "Wang-Henke (Bubble Point)" Then
                    .SolvingMethodName = "Burningham-Otto (Sum Rates)"
                End If
            End If
            Dim extrasolvers = Column.ExternalColumnSolvers.Keys.ToArray()
            cbSolvingMethod.Items.AddRange(extrasolvers)

            Try
                cbSolvingMethod.SelectedItem = .SolvingMethodName
            Catch ex As Exception
                .SolvingMethodName = "Wang-Henke (Bubble Point)"
                cbSolvingMethod.SelectedIndex = 0
            End Try

            Dim ieproviders = Column.ExternalInitialEstimatesProviders.Keys.ToArray()
            cbInitialEstimatesProvider.Items.Clear()
            cbInitialEstimatesProvider.Items.Add("Internal (Default)")
            cbInitialEstimatesProvider.Items.AddRange(ieproviders)

            Try
                cbInitialEstimatesProvider.SelectedItem = .InitialEstimatesProvider
            Catch ex As Exception
                .InitialEstimatesProvider = "Internal (Default)"
                cbInitialEstimatesProvider.SelectedIndex = 0
            End Try

            If TypeOf SimObject Is DistillationColumn Then
                chkNoCondenser.Checked = DirectCast(SimObject, DistillationColumn).ReboiledAbsorber
                chkNoReboiler.Checked = DirectCast(SimObject, DistillationColumn).RefluxedAbsorber
            ElseIf TypeOf SimObject Is AbsorptionColumn Then
                TabContainerSpecification.TabPages.Remove(TabCondenser)
                TabContainerSpecification.TabPages.Remove(TabReboiler)
            End If

            'parameters

            cbCondPressureUnits.Items.Clear()
            cbCondPressureUnits.Items.AddRange(units.GetUnitSet(Interfaces.Enums.UnitOfMeasure.pressure).ToArray)
            cbCondPressureUnits.SelectedItem = units.pressure

            cbCondPDropUnits.Items.Clear()
            cbCondPDropUnits.Items.AddRange(units.GetUnitSet(Interfaces.Enums.UnitOfMeasure.deltaP).ToArray)
            cbCondPDropUnits.SelectedItem = units.deltaP

            cbSubcooling.Items.Clear()
            cbSubcooling.Items.AddRange(units.GetUnitSet(Interfaces.Enums.UnitOfMeasure.deltaT).ToArray)
            cbSubcooling.SelectedItem = units.deltaT

            cbColPDrop.Items.Clear()
            cbColPDrop.Items.AddRange(units.GetUnitSet(Interfaces.Enums.UnitOfMeasure.deltaP).ToArray)
            cbColPDrop.SelectedItem = units.deltaP

            cbTS.Items.Clear()
            cbTS.Items.AddRange(units.GetUnitSet(Interfaces.Enums.UnitOfMeasure.distance).ToArray)
            cbTS.SelectedItem = units.distance

            tbTS.Text = .TraySpacing.ConvertFromSI(units.distance).ToString(nf)

            tbColPDrop.Text = su.Converter.ConvertFromSI(units.deltaP, .ColumnPressureDrop).ToString(nf)

            tbNStages.Text = .NumberOfStages

            If TypeOf SimObject Is AbsorptionColumn Then cbAbsorberMode.SelectedIndex = DirectCast(SimObject, AbsorptionColumn).OperationMode Else cbAbsorberMode.Enabled = False

            tbMaxIt.Text = .MaxIterations
            tbConvTol.Text = .ExternalLoopTolerance.ToString("R")

            cbCondType.SelectedIndex = .CondenserType
            tbCondPressure.Text = su.Converter.ConvertFromSI(units.pressure, .Stages.First.P).ToString(nf)
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
                Case ColumnSpec.SpecType.Feed_Recovery
                    cunits = New String() {"%"}
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

            If TypeOf SimObject Is DistillationColumn Then
                tbSubcooling.Text = su.Converter.ConvertFromSI(units.deltaT,
                                                               DirectCast(SimObject, DistillationColumn).TotalCondenserSubcoolingDeltaT).ToString(nf)
            Else
                tbSubcooling.Text = "0"
            End If

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
                Case ColumnSpec.SpecType.Feed_Recovery
                    runits = New String() {"%"}
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

            Dim spval1, spval2 As Double, spu1, spu2 As String

            Select Case .Specs("C").SType
                Case ColumnSpec.SpecType.Temperature,
                      ColumnSpec.SpecType.Product_Molar_Flow_Rate,
                      ColumnSpec.SpecType.Product_Mass_Flow_Rate,
                      ColumnSpec.SpecType.Heat_Duty,
                      ColumnSpec.SpecType.Component_Molar_Flow_Rate,
                      ColumnSpec.SpecType.Component_Mass_Flow_Rate
                    spval1 = su.Converter.ConvertFromSI(.Specs("C").SpecUnit, .Specs("C").CalculatedValue)
                    spu1 = .Specs("C").SpecUnit
                Case Else
                    spval1 = .Specs("C").CalculatedValue
                    spu1 = ""
            End Select

            Select Case .Specs("R").SType
                Case ColumnSpec.SpecType.Temperature,
                      ColumnSpec.SpecType.Product_Molar_Flow_Rate,
                      ColumnSpec.SpecType.Product_Mass_Flow_Rate,
                      ColumnSpec.SpecType.Heat_Duty,
                      ColumnSpec.SpecType.Component_Molar_Flow_Rate,
                      ColumnSpec.SpecType.Component_Mass_Flow_Rate
                    spval2 = su.Converter.ConvertFromSI(.Specs("R").SpecUnit, .Specs("R").CalculatedValue)
                    spu2 = .Specs("R").SpecUnit
                Case Else
                    spval2 = .Specs("R").CalculatedValue
                    spu2 = ""
            End Select

            gridResults.Rows.Clear()
            Select Case .ColumnType
                Case ColType.DistillationColumn
                    gridResults.Rows.Add(New Object() { .FlowSheet.GetTranslatedString("DCCondenserDuty"), su.Converter.ConvertFromSI(units.heatflow, .CondenserDuty).ToString(nf), units.heatflow})
                    gridResults.Rows.Add(New Object() { .FlowSheet.GetTranslatedString("DCReboilerDuty"), su.Converter.ConvertFromSI(units.heatflow, .ReboilerDuty).ToString(nf), units.heatflow})
                    gridResults.Rows.Add(New Object() { .FlowSheet.GetTranslatedString("CondenserSpecValue"), .Specs("C").SpecValue.ToString(nf), spu2})
                    gridResults.Rows.Add(New Object() { .FlowSheet.GetTranslatedString("CondenserCalcValue"), spval1.ToString(nf), spu1})
                    gridResults.Rows.Add(New Object() { .FlowSheet.GetTranslatedString("ReboilerSpecValue"), .Specs("R").SpecValue.ToString(nf), spu2})
                    gridResults.Rows.Add(New Object() { .FlowSheet.GetTranslatedString("ReboilerCalcValue"), spval2.ToString(nf), spu2})
                Case ColType.AbsorptionColumn
            End Select
            gridResults.Rows.Add(New Object() { .FlowSheet.GetTranslatedString("DCILIts"), .ic, ""})
            gridResults.Rows.Add(New Object() { .FlowSheet.GetTranslatedString("Estimated Height"), .EstimatedHeight.ConvertFromSI(units.diameter).ToString(nf), units.diameter})
            gridResults.Rows.Add(New Object() { .FlowSheet.GetTranslatedString("Estimated Diameter"), .EstimatedDiameter.ConvertFromSI(units.diameter).ToString(nf), units.diameter})

            btnResults.Enabled = .x0.Count > 0

            btnViewPropertiesReport.Enabled = .Calculated

            'property package

            Dim proppacks As String() = .FlowSheet.PropertyPackages.Values.Select(Function(m) m.Tag).ToArray
            cbPropPack.Items.Clear()
            cbPropPack.Items.AddRange(proppacks)
            cbPropPack.SelectedItem = .PropertyPackage?.Tag

        End With

        Loaded = True

        If reditor IsNot Nothing Then
            reditor.Text = SimObject.GetDisplayName() & ": " & SimObject.GraphicObject.Tag
            reditor.TabText = reditor.Text
            reditor.FillGraphs()
            reditor.FillTables()
        End If

        If fr IsNot Nothing Then
            fr.TextBox1.Text = SimObject.ColumnPropertiesProfile
            fr.Text = SimObject.GraphicObject.Tag + ": Properties Profile"
            fr.TabText = SimObject.GraphicObject.Tag + ": Properties Profile"
            fr.TextBox1.DeselectAll()
        End If

        If fr2 IsNot Nothing Then
            fr2.TextBox1.Text = SimObject.ColumnSolverConvergenceReport
            fr2.Text = SimObject.GraphicObject.Tag + ": Convergence Report"
            fr2.TabText = SimObject.GraphicObject.Tag + ": Convergence Report"
            fr2.TextBox1.DeselectAll()
        End If

    End Sub

    Private Sub btnConfigurePP_Click(sender As Object, e As EventArgs) Handles btnConfigurePP.Click
        SimObject.FlowSheet.PropertyPackages.Values.Where(Function(x) x.Tag = cbPropPack.SelectedItem.ToString).FirstOrDefault()?.DisplayGroupedEditingForm()
    End Sub

    Private Sub lblTag_TextChanged(sender As Object, e As EventArgs) Handles lblTag.TextChanged
        If Loaded Then ToolTipChangeTag.Show("Press ENTER to commit changes.", lblTag, New System.Drawing.Point(0, lblTag.Height + 3), 3000)

    End Sub

    Sub RequestCalc()

        'SimObject.FlowSheet.RequestCalculation3(SimObject, False)

    End Sub

    Private Sub cbPropPack_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbPropPack.SelectedIndexChanged
        If Loaded Then
            SimObject.FlowSheet.RegisterSnapshot(Interfaces.Enums.SnapshotType.ObjectData, SimObject)
            SimObject.PropertyPackage = SimObject.FlowSheet.PropertyPackages.Values.Where(Function(x) x.Tag = cbPropPack.SelectedItem.ToString).SingleOrDefault
            RequestCalc()
        End If
    End Sub

    Private Sub chkActive_CheckedChanged(sender As Object, e As EventArgs) Handles chkActive.CheckedChanged
        If Loaded Then
            SimObject.GraphicObject.Active = chkActive.Checked
            SimObject.FlowSheet.UpdateInterface()
            UpdateInfo()
        End If
    End Sub

    Private Sub tb_TextChanged(sender As Object, e As EventArgs)

        Dim tbox = DirectCast(sender, TextBox)

        If tbox.Text.IsValidDoubleExpression Then
            tbox.ForeColor = System.Drawing.Color.Blue
        Else
            tbox.ForeColor = System.Drawing.Color.Red
        End If

    End Sub

    Sub UpdateProps(sender As Object)

        RequestCalc()

    End Sub

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles btnResults.Click

        If reditor Is Nothing Then reditor = New EditingForm_Column_Results With {.dc = Me.SimObject}
        reditor.Text = SimObject.GetDisplayName() & ": " & SimObject.GraphicObject.Tag
        reditor.TabText = reditor.Text
        reditor.ShowHint = WeifenLuo.WinFormsUI.Docking.DockState.Document
        SimObject.FlowSheet.DisplayForm(reditor)

    End Sub

    Private Sub tbNStages_TextChanged(sender As Object, e As KeyEventArgs) Handles tbNStages.KeyDown

        If Loaded And e.KeyCode = Keys.Enter Then

            SimObject.FlowSheet.RegisterSnapshot(Interfaces.Enums.SnapshotType.ObjectData, SimObject)

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
                Dim P0 = SimObject.Stages(SimObject.Stages.Count - 2).P
                Dim E0 = SimObject.Stages(SimObject.Stages.Count - 2).Efficiency
                Dim P1 = SimObject.Stages.Last.P
                Dim E1 = SimObject.Stages.Last.Efficiency
                For i = 1 To dif
                    Dim stage = New Stage(Guid.NewGuid().ToString)
                    SimObject.Stages.Insert(SimObject.Stages.Count - 1, stage)
                    stage.Name = SimObject.FlowSheet.GetTranslatedString("DCStage") & "_" & SimObject.Stages.Count - 2
                    stage.P = P0 + Convert.ToDouble(i) / Convert.ToDouble(dif) * (P1 - P0)
                    stage.Efficiency = E0 + Convert.ToDouble(i) / Convert.ToDouble(dif) * (E1 - E0)
                    With SimObject.InitialEstimates
                        Dim dl As New Dictionary(Of String, Parameter)
                        Dim dv As New Dictionary(Of String, Parameter)
                        For Each cp In SimObject.FlowSheet.SelectedCompounds.Values
                            Dim pl = New Parameter()
                            pl.LoadData(.LiqCompositions(.LiqCompositions.Count - 1)(cp.Name).SaveData())
                            dl.Add(cp.Name, pl)
                            Dim pv = New Parameter()
                            pv.LoadData(.VapCompositions(.VapCompositions.Count - 1)(cp.Name).SaveData())
                            dv.Add(cp.Name, pv)
                        Next
                        .LiqCompositions.Insert(.LiqCompositions.Count - 1, dl)
                        .VapCompositions.Insert(.VapCompositions.Count - 1, dv)
                        Dim L0 = .LiqMolarFlows(.LiqMolarFlows.Count - 2).Value
                        Dim V0 = .VapMolarFlows(.VapMolarFlows.Count - 2).Value
                        Dim T0 = .StageTemps(.StageTemps.Count - 1).Value
                        Dim T1 = .StageTemps(.StageTemps.Count - 1).Value
                        Dim plt = New Parameter
                        Dim pvt = New Parameter
                        Dim pt = New Parameter
                        plt.Value = L0
                        pvt.Value = V0
                        pt.Value = T0 + Convert.ToDouble(i) / Convert.ToDouble(dif) * (T1 - T0)
                        .LiqMolarFlows.Insert(.LiqMolarFlows.Count - 1, plt)
                        .VapMolarFlows.Insert(.VapMolarFlows.Count - 1, pvt)
                        .StageTemps.Insert(.StageTemps.Count - 1, pt)
                    End With
                Next
            End If

            UpdateInfo()

        End If

    End Sub

    Private Sub tbMaxIt_TextChanged(sender As Object, e As KeyEventArgs) Handles tbMaxIt.KeyDown

        If Loaded And e.KeyCode = Keys.Enter Then

            SimObject.FlowSheet.RegisterSnapshot(Interfaces.Enums.SnapshotType.ObjectData, SimObject)

            SimObject.MaxIterations = Convert.ToInt32(Double.Parse(tbMaxIt.Text))

        End If

    End Sub

    Private Sub tbConvTol_TextChanged(sender As Object, e As KeyEventArgs) Handles tbConvTol.KeyDown

        If Loaded And e.KeyCode = Keys.Enter Then

            SimObject.FlowSheet.RegisterSnapshot(Interfaces.Enums.SnapshotType.ObjectData, SimObject)

            SimObject.ExternalLoopTolerance = Double.Parse(tbConvTol.Text)
            SimObject.InternalLoopTolerance = Double.Parse(tbConvTol.Text)

        End If

    End Sub

    Private Sub cbAbsorberMode_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbAbsorberMode.SelectedIndexChanged

        If Loaded Then

            SimObject.FlowSheet.RegisterSnapshot(Interfaces.Enums.SnapshotType.ObjectData, SimObject)

            DirectCast(SimObject, AbsorptionColumn).OperationMode = cbAbsorberMode.SelectedIndex

        End If

    End Sub

    Private Sub cbCondType_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbCondType.SelectedIndexChanged

        If Loaded Then SimObject.FlowSheet.RegisterSnapshot(Interfaces.Enums.SnapshotType.ObjectData, SimObject)

        SimObject.CondenserType = cbCondType.SelectedIndex

        If SimObject.CondenserType = condtype.Partial_Condenser Then
            tbCondVapFlow.Enabled = True
            cbCondVapFlowUnits.Enabled = True
        Else
            tbCondVapFlow.Enabled = False
            cbCondVapFlowUnits.Enabled = False
        End If
        If SimObject.CondenserType = condtype.Total_Condenser Then
            tbSubcooling.Enabled = True
            cbSubcooling.Enabled = True
        Else
            tbSubcooling.Enabled = False
            cbSubcooling.Enabled = False
        End If

        SimObject.FlowSheet.UpdateInterface()

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
            Case ColumnSpec.SpecType.Feed_Recovery
                cunits = New String() {"%"}
                cbCondComp.Enabled = False
        End Select
        cbCondSpecUnits.Items.Clear()
        cbCondSpecUnits.Items.AddRange(cunits)
        cbCondSpecUnits.SelectedIndex = 0

    End Sub

    Private Sub tbCondPressure_TextChanged(sender As Object, e As KeyEventArgs) Handles tbCondPressure.KeyDown

        If Loaded And e.KeyCode = Keys.Enter Then

            SimObject.FlowSheet.RegisterSnapshot(Interfaces.Enums.SnapshotType.ObjectData, SimObject)

            SimObject.Stages(0).P = su.Converter.ConvertToSI(units.pressure, tbCondPressure.Text)

            UpdateInfo()
            RequestCalc()

        End If

    End Sub

    Private Sub tbCondPDrop_TextChanged(sender As Object, e As KeyEventArgs) Handles tbCondPDrop.KeyDown

        If Loaded And e.KeyCode = Keys.Enter Then

            SimObject.FlowSheet.RegisterSnapshot(Interfaces.Enums.SnapshotType.ObjectData, SimObject)

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
            Case ColumnSpec.SpecType.Feed_Recovery
                cunits = New String() {"%"}
                cbRebComp.Enabled = False
        End Select
        cbRebSpecUnits.Items.Clear()
        cbRebSpecUnits.Items.AddRange(cunits)
        cbRebSpecUnits.SelectedIndex = 0

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
                                                                                tbConvTol.TextChanged, tbMaxIt.TextChanged, tbNStages.TextChanged, tbRebSpecValue.TextChanged, tbNStages.KeyDown,
                                                                                tbSubcooling.TextChanged, tbColPDrop.TextChanged, tbTS.TextChanged
        Dim tbox = DirectCast(sender, TextBox)

        If Loaded Then
            ToolTip1.ToolTipTitle = SimObject.FlowSheet.GetTranslatedString("Informao")
            ToolTip1.ToolTipIcon = ToolTipIcon.Info
            ToolTip1.Show(SimObject.FlowSheet.GetTranslatedString("CommitChanges"), tbox, 0, tbox.Height + 4, 2000)
        End If

    End Sub

    Private Sub chkNoCondenser_CheckedChanged(sender As Object, e As EventArgs) Handles chkNoCondenser.CheckedChanged
        SimObject.FlowSheet.RegisterSnapshot(Interfaces.Enums.SnapshotType.ObjectData, SimObject)
        PanelCondenser.Enabled = Not chkNoCondenser.Checked
        If TypeOf SimObject Is DistillationColumn Then
            DirectCast(SimObject, DistillationColumn).ReboiledAbsorber = chkNoCondenser.Checked
        End If
    End Sub

    Private Sub chkNoReboiler_CheckedChanged(sender As Object, e As EventArgs) Handles chkNoReboiler.CheckedChanged
        SimObject.FlowSheet.RegisterSnapshot(Interfaces.Enums.SnapshotType.ObjectData, SimObject)
        PanelReboiler.Enabled = Not chkNoReboiler.Checked
        If TypeOf SimObject Is DistillationColumn Then
            DirectCast(SimObject, DistillationColumn).RefluxedAbsorber = chkNoReboiler.Checked
        End If
    End Sub

    Private Sub cbSubcooling_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbSubcooling.SelectedIndexChanged

        If Loaded Then

            SimObject.FlowSheet.RegisterSnapshot(Interfaces.Enums.SnapshotType.ObjectData, SimObject)

            tbSubcooling.Text = su.Converter.Convert(cbSubcooling.SelectedItem.ToString(),
                                                 units.deltaT, tbSubcooling.Text.ParseExpressionToDouble)

            DirectCast(SimObject, DistillationColumn).TotalCondenserSubcoolingDeltaT =
                    su.Converter.ConvertToSI(units.deltaT, tbSubcooling.Text.ParseExpressionToDouble)

        End If

    End Sub

    Private Sub tbSubcooling_KeyDown(sender As Object, e As KeyEventArgs) Handles tbSubcooling.KeyDown

        If Loaded And e.KeyCode = Keys.Enter Then

            SimObject.FlowSheet.RegisterSnapshot(Interfaces.Enums.SnapshotType.ObjectData, SimObject)

            DirectCast(SimObject, DistillationColumn).TotalCondenserSubcoolingDeltaT =
                su.Converter.ConvertToSI(units.deltaT, tbSubcooling.Text.ParseExpressionToDouble)

        End If

    End Sub

    Private Sub tbColPDrop_KeyDown(sender As Object, e As KeyEventArgs) Handles tbColPDrop.KeyDown

        If Loaded And e.KeyCode = Keys.Enter Then

            SimObject.FlowSheet.RegisterSnapshot(Interfaces.Enums.SnapshotType.ObjectData, SimObject)

            SimObject.ColumnPressureDrop = su.Converter.ConvertToSI(units.deltaP, tbColPDrop.Text)

            UpdateInfo()
            RequestCalc()

        End If

    End Sub

    Private Sub Button1_Click_1(sender As Object, e As EventArgs) Handles btnViewReport.Click

        If fr2 Is Nothing Then
            fr2 = New ReportViewer()
            fr2.HideOnClose = True
        End If
        fr2.TextBox1.Text = SimObject.ColumnSolverConvergenceReport
        fr2.Text = SimObject.GraphicObject.Tag + ": Convergence Report"
        fr2.TabText = SimObject.GraphicObject.Tag + ": Convergence Report"
        fr2.TextBox1.DeselectAll()
        SimObject.FlowSheet.DisplayForm(fr2)

    End Sub

    Private Sub CheckBox1_CheckedChanged(sender As Object, e As EventArgs) Handles chkCreateConvReport.CheckedChanged
        SimObject.CreateSolverConvergengeReport = chkCreateConvReport.Checked
    End Sub

    Private Sub btnViewPropertiesReport_Click(sender As Object, e As EventArgs) Handles btnViewPropertiesReport.Click

        If fr Is Nothing Then
            fr = New ReportViewer()
            fr.HideOnClose = True
        End If
        fr.TextBox1.Text = SimObject.ColumnPropertiesProfile
        fr.Text = SimObject.GraphicObject.Tag + ": Properties Profile"
        fr.TabText = SimObject.GraphicObject.Tag + ": Properties Profile"
        fr.TextBox1.DeselectAll()
        SimObject.FlowSheet.DisplayForm(fr)

    End Sub

    Private Sub tbTS_KeyDown(sender As Object, e As KeyEventArgs) Handles tbTS.KeyDown

        If Loaded And e.KeyCode = Keys.Enter Then

            SimObject.FlowSheet.RegisterSnapshot(Interfaces.Enums.SnapshotType.ObjectData, SimObject)

            SimObject.TraySpacing = su.Converter.ConvertToSI(units.distance, tbTS.Text)

            UpdateInfo()

        End If

    End Sub

    Private Sub cbSolvingMethod_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbSolvingMethod.SelectedIndexChanged

        If Loaded Then
            Try
                SimObject.SolvingMethodName = cbSolvingMethod.SelectedItem.ToString()
            Catch ex As Exception
            End Try
        End If

    End Sub

    Private Sub cbInitialEstimatesProvider_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbInitialEstimatesProvider.SelectedIndexChanged

        If Loaded Then
            Try
                SimObject.InitialEstimatesProvider = cbInitialEstimatesProvider.SelectedItem.ToString()
            Catch ex As Exception
            End Try
        End If

    End Sub

    Private Sub lblTag_KeyPress(sender As Object, e As KeyEventArgs) Handles lblTag.KeyUp

        If e.KeyCode = Keys.Enter Then

            SimObject.FlowSheet.RegisterSnapshot(Interfaces.Enums.SnapshotType.ObjectLayout)

            If Loaded Then SimObject.GraphicObject.Tag = lblTag.Text
            If Loaded Then SimObject.FlowSheet.UpdateOpenEditForms()
            Me.Text = SimObject.GraphicObject.Tag & " (" & SimObject.GetDisplayName() & ")"
            DirectCast(SimObject.FlowSheet, Interfaces.IFlowsheetGUI).UpdateInterface()

        End If

    End Sub

End Class