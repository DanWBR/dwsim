Imports System.Windows.Forms
Imports DWSIM.Interfaces.Enums.GraphicObjects
Imports DWSIM.SharedClasses.UnitOperations
Imports su = DWSIM.SharedClasses.SystemsOfUnits
Imports DWSIM.UnitOperations.UnitOperations
Imports System.Drawing
Imports unvell.ReoGrid.DataFormat

Public Class EditingForm_ReactorPFR

    Inherits SharedClasses.ObjectEditorForm

    Public Property SimObject As Reactors.Reactor_PFR

    Public Loaded As Boolean = False

    Dim units As SharedClasses.SystemsOfUnits.Units
    Dim nf As String

    Private Sub EditingForm_HeaterCooler_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        Me.ShowHint = GlobalSettings.Settings.DefaultEditFormLocation

        ChangeDefaultFont()

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

            'connections

            Dim mslist As String() = .FlowSheet.GraphicObjects.Values.Where(Function(x) x.ObjectType = ObjectType.MaterialStream).Select(Function(m) m.Tag).OrderBy(Function(m) m).ToArray

            cbInlet1.Items.Clear()
            cbInlet1.Items.AddRange(mslist)

            cbOutlet1.Items.Clear()
            cbOutlet1.Items.AddRange(mslist)

            If .GraphicObject.InputConnectors(0).IsAttached Then cbInlet1.SelectedItem = .GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Tag
            If .GraphicObject.OutputConnectors(0).IsAttached Then cbOutlet1.SelectedItem = .GraphicObject.OutputConnectors(0).AttachedConnector.AttachedTo.Tag

            Dim eslist As String() = .FlowSheet.SimulationObjects.Values.Where(Function(x) x.GraphicObject.ObjectType = ObjectType.EnergyStream).Select(Function(m) m.GraphicObject.Tag).OrderBy(Function(m) m).ToArray()

            cbEnergy.Items.Clear()
            cbEnergy.Items.AddRange(eslist)

            If .GraphicObject.InputConnectors(1).IsAttached Then cbEnergy.SelectedItem = .GraphicObject.InputConnectors(1).AttachedConnector.AttachedFrom.Tag

            'parameters

            cbTemp.Items.Clear()
            cbTemp.Items.AddRange(units.GetUnitSet(Interfaces.Enums.UnitOfMeasure.temperature).ToArray)
            cbTemp.SelectedItem = units.temperature

            cbVol.Items.Clear()
            cbVol.Items.AddRange(units.GetUnitSet(Interfaces.Enums.UnitOfMeasure.volume).ToArray)
            cbVol.SelectedItem = units.volume

            cbCatDiam.Items.Clear()
            cbCatDiam.Items.AddRange(units.GetUnitSet(Interfaces.Enums.UnitOfMeasure.diameter).ToArray)
            cbCatDiam.SelectedItem = units.diameter

            cbCatLoad.Items.Clear()
            cbCatLoad.Items.AddRange(units.GetUnitSet(Interfaces.Enums.UnitOfMeasure.density).ToArray)
            cbCatLoad.SelectedItem = units.density

            cbLength.Items.Clear()
            cbLength.Items.AddRange(units.GetUnitSet(Interfaces.Enums.UnitOfMeasure.distance).ToArray)
            cbLength.SelectedItem = units.distance

            cbDiam.Items.Clear()
            cbDiam.Items.AddRange(units.GetUnitSet(Interfaces.Enums.UnitOfMeasure.diameter).ToArray)
            cbDiam.SelectedItem = units.diameter

            Select Case .ReactorOperationMode
                Case Reactors.OperationMode.Isothermic
                    cbCalcMode.SelectedIndex = 0
                Case Reactors.OperationMode.Adiabatic
                    cbCalcMode.SelectedIndex = 1
                Case Reactors.OperationMode.OutletTemperature
                    cbCalcMode.SelectedIndex = 2
                Case Reactors.OperationMode.NonIsothermalNonAdiabatic
                    cbCalcMode.SelectedIndex = 3
            End Select

            tbOutletTemperature.Text = su.Converter.ConvertFromSI(units.temperature, .OutletTemperature).ToString(nf)
            tbVol.Text = su.Converter.ConvertFromSI(units.volume, .Volume).ToString(nf)
            tbLength.Text = su.Converter.ConvertFromSI(units.distance, .Length).ToString(nf)
            tbCatLoad.Text = su.Converter.ConvertFromSI(units.density, .CatalystLoading).ToString(nf)
            tbCatDiam.Text = su.Converter.ConvertFromSI(units.diameter, .CatalystParticleDiameter).ToString(nf)
            tbCatVoidFrac.Text = .CatalystVoidFraction.ToString(nf)
            tbDiam.Text = su.Converter.ConvertFromSI(units.diameter, .Diameter).ToString(nf)

            TextBox1.Text = .dV.ToString()

            nupNT.Value = SimObject.NumberOfTubes

            Select Case .ReactorSizingType
                Case Reactors.Reactor_PFR.SizingType.Diameter
                    rbDiameter.Checked = True
                    cbDiam.Enabled = True
                    tbDiam.Enabled = True
                    cbLength.Enabled = False
                    tbLength.Enabled = False
                Case Reactors.Reactor_PFR.SizingType.Length
                    rbLength.Checked = True
                    cbDiam.Enabled = False
                    tbDiam.Enabled = False
                    cbLength.Enabled = True
                    tbLength.Enabled = True
            End Select

            Dim rsets As String() = .FlowSheet.ReactionSets.Values.Select(Function(m) m.Name).ToArray
            cbReacSet.Items.Clear()
            cbReacSet.Items.AddRange(rsets)

            Try
                If Not .FlowSheet.ReactionSets.ContainsKey(.ReactionSetID) Then .ReactionSetID = "DefaultSet"
                cbReacSet.SelectedItem = .FlowSheet.ReactionSets(.ReactionSetID).Name
                Dim reactions = .FlowSheet.ReactionSets(.ReactionSetID).Reactions.Values.Select(Function(r) r.ReactionID).ToArray()
                tbCatDiam.Enabled = False
                tbCatLoad.Enabled = False
                tbCatVoidFrac.Enabled = False
                cbCatDiam.Enabled = False
                cbCatLoad.Enabled = False
                For Each r In reactions
                    If .FlowSheet.Reactions(r).ReactionType = Enums.ReactionType.Heterogeneous_Catalytic Then
                        tbCatDiam.Enabled = True
                        tbCatLoad.Enabled = True
                        tbCatVoidFrac.Enabled = True
                        cbCatDiam.Enabled = True
                        cbCatLoad.Enabled = True
                        Exit For
                    End If
                Next
            Catch ex As Exception
            End Try

            'results

            gridResults.Rows.Clear()
            gridReactions.Rows.Clear()

            gridResults.Rows.Add(New Object() { .FlowSheet.GetTranslatedString("DeltaT"), su.Converter.ConvertFromSI(units.deltaT, .DeltaT.GetValueOrDefault).ToString(nf), units.deltaT})
            gridResults.Rows.Add(New Object() { .FlowSheet.GetTranslatedString("RConvPGridItem3"), su.Converter.ConvertFromSI(units.heatflow, .DeltaQ.GetValueOrDefault).ToString(nf), units.heatflow})
            gridResults.Rows.Add(New Object() { .FlowSheet.GetTranslatedString("Residence Time"), su.Converter.ConvertFromSI(units.time, .ResidenceTime).ToString(nf), units.time})
            gridResults.Rows.Add(New Object() { .FlowSheet.GetTranslatedString("Quedadepresso"), su.Converter.ConvertFromSI(units.deltaP, .DeltaP.GetValueOrDefault).ToString(nf), units.deltaP})

            'reaction props

            For Each dbl As KeyValuePair(Of String, Double) In .RxiT
                gridReactions.Rows.Add(New Object() { .FlowSheet.Reactions(dbl.Key).Name, .FlowSheet.GetTranslatedString("ReactionExtent"), su.Converter.ConvertFromSI(units.molarflow, dbl.Value).ToString(nf), units.molarflow})
            Next

            For Each dbl As KeyValuePair(Of String, Double) In .RxiT
                gridReactions.Rows.Add(New Object() { .FlowSheet.Reactions(dbl.Key).Name, .FlowSheet.GetTranslatedString("ReactionRate"), su.Converter.ConvertFromSI(units.reac_rate, (dbl.Value / .Volume)).ToString(nf), units.reac_rate})
            Next

            For Each dbl As KeyValuePair(Of String, Double) In .DHRi
                gridReactions.Rows.Add(New Object() { .FlowSheet.Reactions(dbl.Key).Name, .FlowSheet.GetTranslatedString("ReactionHeat"), su.Converter.ConvertFromSI(units.heatflow, dbl.Value).ToString(nf), units.heatflow})
            Next

            If .Calculated Then
                Try
                    UpdateGraph()
                Catch ex As Exception
                End Try
            End If

            'conversions

            gridConversions.Rows.Clear()
            For Each dbl As KeyValuePair(Of String, Double) In .ComponentConversions
                If dbl.Value > 0.0# And dbl.Value <= 1.0 Then
                    gridConversions.Rows.Add(New Object() {dbl.Key, Format(dbl.Value * 100, nf)})
                End If
            Next

            'property package

            Dim proppacks As String() = .FlowSheet.PropertyPackages.Values.Select(Function(m) m.Tag).ToArray
            cbPropPack.Items.Clear()
            cbPropPack.Items.AddRange(proppacks)
            cbPropPack.SelectedItem = .PropertyPackage?.Tag

            'annotation

            Try
                rtbAnnotations.Rtf = .Annotation
            Catch ex As Exception

            End Try

            'external solvers

            cbExternalSolver.Items.Clear()
            cbExternalSolver.Items.Add("")
            For Each solver In .FlowSheet.ExternalSolvers.Values
                If solver.Category = Enums.ExternalSolverCategory.OrdinaryDifferentialEquations Then
                    cbExternalSolver.Items.Add(solver.DisplayText)
                End If
            Next
            Dim selectedsolver = .FlowSheet.ExternalSolvers.Values.Where(Function(s) s.ID = .ExternalSolverID).FirstOrDefault()
            If selectedsolver IsNot Nothing Then
                cbExternalSolver.SelectedItem = selectedsolver.DisplayText
            Else
                cbExternalSolver.SelectedIndex = 0
            End If

            chkUseUserDefDP.Checked = .UseUserDefinedPressureDrop

            tbUserDefDP.Text = .UserDefinedPressureDrop.ConvertFromSI(units.deltaP).ToString(nf)

            lblPDrop.Text = units.deltaP

        End With

        cbExternalSolver.SetDropDownMaxWidth()

        Dim ssolver = SimObject.FlowSheet.ExternalSolvers.Values.Where(Function(s) s.ID = SimObject.ExternalSolverID).FirstOrDefault()
        If ssolver IsNot Nothing Then
            If TryCast(ssolver, IExternalSolverConfiguration) IsNot Nothing Then
                btnConfigExtSolver.Enabled = True
            Else
                btnConfigExtSolver.Enabled = False
            End If
        End If

        Loaded = True

    End Sub

    Private Sub btnConfigurePP_Click(sender As Object, e As EventArgs) Handles btnConfigurePP.Click
        SimObject.FlowSheet.PropertyPackages.Values.Where(Function(x) x.Tag = cbPropPack.SelectedItem.ToString).FirstOrDefault()?.DisplayGroupedEditingForm()
    End Sub

    Private Sub lblTag_TextChanged(sender As Object, e As EventArgs) Handles lblTag.TextChanged

        If Loaded Then ToolTipChangeTag.Show("Press ENTER to commit changes.", lblTag, New System.Drawing.Point(0, lblTag.Height + 3), 3000)

    End Sub

    Private Sub btnDisconnect1_Click(sender As Object, e As EventArgs) Handles btnDisconnect1.Click
        If cbInlet1.SelectedItem IsNot Nothing Then
            SimObject.FlowSheet.DisconnectObjects(SimObject.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom, SimObject.GraphicObject)
            cbInlet1.SelectedItem = Nothing
        End If
    End Sub

    Private Sub btnDisconnectOutlet1_Click(sender As Object, e As EventArgs) Handles btnDisconnectOutlet1.Click
        If cbOutlet1.SelectedItem IsNot Nothing Then
            SimObject.FlowSheet.DisconnectObjects(SimObject.GraphicObject, SimObject.GraphicObject.OutputConnectors(0).AttachedConnector.AttachedTo)
            cbOutlet1.SelectedItem = Nothing
        End If
    End Sub

    Private Sub btnDisconnectEnergy_Click(sender As Object, e As EventArgs) Handles btnDisconnectEnergy.Click
        If cbEnergy.SelectedItem IsNot Nothing Then
            SimObject.FlowSheet.DisconnectObjects(SimObject.GraphicObject.InputConnectors(1).AttachedConnector.AttachedFrom, SimObject.GraphicObject)
            cbEnergy.SelectedItem = Nothing
        End If
    End Sub

    Sub RequestCalc()

        SimObject.FlowSheet.RequestCalculation3(SimObject, False)

    End Sub

    Private Sub cbPropPack_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbPropPack.SelectedIndexChanged
        If Loaded Then
            SimObject.FlowSheet.RegisterSnapshot(Interfaces.Enums.SnapshotType.ObjectData, SimObject)
            SimObject.PropertyPackage = SimObject.FlowSheet.PropertyPackages.Values.Where(Function(x) x.Tag = cbPropPack.SelectedItem.ToString).SingleOrDefault
            RequestCalc()
        End If
    End Sub

    Private Sub cbInlet1_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbInlet1.SelectedIndexChanged

        If Loaded Then

            Dim text As String = cbInlet1.Text

            If text <> "" Then

                Dim index As Integer = 0

                Dim gobj = SimObject.GraphicObject
                Dim flowsheet = SimObject.FlowSheet

                If flowsheet.GetFlowsheetSimulationObject(text).GraphicObject.OutputConnectors(0).IsAttached Then
                    MessageBox.Show(flowsheet.GetTranslatedString("Todasasconexespossve"), flowsheet.GetTranslatedString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                Else
                    Try
                        If gobj.InputConnectors(index).IsAttached Then flowsheet.DisconnectObjects(gobj.InputConnectors(index).AttachedConnector.AttachedFrom, gobj)
                        flowsheet.ConnectObjects(flowsheet.GetFlowsheetSimulationObject(text).GraphicObject, gobj, 0, index)
                    Catch ex As Exception
                        MessageBox.Show(ex.Message, flowsheet.GetTranslatedString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                    End Try
                End If
                UpdateInfo()

            End If

        End If

    End Sub

    Private Sub cbOutlet1_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbOutlet1.SelectedIndexChanged

        If Loaded Then

            Dim text As String = cbOutlet1.Text

            If text <> "" Then

                Dim index As Integer = 0

                Dim gobj = SimObject.GraphicObject
                Dim flowsheet = SimObject.FlowSheet

                If flowsheet.GetFlowsheetSimulationObject(text).GraphicObject.InputConnectors(0).IsAttached Then
                    MessageBox.Show(flowsheet.GetTranslatedString("Todasasconexespossve"), flowsheet.GetTranslatedString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                Else
                    Try
                        If gobj.OutputConnectors(0).IsAttached Then flowsheet.DisconnectObjects(gobj, gobj.OutputConnectors(0).AttachedConnector.AttachedTo)
                        flowsheet.ConnectObjects(gobj, flowsheet.GetFlowsheetSimulationObject(text).GraphicObject, 0, 0)
                    Catch ex As Exception
                        MessageBox.Show(ex.Message, flowsheet.GetTranslatedString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                    End Try
                End If
                UpdateInfo()

            End If

        End If

    End Sub

    Private Sub cbEnergy_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbEnergy.SelectedIndexChanged

        If Loaded Then

            Dim text As String = cbEnergy.Text

            If text <> "" Then

                Dim index As Integer = 0

                Dim gobj = SimObject.GraphicObject
                Dim flowsheet = SimObject.FlowSheet

                If flowsheet.GetFlowsheetSimulationObject(text).GraphicObject.InputConnectors(0).IsAttached Then
                    MessageBox.Show(flowsheet.GetTranslatedString("Todasasconexespossve"), flowsheet.GetTranslatedString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                Else
                    Try
                        If gobj.InputConnectors(1).IsAttached Then flowsheet.DisconnectObjects(gobj, gobj.InputConnectors(1).AttachedConnector.AttachedTo)
                        flowsheet.ConnectObjects(flowsheet.GetFlowsheetSimulationObject(text).GraphicObject, gobj, 0, 1)
                    Catch ex As Exception
                        MessageBox.Show(ex.Message, flowsheet.GetTranslatedString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                    End Try
                End If
                UpdateInfo()


            End If

        End If

    End Sub

    Private Sub rtbAnnotations_RtfChanged(sender As Object, e As EventArgs) Handles rtbAnnotations.RtfChanged
        If Loaded Then SimObject.Annotation = rtbAnnotations.Rtf
    End Sub

    Private Sub chkActive_CheckedChanged(sender As Object, e As EventArgs) Handles chkActive.CheckedChanged
        If Loaded Then
            SimObject.GraphicObject.Active = chkActive.Checked
            SimObject.FlowSheet.UpdateInterface()
            UpdateInfo()
        End If
    End Sub

    Private Sub tb_TextChanged(sender As Object, e As EventArgs) Handles tbOutletTemperature.TextChanged, tbOutletTemperature.TextChanged, tbCatDiam.TextChanged,
                                                                        tbCatLoad.TextChanged, tbCatVoidFrac.TextChanged, tbLength.TextChanged, tbVol.TextChanged, tbDiam.TextChanged

        Dim tbox = DirectCast(sender, TextBox)

        If tbox.Text.IsValidDoubleExpression Then
            tbox.ForeColor = System.Drawing.Color.Blue
        Else
            tbox.ForeColor = System.Drawing.Color.Red
        End If

    End Sub

    Private Sub TextBoxKeyDown(sender As Object, e As KeyEventArgs) Handles tbOutletTemperature.KeyDown, tbCatDiam.KeyDown, tbCatLoad.KeyDown, tbCatVoidFrac.KeyDown, tbLength.KeyDown,
                                                                            tbVol.KeyDown, tbDiam.KeyDown, tbDiam.KeyDown

        If e.KeyCode = Keys.Enter And Loaded And DirectCast(sender, TextBox).ForeColor = System.Drawing.Color.Blue Then

            SimObject.FlowSheet.RegisterSnapshot(Interfaces.Enums.SnapshotType.ObjectData, SimObject)

            UpdateProps(sender)

            DirectCast(sender, TextBox).SelectAll()

        End If

    End Sub

    Sub UpdateProps(sender As Object)

        If sender Is tbOutletTemperature Then SimObject.OutletTemperature = su.Converter.ConvertToSI(cbTemp.SelectedItem.ToString, tbOutletTemperature.Text.ParseExpressionToDouble)
        If sender Is tbVol Then SimObject.Volume = su.Converter.ConvertToSI(cbVol.SelectedItem.ToString, tbVol.Text.ParseExpressionToDouble)
        If sender Is tbLength Then SimObject.Length = su.Converter.ConvertToSI(cbLength.SelectedItem.ToString, tbLength.Text.ParseExpressionToDouble)
        If sender Is tbCatDiam Then SimObject.CatalystParticleDiameter = su.Converter.ConvertToSI(cbCatDiam.SelectedItem.ToString, tbCatDiam.Text.ParseExpressionToDouble)
        If sender Is tbCatLoad Then SimObject.CatalystLoading = su.Converter.ConvertToSI(cbCatLoad.SelectedItem.ToString, tbCatLoad.Text.ParseExpressionToDouble)
        If sender Is tbCatVoidFrac Then SimObject.CatalystVoidFraction = tbCatVoidFrac.Text.ParseExpressionToDouble
        If sender Is tbDiam Then SimObject.Diameter = su.Converter.ConvertToSI(cbDiam.SelectedItem.ToString, tbDiam.Text.ParseExpressionToDouble)

        RequestCalc()

    End Sub

    Private Sub cbReacSet_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbReacSet.SelectedIndexChanged
        If Loaded Then
            SimObject.FlowSheet.RegisterSnapshot(Interfaces.Enums.SnapshotType.ObjectData, SimObject)
            SimObject.ReactionSetID = SimObject.FlowSheet.ReactionSets.Values.Where(Function(x) x.Name = cbReacSet.SelectedItem.ToString).FirstOrDefault.ID
            Dim reactions = SimObject.FlowSheet.ReactionSets(SimObject.ReactionSetID).Reactions.Values.Select(Function(r) r.ReactionID).ToArray()
            tbCatDiam.Enabled = False
            tbCatLoad.Enabled = False
            tbCatVoidFrac.Enabled = False
            cbCatDiam.Enabled = False
            cbCatLoad.Enabled = False
            For Each r In reactions
                If SimObject.FlowSheet.Reactions(r).ReactionType = Enums.ReactionType.Heterogeneous_Catalytic Then
                    tbCatDiam.Enabled = True
                    tbCatLoad.Enabled = True
                    tbCatVoidFrac.Enabled = True
                    cbCatDiam.Enabled = True
                    cbCatLoad.Enabled = True
                    Exit For
                End If
            Next
            RequestCalc()
        End If
    End Sub

    Private Sub cbCalcMode_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbCalcMode.SelectedIndexChanged

        If Loaded Then SimObject.FlowSheet.RegisterSnapshot(Interfaces.Enums.SnapshotType.ObjectData, SimObject)

        Select Case cbCalcMode.SelectedIndex
            Case 0
                tbOutletTemperature.Enabled = False
                cbTemp.Enabled = False
                SimObject.ReactorOperationMode = Reactors.OperationMode.Isothermic
            Case 1
                tbOutletTemperature.Enabled = False
                cbTemp.Enabled = False
                SimObject.ReactorOperationMode = Reactors.OperationMode.Adiabatic
            Case 2
                tbOutletTemperature.Enabled = True
                cbTemp.Enabled = True
                SimObject.ReactorOperationMode = Reactors.OperationMode.OutletTemperature
            Case 3
                tbOutletTemperature.Enabled = False
                cbTemp.Enabled = False
                SimObject.ReactorOperationMode = Reactors.OperationMode.NonIsothermalNonAdiabatic
        End Select
        If Loaded Then RequestCalc()

    End Sub

    Private Sub btnCreateAndConnectInlet1_Click(sender As Object, e As EventArgs) Handles btnCreateAndConnectInlet1.Click, btnCreateAndConnectOutlet1.Click, btnCreateAndConnectEnergy.Click

        Dim sgobj = SimObject.GraphicObject
        Dim fs = SimObject.FlowSheet

        If sender Is btnCreateAndConnectInlet1 Then

            Dim obj = fs.AddObject(ObjectType.MaterialStream, sgobj.InputConnectors(0).Position.X - 50, sgobj.InputConnectors(0).Position.Y, "")

            If sgobj.InputConnectors(0).IsAttached Then fs.DisconnectObjects(sgobj.InputConnectors(0).AttachedConnector.AttachedFrom, sgobj)
            fs.ConnectObjects(obj.GraphicObject, sgobj, 0, 0)

        ElseIf sender Is btnCreateAndConnectOutlet1 Then

            Dim obj = fs.AddObject(ObjectType.MaterialStream, sgobj.OutputConnectors(0).Position.X + 30, sgobj.OutputConnectors(0).Position.Y, "")

            If sgobj.OutputConnectors(0).IsAttached Then fs.DisconnectObjects(sgobj, sgobj.OutputConnectors(0).AttachedConnector.AttachedTo)
            fs.ConnectObjects(sgobj, obj.GraphicObject, 0, 0)

        ElseIf sender Is btnCreateAndConnectEnergy Then

            Dim obj = fs.AddObject(ObjectType.EnergyStream, sgobj.InputConnectors(1).Position.X - 50, sgobj.InputConnectors(1).Position.Y + 50, "")

            If sgobj.InputConnectors(1).IsAttached Then fs.DisconnectObjects(sgobj.InputConnectors(1).AttachedConnector.AttachedFrom, sgobj)
            fs.ConnectObjects(obj.GraphicObject, sgobj, 0, 1)

        End If

        UpdateInfo()
        RequestCalc()

    End Sub

    Protected vx As Double()
    Protected vy As Double()
    Protected vn As String()
    Protected vya As ArrayList
    Protected m_ytitle As String
    Protected m_xtitle As String

    Private Sub UpdateGraph()

        If SimObject.points Is Nothing Then Exit Sub

        vya = New ArrayList
        ReDim vx(Me.SimObject.points.Count - 1)
        ReDim vy(Me.SimObject.points.Count - 1)

        Dim i As Integer = 0
        Dim j As Integer
        For Each obj As Object In Me.SimObject.points
            vx(i) = obj(0)
            i += 1
        Next

        j = 1
        Do
            i = 0
            For Each obj As Object In Me.SimObject.points
                vy(i) = obj(j)
                i += 1
            Next
            j += 1
            vya.Add(vy.Clone)
        Loop Until j = Me.SimObject.ComponentConversions.Count + 3

        ReDim vn(CType(Me.SimObject.points(0), Double()).Length - 3)

        i = 0
        For Each s As String In SimObject.ComponentConversions.Keys
            vn(i) = s
            i += 1
        Next

        DrawGraph()

    End Sub

    Private Sub DrawGraph()


        Dim rnd As New Random(231)

        Dim i As Integer
        Dim color1 As System.Drawing.Color

        With Me.ZedGraphControl1.GraphPane
            .CurveList.Clear()
            .AddY2Axis("T / " & units.temperature)
            .AddY2Axis("P / " & units.pressure)
            With .Y2AxisList(0)
                .IsVisible = True
                .Title.Text = "T / " & units.temperature
                .Title.FontSpec.Size = 16
                .Scale.FontSpec.Size = 16
                .Scale.Format = nf
            End With
            With .Y2AxisList(1)
                .IsVisible = True
                .Title.Text = "P / " & units.pressure
                .Title.FontSpec.Size = 16
                .Scale.FontSpec.Size = 16
                .Scale.Format = nf
            End With
            i = 0
            Do
                color1 = Color.FromArgb(255, rnd.Next(0, 255), rnd.Next(0, 255), rnd.Next(0, 255))
                With .AddCurve(vn(i), su.Converter.ConvertArrayFromSI(units.distance, vx), su.Converter.ConvertArrayFromSI(units.molar_conc, vya(i)), color1, ZedGraph.SymbolType.Circle)
                    .Line.IsSmooth = True
                    .Symbol.IsVisible = False
                End With
                i += 1
            Loop Until i = vn.Length - 1
            color1 = Color.FromArgb(255, rnd.Next(0, 255), rnd.Next(0, 255), rnd.Next(0, 255))
            With .AddCurve("T / " & units.temperature, su.Converter.ConvertArrayFromSI(units.distance, vx), su.Converter.ConvertArrayFromSI(units.temperature, vya(i)), color1, ZedGraph.SymbolType.Circle)
                .Line.IsSmooth = True
                .Symbol.IsVisible = False
                .IsY2Axis = True
                .YAxisIndex = 0
            End With
            color1 = Color.FromArgb(255, rnd.Next(0, 255), rnd.Next(0, 255), rnd.Next(0, 255))
            With .AddCurve("P / " & units.pressure, su.Converter.ConvertArrayFromSI(units.volume, vx), su.Converter.ConvertArrayFromSI(units.pressure, vya(i + 1)), color1, ZedGraph.SymbolType.Circle)
                .Line.IsSmooth = True
                .Symbol.IsVisible = False
                .IsY2Axis = True
                .YAxisIndex = 1
            End With
            .XAxis.Title.Text = "Reactor Length / " & units.distance
            .XAxis.Title.FontSpec.Size = 16
            .XAxis.Scale.FontSpec.Size = 16
            .XAxis.Scale.MaxAuto = False
            .XAxis.Scale.Max = su.Converter.ConvertFromSI(units.volume, vx.Max)
            .YAxis.Title.Text = "C / " & units.molar_conc
            .YAxis.Scale.FontSpec.Size = 16
            .YAxis.Title.FontSpec.Size = 16
            .XAxis.Scale.Format = nf
            .YAxis.Scale.Format = nf
            With .Legend
                .Position = ZedGraph.LegendPos.TopFlushLeft
                .Border.IsVisible = False
                .FontSpec.Size = 14
                .FontSpec.IsDropShadow = False
            End With
            .Title.IsVisible = False
            .AxisChange(Me.CreateGraphics)
        End With
        Me.ZedGraphControl1.Invalidate()

    End Sub

    Private Sub cbTemp_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbTemp.SelectedIndexChanged, cbCatDiam.SelectedIndexChanged, cbCatLoad.SelectedIndexChanged, cbVol.SelectedIndexChanged, cbLength.SelectedIndexChanged, cbDiam.SelectedIndexChanged

        If Loaded Then
            Try
                If sender Is cbTemp Then
                    tbOutletTemperature.Text = su.Converter.Convert(cbTemp.SelectedItem.ToString, units.time, Double.Parse(tbOutletTemperature.Text)).ToString(nf)
                    cbTemp.SelectedItem = units.time
                    UpdateProps(tbOutletTemperature)
                ElseIf sender Is cbVol Then
                    tbVol.Text = su.Converter.Convert(cbVol.SelectedItem.ToString, units.volume, Double.Parse(tbVol.Text)).ToString(nf)
                    cbVol.SelectedItem = units.volume
                    UpdateProps(tbVol)
                ElseIf sender Is cbLength Then
                    tbLength.Text = su.Converter.Convert(cbLength.SelectedItem.ToString, units.distance, Double.Parse(tbLength.Text)).ToString(nf)
                    cbLength.SelectedItem = units.distance
                    UpdateProps(tbLength)
                ElseIf sender Is cbCatDiam Then
                    tbCatDiam.Text = su.Converter.Convert(cbCatDiam.SelectedItem.ToString, units.diameter, Double.Parse(tbCatDiam.Text)).ToString(nf)
                    cbCatDiam.SelectedItem = units.diameter
                    UpdateProps(tbCatDiam)
                ElseIf sender Is cbDiam Then
                    tbDiam.Text = su.Converter.Convert(cbDiam.SelectedItem.ToString, units.diameter, Double.Parse(tbDiam.Text)).ToString(nf)
                    cbDiam.SelectedItem = units.diameter
                    UpdateProps(tbDiam)
                ElseIf sender Is cbCatLoad Then
                    tbCatLoad.Text = su.Converter.Convert(cbCatLoad.SelectedItem.ToString, units.density, Double.Parse(tbCatLoad.Text)).ToString(nf)
                    cbCatLoad.SelectedItem = units.density
                    UpdateProps(tbCatLoad)
                End If
            Catch ex As Exception
                SimObject.FlowSheet.ShowMessage(ex.Message.ToString, Interfaces.IFlowsheet.MessageType.GeneralError)
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

    Private Sub cbExternalSolver_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbExternalSolver.SelectedIndexChanged
        If Loaded Then
            Dim selectedsolver = SimObject.FlowSheet.ExternalSolvers.Values.Where(
                Function(s) s.DisplayText = cbExternalSolver.SelectedItem.ToString()).FirstOrDefault()
            If selectedsolver IsNot Nothing Then
                SimObject.ExternalSolverID = selectedsolver.ID
                If TryCast(selectedsolver, IExternalSolverConfiguration) IsNot Nothing Then
                    btnConfigExtSolver.Enabled = True
                Else
                    btnConfigExtSolver.Enabled = False
                End If
            Else
                SimObject.ExternalSolverID = ""
                btnConfigExtSolver.Enabled = False
            End If
        End If
    End Sub

    Private Sub btnConfigExtSolver_Click(sender As Object, e As EventArgs) Handles btnConfigExtSolver.Click
        Dim selectedsolver = SimObject.FlowSheet.ExternalSolvers.Values.Where(
                Function(s) s.DisplayText = cbExternalSolver.SelectedItem.ToString()).FirstOrDefault()
        If TryCast(selectedsolver, IExternalSolverConfiguration) IsNot Nothing Then
            SimObject.ExternalSolverConfigData = DirectCast(selectedsolver, IExternalSolverConfiguration).Edit(SimObject.ExternalSolverConfigData)
        End If
    End Sub

    Private Sub rbLength_CheckedChanged(sender As Object, e As EventArgs) Handles rbLength.CheckedChanged, rbDiameter.CheckedChanged

        tbDiam.Enabled = rbDiameter.Checked
        tbLength.Enabled = rbLength.Checked

        If rbDiameter.Checked Then
            SimObject.ReactorSizingType = Reactors.Reactor_PFR.SizingType.Diameter
        Else
            SimObject.ReactorSizingType = Reactors.Reactor_PFR.SizingType.Length
        End If

    End Sub

    Private Sub nupNT_ValueChanged(sender As Object, e As EventArgs) Handles nupNT.ValueChanged
        If Loaded Then
            SimObject.NumberOfTubes = nupNT.Value
        End If
    End Sub

    Private Sub TextBox1_TextChanged(sender As Object, e As EventArgs) Handles TextBox1.TextChanged
        If TextBox1.Text.IsValidDouble Then
            SimObject.dV = TextBox1.Text.ToDoubleFromCurrent()
        End If
    End Sub

    Private Sub chkUseUserDefDP_CheckedChanged(sender As Object, e As EventArgs) Handles chkUseUserDefDP.CheckedChanged
        SimObject.UseUserDefinedPressureDrop = chkUseUserDefDP.Checked
    End Sub

    Private Sub tbUserDefDP_TextChanged(sender As Object, e As EventArgs) Handles tbUserDefDP.TextChanged
        If Loaded Then
            Try
                SimObject.UserDefinedPressureDrop = tbUserDefDP.Text.ToDoubleFromCurrent().ConvertToSI(units.deltaP)
                tbUserDefDP.ForeColor = Color.Blue
            Catch ex As Exception
                tbUserDefDP.ForeColor = Color.Red
            End Try
        End If
    End Sub

    Private Sub btnExportProfile_Click(sender As Object, e As EventArgs) Handles btnExportProfile.Click

        If SimObject.Profile.Count > 0 Then

            Dim grid As unvell.ReoGrid.ReoGridControl = SimObject.FlowSheet().GetSpreadsheetObject()

            Dim sheet = grid.CreateWorksheet(SimObject.GraphicObject.Tag + "_" + New Random().Next(1000).ToString())
            grid.Worksheets.Add(sheet)

            grid.CurrentWorksheet = sheet

            Dim item = SimObject.Profile(0)

            sheet.Cells(0, 0).Data = String.Format("Length ({0})", units.distance)
            sheet.Cells(0, 1).Data = String.Format("Temperature ({0})", units.temperature)
            sheet.Cells(0, 2).Data = String.Format("Pressure ({0})", units.pressure)

            Dim i, j As Integer
            j = 3
            For Each pitem In item.Item4
                sheet.Cells(0, j).Data = String.Format("{0} MolFrac", pitem.Compound)
                sheet.Cells(0, j + 1).Data = String.Format("{0} MassFrac", pitem.Compound)
                sheet.Cells(0, j + 2).Data = String.Format("{0} MolFlow ({1})", pitem.Compound, units.molarflow)
                sheet.Cells(0, j + 3).Data = String.Format("{0} MassFlow ({1})", pitem.Compound, units.massflow)
                sheet.Cells(0, j + 4).Data = String.Format("{0} MolConc ({1})", pitem.Compound, units.molar_conc)
                sheet.Cells(0, j + 5).Data = String.Format("{0} MassConc ({1})", pitem.Compound, units.mass_conc)
                j += 6
            Next

            i = 1
            For Each item In SimObject.Profile
                sheet.Cells(i, 0).Data = item.Item1.ConvertFromSI(units.distance)
                sheet.Cells(i, 1).Data = item.Item2.ConvertFromSI(units.temperature)
                sheet.Cells(i, 2).Data = item.Item3.ConvertFromSI(units.pressure)
                j = 3
                For Each pitem In item.Item4
                    sheet.Cells(i, j).Data = pitem.MolarFraction
                    sheet.Cells(i, j + 1).Data = pitem.MassFraction
                    sheet.Cells(i, j + 2).Data = pitem.MolarFlow.ConvertFromSI(units.molarflow)
                    sheet.Cells(i, j + 3).Data = pitem.MassFlow.ConvertFromSI(units.massflow)
                    sheet.Cells(i, j + 4).Data = pitem.MolarConcentration.ConvertFromSI(units.molar_conc)
                    sheet.Cells(i, j + 5).Data = pitem.MassConcentration.ConvertFromSI(units.mass_conc)
                    j += 6
                Next
                i += 1
            Next

            sheet.SetRangeDataFormat(New unvell.ReoGrid.RangePosition(1, 0, i, j + 5), unvell.ReoGrid.DataFormat.CellDataFormatFlag.Number,
                  New NumberDataFormatter.NumberFormatArgs() With
                  {
                    .DecimalPlaces = 6, .NegativeStyle = NumberDataFormatter.NumberNegativeStyle.Minus, .UseSeparator = False
                  })

            sheet.ScaleFactor = GlobalSettings.Settings.DpiScale

            For k = 0 To j
                sheet.AutoFitColumnWidth(k)
            Next

            MessageBox.Show(String.Format("Data export finished successfully to sheet '{0}'.", sheet.Name), "DWSIM", MessageBoxButtons.OK, MessageBoxIcon.Information)

        End If

    End Sub

End Class