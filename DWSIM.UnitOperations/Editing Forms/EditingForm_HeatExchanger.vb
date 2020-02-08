Imports System.Windows.Forms
Imports DWSIM.Interfaces.Enums.GraphicObjects
Imports DWSIM.SharedClasses.UnitOperations
Imports su = DWSIM.SharedClasses.SystemsOfUnits

Public Class EditingForm_HeatExchanger

    Inherits SharedClasses.ObjectEditorForm

    Public Property SimObject As UnitOperations.HeatExchanger

    Public Loaded As Boolean = False

    Dim units As SharedClasses.SystemsOfUnits.Units
    Dim nf As String

    Private Sub EditingForm_HeaterCooler_Load(sender As Object, e As EventArgs) Handles MyBase.Load

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
                    If .ErrorMessage.Length > 50 Then
                        lblStatus.Text = .FlowSheet.GetTranslatedString("Erro") & " (" & .ErrorMessage.Substring(50) & "...)"
                    Else
                        lblStatus.Text = .FlowSheet.GetTranslatedString("Erro") & " (" & .ErrorMessage & ")"
                    End If
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

            Dim mslist As String() = .FlowSheet.GraphicObjects.Values.Where(Function(x) x.ObjectType = ObjectType.MaterialStream).Select(Function(m) m.Tag).ToArray

            cbInlet1.Items.Clear()
            cbInlet1.Items.AddRange(mslist)

            cbInlet2.Items.Clear()
            cbInlet2.Items.AddRange(mslist)

            cbOutlet1.Items.Clear()
            cbOutlet1.Items.AddRange(mslist)

            cbOutlet2.Items.Clear()
            cbOutlet2.Items.AddRange(mslist)

            If .GraphicObject.InputConnectors(0).IsAttached Then cbInlet1.SelectedItem = .GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Tag
            If .GraphicObject.OutputConnectors(0).IsAttached Then cbOutlet1.SelectedItem = .GraphicObject.OutputConnectors(0).AttachedConnector.AttachedTo.Tag

            If .GraphicObject.InputConnectors(1).IsAttached Then cbInlet2.SelectedItem = .GraphicObject.InputConnectors(1).AttachedConnector.AttachedFrom.Tag
            If .GraphicObject.OutputConnectors(1).IsAttached Then cbOutlet2.SelectedItem = .GraphicObject.OutputConnectors(1).AttachedConnector.AttachedTo.Tag

            'property package

            Dim proppacks As String() = .FlowSheet.PropertyPackages.Values.Select(Function(m) m.Tag).ToArray
            cbPropPack.Items.Clear()
            cbPropPack.Items.AddRange(proppacks)
            cbPropPack.SelectedItem = .PropertyPackage?.Tag

            Dim flashalgos As String() = .FlowSheet.FlowsheetOptions.FlashAlgorithms.Select(Function(x) x.Tag).ToArray
            cbFlashAlg.Items.Clear()
            cbFlashAlg.Items.Add("Default")
            cbFlashAlg.Items.AddRange(flashalgos)
            If .PreferredFlashAlgorithmTag <> "" Then cbFlashAlg.SelectedItem = .PreferredFlashAlgorithmTag Else cbFlashAlg.SelectedIndex = 0

            'parameters

            cbHotFluidPDrop.Items.Clear()
            cbHotFluidPDrop.Items.AddRange(units.GetUnitSet(Interfaces.Enums.UnitOfMeasure.deltaP).ToArray)
            cbHotFluidPDrop.SelectedItem = units.deltaP

            cbColdFluidPDrop.Items.Clear()
            cbColdFluidPDrop.Items.AddRange(units.GetUnitSet(Interfaces.Enums.UnitOfMeasure.deltaP).ToArray)
            cbColdFluidPDrop.SelectedItem = units.deltaP

            cbColdFluidOutletT.Items.Clear()
            cbColdFluidOutletT.Items.AddRange(units.GetUnitSet(Interfaces.Enums.UnitOfMeasure.temperature).ToArray)
            cbColdFluidOutletT.SelectedItem = units.temperature

            cbHotFluidOutletT.Items.Clear()
            cbHotFluidOutletT.Items.AddRange(units.GetUnitSet(Interfaces.Enums.UnitOfMeasure.temperature).ToArray)
            cbHotFluidOutletT.SelectedItem = units.temperature

            cbArea.Items.Clear()
            cbArea.Items.AddRange(units.GetUnitSet(Interfaces.Enums.UnitOfMeasure.area).ToArray)
            cbArea.SelectedItem = units.area

            cbOverallHTC.Items.Clear()
            cbOverallHTC.Items.AddRange(units.GetUnitSet(Interfaces.Enums.UnitOfMeasure.heat_transf_coeff).ToArray)
            cbOverallHTC.SelectedItem = units.heat_transf_coeff

            cbHeat.Items.Clear()
            cbHeat.Items.AddRange(units.GetUnitSet(Interfaces.Enums.UnitOfMeasure.heatflow).ToArray)
            cbHeat.SelectedItem = units.heatflow

            cbHeatLoss.Items.Clear()
            cbHeatLoss.Items.AddRange(units.GetUnitSet(Interfaces.Enums.UnitOfMeasure.heatflow).ToArray)
            cbHeatLoss.SelectedItem = units.heatflow

            cbMITA.Items.Clear()
            cbMITA.Items.AddRange(units.GetUnitSet(Interfaces.Enums.UnitOfMeasure.deltaT).ToArray)
            cbMITA.SelectedItem = units.deltaT

            cbEfficiency.SelectedIndex = 0

            cbCalcMode.SelectedIndex = .CalculationMode

            cbFlowDir.SelectedIndex = .FlowDir

            tbHotFluidPDrop.Text = su.Converter.ConvertFromSI(units.deltaP, .HotSidePressureDrop).ToString(nf)
            tbColdFluidPDrop.Text = su.Converter.ConvertFromSI(units.deltaP, .ColdSidePressureDrop).ToString(nf)
            tbColdFluidOutletT.Text = su.Converter.ConvertFromSI(units.temperature, .ColdSideOutletTemperature).ToString(nf)
            tbHotFluidOutletT.Text = su.Converter.ConvertFromSI(units.temperature, .HotSideOutletTemperature).ToString(nf)

            tbOverallU.Text = su.Converter.ConvertFromSI(units.heat_transf_coeff, .OverallCoefficient).ToString(nf)
            tbArea.Text = su.Converter.ConvertFromSI(units.area, .Area).ToString(nf)
            tbHeat.Text = su.Converter.ConvertFromSI(units.heatflow, .Q.GetValueOrDefault).ToString(nf)
            tbHeatLoss.Text = su.Converter.ConvertFromSI(units.heatflow, .HeatLoss).ToString(nf)

            tbMITA.Text = su.Converter.ConvertFromSI(units.deltaT, .MITA).ToString(nf)

            tbEfficiency.Text = .ThermalEfficiency.ToString(nf)

            chkIgnoreLMTD.Checked = .IgnoreLMTDError

            'results

            gridResults.Rows.Clear()

            If .Calculated Then

                gridResults.Rows.Add(New Object() {.FlowSheet.GetTranslatedString("MaximumHeatExchange"), .MaxHeatExchange.ToString(nf), units.heatflow})
                gridResults.Rows.Add(New Object() {.FlowSheet.GetTranslatedString("ThermalEfficiency"), .ThermalEfficiency.ToString(nf), "%"})
                gridResults.Rows.Add(New Object() {.FlowSheet.GetTranslatedString("HXLMTD"), .LMTD.ToString(nf), units.deltaT})

                If .CalculationMode = UnitOperations.HeatExchangerCalcMode.ShellandTube_CalcFoulingFactor Or .CalculationMode = UnitOperations.HeatExchangerCalcMode.ShellandTube_Rating Then
                    gridResults.Rows.Add(New Object() {"Re Shell", .STProperties.ReS.ToString(nf), ""})
                    gridResults.Rows.Add(New Object() {"Re Tube", .STProperties.ReT.ToString(nf), ""})
                    gridResults.Rows.Add(New Object() {"F Shell", .STProperties.Fs.ToString("E6"), units.foulingfactor})
                    gridResults.Rows.Add(New Object() {"F Tube", .STProperties.Ft.ToString("E6"), units.foulingfactor})
                    gridResults.Rows.Add(New Object() {"F Pipe", .STProperties.Fc.ToString("E6"), units.foulingfactor})
                    gridResults.Rows.Add(New Object() {"F Fouling", .STProperties.Ff.ToString("E6"), units.foulingfactor})
                End If

                If .CalculationMode = UnitOperations.HeatExchangerCalcMode.PinchPoint Then
                    btnViewProfile.Enabled = True
                Else
                    btnViewProfile.Enabled = False
                End If

            End If

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

    Private Sub cbCalcMode_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbCalcMode.SelectedIndexChanged

        SimObject.CalculationMode = cbCalcMode.SelectedIndex

        tbColdFluidOutletT.Enabled = True
        tbHotFluidOutletT.Enabled = True
        tbOverallU.Enabled = True
        tbArea.Enabled = True
        tbHeat.Enabled = True
        tbMITA.Enabled = False
        tbEfficiency.Enabled = False

        cbColdFluidOutletT.Enabled = True
        cbHotFluidOutletT.Enabled = True
        cbOverallHTC.Enabled = True
        cbArea.Enabled = True
        cbHeat.Enabled = True
        cbMITA.Enabled = False

        btnEditSTProps.Enabled = False

        Select Case cbCalcMode.SelectedIndex
            Case 0
                'Temperatura de Saída do Fluido Quente
                tbHotFluidOutletT.Enabled = False
                cbHotFluidOutletT.Enabled = False
                tbHeat.Enabled = False
                cbHeat.Enabled = False
                tbColdFluidPDrop.Enabled = True
                tbHotFluidPDrop.Enabled = True
                tbEfficiency.Enabled = False
            Case 1
                'Temperatura de Saída do Fluido Frio
                tbColdFluidOutletT.Enabled = False
                cbColdFluidOutletT.Enabled = False
                tbHeat.Enabled = False
                cbHeat.Enabled = False
                tbColdFluidPDrop.Enabled = True
                tbHotFluidPDrop.Enabled = True
                tbEfficiency.Enabled = False
            Case 2
                'Temperaturas de Saída
                tbHotFluidOutletT.Enabled = False
                cbHotFluidOutletT.Enabled = False
                tbColdFluidOutletT.Enabled = False
                cbColdFluidOutletT.Enabled = False
                tbOverallU.Enabled = False
                cbOverallHTC.Enabled = False
                tbColdFluidPDrop.Enabled = True
                tbHotFluidPDrop.Enabled = True
                tbEfficiency.Enabled = False
            Case 3
                'Temperaturas de Saída (UA)
                tbHotFluidOutletT.Enabled = False
                cbHotFluidOutletT.Enabled = False
                tbColdFluidOutletT.Enabled = False
                cbColdFluidOutletT.Enabled = False
                tbHeat.Enabled = False
                cbHeat.Enabled = False
                tbColdFluidPDrop.Enabled = True
                tbHotFluidPDrop.Enabled = True
                tbEfficiency.Enabled = False
            Case 4
                'Área
                tbArea.Enabled = False
                cbArea.Enabled = False
                tbHeat.Enabled = False
                cbHeat.Enabled = False
                tbColdFluidPDrop.Enabled = True
                tbHotFluidPDrop.Enabled = True
                tbEfficiency.Enabled = False
            Case 5
                'Avaliação de Trocador Casco e Tubos
                tbHotFluidOutletT.Enabled = False
                cbHotFluidOutletT.Enabled = False
                tbColdFluidOutletT.Enabled = False
                cbColdFluidOutletT.Enabled = False
                tbOverallU.Enabled = False
                tbArea.Enabled = False
                tbHeat.Enabled = False
                cbOverallHTC.Enabled = False
                cbArea.Enabled = False
                cbHeat.Enabled = False
                btnEditSTProps.Enabled = True
                tbColdFluidPDrop.Enabled = False
                tbHotFluidPDrop.Enabled = False
                tbEfficiency.Enabled = False
            Case 6
                'Trocador Casco e Tubos - Fator de Fouling
                tbOverallU.Enabled = False
                tbArea.Enabled = False
                tbHeat.Enabled = False
                cbOverallHTC.Enabled = False
                cbArea.Enabled = False
                cbHeat.Enabled = False
                btnEditSTProps.Enabled = True
                tbColdFluidPDrop.Enabled = False
                tbHotFluidPDrop.Enabled = False
                tbEfficiency.Enabled = False
            Case 7
                'Ponto de 'Pinch'
                tbMITA.Enabled = True
                cbMITA.Enabled = True
                tbHotFluidOutletT.Enabled = False
                cbHotFluidOutletT.Enabled = False
                tbColdFluidOutletT.Enabled = False
                cbColdFluidOutletT.Enabled = False
                tbArea.Enabled = False
                cbArea.Enabled = False
                tbHeat.Enabled = False
                cbHeat.Enabled = False
                tbColdFluidPDrop.Enabled = True
                tbHotFluidPDrop.Enabled = True
                tbEfficiency.Enabled = False
            Case 8
                'Thermal Efficiency
                tbHotFluidOutletT.Enabled = False
                cbHotFluidOutletT.Enabled = False
                tbColdFluidOutletT.Enabled = False
                cbColdFluidOutletT.Enabled = False
                tbOverallU.Enabled = True
                tbArea.Enabled = False
                tbHeat.Enabled = False
                cbOverallHTC.Enabled = True
                cbArea.Enabled = False
                cbHeat.Enabled = False
                btnEditSTProps.Enabled = False
                tbColdFluidPDrop.Enabled = True
                tbHotFluidPDrop.Enabled = True
                tbEfficiency.Enabled = True
        End Select

    End Sub

    Private Sub cb_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbColdFluidPDrop.SelectedIndexChanged,
                                                                                  cbHotFluidPDrop.SelectedIndexChanged,
                                                                                  cbColdFluidOutletT.SelectedIndexChanged,
                                                                                  cbHotFluidOutletT.SelectedIndexChanged,
                                                                                  cbArea.SelectedIndexChanged, cbOverallHTC.SelectedIndexChanged,
                                                                                  cbHeat.SelectedIndexChanged, cbMITA.SelectedIndexChanged, cbHeatLoss.SelectedIndexChanged

        If Loaded Then
            Try
                If sender Is cbHotFluidPDrop Then
                    tbHotFluidPDrop.Text = su.Converter.Convert(cbHotFluidPDrop.SelectedItem.ToString, units.deltaP, Double.Parse(tbHotFluidPDrop.Text)).ToString(nf)
                    cbHotFluidPDrop.SelectedItem = units.deltaP
                    UpdateProps(tbHotFluidPDrop)
                ElseIf sender Is cbColdFluidPDrop Then
                    tbColdFluidPDrop.Text = su.Converter.Convert(cbColdFluidPDrop.SelectedItem.ToString, units.deltaP, Double.Parse(tbColdFluidPDrop.Text)).ToString(nf)
                    cbColdFluidPDrop.SelectedItem = units.deltaP
                    UpdateProps(tbColdFluidPDrop)
                ElseIf sender Is cbHotFluidOutletT Then
                    tbHotFluidOutletT.Text = su.Converter.Convert(cbHotFluidOutletT.SelectedItem.ToString, units.temperature, Double.Parse(tbHotFluidOutletT.Text)).ToString(nf)
                    cbHotFluidOutletT.SelectedItem = units.temperature
                    UpdateProps(tbHotFluidOutletT)
                ElseIf sender Is cbColdFluidOutletT Then
                    tbColdFluidOutletT.Text = su.Converter.Convert(cbColdFluidOutletT.SelectedItem.ToString, units.temperature, Double.Parse(tbColdFluidOutletT.Text)).ToString(nf)
                    cbColdFluidOutletT.SelectedItem = units.temperature
                    UpdateProps(tbColdFluidOutletT)
                ElseIf sender Is cbArea Then
                    tbArea.Text = su.Converter.Convert(cbArea.SelectedItem.ToString, units.area, Double.Parse(tbArea.Text)).ToString(nf)
                    cbArea.SelectedItem = units.area
                    UpdateProps(tbArea)
                ElseIf sender Is cbOverallHTC Then
                    tbOverallU.Text = su.Converter.Convert(cbOverallHTC.SelectedItem.ToString, units.heat_transf_coeff, Double.Parse(tbOverallU.Text)).ToString(nf)
                    cbOverallHTC.SelectedItem = units.heat_transf_coeff
                    UpdateProps(tbOverallU)
                ElseIf sender Is cbHeat Then
                    tbHeat.Text = su.Converter.Convert(cbHeat.SelectedItem.ToString, units.heatflow, Double.Parse(tbHeat.Text)).ToString(nf)
                    cbHeat.SelectedItem = units.heatflow
                    UpdateProps(tbHeat)
                ElseIf sender Is cbHeatLoss Then
                    tbHeatLoss.Text = su.Converter.Convert(cbHeatLoss.SelectedItem.ToString, units.heatflow, Double.Parse(tbHeatLoss.Text)).ToString(nf)
                    cbHeatLoss.SelectedItem = units.heatflow
                    UpdateProps(tbHeatLoss)
                ElseIf sender Is cbMITA Then
                    tbMITA.Text = su.Converter.Convert(cbMITA.SelectedItem.ToString, units.deltaT, Double.Parse(tbMITA.Text)).ToString(nf)
                    cbMITA.SelectedItem = units.deltaT
                    UpdateProps(tbMITA)
                End If
            Catch ex As Exception
                SimObject.FlowSheet.ShowMessage(ex.Message.ToString, Interfaces.IFlowsheet.MessageType.GeneralError)
            End Try
        End If

    End Sub

    Sub UpdateProps(sender As Object)

        'Pressão na Saída
        'Variação da Pressão

        Dim uobj = SimObject

        uobj.CalculationMode = cbCalcMode.SelectedIndex

        If sender Is tbHotFluidPDrop Then uobj.HotSidePressureDrop = su.Converter.ConvertToSI(cbHotFluidPDrop.SelectedItem.ToString, tbHotFluidPDrop.Text.ParseExpressionToDouble)
        If sender Is tbColdFluidPDrop Then uobj.ColdSidePressureDrop = su.Converter.ConvertToSI(cbColdFluidPDrop.SelectedItem.ToString, tbColdFluidPDrop.Text.ParseExpressionToDouble)
        If sender Is tbHotFluidOutletT Then uobj.HotSideOutletTemperature = su.Converter.ConvertToSI(cbHotFluidOutletT.SelectedItem.ToString, tbHotFluidOutletT.Text.ParseExpressionToDouble)
        If sender Is tbColdFluidOutletT Then uobj.ColdSideOutletTemperature = su.Converter.ConvertToSI(cbColdFluidOutletT.SelectedItem.ToString, tbColdFluidOutletT.Text.ParseExpressionToDouble)
        If sender Is tbArea Then uobj.Area = su.Converter.ConvertToSI(cbArea.SelectedItem.ToString, tbArea.Text.ParseExpressionToDouble)
        If sender Is tbOverallU Then uobj.OverallCoefficient = su.Converter.ConvertToSI(cbOverallHTC.SelectedItem.ToString, tbOverallU.Text.ParseExpressionToDouble)
        If sender Is tbHeat Then uobj.Q = su.Converter.ConvertToSI(cbHeat.SelectedItem.ToString, tbHeat.Text.ParseExpressionToDouble)
        If sender Is tbHeatLoss Then uobj.HeatLoss = su.Converter.ConvertToSI(cbHeatLoss.SelectedItem.ToString, tbHeatLoss.Text.ParseExpressionToDouble)
        If sender Is tbMITA Then uobj.MITA = su.Converter.ConvertToSI(cbMITA.SelectedItem.ToString, tbMITA.Text.ParseExpressionToDouble)
        If sender Is tbEfficiency Then uobj.ThermalEfficiency = tbEfficiency.Text.ParseExpressionToDouble

        RequestCalc()

    End Sub

    Sub RequestCalc()

        SimObject.FlowSheet.RequestCalculation(SimObject)

    End Sub

    Private Sub tb_TextChanged(sender As Object, e As EventArgs) Handles tbColdFluidPDrop.TextChanged, tbHotFluidPDrop.TextChanged,
                                                                        tbColdFluidOutletT.TextChanged, tbHotFluidOutletT.TextChanged,
                                                                        tbArea.TextChanged, tbHeat.TextChanged, tbOverallU.TextChanged,
                                                                        tbMITA.TextChanged, tbHeatLoss.TextChanged, tbEfficiency.TextChanged

        Dim tbox = DirectCast(sender, TextBox)

        If tbox.Text.IsValidDoubleExpression Then
            tbox.ForeColor = System.Drawing.Color.Blue
        Else
            tbox.ForeColor = System.Drawing.Color.Red
        End If

    End Sub

    Private Sub TextBoxKeyDown(sender As Object, e As KeyEventArgs) Handles tbColdFluidPDrop.KeyDown, tbHotFluidPDrop.KeyDown,
                                                                        tbColdFluidOutletT.KeyDown, tbHotFluidOutletT.KeyDown,
                                                                        tbArea.KeyDown, tbHeat.KeyDown, tbOverallU.KeyDown,
                                                                        tbMITA.KeyDown, tbHeatLoss.KeyDown, tbEfficiency.KeyDown

        If e.KeyCode = Keys.Enter And Loaded And DirectCast(sender, TextBox).ForeColor = System.Drawing.Color.Blue Then

            UpdateProps(sender)

            DirectCast(sender, TextBox).SelectAll()

        End If

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

    Private Sub cbInlet1_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbInlet1.SelectedIndexChanged

        If Loaded Then

            Dim text As String = cbInlet1.Text

            If text <> "" Then

                Dim index As Integer = 0

                Dim gobj = SimObject.GraphicObject
                Dim flowsheet = SimObject.FlowSheet

                If flowsheet.GetFlowsheetSimulationObject(text).GraphicObject.OutputConnectors(0).IsAttached Then
                    MessageBox.Show(flowsheet.GetTranslatedString("Todasasconexespossve"), flowsheet.GetTranslatedString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                    Exit Sub
                End If
                If gobj.InputConnectors(index).IsAttached Then flowsheet.DisconnectObjects(gobj.InputConnectors(index).AttachedConnector.AttachedFrom, gobj)
                flowsheet.ConnectObjects(flowsheet.GetFlowsheetSimulationObject(text).GraphicObject, gobj, 0, index)

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
                    Exit Sub
                End If
                If gobj.OutputConnectors(0).IsAttached Then flowsheet.DisconnectObjects(gobj, gobj.OutputConnectors(0).AttachedConnector.AttachedTo)
                flowsheet.ConnectObjects(gobj, flowsheet.GetFlowsheetSimulationObject(text).GraphicObject, 0, 0)

            End If

        End If

    End Sub

    Private Sub chkActive_CheckedChanged(sender As Object, e As EventArgs) Handles chkActive.CheckedChanged
        If Loaded Then SimObject.GraphicObject.Active = chkActive.Checked
    End Sub

    Private Sub btnCreateAndConnectInlet1_Click(sender As Object, e As EventArgs) Handles btnCreateAndConnectInlet1.Click, btnCreateAndConnectOutlet1.Click, btnCreateAndConnectInlet2.Click, btnCreateAndConnectOutlet2.Click

        Dim sgobj = SimObject.GraphicObject
        Dim fs = SimObject.FlowSheet

        Dim iidx As Integer = -1
        Dim oidx As Integer = -1

        If sender Is btnCreateAndConnectInlet1 Then

            iidx = 0

        ElseIf sender Is btnCreateAndConnectOutlet1 Then

            oidx = 0

        ElseIf sender Is btnCreateAndConnectInlet2 Then

            iidx = 1

        ElseIf sender Is btnCreateAndConnectOutlet2 Then

            oidx = 1

        End If

        If iidx >= 0 Then

            Dim obj = fs.AddObject(ObjectType.MaterialStream, sgobj.InputConnectors(iidx).Position.X - 50, sgobj.InputConnectors(iidx).Position.Y, "")

            If sgobj.InputConnectors(iidx).IsAttached Then fs.DisconnectObjects(sgobj.InputConnectors(iidx).AttachedConnector.AttachedFrom, sgobj)
            fs.ConnectObjects(obj.GraphicObject, sgobj, 0, iidx)

        End If

        If oidx >= 0 Then

            Dim obj = fs.AddObject(ObjectType.MaterialStream, sgobj.OutputConnectors(oidx).Position.X + 30, sgobj.OutputConnectors(oidx).Position.Y, "")

            If sgobj.OutputConnectors(oidx).IsAttached Then fs.DisconnectObjects(sgobj, sgobj.OutputConnectors(oidx).AttachedConnector.AttachedTo)
            fs.ConnectObjects(sgobj, obj.GraphicObject, oidx, 0)

        End If

        UpdateInfo()
        RequestCalc()

    End Sub

    Private Sub cbInlet2_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbInlet2.SelectedIndexChanged

        If Loaded Then

            Dim text As String = cbInlet2.Text

            If text <> "" Then

                Dim index As Integer = 1

                Dim gobj = SimObject.GraphicObject
                Dim flowsheet = SimObject.FlowSheet

                If flowsheet.GetFlowsheetSimulationObject(text).GraphicObject.OutputConnectors(0).IsAttached Then
                    MessageBox.Show(flowsheet.GetTranslatedString("Todasasconexespossve"), flowsheet.GetTranslatedString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                    Exit Sub
                End If
                If gobj.InputConnectors(index).IsAttached Then flowsheet.DisconnectObjects(gobj.InputConnectors(index).AttachedConnector.AttachedFrom, gobj)
                flowsheet.ConnectObjects(flowsheet.GetFlowsheetSimulationObject(text).GraphicObject, gobj, 0, index)

            End If

        End If

    End Sub

    Private Sub cbOutlet2_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbOutlet2.SelectedIndexChanged

        If Loaded Then

            Dim text As String = cbOutlet2.Text

            If text <> "" Then

                Dim index As Integer = 1

                Dim gobj = SimObject.GraphicObject
                Dim flowsheet = SimObject.FlowSheet

                If flowsheet.GetFlowsheetSimulationObject(text).GraphicObject.InputConnectors(0).IsAttached Then
                    MessageBox.Show(flowsheet.GetTranslatedString("Todasasconexespossve"), flowsheet.GetTranslatedString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                    Exit Sub
                End If
                If gobj.OutputConnectors(index).IsAttached Then flowsheet.DisconnectObjects(gobj, gobj.OutputConnectors(index).AttachedConnector.AttachedTo)
                flowsheet.ConnectObjects(gobj, flowsheet.GetFlowsheetSimulationObject(text).GraphicObject, index, 0)

            End If

        End If

    End Sub

    Private Sub btnDisconnect2_Click(sender As Object, e As EventArgs) Handles btnDisconnect2.Click
        If cbInlet2.SelectedItem IsNot Nothing Then
            SimObject.FlowSheet.DisconnectObjects(SimObject.GraphicObject.InputConnectors(1).AttachedConnector.AttachedFrom, SimObject.GraphicObject)
            cbInlet2.SelectedItem = Nothing
        End If
    End Sub

    Private Sub btnDisconnectOutlet2_Click(sender As Object, e As EventArgs) Handles btnDisconnectOutlet2.Click
        If cbOutlet2.SelectedItem IsNot Nothing Then
            SimObject.FlowSheet.DisconnectObjects(SimObject.GraphicObject, SimObject.GraphicObject.OutputConnectors(1).AttachedConnector.AttachedTo)
            cbOutlet2.SelectedItem = Nothing
        End If
    End Sub

    Private Sub btnEditSTProps_Click(sender As Object, e As EventArgs) Handles btnEditSTProps.Click

        Dim f As New EditingForm_HeatExchanger_SHProperties With {.hx = SimObject}
        f.Show()

    End Sub

    Private Sub cbFlowDir_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbFlowDir.SelectedIndexChanged
        SimObject.FlowDir = cbFlowDir.SelectedIndex
        If Loaded Then RequestCalc()
    End Sub

    Private Sub btnViewProfile_Click(sender As Object, e As EventArgs) Handles btnViewProfile.Click

        Dim f As New EditingForm_HeatExchanger_ViewProfile With {.hx = SimObject}
        f.Show()

    End Sub

    Private Sub GroupBox2_MouseMove(sender As Object, e As MouseEventArgs) Handles GroupBox2.MouseMove
        MyBase.Editor_MouseMove(sender, e)
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