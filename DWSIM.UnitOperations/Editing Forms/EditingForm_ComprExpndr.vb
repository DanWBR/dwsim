Imports System.Windows.Forms
Imports DWSIM.Interfaces.Enums.GraphicObjects
Imports DWSIM.SharedClasses.UnitOperations
Imports su = DWSIM.SharedClasses.SystemsOfUnits

Public Class EditingForm_ComprExpndr

    Inherits WeifenLuo.WinFormsUI.Docking.DockContent

    Public Property SimObject As SharedClasses.UnitOperations.UnitOpBaseClass

    Public Loaded As Boolean = False

    Dim units As SharedClasses.SystemsOfUnits.Units
    Dim nf As String

    Private Sub EditingForm_HeaterCooler_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        Me.ShowHint = GlobalSettings.Settings.DefaultEditFormLocation

        units = SimObject.FlowSheet.FlowsheetOptions.SelectedUnitSystem
        nf = SimObject.FlowSheet.FlowsheetOptions.NumberFormat

        UpdateInfo()

    End Sub

    Sub UpdateInfo()

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

            'connections

            Dim mslist As String() = .FlowSheet.GraphicObjects.Values.Where(Function(x) x.ObjectType = ObjectType.MaterialStream).Select(Function(m) m.Tag).ToArray

            cbInlet1.Items.Clear()
            cbInlet1.Items.AddRange(mslist)

            cbOutlet1.Items.Clear()
            cbOutlet1.Items.AddRange(mslist)

            If .GraphicObject.InputConnectors(0).IsAttached Then cbInlet1.SelectedItem = .GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Tag
            If .GraphicObject.OutputConnectors(0).IsAttached Then cbOutlet1.SelectedItem = .GraphicObject.OutputConnectors(0).AttachedConnector.AttachedTo.Tag

            Dim eslist As String() = .FlowSheet.SimulationObjects.Values.Where(Function(x) x.GraphicObject.ObjectType = ObjectType.EnergyStream).Select(Function(m) m.GraphicObject.Tag).ToArray

            cbEnergy.Items.Clear()
            cbEnergy.Items.AddRange(eslist)

            If TypeOf SimObject Is UnitOperations.Compressor Then
                If .GraphicObject.InputConnectors(1).IsAttached Then cbEnergy.SelectedItem = .GraphicObject.InputConnectors(1).AttachedConnector.AttachedFrom.Tag
            Else
                If .GraphicObject.EnergyConnector.IsAttached Then cbEnergy.SelectedItem = .GraphicObject.EnergyConnector.AttachedConnector.AttachedTo.Tag
            End If

            'property package

            Dim proppacks As String() = .FlowSheet.PropertyPackages.Values.Select(Function(m) m.Tag).ToArray
            cbPropPack.Items.Clear()
            cbPropPack.Items.AddRange(proppacks)
            cbPropPack.SelectedItem = .PropertyPackage.Tag

            Dim flashalgos As String() = .FlowSheet.FlowsheetOptions.FlashAlgorithms.Select(Function(x) x.Tag).ToArray
            cbFlashAlg.Items.Clear()
            cbFlashAlg.Items.AddRange(flashalgos)
            cbFlashAlg.SelectedItem = .PreferredFlashAlgorithmTag

            'annotation

            Try
                rtbAnnotations.Rtf = .Annotation
            Catch ex As Exception

            End Try

            'parameters

            cbPress.Items.Clear()
            cbPress.Items.AddRange(units.GetUnitSet(Interfaces.Enums.UnitOfMeasure.pressure).ToArray)
            cbPress.SelectedItem = units.pressure

            cbPressureDropU.Items.Clear()
            cbPressureDropU.Items.AddRange(units.GetUnitSet(Interfaces.Enums.UnitOfMeasure.deltaP).ToArray)
            cbPressureDropU.SelectedItem = units.deltaP

            cbPower.Items.Clear()
            cbPower.Items.AddRange(units.GetUnitSet(Interfaces.Enums.UnitOfMeasure.heatflow).ToArray)
            cbPower.SelectedItem = units.heatflow

            cbTemp.Items.Clear()
            cbTemp.Items.AddRange(units.GetUnitSet(Interfaces.Enums.UnitOfMeasure.temperature).ToArray)
            cbTemp.SelectedItem = units.temperature

            cbDeltaT.Items.Clear()
            cbDeltaT.Items.AddRange(units.GetUnitSet(Interfaces.Enums.UnitOfMeasure.deltaT).ToArray)
            cbDeltaT.SelectedItem = units.deltaT

            If TypeOf SimObject Is UnitOperations.Compressor Then

                Dim uobj = DirectCast(SimObject, UnitOperations.Compressor)

                Select Case uobj.CalcMode
                    Case UnitOperations.Compressor.CalculationMode.OutletPressure
                        cbCalcMode.SelectedIndex = 0
                    Case UnitOperations.Compressor.CalculationMode.Delta_P
                        cbCalcMode.SelectedIndex = 1
                    Case UnitOperations.Compressor.CalculationMode.PowerRequired
                        cbCalcMode.SelectedIndex = 2
                    Case UnitOperations.Compressor.CalculationMode.EnergyStream
                        cbCalcMode.SelectedIndex = 3
                End Select

                tbEfficiency.Text = uobj.EficienciaAdiabatica.GetValueOrDefault.ToString(nf)
                tbPower.Text = su.Converter.ConvertFromSI(units.heatflow, uobj.DeltaQ.GetValueOrDefault).ToString(nf)
                tbOutletPressure.Text = su.Converter.ConvertFromSI(units.temperature, uobj.POut.GetValueOrDefault).ToString(nf)
                tbPressureDrop.Text = su.Converter.ConvertFromSI(units.deltaP, uobj.DeltaP.GetValueOrDefault).ToString(nf)
                tbTemp.Text = su.Converter.ConvertFromSI(units.temperature, uobj.OutletTemperature).ToString(nf)
                tbDeltaT.Text = su.Converter.ConvertFromSI(units.deltaT, uobj.DeltaT.GetValueOrDefault).ToString(nf)

            Else

                Dim uobj = DirectCast(SimObject, UnitOperations.Expander)

                Select Case uobj.CalcMode
                    Case UnitOperations.Expander.CalculationMode.OutletPressure
                        cbCalcMode.SelectedIndex = 0
                    Case UnitOperations.Expander.CalculationMode.Delta_P
                        cbCalcMode.SelectedIndex = 1
                    Case UnitOperations.Expander.CalculationMode.PowerGenerated
                        cbCalcMode.SelectedIndex = 2
                End Select

                tbEfficiency.Text = uobj.EficienciaAdiabatica.GetValueOrDefault.ToString(nf)
                tbPower.Text = su.Converter.ConvertFromSI(units.heatflow, uobj.DeltaQ.GetValueOrDefault).ToString(nf)
                tbOutletPressure.Text = su.Converter.ConvertFromSI(units.temperature, uobj.POut.GetValueOrDefault).ToString(nf)
                tbPressureDrop.Text = su.Converter.ConvertFromSI(units.deltaP, uobj.DeltaP.GetValueOrDefault).ToString(nf)
                tbTemp.Text = su.Converter.ConvertFromSI(units.temperature, uobj.OutletTemperature).ToString(nf)
                tbDeltaT.Text = su.Converter.ConvertFromSI(units.deltaT, uobj.DeltaT.GetValueOrDefault).ToString(nf)

            End If

        End With

        Loaded = True

    End Sub

    Private Sub btnConfigurePP_Click(sender As Object, e As EventArgs) Handles btnConfigurePP.Click
        SimObject.FlowSheet.PropertyPackages.Values.Where(Function(x) x.Tag = cbPropPack.SelectedItem.ToString).SingleOrDefault.DisplayEditingForm()
    End Sub

    Private Sub btnConfigureFlashAlg_Click(sender As Object, e As EventArgs) Handles btnConfigureFlashAlg.Click

        'Dim fa As Interfaces.Enums.FlashMethod = [Enum].Parse(SimObject.PreferredFlashAlgorithm.GetType, cbFlashAlg.SelectedItem)

        'Dim f As New Thermodynamics.FlashAlgorithmConfig() With {.Settings = SimObject.FlowSheet.FlowsheetOptions.FlashSettings(fa),
        '                                                        .AvailableCompounds = SimObject.FlowSheet.SelectedCompounds.Values.Select(Function(x) x.Name).ToList,
        '                                                         .FlashAlgo = fa}
        'f.ShowDialog(Me)

        'SimObject.FlowSheet.FlowsheetOptions.FlashSettings(fa) = f.Settings

    End Sub

    Private Sub lblTag_TextChanged(sender As Object, e As EventArgs) Handles lblTag.TextChanged
        If Loaded Then SimObject.GraphicObject.Tag = lblTag.Text
        Me.Text = SimObject.GetDisplayName() & ": " & SimObject.GraphicObject.Tag
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
        If TypeOf SimObject Is UnitOperations.Heater Then
            If cbEnergy.SelectedItem IsNot Nothing Then
                SimObject.FlowSheet.DisconnectObjects(SimObject.GraphicObject.InputConnectors(1).AttachedConnector.AttachedFrom, SimObject.GraphicObject)
                cbEnergy.SelectedItem = Nothing
            End If
        Else
            If cbEnergy.SelectedItem IsNot Nothing Then
                SimObject.FlowSheet.DisconnectObjects(SimObject.GraphicObject, SimObject.GraphicObject.EnergyConnector.AttachedConnector.AttachedTo)
                cbEnergy.SelectedItem = Nothing
            End If
        End If
    End Sub

    Private Sub cbCalcMode_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbCalcMode.SelectedIndexChanged
        'Pressão na Saída
        'Variação da Pressão
        'Potência Fornecida / Produzida
        'Corrente de Energia
        Select Case cbCalcMode.SelectedIndex
            Case 0
                tbPressureDrop.Enabled = False
                tbOutletPressure.Enabled = True
                tbPower.Enabled = False
                If TypeOf SimObject Is UnitOperations.Compressor Then
                    DirectCast(SimObject, UnitOperations.Compressor).CalcMode = UnitOperations.Compressor.CalculationMode.OutletPressure
                Else
                    DirectCast(SimObject, UnitOperations.Expander).CalcMode = UnitOperations.Expander.CalculationMode.OutletPressure
                End If
            Case 1
                tbPressureDrop.Enabled = True
                tbOutletPressure.Enabled = False
                tbPower.Enabled = False
                If TypeOf SimObject Is UnitOperations.Compressor Then
                    DirectCast(SimObject, UnitOperations.Compressor).CalcMode = UnitOperations.Compressor.CalculationMode.Delta_P
                Else
                    DirectCast(SimObject, UnitOperations.Expander).CalcMode = UnitOperations.Expander.CalculationMode.Delta_P
                End If
            Case 2
                tbPressureDrop.Enabled = False
                tbOutletPressure.Enabled = False
                tbPower.Enabled = True
                If TypeOf SimObject Is UnitOperations.Compressor Then
                    DirectCast(SimObject, UnitOperations.Compressor).CalcMode = UnitOperations.Compressor.CalculationMode.PowerRequired
                Else
                    DirectCast(SimObject, UnitOperations.Expander).CalcMode = UnitOperations.Expander.CalculationMode.PowerGenerated
                End If
            Case 3
                tbPressureDrop.Enabled = False
                tbOutletPressure.Enabled = False
                tbPower.Enabled = False
                If TypeOf SimObject Is UnitOperations.Compressor Then
                    DirectCast(SimObject, UnitOperations.Compressor).CalcMode = UnitOperations.Compressor.CalculationMode.EnergyStream
                Else
                    DirectCast(SimObject, UnitOperations.Expander).CalcMode = UnitOperations.Expander.CalculationMode.PowerGenerated
                End If
                If TypeOf SimObject Is UnitOperations.Expander Then cbCalcMode.SelectedIndex = 2
        End Select
    End Sub

    Private Sub cb_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbPressureDropU.SelectedIndexChanged,
                                                                                  cbPress.SelectedIndexChanged,
                                                                                  cbPower.SelectedIndexChanged

        If Loaded Then
            Try
                If sender Is cbPress Then
                    tbOutletPressure.Text = su.Converter.Convert(cbPress.SelectedItem.ToString, units.pressure, Double.Parse(tbOutletPressure.Text)).ToString(nf)
                    cbPress.SelectedItem = units.pressure
                    UpdateProps(tbOutletPressure)
                ElseIf sender Is cbPressureDropU Then
                    tbPressureDrop.Text = su.Converter.Convert(cbPressureDropU.SelectedItem.ToString, units.deltaP, Double.Parse(tbPressureDrop.Text)).ToString(nf)
                    cbPressureDropU.SelectedItem = units.deltaP
                    UpdateProps(tbPressureDrop)
                ElseIf sender Is cbPower Then
                    tbPower.Text = su.Converter.Convert(cbPower.SelectedItem.ToString, units.heatflow, Double.Parse(tbPower.Text)).ToString(nf)
                    cbPower.SelectedItem = units.heatflow
                    UpdateProps(tbPower)
                End If

            Catch ex As Exception
                SimObject.FlowSheet.ShowMessage(ex.Message.ToString, Interfaces.IFlowsheet.MessageType.GeneralError)
            End Try
        End If

    End Sub

    Sub UpdateProps(sender As Object)

        'Pressão na Saída
        'Variação da Pressão
        'Potência Fornecida / Produzida
        'Corrente de Energia

        If TypeOf SimObject Is UnitOperations.Compressor Then

            Dim uobj = DirectCast(SimObject, UnitOperations.Compressor)

            Select Case cbCalcMode.SelectedIndex
                Case 0
                    uobj.CalcMode = UnitOperations.Compressor.CalculationMode.OutletPressure
                Case 1
                    uobj.CalcMode = UnitOperations.Compressor.CalculationMode.Delta_P
                Case 2
                    uobj.CalcMode = UnitOperations.Compressor.CalculationMode.PowerRequired
                Case 3
                    uobj.CalcMode = UnitOperations.Compressor.CalculationMode.EnergyStream
            End Select

            If sender Is tbEfficiency Then uobj.EficienciaAdiabatica = Double.Parse(tbEfficiency.Text)
            If sender Is tbPower Then uobj.DeltaQ = su.Converter.ConvertToSI(cbPower.SelectedItem.ToString, tbPower.Text)
            If sender Is tbOutletPressure Then uobj.POut = su.Converter.ConvertToSI(cbPress.SelectedItem.ToString, tbOutletPressure.Text)
            If sender Is tbPressureDrop Then uobj.DeltaP = su.Converter.ConvertToSI(cbPressureDropU.SelectedItem.ToString, tbPressureDrop.Text)
      
        Else

            Dim uobj = DirectCast(SimObject, UnitOperations.Expander)

            Select Case cbCalcMode.SelectedIndex
                Case 0
                    uobj.CalcMode = UnitOperations.Expander.CalculationMode.OutletPressure
                Case 1
                    uobj.CalcMode = UnitOperations.Expander.CalculationMode.Delta_P
                Case 2
                    uobj.CalcMode = UnitOperations.Expander.CalculationMode.PowerGenerated
            End Select

            If sender Is tbEfficiency Then uobj.EficienciaAdiabatica = Double.Parse(tbEfficiency.Text)
            If sender Is tbPower Then uobj.DeltaQ = su.Converter.ConvertToSI(cbPower.SelectedItem.ToString, tbPower.Text)
            If sender Is tbOutletPressure Then uobj.POut = su.Converter.ConvertToSI(cbPress.SelectedItem.ToString, tbOutletPressure.Text)
            If sender Is tbPressureDrop Then uobj.DeltaP = su.Converter.ConvertToSI(cbPressureDropU.SelectedItem.ToString, tbPressureDrop.Text)

        End If

        RequestCalc()

    End Sub

    Sub RequestCalc()

        SimObject.FlowSheet.RequestCalculation(SimObject)

    End Sub

    Private Sub tb_TextChanged(sender As Object, e As EventArgs) Handles tbPressureDrop.TextChanged, tbOutletPressure.TextChanged,
                                                                        tbPower.TextChanged, tbEfficiency.TextChanged

        Dim tbox = DirectCast(sender, TextBox)

        If Double.TryParse(tbox.Text, New Double()) Then
            tbox.ForeColor = Drawing.Color.Blue
        Else
            tbox.ForeColor = Drawing.Color.Red
        End If

    End Sub

    Private Sub TextBoxKeyDown(sender As Object, e As KeyEventArgs) Handles tbPressureDrop.KeyDown, tbOutletPressure.KeyDown,
                                                                         tbPower.KeyDown, tbEfficiency.KeyDown

        If e.KeyCode = Keys.Enter And Loaded And DirectCast(sender, TextBox).ForeColor = Drawing.Color.Blue Then

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

    Private Sub cbEnergy_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbEnergy.SelectedIndexChanged

        If Loaded Then

            Dim text As String = cbEnergy.Text

            If TypeOf SimObject Is UnitOperations.Compressor Then

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

            Else

                If text <> "" Then

                    Dim index As Integer = 0

                    Dim gobj = SimObject.GraphicObject
                    Dim flowsheet = SimObject.FlowSheet

                    If flowsheet.GetFlowsheetSimulationObject(text).GraphicObject.InputConnectors(0).IsAttached Then
                        MessageBox.Show(flowsheet.GetTranslatedString("Todasasconexespossve"), flowsheet.GetTranslatedString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                        Exit Sub
                    End If

                    If gobj.EnergyConnector.IsAttached Then flowsheet.DisconnectObjects(gobj, gobj.EnergyConnector.AttachedConnector.AttachedTo)
                    flowsheet.ConnectObjects(gobj, flowsheet.GetFlowsheetSimulationObject(text).GraphicObject, 0, 0)

                End If

            End If

        End If

    End Sub

    Private Sub rtbAnnotations_RtfChanged(sender As Object, e As EventArgs) Handles rtbAnnotations.RtfChanged
        If Loaded Then SimObject.Annotation = rtbAnnotations.Rtf
    End Sub

    Private Sub chkActive_CheckedChanged(sender As Object, e As EventArgs) Handles chkActive.CheckedChanged
        If Loaded Then SimObject.GraphicObject.Active = chkActive.Checked
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

            Dim obj = fs.AddObject(ObjectType.EnergyStream, sgobj.EnergyConnector.Position.X + 30, sgobj.EnergyConnector.Position.Y + 30, "")

            If TypeOf SimObject Is UnitOperations.Compressor Then

                If sgobj.InputConnectors(1).IsAttached Then fs.DisconnectObjects(sgobj.InputConnectors(1).AttachedConnector.AttachedFrom, sgobj)
                fs.ConnectObjects(obj.GraphicObject, sgobj, 0, 1)

            Else

                If sgobj.EnergyConnector.IsAttached Then fs.DisconnectObjects(sgobj, sgobj.EnergyConnector.AttachedConnector.AttachedTo)
                fs.ConnectObjects(sgobj, obj.GraphicObject, 0, 0)

            End If

        End If

        UpdateInfo()
        RequestCalc()

    End Sub

End Class