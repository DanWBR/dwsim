Imports System.Windows.Forms
Imports DWSIM.Interfaces.Enums.GraphicObjects
Imports DWSIM.SharedClasses.UnitOperations
Imports su = DWSIM.SharedClasses.SystemsOfUnits

Public Class EditingForm_HeaterCooler

    Inherits WeifenLuo.WinFormsUI.Docking.DockContent

    Public Property SimObject As SharedClasses.UnitOperations.UnitOpBaseClass

    Public Loaded As Boolean = False

    Dim units As SharedClasses.SystemsOfUnits.Units
    Dim nf As String

    Private Sub EditingForm_HeaterCooler_Load(sender As Object, e As EventArgs) Handles MyBase.Load

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
                    lblStatus.Text = .FlowSheet.GetTranslatedString("Erro") & " (" & .ErrorMessage.Substring(50) & "...)"
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

            If TypeOf SimObject Is UnitOperations.Heater Then
                If .GraphicObject.InputConnectors(1).IsAttached Then cbEnergy.SelectedItem = .GraphicObject.InputConnectors(1).AttachedConnector.AttachedFrom.Tag
            Else
                If .GraphicObject.EnergyConnector.IsAttached Then cbEnergy.SelectedItem = .GraphicObject.EnergyConnector.AttachedConnector.AttachedTo.Tag
            End If

            'property package

            Dim proppacks As String() = .FlowSheet.PropertyPackages.Values.Select(Function(m) m.Tag).ToArray
            cbPropPack.Items.Clear()
            cbPropPack.Items.AddRange(proppacks)
            cbPropPack.SelectedItem = .PropertyPackage.Tag

            Dim flashalgos As String() = [Enum].GetNames(.PreferredFlashAlgorithm.GetType)
            cbFlashAlg.Items.Clear()
            cbFlashAlg.Items.AddRange(flashalgos)
            cbFlashAlg.SelectedItem = .PreferredFlashAlgorithm.ToString

            'annotation

            Try
                rtbAnnotations.Rtf = .Annotation
            Catch ex As Exception

            End Try

            'parameters

            cbTemp.Items.Clear()
            cbTemp.Items.AddRange(units.GetUnitSet(Interfaces.Enums.UnitOfMeasure.temperature).ToArray)
            cbTemp.SelectedItem = units.temperature

            cbPressureDropU.Items.Clear()
            cbPressureDropU.Items.AddRange(units.GetUnitSet(Interfaces.Enums.UnitOfMeasure.deltaP).ToArray)
            cbPressureDropU.SelectedItem = units.deltaP

            cbDeltaTemp.Items.Clear()
            cbDeltaTemp.Items.AddRange(units.GetUnitSet(Interfaces.Enums.UnitOfMeasure.deltaT).ToArray)
            cbDeltaTemp.SelectedItem = units.deltaT

            cbHeating.Items.Clear()
            cbHeating.Items.AddRange(units.GetUnitSet(Interfaces.Enums.UnitOfMeasure.heatflow).ToArray)
            cbHeating.SelectedItem = units.heatflow

            If TypeOf SimObject Is UnitOperations.Heater Then

                Dim uobj = DirectCast(SimObject, UnitOperations.Heater)

                Select Case uobj.CalcMode
                    Case UnitOperations.Heater.CalculationMode.HeatAdded
                        cbCalcMode.SelectedIndex = 0
                    Case UnitOperations.Heater.CalculationMode.TemperatureChange
                        cbCalcMode.SelectedIndex = 1
                    Case UnitOperations.Heater.CalculationMode.OutletTemperature
                        cbCalcMode.SelectedIndex = 2
                    Case UnitOperations.Heater.CalculationMode.OutletVaporFraction
                        cbCalcMode.SelectedIndex = 3
                    Case UnitOperations.Heater.CalculationMode.EnergyStream
                        cbCalcMode.SelectedIndex = 4
                End Select

                tbEfficiency.Text = uobj.Eficiencia.GetValueOrDefault.ToString(nf)
                tbHeatingChange.Text = su.Converter.ConvertFromSI(units.heatflow, uobj.DeltaQ.GetValueOrDefault).ToString(nf)
                tbOutletTemperature.Text = su.Converter.ConvertFromSI(units.temperature, uobj.OutletTemperature.GetValueOrDefault).ToString(nf)
                tbOutletVapFrac.Text = uobj.OutletVaporFraction.GetValueOrDefault.ToString(nf)
                tbPressureDrop.Text = su.Converter.ConvertFromSI(units.deltaP, uobj.DeltaP.GetValueOrDefault).ToString(nf)
                tbTemperatureChange.Text = su.Converter.ConvertFromSI(units.deltaT, uobj.DeltaT.GetValueOrDefault).ToString(nf)

            Else

                Dim uobj = DirectCast(SimObject, UnitOperations.Cooler)

                Select Case uobj.CalcMode
                    Case UnitOperations.Cooler.CalculationMode.HeatRemoved
                        cbCalcMode.SelectedIndex = 0
                    Case UnitOperations.Cooler.CalculationMode.TemperatureChange
                        cbCalcMode.SelectedIndex = 1
                    Case UnitOperations.Cooler.CalculationMode.OutletTemperature
                        cbCalcMode.SelectedIndex = 2
                    Case UnitOperations.Cooler.CalculationMode.OutletVaporFraction
                        cbCalcMode.SelectedIndex = 3
                End Select

                tbEfficiency.Text = uobj.Eficiencia.GetValueOrDefault.ToString(nf)
                tbHeatingChange.Text = su.Converter.ConvertFromSI(units.heatflow, uobj.DeltaQ.GetValueOrDefault).ToString(nf)
                tbOutletTemperature.Text = su.Converter.ConvertFromSI(units.temperature, uobj.OutletTemperature.GetValueOrDefault).ToString(nf)
                tbOutletVapFrac.Text = uobj.OutletVaporFraction.GetValueOrDefault.ToString(nf)
                tbPressureDrop.Text = su.Converter.ConvertFromSI(units.deltaP, uobj.DeltaP.GetValueOrDefault).ToString(nf)
                tbTemperatureChange.Text = su.Converter.ConvertFromSI(units.deltaT, uobj.DeltaT.GetValueOrDefault).ToString(nf)

            End If

        End With

        Loaded = True

    End Sub

    Private Sub btnConfigurePP_Click(sender As Object, e As EventArgs) Handles btnConfigurePP.Click
        SimObject.FlowSheet.PropertyPackages.Values.Where(Function(x) x.Tag = cbPropPack.SelectedItem.ToString).SingleOrDefault.DisplayEditingForm()
    End Sub

    Private Sub btnConfigureFlashAlg_Click(sender As Object, e As EventArgs) Handles btnConfigureFlashAlg.Click

        Dim fa As Interfaces.Enums.FlashMethod = [Enum].Parse(SimObject.PreferredFlashAlgorithm.GetType, cbFlashAlg.SelectedItem)

        Dim f As New Thermodynamics.FlashAlgorithmConfig() With {.Settings = SimObject.FlowSheet.FlowsheetOptions.FlashSettings(fa),
                                                                .AvailableCompounds = SimObject.FlowSheet.SelectedCompounds.Values.Select(Function(x) x.Name).ToList,
                                                                 .FlashAlgo = fa}
        f.ShowDialog(Me)

        SimObject.FlowSheet.FlowsheetOptions.FlashSettings(fa) = f.Settings

    End Sub

    Private Sub lblTag_TextChanged(sender As Object, e As EventArgs) Handles lblTag.TextChanged
        If Loaded Then SimObject.GraphicObject.Tag = lblTag.Text
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
        '0 Calor Adicionado / Removido
        '1 Variação de Temperatura
        '2 Temperatura na Saída
        '3 Fracao de Vapor na Saída
        '4 Corrente de Energia
        Select Case cbCalcMode.SelectedIndex
            Case 0
                tbTemperatureChange.Enabled = False
                tbOutletTemperature.Enabled = False
                tbOutletVapFrac.Enabled = False
                tbHeatingChange.Enabled = True
                If TypeOf SimObject Is UnitOperations.Heater Then
                    DirectCast(SimObject, UnitOperations.Heater).CalcMode = UnitOperations.Heater.CalculationMode.HeatAdded
                Else
                    DirectCast(SimObject, UnitOperations.Cooler).CalcMode = UnitOperations.Cooler.CalculationMode.HeatRemoved
                End If
            Case 1
                tbTemperatureChange.Enabled = True
                tbOutletTemperature.Enabled = False
                tbOutletVapFrac.Enabled = False
                tbHeatingChange.Enabled = False
                If TypeOf SimObject Is UnitOperations.Heater Then
                    DirectCast(SimObject, UnitOperations.Heater).CalcMode = UnitOperations.Heater.CalculationMode.TemperatureChange
                Else
                    DirectCast(SimObject, UnitOperations.Cooler).CalcMode = UnitOperations.Cooler.CalculationMode.TemperatureChange
                End If
            Case 2
                tbTemperatureChange.Enabled = False
                tbOutletTemperature.Enabled = True
                tbOutletVapFrac.Enabled = False
                tbHeatingChange.Enabled = False
                If TypeOf SimObject Is UnitOperations.Heater Then
                    DirectCast(SimObject, UnitOperations.Heater).CalcMode = UnitOperations.Heater.CalculationMode.OutletTemperature
                Else
                    DirectCast(SimObject, UnitOperations.Cooler).CalcMode = UnitOperations.Cooler.CalculationMode.OutletTemperature
                End If
            Case 3
                tbTemperatureChange.Enabled = False
                tbOutletTemperature.Enabled = False
                tbOutletVapFrac.Enabled = True
                tbHeatingChange.Enabled = False
                If TypeOf SimObject Is UnitOperations.Heater Then
                    DirectCast(SimObject, UnitOperations.Heater).CalcMode = UnitOperations.Heater.CalculationMode.OutletVaporFraction
                Else
                    DirectCast(SimObject, UnitOperations.Cooler).CalcMode = UnitOperations.Cooler.CalculationMode.OutletVaporFraction
                End If
            Case 4
                tbTemperatureChange.Enabled = False
                tbOutletTemperature.Enabled = False
                tbOutletVapFrac.Enabled = False
                tbHeatingChange.Enabled = False
                If TypeOf SimObject Is UnitOperations.Heater Then
                    DirectCast(SimObject, UnitOperations.Heater).CalcMode = UnitOperations.Heater.CalculationMode.EnergyStream
                Else
                    DirectCast(SimObject, UnitOperations.Cooler).CalcMode = UnitOperations.Cooler.CalculationMode.HeatRemoved
                End If
                If TypeOf SimObject Is UnitOperations.Cooler Then cbCalcMode.SelectedIndex = 0
        End Select
    End Sub

    Private Sub cb_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbDeltaTemp.SelectedIndexChanged, cbPressureDropU.SelectedIndexChanged,
                                                                                           cbTemp.SelectedIndexChanged, cbHeating.SelectedIndexChanged

        If Loaded Then
            Try
                If sender Is cbTemp Then
                    tbOutletTemperature.Text = su.Converter.Convert(cbTemp.SelectedItem.ToString, units.temperature, Double.Parse(tbOutletTemperature.Text)).ToString(nf)
                    cbTemp.SelectedItem = units.temperature
                    UpdateProps(tbOutletTemperature)
                ElseIf sender Is cbDeltaTemp Then
                    tbTemperatureChange.Text = su.Converter.Convert(cbDeltaTemp.SelectedItem.ToString, units.pressure, Double.Parse(tbTemperatureChange.Text)).ToString(nf)
                    cbDeltaTemp.SelectedItem = units.pressure
                    UpdateProps(tbTemperatureChange)
                ElseIf sender Is cbPressureDropU Then
                    tbPressureDrop.Text = su.Converter.Convert(cbPressureDropU.SelectedItem.ToString, units.massflow, Double.Parse(tbPressureDrop.Text)).ToString(nf)
                    cbPressureDropU.SelectedItem = units.massflow
                    UpdateProps(tbPressureDrop)
                ElseIf sender Is cbHeating Then
                    tbHeatingChange.Text = su.Converter.Convert(cbHeating.SelectedItem.ToString, units.molarflow, Double.Parse(tbHeatingChange.Text)).ToString(nf)
                    cbHeating.SelectedItem = units.molarflow
                    UpdateProps(tbHeatingChange)
                End If

            Catch ex As Exception
                SimObject.FlowSheet.ShowMessage(ex.Message.ToString, Interfaces.IFlowsheet.MessageType.GeneralError)
            End Try
        End If

    End Sub

    Sub UpdateProps(sender As Object)

        If TypeOf SimObject Is UnitOperations.Heater Then

            Dim uobj = DirectCast(SimObject, UnitOperations.Heater)

            Select Case cbCalcMode.SelectedIndex
                Case 0
                    uobj.CalcMode = UnitOperations.Heater.CalculationMode.HeatAdded
                Case 1
                    uobj.CalcMode = UnitOperations.Heater.CalculationMode.TemperatureChange
                Case 2
                    uobj.CalcMode = UnitOperations.Heater.CalculationMode.OutletTemperature
                Case 3
                    uobj.CalcMode = UnitOperations.Heater.CalculationMode.OutletVaporFraction
                Case 4
                    uobj.CalcMode = UnitOperations.Heater.CalculationMode.EnergyStream
            End Select

            If sender Is tbEfficiency Then uobj.Eficiencia = Double.Parse(tbEfficiency.Text)
            If sender Is tbHeatingChange Then uobj.DeltaQ = su.Converter.ConvertToSI(cbHeating.SelectedItem.ToString, tbHeatingChange.Text)
            If sender Is tbOutletTemperature Then uobj.OutletTemperature = su.Converter.ConvertToSI(cbTemp.SelectedItem.ToString, tbOutletTemperature.Text)
            If sender Is tbOutletVapFrac Then uobj.OutletVaporFraction = Double.Parse(tbOutletVapFrac.Text)
            If sender Is tbPressureDrop Then uobj.DeltaP = su.Converter.ConvertToSI(cbPressureDropU.SelectedItem.ToString, tbPressureDrop.Text)
            If sender Is tbTemperatureChange Then uobj.DeltaT = su.Converter.ConvertToSI(cbDeltaTemp.SelectedItem.ToString, tbTemperatureChange.Text)

        Else

            Dim uobj = DirectCast(SimObject, UnitOperations.Cooler)

            Select Case cbCalcMode.SelectedIndex
                Case 0
                    uobj.CalcMode = UnitOperations.Cooler.CalculationMode.HeatRemoved
                Case 1
                    uobj.CalcMode = UnitOperations.Cooler.CalculationMode.TemperatureChange
                Case 2
                    uobj.CalcMode = UnitOperations.Cooler.CalculationMode.OutletTemperature
                Case 3
                    uobj.CalcMode = UnitOperations.Cooler.CalculationMode.OutletVaporFraction
            End Select

            If sender Is tbEfficiency Then uobj.Eficiencia = Double.Parse(tbEfficiency.Text)
            If sender Is tbHeatingChange Then uobj.DeltaQ = su.Converter.ConvertToSI(cbHeating.SelectedItem.ToString, tbHeatingChange.Text)
            If sender Is tbOutletTemperature Then uobj.OutletTemperature = su.Converter.ConvertToSI(cbTemp.SelectedItem.ToString, tbOutletTemperature.Text)
            If sender Is tbOutletVapFrac Then uobj.OutletVaporFraction = Double.Parse(tbOutletVapFrac.Text)
            If sender Is tbPressureDrop Then uobj.DeltaP = su.Converter.ConvertToSI(cbPressureDropU.SelectedItem.ToString, tbPressureDrop.Text)
            If sender Is tbTemperatureChange Then uobj.DeltaT = su.Converter.ConvertToSI(cbDeltaTemp.SelectedItem.ToString, tbTemperatureChange.Text)

        End If

        RequestCalc()

    End Sub

    Sub RequestCalc()

        SimObject.FlowSheet.RequestCalculation(SimObject)

    End Sub

    Private Sub tb_TextChanged(sender As Object, e As EventArgs) Handles tbTemperatureChange.TextChanged, tbPressureDrop.TextChanged, tbOutletVapFrac.TextChanged, tbOutletTemperature.TextChanged,
                                                                        tbHeatingChange.TextChanged, tbEfficiency.TextChanged

        Dim tbox = DirectCast(sender, TextBox)

        If Double.TryParse(tbox.Text, New Double()) Then
            tbox.ForeColor = Drawing.Color.Blue
        Else
            tbox.ForeColor = Drawing.Color.Red
        End If

    End Sub

    Private Sub TextBoxKeyDown(sender As Object, e As KeyEventArgs) Handles tbTemperatureChange.KeyDown, tbPressureDrop.KeyDown, tbOutletVapFrac.KeyDown, tbOutletTemperature.KeyDown,
                                                                         tbHeatingChange.KeyDown, tbEfficiency.KeyDown

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
            SimObject.PreferredFlashAlgorithm = cbFlashAlg.SelectedIndex
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

            If TypeOf SimObject Is UnitOperations.Heater Then

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

End Class