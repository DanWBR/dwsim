Imports System.Windows.Forms
Imports DWSIM.Interfaces.Enums.GraphicObjects
Imports DWSIM.SharedClasses.UnitOperations
Imports su = DWSIM.SharedClasses.SystemsOfUnits
Imports DWSIM.UnitOperations.UnitOperations
Imports CapeOpen
Imports DWSIM.UnitOperations.UnitOperations.Auxiliary.CapeOpen

Public Class EditingForm_CAPEOPENUO

    Inherits SharedClasses.ObjectEditorForm

    Public Property SimObject As UnitOperations.CapeOpenUO

    Public Loaded As Boolean = False
    Public Editing As Boolean = False

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

            If Settings.IsRunningOnMono Then
                lblStatus.Text = "macOS/Linux Read-Only Bypass Mode"
                lblStatus.ForeColor = System.Drawing.Color.Green
            End If

            lblConnectedTo.Text = ""

            If .IsSpecAttached Then lblConnectedTo.Text = .FlowSheet.SimulationObjects(.AttachedSpecId).GraphicObject.Tag
            If .IsAdjustAttached Then lblConnectedTo.Text = .FlowSheet.SimulationObjects(.AttachedAdjustId).GraphicObject.Tag

            'cape-open info

            If Not ._seluo Is Nothing Then
                lblCOName.Text = ._seluo.Name
                lblCODesc.Text = ._seluo.Description
                lblCOVer.Text = ._seluo.Version & " / " & ._seluo.CapeVersion
            Else
                lblCOName.Text = "N/A"
                lblCODesc.Text = "N/A"
                lblCOVer.Text = "N/A"
            End If

            'connections

            Dim mslist As String() = .FlowSheet.GraphicObjects.Values.Where(Function(x) x.ObjectType = ObjectType.MaterialStream).Select(Function(m) m.Tag).ToArray
            Dim eslist As String() = .FlowSheet.SimulationObjects.Values.Where(Function(x) x.GraphicObject.ObjectType = ObjectType.EnergyStream).Select(Function(m) m.GraphicObject.Tag).ToArray

            DirectCast(dginlets.Columns(1), DataGridViewComboBoxColumn).Items.Clear()
            DirectCast(dginlets.Columns(1), DataGridViewComboBoxColumn).Items.AddRange(mslist)

            DirectCast(dgoutlets.Columns(1), DataGridViewComboBoxColumn).Items.Clear()
            DirectCast(dgoutlets.Columns(1), DataGridViewComboBoxColumn).Items.AddRange(mslist)

            DirectCast(dgenergy.Columns(1), DataGridViewComboBoxColumn).Items.Clear()
            DirectCast(dgenergy.Columns(1), DataGridViewComboBoxColumn).Items.AddRange(eslist)

            'populate ports

            dginlets.Rows.Clear()
            dgoutlets.Rows.Clear()
            dgenergy.Rows.Clear()

            Dim cnobj As Object = Nothing

            For Each p As UnitPort In ._ports
                If p.portType = CapePortType.CAPE_MATERIAL Then
                    Dim tag As String = ""
                    Try
                        cnobj = p.connectedObject
                    Catch ex As Exception
                        cnobj = Nothing
                    End Try
                    Dim conobj As Thermodynamics.Streams.MaterialStream = cnobj
                    If Not conobj Is Nothing Then tag = conobj.GraphicObject.Tag
                    If p.direction = CapePortDirection.CAPE_INLET Then
                        dginlets.Rows.Add(New Object() {p.ComponentName, tag})
                        dginlets.Rows(dginlets.Rows.Count - 1).Cells(0).Tag = p
                        dginlets.Rows(dginlets.Rows.Count - 1).Cells(2).ToolTipText = dginlets.Columns(2).ToolTipText
                        dginlets.Rows(dginlets.Rows.Count - 1).Cells(3).ToolTipText = dginlets.Columns(3).ToolTipText
                    Else
                        dgoutlets.Rows.Add(New Object() {p.ComponentName, tag})
                        dgoutlets.Rows(dgoutlets.Rows.Count - 1).Cells(0).Tag = p
                        dgoutlets.Rows(dgoutlets.Rows.Count - 1).Cells(2).ToolTipText = dgoutlets.Columns(2).ToolTipText
                        dgoutlets.Rows(dgoutlets.Rows.Count - 1).Cells(3).ToolTipText = dgoutlets.Columns(3).ToolTipText
                    End If
                ElseIf p.portType = CapePortType.CAPE_ENERGY Then
                    Dim tag As String = ""
                    Try
                        cnobj = p.connectedObject
                    Catch ex As Exception
                        cnobj = Nothing
                    End Try
                    Dim conobj As Streams.EnergyStream = cnobj
                    If Not conobj Is Nothing Then tag = conobj.GraphicObject.Tag
                    dgenergy.Rows.Add(New Object() {p.ComponentName, tag})
                    dgenergy.Rows(dgenergy.Rows.Count - 1).Cells(0).Tag = p
                    dgenergy.Rows(dgenergy.Rows.Count - 1).Cells(2).ToolTipText = dgenergy.Columns(2).ToolTipText
                    dgenergy.Rows(dgenergy.Rows.Count - 1).Cells(3).ToolTipText = dgenergy.Columns(3).ToolTipText
                End If
            Next

            'variables

            dgvinputvars.Rows.Clear()
            dgvoutputvars.Rows.Clear()

            For Each p As Object In ._params
                'find parameter type
                Dim myp As ICapeParameterSpec = TryCast(p, ICapeParameterSpec)
                Select Case myp.Type
                    Case CapeParamType.CAPE_ARRAY
                        Dim par As CapeArrayParameter = p
                        If par.value IsNot Nothing Then
                            If par.Mode <> CapeParamMode.CAPE_OUTPUT Then
                                dgvinputvars.Rows.Add(New Object() {par.ComponentName, DirectCast(par.value, Array).ToArrayString})
                                dgvinputvars.Rows(dgvinputvars.Rows.Count - 1).Cells(0).Tag = p
                            Else
                                dgvoutputvars.Rows.Add(New Object() {par.ComponentName, DirectCast(par.value, Array).ToArrayString})
                            End If
                        End If
                    Case CapeParamType.CAPE_BOOLEAN
                        Dim par As BooleanParameter = TryCast(p, BooleanParameter)
                        If par.Mode <> CapeParamMode.CAPE_OUTPUT Then
                            dgvinputvars.Rows.Add(New Object() {par.ComponentName, par.Value})
                            dgvinputvars.Rows(dgvinputvars.Rows.Count - 1).Cells(0).Tag = p
                        Else
                            dgvoutputvars.Rows.Add(New Object() {par.ComponentName, par.Value})
                        End If
                    Case CapeParamType.CAPE_INT
                        Dim par As IntegerParameter = TryCast(p, IntegerParameter)
                        If par.Mode <> CapeParamMode.CAPE_OUTPUT Then
                            dgvinputvars.Rows.Add(New Object() {par.ComponentName, par.Value})
                            dgvinputvars.Rows(dgvinputvars.Rows.Count - 1).Cells(0).Tag = p
                        Else
                            dgvoutputvars.Rows.Add(New Object() {par.ComponentName, par.Value})
                        End If
                    Case CapeParamType.CAPE_OPTION
                        Dim par As OptionParameter = TryCast(p, OptionParameter)
                        If par.Mode <> CapeParamMode.CAPE_OUTPUT Then
                            dgvinputvars.Rows.Add(New Object() {par.ComponentName, par.Value})
                            If par.OptionList IsNot Nothing Then dgvinputvars.Rows(dgvinputvars.Rows.Count - 1).Cells(1).ToolTipText = par.OptionList.ToArrayString
                            dgvinputvars.Rows(dgvinputvars.Rows.Count - 1).Cells(0).Tag = p
                        Else
                            dgvoutputvars.Rows.Add(New Object() {par.ComponentName, par.Value})
                        End If
                    Case CapeParamType.CAPE_REAL
                        Dim par As RealParameter = TryCast(p, RealParameter)
                        If par.Mode <> CapeParamMode.CAPE_OUTPUT Then
                            dgvinputvars.Rows.Add(New Object() {par.ComponentName, par.SIValue})
                            dgvinputvars.Rows(dgvinputvars.Rows.Count - 1).Cells(0).Tag = p
                        Else
                            dgvoutputvars.Rows.Add(New Object() {par.ComponentName, par.SIValue})
                        End If
                End Select
            Next

            Dim shapes As String() = [Enum].GetNames(.GraphicObject.ShapeOverride.GetType)
            cbOverrideShape.Items.Clear()
            cbOverrideShape.Items.AddRange(shapes)
            cbOverrideShape.SelectedItem = .GraphicObject.ShapeOverride.ToString

            'property package

            Dim proppacks As String() = .FlowSheet.PropertyPackages.Values.Select(Function(m) m.Tag).ToArray
            cbPropPack.Items.Clear()
            cbPropPack.Items.AddRange(proppacks)
            cbPropPack.SelectedItem = .PropertyPackage?.Tag

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

    Private Sub cbFlashAlg_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbFlashAlg.SelectedIndexChanged
        If Loaded Then
            SimObject.PreferredFlashAlgorithmTag = cbFlashAlg.SelectedItem.ToString
            RequestCalc()
        End If
    End Sub

    Private Sub chkActive_CheckedChanged(sender As Object, e As EventArgs) Handles chkActive.CheckedChanged
        If Loaded Then SimObject.GraphicObject.Active = chkActive.Checked
    End Sub

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
        If Not Settings.IsRunningOnMono Then
            SimObject.Edit()
            UpdateInfo()
        End If
    End Sub

    Private Sub dgvinputvars_CellValueChanged(sender As Object, e As DataGridViewCellEventArgs) Handles dgvinputvars.CellValueChanged

        If Loaded Then
            Dim p = dgvinputvars.Rows(e.RowIndex).Cells(0).Tag
            Dim myp As ICapeParameterSpec = TryCast(p, ICapeParameterSpec)
            Dim value = dgvinputvars.Rows(e.RowIndex).Cells(1).Value
            Try
                Select Case myp.Type
                    Case CapeParamType.CAPE_ARRAY
                        Dim par As CapeArrayParameter = p
                        par.value = value
                    Case CapeParamType.CAPE_BOOLEAN
                        Dim par As BooleanParameter = p
                        par.Value = value
                    Case CapeParamType.CAPE_INT
                        Dim par As IntegerParameter = p
                        par.Value = value
                    Case CapeParamType.CAPE_OPTION
                        Dim par As OptionParameter = p
                        par.Value = value
                    Case CapeParamType.CAPE_REAL
                        Dim par As RealParameter = p
                        par.SIValue = value
                End Select
                SimObject.RestoreParams()
                RequestCalc()
            Catch ex As Exception
                SimObject.FlowSheet.ShowMessage("Error: " & ex.Message, IFlowsheet.MessageType.GeneralError)
            End Try
        End If

    End Sub

    Private Sub dginlets_CellValueChanged(sender As Object, e As DataGridViewCellEventArgs) Handles dginlets.CellValueChanged, dgoutlets.CellValueChanged, dgenergy.CellValueChanged

        If Loaded And Not Editing Then

            Dim p As UnitPort = Nothing
            Dim obj As ISimulationObject = Nothing

            If sender Is dginlets Then

                p = dginlets.Rows(e.RowIndex).Cells(0).Tag
                obj = SimObject.FlowSheet.GetFlowsheetSimulationObject(dginlets.Rows(e.RowIndex).Cells(1).Value)

                UpdateInletConnection(obj, p)

            ElseIf sender Is dgoutlets Then

                p = dgoutlets.Rows(e.RowIndex).Cells(0).Tag
                obj = SimObject.FlowSheet.GetFlowsheetSimulationObject(dgoutlets.Rows(e.RowIndex).Cells(1).Value)

                UpdateOutletConnection(obj, p)

            ElseIf sender Is dgenergy Then

                p = dgenergy.Rows(e.RowIndex).Cells(0).Tag
                obj = SimObject.FlowSheet.GetFlowsheetSimulationObject(dgenergy.Rows(e.RowIndex).Cells(1).Value)

                If p.direction = CapePortDirection.CAPE_INLET Then

                    UpdateInletConnection(obj, p)

                Else

                    UpdateOutletConnection(obj, p)

                End If

            End If

            SimObject.UpdateConnectorPositions()
            UpdateInfo()
            RequestCalc()

        End If

    End Sub

    Sub UpdateInletConnection(obj As ISimulationObject, p As UnitPort)

        Dim fs = SimObject.FlowSheet
        Dim gobj = SimObject.GraphicObject

        If obj.GraphicObject.OutputConnectors(0).IsAttached Then
            MessageBox.Show(fs.GetTranslatedString("Todasasconexespossve"), fs.GetTranslatedString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
            Exit Sub
        End If
        Dim cpoint = gobj.InputConnectors.Where(Function(x) x.ConnectorName = p.ComponentName).FirstOrDefault
        If cpoint.IsAttached Then
            fs.DisconnectObjects(cpoint.AttachedConnector.AttachedFrom, gobj)
            p.Disconnect()
        End If
        fs.ConnectObjects(obj.GraphicObject, gobj, 0, gobj.InputConnectors.IndexOf(cpoint))
        p.Connect(obj)

    End Sub

    Sub UpdateOutletConnection(obj As ISimulationObject, p As UnitPort)

        Dim fs = SimObject.FlowSheet
        Dim gobj = SimObject.GraphicObject

        If obj.GraphicObject.InputConnectors(0).IsAttached Then
            MessageBox.Show(fs.GetTranslatedString("Todasasconexespossve"), fs.GetTranslatedString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
            Exit Sub
        End If
        Dim cpoint = gobj.OutputConnectors.Where(Function(x) x.ConnectorName = p.ComponentName).FirstOrDefault
        If cpoint.IsAttached Then
            fs.DisconnectObjects(gobj, cpoint.AttachedConnector.AttachedTo)
            p.Disconnect()
        End If
        fs.ConnectObjects(gobj, obj.GraphicObject, gobj.OutputConnectors.IndexOf(cpoint), 0)
        p.Connect(obj)

    End Sub

    Sub DisconnectInletConnection(obj As ISimulationObject, p As UnitPort)

        Dim fs = SimObject.FlowSheet
        Dim gobj = SimObject.GraphicObject

        Dim cpoint = gobj.InputConnectors.Where(Function(x) x.ConnectorName = p.ComponentName).FirstOrDefault
        If cpoint.IsAttached Then
            fs.DisconnectObjects(cpoint.AttachedConnector.AttachedFrom, gobj)
            p.Disconnect()
        End If

    End Sub

    Sub DisconnectOutletConnection(obj As ISimulationObject, p As UnitPort)

        Dim fs = SimObject.FlowSheet
        Dim gobj = SimObject.GraphicObject

        Dim cpoint = gobj.OutputConnectors.Where(Function(x) x.ConnectorName = p.ComponentName).FirstOrDefault
        If cpoint.IsAttached Then
            fs.DisconnectObjects(gobj, cpoint.AttachedConnector.AttachedTo)
            p.Disconnect()
        End If

    End Sub

    Function CreateAndConnectInletConnection(p As UnitPort, energy As Boolean) As String

        Dim fs = SimObject.FlowSheet
        Dim gobj = SimObject.GraphicObject

        Dim otype = If(Not energy, ObjectType.MaterialStream, ObjectType.EnergyStream)
        Dim cpoint = gobj.InputConnectors.Where(Function(x) x.ConnectorName = p.ComponentName).FirstOrDefault
        Dim obj = fs.AddObject(otype, cpoint.Position.X - 50, cpoint.Position.Y, "")

        fs.ConnectObjects(obj.GraphicObject, gobj, 0, gobj.InputConnectors.IndexOf(cpoint))
        p.Connect(obj)

        Return obj.GraphicObject.Tag

    End Function

    Function CreateAndConnectOutletConnection(p As UnitPort, energy As Boolean) As String

        Dim fs = SimObject.FlowSheet
        Dim gobj = SimObject.GraphicObject

        Dim otype = If(Not energy, ObjectType.MaterialStream, ObjectType.EnergyStream)
        Dim cpoint = gobj.OutputConnectors.Where(Function(x) x.ConnectorName = p.ComponentName).FirstOrDefault
        Dim obj = fs.AddObject(otype, cpoint.Position.X + 50, cpoint.Position.Y, "")

        fs.ConnectObjects(gobj, obj.GraphicObject, gobj.OutputConnectors.IndexOf(cpoint), 0)
        p.Connect(obj)

        Return obj.GraphicObject.Tag

    End Function

    Private Sub dginlets_CellContentClick(sender As Object, e As DataGridViewCellEventArgs) Handles dginlets.CellContentClick, dgoutlets.CellContentClick, dgenergy.CellContentClick

        If Loaded Then

            Dim p As UnitPort = Nothing
            Dim obj As ISimulationObject = Nothing

            Editing = True

            If sender Is dginlets Then

                p = dginlets.Rows(e.RowIndex).Cells(0).Tag
                obj = SimObject.FlowSheet.GetFlowsheetSimulationObject(dginlets.Rows(e.RowIndex).Cells(1).Value)

                If e.ColumnIndex = 3 Then
                    DisconnectInletConnection(obj, p)
                    dginlets.Rows(e.RowIndex).Cells(1).Value = Nothing
                ElseIf e.ColumnIndex = 2 Then
                    dginlets.Rows(e.RowIndex).Cells(1).Value = CreateAndConnectInletConnection(p, False)
                End If

            ElseIf sender Is dgoutlets Then

                p = dgoutlets.Rows(e.RowIndex).Cells(0).Tag
                obj = SimObject.FlowSheet.GetFlowsheetSimulationObject(dgoutlets.Rows(e.RowIndex).Cells(1).Value)

                If e.ColumnIndex = 3 Then
                    DisconnectOutletConnection(obj, p)
                    dgoutlets.Rows(e.RowIndex).Cells(1).Value = Nothing
                ElseIf e.ColumnIndex = 2 Then
                    dgoutlets.Rows(e.RowIndex).Cells(1).Value = CreateAndConnectOutletConnection(p, False)
                End If

            ElseIf sender Is dgenergy Then

                p = dgenergy.Rows(e.RowIndex).Cells(0).Tag
                obj = SimObject.FlowSheet.GetFlowsheetSimulationObject(dgenergy.Rows(e.RowIndex).Cells(1).Value)

                If p.direction = CapePortDirection.CAPE_INLET Then

                    If e.ColumnIndex = 3 Then
                        DisconnectInletConnection(obj, p)
                        dgenergy.Rows(e.RowIndex).Cells(1).Value = Nothing
                    ElseIf e.ColumnIndex = 2 Then
                        dgenergy.Rows(e.RowIndex).Cells(1).Value = CreateAndConnectInletConnection(p, True)
                    End If

                Else

                    If e.ColumnIndex = 3 Then
                        DisconnectOutletConnection(obj, p)
                        dgenergy.Rows(e.RowIndex).Cells(1).Value = Nothing
                    ElseIf e.ColumnIndex = 2 Then
                        dgenergy.Rows(e.RowIndex).Cells(1).Value = CreateAndConnectOutletConnection(p, True)
                    End If

                End If

            End If

            Editing = False

            SimObject.UpdateConnectorPositions()
            UpdateInfo()
            RequestCalc()

        End If

    End Sub

    Private Sub CheckBox1_CheckedChanged(sender As Object, e As EventArgs) Handles chkRecalc.CheckedChanged
        If Loaded Then
            SimObject.RecalcOutputStreams = chkRecalc.Checked
            RequestCalc()
        End If
    End Sub

    Private Sub cbOverrideShape_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbOverrideShape.SelectedIndexChanged
        If Loaded Then
            SimObject.GraphicObject.ShapeOverride = cbOverrideShape.SelectedIndex
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