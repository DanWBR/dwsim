Imports System.Windows.Forms
Imports DWSIM.Interfaces.Enums.GraphicObjects
Imports DWSIM.SharedClasses.UnitOperations
Imports su = DWSIM.SharedClasses.SystemsOfUnits
Imports DWSIM.UnitOperations.UnitOperations
Imports DWSIM.SharedClassesCSharp.FilePicker

Public Class EditingForm_FlowsheetUO

    Inherits SharedClasses.ObjectEditorForm

    Public Property SimObject As UnitOperations.Flowsheet

    Public Loaded As Boolean = False

    Dim units As SharedClasses.SystemsOfUnits.Units
    Dim nf As String

    Private Sub EditingForm_HeaterCooler_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        Me.ShowHint = GlobalSettings.Settings.DefaultEditFormLocation

        UpdateInfo()

        ChangeDefaultFont()

    End Sub

    Sub UpdateInfo()

        units = SimObject.FlowSheet.FlowsheetOptions.SelectedUnitSystem
        nf = SimObject.FlowSheet.FlowsheetOptions.NumberFormat

        Loaded = False

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
            cbInlet2.Items.Clear()
            cbInlet2.Items.AddRange(mslist)
            cbInlet3.Items.Clear()
            cbInlet3.Items.AddRange(mslist)
            cbInlet4.Items.Clear()
            cbInlet4.Items.AddRange(mslist)
            cbInlet5.Items.Clear()
            cbInlet5.Items.AddRange(mslist)
            cbInlet6.Items.Clear()
            cbInlet6.Items.AddRange(mslist)
            cbInlet7.Items.Clear()
            cbInlet7.Items.AddRange(mslist)
            cbInlet8.Items.Clear()
            cbInlet8.Items.AddRange(mslist)
            cbInlet9.Items.Clear()
            cbInlet9.Items.AddRange(mslist)
            cbInlet10.Items.Clear()
            cbInlet10.Items.AddRange(mslist)

            cbOutlet1.Items.Clear()
            cbOutlet1.Items.AddRange(mslist)
            cbOutlet2.Items.Clear()
            cbOutlet2.Items.AddRange(mslist)
            cbOutlet3.Items.Clear()
            cbOutlet3.Items.AddRange(mslist)
            cbOutlet4.Items.Clear()
            cbOutlet4.Items.AddRange(mslist)
            cbOutlet5.Items.Clear()
            cbOutlet5.Items.AddRange(mslist)
            cbOutlet6.Items.Clear()
            cbOutlet6.Items.AddRange(mslist)
            cbOutlet7.Items.Clear()
            cbOutlet7.Items.AddRange(mslist)
            cbOutlet8.Items.Clear()
            cbOutlet8.Items.AddRange(mslist)
            cbOutlet9.Items.Clear()
            cbOutlet9.Items.AddRange(mslist)
            cbOutlet10.Items.Clear()
            cbOutlet10.Items.AddRange(mslist)

            If .GraphicObject.InputConnectors(0).IsAttached Then cbInlet1.SelectedItem = .GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Tag
            If .GraphicObject.InputConnectors(1).IsAttached Then cbInlet2.SelectedItem = .GraphicObject.InputConnectors(1).AttachedConnector.AttachedFrom.Tag
            If .GraphicObject.InputConnectors(2).IsAttached Then cbInlet3.SelectedItem = .GraphicObject.InputConnectors(2).AttachedConnector.AttachedFrom.Tag
            If .GraphicObject.InputConnectors(3).IsAttached Then cbInlet4.SelectedItem = .GraphicObject.InputConnectors(3).AttachedConnector.AttachedFrom.Tag
            If .GraphicObject.InputConnectors(4).IsAttached Then cbInlet5.SelectedItem = .GraphicObject.InputConnectors(4).AttachedConnector.AttachedFrom.Tag
            If .GraphicObject.InputConnectors(5).IsAttached Then cbInlet6.SelectedItem = .GraphicObject.InputConnectors(5).AttachedConnector.AttachedFrom.Tag
            If .GraphicObject.InputConnectors(6).IsAttached Then cbInlet7.SelectedItem = .GraphicObject.InputConnectors(6).AttachedConnector.AttachedFrom.Tag
            If .GraphicObject.InputConnectors(7).IsAttached Then cbInlet8.SelectedItem = .GraphicObject.InputConnectors(7).AttachedConnector.AttachedFrom.Tag
            If .GraphicObject.InputConnectors(8).IsAttached Then cbInlet9.SelectedItem = .GraphicObject.InputConnectors(8).AttachedConnector.AttachedFrom.Tag
            If .GraphicObject.InputConnectors(9).IsAttached Then cbInlet10.SelectedItem = .GraphicObject.InputConnectors(9).AttachedConnector.AttachedFrom.Tag

            If .GraphicObject.OutputConnectors(0).IsAttached Then cbOutlet1.SelectedItem = .GraphicObject.OutputConnectors(0).AttachedConnector.AttachedTo.Tag
            If .GraphicObject.OutputConnectors(1).IsAttached Then cbOutlet2.SelectedItem = .GraphicObject.OutputConnectors(1).AttachedConnector.AttachedTo.Tag
            If .GraphicObject.OutputConnectors(2).IsAttached Then cbOutlet3.SelectedItem = .GraphicObject.OutputConnectors(2).AttachedConnector.AttachedTo.Tag
            If .GraphicObject.OutputConnectors(3).IsAttached Then cbOutlet4.SelectedItem = .GraphicObject.OutputConnectors(3).AttachedConnector.AttachedTo.Tag
            If .GraphicObject.OutputConnectors(4).IsAttached Then cbOutlet5.SelectedItem = .GraphicObject.OutputConnectors(4).AttachedConnector.AttachedTo.Tag
            If .GraphicObject.OutputConnectors(5).IsAttached Then cbOutlet6.SelectedItem = .GraphicObject.OutputConnectors(5).AttachedConnector.AttachedTo.Tag
            If .GraphicObject.OutputConnectors(6).IsAttached Then cbOutlet7.SelectedItem = .GraphicObject.OutputConnectors(6).AttachedConnector.AttachedTo.Tag
            If .GraphicObject.OutputConnectors(7).IsAttached Then cbOutlet8.SelectedItem = .GraphicObject.OutputConnectors(7).AttachedConnector.AttachedTo.Tag
            If .GraphicObject.OutputConnectors(8).IsAttached Then cbOutlet9.SelectedItem = .GraphicObject.OutputConnectors(8).AttachedConnector.AttachedTo.Tag
            If .GraphicObject.OutputConnectors(9).IsAttached Then cbOutlet10.SelectedItem = .GraphicObject.OutputConnectors(9).AttachedConnector.AttachedTo.Tag

            If .FileIsEmbedded Then
                rbFileEmbedded.Checked = True
            Else
                rbFileExternal.Checked = True
            End If

            Dim files = .FlowSheet.FileDatabaseProvider.GetFiles()
            cbEmbeddedFiles.Items.Clear()
            cbEmbeddedFiles.Items.AddRange(files.ToArray())
            Try
                cbEmbeddedFiles.SelectedItem = .EmbeddedFileName
            Catch ex As Exception
            End Try

            'parameters

            TbFileName.Text = .SimulationFile
            chkInitialize.Checked = .InitializeOnLoad
            chkRedirect.Checked = .RedirectOutput
            chkUpdateProcessData.Checked = .UpdateOnSave

            'variables

            If .Fsheet IsNot Nothing Then

                dgvinputvars.Rows.Clear()
                For Each item In .InputParams
                    If .Fsheet.SimulationObjects.ContainsKey(item.Value.ObjectID) Then
                        dgvinputvars.Rows.Add(New Object() {item.Key,
                                                        .Fsheet.SimulationObjects(item.Value.ObjectID).GraphicObject.Tag & ", " &
                                                        .FlowSheet.GetTranslatedString(item.Value.ObjectProperty),
                                                        .Fsheet.SimulationObjects(item.Value.ObjectID).GetPropertyValue(item.Value.ObjectProperty, .FlowSheet.FlowsheetOptions.SelectedUnitSystem),
                                                        .Fsheet.SimulationObjects(item.Value.ObjectID).GetPropertyUnit(item.Value.ObjectProperty, .FlowSheet.FlowsheetOptions.SelectedUnitSystem)})
                    End If
                Next

                dgvoutputvars.Rows.Clear()
                For Each item In .OutputParams
                    If .Fsheet.SimulationObjects.ContainsKey(item.Value.ObjectID) Then
                        dgvoutputvars.Rows.Add(New Object() { .Fsheet.SimulationObjects(item.Value.ObjectID).GraphicObject.Tag & ", " &
                                                        .FlowSheet.GetTranslatedString(item.Value.ObjectProperty),
                                                        .Fsheet.SimulationObjects(item.Value.ObjectID).GetPropertyValue(item.Value.ObjectProperty, .FlowSheet.FlowsheetOptions.SelectedUnitSystem),
                                                        .Fsheet.SimulationObjects(item.Value.ObjectID).GetPropertyUnit(item.Value.ObjectProperty, .FlowSheet.FlowsheetOptions.SelectedUnitSystem)})
                    End If
                Next

            End If

        End With

        Loaded = True

    End Sub

    Private Sub lblTag_TextChanged(sender As Object, e As EventArgs) Handles lblTag.TextChanged

        If Loaded Then ToolTipChangeTag.Show("Press ENTER to commit changes.", lblTag, New System.Drawing.Point(0, lblTag.Height + 3), 3000)

    End Sub

    Sub RequestCalc()

        SimObject.FlowSheet.RequestCalculation3(SimObject, False)

    End Sub

    Private Sub chkActive_CheckedChanged(sender As Object, e As EventArgs) Handles chkActive.CheckedChanged
        If Loaded Then
            SimObject.GraphicObject.Active = chkActive.Checked
            SimObject.FlowSheet.UpdateInterface()
            UpdateInfo()
        End If
    End Sub

    Private Sub btnCreateAndConnectInlet1_Click(sender As Object, e As EventArgs) Handles btnCreateAndConnectInlet1.Click, btnCreateAndConnectInlet2.Click,
                                                                                            btnCreateAndConnectInlet3.Click, btnCreateAndConnectInlet4.Click,
                                                                                            btnCreateAndConnectInlet5.Click, btnCreateAndConnectInlet6.Click,
                                                                                            btnCreateAndConnectInlet7.Click, btnCreateAndConnectInlet8.Click,
                                                                                            btnCreateAndConnectInlet9.Click, btnCreateAndConnectInlet10.Click,
                                                                                            btnCreateAndConnectOutlet1.Click, btnCreateAndConnectOutlet2.Click,
                                                                                            btnCreateAndConnectOutlet3.Click, btnCreateAndConnectOutlet4.Click,
                                                                                            btnCreateAndConnectOutlet5.Click, btnCreateAndConnectOutlet6.Click,
                                                                                            btnCreateAndConnectOutlet7.Click, btnCreateAndConnectOutlet8.Click,
                                                                                            btnCreateAndConnectOutlet9.Click, btnCreateAndConnectOutlet10.Click

        Dim sgobj = SimObject.GraphicObject
        Dim fs = SimObject.FlowSheet

        Dim iidx As Integer = -1
        Dim oidx As Integer = -1

        If sender Is btnCreateAndConnectInlet1 Then
            iidx = 0
        ElseIf sender Is btnCreateAndConnectInlet2 Then
            iidx = 1
        ElseIf sender Is btnCreateAndConnectInlet3 Then
            iidx = 2
        ElseIf sender Is btnCreateAndConnectInlet4 Then
            iidx = 3
        ElseIf sender Is btnCreateAndConnectInlet5 Then
            iidx = 4
        ElseIf sender Is btnCreateAndConnectInlet6 Then
            iidx = 5
        ElseIf sender Is btnCreateAndConnectInlet7 Then
            iidx = 6
        ElseIf sender Is btnCreateAndConnectInlet8 Then
            iidx = 7
        ElseIf sender Is btnCreateAndConnectInlet9 Then
            iidx = 8
        ElseIf sender Is btnCreateAndConnectInlet10 Then
            iidx = 9
        ElseIf sender Is btnCreateAndConnectOutlet1 Then
            oidx = 0
        ElseIf sender Is btnCreateAndConnectOutlet2 Then
            oidx = 1
        ElseIf sender Is btnCreateAndConnectOutlet3 Then
            oidx = 2
        ElseIf sender Is btnCreateAndConnectOutlet4 Then
            oidx = 3
        ElseIf sender Is btnCreateAndConnectOutlet5 Then
            oidx = 4
        ElseIf sender Is btnCreateAndConnectOutlet6 Then
            oidx = 5
        ElseIf sender Is btnCreateAndConnectOutlet7 Then
            oidx = 6
        ElseIf sender Is btnCreateAndConnectOutlet8 Then
            oidx = 7
        ElseIf sender Is btnCreateAndConnectOutlet9 Then
            oidx = 8
        ElseIf sender Is btnCreateAndConnectOutlet10 Then
            oidx = 9
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

    Private Sub btnDisconnect_Click(sender As Object, e As EventArgs) Handles btnDisconnect1.Click, btnDisconnect2.Click, btnDisconnect3.Click, btnDisconnect4.Click, btnDisconnect5.Click, btnDisconnect6.Click,
                                                                              btnDisconnect7.Click, btnDisconnect8.Click, btnDisconnect9.Click, btnDisconnect10.Click,
                                                                              btnDisconnectOutlet1.Click, btnDisconnectOutlet2.Click, btnDisconnectOutlet3.Click, btnDisconnectOutlet4.Click, btnDisconnectOutlet5.Click, btnDisconnectOutlet6.Click,
                                                                              btnDisconnectOutlet7.Click, btnDisconnectOutlet8.Click, btnDisconnectOutlet9.Click, btnDisconnectOutlet10.Click

        Dim iindex As Integer = -1
        Dim oindex As Integer = -1

        Select Case DirectCast(sender, Button).Name
            Case "btnDisconnect1"
                If cbInlet1.SelectedItem.ToString <> "" Then
                    iindex = 0
                    cbInlet1.SelectedItem = Nothing
                End If
            Case "btnDisconnect2"
                If cbInlet2.SelectedItem.ToString <> "" Then
                    iindex = 1
                    cbInlet2.SelectedItem = Nothing
                End If
            Case "btnDisconnect3"
                If cbInlet3.SelectedItem.ToString <> "" Then
                    iindex = 2
                    cbInlet3.SelectedItem = Nothing
                End If
            Case "btnDisconnect4"
                If cbInlet4.SelectedItem.ToString <> "" Then
                    iindex = 3
                    cbInlet4.SelectedItem = Nothing
                End If
            Case "btnDisconnect5"
                If cbInlet5.SelectedItem.ToString <> "" Then
                    iindex = 4
                    cbInlet5.SelectedItem = Nothing
                End If
            Case "btnDisconnect6"
                If cbInlet6.SelectedItem.ToString <> "" Then
                    iindex = 5
                    cbInlet6.SelectedItem = Nothing
                End If
            Case "btnDisconnect7"
                If cbInlet7.SelectedItem.ToString <> "" Then
                    iindex = 6
                    cbInlet7.SelectedItem = Nothing
                End If
            Case "btnDisconnect8"
                If cbInlet8.SelectedItem.ToString <> "" Then
                    iindex = 7
                    cbInlet8.SelectedItem = Nothing
                End If
            Case "btnDisconnect9"
                If cbInlet9.SelectedItem.ToString <> "" Then
                    iindex = 8
                    cbInlet9.SelectedItem = Nothing
                End If
            Case "btnDisconnect10"
                If cbInlet10.SelectedItem.ToString <> "" Then
                    iindex = 9
                    cbInlet10.SelectedItem = Nothing
                End If
            Case "btnDisconnectOutlet1"
                If cbOutlet1.SelectedItem.ToString <> "" Then
                    oindex = 0
                    cbOutlet1.SelectedItem = Nothing
                End If
            Case "btnDisconnectOutlet2"
                If cbOutlet2.SelectedItem.ToString <> "" Then
                    oindex = 1
                    cbOutlet2.SelectedItem = Nothing
                End If
            Case "btnDisconnectOutlet3"
                If cbOutlet3.SelectedItem.ToString <> "" Then
                    oindex = 2
                    cbOutlet3.SelectedItem = Nothing
                End If
            Case "btnDisconnectOutlet4"
                If cbOutlet4.SelectedItem.ToString <> "" Then
                    oindex = 3
                    cbOutlet4.SelectedItem = Nothing
                End If
            Case "btnDisconnectOutlet5"
                If cbOutlet5.SelectedItem.ToString <> "" Then
                    oindex = 4
                    cbOutlet5.SelectedItem = Nothing
                End If
            Case "btnDisconnectOutlet6"
                If cbOutlet6.SelectedItem.ToString <> "" Then
                    oindex = 5
                    cbOutlet6.SelectedItem = Nothing
                End If
            Case "btnDisconnectOutlet7"
                If cbOutlet7.SelectedItem.ToString <> "" Then
                    oindex = 6
                    cbOutlet7.SelectedItem = Nothing
                End If
            Case "btnDisconnectOutlet8"
                If cbOutlet8.SelectedItem.ToString <> "" Then
                    oindex = 7
                    cbOutlet8.SelectedItem = Nothing
                End If
            Case "btnDisconnectOutlet9"
                If cbOutlet9.SelectedItem.ToString <> "" Then
                    oindex = 8
                    cbOutlet9.SelectedItem = Nothing
                End If
            Case "btnDisconnectOutlet10"
                If cbOutlet10.SelectedItem.ToString <> "" Then
                    oindex = 9
                    cbOutlet10.SelectedItem = Nothing
                End If
        End Select

        If iindex > -1 Then SimObject.FlowSheet.DisconnectObjects(SimObject.GraphicObject.InputConnectors(iindex).AttachedConnector.AttachedFrom, SimObject.GraphicObject)
        If oindex > -1 Then SimObject.FlowSheet.DisconnectObjects(SimObject.GraphicObject, SimObject.GraphicObject.OutputConnectors(oindex).AttachedConnector.AttachedTo)

    End Sub

    Sub HandleInletConnections(sender As Object, e As EventArgs) Handles cbInlet1.SelectedIndexChanged, cbInlet2.SelectedIndexChanged,
                                                                        cbInlet3.SelectedIndexChanged, cbInlet4.SelectedIndexChanged, cbInlet5.SelectedIndexChanged,
                                                                        cbInlet6.SelectedIndexChanged, cbInlet7.SelectedIndexChanged, cbInlet8.SelectedIndexChanged,
                                                                        cbInlet9.SelectedIndexChanged, cbInlet10.SelectedIndexChanged
        If Loaded Then UpdateInletConnection(sender)
    End Sub

    Sub HandleOutletConnections(sender As Object, e As EventArgs) Handles cbOutlet1.SelectedIndexChanged, cbOutlet2.SelectedIndexChanged, cbOutlet3.SelectedIndexChanged,
                                                                            cbOutlet4.SelectedIndexChanged, cbOutlet5.SelectedIndexChanged, cbOutlet6.SelectedIndexChanged,
                                                                            cbOutlet7.SelectedIndexChanged, cbOutlet8.SelectedIndexChanged, cbOutlet9.SelectedIndexChanged, cbOutlet10.SelectedIndexChanged
        If Loaded Then UpdateOutletConnection(sender)

    End Sub

    Sub UpdateInletConnection(cb As ComboBox)

        Dim text As String = cb.Text

        If text <> "" Then

            Dim index As Integer = Convert.ToInt32(cb.Name.Substring(7)) - 1

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

    End Sub

    Sub UpdateOutletConnection(cb As ComboBox)

        Dim text As String = cb.Text

        If text <> "" Then

            Dim index As Integer = Convert.ToInt32(cb.Name.Substring(8)) - 1

            Dim gobj = SimObject.GraphicObject
            Dim flowsheet = SimObject.FlowSheet

            If flowsheet.GetFlowsheetSimulationObject(text).GraphicObject.InputConnectors(0).IsAttached Then
            Else
                Try
                    If gobj.OutputConnectors(index).IsAttached Then flowsheet.DisconnectObjects(gobj, gobj.OutputConnectors(index).AttachedConnector.AttachedTo)
                    flowsheet.ConnectObjects(gobj, flowsheet.GetFlowsheetSimulationObject(text).GraphicObject, index, 0)
                Catch ex As Exception
                    MessageBox.Show(ex.Message, flowsheet.GetTranslatedString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                End Try
            End If
            UpdateInfo()
        End If

    End Sub

    Private Sub dgvinputvars_CellValueChanged(sender As Object, e As DataGridViewCellEventArgs) Handles dgvinputvars.CellValueChanged

        If Loaded Then

            Dim par = SimObject.InputParams(dgvinputvars.Rows(e.RowIndex).Cells(0).Value)
            Dim value = dgvinputvars.Rows(e.RowIndex).Cells(e.ColumnIndex).Value.ToString()

            Try
                SimObject.Fsheet.SimulationObjects(par.ObjectID).SetPropertyValue(par.ObjectProperty, value.ToDoubleFromCurrent(), SimObject.FlowSheet.FlowsheetOptions.SelectedUnitSystem)
                RequestCalc()
                UpdateInfo()
            Catch ex As Exception
                SimObject.FlowSheet.ShowMessage(ex.Message.ToString, IFlowsheet.MessageType.GeneralError)
            End Try

        End If

    End Sub

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles btnOpenControlPanel.Click

        Dim f As New EditingForm_Flowsheet_Editor() With {.fsuo = SimObject}
        f.ShowDialog(Me)

    End Sub

    Private Sub BtnSearch_Click(sender As Object, e As EventArgs) Handles BtnSearch.Click

        Dim filePickerForm As IFilePicker = FilePickerService.GetInstance().GetFilePicker()

        If TypeOf filePickerForm Is DWSIM.SharedClassesCSharp.FilePicker.Windows.WindowsFilePicker Then
            Dim handler As IVirtualFile = filePickerForm.ShowOpenDialog(
            New List(Of FilePickerAllowedType) From {New FilePickerAllowedType("Simulation Files", New String() {"*.dwxml", "*.dwxmz"})})

            If handler IsNot Nothing Then
                TbFileName.Text = handler.FullPath
                If SimObject.SimulationFile <> handler.FullPath Then
                    SimObject.Initialized = False
                End If
                SimObject.SimulationFile = handler.FullPath
            End If
        Else
            MessageBox.Show("Sorry, this feature is not available. Try using an embedded flowsheet instead.")
        End If

    End Sub

    Private Sub btnViewFlowsheet_Click(sender As Object, e As EventArgs) Handles btnViewFlowsheet.Click

        Dim selectionControl As New EditingForm_Flowsheet_Viewer

        selectionControl.Text = SimObject.GraphicObject.Tag
        selectionControl.TabText = SimObject.GraphicObject.Tag
        selectionControl.fsuo = SimObject
        SimObject.FlowSheet.DisplayForm(selectionControl)

    End Sub

    Private Sub chkRedirect_CheckedChanged(sender As Object, e As EventArgs) Handles chkRedirect.CheckedChanged
        SimObject.RedirectOutput = chkRedirect.Checked
    End Sub

    Private Sub chkInitialize_CheckedChanged(sender As Object, e As EventArgs) Handles chkInitialize.CheckedChanged
        SimObject.InitializeOnLoad = chkInitialize.Checked
    End Sub

    Private Sub chkUpdateProcessData_CheckedChanged(sender As Object, e As EventArgs) Handles chkUpdateProcessData.CheckedChanged
        SimObject.UpdateOnSave = chkUpdateProcessData.Checked
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

    Private Sub rbFileEmbedded_CheckedChanged(sender As Object, e As EventArgs) Handles rbFileEmbedded.CheckedChanged
        If rbFileEmbedded.Checked Then
            TbFileName.Enabled = False
            BtnSearch.Enabled = False
            cbEmbeddedFiles.Enabled = True
            SimObject.FileIsEmbedded = True
        End If
        If rbFileExternal.Checked Then
            TbFileName.Enabled = True
            BtnSearch.Enabled = True
            cbEmbeddedFiles.Enabled = False
            SimObject.FileIsEmbedded = False
        End If
    End Sub

    Private Sub cbEmbeddedFiles_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbEmbeddedFiles.SelectedIndexChanged
        If Loaded Then
            SimObject.EmbeddedFileName = cbEmbeddedFiles.SelectedItem.ToString()
        End If
    End Sub
End Class