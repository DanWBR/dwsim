Imports System.Windows.Forms
Imports DWSIM.Interfaces.Enums.GraphicObjects
Imports DWSIM.SharedClasses.UnitOperations
Imports su = DWSIM.SharedClasses.SystemsOfUnits
Imports DWSIM.UnitOperations.UnitOperations

Public Class EditingForm_CustomUO

    Inherits SharedClasses.ObjectEditorForm

    Public Property SimObject As UnitOperations.CustomUO

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

            chkUseEmbeddedImage.Checked = .UseEmbeddedImage

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
            cbInlet3.Items.Clear()
            cbInlet3.Items.AddRange(mslist)
            cbInlet4.Items.Clear()
            cbInlet4.Items.AddRange(mslist)
            cbInlet5.Items.Clear()
            cbInlet5.Items.AddRange(mslist)
            cbInlet6.Items.Clear()
            cbInlet6.Items.AddRange(mslist)

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

            If .GraphicObject.InputConnectors(0).IsAttached Then cbInlet1.SelectedItem = .GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Tag
            If .GraphicObject.InputConnectors(1).IsAttached Then cbInlet2.SelectedItem = .GraphicObject.InputConnectors(1).AttachedConnector.AttachedFrom.Tag
            If .GraphicObject.InputConnectors(2).IsAttached Then cbInlet3.SelectedItem = .GraphicObject.InputConnectors(2).AttachedConnector.AttachedFrom.Tag
            If .GraphicObject.InputConnectors(4).IsAttached Then cbInlet4.SelectedItem = .GraphicObject.InputConnectors(4).AttachedConnector.AttachedFrom.Tag
            If .GraphicObject.InputConnectors(5).IsAttached Then cbInlet5.SelectedItem = .GraphicObject.InputConnectors(5).AttachedConnector.AttachedFrom.Tag
            If .GraphicObject.InputConnectors(6).IsAttached Then cbInlet6.SelectedItem = .GraphicObject.InputConnectors(6).AttachedConnector.AttachedFrom.Tag
            If .GraphicObject.OutputConnectors(0).IsAttached Then cbOutlet1.SelectedItem = .GraphicObject.OutputConnectors(0).AttachedConnector.AttachedTo.Tag
            If .GraphicObject.OutputConnectors(1).IsAttached Then cbOutlet2.SelectedItem = .GraphicObject.OutputConnectors(1).AttachedConnector.AttachedTo.Tag
            If .GraphicObject.OutputConnectors(2).IsAttached Then cbOutlet3.SelectedItem = .GraphicObject.OutputConnectors(2).AttachedConnector.AttachedTo.Tag
            If .GraphicObject.OutputConnectors(4).IsAttached Then cbOutlet4.SelectedItem = .GraphicObject.OutputConnectors(4).AttachedConnector.AttachedTo.Tag
            If .GraphicObject.OutputConnectors(5).IsAttached Then cbOutlet5.SelectedItem = .GraphicObject.OutputConnectors(5).AttachedConnector.AttachedTo.Tag
            If .GraphicObject.OutputConnectors(6).IsAttached Then cbOutlet6.SelectedItem = .GraphicObject.OutputConnectors(6).AttachedConnector.AttachedTo.Tag

            Dim eslist As String() = .FlowSheet.SimulationObjects.Values.Where(Function(x) x.GraphicObject.ObjectType = ObjectType.EnergyStream).Select(Function(m) m.GraphicObject.Tag).ToArray

            cbEnergyE.Items.Clear()
            cbEnergyE.Items.AddRange(eslist)

            If .GraphicObject.InputConnectors(3).IsAttached Then cbEnergyE.SelectedItem = .GraphicObject.InputConnectors(3).AttachedConnector.AttachedFrom.Tag

            cbEnergyS.Items.Clear()
            cbEnergyS.Items.AddRange(eslist)

            If .GraphicObject.OutputConnectors(3).IsAttached Then cbEnergyS.SelectedItem = .GraphicObject.OutputConnectors(3).AttachedConnector.AttachedTo.Tag

            'exec engine

            cbExecEngine.SelectedIndex = .ExecutionEngine

            'variables

            dgvinputvars.Rows.Clear()
            For Each item In .InputVariables
                dgvinputvars.Rows.Add(New Object() {item.Key, item.Value})
            Next

            dgvinputstringvars.Rows.Clear()
            For Each item In .InputStringVariables
                dgvinputstringvars.Rows.Add(New Object() {item.Key, item.Value})
            Next

            dgvoutputvars.Rows.Clear()
            For Each item In .OutputVariables
                dgvoutputvars.Rows.Add(New Object() {item.Key, item.Value})
            Next

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

    Private Sub btnDisconnect1_Click(sender As Object, e As EventArgs)
        If cbInlet1.SelectedItem IsNot Nothing Then
            SimObject.FlowSheet.DisconnectObjects(SimObject.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom, SimObject.GraphicObject)
            cbInlet1.SelectedItem = Nothing
        End If
    End Sub

    Private Sub btnDisconnectOutlet1_Click(sender As Object, e As EventArgs)
        If cbOutlet1.SelectedItem IsNot Nothing Then
            SimObject.FlowSheet.DisconnectObjects(SimObject.GraphicObject, SimObject.GraphicObject.OutputConnectors(0).AttachedConnector.AttachedTo)
            cbOutlet1.SelectedItem = Nothing
        End If
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

    Private Sub cbEnergy_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbEnergyE.SelectedIndexChanged

        If Loaded Then

            Dim text As String = cbEnergyE.SelectedItem.ToString

            If text <> "" Then

                Dim gobj = SimObject.GraphicObject
                Dim flowsheet = SimObject.FlowSheet

                If flowsheet.GetFlowsheetSimulationObject(text).GraphicObject.OutputConnectors(0).IsAttached Then
                    MessageBox.Show(flowsheet.GetTranslatedString("Todasasconexespossve"), flowsheet.GetTranslatedString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                    Exit Sub
                End If

                If gobj.InputConnectors(3).IsAttached Then flowsheet.DisconnectObjects(gobj.InputConnectors(3).AttachedConnector.AttachedFrom, gobj)
                flowsheet.ConnectObjects(flowsheet.GetFlowsheetSimulationObject(text).GraphicObject, gobj, 0, 3)

            End If

        End If

    End Sub

    Private Sub cbEnergyS_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbEnergyS.SelectedIndexChanged

        If Loaded Then

            Dim text As String = cbEnergyS.SelectedItem.ToString

            If text <> "" Then

                Dim gobj = SimObject.GraphicObject
                Dim flowsheet = SimObject.FlowSheet

                If flowsheet.GetFlowsheetSimulationObject(text).GraphicObject.InputConnectors(0).IsAttached Then
                    MessageBox.Show(flowsheet.GetTranslatedString("Todasasconexespossve"), flowsheet.GetTranslatedString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                    Exit Sub
                End If

                If gobj.OutputConnectors(3).IsAttached Then flowsheet.DisconnectObjects(gobj, gobj.OutputConnectors(3).AttachedConnector.AttachedTo)
                flowsheet.ConnectObjects(gobj, flowsheet.GetFlowsheetSimulationObject(text).GraphicObject, 3, 0)

            End If

        End If

    End Sub

    Private Sub chkActive_CheckedChanged(sender As Object, e As EventArgs) Handles chkActive.CheckedChanged
        If Loaded Then SimObject.GraphicObject.Active = chkActive.Checked
    End Sub

    Private Sub btnCreateAndConnectInlet1_Click(sender As Object, e As EventArgs) Handles btnCreateAndConnectInlet1.Click, btnCreateAndConnectInlet2.Click,
                                                                                            btnCreateAndConnectInlet3.Click, btnCreateAndConnectInlet4.Click,
                                                                                            btnCreateAndConnectInlet5.Click, btnCreateAndConnectInlet6.Click,
                                                                                            btnCreateAndConnectOutlet1.Click, btnCreateAndConnectOutlet2.Click,
                                                                                            btnCreateAndConnectOutlet3.Click, btnCreateAndConnectOutlet4.Click,
                                                                                            btnCreateAndConnectOutlet5.Click, btnCreateAndConnectOutlet6.Click,
                                                                                            btnCreateAndConnectEnergy.Click, btnCreateAndConnectEnergyS.Click

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

            iidx = 4

        ElseIf sender Is btnCreateAndConnectInlet5 Then

            iidx = 5

        ElseIf sender Is btnCreateAndConnectInlet6 Then

            iidx = 6

        ElseIf sender Is btnCreateAndConnectOutlet1 Then

            oidx = 0

        ElseIf sender Is btnCreateAndConnectOutlet2 Then

            oidx = 1

        ElseIf sender Is btnCreateAndConnectOutlet3 Then

            oidx = 2

        ElseIf sender Is btnCreateAndConnectOutlet4 Then

            oidx = 4

        ElseIf sender Is btnCreateAndConnectOutlet5 Then

            oidx = 5

        ElseIf sender Is btnCreateAndConnectOutlet6 Then

            oidx = 6

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

        If sender Is btnCreateAndConnectEnergy Then

            Dim obj = fs.AddObject(ObjectType.EnergyStream, sgobj.InputConnectors(3).Position.X - 50, sgobj.InputConnectors(3).Position.Y, "")

            If sgobj.InputConnectors(3).IsAttached Then fs.DisconnectObjects(sgobj.InputConnectors(3).AttachedConnector.AttachedFrom, sgobj)
            fs.ConnectObjects(obj.GraphicObject, sgobj, 0, 3)

        End If

        If sender Is btnCreateAndConnectEnergyS Then

            Dim obj = fs.AddObject(ObjectType.EnergyStream, sgobj.OutputConnectors(3).Position.X + 30, sgobj.OutputConnectors(0).Position.Y, "")

            If sgobj.OutputConnectors(3).IsAttached Then fs.DisconnectObjects(sgobj, sgobj.OutputConnectors(3).AttachedConnector.AttachedTo)
            fs.ConnectObjects(sgobj, obj.GraphicObject, 3, 0)

        End If


        UpdateInfo()
        RequestCalc()

    End Sub

    Private Sub btnDisconnect_Click(sender As Object, e As EventArgs) Handles btnDisconnect1.Click, btnDisconnect2.Click, btnDisconnect3.Click, btnDisconnect4.Click, btnDisconnect5.Click, btnDisconnect6.Click,
                                                                              btnDisconnectOutlet1.Click, btnDisconnectOutlet2.Click, btnDisconnectOutlet3.Click, btnDisconnectOutlet4.Click, btnDisconnectOutlet5.Click, btnDisconnectOutlet6.Click,
                                                                              btnDisconnectEnergyE.Click, btnDisconnectEnergyS.Click

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
                    iindex = 4
                    cbInlet4.SelectedItem = Nothing
                End If
            Case "btnDisconnect5"
                If cbInlet5.SelectedItem.ToString <> "" Then
                    iindex = 5
                    cbInlet5.SelectedItem = Nothing
                End If
            Case "btnDisconnect6"
                If cbInlet6.SelectedItem.ToString <> "" Then
                    iindex = 6
                    cbInlet6.SelectedItem = Nothing
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
                    oindex = 4
                    cbOutlet4.SelectedItem = Nothing
                End If
            Case "btnDisconnectOutlet5"
                If cbOutlet5.SelectedItem.ToString <> "" Then
                    oindex = 5
                    cbOutlet5.SelectedItem = Nothing
                End If
            Case "btnDisconnectOutlet6"
                If cbOutlet6.SelectedItem.ToString <> "" Then
                    oindex = 6
                    cbOutlet6.SelectedItem = Nothing
                End If
            Case "btnDisconnectEnergyE"
                If cbEnergyE.SelectedItem.ToString <> "" Then
                    oindex = 3
                    cbEnergyE.SelectedItem = Nothing
                End If
            Case "btnDisconnectEnergyS"
                If cbEnergyS.SelectedItem.ToString <> "" Then
                    oindex = 3
                    cbEnergyS.SelectedItem = Nothing
                End If
        End Select

        If iindex > -1 Then SimObject.FlowSheet.DisconnectObjects(SimObject.GraphicObject.InputConnectors(iindex).AttachedConnector.AttachedFrom, SimObject.GraphicObject)
        If oindex > -1 Then SimObject.FlowSheet.DisconnectObjects(SimObject.GraphicObject, SimObject.GraphicObject.OutputConnectors(oindex).AttachedConnector.AttachedTo)

    End Sub

    Sub HandleInletConnections(sender As Object, e As EventArgs) Handles cbInlet1.SelectedIndexChanged, cbInlet2.SelectedIndexChanged, cbInlet3.SelectedIndexChanged, cbInlet4.SelectedIndexChanged, cbInlet5.SelectedIndexChanged, cbInlet6.SelectedIndexChanged
        If Loaded Then UpdateInletConnection(sender)
    End Sub

    Sub HandleOutletConnections(sender As Object, e As EventArgs) Handles cbOutlet1.SelectedIndexChanged, cbOutlet2.SelectedIndexChanged, cbOutlet3.SelectedIndexChanged, cbOutlet4.SelectedIndexChanged, cbOutlet5.SelectedIndexChanged, cbOutlet6.SelectedIndexChanged
        If Loaded Then UpdateOutletConnection(sender)
    End Sub

    Sub UpdateInletConnection(cb As ComboBox)

        Dim text As String = cb.Text

        If text <> "" Then

            Dim index As Integer = Convert.ToInt32(cb.Name.Substring(cb.Name.Length - 1)) - 1

            If index >= 3 Then index += 1

            Dim gobj = SimObject.GraphicObject
            Dim flowsheet = SimObject.FlowSheet

            If flowsheet.GetFlowsheetSimulationObject(text).GraphicObject.OutputConnectors(0).IsAttached Then
                MessageBox.Show(flowsheet.GetTranslatedString("Todasasconexespossve"), flowsheet.GetTranslatedString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                Exit Sub
            End If
            If gobj.InputConnectors(index).IsAttached Then flowsheet.DisconnectObjects(gobj.InputConnectors(index).AttachedConnector.AttachedFrom, gobj)
            flowsheet.ConnectObjects(flowsheet.GetFlowsheetSimulationObject(text).GraphicObject, gobj, 0, index)

        End If

    End Sub

    Sub UpdateOutletConnection(cb As ComboBox)

        Dim text As String = cb.Text

        If text <> "" Then

            Dim index As Integer = Convert.ToInt32(cb.Name.Substring(cb.Name.Length - 1)) - 1

            If index >= 3 Then index += 1

            Dim gobj = SimObject.GraphicObject
            Dim flowsheet = SimObject.FlowSheet

            If flowsheet.GetFlowsheetSimulationObject(text).GraphicObject.InputConnectors(0).IsAttached Then
                MessageBox.Show(flowsheet.GetTranslatedString("Todasasconexespossve"), flowsheet.GetTranslatedString("Erro"), MessageBoxButtons.OK, MessageBoxIcon.Error)
                Exit Sub
            End If
            If gobj.OutputConnectors(index).IsAttached Then flowsheet.DisconnectObjects(gobj, gobj.OutputConnectors(index).AttachedConnector.AttachedTo)
            flowsheet.ConnectObjects(gobj, flowsheet.GetFlowsheetSimulationObject(text).GraphicObject, index, 0)

        End If

    End Sub

    Private Sub ToolStripButton1_Click(sender As Object, e As EventArgs) Handles btnAddVar.Click

        dgvinputvars.Rows.Add(New Object() {"var" + (SimObject.InputVariables.Count + 1).ToString, "0"})

    End Sub

    Private Sub dgvinputvars_CellValueChanged(sender As Object, e As DataGridViewCellEventArgs) Handles dgvinputvars.CellValueChanged
        UpdateInputVars()
    End Sub

    Private Sub btnRemoveVar_Click(sender As Object, e As EventArgs) Handles btnRemoveVar.Click
        dgvinputvars.Rows.Remove(dgvinputvars.SelectedCells(0).OwningRow)
        UpdateInputVars()
    End Sub

    Private Sub UpdateInputVars()
        If Loaded Then
            SimObject.InputVariables.Clear()
            For Each row As DataGridViewRow In dgvinputvars.Rows
                Try
                    SimObject.InputVariables.Add(row.Cells(0).Value, row.Cells(1).Value)
                Catch ex As Exception
                    MessageBox.Show(ex.Message)
                End Try
            Next
        End If
    End Sub

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
        If Thermodynamics.Calculator.IsRunningOnMono Then
            Dim f As New EditingForm_CustomUO_ScriptEditor_Mono With {.ScriptUO = SimObject}
            SimObject.FlowSheet.DisplayForm(f)
        Else
            Dim f As New EditingForm_CustomUO_ScriptEditor With {.ScriptUO = SimObject}
            SimObject.FlowSheet.DisplayForm(f)
        End If
    End Sub

    Private Sub btnAddInputStringVar_Click(sender As Object, e As EventArgs) Handles btnAddInputStringVar.Click

        dgvinputstringvars.Rows.Add(New Object() {"var" + (SimObject.InputStringVariables.Count + 1).ToString, "myvar"})

    End Sub

    Private Sub btnRemoveInputStringVar_Click(sender As Object, e As EventArgs) Handles btnRemoveInputStringVar.Click
        dgvinputstringvars.Rows.Remove(dgvinputstringvars.SelectedCells(0).OwningRow)
        UpdateInputStringVars()
    End Sub

    Private Sub UpdateInputStringVars()
        If Loaded Then
            SimObject.InputStringVariables.Clear()
            For Each row As DataGridViewRow In dgvinputstringvars.Rows
                Try
                    SimObject.InputStringVariables.Add(row.Cells(0).Value, row.Cells(1).Value)
                Catch ex As Exception
                    MessageBox.Show(ex.Message)
                End Try
            Next
        End If
    End Sub

    Private Sub dgvinputstringvars_CellValueChanged(sender As Object, e As DataGridViewCellEventArgs) Handles dgvinputstringvars.CellValueChanged
        UpdateInputStringVars()
    End Sub

    Private Sub cbExecEngine_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbExecEngine.SelectedIndexChanged
        SimObject.ExecutionEngine = cbExecEngine.SelectedIndex
    End Sub

    Private Sub lblTag_KeyPress(sender As Object, e As KeyEventArgs) Handles lblTag.KeyUp

        If e.KeyCode = Keys.Enter Then

            If Loaded Then SimObject.GraphicObject.Tag = lblTag.Text
            If Loaded Then SimObject.FlowSheet.UpdateOpenEditForms()
            Me.Text = SimObject.GraphicObject.Tag & " (" & SimObject.GetDisplayName() & ")"
            DirectCast(SimObject.FlowSheet, Interfaces.IFlowsheetGUI).UpdateInterface()

        End If

    End Sub

    Private Sub chkUseEmbeddedImage_CheckedChanged(sender As Object, e As EventArgs) Handles chkUseEmbeddedImage.CheckedChanged
        SimObject.UseEmbeddedImage = chkUseEmbeddedImage.Checked
    End Sub

    Private Sub Button2_Click(sender As Object, e As EventArgs) Handles Button2.Click
        With Me.OpenFileName
            .CheckFileExists = True
            .CheckPathExists = True
            .Title = SimObject.FlowSheet.GetTranslatedString("Import")
            .Filter = "Images|*.bmp;*.jpg;*.png;*.gif"
            .AddExtension = True
            .Multiselect = False
            .RestoreDirectory = True
            Dim res As DialogResult = .ShowDialog
            If res = Windows.Forms.DialogResult.OK Then
                Try
                    Using bmp = CType(System.Drawing.Bitmap.FromFile(OpenFileName.FileName), System.Drawing.Bitmap)
                        Using img = SkiaSharp.Views.Desktop.Extensions.ToSKImage(bmp)
                            SimObject.EmbeddedImageData = DWSIM.Drawing.SkiaSharp.GraphicObjects.Shapes.EmbeddedImageGraphic.ImageToBase64(img, SkiaSharp.SKEncodedImageFormat.Png)
                            MessageBox.Show("Image data read successfully.", "DWSIM", MessageBoxButtons.OK)
                        End Using
                    End Using
                Catch ex As Exception
                    MessageBox.Show("Error reading image data.", "DWSIM", MessageBoxButtons.OK)
                End Try
            End If
        End With
    End Sub
End Class