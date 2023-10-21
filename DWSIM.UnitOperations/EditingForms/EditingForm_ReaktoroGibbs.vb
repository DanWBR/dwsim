Imports System.Drawing
Imports DWSIM.Interfaces.Enums.GraphicObjects
Imports DWSIM.SharedClassesCSharp.FilePicker
Imports DWSIM.UnitOperations.Reactors

Public Class EditingForm_ReaktoroGibbs

    Inherits SharedClasses.ObjectEditorForm

    Public Property SimObject As Reactor_ReaktoroGibbs


    Public Loaded As Boolean = False

    Dim units As SharedClasses.SystemsOfUnits.Units
    Dim nf As String

    Private Sub EditingForm_WaterElectrolyzer_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        Me.ShowHint = GlobalSettings.Settings.DefaultEditFormLocation

        ChangeDefaultFont()

        UpdateInfo()

    End Sub

    Sub UpdateInfo()

        Loaded = False

        UpdateGHGEditor(gbGHG, SimObject)

        nf = SimObject.FlowSheet.FlowsheetOptions.NumberFormat

        units = SimObject.FlowSheet.FlowsheetOptions.SelectedUnitSystem

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

            If .GraphicObject.OutputConnectors(1).IsAttached Then cbEnergy.SelectedItem = .GraphicObject.OutputConnectors(1).AttachedConnector.AttachedTo.Tag

            'annotation

            Try
                rtbAnnotations.Rtf = .Annotation
            Catch ex As Exception

            End Try

            'property package

            Dim proppacks As String() = .FlowSheet.PropertyPackages.Values.Select(Function(m) m.Tag).ToArray
            cbPropPack.Items.Clear()
            cbPropPack.Items.AddRange(proppacks)
            cbPropPack.SelectedItem = .PropertyPackage?.Tag

            'properties

            cbDatabase.SelectedItem = .DatabaseName

            cbPDrop.Items.Clear()
            cbPDrop.Items.AddRange(units.GetUnitSet(Interfaces.Enums.UnitOfMeasure.deltaP).ToArray)
            cbPDrop.SelectedItem = units.deltaP

            tbPDrop.Text = .DeltaP.GetValueOrDefault().ConvertFromSI(units.deltaP).ToString(nf)

            CheckBox1.Checked = .GaseousPhase
            CheckBox2.Checked = .AqueousPhase
            CheckBox3.Checked = .LiquidPhase
            CheckBox4.Checked = .MineralPhase

            Dim cbComps As New DataGridViewComboBoxCell

            cbComps.Items.Add("")
            cbComps.Items.AddRange(.FlowSheet.SelectedCompounds.Keys.ToArray())

            DirectCast(gridSpeciesMappings.Columns(1), DataGridViewComboBoxColumn).CellTemplate = cbComps

            gridSpeciesMappings.Rows.Clear()
            For Each item In .SpeciesMaps
                gridSpeciesMappings.Rows.Add(New Object() {item.Key, item.Value})
            Next

            gridCompNames.Rows.Clear()
            For Each item In .CompoundNames
                gridCompNames.Rows.Add(New Object() {item.Key, item.Value})
            Next

            Dim comps = SimObject.FlowSheet.SelectedCompounds.Keys.ToList()

            lvComps.Clear()
            For Each c In comps
                lvComps.Items.Add(c)
                If .CompoundsList.Contains(c) Then lvComps.Items(lvComps.Items.Count - 1).Checked = True
            Next

            Dim elements As New List(Of String)
            For Each c In comps
                For Each el In SimObject.FlowSheet.SelectedCompounds(c).Elements.Keys
                    If Not elements.Contains(el) Then elements.Add(el)
                Next
            Next

            lvElements.Clear()
            For Each el In elements
                lvElements.Items.Add(el)
                If .ElementsList.Contains(el) Then lvElements.Items(lvElements.Items.Count - 1).Checked = True
            Next

            gridConversions.Rows.Clear()
            For Each conv In .ComponentConversions
                gridConversions.Rows.Add(New Object() {conv.Key, (conv.Value * 100).ToString(nf)})
            Next

            chkUseEmbeddedImage.Checked = .UseEmbeddedImage

            chkUseExternalDatabase.Checked = .UseExternalDatabase

            tbExternalDB.Text = .ExternalDatabaseFileName

        End With

        tbExternalDB.Visible = False
        chkUseExternalDatabase.Visible = False
        Button7.Visible = False

        Loaded = True

    End Sub

    Private Sub lblTag_TextChanged(sender As Object, e As EventArgs) Handles lblTag.TextChanged

        If Loaded Then ToolTipChangeTag.Show("Press ENTER to commit changes.", lblTag, New System.Drawing.Point(0, lblTag.Height + 3), 3000)

    End Sub

    Private Sub btnDisconnect1_Click(sender As Object, e As EventArgs) Handles btnDisconnect1.Click
        If cbInlet1.SelectedItem IsNot Nothing Then
            SimObject.FlowSheet.DisconnectObjects(SimObject.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom, SimObject.GraphicObject)
            cbInlet1.SelectedItem = Nothing
            SimObject.FlowSheet.UpdateInterface()
        End If
    End Sub

    Private Sub btnDisconnectOutlet1_Click(sender As Object, e As EventArgs) Handles btnDisconnectOutlet1.Click
        If cbOutlet1.SelectedItem IsNot Nothing Then
            SimObject.FlowSheet.DisconnectObjects(SimObject.GraphicObject, SimObject.GraphicObject.OutputConnectors(0).AttachedConnector.AttachedTo)
            cbOutlet1.SelectedItem = Nothing
            SimObject.FlowSheet.UpdateInterface()
        End If
    End Sub

    Private Sub btnDisconnectEnergy_Click(sender As Object, e As EventArgs) Handles btnDisconnectEnergy.Click
        If cbEnergy.SelectedItem IsNot Nothing Then
            SimObject.FlowSheet.DisconnectObjects(SimObject.GraphicObject, SimObject.GraphicObject.OutputConnectors(1).AttachedConnector.AttachedTo)
            cbEnergy.SelectedItem = Nothing
            SimObject.FlowSheet.UpdateInterface()
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

                flowsheet.UpdateInterface()

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
                SimObject.FlowSheet.UpdateInterface()

            End If

        End If

    End Sub

    Private Sub cbEnergy_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbEnergy.SelectedIndexChanged

        If Loaded Then

            Dim text As String = cbEnergy.Text

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
                SimObject.FlowSheet.UpdateInterface()

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

            Dim obj = fs.AddObject(ObjectType.EnergyStream, sgobj.OutputConnectors(1).Position.X + 30, sgobj.OutputConnectors(1).Position.Y + 30, "")

            If sgobj.OutputConnectors(1).IsAttached Then fs.DisconnectObjects(sgobj, sgobj.OutputConnectors(1).AttachedConnector.AttachedTo)
            fs.ConnectObjects(sgobj, obj.GraphicObject, 1, 0)

        End If
        SimObject.FlowSheet.UpdateInterface()

        UpdateInfo()

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

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click

        Dim comps = SimObject.FlowSheet.SelectedCompounds.Keys.ToList()

        lvComps.Clear()
        For Each c In comps
            lvComps.Items.Add(c)
        Next

    End Sub

    Private Sub Button2_Click(sender As Object, e As EventArgs) Handles Button2.Click

        Dim elements As New List(Of String)
        Dim comps = SimObject.FlowSheet.SelectedCompounds.Keys.ToList()
        For Each c In comps
            For Each el In SimObject.FlowSheet.SelectedCompounds(c).Elements.Keys
                If Not elements.Contains(el) Then elements.Add(el)
            Next
        Next

        lvElements.Clear()
        For Each el In elements
            lvElements.Items.Add(el)
        Next

    End Sub

    Private Sub lvComps_ItemChecked(sender As Object, e As ItemCheckedEventArgs) Handles lvComps.ItemChecked

        If Loaded Then

            Dim comp = e.Item.Text

            If e.Item.Checked Then
                SimObject.CompoundsList.Add(comp)
            Else
                SimObject.CompoundsList.Remove(comp)
            End If

        End If

    End Sub

    Private Sub lvElements_ItemChecked(sender As Object, e As ItemCheckedEventArgs) Handles lvElements.ItemChecked

        If Loaded Then

            Dim element = e.Item.Text

            If e.Item.Checked Then
                SimObject.ElementsList.Add(element)
            Else
                SimObject.ElementsList.Remove(element)
            End If

        End If

    End Sub

    Private Sub CheckBox1_CheckedChanged(sender As Object, e As EventArgs) Handles CheckBox1.CheckedChanged
        If Loaded Then SimObject.GaseousPhase = CheckBox1.Checked
    End Sub

    Private Sub CheckBox2_CheckedChanged(sender As Object, e As EventArgs) Handles CheckBox2.CheckedChanged
        If Loaded Then SimObject.AqueousPhase = CheckBox2.Checked
    End Sub

    Private Sub CheckBox3_CheckedChanged(sender As Object, e As EventArgs) Handles CheckBox3.CheckedChanged
        If Loaded Then SimObject.LiquidPhase = CheckBox3.Checked
    End Sub

    Private Sub CheckBox4_CheckedChanged(sender As Object, e As EventArgs) Handles CheckBox4.CheckedChanged
        If Loaded Then SimObject.MineralPhase = CheckBox4.Checked
    End Sub

    Private Sub cbPropPack_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbPropPack.SelectedIndexChanged
        If Loaded Then
            SimObject.PropertyPackage = SimObject.FlowSheet.PropertyPackages.Values.Where(Function(x) x.Tag = cbPropPack.SelectedItem.ToString).SingleOrDefault
        End If
    End Sub

    Private Sub btnConfigurePP_Click(sender As Object, e As EventArgs) Handles btnConfigurePP.Click
        SimObject.FlowSheet.PropertyPackages.Values.Where(Function(x) x.Tag =  cbPropPack.SelectedItem.ToString).FirstOrDefault()?.DisplayGroupedEditingForm()
    End Sub

    Private Sub tb_TextChanged(sender As Object, e As EventArgs) Handles tbPDrop.TextChanged

        Dim tbox = DirectCast(sender, TextBox)

        If tbox.Text.IsValidDoubleExpression Then
            tbox.ForeColor = System.Drawing.Color.Blue
        Else
            tbox.ForeColor = System.Drawing.Color.Red
        End If

    End Sub

    Private Sub grid_DataError(sender As Object, e As DataGridViewDataErrorEventArgs) Handles gridSpeciesMappings.DataError

        'data error

    End Sub

    Private Sub gridFeeds_CellValueChanged(sender As Object, e As DataGridViewCellEventArgs) Handles gridSpeciesMappings.CellValueChanged

        If Loaded Then
            Dim id = gridSpeciesMappings.Rows(e.RowIndex).Cells(0).Value
            Dim value = gridSpeciesMappings.Rows(e.RowIndex).Cells(1).Value
            SimObject.SpeciesMaps(id) = value
        End If

    End Sub

    Private Sub TextBoxKeyDown(sender As Object, e As KeyEventArgs) Handles tbPDrop.KeyDown

        If e.KeyCode = Keys.Enter And Loaded And DirectCast(sender, TextBox).ForeColor = System.Drawing.Color.Blue Then

            If sender Is tbPDrop Then SimObject.DeltaP = tbPDrop.Text.ToDoubleFromCurrent().ConvertToSI(units.deltaP)

            DirectCast(sender, TextBox).SelectAll()

        End If

    End Sub

    Private Sub cbDatabase_SelectedIndexChanged(sender As Object, e As EventArgs) Handles cbDatabase.SelectedIndexChanged
        If Loaded Then SimObject.DatabaseName = cbDatabase.SelectedItem.ToString()
    End Sub

    Private Sub Button3_Click(sender As Object, e As EventArgs) Handles Button3.Click

        Loaded = False

        Dim comps = SimObject.FlowSheet.SelectedCompounds.Values.ToList()
        gridCompNames.Rows.Clear()
        SimObject.CompoundNames.Clear()
        For Each c In comps
            SimObject.CompoundNames.Add(c.Name, c.Formula)
            gridCompNames.Rows.Add(New Object() {c.Name, c.Formula})
        Next

        Loaded = True

    End Sub

    Private Sub gridCompNames_CellValueChanged(sender As Object, e As DataGridViewCellEventArgs) Handles gridCompNames.CellValueChanged

        If Loaded Then

            Try

                Dim value = gridCompNames.Rows(e.RowIndex).Cells(1).Value

                Dim comp = gridCompNames.Rows(e.RowIndex).Cells(0).Value

                SimObject.CompoundNames(comp) = value

            Catch ex As Exception

                MessageBox.Show(ex.Message, "Error", MessageBoxButtons.OK, MessageBoxIcon.Error)

            End Try

        End If

    End Sub

    Private Sub Button4_Click(sender As Object, e As EventArgs) Handles Button4.Click
        Dim filePickerForm As IFilePicker = FilePickerService.GetInstance().GetFilePicker()

        Dim handler As IVirtualFile = filePickerForm.ShowOpenDialog(
            New List(Of FilePickerAllowedType) From {New FilePickerAllowedType("Image files", New String() {"*.bmp", "*.jpg", "*.png", "*.gif"})})

        If handler IsNot Nothing Then
            Using str = handler.OpenRead()
                Using bmp = Bitmap.FromStream(str)
                    Try
                        Using img = SkiaSharp.Views.Desktop.Extensions.ToSKImage(bmp)
                            SimObject.EmbeddedImageData = DWSIM.Drawing.SkiaSharp.GraphicObjects.Shapes.EmbeddedImageGraphic.ImageToBase64(img, SkiaSharp.SKEncodedImageFormat.Png)
                            MessageBox.Show("Image data read successfully.", "DWSIM", MessageBoxButtons.OK)
                        End Using
                    Catch ex As Exception
                        MessageBox.Show("Error reading image data.", "DWSIM", MessageBoxButtons.OK)
                    End Try
                End Using
            End Using
        End If
    End Sub

    Private Sub chkUseEmbeddedImage_CheckedChanged(sender As Object, e As EventArgs) Handles chkUseEmbeddedImage.CheckedChanged
        SimObject.UseEmbeddedImage = chkUseEmbeddedImage.Checked
    End Sub

    Private Sub Button5_Click(sender As Object, e As EventArgs) Handles Button5.Click
        SimObject.FlowSheet.DisplayBrowserWindow("https://reaktoro.org/v1/thermodynamic-databases.html")
    End Sub

    Private Sub Button6_Click(sender As Object, e As EventArgs) Handles Button6.Click

        Dim comps = SimObject.GetListOfCompounds()

        Dim ft = New FormViewMultilineText()
        ft.Text = String.Format("Reaktoro '{0}' Database Compounds List", SimObject.DatabaseName)
        ft.TextBox1.Text = comps
        ft.TextBox1.SelectedText = ""
        ft.ShowDialog()

    End Sub

    Private Sub Button7_Click(sender As Object, e As EventArgs) Handles Button7.Click

        Dim filePickerForm As IFilePicker = FilePickerService.GetInstance().GetFilePicker()

        Dim handler As IVirtualFile = filePickerForm.ShowOpenDialog(
            New List(Of FilePickerAllowedType) From {New FilePickerAllowedType("YAML files", "*.yaml")})

        If handler IsNot Nothing Then
            Try
                Dim text = handler.ReadAllText()
                tbExternalDB.Text = handler.Filename
                SimObject.ExternalDatabaseFileName = handler.Filename
                SimObject.ExternalDatabaseContents = text
            Catch ex As Exception
                MessageBox.Show(ex.Message, "Error", MessageBoxButtons.OK, MessageBoxIcon.Error)
            End Try
        End If

    End Sub

    Private Sub chkUseExternalDatabase_CheckedChanged(sender As Object, e As EventArgs) Handles chkUseExternalDatabase.CheckedChanged

        SimObject.UseExternalDatabase = chkUseExternalDatabase.Checked

    End Sub

End Class