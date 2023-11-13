Imports DWSIM.Drawing.SkiaSharp.GraphicObjects
Imports WeifenLuo.WinFormsUI.Docking
Imports System.Linq
Imports System.Threading.Tasks
Imports DWSIM.UnitOperations
Imports DWSIM.SharedClasses.DWSIM.Flowsheet
Imports DWSIM.Drawing.SkiaSharp.GraphicObjects.Shapes
Imports DWSIM.Drawing.SkiaSharp
Imports SkiaSharp
Imports DWSIM.Thermodynamics.BaseClasses
Imports DWSIM.SharedClassesCSharp.FilePicker
Imports DWSIM.Interfaces
Imports System.Device.Location
Imports System.Text.RegularExpressions

Public Class FlowsheetSurface_SkiaSharp

    Inherits DockContent

    Private m_connecting As Boolean = False

    Public Flowsheet As FormFlowsheet

    Public m_startobj, m_endobj As GraphicObject

    Public ticks As Integer

    Public SimObjPanel As SimulationObjectsPanel

    Public FlowsheetSurface As Drawing.SkiaSharp.GraphicsSurface

    Public FControl As FlowsheetSurfaceControl

    Public Loaded As Boolean = False

    Public AnimationTimer As New System.Timers.Timer(42) '24 fps

    Private Handlers As New List(Of EventHandler)
    Private TSMenuItems As New List(Of ToolStripMenuItem)

    Public Sub New()

        ' This call is required by the designer.
        InitializeComponent()

        ToolStripContainer1.TopToolStripPanel.SuspendLayout()
        ToolStripFlowsheet.Dock = DockStyle.None
        ToolStrip1.Dock = DockStyle.None
        ToolStripFlowsheet.Location = New Point(0, 0)
        ToolStrip1.Location = New Point(0, ToolStrip1.Height)
        ToolStripContainer1.TopToolStripPanel.ResumeLayout()

        FControl = New FlowsheetSurfaceControl
        FControl.Dock = DockStyle.Fill
        FControl.FlowsheetObject = Flowsheet
        FlowsheetSurface = FControl.FlowsheetSurface
        FormMain.AnalyticsProvider?.RegisterEvent("Flowsheet Renderer", "Software", Nothing)

        FlowsheetSurface.Zoom = Settings.DpiScale * 2.0

    End Sub

    Public Sub HandleKeyDown(e As KeyEventArgs)

        FControl.FlowsheetDesignSurface_KeyDown(Me, e)

    End Sub

    Private Sub frmSurface_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load

        Loaded = False

        ExtensionMethods.ChangeDefaultFont(Me)

        If Settings.DpiScale > 1.0 Then
            Me.ToolStrip1.AutoSize = False
            Me.ToolStrip1.Size = New Size(ToolStrip1.Width + (tsbColorTheme.Width) * (Settings.DpiScale - 1) + 60, 28 * Settings.DpiScale)
            tsbColorTheme.Size = New Size(Me.tsbColorTheme.Width * Settings.DpiScale, tsbColorTheme.Height)
            Me.ToolStrip1.ImageScalingSize = New Size(20 * Settings.DpiScale, 20 * Settings.DpiScale)
            For Each item In Me.ToolStrip1.Items
                If TryCast(item, ToolStripButton) IsNot Nothing Then
                    DirectCast(item, ToolStripButton).Size = New Size(ToolStrip1.ImageScalingSize.Width, ToolStrip1.ImageScalingSize.Height)
                End If
            Next
            Me.ToolStrip1.AutoSize = True
            Me.ToolStrip1.Invalidate()

            Me.ToolStripFlowsheet.AutoSize = False
            Me.tstbSearch.Size = New Size(Me.tstbSearch.Width * Settings.DpiScale, tstbSearch.Height)
            Me.tbFontSize.Size = New Size(Me.tbFontSize.Width * Settings.DpiScale, tbFontSize.Height)
            Me.TSTBZoom.Size = New Size(Me.TSTBZoom.Width * Settings.DpiScale, TSTBZoom.Height)
            Me.ToolStripFlowsheet.Size = New Size(ToolStripFlowsheet.Width + (tstbSearch.Width) * (Settings.DpiScale - 1) + 60, 28 * 2 * Settings.DpiScale)
            Me.ToolStripFlowsheet.ImageScalingSize = New Size(20 * Settings.DpiScale, 20 * Settings.DpiScale)
            For Each item In Me.ToolStripFlowsheet.Items
                If TryCast(item, ToolStripButton) IsNot Nothing Then
                    DirectCast(item, ToolStripButton).Size = New Size(ToolStrip1.ImageScalingSize.Width, ToolStrip1.ImageScalingSize.Height)
                End If
            Next
            Me.ToolStripFlowsheet.AutoSize = True
            Me.ToolStripFlowsheet.Invalidate()
        End If

        If TypeOf Me.ParentForm Is FormFlowsheet Then
            Flowsheet = Me.ParentForm
        ElseIf Flowsheet Is Nothing Then
            Flowsheet = My.Application.ActiveSimulation
        End If

        If TypeOf Me.ParentForm Is FormFlowsheet Then
            Flowsheet = Me.ParentForm
        ElseIf Flowsheet Is Nothing Then
            Flowsheet = My.Application.ActiveSimulation
        End If

        FlowsheetSurface.Flowsheet = Flowsheet

        DirectCast(FControl, FlowsheetSurfaceControl).FlowsheetObject = Flowsheet

        PanelFlowsheetControl.Controls.Add(FControl)

        SimObjPanel = New SimulationObjectsPanel() With {.Dock = DockStyle.Fill, .Flowsheet = Flowsheet}

        AddHandler SimObjPanel.Button1.Click,
            Sub(s2, e2)
                If SimObjPanel.Button1.Tag = "Expanded" Then
                    SimObjPanel.Button1.Tag = "Collapsed"
                    SimObjPanel.Button1.BackgroundImage = My.Resources.double_left_32px
                    SplitContainerVertical.SplitterDistance = SplitContainerVertical.Width - 40 * Settings.DpiScale
                    SimObjPanel.TableLayoutPanel1.Visible = False
                    SimObjPanel.Label1.Visible = False
                Else
                    SimObjPanel.Button1.Tag = "Expanded"
                    SimObjPanel.Button1.BackgroundImage = My.Resources.double_right_32px
                    SplitContainerVertical.SplitterDistance = SplitContainerVertical.Width - 420 * Settings.DpiScale
                    SimObjPanel.TableLayoutPanel1.Visible = True
                    SimObjPanel.Label1.Visible = True
                End If
            End Sub

        SplitContainerVertical.Panel2.Controls.Add(SimObjPanel)

        If GlobalSettings.Settings.DpiScale > 1.0 Then

            SplitContainerHorizontal.Panel2MinSize *= GlobalSettings.Settings.DpiScale

            SplitContainerVertical.Panel2MinSize *= GlobalSettings.Settings.DpiScale

        End If

        tsbColorTheme.SelectedIndex = Flowsheet.Options.FlowsheetColorTheme

        tbFontSize.Text = Flowsheet.Options.LabelFontSize.ToString("G2")

        FlowsheetSurface.SetRegularFont(Flowsheet.FlowsheetOptions.RegularFontName)
        FlowsheetSurface.SetBoldFont(Flowsheet.FlowsheetOptions.BoldFontName)
        FlowsheetSurface.SetItalicFont(Flowsheet.FlowsheetOptions.ItalicFontName)
        FlowsheetSurface.SetBoldItalicFont(Flowsheet.FlowsheetOptions.BoldItalicFontName)

        tscbAddObjectsWithStreams.SelectedIndex = Flowsheet.FlowsheetOptions.AddObjectsWithStreams

        AddHandler CopyFromTSMI.DropDownItemClicked, AddressOf MaterialStreamClickHandler

        'load context menu extenders
        For Each extender In My.Application.MainWindowForm.Extenders.Values
            Try
                If extender.Level = ExtenderLevel.FlowsheetWindow Then
                    For Each item In extender.Collection
                        If extender.Category = ExtenderCategory.FlowsheetSurfaceNotSelected Or extender.Category = ExtenderCategory.FlowsheetSurfaceSelected Then
                            Dim exttsmi As New ToolStripMenuItem
                            exttsmi.Text = item.DisplayText
                            exttsmi.Image = item.DisplayImage
                            Dim h1 = Sub(s2, e2)
                                         item.SetMainWindow(My.Application.MainWindowForm)
                                         item.SetFlowsheet(Flowsheet)
                                         item.Run()
                                     End Sub
                            Handlers.Add(h1)
                            AddHandler exttsmi.Click, h1
                            TSMenuItems.Add(exttsmi)
                            Select Case extender.Category
                                Case ExtenderCategory.FlowsheetSurfaceNotSelected
                                    If item.InsertAtPosition > 0 Then
                                        CMS_NoSel.Items.Insert(item.InsertAtPosition, exttsmi)
                                    Else
                                        CMS_NoSel.Items.Add(exttsmi)
                                    End If
                                Case ExtenderCategory.FlowsheetSurfaceSelected
                                    If item.InsertAtPosition > 0 Then
                                        CMS_Sel.Items.Insert(item.InsertAtPosition, exttsmi)
                                    Else
                                        CMS_Sel.Items.Add(exttsmi)
                                    End If
                            End Select
                        End If
                    Next
                End If
            Catch ex As Exception
            End Try
        Next

        If FormMain.IsPro Then
            FindTearStreamsAutomaticallyToolStripMenuItem.Visible = False
            tsmiHeatMap.Visible = False
            tsmiLiveFlow.Visible = False
            tss1.Visible = False
            tss2.Visible = False
            CopiarComoImagem200ToolStripMenuItem.Visible = False
            CopiarComoImagem300ToolStripMenuItem.Visible = False
            CopiarDadosParaareaDeTransferenciaToolStripMenuItem.Visible = False
            CopyAsImageToolStripMenuItem.Visible = False
        End If

        AddHandler AnimationTimer.Elapsed, Sub(s2, e2)
                                               If My.Settings.FlowsheetRenderer = 0 Then FControl.Invalidate()
                                           End Sub
        'AnimationTimer.Start()

        'weather

        ReadWeather(Flowsheet.Options.CurrentWeather)

        PanelWeather.Visible = My.Settings.WeatherPanelVisible

        Loaded = True

    End Sub

    Private Sub ReadWeather(cw As IWeatherData)

        Loaded = False

        tbAmbientTemperature.Text = cw.Temperature_C

        tbWindSpeed.Text = cw.WindSpeed_km_h

        tbHumidity.Text = cw.RelativeHumidity_pct

        tbSolarIrradiation.Text = cw.SolarIrradiation_kWh_m2.ToString("N2")

        tbAtmPress.Text = (cw.AtmosphericPressure_Pa / 100.0).ToString("N0")

        tbCurrentLocation.Text = cw.Latitude.ToString() + ", " + cw.Longitude.ToString()

        Loaded = True

    End Sub

    Public Function ReturnForm(ByVal str As String) As IDockContent

        If str = Me.ToString Then
            Return Me
        Else
            Return Nothing
        End If

    End Function

    Public Sub UpdateSelectedObject()

        UpdateSelectedObject_New()

    End Sub

    Public Sub UpdateSelectedObject_New()

        If Not FlowsheetSurface.SelectedObject Is Nothing Then

            If Flowsheet.SimulationObjects.ContainsKey(FlowsheetSurface.SelectedObject.Name) Then

                Dim obj = Flowsheet.SimulationObjects(FlowsheetSurface.SelectedObject.Name)

                obj.UpdateEditForm()
                EditorTooltips.Update(obj, Flowsheet)

            End If

        End If

    End Sub


    Private Sub CMS_Sel_Enter(ByVal sender As Object, ByVal e As System.EventArgs) Handles CMS_Sel.Opened

        Dim naoLimparListaDeDesconectar As Boolean = False

        Me.CMS_ItemsToDisconnect.Items.Clear()
        Me.CMS_ItemsToConnect.Items.Clear()
        Me.CopyFromTSMI.DropDownItems.Clear()

        Me.DesconectarDeToolStripMenuItem.Visible = False
        Me.ConectarAToolStripMenuItem.Visible = False
        Me.ToolStripSeparator4.Visible = False
        Me.CopyFromTSMI.Visible = False

        Me.ToolStripSeparator8.Visible = False
        Me.SplitToolStripMenuItem.Visible = False
        Me.MergeStreamsToolStripMenuItem.Visible = False
        Me.SplitAndInsertRecycleMenuItem.Visible = False

        Me.SplitAndInsertValveTSMI.Visible = False

        Me.AtivadoToolStripMenuItem.Checked = FlowsheetSurface.SelectedObject.Active

        If AtivadoToolStripMenuItem.Checked Then
            AtivadoToolStripMenuItem.Text = DWSIM.App.GetLocalString("ObjectIsActive")
        Else
            AtivadoToolStripMenuItem.Text = DWSIM.App.GetLocalString("ObjectIsInactive")
        End If

        DepurarObjetoToolStripMenuItem.Visible = Flowsheet.Collections.FlowsheetObjectCollection.ContainsKey(FlowsheetSurface.SelectedObject.Name)

        If FlowsheetSurface.SelectedObject.ObjectType <> ObjectType.GO_Image And
            FlowsheetSurface.SelectedObject.ObjectType <> ObjectType.GO_Table And
            FlowsheetSurface.SelectedObject.ObjectType <> ObjectType.GO_MasterTable And
            FlowsheetSurface.SelectedObject.ObjectType <> ObjectType.GO_SpreadsheetTable And
            FlowsheetSurface.SelectedObject.ObjectType <> ObjectType.GO_FloatingTable And
            FlowsheetSurface.SelectedObject.ObjectType <> ObjectType.DistillationColumn And
            FlowsheetSurface.SelectedObject.ObjectType <> ObjectType.AbsorptionColumn And
            FlowsheetSurface.SelectedObject.ObjectType <> ObjectType.ReboiledAbsorber And
            FlowsheetSurface.SelectedObject.ObjectType <> ObjectType.RefluxedAbsorber And
            FlowsheetSurface.SelectedObject.ObjectType <> ObjectType.GO_Rectangle And
            FlowsheetSurface.SelectedObject.ObjectType <> ObjectType.GO_Chart And
            FlowsheetSurface.SelectedObject.ObjectType <> ObjectType.GO_HTMLText And
            FlowsheetSurface.SelectedObject.ObjectType <> ObjectType.GO_Button And
            FlowsheetSurface.SelectedObject.ObjectType <> ObjectType.GO_Text Then

            Me.RecalcularToolStripMenuItem.Visible = True
            Me.ToolStripSeparator6.Visible = True
            Me.ClonarToolStripMenuItem.Visible = True
            Me.ExcluirToolStripMenuItem.Visible = True
            Me.HorizontalmenteToolStripMenuItem.Visible = True

            Try

                If FlowsheetSurface.SelectedObjects.Count = 2 AndAlso
                    FlowsheetSurface.SelectedObjects.Values.Where(Function(x) x.ObjectType = ObjectType.MaterialStream).Count = 2 Or
                    FlowsheetSurface.SelectedObjects.Values.Where(Function(x) x.ObjectType = ObjectType.EnergyStream).Count = 2 Then

                    Me.ToolStripSeparator8.Visible = True
                    Me.MergeStreamsToolStripMenuItem.Visible = True

                End If

                If FlowsheetSurface.SelectedObjects.Count = 1 AndAlso
                    FlowsheetSurface.SelectedObjects.Values.Where(Function(x) x.ObjectType = ObjectType.MaterialStream).Count = 1 Or
                    FlowsheetSurface.SelectedObjects.Values.Where(Function(x) x.ObjectType = ObjectType.EnergyStream).Count = 1 Then

                    Me.ToolStripSeparator8.Visible = True
                    Me.SplitToolStripMenuItem.Visible = True
                    Me.SplitAndInsertRecycleMenuItem.Visible = True

                End If

                Dim obj As SharedClasses.UnitOperations.BaseClass = Flowsheet.Collections.FlowsheetObjectCollection(FlowsheetSurface.SelectedObject.Name)

                If Me.IsObjectDownstreamConnectable(obj.GraphicObject.Tag) Then
                    Dim arr As ArrayList = Me.ReturnDownstreamConnectibles(obj.GraphicObject.Tag)
                    Me.CMS_ItemsToConnect.Items.Clear()
                    If arr.Count <> 0 Then
                        Dim i As Integer = 0
                        Do
                            Me.CMS_ItemsToConnect.Items.Add(arr(i))
                            i = i + 1
                            Me.ConectarAToolStripMenuItem.Visible = True
                            Me.ToolStripSeparator4.Visible = True
                            Me.ConectarAToolStripMenuItem.DropDown = Me.CMS_ItemsToConnect
                        Loop Until i = arr.Count
                    End If
                Else
                    Dim arr As ArrayList = Me.ReturnDownstreamDisconnectables(obj.GraphicObject.Tag)
                    Me.CMS_ItemsToDisconnect.Items.Clear()
                    If arr.Count <> 0 Then
                        naoLimparListaDeDesconectar = True
                        Dim i As Integer = 0
                        Do
                            Me.CMS_ItemsToDisconnect.Items.Add(arr(i))
                            i = i + 1
                        Loop Until i = arr.Count
                        Me.DesconectarDeToolStripMenuItem.Visible = True
                        Me.ToolStripSeparator4.Visible = True
                        Me.DesconectarDeToolStripMenuItem.DropDown = Me.CMS_ItemsToDisconnect
                    End If
                End If

                If Me.IsObjectUpstreamConnectable(obj.GraphicObject.Tag) = False Then
                    Dim arr As ArrayList = Me.ReturnUpstreamDisconnectables(obj.GraphicObject.Tag)
                    If naoLimparListaDeDesconectar = False Then Me.CMS_ItemsToDisconnect.Items.Clear()
                    If arr.Count <> 0 Then
                        Dim i As Integer = 0
                        Do
                            Me.CMS_ItemsToDisconnect.Items.Add(arr(i))
                            i = i + 1
                        Loop Until i = arr.Count
                        Me.DesconectarDeToolStripMenuItem.Visible = True
                        Me.ToolStripSeparator4.Visible = True
                        Me.DesconectarDeToolStripMenuItem.DropDown = Me.CMS_ItemsToDisconnect
                    End If
                End If

                If obj.GraphicObject.FlippedH Then
                    Me.HorizontalmenteToolStripMenuItem.Checked = True
                Else
                    Me.HorizontalmenteToolStripMenuItem.Checked = False
                End If

                If obj.GraphicObject.FlippedV Then
                    Me.tsmiInvertVertically.Checked = True
                Else
                    Me.tsmiInvertVertically.Checked = False
                End If

                If FlowsheetSurface.SelectedObject.ObjectType = ObjectType.MaterialStream Then

                    Me.SplitAndInsertValveTSMI.Visible = True

                    Dim cancopy As Boolean

                    If Not obj.GraphicObject.InputConnectors(0).IsAttached Then
                        cancopy = True
                    Else
                        If obj.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.ObjectType = ObjectType.OT_Recycle Then
                            cancopy = True
                        Else
                            cancopy = False
                        End If
                    End If

                    If cancopy Then
                        Me.CopyFromTSMI.Visible = True
                        For Each mstr As Thermodynamics.Streams.MaterialStream In Flowsheet.Collections.FlowsheetObjectCollection.Values.Where(Function(x) TypeOf x Is Thermodynamics.Streams.MaterialStream)
                            If mstr.GraphicObject.Tag <> obj.GraphicObject.Tag Then
                                Dim newtsmi As New ToolStripMenuItem(mstr.GraphicObject.Tag)
                                CopyFromTSMI.DropDownItems.Add(newtsmi)
                            End If
                        Next
                    End If

                ElseIf FlowsheetSurface.SelectedObject.ObjectType = ObjectType.EnergyStream Then

                    Dim cancopy As Boolean

                    If Not obj.GraphicObject.InputConnectors(0).IsAttached Then
                        cancopy = True
                    Else
                        If obj.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.ObjectType = ObjectType.OT_EnergyRecycle Then
                            cancopy = True
                        Else
                            cancopy = False
                        End If
                    End If

                    If cancopy Then
                        Me.CopyFromTSMI.Visible = True
                        For Each estr As UnitOperations.Streams.EnergyStream In Flowsheet.Collections.FlowsheetObjectCollection.Values.Where(Function(x) TypeOf x Is UnitOperations.Streams.EnergyStream)
                            If estr.GraphicObject.Tag <> obj.GraphicObject.Tag Then
                                Dim newtsmi As New ToolStripMenuItem(estr.GraphicObject.Tag)
                                CopyFromTSMI.DropDownItems.Add(newtsmi)
                            End If
                        Next
                    End If


                End If

            Catch ex As Exception
                CMS_Sel.Hide()
            End Try

        ElseIf FlowsheetSurface.SelectedObject.ObjectType = ObjectType.AbsorptionColumn Or
        FlowsheetSurface.SelectedObject.ObjectType = ObjectType.DistillationColumn Or
        FlowsheetSurface.SelectedObject.ObjectType = ObjectType.ReboiledAbsorber Or
        FlowsheetSurface.SelectedObject.ObjectType = ObjectType.RefluxedAbsorber Then

            Me.RecalcularToolStripMenuItem.Visible = True
            Me.ToolStripSeparator6.Visible = True

            Me.ConectarAToolStripMenuItem.Visible = False
            Me.DesconectarDeToolStripMenuItem.Visible = False
            Me.ClonarToolStripMenuItem.Visible = True
            Me.ExcluirToolStripMenuItem.Visible = True
            Me.HorizontalmenteToolStripMenuItem.Visible = True
            Dim obj As SharedClasses.UnitOperations.BaseClass = Flowsheet.Collections.FlowsheetObjectCollection(FlowsheetSurface.SelectedObject.Name)

            If obj.GraphicObject.FlippedH Then
                Me.HorizontalmenteToolStripMenuItem.Checked = True
            Else
                Me.HorizontalmenteToolStripMenuItem.Checked = False
            End If

        Else

            Me.TSMI_Label.Text = DWSIM.App.GetLocalString("Tabela")
            Me.ExcluirToolStripMenuItem.Visible = True
            Me.ClonarToolStripMenuItem.Visible = False
            Me.HorizontalmenteToolStripMenuItem.Visible = False
            Me.RecalcularToolStripMenuItem.Visible = False
            Me.ToolStripSeparator6.Visible = False

        End If

    End Sub

    Sub MaterialStreamClickHandler(ByVal sender As System.Object, ByVal e As ToolStripItemClickedEventArgs)

        Flowsheet.RegisterSnapshot(SnapshotType.ObjectAddedOrRemoved)

        Dim obj1 As Thermodynamics.Streams.MaterialStream = Flowsheet.Collections.FlowsheetObjectCollection(FlowsheetSurface.SelectedObject.Name)

        Dim obj2 As Thermodynamics.Streams.MaterialStream = Flowsheet.GetFlowsheetSimulationObject(e.ClickedItem.Text)

        obj1.Assign(obj2)

        CMS_Sel.Hide()

        Application.DoEvents()

        Flowsheet.UpdateOpenEditForms()

    End Sub

    Private Sub ToolStripMenuItem5_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)
        PreviewDialog.ShowDialog()
    End Sub

    Private Sub Timer1_Tick(ByVal sender As Object, ByVal e As System.EventArgs) Handles Timer1.Tick
        Me.ticks += 1
    End Sub

    Public Sub ClonarToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ClonarToolStripMenuItem.Click

        Flowsheet.RegisterSnapshot(SnapshotType.ObjectAddedOrRemoved)

        For Each obj In FlowsheetSurface.SelectedObjects.Values
            If TypeOf obj.Owner Is ISimulationObject Then
                Try
                    CloneObject(obj)
                Catch ex As Exception
                End Try
            End If
        Next

    End Sub

    Public Function CloneObject(gobj As GraphicObject) As GraphicObject

        If Flowsheet Is Nothing Then Flowsheet = My.Application.ActiveSimulation

        Try
            FormMain.AnalyticsProvider?.RegisterEvent("Cloning Object", gobj.Owner.GetDisplayName(), Nothing)
        Catch ex As Exception
        End Try

        Dim obj As SharedClasses.UnitOperations.BaseClass = Flowsheet.Collections.FlowsheetObjectCollection(gobj.Name)
        Dim newobj As SharedClasses.UnitOperations.BaseClass = obj.Clone

        newobj.GraphicObject = gobj.Clone
        newobj.GraphicObject.Owner = newobj

        Dim searchtext As String = gobj.Tag.Split("(")(0).Trim()

        Dim objcount As Integer = (From go As GraphicObject In FlowsheetSurface.DrawingObjects Select go Where go.Tag.Contains(searchtext)).Count

        Dim mpx = gobj.X + gobj.Width * 1.2
        Dim mpy = gobj.Y + gobj.Height * 1.2

        Select Case gobj.ObjectType

            Case ObjectType.External, ObjectType.AirCooler2, ObjectType.WindTurbine, ObjectType.HydroelectricTurbine,
                 ObjectType.WaterElectrolyzer, ObjectType.PEMFuelCell, ObjectType.RCT_GibbsReaktoro

                Dim myDWOBJ As Interfaces.IExternalUnitOperation = newobj
                With myDWOBJ.GraphicObject
                    .Calculated = False
                    .Name = myDWOBJ.Prefix & Guid.NewGuid.ToString
                    .Tag = searchtext & " (" & (objcount + 1).ToString & ")"
                    .X = mpx
                    .Y = mpy
                    For Each con As ConnectionPoint In .InputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    For Each con As ConnectionPoint In .OutputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    If Not .SpecialConnectors Is Nothing Then
                        For Each con As ConnectionPoint In .SpecialConnectors
                            con.AttachedConnector = Nothing
                            con.IsAttached = False
                        Next
                    End If
                    .EnergyConnector.AttachedConnector = Nothing
                    .EnergyConnector.IsAttached = False
                End With
                DirectCast(myDWOBJ, Interfaces.ISimulationObject).Name = myDWOBJ.GraphicObject.Name
                Flowsheet.Collections.GraphicObjectCollection.Add(myDWOBJ.GraphicObject.Name, myDWOBJ.GraphicObject)
                Flowsheet.Collections.FlowsheetObjectCollection.Add(DirectCast(myDWOBJ, Interfaces.ISimulationObject).Name, myDWOBJ)
                FlowsheetSurface.DrawingObjects.Add(myDWOBJ.GraphicObject)

            Case ObjectType.OT_Adjust

                Dim myDWOBJ As Adjust = CType(newobj, Adjust)
                With myDWOBJ.GraphicObject
                    .Calculated = False
                    .Name = "ADJ-" & Guid.NewGuid.ToString
                    .Tag = searchtext & " (" & (objcount + 1).ToString & ")"
                    .X = mpx
                    .Y = mpy
                    For Each con As ConnectionPoint In .InputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    For Each con As ConnectionPoint In .OutputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    If Not .SpecialConnectors Is Nothing Then
                        For Each con As ConnectionPoint In .SpecialConnectors
                            con.AttachedConnector = Nothing
                            con.IsAttached = False
                        Next
                    End If
                    .EnergyConnector.AttachedConnector = Nothing
                    .EnergyConnector.IsAttached = False
                End With
                myDWOBJ.Name = myDWOBJ.GraphicObject.Name
                Flowsheet.Collections.GraphicObjectCollection.Add(myDWOBJ.GraphicObject.Name, myDWOBJ.GraphicObject)
                Flowsheet.Collections.FlowsheetObjectCollection.Add(myDWOBJ.Name, myDWOBJ)
                FlowsheetSurface.DrawingObjects.Add(myDWOBJ.GraphicObject)
            Case ObjectType.OT_Spec
                Dim myDWOBJ As Spec = CType(newobj, Spec)
                With myDWOBJ.GraphicObject
                    .Calculated = False
                    .Name = "SPEC-" & Guid.NewGuid.ToString
                    .Tag = searchtext & " (" & (objcount + 1).ToString & ")"
                    .X = mpx
                    .Y = mpy
                    For Each con As ConnectionPoint In .InputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    For Each con As ConnectionPoint In .OutputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    If Not .SpecialConnectors Is Nothing Then
                        For Each con As ConnectionPoint In .SpecialConnectors
                            con.AttachedConnector = Nothing
                            con.IsAttached = False
                        Next
                    End If
                    .EnergyConnector.AttachedConnector = Nothing
                    .EnergyConnector.IsAttached = False
                End With
                myDWOBJ.Name = myDWOBJ.GraphicObject.Name
                Flowsheet.Collections.GraphicObjectCollection.Add(myDWOBJ.GraphicObject.Name, myDWOBJ.GraphicObject)
                Flowsheet.Collections.FlowsheetObjectCollection.Add(myDWOBJ.Name, myDWOBJ)
                FlowsheetSurface.DrawingObjects.Add(myDWOBJ.GraphicObject)
            Case ObjectType.OT_Recycle
                Dim myDWOBJ As Recycle = CType(newobj, Recycle)
                With myDWOBJ.GraphicObject
                    .Calculated = False
                    .Name = "REC-" & Guid.NewGuid.ToString
                    .Tag = searchtext & " (" & (objcount + 1).ToString & ")"
                    .X = mpx
                    .Y = mpy
                    For Each con As ConnectionPoint In .InputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    For Each con As ConnectionPoint In .OutputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    If Not .SpecialConnectors Is Nothing Then
                        For Each con As ConnectionPoint In .SpecialConnectors
                            con.AttachedConnector = Nothing
                            con.IsAttached = False
                        Next
                    End If
                    .EnergyConnector.AttachedConnector = Nothing
                    .EnergyConnector.IsAttached = False
                End With
                myDWOBJ.Name = myDWOBJ.GraphicObject.Name
                Flowsheet.Collections.GraphicObjectCollection.Add(myDWOBJ.GraphicObject.Name, myDWOBJ.GraphicObject)
                Flowsheet.Collections.FlowsheetObjectCollection.Add(myDWOBJ.Name, myDWOBJ)
                FlowsheetSurface.DrawingObjects.Add(myDWOBJ.GraphicObject)
            Case ObjectType.OT_EnergyRecycle
                Dim myDWOBJ As EnergyRecycle = CType(newobj, EnergyRecycle)
                With myDWOBJ.GraphicObject
                    .Calculated = False
                    .Name = "EREC-" & Guid.NewGuid.ToString
                    .Tag = searchtext & " (" & (objcount + 1).ToString & ")"
                    .X = mpx
                    .Y = mpy
                    For Each con As ConnectionPoint In .InputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    For Each con As ConnectionPoint In .OutputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    If Not .SpecialConnectors Is Nothing Then
                        For Each con As ConnectionPoint In .SpecialConnectors
                            con.AttachedConnector = Nothing
                            con.IsAttached = False
                        Next
                    End If
                    .EnergyConnector.AttachedConnector = Nothing
                    .EnergyConnector.IsAttached = False
                End With
                myDWOBJ.Name = myDWOBJ.GraphicObject.Name
                Flowsheet.Collections.GraphicObjectCollection.Add(myDWOBJ.GraphicObject.Name, myDWOBJ.GraphicObject)
                Flowsheet.Collections.FlowsheetObjectCollection.Add(myDWOBJ.Name, myDWOBJ)
                FlowsheetSurface.DrawingObjects.Add(myDWOBJ.GraphicObject)
            Case ObjectType.NodeIn
                Dim myDWOBJ As Mixer = CType(newobj, Mixer)
                With myDWOBJ.GraphicObject
                    .Calculated = False
                    .Name = "MIX-" & Guid.NewGuid.ToString
                    .Tag = searchtext & " (" & (objcount + 1).ToString & ")"
                    .X = mpx
                    .Y = mpy
                    For Each con As ConnectionPoint In .InputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    For Each con As ConnectionPoint In .OutputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    If Not .SpecialConnectors Is Nothing Then
                        For Each con As ConnectionPoint In .SpecialConnectors
                            con.AttachedConnector = Nothing
                            con.IsAttached = False
                        Next
                    End If
                    .EnergyConnector.AttachedConnector = Nothing
                    .EnergyConnector.IsAttached = False
                End With
                myDWOBJ.Name = myDWOBJ.GraphicObject.Name
                Flowsheet.Collections.GraphicObjectCollection.Add(myDWOBJ.GraphicObject.Name, myDWOBJ.GraphicObject)
                Flowsheet.Collections.FlowsheetObjectCollection.Add(myDWOBJ.Name, myDWOBJ)
                FlowsheetSurface.DrawingObjects.Add(myDWOBJ.GraphicObject)
            Case ObjectType.NodeOut
                Dim myDWOBJ As UnitOperations.UnitOperations.Splitter = CType(newobj, UnitOperations.UnitOperations.Splitter)
                With myDWOBJ.GraphicObject
                    .Calculated = False
                    .Name = "SPLT-" & Guid.NewGuid.ToString
                    .Tag = searchtext & " (" & (objcount + 1).ToString & ")"
                    .X = mpx
                    .Y = mpy
                    For Each con As ConnectionPoint In .InputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    For Each con As ConnectionPoint In .OutputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    If Not .SpecialConnectors Is Nothing Then
                        For Each con As ConnectionPoint In .SpecialConnectors
                            con.AttachedConnector = Nothing
                            con.IsAttached = False
                        Next
                    End If
                    .EnergyConnector.AttachedConnector = Nothing
                    .EnergyConnector.IsAttached = False
                End With
                myDWOBJ.Name = myDWOBJ.GraphicObject.Name
                Flowsheet.Collections.GraphicObjectCollection.Add(myDWOBJ.GraphicObject.Name, myDWOBJ.GraphicObject)
                Flowsheet.Collections.FlowsheetObjectCollection.Add(myDWOBJ.Name, myDWOBJ)
                FlowsheetSurface.DrawingObjects.Add(myDWOBJ.GraphicObject)
            Case ObjectType.Pump
                Dim myDWOBJ As Pump = CType(newobj, Pump)
                With myDWOBJ.GraphicObject
                    .Calculated = False
                    .Name = "PUMP-" & Guid.NewGuid.ToString
                    .Tag = searchtext & " (" & (objcount + 1).ToString & ")"
                    .X = mpx
                    .Y = mpy
                    For Each con As ConnectionPoint In .InputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    For Each con As ConnectionPoint In .OutputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    If Not .SpecialConnectors Is Nothing Then
                        For Each con As ConnectionPoint In .SpecialConnectors
                            con.AttachedConnector = Nothing
                            con.IsAttached = False
                        Next
                    End If
                    .EnergyConnector.AttachedConnector = Nothing
                    .EnergyConnector.IsAttached = False
                End With
                myDWOBJ.Name = myDWOBJ.GraphicObject.Name
                Flowsheet.Collections.GraphicObjectCollection.Add(myDWOBJ.GraphicObject.Name, myDWOBJ.GraphicObject)
                Flowsheet.Collections.FlowsheetObjectCollection.Add(myDWOBJ.Name, myDWOBJ)
                FlowsheetSurface.DrawingObjects.Add(myDWOBJ.GraphicObject)
            Case ObjectType.Tank
                Dim myDWOBJ As Tank = CType(newobj, Tank)
                With myDWOBJ.GraphicObject
                    .Calculated = False
                    .Name = "TANK-" & Guid.NewGuid.ToString
                    .Tag = searchtext & " (" & (objcount + 1).ToString & ")"
                    .X = mpx
                    .Y = mpy
                    For Each con As ConnectionPoint In .InputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    For Each con As ConnectionPoint In .OutputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    If Not .SpecialConnectors Is Nothing Then
                        For Each con As ConnectionPoint In .SpecialConnectors
                            con.AttachedConnector = Nothing
                            con.IsAttached = False
                        Next
                    End If
                    .EnergyConnector.AttachedConnector = Nothing
                    .EnergyConnector.IsAttached = False
                End With
                myDWOBJ.Name = myDWOBJ.GraphicObject.Name
                Flowsheet.Collections.GraphicObjectCollection.Add(myDWOBJ.GraphicObject.Name, myDWOBJ.GraphicObject)
                Flowsheet.Collections.FlowsheetObjectCollection.Add(myDWOBJ.Name, myDWOBJ)
                FlowsheetSurface.DrawingObjects.Add(myDWOBJ.GraphicObject)
            Case ObjectType.Vessel
                Dim myDWOBJ As Vessel = CType(newobj, Vessel)
                With myDWOBJ.GraphicObject
                    .Calculated = False
                    .Name = "SEP-" & Guid.NewGuid.ToString
                    .Tag = searchtext & " (" & (objcount + 1).ToString & ")"
                    .X = mpx
                    .Y = mpy
                    For Each con As ConnectionPoint In .InputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    For Each con As ConnectionPoint In .OutputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    If Not .SpecialConnectors Is Nothing Then
                        For Each con As ConnectionPoint In .SpecialConnectors
                            con.AttachedConnector = Nothing
                            con.IsAttached = False
                        Next
                    End If
                    .EnergyConnector.AttachedConnector = Nothing
                    .EnergyConnector.IsAttached = False
                End With
                myDWOBJ.Name = myDWOBJ.GraphicObject.Name
                Flowsheet.Collections.GraphicObjectCollection.Add(myDWOBJ.GraphicObject.Name, myDWOBJ.GraphicObject)
                Flowsheet.Collections.FlowsheetObjectCollection.Add(myDWOBJ.Name, myDWOBJ)
                FlowsheetSurface.DrawingObjects.Add(myDWOBJ.GraphicObject)
            Case ObjectType.MaterialStream
                Dim myDWOBJ As Thermodynamics.Streams.MaterialStream = CType(newobj, Thermodynamics.Streams.MaterialStream)
                With myDWOBJ.GraphicObject
                    .Calculated = False
                    .Name = "MSTR-" & Guid.NewGuid.ToString
                    .Tag = searchtext & " (" & (objcount + 1).ToString & ")"
                    .X = mpx
                    .Y = mpy
                    For Each con As ConnectionPoint In .InputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    For Each con As ConnectionPoint In .OutputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    If Not .SpecialConnectors Is Nothing Then
                        For Each con As ConnectionPoint In .SpecialConnectors
                            con.AttachedConnector = Nothing
                            con.IsAttached = False
                        Next
                    End If
                    .EnergyConnector.AttachedConnector = Nothing
                    .EnergyConnector.IsAttached = False
                End With
                myDWOBJ.Name = myDWOBJ.GraphicObject.Name
                Flowsheet.Collections.GraphicObjectCollection.Add(myDWOBJ.GraphicObject.Name, myDWOBJ.GraphicObject)
                Flowsheet.Collections.FlowsheetObjectCollection.Add(myDWOBJ.Name, myDWOBJ)
                FlowsheetSurface.DrawingObjects.Add(myDWOBJ.GraphicObject)
            Case ObjectType.EnergyStream
                Dim myDWOBJ As EnergyStream = CType(newobj, Streams.EnergyStream)
                With myDWOBJ.GraphicObject
                    .Calculated = False
                    .Name = "ESTR-" & Guid.NewGuid.ToString
                    .Tag = searchtext & " (" & (objcount + 1).ToString & ")"
                    .X = mpx
                    .Y = mpy
                    For Each con As ConnectionPoint In .InputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    For Each con As ConnectionPoint In .OutputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    If Not .SpecialConnectors Is Nothing Then
                        For Each con As ConnectionPoint In .SpecialConnectors
                            con.AttachedConnector = Nothing
                            con.IsAttached = False
                        Next
                    End If
                    .EnergyConnector.AttachedConnector = Nothing
                    .EnergyConnector.IsAttached = False
                End With
                myDWOBJ.Name = myDWOBJ.GraphicObject.Name
                Flowsheet.Collections.GraphicObjectCollection.Add(myDWOBJ.GraphicObject.Name, myDWOBJ.GraphicObject)
                Flowsheet.Collections.FlowsheetObjectCollection.Add(myDWOBJ.Name, myDWOBJ)
                FlowsheetSurface.DrawingObjects.Add(myDWOBJ.GraphicObject)
            Case ObjectType.Compressor
                Dim myDWOBJ As Compressor = CType(newobj, Compressor)
                With myDWOBJ.GraphicObject
                    .Calculated = False
                    .Name = "COMP-" & Guid.NewGuid.ToString
                    .Tag = searchtext & " (" & (objcount + 1).ToString & ")"
                    .X = mpx
                    .Y = mpy
                    For Each con As ConnectionPoint In .InputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    For Each con As ConnectionPoint In .OutputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    If Not .SpecialConnectors Is Nothing Then
                        For Each con As ConnectionPoint In .SpecialConnectors
                            con.AttachedConnector = Nothing
                            con.IsAttached = False
                        Next
                    End If
                    .EnergyConnector.AttachedConnector = Nothing
                    .EnergyConnector.IsAttached = False
                End With
                myDWOBJ.Name = myDWOBJ.GraphicObject.Name
                Flowsheet.Collections.GraphicObjectCollection.Add(myDWOBJ.GraphicObject.Name, myDWOBJ.GraphicObject)
                Flowsheet.Collections.FlowsheetObjectCollection.Add(myDWOBJ.Name, myDWOBJ)
                FlowsheetSurface.DrawingObjects.Add(myDWOBJ.GraphicObject)
            Case ObjectType.Expander
                Dim myDWOBJ As Expander = CType(newobj, Expander)
                With myDWOBJ.GraphicObject
                    .Calculated = False
                    .Name = "EXP-" & Guid.NewGuid.ToString
                    .Tag = searchtext & " (" & (objcount + 1).ToString & ")"
                    .X = mpx
                    .Y = mpy
                    For Each con As ConnectionPoint In .InputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    For Each con As ConnectionPoint In .OutputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    If Not .SpecialConnectors Is Nothing Then
                        For Each con As ConnectionPoint In .SpecialConnectors
                            con.AttachedConnector = Nothing
                            con.IsAttached = False
                        Next
                    End If
                    .EnergyConnector.AttachedConnector = Nothing
                    .EnergyConnector.IsAttached = False
                End With
                myDWOBJ.Name = myDWOBJ.GraphicObject.Name
                Flowsheet.Collections.GraphicObjectCollection.Add(myDWOBJ.GraphicObject.Name, myDWOBJ.GraphicObject)
                Flowsheet.Collections.FlowsheetObjectCollection.Add(myDWOBJ.Name, myDWOBJ)
                FlowsheetSurface.DrawingObjects.Add(myDWOBJ.GraphicObject)
            Case ObjectType.Cooler
                Dim myDWOBJ As Cooler = CType(newobj, Cooler)
                With myDWOBJ.GraphicObject
                    .Calculated = False
                    .Name = "COOL-" & Guid.NewGuid.ToString
                    .Tag = searchtext & " (" & (objcount + 1).ToString & ")"
                    .X = mpx
                    .Y = mpy
                    For Each con As ConnectionPoint In .InputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    For Each con As ConnectionPoint In .OutputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    If Not .SpecialConnectors Is Nothing Then
                        For Each con As ConnectionPoint In .SpecialConnectors
                            con.AttachedConnector = Nothing
                            con.IsAttached = False
                        Next
                    End If
                    .EnergyConnector.AttachedConnector = Nothing
                    .EnergyConnector.IsAttached = False
                End With
                myDWOBJ.Name = myDWOBJ.GraphicObject.Name
                Flowsheet.Collections.GraphicObjectCollection.Add(myDWOBJ.GraphicObject.Name, myDWOBJ.GraphicObject)
                Flowsheet.Collections.FlowsheetObjectCollection.Add(myDWOBJ.Name, myDWOBJ)
                FlowsheetSurface.DrawingObjects.Add(myDWOBJ.GraphicObject)
            Case ObjectType.Heater
                Dim myDWOBJ As Heater = CType(newobj, Heater)
                With myDWOBJ.GraphicObject
                    .Calculated = False
                    .Name = "HEAT-" & Guid.NewGuid.ToString
                    .Tag = searchtext & " (" & (objcount + 1).ToString & ")"
                    .X = mpx
                    .Y = mpy
                    For Each con As ConnectionPoint In .InputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    For Each con As ConnectionPoint In .OutputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    If Not .SpecialConnectors Is Nothing Then
                        For Each con As ConnectionPoint In .SpecialConnectors
                            con.AttachedConnector = Nothing
                            con.IsAttached = False
                        Next
                    End If
                    .EnergyConnector.AttachedConnector = Nothing
                    .EnergyConnector.IsAttached = False
                End With
                myDWOBJ.Name = myDWOBJ.GraphicObject.Name
                Flowsheet.Collections.GraphicObjectCollection.Add(myDWOBJ.GraphicObject.Name, myDWOBJ.GraphicObject)
                Flowsheet.Collections.FlowsheetObjectCollection.Add(myDWOBJ.Name, myDWOBJ)
                FlowsheetSurface.DrawingObjects.Add(myDWOBJ.GraphicObject)
            Case ObjectType.Pipe
                Dim myDWOBJ As Pipe = CType(newobj, Pipe)
                With myDWOBJ.GraphicObject
                    .Calculated = False
                    .Name = "PIPE-" & Guid.NewGuid.ToString
                    .Tag = searchtext & " (" & (objcount + 1).ToString & ")"
                    .X = mpx
                    .Y = mpy
                    For Each con As ConnectionPoint In .InputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    For Each con As ConnectionPoint In .OutputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    If Not .SpecialConnectors Is Nothing Then
                        For Each con As ConnectionPoint In .SpecialConnectors
                            con.AttachedConnector = Nothing
                            con.IsAttached = False
                        Next
                    End If
                    .EnergyConnector.AttachedConnector = Nothing
                    .EnergyConnector.IsAttached = False
                End With
                myDWOBJ.Name = myDWOBJ.GraphicObject.Name
                Flowsheet.Collections.GraphicObjectCollection.Add(myDWOBJ.GraphicObject.Name, myDWOBJ.GraphicObject)
                Flowsheet.Collections.FlowsheetObjectCollection.Add(myDWOBJ.Name, myDWOBJ)
                FlowsheetSurface.DrawingObjects.Add(myDWOBJ.GraphicObject)
            Case ObjectType.Valve
                Dim myDWOBJ As Valve = CType(newobj, Valve)
                With myDWOBJ.GraphicObject
                    .Calculated = False
                    .Name = "VALV-" & Guid.NewGuid.ToString
                    .Tag = searchtext & " (" & (objcount + 1).ToString & ")"
                    .X = mpx
                    .Y = mpy
                    For Each con As ConnectionPoint In .InputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    For Each con As ConnectionPoint In .OutputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    If Not .SpecialConnectors Is Nothing Then
                        For Each con As ConnectionPoint In .SpecialConnectors
                            con.AttachedConnector = Nothing
                            con.IsAttached = False
                        Next
                    End If
                    .EnergyConnector.AttachedConnector = Nothing
                    .EnergyConnector.IsAttached = False
                End With
                myDWOBJ.Name = myDWOBJ.GraphicObject.Name
                Flowsheet.Collections.GraphicObjectCollection.Add(myDWOBJ.GraphicObject.Name, myDWOBJ.GraphicObject)
                Flowsheet.Collections.FlowsheetObjectCollection.Add(myDWOBJ.Name, myDWOBJ)
                FlowsheetSurface.DrawingObjects.Add(myDWOBJ.GraphicObject)
            Case ObjectType.RCT_Conversion
                Dim myDWOBJ As Reactor_Conversion = CType(newobj, Reactor_Conversion)
                With myDWOBJ.GraphicObject
                    .Calculated = False
                    .Name = "RC-" & Guid.NewGuid.ToString
                    .Tag = searchtext & " (" & (objcount + 1).ToString & ")"
                    .X = mpx
                    .Y = mpy
                    For Each con As ConnectionPoint In .InputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    For Each con As ConnectionPoint In .OutputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    If Not .SpecialConnectors Is Nothing Then
                        For Each con As ConnectionPoint In .SpecialConnectors
                            con.AttachedConnector = Nothing
                            con.IsAttached = False
                        Next
                    End If
                    .EnergyConnector.AttachedConnector = Nothing
                    .EnergyConnector.IsAttached = False
                End With
                myDWOBJ.Name = myDWOBJ.GraphicObject.Name
                Flowsheet.Collections.GraphicObjectCollection.Add(myDWOBJ.GraphicObject.Name, myDWOBJ.GraphicObject)
                Flowsheet.Collections.FlowsheetObjectCollection.Add(myDWOBJ.Name, myDWOBJ)
                FlowsheetSurface.DrawingObjects.Add(myDWOBJ.GraphicObject)
            Case ObjectType.RCT_Equilibrium
                Dim myDWOBJ As Reactor_Equilibrium = CType(newobj, Reactor_Equilibrium)
                With myDWOBJ.GraphicObject
                    .Calculated = False
                    .Name = "RE-" & Guid.NewGuid.ToString
                    .Tag = searchtext & " (" & (objcount + 1).ToString & ")"
                    .X = mpx
                    .Y = mpy
                    For Each con As ConnectionPoint In .InputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    For Each con As ConnectionPoint In .OutputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    If Not .SpecialConnectors Is Nothing Then
                        For Each con As ConnectionPoint In .SpecialConnectors
                            con.AttachedConnector = Nothing
                            con.IsAttached = False
                        Next
                    End If
                    .EnergyConnector.AttachedConnector = Nothing
                    .EnergyConnector.IsAttached = False
                End With
                myDWOBJ.Name = myDWOBJ.GraphicObject.Name
                Flowsheet.Collections.GraphicObjectCollection.Add(myDWOBJ.GraphicObject.Name, myDWOBJ.GraphicObject)
                Flowsheet.Collections.FlowsheetObjectCollection.Add(myDWOBJ.Name, myDWOBJ)
                FlowsheetSurface.DrawingObjects.Add(myDWOBJ.GraphicObject)
            Case ObjectType.RCT_Gibbs
                Dim myDWOBJ As Reactor_Gibbs = CType(newobj, Reactor_Gibbs)
                With myDWOBJ.GraphicObject
                    .Calculated = False
                    .Name = "RG-" & Guid.NewGuid.ToString
                    .Tag = searchtext & " (" & (objcount + 1).ToString & ")"
                    .X = mpx
                    .Y = mpy
                    For Each con As ConnectionPoint In .InputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    For Each con As ConnectionPoint In .OutputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    If Not .SpecialConnectors Is Nothing Then
                        For Each con As ConnectionPoint In .SpecialConnectors
                            con.AttachedConnector = Nothing
                            con.IsAttached = False
                        Next
                    End If
                    .EnergyConnector.AttachedConnector = Nothing
                    .EnergyConnector.IsAttached = False
                End With
                myDWOBJ.Name = myDWOBJ.GraphicObject.Name
                Flowsheet.Collections.GraphicObjectCollection.Add(myDWOBJ.GraphicObject.Name, myDWOBJ.GraphicObject)
                Flowsheet.Collections.FlowsheetObjectCollection.Add(myDWOBJ.Name, myDWOBJ)
                FlowsheetSurface.DrawingObjects.Add(myDWOBJ.GraphicObject)
            Case ObjectType.RCT_CSTR
                Dim myDWOBJ As Reactor_CSTR = CType(newobj, Reactor_CSTR)
                With myDWOBJ.GraphicObject
                    .Calculated = False
                    .Name = "CSTR-" & Guid.NewGuid.ToString
                    .Tag = searchtext & " (" & (objcount + 1).ToString & ")"
                    .X = mpx
                    .Y = mpy
                    For Each con As ConnectionPoint In .InputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    For Each con As ConnectionPoint In .OutputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    If Not .SpecialConnectors Is Nothing Then
                        For Each con As ConnectionPoint In .SpecialConnectors
                            con.AttachedConnector = Nothing
                            con.IsAttached = False
                        Next
                    End If
                    .EnergyConnector.AttachedConnector = Nothing
                    .EnergyConnector.IsAttached = False
                End With
                myDWOBJ.Name = myDWOBJ.GraphicObject.Name
                Flowsheet.Collections.GraphicObjectCollection.Add(myDWOBJ.GraphicObject.Name, myDWOBJ.GraphicObject)
                Flowsheet.Collections.FlowsheetObjectCollection.Add(myDWOBJ.Name, myDWOBJ)
                FlowsheetSurface.DrawingObjects.Add(myDWOBJ.GraphicObject)
            Case ObjectType.RCT_PFR
                Dim myDWOBJ As Reactor_PFR = CType(newobj, Reactor_PFR)
                With myDWOBJ.GraphicObject
                    .Calculated = False
                    .Name = "PFR-" & Guid.NewGuid.ToString
                    .Tag = searchtext & " (" & (objcount + 1).ToString & ")"
                    .X = mpx
                    .Y = mpy
                    For Each con As ConnectionPoint In .InputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    For Each con As ConnectionPoint In .OutputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    If Not .SpecialConnectors Is Nothing Then
                        For Each con As ConnectionPoint In .SpecialConnectors
                            con.AttachedConnector = Nothing
                            con.IsAttached = False
                        Next
                    End If
                    .EnergyConnector.AttachedConnector = Nothing
                    .EnergyConnector.IsAttached = False
                End With
                myDWOBJ.Name = myDWOBJ.GraphicObject.Name
                Flowsheet.Collections.GraphicObjectCollection.Add(myDWOBJ.GraphicObject.Name, myDWOBJ.GraphicObject)
                Flowsheet.Collections.FlowsheetObjectCollection.Add(myDWOBJ.Name, myDWOBJ)
                FlowsheetSurface.DrawingObjects.Add(myDWOBJ.GraphicObject)
            Case ObjectType.HeatExchanger
                Dim myDWOBJ As HeatExchanger = CType(newobj, HeatExchanger)
                With myDWOBJ.GraphicObject
                    .Calculated = False
                    .Name = "HE-" & Guid.NewGuid.ToString
                    .Tag = searchtext & " (" & (objcount + 1).ToString & ")"
                    .X = mpx
                    .Y = mpy
                    For Each con As ConnectionPoint In .InputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    For Each con As ConnectionPoint In .OutputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    If Not .SpecialConnectors Is Nothing Then
                        For Each con As ConnectionPoint In .SpecialConnectors
                            con.AttachedConnector = Nothing
                            con.IsAttached = False
                        Next
                    End If
                    .EnergyConnector.AttachedConnector = Nothing
                    .EnergyConnector.IsAttached = False
                End With
                myDWOBJ.Name = myDWOBJ.GraphicObject.Name
                Flowsheet.Collections.GraphicObjectCollection.Add(myDWOBJ.GraphicObject.Name, myDWOBJ.GraphicObject)
                Flowsheet.Collections.FlowsheetObjectCollection.Add(myDWOBJ.Name, myDWOBJ)
                FlowsheetSurface.DrawingObjects.Add(myDWOBJ.GraphicObject)
            Case ObjectType.ShortcutColumn
                Dim myDWOBJ As ShortcutColumn = CType(newobj, ShortcutColumn)
                With myDWOBJ.GraphicObject
                    .Calculated = False
                    .Name = "SC-" & Guid.NewGuid.ToString
                    .Tag = searchtext & " (" & (objcount + 1).ToString & ")"
                    .X = mpx
                    .Y = mpy
                    For Each con As ConnectionPoint In .InputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    For Each con As ConnectionPoint In .OutputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    If Not .SpecialConnectors Is Nothing Then
                        For Each con As ConnectionPoint In .SpecialConnectors
                            con.AttachedConnector = Nothing
                            con.IsAttached = False
                        Next
                    End If
                    .EnergyConnector.AttachedConnector = Nothing
                    .EnergyConnector.IsAttached = False
                End With
                myDWOBJ.Name = myDWOBJ.GraphicObject.Name
                Flowsheet.Collections.GraphicObjectCollection.Add(myDWOBJ.GraphicObject.Name, myDWOBJ.GraphicObject)
                Flowsheet.Collections.FlowsheetObjectCollection.Add(myDWOBJ.Name, myDWOBJ)
                FlowsheetSurface.DrawingObjects.Add(myDWOBJ.GraphicObject)
            Case ObjectType.DistillationColumn
                Dim myDWOBJ As DistillationColumn = CType(newobj, DistillationColumn)
                With myDWOBJ.GraphicObject
                    .Calculated = False
                    .Name = "DC-" & Guid.NewGuid.ToString
                    .Tag = searchtext & " (" & (objcount + 1).ToString & ")"
                    .X = mpx
                    .Y = mpy
                    For Each con As ConnectionPoint In .InputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    For Each con As ConnectionPoint In .OutputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    If Not .SpecialConnectors Is Nothing Then
                        For Each con As ConnectionPoint In .SpecialConnectors
                            con.AttachedConnector = Nothing
                            con.IsAttached = False
                        Next
                    End If
                    .EnergyConnector.AttachedConnector = Nothing
                    .EnergyConnector.IsAttached = False
                End With
                myDWOBJ.Name = myDWOBJ.GraphicObject.Name
                Flowsheet.Collections.GraphicObjectCollection.Add(myDWOBJ.GraphicObject.Name, myDWOBJ.GraphicObject)
                Flowsheet.Collections.FlowsheetObjectCollection.Add(myDWOBJ.Name, myDWOBJ)
                FlowsheetSurface.DrawingObjects.Add(myDWOBJ.GraphicObject)
            Case ObjectType.AbsorptionColumn
                Dim myDWOBJ As AbsorptionColumn = CType(newobj, AbsorptionColumn)
                With myDWOBJ.GraphicObject
                    .Calculated = False
                    .Name = "ABS-" & Guid.NewGuid.ToString
                    .Tag = searchtext & " (" & (objcount + 1).ToString & ")"
                    .X = mpx
                    .Y = mpy
                    For Each con As ConnectionPoint In .InputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    For Each con As ConnectionPoint In .OutputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    If Not .SpecialConnectors Is Nothing Then
                        For Each con As ConnectionPoint In .SpecialConnectors
                            con.AttachedConnector = Nothing
                            con.IsAttached = False
                        Next
                    End If
                    .EnergyConnector.AttachedConnector = Nothing
                    .EnergyConnector.IsAttached = False
                End With
                myDWOBJ.Name = myDWOBJ.GraphicObject.Name
                Flowsheet.Collections.GraphicObjectCollection.Add(myDWOBJ.GraphicObject.Name, myDWOBJ.GraphicObject)
                Flowsheet.Collections.FlowsheetObjectCollection.Add(myDWOBJ.Name, myDWOBJ)
                FlowsheetSurface.DrawingObjects.Add(myDWOBJ.GraphicObject)
            Case ObjectType.ComponentSeparator
                Dim myDWOBJ As ComponentSeparator = CType(newobj, ComponentSeparator)
                With myDWOBJ.GraphicObject
                    .Calculated = False
                    .Name = "CS-" & Guid.NewGuid.ToString
                    .Tag = searchtext & " (" & (objcount + 1).ToString & ")"
                    .X = mpx
                    .Y = mpy
                    For Each con As ConnectionPoint In .InputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    For Each con As ConnectionPoint In .OutputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    If Not .SpecialConnectors Is Nothing Then
                        For Each con As ConnectionPoint In .SpecialConnectors
                            con.AttachedConnector = Nothing
                            con.IsAttached = False
                        Next
                    End If
                    .EnergyConnector.AttachedConnector = Nothing
                    .EnergyConnector.IsAttached = False
                End With
                myDWOBJ.Name = myDWOBJ.GraphicObject.Name
                Flowsheet.Collections.GraphicObjectCollection.Add(myDWOBJ.GraphicObject.Name, myDWOBJ.GraphicObject)
                Flowsheet.Collections.FlowsheetObjectCollection.Add(myDWOBJ.Name, myDWOBJ)
                FlowsheetSurface.DrawingObjects.Add(myDWOBJ.GraphicObject)
            Case ObjectType.SolidSeparator
                Dim myDWOBJ As SolidsSeparator = CType(newobj, SolidsSeparator)
                With myDWOBJ.GraphicObject
                    .Calculated = False
                    .Name = "SS-" & Guid.NewGuid.ToString
                    .Tag = searchtext & " (" & (objcount + 1).ToString & ")"
                    .X = mpx
                    .Y = mpy
                    For Each con As ConnectionPoint In .InputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    For Each con As ConnectionPoint In .OutputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    If Not .SpecialConnectors Is Nothing Then
                        For Each con As ConnectionPoint In .SpecialConnectors
                            con.AttachedConnector = Nothing
                            con.IsAttached = False
                        Next
                    End If
                    .EnergyConnector.AttachedConnector = Nothing
                    .EnergyConnector.IsAttached = False
                End With
                myDWOBJ.Name = myDWOBJ.GraphicObject.Name
                Flowsheet.Collections.GraphicObjectCollection.Add(myDWOBJ.GraphicObject.Name, myDWOBJ.GraphicObject)
                Flowsheet.Collections.FlowsheetObjectCollection.Add(myDWOBJ.Name, myDWOBJ)
                FlowsheetSurface.DrawingObjects.Add(myDWOBJ.GraphicObject)
            Case ObjectType.Filter
                Dim myDWOBJ As Filter = CType(newobj, Filter)
                With myDWOBJ.GraphicObject
                    .Calculated = False
                    .Name = "FT-" & Guid.NewGuid.ToString
                    .Tag = searchtext & " (" & (objcount + 1).ToString & ")"
                    .X = mpx
                    .Y = mpy
                    For Each con As ConnectionPoint In .InputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    For Each con As ConnectionPoint In .OutputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    If Not .SpecialConnectors Is Nothing Then
                        For Each con As ConnectionPoint In .SpecialConnectors
                            con.AttachedConnector = Nothing
                            con.IsAttached = False
                        Next
                    End If
                    .EnergyConnector.AttachedConnector = Nothing
                    .EnergyConnector.IsAttached = False
                End With
                myDWOBJ.Name = myDWOBJ.GraphicObject.Name
                Flowsheet.Collections.GraphicObjectCollection.Add(myDWOBJ.GraphicObject.Name, myDWOBJ.GraphicObject)
                Flowsheet.Collections.FlowsheetObjectCollection.Add(myDWOBJ.Name, myDWOBJ)
                FlowsheetSurface.DrawingObjects.Add(myDWOBJ.GraphicObject)
            Case ObjectType.OrificePlate
                Dim myDWOBJ As OrificePlate = CType(newobj, OrificePlate)
                With myDWOBJ.GraphicObject
                    .Calculated = False
                    .Name = "OP-" & Guid.NewGuid.ToString
                    .Tag = searchtext & " (" & (objcount + 1).ToString & ")"
                    .X = mpx
                    .Y = mpy
                    For Each con As ConnectionPoint In .InputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    For Each con As ConnectionPoint In .OutputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    If Not .SpecialConnectors Is Nothing Then
                        For Each con As ConnectionPoint In .SpecialConnectors
                            con.AttachedConnector = Nothing
                            con.IsAttached = False
                        Next
                    End If
                    .EnergyConnector.AttachedConnector = Nothing
                    .EnergyConnector.IsAttached = False
                End With
                myDWOBJ.Name = myDWOBJ.GraphicObject.Name
                Flowsheet.Collections.GraphicObjectCollection.Add(myDWOBJ.GraphicObject.Name, myDWOBJ.GraphicObject)
                Flowsheet.Collections.FlowsheetObjectCollection.Add(myDWOBJ.Name, myDWOBJ)
                FlowsheetSurface.DrawingObjects.Add(myDWOBJ.GraphicObject)
            Case ObjectType.CustomUO
                Dim myDWOBJ As CustomUO = CType(newobj, CustomUO)
                With myDWOBJ.GraphicObject
                    .Calculated = False
                    .Name = "UO-" & Guid.NewGuid.ToString
                    .Tag = searchtext & " (" & (objcount + 1).ToString & ")"
                    .X = mpx
                    .Y = mpy
                    For Each con As ConnectionPoint In .InputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    For Each con As ConnectionPoint In .OutputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    If Not .SpecialConnectors Is Nothing Then
                        For Each con As ConnectionPoint In .SpecialConnectors
                            con.AttachedConnector = Nothing
                            con.IsAttached = False
                        Next
                    End If
                    .EnergyConnector.AttachedConnector = Nothing
                    .EnergyConnector.IsAttached = False
                End With
                myDWOBJ.Name = myDWOBJ.GraphicObject.Name
                Flowsheet.Collections.GraphicObjectCollection.Add(myDWOBJ.GraphicObject.Name, myDWOBJ.GraphicObject)
                Flowsheet.Collections.FlowsheetObjectCollection.Add(myDWOBJ.Name, myDWOBJ)
                FlowsheetSurface.DrawingObjects.Add(myDWOBJ.GraphicObject)
            Case ObjectType.CapeOpenUO, ObjectType.FlowsheetUO
                MessageBox.Show("Cloning is not supported by CAPE-OPEN/Flowsheet Unit Operations.", "Error", MessageBoxButtons.OK, MessageBoxIcon.Error)
            Case Else
                Dim myUO = newobj.Clone()
                With myUO.GraphicObject
                    .Calculated = False
                    .Name = Guid.NewGuid.ToString
                    .Tag = searchtext & " (" & (objcount + 1).ToString & ")"
                    .X = mpx
                    .Y = mpy
                    For Each con As ConnectionPoint In .InputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    For Each con As ConnectionPoint In .OutputConnectors
                        con.AttachedConnector = Nothing
                        con.IsAttached = False
                    Next
                    If Not .SpecialConnectors Is Nothing Then
                        For Each con As ConnectionPoint In .SpecialConnectors
                            con.AttachedConnector = Nothing
                            con.IsAttached = False
                        Next
                    End If
                    .EnergyConnector.AttachedConnector = Nothing
                    .EnergyConnector.IsAttached = False
                End With
                myUO.Name = myUO.GraphicObject.Name
                Flowsheet.Collections.GraphicObjectCollection.Add(myUO.GraphicObject.Name, myUO.GraphicObject)
                Flowsheet.Collections.FlowsheetObjectCollection.Add(myUO.Name, myUO)
                FlowsheetSurface.DrawingObjects.Add(myUO.GraphicObject)
        End Select

        SplitContainerHorizontal.Panel1.Invalidate()

        newobj.SetFlowsheet(Flowsheet)

        newobj.GraphicObject.Flowsheet = Flowsheet

        Return newobj.GraphicObject

    End Function

    Private Sub CMS_ItemsToDisconnect_ItemClicked(ByVal sender As System.Object, ByVal e As System.Windows.Forms.ToolStripItemClickedEventArgs) Handles CMS_ItemsToDisconnect.ItemClicked

        Try
            Me.Flowsheet.DisconnectObject(FlowsheetSurface.SelectedObject, FormFlowsheet.SearchSurfaceObjectsByTag(e.ClickedItem.Text, FlowsheetSurface), True)
        Catch ex As Exception
            Flowsheet.ShowMessage("Error: " + ex.Message, IFlowsheet.MessageType.GeneralError)
        Finally
            Flowsheet.UpdateOpenEditForms()
        End Try

    End Sub

    Private Sub CMS_ItemsToConnect_ItemClicked(ByVal sender As System.Object, ByVal e As System.Windows.Forms.ToolStripItemClickedEventArgs) Handles CMS_ItemsToConnect.ItemClicked

        Try
            Flowsheet.ConnectObject(FlowsheetSurface.SelectedObject, FormFlowsheet.SearchSurfaceObjectsByTag(e.ClickedItem.Text, FlowsheetSurface))
        Catch ex As Exception
            Flowsheet.ShowMessage("Error: " + ex.Message, IFlowsheet.MessageType.GeneralError)
        Finally
            Flowsheet.UpdateOpenEditForms()
        End Try

    End Sub

    Function IsObjectDownstreamConnectable(ByVal objTag As String) As Boolean

        Dim obj As GraphicObject = FormFlowsheet.SearchSurfaceObjectsByTag(objTag, FlowsheetSurface)

        If Not obj Is Nothing Then

            Dim cp As ConnectionPoint
            For Each cp In obj.OutputConnectors
                If cp.IsAttached = False Then Return True
            Next

        End If

        Return False

    End Function

    Function IsObjectUpstreamConnectable(ByVal objTag As String) As Boolean

        Dim obj As GraphicObject = FormFlowsheet.SearchSurfaceObjectsByTag(objTag, FlowsheetSurface)

        If Not obj Is Nothing Then

            Dim cp As ConnectionPoint
            For Each cp In obj.InputConnectors
                If cp.IsAttached = False Then Return True
            Next

        End If

        Return False

    End Function

    Function ReturnDownstreamConnectibles(ByVal objtag As String)

        Dim refobj As GraphicObject = FormFlowsheet.SearchSurfaceObjectsByTag(objtag, FlowsheetSurface)

        Dim obj As SharedClasses.UnitOperations.BaseClass
        Dim cp As ConnectionPoint

        Dim conables As New ArrayList

        For Each obj In Me.Flowsheet.Collections.FlowsheetObjectCollection.Values.OrderBy(Function(o) o.GraphicObject.Tag)
            If obj.GraphicObject.Tag <> refobj.Tag Then
                If obj.GraphicObject.ObjectType <> ObjectType.GO_Text And
                    obj.GraphicObject.ObjectType <> ObjectType.GO_HTMLText And
                    obj.GraphicObject.ObjectType <> ObjectType.GO_Button And
                    obj.GraphicObject.ObjectType <> ObjectType.GO_FloatingTable And
                    obj.GraphicObject.ObjectType <> ObjectType.GO_MasterTable And
                    obj.GraphicObject.ObjectType <> ObjectType.GO_SpreadsheetTable And
                    obj.GraphicObject.ObjectType <> ObjectType.GO_Table And
                    obj.GraphicObject.ObjectType <> ObjectType.GO_Chart And
                    obj.GraphicObject.ObjectType <> ObjectType.GO_Rectangle And
                    obj.GraphicObject.ObjectType <> ObjectType.OT_Adjust And
                    obj.GraphicObject.ObjectType <> ObjectType.OT_Spec And
                    obj.GraphicObject.ObjectType <> ObjectType.DistillationColumn And
                    obj.GraphicObject.ObjectType <> ObjectType.AbsorptionColumn And
                    obj.GraphicObject.ObjectType <> ObjectType.RefluxedAbsorber And
                    obj.GraphicObject.ObjectType <> ObjectType.ReboiledAbsorber And
                    obj.GraphicObject.ObjectType <> ObjectType.Nenhum Then

                    If refobj.ObjectType = ObjectType.MaterialStream Then
                        For Each cp In obj.GraphicObject.InputConnectors
                            If Not cp.IsAttached And Not conables.Contains(obj.GraphicObject.Tag) And Not _
                            obj.GraphicObject.ObjectType = ObjectType.MaterialStream And Not _
                            obj.GraphicObject.ObjectType = ObjectType.EnergyStream And
                            cp.Type = ConType.ConIn Then conables.Add(obj.GraphicObject.Tag)
                        Next
                    ElseIf refobj.ObjectType = ObjectType.EnergyStream Then
                        If obj.GraphicObject.ObjectType <> ObjectType.Heater And
                        obj.GraphicObject.ObjectType <> ObjectType.Pump And
                        obj.GraphicObject.ObjectType <> ObjectType.Compressor And
                        obj.GraphicObject.ObjectType <> ObjectType.MaterialStream Then
                            cp = obj.GraphicObject.EnergyConnector
                            If Not cp.IsAttached And Not conables.Contains(obj.GraphicObject.Tag) Then conables.Add(obj.GraphicObject.Tag)
                        ElseIf obj.GraphicObject.ObjectType = ObjectType.MaterialStream Then

                        Else
                            For Each cp In obj.GraphicObject.InputConnectors
                                If Not cp.IsAttached And Not conables.Contains(obj.GraphicObject.Tag) And Not _
                                obj.GraphicObject.ObjectType = ObjectType.MaterialStream And Not _
                                obj.GraphicObject.ObjectType = ObjectType.EnergyStream And
                                cp.Type = ConType.ConEn Then conables.Add(obj.GraphicObject.Tag)
                            Next
                        End If
                    ElseIf TypeOf refobj.Owner Is IUnitOperation Then
                        If obj.GraphicObject.ObjectType = ObjectType.MaterialStream Then
                            cp = obj.GraphicObject.InputConnectors(0)
                            If Not cp.IsAttached And Not conables.Contains(obj.GraphicObject.Tag) Then conables.Add(obj.GraphicObject.Tag)
                        ElseIf obj.GraphicObject.ObjectType = ObjectType.EnergyStream Then
                            cp = obj.GraphicObject.InputConnectors(0)
                            If Not cp.IsAttached And Not refobj.EnergyConnector.IsAttached And Not conables.Contains(obj.GraphicObject.Tag) Then conables.Add(obj.GraphicObject.Tag)
                        End If
                    Else
                        For Each cp In obj.GraphicObject.InputConnectors
                            If Not cp.IsAttached And Not conables.Contains(obj.GraphicObject.Tag) Then conables.Add(obj.GraphicObject.Tag)
                        Next
                        If obj.GraphicObject.ObjectType = ObjectType.MaterialStream Then
                            cp = obj.GraphicObject.InputConnectors(0)
                            If Not cp.IsAttached And Not conables.Contains(obj.GraphicObject.Tag) Then conables.Add(obj.GraphicObject.Tag)
                        ElseIf obj.GraphicObject.ObjectType = ObjectType.EnergyStream Then
                            cp = obj.GraphicObject.InputConnectors(0)
                            If Not cp.IsAttached And Not refobj.EnergyConnector.IsAttached And Not conables.Contains(obj.GraphicObject.Tag) Then conables.Add(obj.GraphicObject.Tag)
                        End If
                    End If
                End If
            End If
        Next

        Return conables

    End Function

    Function ReturnDownstreamDisconnectables(ByVal objTag As String) As ArrayList

        Dim obj As GraphicObject = FormFlowsheet.SearchSurfaceObjectsByTag(objTag, FlowsheetSurface)

        Dim conables As New ArrayList

        If Not obj Is Nothing Then

            Dim cp As ConnectionPoint
            For Each cp In obj.OutputConnectors
                If cp.AttachedConnector.AttachedTo.ObjectType <> ObjectType.AbsorptionColumn And cp.AttachedConnector.AttachedTo.ObjectType <> ObjectType.DistillationColumn And
                cp.AttachedConnector.AttachedTo.ObjectType <> ObjectType.RefluxedAbsorber And cp.AttachedConnector.AttachedTo.ObjectType <> ObjectType.ReboiledAbsorber Then
                    If cp.IsAttached = True And Not conables.Contains(cp.AttachedConnector.AttachedTo.Tag) Then conables.Add(cp.AttachedConnector.AttachedTo.Tag)
                End If
            Next
        End If

        Return conables

    End Function

    Function ReturnUpstreamDisconnectables(ByVal objTag As String) As ArrayList

        Dim obj As GraphicObject = FormFlowsheet.SearchSurfaceObjectsByTag(objTag, FlowsheetSurface)

        Dim conables As New ArrayList

        If Not obj Is Nothing Then

            Dim cp As ConnectionPoint
            For Each cp In obj.InputConnectors
                If cp.AttachedConnector.AttachedTo.ObjectType <> ObjectType.AbsorptionColumn And cp.AttachedConnector.AttachedTo.ObjectType <> ObjectType.DistillationColumn And
                cp.AttachedConnector.AttachedTo.ObjectType <> ObjectType.RefluxedAbsorber And cp.AttachedConnector.AttachedTo.ObjectType <> ObjectType.ReboiledAbsorber Then
                    If cp.IsAttached = True And Not conables.Contains(cp.AttachedConnector.AttachedFrom.Tag) Then conables.Add(cp.AttachedConnector.AttachedFrom.Tag)
                End If
            Next

        End If

        Return conables

    End Function

    Private Sub ExcluirToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ExcluirToolStripMenuItem.Click

        Flowsheet.tsmiRemoveSelected_Click(sender, e)

    End Sub

    Public Function AddObjectToSurface(type As ObjectType, x As Integer, y As Integer,
                                       chemsep As Boolean, Optional tag As String = "",
                                       Optional id As String = "",
                                       Optional uoobj As Interfaces.IExternalUnitOperation = Nothing,
                                       Optional CreateConnected As Boolean = False) As String

        Flowsheet.RegisterSnapshot(SnapshotType.ObjectAddedOrRemoved)

        If Flowsheet Is Nothing Then Flowsheet = My.Application.ActiveSimulation

        Dim gObj As GraphicObject = Nothing
        Dim fillclr As SKColor = SKColors.White
        Dim lineclr As SKColor = SKColors.SteelBlue
        Dim mpx = x '- SplitContainer1.SplitterDistance
        Dim mpy = y '- ToolStripContainer1.TopToolStripPanel.Height

        Dim objindex = (Flowsheet.SimulationObjects.Values.Where(Function(o) o.GraphicObject.ObjectType = type).Count + 1).ToString()

        Select Case type

            Case ObjectType.External

                Dim sobj = DirectCast(uoobj, Interfaces.ISimulationObject)
                Dim myNode As New ExternalUnitOperationGraphic(mpx, mpy, sobj.GetPreferredGraphicObjectWidth(), sobj.GetPreferredGraphicObjectHeight())
                myNode.Tag = uoobj.Prefix + objindex
                If tag <> "" Then myNode.Tag = tag
                gObj = myNode
                CheckTag(gObj)
                gObj.Name = Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                sobj.Name = gObj.Name
                Flowsheet.Collections.GraphicObjectCollection.Add(gObj.Name, myNode)
                sobj.GraphicObject = myNode
                myNode.CreateConnectors(0, 0)
                Flowsheet.Collections.FlowsheetObjectCollection.Add(myNode.Name, uoobj)

            Case ObjectType.Switch

                Dim myGobj As New SwitchGraphic(mpx, mpy, 50, 40)
                myGobj.Tag = "SW-" + objindex
                If tag <> "" Then myGobj.Tag = tag
                gObj = myGobj
                CheckTag(gObj)
                gObj.Name = "SW-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                Flowsheet.Collections.GraphicObjectCollection.Add(gObj.Name, myGobj)
                Dim myObj As UnitOperations.UnitOperations.Switch = New UnitOperations.UnitOperations.Switch(gObj.Name, "")
                myObj.GraphicObject = myGobj
                Flowsheet.Collections.FlowsheetObjectCollection.Add(myGobj.Name, myObj)

            Case ObjectType.Input

                Dim myGobj As New InputGraphic(mpx, mpy, 50, 25)
                myGobj.Tag = "IN-" + objindex
                If tag <> "" Then myGobj.Tag = tag
                gObj = myGobj
                CheckTag(gObj)
                gObj.Name = "IN-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                Flowsheet.Collections.GraphicObjectCollection.Add(gObj.Name, myGobj)
                Dim myObj As Input = New Input(gObj.Name, "")
                myObj.GraphicObject = myGobj
                Flowsheet.Collections.FlowsheetObjectCollection.Add(myGobj.Name, myObj)

                GraphicObjectControlPanelModeEditors.SetInputDelegate(myGobj, myObj)

            Case ObjectType.Controller_PID

                Dim myGobj As New PIDControllerGraphic(mpx, mpy, 50, 50)
                myGobj.Tag = "PID-" + objindex
                If tag <> "" Then myGobj.Tag = tag
                gObj = myGobj
                CheckTag(gObj)
                gObj.Name = "PID-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                Flowsheet.Collections.GraphicObjectCollection.Add(gObj.Name, myGobj)
                Dim myObj As PIDController = New PIDController(gObj.Name, "")
                myObj.GraphicObject = myGobj
                Flowsheet.Collections.FlowsheetObjectCollection.Add(myGobj.Name, myObj)

                GraphicObjectControlPanelModeEditors.SetPIDDelegate(myGobj, myObj)

            Case ObjectType.Controller_Python

                Dim myGobj As New PythonControllerGraphic(mpx, mpy, 50, 50)
                myGobj.Tag = "PC-" + objindex
                If tag <> "" Then myGobj.Tag = tag
                gObj = myGobj
                CheckTag(gObj)
                gObj.Name = "PC-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                Flowsheet.Collections.GraphicObjectCollection.Add(gObj.Name, myGobj)
                Dim myObj As PythonController = New PythonController(gObj.Name, "")
                myObj.GraphicObject = myGobj
                Flowsheet.Collections.FlowsheetObjectCollection.Add(myGobj.Name, myObj)

            Case ObjectType.LevelGauge

                Dim myGobj As New LevelGaugeGraphic(mpx, mpy, 40, 70)
                myGobj.Tag = "LG-" + objindex
                If tag <> "" Then myGobj.Tag = tag
                gObj = myGobj
                gObj.Name = "LG-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                Flowsheet.Collections.GraphicObjectCollection.Add(gObj.Name, myGobj)
                Dim myObj As LevelGauge = New LevelGauge(gObj.Name, "")
                myObj.GraphicObject = myGobj
                Flowsheet.Collections.FlowsheetObjectCollection.Add(myGobj.Name, myObj)

            Case ObjectType.DigitalGauge

                Dim myGobj As New DigitalGaugeGraphic(mpx, mpy, 40, 20)
                myGobj.Tag = "DG-" + objindex
                If tag <> "" Then myGobj.Tag = tag
                gObj = myGobj
                CheckTag(gObj)
                gObj.Name = "DG-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                Flowsheet.Collections.GraphicObjectCollection.Add(gObj.Name, myGobj)
                Dim myObj As DigitalGauge = New DigitalGauge(gObj.Name, "")
                myObj.GraphicObject = myGobj
                Flowsheet.Collections.FlowsheetObjectCollection.Add(myGobj.Name, myObj)

            Case ObjectType.AnalogGauge

                Dim myGobj As New AnalogGaugeGraphic(mpx, mpy, 50, 50)
                myGobj.Tag = "AG-" + objindex
                If tag <> "" Then myGobj.Tag = tag
                gObj = myGobj
                CheckTag(gObj)
                gObj.Name = "AG-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                Flowsheet.Collections.GraphicObjectCollection.Add(gObj.Name, myGobj)
                Dim myObj As AnalogGauge = New AnalogGauge(gObj.Name, "")
                myObj.GraphicObject = myGobj
                Flowsheet.Collections.FlowsheetObjectCollection.Add(myGobj.Name, myObj)

            Case ObjectType.OT_Adjust

                Dim myNode As New AdjustGraphic(mpx, mpy, 20, 20)
                myNode.LineWidth = 2
                myNode.Fill = True
                myNode.FillColor = fillclr
                myNode.LineColor = lineclr
                myNode.Tag = "C-" + objindex
                If tag <> "" Then myNode.Tag = tag
                gObj = myNode
                CheckTag(gObj)
                gObj.Name = DWSIM.App.GetLocalString("AJ") & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                Flowsheet.Collections.GraphicObjectCollection.Add(gObj.Name, myNode)
                Dim myADJ As Adjust = New Adjust(myNode.Name, "Ajuste")
                myADJ.GraphicObject = myNode
                Flowsheet.Collections.FlowsheetObjectCollection.Add(myNode.Name, myADJ)
                Flowsheet.WriteToLog(DWSIM.App.GetLocalTipString("ADJT001"), Color.Black, MessageType.Tip)

            Case ObjectType.OT_Spec

                Dim myNode As New SpecGraphic(mpx, mpy, 20, 20)
                myNode.LineWidth = 2
                myNode.Fill = True
                myNode.FillColor = fillclr
                myNode.LineColor = lineclr
                myNode.Tag = "SP-" + objindex
                If tag <> "" Then myNode.Tag = tag
                gObj = myNode
                CheckTag(gObj)
                gObj.Name = "ES-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                Flowsheet.Collections.GraphicObjectCollection.Add(gObj.Name, myNode)
                Dim myADJ As Spec = New Spec(myNode.Name, "Especificao")
                myADJ.GraphicObject = myNode
                Flowsheet.Collections.FlowsheetObjectCollection.Add(myNode.Name, myADJ)

            Case ObjectType.OT_Recycle

                Dim myNode As New RecycleGraphic(mpx, mpy, 20, 20)
                myNode.LineWidth = 2
                myNode.Fill = True
                myNode.FillColor = fillclr
                myNode.LineColor = lineclr
                myNode.Tag = "R-" + objindex
                If tag <> "" Then myNode.Tag = tag
                gObj = myNode
                CheckTag(gObj)
                gObj.Name = "REC-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                Flowsheet.Collections.GraphicObjectCollection.Add(gObj.Name, myNode)
                Dim myADJ As Recycle = New Recycle(myNode.Name, "Reciclo")
                myADJ.GraphicObject = myNode
                Flowsheet.Collections.FlowsheetObjectCollection.Add(myNode.Name, myADJ)

                Flowsheet.WriteToLog(DWSIM.App.GetLocalTipString("RECY001"), Color.Black, MessageType.Tip)

            Case ObjectType.OT_EnergyRecycle

                Dim myNode As New EnergyRecycleGraphic(mpx, mpy, 20, 20)
                myNode.LineWidth = 2
                myNode.Fill = True
                myNode.FillColor = fillclr
                myNode.LineColor = lineclr
                myNode.Tag = "ER-" + objindex
                If tag <> "" Then myNode.Tag = tag
                gObj = myNode
                CheckTag(gObj)
                gObj.Name = "EREC-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                Flowsheet.Collections.GraphicObjectCollection.Add(gObj.Name, myNode)
                Dim myADJ As EnergyRecycle = New EnergyRecycle(myNode.Name, "EnergyRecycle")
                myADJ.GraphicObject = myNode
                Flowsheet.Collections.FlowsheetObjectCollection.Add(myNode.Name, myADJ)

            Case ObjectType.NodeIn, ObjectType.Mixer

                Dim myNode As New MixerGraphic(mpx, mpy, 20, 20)
                myNode.LineWidth = 2
                myNode.Fill = True
                myNode.FillColor = fillclr
                myNode.LineColor = lineclr
                myNode.Tag = "MIX-" + objindex
                If tag <> "" Then myNode.Tag = tag
                gObj = myNode
                CheckTag(gObj)
                gObj.Name = "MIST-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                Flowsheet.Collections.GraphicObjectCollection.Add(gObj.Name, myNode)
                Dim myCOMIX As Mixer = New Mixer(myNode.Name, "Misturador")
                myCOMIX.GraphicObject = myNode
                Flowsheet.Collections.FlowsheetObjectCollection.Add(myNode.Name, myCOMIX)

            Case ObjectType.NodeOut, ObjectType.Splitter

                Dim myNodeo As New SplitterGraphic(mpx, mpy, 20, 20)
                myNodeo.LineWidth = 2
                myNodeo.Fill = True
                myNodeo.FillColor = fillclr
                myNodeo.LineColor = lineclr
                myNodeo.Tag = "SPL-" + objindex
                If tag <> "" Then myNodeo.Tag = tag
                gObj = myNodeo
                CheckTag(gObj)
                gObj.Name = "DIV-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                Flowsheet.Collections.GraphicObjectCollection.Add(gObj.Name, myNodeo)
                'OBJETO DWSIM
                Dim myCOSP As UnitOperations.UnitOperations.Splitter = New UnitOperations.UnitOperations.Splitter(myNodeo.Name, "Divisor")
                myCOSP.GraphicObject = myNodeo
                Flowsheet.Collections.FlowsheetObjectCollection.Add(myNodeo.Name, myCOSP)

            Case ObjectType.Pump

                Dim myPump As New PumpGraphic(mpx, mpy, 25, 25)
                If Flowsheet.FlowsheetOptions.FlowsheetColorTheme = 2 Then
                    myPump.SetSize(New SKSize(40, 40))
                End If
                myPump.LineWidth = 2
                myPump.Fill = True
                myPump.FillColor = fillclr
                myPump.LineColor = lineclr
                myPump.Tag = "PUMP-" + objindex
                If tag <> "" Then myPump.Tag = tag
                gObj = myPump
                CheckTag(gObj)
                gObj.Name = "BB-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                Flowsheet.Collections.GraphicObjectCollection.Add(gObj.Name, myPump)
                'OBJETO DWSIM
                Dim myCOSP As Pump = New Pump(myPump.Name, "Bomba")
                myCOSP.GraphicObject = myPump
                Flowsheet.Collections.FlowsheetObjectCollection.Add(myPump.Name, myCOSP)

            Case ObjectType.Tank

                Dim myTank As New TankGraphic(mpx, mpy, 50, 50)
                myTank.LineWidth = 2
                myTank.Fill = True
                myTank.FillColor = fillclr
                myTank.LineColor = lineclr
                myTank.Tag = "TANK-" + objindex
                If tag <> "" Then myTank.Tag = tag
                gObj = myTank
                CheckTag(gObj)
                gObj.Name = "TQ-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                Flowsheet.Collections.GraphicObjectCollection.Add(gObj.Name, myTank)
                'OBJETO DWSIM
                Dim myCOTK As Tank = New Tank(myTank.Name, "Tanque")
                myCOTK.GraphicObject = myTank
                Flowsheet.Collections.FlowsheetObjectCollection.Add(myTank.Name, myCOTK)

            Case ObjectType.Vessel

                Dim myVessel As New VesselGraphic(mpx, mpy, 50, 50)
                If Flowsheet.FlowsheetOptions.FlowsheetColorTheme = 2 Then
                    myVessel.SetSize(New SKSize(70, 60))
                End If
                myVessel.LineWidth = 2
                myVessel.Fill = True
                myVessel.FillColor = fillclr
                myVessel.LineColor = lineclr
                myVessel.Tag = "V-" + objindex
                If tag <> "" Then myVessel.Tag = tag
                gObj = myVessel
                CheckTag(gObj)
                gObj.Name = "SEP-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                Flowsheet.Collections.GraphicObjectCollection.Add(gObj.Name, myVessel)
                'OBJETO DWSIM
                Dim myCOVESSEL As Vessel = New Vessel(myVessel.Name, "VasoSeparadorGL")
                myCOVESSEL.GraphicObject = myVessel
                Flowsheet.Collections.FlowsheetObjectCollection.Add(myVessel.Name, myCOVESSEL)

            Case ObjectType.MaterialStream

                Dim myMStr As New MaterialStreamGraphic(mpx, mpy, 20, 20)
                myMStr.LineWidth = 2
                myMStr.Fill = True
                myMStr.FillColor = fillclr
                myMStr.LineColor = lineclr
                myMStr.Tag = objindex
                If tag <> "" Then myMStr.Tag = tag
                gObj = myMStr
                CheckTag(gObj)
                gObj.Name = "MAT-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                Flowsheet.Collections.GraphicObjectCollection.Add(gObj.Name, myMStr)
                'OBJETO DWSIM
                Dim myCOMS As Thermodynamics.Streams.MaterialStream = New Thermodynamics.Streams.MaterialStream(myMStr.Name, "CorrentedeMatria", Flowsheet, Nothing)
                myCOMS.GraphicObject = myMStr
                Flowsheet.AddComponentsRows(myCOMS)
                Flowsheet.Collections.FlowsheetObjectCollection.Add(myCOMS.Name, myCOMS)

                'Flowsheet.WriteToLog(DWSIM.App.GetLocalTipString("MSTR001"), Color.Black, MessageType.Tip)
                'Flowsheet.WriteToLog(DWSIM.App.GetLocalTipString("MSTR002"), Color.Black, MessageType.Tip)

            Case ObjectType.EnergyStream

                Dim myMStr As New EnergyStreamGraphic(mpx, mpy, 20, 20)
                myMStr.LineWidth = 2
                myMStr.Fill = True
                myMStr.Tag = "E" + objindex
                If tag <> "" Then myMStr.Tag = tag
                gObj = myMStr
                CheckTag(gObj)
                gObj.Name = "EN-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                Flowsheet.Collections.GraphicObjectCollection.Add(gObj.Name, myMStr)
                'OBJETO DWSIM
                Dim myCOES As EnergyStream = New Streams.EnergyStream(myMStr.Name, "CorrentedeEnergia")
                myCOES.GraphicObject = myMStr
                Flowsheet.Collections.FlowsheetObjectCollection.Add(myCOES.Name, myCOES)

            Case ObjectType.Compressor

                Dim myComp As New CompressorGraphic(mpx, mpy, 25, 25)
                If Flowsheet.FlowsheetOptions.FlowsheetColorTheme = 2 Then
                    myComp.SetSize(New SKSize(50, 50))
                End If
                myComp.LineWidth = 2
                myComp.Fill = True
                myComp.FillColor = fillclr
                myComp.LineColor = lineclr
                myComp.Tag = "C-" + objindex
                If tag <> "" Then myComp.Tag = tag
                gObj = myComp
                CheckTag(gObj)
                gObj.Name = "COMP-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                Flowsheet.Collections.GraphicObjectCollection.Add(gObj.Name, myComp)
                'OBJETO DWSIM
                Dim myCOCP As Compressor = New Compressor(myComp.Name, "CompressorAdiabtico")
                myCOCP.GraphicObject = myComp
                Flowsheet.Collections.FlowsheetObjectCollection.Add(myComp.Name, myCOCP)

            Case ObjectType.Expander

                Dim myComp As New TurbineGraphic(mpx, mpy, 25, 25)
                If Flowsheet.FlowsheetOptions.FlowsheetColorTheme = 2 Then
                    myComp.SetSize(New SKSize(50, 50))
                End If
                myComp.LineWidth = 2
                myComp.Fill = True
                myComp.FillColor = fillclr
                myComp.LineColor = lineclr
                myComp.Tag = "X-" + objindex
                If tag <> "" Then myComp.Tag = tag
                gObj = myComp
                gObj.Name = "TURB-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                Flowsheet.Collections.GraphicObjectCollection.Add(gObj.Name, myComp)
                'OBJETO DWSIM
                Dim myCOCP As Expander = New Expander(myComp.Name, "TurbinaAdiabtica")
                myCOCP.GraphicObject = myComp
                Flowsheet.Collections.FlowsheetObjectCollection.Add(myComp.Name, myCOCP)

            Case ObjectType.Cooler

                Dim myCool As New CoolerGraphic(mpx, mpy, 25, 25)
                myCool.LineWidth = 2
                myCool.Fill = True
                myCool.FillColor = fillclr
                myCool.LineColor = lineclr
                myCool.Tag = "CL-" + objindex
                If tag <> "" Then myCool.Tag = tag
                gObj = myCool
                CheckTag(gObj)
                gObj.Name = "RESF-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                Flowsheet.Collections.GraphicObjectCollection.Add(gObj.Name, myCool)
                'OBJETO DWSIM
                Dim myCOCL As Cooler = New Cooler(myCool.Name, "Resfriador")
                myCOCL.GraphicObject = myCool
                Flowsheet.Collections.FlowsheetObjectCollection.Add(myCool.Name, myCOCL)

            Case ObjectType.Heater

                Dim myHeat As New HeaterGraphic(mpx, mpy, 25, 25)
                myHeat.LineWidth = 2
                myHeat.Fill = True
                myHeat.FillColor = fillclr
                myHeat.LineColor = lineclr
                myHeat.Tag = "HT-" + objindex
                If tag <> "" Then myHeat.Tag = tag
                gObj = myHeat
                CheckTag(gObj)
                gObj.Name = "AQ-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                Flowsheet.Collections.GraphicObjectCollection.Add(gObj.Name, myHeat)
                'OBJETO DWSIM
                Dim myCOCL As Heater = New Heater(myHeat.Name, "Aquecedor")
                myCOCL.GraphicObject = myHeat
                Flowsheet.Collections.FlowsheetObjectCollection.Add(myHeat.Name, myCOCL)

            Case ObjectType.Pipe

                Dim myPipe As New PipeSegmentGraphic(mpx, mpy, 50, 10)
                If Flowsheet.FlowsheetOptions.FlowsheetColorTheme = 2 Then
                    myPipe.SetSize(New SKSize(50, 20))
                End If
                myPipe.LineWidth = 2
                myPipe.Fill = True
                myPipe.FillColor = fillclr
                myPipe.LineColor = lineclr
                myPipe.Tag = "PIPE-" + objindex
                If tag <> "" Then myPipe.Tag = tag
                gObj = myPipe
                CheckTag(gObj)
                gObj.Name = "TUB-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                Flowsheet.Collections.GraphicObjectCollection.Add(gObj.Name, myPipe)
                'OBJETO DWSIM
                Dim myCOPIPE As Pipe = New Pipe(myPipe.Name, "Tubulao")
                myCOPIPE.GraphicObject = myPipe
                Flowsheet.Collections.FlowsheetObjectCollection.Add(myPipe.Name, myCOPIPE)

            Case ObjectType.Valve

                Dim myValve As New ValveGraphic(mpx, mpy, 20, 20)
                Dim myPipe As New PipeSegmentGraphic(mpx, mpy, 50, 10)
                If Flowsheet.FlowsheetOptions.FlowsheetColorTheme = 2 Then
                    myValve.SetSize(New SKSize(30, 30))
                End If
                myValve.LineWidth = 2
                myValve.Fill = True
                myValve.FillColor = fillclr
                myValve.LineColor = lineclr
                myValve.Tag = "VALVE-" + objindex
                If tag <> "" Then myValve.Tag = tag
                gObj = myValve
                CheckTag(gObj)
                gObj.Name = "VALV-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                Flowsheet.Collections.GraphicObjectCollection.Add(gObj.Name, myValve)
                'OBJETO DWSIM
                Dim myCOVALVE As Valve = New Valve(myValve.Name, "Vlvula")
                myCOVALVE.GraphicObject = myValve
                Flowsheet.Collections.FlowsheetObjectCollection.Add(myValve.Name, myCOVALVE)

            Case ObjectType.RCT_Conversion

                Dim myRconv As New ConversionReactorGraphic(mpx, mpy, 50, 50)
                myRconv.LineWidth = 2
                myRconv.Fill = True
                myRconv.FillColor = fillclr
                myRconv.LineColor = lineclr
                myRconv.Tag = "RCONV-" + objindex
                If tag <> "" Then myRconv.Tag = tag
                gObj = myRconv
                gObj.Name = "RC-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                Flowsheet.Collections.GraphicObjectCollection.Add(gObj.Name, myRconv)
                'OBJETO DWSIM
                Dim myCORCONV As Reactor_Conversion = New Reactor_Conversion(myRconv.Name, "ReatorConversao")
                myCORCONV.GraphicObject = myRconv
                Flowsheet.Collections.FlowsheetObjectCollection.Add(myRconv.Name, myCORCONV)

                Flowsheet.WriteToLog(DWSIM.App.GetLocalTipString("RCON001"), Color.Black, MessageType.Tip)

            Case ObjectType.RCT_Equilibrium

                Dim myReq As New EquilibriumReactorGraphic(mpx, mpy, 50, 50)
                myReq.LineWidth = 2
                myReq.Fill = True
                myReq.FillColor = fillclr
                myReq.LineColor = lineclr
                myReq.Tag = "REQ-" + objindex
                If tag <> "" Then myReq.Tag = tag
                gObj = myReq
                CheckTag(gObj)
                gObj.Name = "RE-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                Flowsheet.Collections.GraphicObjectCollection.Add(gObj.Name, myReq)
                'OBJETO DWSIM
                Dim myCOREQ As Reactors.Reactor_Equilibrium = New Reactor_Equilibrium(myReq.Name, "ReatorEquilibrio")
                myCOREQ.GraphicObject = myReq
                Flowsheet.Collections.FlowsheetObjectCollection.Add(myReq.Name, myCOREQ)

                Flowsheet.WriteToLog(DWSIM.App.GetLocalTipString("REQL001"), Color.Black, MessageType.Tip)

            Case ObjectType.RCT_Gibbs

                Dim myRgibbs As New GibbsReactorGraphic(mpx, mpy, 50, 50)
                myRgibbs.LineWidth = 2
                myRgibbs.Fill = True
                myRgibbs.FillColor = fillclr
                myRgibbs.LineColor = lineclr
                myRgibbs.Tag = "RGIBBS-" + objindex
                If tag <> "" Then myRgibbs.Tag = tag
                gObj = myRgibbs
                gObj.Name = "RG-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                Flowsheet.Collections.GraphicObjectCollection.Add(gObj.Name, myRgibbs)
                'OBJETO DWSIM
                Dim myCORGIBBS As Reactor_Gibbs = New Reactor_Gibbs(myRgibbs.Name, "ReatorGibbs")
                myCORGIBBS.GraphicObject = myRgibbs
                Flowsheet.Collections.FlowsheetObjectCollection.Add(myRgibbs.Name, myCORGIBBS)

                Flowsheet.WriteToLog(DWSIM.App.GetLocalTipString("RGIB001"), Color.Black, MessageType.Tip)

            Case ObjectType.RCT_CSTR

                Dim myRcstr As New CSTRGraphic(mpx, mpy, 50, 50)
                myRcstr.LineWidth = 2
                myRcstr.Fill = True
                myRcstr.FillColor = fillclr
                myRcstr.LineColor = lineclr
                myRcstr.Tag = "CSTR-" + objindex
                If tag <> "" Then myRcstr.Tag = tag
                gObj = myRcstr
                CheckTag(gObj)
                gObj.Name = "CSTR-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                Flowsheet.Collections.GraphicObjectCollection.Add(gObj.Name, myRcstr)
                'OBJETO DWSIM
                Dim myCORCSTR As Reactor_CSTR = New Reactor_CSTR(myRcstr.Name, "ReatorCSTR")
                myCORCSTR.GraphicObject = myRcstr
                Flowsheet.Collections.FlowsheetObjectCollection.Add(myRcstr.Name, myCORCSTR)

                Flowsheet.WriteToLog(DWSIM.App.GetLocalTipString("CSTR001"), Color.Black, MessageType.Tip)

            Case ObjectType.RCT_PFR

                Dim myRpfr As New PFRGraphic(mpx, mpy, 70, 20)
                myRpfr.LineWidth = 2
                myRpfr.Fill = True
                myRpfr.FillColor = fillclr
                myRpfr.LineColor = lineclr
                myRpfr.Tag = "PFR-" + objindex
                If tag <> "" Then myRpfr.Tag = tag
                gObj = myRpfr
                CheckTag(gObj)
                gObj.Name = "PFR-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                Flowsheet.Collections.GraphicObjectCollection.Add(gObj.Name, myRpfr)
                'OBJETO DWSIM
                Dim myCOPFR As Reactor_PFR = New Reactor_PFR(myRpfr.Name, "ReatorPFR")
                myCOPFR.GraphicObject = myRpfr
                Flowsheet.Collections.FlowsheetObjectCollection.Add(myRpfr.Name, myCOPFR)

                Flowsheet.WriteToLog(DWSIM.App.GetLocalTipString("PFR001"), Color.Black, MessageType.Tip)

            Case ObjectType.HeatExchanger

                Dim myHeatExchanger As New HeatExchangerGraphic(mpx, mpy, 30, 30)
                If Flowsheet.FlowsheetOptions.FlowsheetColorTheme = 2 Then
                    myHeatExchanger.SetSize(New SKSize(60, 60))
                End If
                myHeatExchanger.LineWidth = 2
                myHeatExchanger.Fill = True
                myHeatExchanger.FillColor = fillclr
                myHeatExchanger.LineColor = lineclr
                myHeatExchanger.Tag = "HX-" + objindex
                If tag <> "" Then myHeatExchanger.Tag = tag
                gObj = myHeatExchanger
                CheckTag(gObj)
                gObj.Name = "HE-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                Flowsheet.Collections.GraphicObjectCollection.Add(gObj.Name, myHeatExchanger)
                'OBJETO DWSIM
                Dim myCOHE As HeatExchanger = New HeatExchanger(myHeatExchanger.Name, "HeatExchanger")
                myCOHE.GraphicObject = myHeatExchanger
                Flowsheet.Collections.FlowsheetObjectCollection.Add(myHeatExchanger.Name, myCOHE)

            Case ObjectType.ShortcutColumn

                Dim mySC As New ShortcutColumnGraphic(mpx, mpy, 144, 180)
                mySC.LineWidth = 2
                mySC.Fill = True
                mySC.FillColor = fillclr
                mySC.LineColor = lineclr
                mySC.Tag = "SCOL-" + objindex
                If tag <> "" Then mySC.Tag = tag
                gObj = mySC
                gObj.Name = "SC-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                Flowsheet.Collections.GraphicObjectCollection.Add(gObj.Name, mySC)
                'OBJETO DWSIM
                Dim myCOSC As ShortcutColumn = New ShortcutColumn(mySC.Name, "ShortcutColumn")
                myCOSC.GraphicObject = mySC
                Flowsheet.Collections.FlowsheetObjectCollection.Add(mySC.Name, myCOSC)

            Case ObjectType.DistillationColumn

                Dim mySC As New RigorousColumnGraphic(mpx, mpy, 144, 180)
                mySC.LineWidth = 2
                mySC.Fill = True
                mySC.FillColor = fillclr
                mySC.LineColor = lineclr
                mySC.Tag = "DCOL-" + objindex
                If tag <> "" Then mySC.Tag = tag
                gObj = mySC
                CheckTag(gObj)
                gObj.Name = "DC-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                Flowsheet.Collections.GraphicObjectCollection.Add(gObj.Name, mySC)
                'OBJETO DWSIM
                Dim myCOSC As DistillationColumn = New DistillationColumn(mySC.Name, "DistillationColumn", Flowsheet)
                myCOSC.GraphicObject = mySC
                Flowsheet.Collections.FlowsheetObjectCollection.Add(mySC.Name, myCOSC)

                Flowsheet.WriteToLog(DWSIM.App.GetLocalTipString("DCOL001"), Color.Black, MessageType.Tip)
                Flowsheet.WriteToLog(DWSIM.App.GetLocalTipString("DCOL002"), Color.Black, MessageType.Tip)
                Flowsheet.WriteToLog(DWSIM.App.GetLocalTipString("DCOL003"), Color.Black, MessageType.Tip)

            Case ObjectType.AbsorptionColumn

                Dim mySC As New AbsorptionColumnGraphic(mpx, mpy, 144, 180)
                mySC.LineWidth = 2
                mySC.Fill = True
                mySC.FillColor = fillclr
                mySC.LineColor = lineclr
                mySC.Tag = "ABS-" + objindex
                If tag <> "" Then mySC.Tag = tag
                gObj = mySC
                gObj.Name = "ABS-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                Flowsheet.Collections.GraphicObjectCollection.Add(gObj.Name, mySC)
                'OBJETO DWSIM
                Dim myCOSC As AbsorptionColumn = New AbsorptionColumn(mySC.Name, "AbsorptionColumn", Flowsheet)
                myCOSC.GraphicObject = mySC
                Flowsheet.Collections.FlowsheetObjectCollection.Add(mySC.Name, myCOSC)

                Flowsheet.WriteToLog(DWSIM.App.GetLocalTipString("DCOL001"), Color.Black, MessageType.Tip)
                Flowsheet.WriteToLog(DWSIM.App.GetLocalTipString("DCOL002"), Color.Black, MessageType.Tip)
                Flowsheet.WriteToLog(DWSIM.App.GetLocalTipString("DCOL003"), Color.Black, MessageType.Tip)

            Case ObjectType.ComponentSeparator

                Dim myCSep As New ComponentSeparatorGraphic(mpx, mpy, 50, 50)
                myCSep.LineWidth = 2
                myCSep.Fill = True
                myCSep.FillColor = fillclr
                myCSep.LineColor = lineclr
                myCSep.Tag = "CS-" + objindex
                If tag <> "" Then myCSep.Tag = tag
                gObj = myCSep
                CheckTag(gObj)
                gObj.Name = "CS-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                Flowsheet.Collections.GraphicObjectCollection.Add(gObj.Name, myCSep)
                'OBJETO DWSIM
                Dim myCOCSEP As ComponentSeparator = New ComponentSeparator(myCSep.Name, "ComponentSeparator")
                myCOCSEP.GraphicObject = myCSep
                Flowsheet.Collections.FlowsheetObjectCollection.Add(myCSep.Name, myCOCSEP)

            Case ObjectType.SolidSeparator

                Dim myCSep As New SolidsSeparatorGraphic(mpx, mpy, 50, 50)
                myCSep.LineWidth = 2
                myCSep.Fill = True
                myCSep.FillColor = fillclr
                myCSep.LineColor = lineclr
                myCSep.Tag = "SS-" + objindex
                If tag <> "" Then myCSep.Tag = tag
                gObj = myCSep
                gObj.Name = "SS-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                Flowsheet.Collections.GraphicObjectCollection.Add(gObj.Name, myCSep)
                'OBJETO DWSIM
                Dim myCOCSEP As SolidsSeparator = New SolidsSeparator(myCSep.Name, "SolidsSeparator")
                myCOCSEP.GraphicObject = myCSep
                Flowsheet.Collections.FlowsheetObjectCollection.Add(myCSep.Name, myCOCSEP)

            Case ObjectType.Filter

                Dim myCSep As New FilterGraphic(mpx, mpy, 50, 50)
                myCSep.LineWidth = 2
                myCSep.Fill = True
                myCSep.FillColor = fillclr
                myCSep.LineColor = lineclr
                myCSep.Tag = "FLT-" + objindex
                If tag <> "" Then myCSep.Tag = tag
                gObj = myCSep
                CheckTag(gObj)
                gObj.Name = "FT-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                Flowsheet.Collections.GraphicObjectCollection.Add(gObj.Name, myCSep)
                'OBJETO DWSIM
                Dim myCOCSEP As Filter = New Filter(myCSep.Name, "Filter")
                myCOCSEP.GraphicObject = myCSep
                Flowsheet.Collections.FlowsheetObjectCollection.Add(myCSep.Name, myCOCSEP)

            Case ObjectType.OrificePlate

                Dim myOPL As New OrificePlateGraphic(mpx, mpy, 25, 25)
                myOPL.LineWidth = 2
                myOPL.Fill = True
                myOPL.FillColor = fillclr
                myOPL.LineColor = lineclr
                myOPL.Tag = "OP-" + objindex
                If tag <> "" Then myOPL.Tag = tag
                gObj = myOPL
                CheckTag(gObj)
                gObj.Name = "OP-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                Flowsheet.Collections.GraphicObjectCollection.Add(gObj.Name, myOPL)
                'OBJETO DWSIM
                Dim myCOOPL As OrificePlate = New OrificePlate(myOPL.Name, "OrificePlate")
                myCOOPL.GraphicObject = myOPL
                Flowsheet.Collections.FlowsheetObjectCollection.Add(myOPL.Name, myCOOPL)

            Case ObjectType.CustomUO

                Dim myCUO As New ScriptGraphic(mpx, mpy, 25, 25)
                myCUO.LineWidth = 2
                myCUO.Fill = True
                myCUO.FillColor = fillclr
                myCUO.LineColor = lineclr
                myCUO.Tag = "CUSTOM-" + objindex
                If tag <> "" Then myCUO.Tag = tag
                gObj = myCUO
                CheckTag(gObj)
                gObj.Name = "UO-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                Flowsheet.Collections.GraphicObjectCollection.Add(gObj.Name, myCUO)
                'OBJETO DWSIM
                Dim myCOCUO As CustomUO = New CustomUO(myCUO.Name, "CustomUnitOp")
                myCOCUO.GraphicObject = myCUO
                Flowsheet.Collections.FlowsheetObjectCollection.Add(myCUO.Name, myCOCUO)

                Flowsheet.WriteToLog(DWSIM.App.GetLocalTipString("CSUO001"), Color.Black, MessageType.Tip)

            Case ObjectType.ExcelUO

                Dim myEUO As New SpreadsheetGraphic(mpx, mpy, 25, 25)
                myEUO.LineWidth = 2
                myEUO.Fill = True
                myEUO.FillColor = fillclr
                myEUO.LineColor = lineclr
                myEUO.Tag = "SHEET-" + objindex
                If tag <> "" Then myEUO.Tag = tag
                gObj = myEUO
                CheckTag(gObj)
                gObj.Name = "EXL-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                Flowsheet.Collections.GraphicObjectCollection.Add(gObj.Name, myEUO)
                'OBJETO DWSIM
                Dim myCOEUO As ExcelUO = New ExcelUO(myEUO.Name, "ExcelUnitOp")
                myCOEUO.GraphicObject = myEUO
                Flowsheet.Collections.FlowsheetObjectCollection.Add(myEUO.Name, myCOEUO)

            Case ObjectType.FlowsheetUO

                Dim myEUO As New FlowsheetGraphic(mpx, mpy, 25, 25)
                myEUO.LineWidth = 2
                myEUO.Fill = True
                myEUO.FillColor = fillclr
                myEUO.LineColor = lineclr
                myEUO.Tag = "FS-" + objindex
                If tag <> "" Then myEUO.Tag = tag
                gObj = myEUO
                CheckTag(gObj)
                gObj.Name = "FS-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                Flowsheet.Collections.GraphicObjectCollection.Add(gObj.Name, myEUO)
                'OBJETO DWSIM
                Dim myCOEUO As UnitOperations.UnitOperations.Flowsheet = New UnitOperations.UnitOperations.Flowsheet(myEUO.Name, "FlowsheetUnitOp")
                myCOEUO.GraphicObject = myEUO
                Flowsheet.Collections.FlowsheetObjectCollection.Add(myEUO.Name, myCOEUO)

            Case ObjectType.CapeOpenUO

                Dim myCUO As New CAPEOPENGraphic(mpx, mpy, 40, 40)
                myCUO.LineWidth = 2
                myCUO.Fill = True
                myCUO.FillColor = fillclr
                myCUO.LineColor = lineclr
                myCUO.Tag = "CO-" + objindex
                If tag <> "" Then myCUO.Tag = tag
                gObj = myCUO
                CheckTag(gObj)
                gObj.Name = "COUO-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                Flowsheet.Collections.GraphicObjectCollection.Add(gObj.Name, myCUO)
                'OBJETO DWSIM
                If chemsep Then
                    gObj.Tag = "CSCOL-" + objindex
                    DirectCast(gObj, CAPEOPENGraphic).ChemSep = True
                    gObj.Width = 144
                    gObj.Height = 180
                End If
                Dim myCOCUO As CapeOpenUO = New CapeOpenUO(myCUO.Name, "CapeOpenUnitOperation", gObj, chemsep)
                myCOCUO.GraphicObject = myCUO
                Flowsheet.Collections.FlowsheetObjectCollection.Add(myCUO.Name, myCOCUO)

                Flowsheet.WriteToLog(DWSIM.App.GetLocalTipString("CAPE001"), Color.Black, MessageType.Tip)

            Case ObjectType.AirCooler2

                Dim fsobj = New AirCooler2()
                Dim grobj As New ExternalUnitOperationGraphic(mpx, mpy, 40, 40)
                grobj.Tag = "AC-" + objindex
                If tag <> "" Then grobj.Tag = tag
                gObj = grobj
                CheckTag(gObj)
                gObj.Name = Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                DirectCast(fsobj, Interfaces.ISimulationObject).Name = gObj.Name
                Flowsheet.Collections.GraphicObjectCollection.Add(gObj.Name, grobj)
                DirectCast(fsobj, Interfaces.ISimulationObject).GraphicObject = grobj
                grobj.CreateConnectors(0, 0)
                Flowsheet.Collections.FlowsheetObjectCollection.Add(grobj.Name, fsobj)

            Case ObjectType.EnergyMixer

                Dim fsobj = New EnergyMixer()
                Dim grobj As New ExternalUnitOperationGraphic(mpx, mpy, 40, 40)
                grobj.Tag = "EMIX-" + objindex
                If tag <> "" Then grobj.Tag = tag
                gObj = grobj
                CheckTag(gObj)
                gObj.Name = Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                DirectCast(fsobj, Interfaces.ISimulationObject).Name = gObj.Name
                Flowsheet.Collections.GraphicObjectCollection.Add(gObj.Name, grobj)
                DirectCast(fsobj, Interfaces.ISimulationObject).GraphicObject = grobj
                grobj.CreateConnectors(0, 0)
                Flowsheet.Collections.FlowsheetObjectCollection.Add(grobj.Name, fsobj)

            Case ObjectType.RCT_GibbsReaktoro

                Dim fsobj = New Reactor_ReaktoroGibbs()
                Dim grobj As New ExternalUnitOperationGraphic(mpx, mpy, 40, 40)
                grobj.Tag = "RGIBBSR-" + objindex
                If tag <> "" Then grobj.Tag = tag
                gObj = grobj
                CheckTag(gObj)
                gObj.Name = Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                DirectCast(fsobj, Interfaces.ISimulationObject).Name = gObj.Name
                Flowsheet.Collections.GraphicObjectCollection.Add(gObj.Name, grobj)
                DirectCast(fsobj, Interfaces.ISimulationObject).GraphicObject = grobj
                grobj.CreateConnectors(0, 0)
                Flowsheet.Collections.FlowsheetObjectCollection.Add(grobj.Name, fsobj)

            Case ObjectType.WindTurbine

                Dim fsobj = New WindTurbine()
                Dim grobj As New ExternalUnitOperationGraphic(mpx, mpy, 40, 40)
                grobj.Tag = "WT-" + objindex
                If tag <> "" Then grobj.Tag = tag
                gObj = grobj
                CheckTag(gObj)
                gObj.Name = Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                DirectCast(fsobj, Interfaces.ISimulationObject).Name = gObj.Name
                Flowsheet.Collections.GraphicObjectCollection.Add(gObj.Name, grobj)
                DirectCast(fsobj, Interfaces.ISimulationObject).GraphicObject = grobj
                grobj.CreateConnectors(0, 0)
                Flowsheet.Collections.FlowsheetObjectCollection.Add(grobj.Name, fsobj)

            Case ObjectType.HydroelectricTurbine

                Dim fsobj = New HydroelectricTurbine()
                Dim grobj As New ExternalUnitOperationGraphic(mpx, mpy, 40, 40)
                grobj.Tag = "HYT-" + objindex
                If tag <> "" Then grobj.Tag = tag
                gObj = grobj
                CheckTag(gObj)
                gObj.Name = Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                DirectCast(fsobj, Interfaces.ISimulationObject).Name = gObj.Name
                Flowsheet.Collections.GraphicObjectCollection.Add(gObj.Name, grobj)
                DirectCast(fsobj, Interfaces.ISimulationObject).GraphicObject = grobj
                grobj.CreateConnectors(0, 0)
                Flowsheet.Collections.FlowsheetObjectCollection.Add(grobj.Name, fsobj)

            Case ObjectType.PEMFuelCell

                Dim fsobj = New PEMFC_Amphlett()
                Dim grobj As New ExternalUnitOperationGraphic(mpx, mpy, 40, 40)
                grobj.Tag = "PEMFC-" + objindex
                If tag <> "" Then grobj.Tag = tag
                gObj = grobj
                CheckTag(gObj)
                gObj.Name = Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                DirectCast(fsobj, Interfaces.ISimulationObject).Name = gObj.Name
                Flowsheet.Collections.GraphicObjectCollection.Add(gObj.Name, grobj)
                DirectCast(fsobj, Interfaces.ISimulationObject).GraphicObject = grobj
                grobj.CreateConnectors(0, 0)
                Flowsheet.Collections.FlowsheetObjectCollection.Add(grobj.Name, fsobj)

            Case ObjectType.SolarPanel

                Dim fsobj = New SolarPanel()
                Dim grobj As New ExternalUnitOperationGraphic(mpx, mpy, 40, 40)
                grobj.Tag = "SP-" + objindex
                If tag <> "" Then grobj.Tag = tag
                gObj = grobj
                CheckTag(gObj)
                gObj.Name = Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                DirectCast(fsobj, Interfaces.ISimulationObject).Name = gObj.Name
                Flowsheet.Collections.GraphicObjectCollection.Add(gObj.Name, grobj)
                DirectCast(fsobj, Interfaces.ISimulationObject).GraphicObject = grobj
                grobj.CreateConnectors(0, 0)
                Flowsheet.Collections.FlowsheetObjectCollection.Add(grobj.Name, fsobj)

            Case ObjectType.WaterElectrolyzer

                Dim fsobj = New WaterElectrolyzer()
                Dim grobj As New ExternalUnitOperationGraphic(mpx, mpy, 40, 40)
                grobj.Tag = "WELEC-" + objindex
                If tag <> "" Then grobj.Tag = tag
                gObj = grobj
                CheckTag(gObj)
                gObj.Name = Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                DirectCast(fsobj, Interfaces.ISimulationObject).Name = gObj.Name
                Flowsheet.Collections.GraphicObjectCollection.Add(gObj.Name, grobj)
                DirectCast(fsobj, Interfaces.ISimulationObject).GraphicObject = grobj
                grobj.CreateConnectors(0, 0)
                Flowsheet.Collections.FlowsheetObjectCollection.Add(grobj.Name, fsobj)

        End Select

        If Not gObj Is Nothing Then
            gObj.Flowsheet = Flowsheet
            gObj.Owner = Me.Flowsheet.SimulationObjects(gObj.Name)
            Me.Flowsheet.SimulationObjects(gObj.Name).SetFlowsheet(Flowsheet)
            FlowsheetSurface.DrawingObjects.Add(gObj)
            SplitContainerHorizontal.Panel1.Invalidate()
            For Each obj In Me.Flowsheet.SimulationObjects.Values
                obj.UpdateEditForm()
                EditorTooltips.Update(obj, Flowsheet)
            Next
            'If TypeOf gObj.Owner Is Thermodynamics.Streams.MaterialStream Then
            '    gObj.CreateConnectors(1, 1)
            '    If Flowsheet.Visible Then FlowsheetSolver.FlowsheetSolver.CalculateObject(Me.Flowsheet, gObj.Name)
            'End If
            gObj.PositionConnectors()
            Application.DoEvents()
        End If

        SplitContainerHorizontal.Panel1.Cursor = Cursors.Arrow

        If CreateConnected Then
            Try
                If Flowsheet.Options.AddObjectsWithStreams = 1 Then
                    AddConnectedObjects(Flowsheet.SimulationObjects(gObj.Name), 1)
                    Flowsheet.UpdateInterface()
                End If
                If Flowsheet.Options.AddObjectsWithStreams = 2 Then
                    AddConnectedObjects(Flowsheet.SimulationObjects(gObj.Name), 2)
                    AddConnectedObjects(Flowsheet.SimulationObjects(gObj.Name), 1)
                    Flowsheet.UpdateInterface()
                End If
            Catch ex As Exception
            End Try
        End If

        Return gObj.Name

    End Function

    Public Sub AddConnectedObjects(obj As Interfaces.ISimulationObject, scheme As Integer)

        Dim x = obj.GraphicObject.X
        Dim y = obj.GraphicObject.Y
        Dim w = obj.GraphicObject.Width
        Dim h = obj.GraphicObject.Height

        Dim gobj = obj.GraphicObject

        Dim mstrs = FlowsheetSurface.FindObjectsAtBounds(x - 250, y - 250, 500, 500).Where(Function(o) o.ObjectType = ObjectType.MaterialStream).ToList()
        Dim mstrsI = FlowsheetSurface.FindObjectsAtBounds(x - 250, y - 250, 250, 500).Where(Function(o) o.ObjectType = ObjectType.MaterialStream And Not o.OutputConnectors(0).IsAttached).ToList()
        Dim mstrsO = FlowsheetSurface.FindObjectsAtBounds(x + w, y - 250, 250, 500).Where(Function(o) o.ObjectType = ObjectType.MaterialStream And Not o.InputConnectors(0).IsAttached).ToList()
        Dim estrs = FlowsheetSurface.FindObjectsAtBounds(x - 250, y - 250, 500, 500).Where(Function(o) o.ObjectType = ObjectType.EnergyStream).ToList()
        Dim estrsI = FlowsheetSurface.FindObjectsAtBounds(x - 250, y - 250, 250, 500).Where(Function(o) o.ObjectType = ObjectType.EnergyStream And Not o.OutputConnectors(0).IsAttached).ToList()
        Dim estrsO = FlowsheetSurface.FindObjectsAtBounds(x + w, y - 250, 250, 500).Where(Function(o) o.ObjectType = ObjectType.EnergyStream And Not o.InputConnectors(0).IsAttached).ToList()

        Dim uosI = FlowsheetSurface.FindObjectsAtBounds(x - 250, y - 250, 250, 500).Where(Function(o) o.ObjectType <> ObjectType.MaterialStream And
                                                                                             o.ObjectType <> ObjectType.EnergyStream).ToList()
        Dim uosO = FlowsheetSurface.FindObjectsAtBounds(x + w, y - 250, 250, 500).Where(Function(o) o.ObjectType <> ObjectType.MaterialStream And
                                                                                             o.ObjectType <> ObjectType.EnergyStream).ToList()

        mstrs = mstrs.OrderBy(Function(o) Math.Abs(gobj.X - o.X) + Math.Abs(gobj.Y - o.Y)).ToList()
        mstrsI = mstrsI.OrderBy(Function(o) Math.Abs(gobj.X - o.X) + Math.Abs(gobj.Y - o.Y)).ToList()
        mstrsO = mstrsO.OrderBy(Function(o) Math.Abs(gobj.X - o.X) + Math.Abs(gobj.Y - o.Y)).ToList()
        estrs = estrs.OrderBy(Function(o) Math.Abs(gobj.X - o.X) + Math.Abs(gobj.Y - o.Y)).ToList()
        estrsI = estrsI.OrderBy(Function(o) Math.Abs(gobj.X - o.X) + Math.Abs(gobj.Y - o.Y)).ToList()
        estrsO = estrsO.OrderBy(Function(o) Math.Abs(gobj.X - o.X) + Math.Abs(gobj.Y - o.Y)).ToList()
        uosI = uosI.OrderBy(Function(o) Math.Abs(gobj.X - o.X) + Math.Abs(gobj.Y - o.Y)).ToList()
        uosO = uosO.OrderBy(Function(o) Math.Abs(gobj.X - o.X) + Math.Abs(gobj.Y - o.Y)).ToList()

        Select Case obj.GraphicObject.ObjectType
            Case ObjectType.MaterialStream
                If scheme = 2 And uosI.Count > 0 Then
                    Try : Flowsheet.ConnectObject(uosI(0), gobj) : Catch : End Try
                End If
                If scheme = 2 And uosO.Count > 0 Then
                    Try : Flowsheet.ConnectObject(gobj, uosO(0)) : Catch : End Try
                End If
            Case ObjectType.EnergyStream
                If scheme = 2 And uosI.Count > 0 Then
                    Try : Flowsheet.ConnectObject(uosI(0), gobj) : Catch : End Try
                End If
                If scheme = 2 And uosO.Count > 0 Then
                    Try : Flowsheet.ConnectObject(gobj, uosO(0)) : Catch : End Try
                End If
            Case ObjectType.AbsorptionColumn
                If scheme = 2 And mstrs.Count > 0 Then
                    For i = 0 To mstrs.Count
                        If i > 3 Then Exit For
                        If i < 2 Then
                            Try : Flowsheet.ConnectObjects(mstrs(i), gobj, 0, i) : Catch : End Try
                        Else
                            Try : Flowsheet.ConnectObjects(gobj, mstrs(i - 2), i - 2, 0) : Catch : End Try
                        End If
                    Next
                Else
                    Dim m0 = AddObjectToSurface(ObjectType.MaterialStream, x - 60, y, False,,,, False)
                    Dim m1 = AddObjectToSurface(ObjectType.MaterialStream, x - 60, y + h - 40, False,,,, False)
                    Dim m2 = AddObjectToSurface(ObjectType.MaterialStream, x + w + 60, y, False,,,, False)
                    Dim m3 = AddObjectToSurface(ObjectType.MaterialStream, x + w + 60, y + h - 40, False,,,, False)
                    Try : Flowsheet.ConnectObjects(Flowsheet.SimulationObjects(m0).GraphicObject, gobj, 0, 0) : Catch : Flowsheet.DeleteObject(m0, False) : End Try
                    Try : Flowsheet.ConnectObjects(Flowsheet.SimulationObjects(m1).GraphicObject, gobj, 0, 1) : Catch : Flowsheet.DeleteObject(m1, False) : End Try
                    Try : Flowsheet.ConnectObjects(gobj, Flowsheet.SimulationObjects(m2).GraphicObject, 0, 0) : Catch : Flowsheet.DeleteObject(m2, False) : End Try
                    Try : Flowsheet.ConnectObjects(gobj, Flowsheet.SimulationObjects(m3).GraphicObject, 1, 0) : Catch : Flowsheet.DeleteObject(m3, False) : End Try
                End If
            Case ObjectType.Compressor
                If scheme = 2 And mstrs.Count > 0 Then
                    If mstrsI.Count > 0 Then
                        Try : Flowsheet.ConnectObjects(mstrsI(0), gobj, 0, 0) : Catch : End Try
                    End If
                    If mstrsO.Count > 0 Then
                        Try : Flowsheet.ConnectObjects(gobj, mstrsO(0), 0, 0) : Catch : End Try
                    End If
                Else
                    Dim m0 = AddObjectToSurface(ObjectType.MaterialStream, x - 60, y, False,,,, False)
                    Dim m1 = AddObjectToSurface(ObjectType.MaterialStream, x + w + 60, y, False,,,, False)
                    Try : Flowsheet.ConnectObjects(Flowsheet.SimulationObjects(m0).GraphicObject, gobj, 0, 0) : Catch : Flowsheet.DeleteObject(m0, False) : End Try
                    Try : Flowsheet.ConnectObjects(gobj, Flowsheet.SimulationObjects(m1).GraphicObject, 0, 0) : Catch : Flowsheet.DeleteObject(m1, False) : End Try
                End If
                If scheme = 2 And estrsI.Count > 0 Then
                    If estrsI.Count > 0 Then
                        Try : Flowsheet.ConnectObjects(estrsI(0), gobj, 0, 0) : Catch : End Try
                    End If
                Else
                    Dim e1 = AddObjectToSurface(ObjectType.EnergyStream, x - 60, y + h + 60, False,,,, False)
                    Try : Flowsheet.ConnectObjects(Flowsheet.SimulationObjects(e1).GraphicObject, gobj, 0, 1) : Catch : Flowsheet.DeleteObject(e1, False) : End Try
                End If
            Case ObjectType.DistillationColumn
                If scheme = 2 And mstrs.Count > 0 Then
                    If mstrsI.Count > 0 Then Try : Flowsheet.ConnectObjects(mstrsI(0), gobj, 0, 0) : Catch : End Try
                    If mstrsO.Count > 0 Then
                        Try : Flowsheet.ConnectObjects(gobj, mstrsO(0), 0, 0) : Catch : End Try
                        Try : Flowsheet.ConnectObjects(gobj, mstrsO(1), 1, 0) : Catch : End Try
                    End If
                Else
                    Dim m0 = AddObjectToSurface(ObjectType.MaterialStream, x - 60, y, False,,,, False)
                    Dim m1 = AddObjectToSurface(ObjectType.MaterialStream, x + w + 60, y - 40, False,,,, False)
                    Dim m2 = AddObjectToSurface(ObjectType.MaterialStream, x + w + 60, y + h + 40, False,,,, False)
                    Try : Flowsheet.ConnectObjects(Flowsheet.SimulationObjects(m0).GraphicObject, gobj, 0, 0) : Catch : Flowsheet.DeleteObject(m0, False) : End Try
                    Try : Flowsheet.ConnectObjects(gobj, Flowsheet.SimulationObjects(m1).GraphicObject, 0, 0) : Catch : Flowsheet.DeleteObject(m1, False) : End Try
                    Try : Flowsheet.ConnectObjects(gobj, Flowsheet.SimulationObjects(m2).GraphicObject, 1, 0) : Catch : Flowsheet.DeleteObject(m2, False) : End Try
                End If
                If scheme = 2 And estrs.Count > 0 Then
                    If estrsI.Count > 0 Then Try : Flowsheet.ConnectObjects(estrsI(0), gobj, 0, 10) : Catch : End Try
                    If estrsO.Count > 0 Then Try : Flowsheet.ConnectObjects(gobj, estrsO(0), 10, 0) : Catch : End Try
                Else
                    Dim e1 = AddObjectToSurface(ObjectType.EnergyStream, x + w + 60, y + 40, False,,,, False)
                    Dim e2 = AddObjectToSurface(ObjectType.EnergyStream, x + w + 60, y + h - 40, False,,,, False)
                    Try : Flowsheet.ConnectObjects(Flowsheet.SimulationObjects(e2).GraphicObject, gobj, 0, 10) : Catch : Flowsheet.DeleteObject(e1, False) : End Try
                    Try : Flowsheet.ConnectObjects(gobj, Flowsheet.SimulationObjects(e1).GraphicObject, 10, 0) : Catch : Flowsheet.DeleteObject(e2, False) : End Try
                End If
            Case ObjectType.Expander, ObjectType.Cooler
                If scheme = 2 And mstrs.Count > 0 Then
                    If mstrsI.Count > 0 Then Try : Flowsheet.ConnectObjects(mstrsI(0), gobj, 0, 0) : Catch : End Try
                    If mstrsO.Count > 0 Then Try : Flowsheet.ConnectObjects(gobj, mstrsO(0), 0, 0) : Catch : End Try
                Else
                    Dim m0 = AddObjectToSurface(ObjectType.MaterialStream, x - 60, y, False,,,, False)
                    Dim m1 = AddObjectToSurface(ObjectType.MaterialStream, x + w + 60, y, False,,,, False)
                    Try : Flowsheet.ConnectObjects(Flowsheet.SimulationObjects(m0).GraphicObject, gobj, 0, 0) : Catch : Flowsheet.DeleteObject(m0, False) : End Try
                    Try : Flowsheet.ConnectObjects(gobj, Flowsheet.SimulationObjects(m1).GraphicObject, 0, 0) : Catch : Flowsheet.DeleteObject(m1, False) : End Try
                End If
                If scheme = 2 And estrs.Count > 0 Then
                    If estrsO.Count > 0 Then Flowsheet.ConnectObjects(gobj, estrsO(0), 0, 0)
                Else
                    Dim e1 = AddObjectToSurface(ObjectType.EnergyStream, x + w + 60, y + h + 60, False,,,, False)
                    Try : Flowsheet.ConnectObjects(gobj, Flowsheet.SimulationObjects(e1).GraphicObject, 0, 0) : Catch : Flowsheet.DeleteObject(e1, False) : End Try
                End If
            Case ObjectType.Heater
                If scheme = 2 And mstrs.Count > 0 Then
                    If mstrsI.Count > 0 Then Try : Flowsheet.ConnectObjects(mstrsI(0), gobj, 0, 0) : Catch : End Try
                    If mstrsO.Count > 0 Then Try : Flowsheet.ConnectObjects(gobj, mstrsO(0), 0, 0) : Catch : End Try
                Else
                    Dim m0 = AddObjectToSurface(ObjectType.MaterialStream, x - 60, y, False,,,, False)
                    Dim m1 = AddObjectToSurface(ObjectType.MaterialStream, x + w + 60, y, False,,,, False)
                    Try : Flowsheet.ConnectObjects(Flowsheet.SimulationObjects(m0).GraphicObject, gobj, 0, 0) : Catch : Flowsheet.DeleteObject(m0, False) : End Try
                    Try : Flowsheet.ConnectObjects(gobj, Flowsheet.SimulationObjects(m1).GraphicObject, 0, 0) : Catch : Flowsheet.DeleteObject(m1, False) : End Try
                End If
                If scheme = 2 And estrs.Count > 0 Then
                    If estrsI.Count > 0 Then Try : Flowsheet.ConnectObjects(estrsI(0), gobj, 0, 1) : Catch : End Try
                Else
                    Dim e1 = AddObjectToSurface(ObjectType.EnergyStream, x - 60, y + h + 60, False,,,, False)
                    Try : Flowsheet.ConnectObjects(Flowsheet.SimulationObjects(e1).GraphicObject, gobj, 0, 1) : Catch : Flowsheet.DeleteObject(e1, False) : End Try
                End If
            Case ObjectType.HeatExchanger
                If scheme = 2 And mstrs.Count > 0 Then
                    If mstrsI.Count > 0 Then
                        Try : Flowsheet.ConnectObjects(mstrsI(0), gobj, 0, 0) : Catch : End Try
                        Try : Flowsheet.ConnectObjects(mstrsI(1), gobj, 0, 1) : Catch : End Try
                    End If
                    If mstrsO.Count > 0 Then
                        Try : Flowsheet.ConnectObjects(gobj, mstrsO(0), 0, 0) : Catch : End Try
                        Try : Flowsheet.ConnectObjects(gobj, mstrsO(1), 1, 0) : Catch : End Try
                    End If
                Else
                    Dim m0 = AddObjectToSurface(ObjectType.MaterialStream, x - 60, y - 40, False,,,, False)
                    Dim m1 = AddObjectToSurface(ObjectType.MaterialStream, x - 60, y + 40, False,,,, False)
                    Dim m2 = AddObjectToSurface(ObjectType.MaterialStream, x + w + 60, y - 40, False,,,, False)
                    Dim m3 = AddObjectToSurface(ObjectType.MaterialStream, x + w + 60, y + 40, False,,,, False)
                    Try : Flowsheet.ConnectObjects(Flowsheet.SimulationObjects(m0).GraphicObject, gobj, 0, 0) : Catch : Flowsheet.DeleteObject(m0, False) : End Try
                    Try : Flowsheet.ConnectObjects(Flowsheet.SimulationObjects(m1).GraphicObject, gobj, 0, 1) : Catch : Flowsheet.DeleteObject(m1, False) : End Try
                    Try : Flowsheet.ConnectObjects(gobj, Flowsheet.SimulationObjects(m2).GraphicObject, 0, 0) : Catch : Flowsheet.DeleteObject(m2, False) : End Try
                    Try : Flowsheet.ConnectObjects(gobj, Flowsheet.SimulationObjects(m3).GraphicObject, 1, 0) : Catch : Flowsheet.DeleteObject(m3, False) : End Try
                End If
            Case ObjectType.NodeIn
                If scheme = 2 And mstrs.Count > 0 Then
                    If mstrsI.Count > 0 Then
                        Try : Flowsheet.ConnectObjects(mstrsI(0), gobj, 0, 0) : Catch : End Try
                        Try : Flowsheet.ConnectObjects(mstrsI(1), gobj, 0, 1) : Catch : End Try
                        Try : Flowsheet.ConnectObjects(mstrsI(2), gobj, 0, 2) : Catch : End Try
                    End If
                    If mstrsO.Count > 0 Then
                        Try : Flowsheet.ConnectObjects(gobj, mstrsO(0), 0, 0) : Catch : End Try
                    End If
                Else
                    Dim m0 = AddObjectToSurface(ObjectType.MaterialStream, x - 60, y, False,,,, False)
                    Dim m1 = AddObjectToSurface(ObjectType.MaterialStream, x - 60, y + 40, False,,,, False)
                    Dim m2 = AddObjectToSurface(ObjectType.MaterialStream, x + w + 60, y + 40, False,,,, False)
                    Try : Flowsheet.ConnectObjects(Flowsheet.SimulationObjects(m0).GraphicObject, gobj, 0, 0) : Catch : Flowsheet.DeleteObject(m0, False) : End Try
                    Try : Flowsheet.ConnectObjects(Flowsheet.SimulationObjects(m1).GraphicObject, gobj, 0, 1) : Catch : Flowsheet.DeleteObject(m1, False) : End Try
                    Try : Flowsheet.ConnectObjects(gobj, Flowsheet.SimulationObjects(m2).GraphicObject, 0, 0) : Catch : Flowsheet.DeleteObject(m2, False) : End Try
                End If
            Case ObjectType.NodeOut
                If scheme = 2 And mstrs.Count > 0 Then
                    If mstrsI.Count > 0 Then
                        Try : Flowsheet.ConnectObjects(mstrsI(0), gobj, 0, 0) : Catch : End Try
                    End If
                    If mstrsO.Count > 0 Then
                        Try : Flowsheet.ConnectObjects(gobj, mstrsO(0), 0, 0) : Catch : End Try
                        Try : Flowsheet.ConnectObjects(gobj, mstrsO(1), 1, 0) : Catch : End Try
                        Try : Flowsheet.ConnectObjects(gobj, mstrsO(2), 2, 0) : Catch : End Try
                    End If
                Else
                    Dim m0 = AddObjectToSurface(ObjectType.MaterialStream, x - 60, y, False,,,, False)
                    Dim m1 = AddObjectToSurface(ObjectType.MaterialStream, x + w + 60, y, False,,,, False)
                    Dim m2 = AddObjectToSurface(ObjectType.MaterialStream, x + w + 60, y + 40, False,,,, False)
                    Try : Flowsheet.ConnectObjects(Flowsheet.SimulationObjects(m0).GraphicObject, gobj, 0, 0) : Catch : Flowsheet.DeleteObject(m0, False) : End Try
                    Try : Flowsheet.ConnectObjects(gobj, Flowsheet.SimulationObjects(m1).GraphicObject, 0, 0) : Catch : Flowsheet.DeleteObject(m1, False) : End Try
                    Try : Flowsheet.ConnectObjects(gobj, Flowsheet.SimulationObjects(m2).GraphicObject, 1, 0) : Catch : Flowsheet.DeleteObject(m2, False) : End Try
                End If
            Case ObjectType.Pipe
                If scheme = 2 And mstrs.Count > 0 Then
                    If mstrsI.Count > 0 Then Try : Flowsheet.ConnectObjects(mstrsI(0), gobj, 0, 0) : Catch : End Try
                    If mstrsO.Count > 0 Then Try : Flowsheet.ConnectObjects(gobj, mstrsO(0), 0, 0) : Catch : End Try
                Else
                    Dim m0 = AddObjectToSurface(ObjectType.MaterialStream, x - 60, y, False,,,, False)
                    Dim m1 = AddObjectToSurface(ObjectType.MaterialStream, x + w + 60, y, False,,,, False)
                    Try : Flowsheet.ConnectObjects(Flowsheet.SimulationObjects(m0).GraphicObject, gobj, 0, 0) : Catch : Flowsheet.DeleteObject(m0, False) : End Try
                    Try : Flowsheet.ConnectObjects(gobj, Flowsheet.SimulationObjects(m1).GraphicObject, 0, 0) : Catch : Flowsheet.DeleteObject(m1, False) : End Try
                End If
                If scheme = 2 And estrs.Count > 0 Then
                    If estrsO.Count > 0 Then Try : Flowsheet.ConnectObjects(gobj, estrsO(0), 0, 0) : Catch : End Try
                Else
                    Dim e1 = AddObjectToSurface(ObjectType.EnergyStream, x + w + 60, y + h + 60, False,,,, False)
                    Try : Flowsheet.ConnectObjects(gobj, Flowsheet.SimulationObjects(e1).GraphicObject, 0, 0) : Catch : Flowsheet.DeleteObject(e1, False) : End Try
                End If
            Case ObjectType.Pump
                If scheme = 2 And mstrs.Count > 0 Then
                    If mstrsI.Count > 0 Then Try : Flowsheet.ConnectObjects(mstrsI(0), gobj, 0, 0) : Catch : End Try
                    If mstrsO.Count > 0 Then Try : Flowsheet.ConnectObjects(gobj, mstrsO(0), 0, 0) : Catch : End Try
                Else
                    Dim m0 = AddObjectToSurface(ObjectType.MaterialStream, x - 60, y, False,,,, False)
                    Dim m1 = AddObjectToSurface(ObjectType.MaterialStream, x + w + 60, y, False,,,, False)
                    Try : Flowsheet.ConnectObjects(Flowsheet.SimulationObjects(m0).GraphicObject, gobj, 0, 0) : Catch : Flowsheet.DeleteObject(m0, False) : End Try
                    Try : Flowsheet.ConnectObjects(gobj, Flowsheet.SimulationObjects(m1).GraphicObject, 0, 0) : Catch : Flowsheet.DeleteObject(m1, False) : End Try
                End If
                If scheme = 2 And estrs.Count > 0 Then
                    If estrsI.Count > 0 Then Try : Flowsheet.ConnectObjects(estrsI(0), gobj, 0, 1) : Catch : End Try
                Else
                    Dim e1 = AddObjectToSurface(ObjectType.EnergyStream, x - 60, y + h + 60, False,,,, False)
                    Try : Flowsheet.ConnectObjects(Flowsheet.SimulationObjects(e1).GraphicObject, gobj, 0, 1) : Catch : Flowsheet.DeleteObject(e1, False) : End Try
                End If
            Case ObjectType.RCT_Conversion
                If scheme = 2 And mstrs.Count > 0 Then
                    If mstrsI.Count > 0 Then Try : Flowsheet.ConnectObjects(mstrsI(0), gobj, 0, 0) : Catch : End Try
                    If mstrsO.Count > 0 Then Try : Flowsheet.ConnectObjects(gobj, mstrsO(0), 0, 0) : Catch : End Try
                Else
                    Dim m0 = AddObjectToSurface(ObjectType.MaterialStream, x - 60, y, False,,,, False)
                    Dim m1 = AddObjectToSurface(ObjectType.MaterialStream, x + w + 60, y - 40, False,,,, False)
                    Dim m2 = AddObjectToSurface(ObjectType.MaterialStream, x + w + 60, y + h + 40, False,,,, False)
                    Try : Flowsheet.ConnectObjects(Flowsheet.SimulationObjects(m0).GraphicObject, gobj, 0, 0) : Catch : Flowsheet.DeleteObject(m0, False) : End Try
                    Try : Flowsheet.ConnectObjects(gobj, Flowsheet.SimulationObjects(m1).GraphicObject, 0, 0) : Catch : Flowsheet.DeleteObject(m1, False) : End Try
                    Try : Flowsheet.ConnectObjects(gobj, Flowsheet.SimulationObjects(m2).GraphicObject, 1, 0) : Catch : Flowsheet.DeleteObject(m2, False) : End Try
                End If
                If scheme = 2 And estrs.Count > 0 Then
                    If estrsO.Count > 0 Then Flowsheet.ConnectObjects(gobj, estrsO(0), 2, 0)
                Else
                    Dim e1 = AddObjectToSurface(ObjectType.EnergyStream, x + w + 60, y + h + 60, False,,,, False)
                    Try : Flowsheet.ConnectObjects(gobj, Flowsheet.SimulationObjects(e1).GraphicObject, 2, 0) : Catch : Flowsheet.DeleteObject(e1, False) : End Try
                End If
            Case ObjectType.RCT_CSTR, ObjectType.RCT_PFR
                If scheme = 2 And mstrs.Count > 0 Then
                    If mstrsI.Count > 0 Then Try : Flowsheet.ConnectObjects(mstrsI(0), gobj, 0, 0) : Catch : End Try
                    If mstrsO.Count > 0 Then Try : Flowsheet.ConnectObjects(gobj, mstrsO(0), 0, 0) : Catch : End Try
                Else
                    Dim m0 = AddObjectToSurface(ObjectType.MaterialStream, x - 60, y, False,,,, False)
                    Dim m1 = AddObjectToSurface(ObjectType.MaterialStream, x + w + 60, y, False,,,, False)
                    Try : Flowsheet.ConnectObjects(Flowsheet.SimulationObjects(m0).GraphicObject, gobj, 0, 0) : Catch : Flowsheet.DeleteObject(m0, False) : End Try
                    Try : Flowsheet.ConnectObjects(gobj, Flowsheet.SimulationObjects(m1).GraphicObject, 0, 0) : Catch : Flowsheet.DeleteObject(m1, False) : End Try
                End If
                If scheme = 2 And estrs.Count > 0 Then
                    If estrsI.Count > 0 Then Try : Flowsheet.ConnectObjects(estrsI(0), gobj, 0, 1) : Catch : End Try
                Else
                    Dim e1 = AddObjectToSurface(ObjectType.EnergyStream, x - 60, y + h + 60, False,,,, False)
                    Try : Flowsheet.ConnectObjects(Flowsheet.SimulationObjects(e1).GraphicObject, gobj, 0, 1) : Catch : Flowsheet.DeleteObject(e1, False) : End Try
                End If
            Case ObjectType.RCT_Equilibrium, ObjectType.RCT_Gibbs
                If scheme = 2 And mstrs.Count > 0 Then
                    If mstrsI.Count > 0 Then Try : Flowsheet.ConnectObjects(mstrsI(0), gobj, 0, 0) : Catch : End Try
                    If mstrsO.Count > 0 Then Try : Flowsheet.ConnectObjects(gobj, mstrsO(0), 0, 0) : Catch : End Try
                Else
                    Dim m0 = AddObjectToSurface(ObjectType.MaterialStream, x - 60, y, False,,,, False)
                    Dim m1 = AddObjectToSurface(ObjectType.MaterialStream, x + w + 60, y - 40, False,,,, False)
                    Dim m2 = AddObjectToSurface(ObjectType.MaterialStream, x + w + 60, y + h + 40, False,,,, False)
                    Try : Flowsheet.ConnectObjects(Flowsheet.SimulationObjects(m0).GraphicObject, gobj, 0, 0) : Catch : Flowsheet.DeleteObject(m0, False) : End Try
                    Try : Flowsheet.ConnectObjects(gobj, Flowsheet.SimulationObjects(m1).GraphicObject, 0, 0) : Catch : Flowsheet.DeleteObject(m1, False) : End Try
                    Try : Flowsheet.ConnectObjects(gobj, Flowsheet.SimulationObjects(m2).GraphicObject, 1, 0) : Catch : Flowsheet.DeleteObject(m2, False) : End Try
                End If
                If scheme = 2 And estrs.Count > 0 Then
                    If estrsI.Count > 0 Then Try : Flowsheet.ConnectObjects(estrsI(0), gobj, 0, 1) : Catch : End Try
                Else
                    Dim e1 = AddObjectToSurface(ObjectType.EnergyStream, x - 60, y + h + 60, False,,,, False)
                    Try : Flowsheet.ConnectObjects(Flowsheet.SimulationObjects(e1).GraphicObject, gobj, 0, 1) : Catch : Flowsheet.DeleteObject(e1, False) : End Try
                End If
            Case ObjectType.ComponentSeparator
                If scheme = 2 And mstrs.Count > 0 Then
                    If mstrsI.Count > 0 Then Try : Flowsheet.ConnectObjects(mstrsI(0), gobj, 0, 0) : Catch : End Try
                    If mstrsO.Count > 0 Then Try : Flowsheet.ConnectObjects(gobj, mstrsO(0), 0, 0) : Catch : End Try
                Else
                    Dim m0 = AddObjectToSurface(ObjectType.MaterialStream, x - 60, y, False,,,, False)
                    Dim m1 = AddObjectToSurface(ObjectType.MaterialStream, x + w + 60, y - 40, False,,,, False)
                    Dim m2 = AddObjectToSurface(ObjectType.MaterialStream, x + w + 60, y + h + 40, False,,,, False)
                    Try : Flowsheet.ConnectObjects(Flowsheet.SimulationObjects(m0).GraphicObject, gobj, 0, 0) : Catch : Flowsheet.DeleteObject(m0, False) : End Try
                    Try : Flowsheet.ConnectObjects(gobj, Flowsheet.SimulationObjects(m1).GraphicObject, 0, 0) : Catch : Flowsheet.DeleteObject(m1, False) : End Try
                    Try : Flowsheet.ConnectObjects(gobj, Flowsheet.SimulationObjects(m2).GraphicObject, 1, 0) : Catch : Flowsheet.DeleteObject(m2, False) : End Try
                End If
                If scheme = 2 And estrs.Count > 0 Then
                    If estrsO.Count > 0 Then Try : Flowsheet.ConnectObjects(gobj, estrsO(0), 0, 0) : Catch : End Try
                Else
                    Dim e1 = AddObjectToSurface(ObjectType.EnergyStream, x + 60, y + h + 60, False,,,, False)
                    Try : Flowsheet.ConnectObjects(gobj, Flowsheet.SimulationObjects(e1).GraphicObject, 0, 0) : Catch : Flowsheet.DeleteObject(e1, False) : End Try
                End If
            Case ObjectType.ShortcutColumn
                If scheme = 2 And mstrs.Count > 0 Then
                    If mstrsI.Count > 0 Then Try : Flowsheet.ConnectObjects(mstrsI(0), gobj, 0, 0) : Catch : End Try
                    If mstrsO.Count > 0 Then
                        Try : Flowsheet.ConnectObjects(gobj, mstrsO(0), 0, 0) : Catch : End Try
                        Try : Flowsheet.ConnectObjects(gobj, mstrsO(1), 1, 0) : Catch : End Try
                    End If
                Else
                    Dim m0 = AddObjectToSurface(ObjectType.MaterialStream, x - 60, y, False,,,, False)
                    Dim m1 = AddObjectToSurface(ObjectType.MaterialStream, x + w + 60, y - 40, False,,,, False)
                    Dim m2 = AddObjectToSurface(ObjectType.MaterialStream, x + w + 60, y + h + 40, False,,,, False)
                    Try : Flowsheet.ConnectObjects(Flowsheet.SimulationObjects(m0).GraphicObject, gobj, 0, 0) : Catch : Flowsheet.DeleteObject(m0, False) : End Try
                    Try : Flowsheet.ConnectObjects(gobj, Flowsheet.SimulationObjects(m1).GraphicObject, 0, 0) : Catch : Flowsheet.DeleteObject(m1, False) : End Try
                    Try : Flowsheet.ConnectObjects(gobj, Flowsheet.SimulationObjects(m2).GraphicObject, 1, 0) : Catch : Flowsheet.DeleteObject(m2, False) : End Try
                End If
                If scheme = 2 And estrs.Count > 0 Then
                    If estrsI.Count > 0 Then Try : Flowsheet.ConnectObjects(estrsI(0), gobj, 0, 1) : Catch : End Try
                    If estrsO.Count > 0 Then Try : Flowsheet.ConnectObjects(gobj, estrsO(0), 0, 0) : Catch : End Try
                Else
                    Dim e1 = AddObjectToSurface(ObjectType.EnergyStream, x + w + 60, y + 40, False,,,, False)
                    Dim e2 = AddObjectToSurface(ObjectType.EnergyStream, x + w + 60, y + h - 40, False,,,, False)
                    Try : Flowsheet.ConnectObjects(Flowsheet.SimulationObjects(e2).GraphicObject, gobj, 0, 1) : Catch : Flowsheet.DeleteObject(e1, False) : End Try
                    Try : Flowsheet.ConnectObjects(gobj, Flowsheet.SimulationObjects(e1).GraphicObject, 0, 0) : Catch : Flowsheet.DeleteObject(e2, False) : End Try
                End If
            Case ObjectType.Tank, ObjectType.Valve, ObjectType.OrificePlate, ObjectType.OT_Recycle
                If scheme = 2 And mstrs.Count > 0 Then
                    If mstrsI.Count > 0 Then Try : Flowsheet.ConnectObjects(mstrsI(0), gobj, 0, 0) : Catch : End Try
                    If mstrsO.Count > 0 Then Try : Flowsheet.ConnectObjects(gobj, mstrsO(0), 0, 0) : Catch : End Try
                Else
                    Dim m0 = AddObjectToSurface(ObjectType.MaterialStream, x - 60, y, False,,,, False)
                    Dim m1 = AddObjectToSurface(ObjectType.MaterialStream, x + w + 60, y, False,,,, False)
                    Try : Flowsheet.ConnectObjects(Flowsheet.SimulationObjects(m0).GraphicObject, gobj, 0, 0) : Catch : Flowsheet.DeleteObject(m0, False) : End Try
                    Try : Flowsheet.ConnectObjects(gobj, Flowsheet.SimulationObjects(m1).GraphicObject, 0, 0) : Catch : Flowsheet.DeleteObject(m1, False) : End Try
                End If
            Case ObjectType.Vessel, ObjectType.SolidSeparator, ObjectType.Filter
                If scheme = 2 And mstrs.Count > 0 Then
                    If mstrsI.Count > 0 Then Try : Flowsheet.ConnectObjects(mstrsI(0), gobj, 0, 0) : Catch : End Try
                    If mstrsO.Count > 0 Then
                        Try : Flowsheet.ConnectObjects(gobj, mstrsO(0), 0, 0) : Catch : End Try
                        Try : Flowsheet.ConnectObjects(gobj, mstrsO(1), 1, 0) : Catch : End Try
                    End If
                Else
                    Dim m0 = AddObjectToSurface(ObjectType.MaterialStream, x - 60, y, False,,,, False)
                    Dim m1 = AddObjectToSurface(ObjectType.MaterialStream, x + w + 60, y - 40, False,,,, False)
                    Dim m2 = AddObjectToSurface(ObjectType.MaterialStream, x + w + 60, y + h + 40, False,,,, False)
                    Try : Flowsheet.ConnectObjects(Flowsheet.SimulationObjects(m0).GraphicObject, gobj, 0, 0) : Catch : Flowsheet.DeleteObject(m0, False) : End Try
                    Try : Flowsheet.ConnectObjects(gobj, Flowsheet.SimulationObjects(m1).GraphicObject, 0, 0) : Catch : Flowsheet.DeleteObject(m1, False) : End Try
                    Try : Flowsheet.ConnectObjects(gobj, Flowsheet.SimulationObjects(m2).GraphicObject, 1, 0) : Catch : Flowsheet.DeleteObject(m2, False) : End Try
                End If
        End Select

    End Sub

    Public Sub CheckTag(obj As Interfaces.IGraphicObject)

        While Flowsheet.GetFlowsheetSimulationObject(obj.Tag) IsNot Nothing
            obj.Tag = Regex.Replace(obj.Tag, "\d+", Function(m) (Integer.Parse(m.Value) + 1).ToString(New String("0", m.Value.Length)))
        End While

    End Sub

    Sub AddObject(objname As String, x As Integer, y As Integer,
                  Optional ByVal cl As Interfaces.Enums.SimulationObjectClass = SimulationObjectClass.Other,
                  Optional CreateConnected As Boolean = False)

        Dim tobj As ObjectType = ObjectType.Nenhum

        Dim chemsep As Boolean = False

        If objname = "CapeOpenUO" And cl = SimulationObjectClass.Columns Then chemsep = True

        Select Case objname
            Case "Adjust"
                tobj = ObjectType.OT_Adjust
            Case "Spec"
                tobj = ObjectType.OT_Spec
            Case "Recycle"
                tobj = ObjectType.OT_Recycle
            Case "EnergyRecycle"
                tobj = ObjectType.OT_EnergyRecycle
            Case "Mixer"
                tobj = ObjectType.NodeIn
            Case "Splitter"
                tobj = ObjectType.NodeOut
            Case "Pump"
                tobj = ObjectType.Pump
            Case "Tank"
                tobj = ObjectType.Tank
            Case "Vessel"
                tobj = ObjectType.Vessel
            Case "MaterialStream"
                tobj = ObjectType.MaterialStream
            Case "EnergyStream"
                tobj = ObjectType.EnergyStream
            Case "Compressor"
                tobj = ObjectType.Compressor
            Case "Expander"
                tobj = ObjectType.Expander
            Case "Cooler"
                tobj = ObjectType.Cooler
            Case "Heater"
                tobj = ObjectType.Heater
            Case "Pipe"
                tobj = ObjectType.Pipe
            Case "Valve"
                tobj = ObjectType.Valve
            Case "Reactor_Conversion"
                tobj = ObjectType.RCT_Conversion
            Case "Reactor_Equilibrium"
                tobj = ObjectType.RCT_Equilibrium
            Case "Reactor_Gibbs"
                tobj = ObjectType.RCT_Gibbs
            Case "Reactor_CSTR"
                tobj = ObjectType.RCT_CSTR
            Case "Reactor_PFR"
                tobj = ObjectType.RCT_PFR
            Case "HeatExchanger"
                tobj = ObjectType.HeatExchanger
            Case "ShortcutColumn"
                tobj = ObjectType.ShortcutColumn
            Case "DistillationColumn"
                tobj = ObjectType.DistillationColumn
            Case "AbsorptionColumn"
                tobj = ObjectType.AbsorptionColumn
            Case "ReboiledAbsorber"
                tobj = ObjectType.ReboiledAbsorber
            Case "RefluxedAbsorber"
                tobj = ObjectType.RefluxedAbsorber
            Case "ComponentSeparator"
                tobj = ObjectType.ComponentSeparator
            Case "OrificePlate"
                tobj = ObjectType.OrificePlate
            Case "CustomUO"
                tobj = ObjectType.CustomUO
            Case "ExcelUO"
                tobj = ObjectType.ExcelUO
            Case "CapeOpenUO"
                tobj = ObjectType.CapeOpenUO
            Case "SolidsSeparator"
                tobj = ObjectType.SolidSeparator
            Case "Filter"
                tobj = ObjectType.Filter
            Case "Flowsheet"
                tobj = ObjectType.FlowsheetUO
            Case "AnalogGauge"
                tobj = ObjectType.AnalogGauge
            Case "DigitalGauge"
                tobj = ObjectType.DigitalGauge
            Case "LevelGauge"
                tobj = ObjectType.LevelGauge
            Case "PIDController"
                tobj = ObjectType.Controller_PID
            Case "PythonController"
                tobj = ObjectType.Controller_Python
            Case "Input"
                tobj = ObjectType.Input
            Case "Switch"
                tobj = ObjectType.Switch
        End Select

        AddObjectToSurface(tobj, x, y, chemsep,,,, CreateConnected)

    End Sub

    Public calcstart As Date

    Public Sub RecalcularToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles RecalcularToolStripMenuItem.Click

        Me.Flowsheet.tsmiRecalc_Click(sender, e)

    End Sub

    Private Sub CopiarDadosParaareaDeTransferenciaToolStripMenuItem_Click(sender As System.Object, e As System.EventArgs) Handles CopiarDadosParaareaDeTransferenciaToolStripMenuItem.Click

        Me.Flowsheet.tsmiExportData_Click(sender, e)

    End Sub

    Private Sub ExibirTudoToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles ExibirTudoToolStripMenuItem.Click
        FlowsheetSurface.ZoomAll(SplitContainerHorizontal.Panel1.Width, SplitContainerHorizontal.Panel1.Height)
        FlowsheetSurface.ZoomAll(SplitContainerHorizontal.Panel1.Width, SplitContainerHorizontal.Panel1.Height)
        FlowsheetSurface.Center(SplitContainerHorizontal.Panel1.Width, SplitContainerHorizontal.Panel1.Height)
        Me.Invalidate()
    End Sub

    Private Sub ZoomPadrao100ToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles ZoomPadrao100ToolStripMenuItem.Click
        FlowsheetSurface.Zoom = 1
        FlowsheetSurface.Center(SplitContainerHorizontal.Panel1.Width, SplitContainerHorizontal.Panel1.Height)
        Me.Invalidate()
    End Sub

    Private Sub CentralizarToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles CentralizarToolStripMenuItem.Click
        FlowsheetSurface.Center(SplitContainerHorizontal.Panel1.Width, SplitContainerHorizontal.Panel1.Height)
        Me.Invalidate()
    End Sub

    Private Sub FloatToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles FloatToolStripMenuItem.Click, DocumentToolStripMenuItem.Click,
                                                                            DockLeftToolStripMenuItem.Click, DockLeftAutoHideToolStripMenuItem.Click,
                                                                            DockRightAutoHideToolStripMenuItem.Click, DockRightToolStripMenuItem.Click,
                                                                            DockTopAutoHideToolStripMenuItem.Click, DockTopToolStripMenuItem.Click,
                                                                            DockBottomAutoHideToolStripMenuItem.Click, DockBottomToolStripMenuItem.Click

        For Each ts As ToolStripMenuItem In dckMenu.Items
            ts.Checked = False
        Next

        sender.Checked = True

        Select Case sender.Name
            Case "FloatToolStripMenuItem"
                Me.DockState = WeifenLuo.WinFormsUI.Docking.DockState.Float
            Case "DocumentToolStripMenuItem"
                Me.DockState = WeifenLuo.WinFormsUI.Docking.DockState.Document
            Case "DockLeftToolStripMenuItem"
                Me.DockState = WeifenLuo.WinFormsUI.Docking.DockState.DockLeft
            Case "DockLeftAutoHideToolStripMenuItem"
                Me.DockState = WeifenLuo.WinFormsUI.Docking.DockState.DockLeftAutoHide
            Case "DockRightAutoHideToolStripMenuItem"
                Me.DockState = WeifenLuo.WinFormsUI.Docking.DockState.DockRightAutoHide
            Case "DockRightToolStripMenuItem"
                Me.DockState = WeifenLuo.WinFormsUI.Docking.DockState.DockRight
            Case "DockBottomAutoHideToolStripMenuItem"
                Me.DockState = WeifenLuo.WinFormsUI.Docking.DockState.DockBottomAutoHide
            Case "DockBottomToolStripMenuItem"
                Me.DockState = WeifenLuo.WinFormsUI.Docking.DockState.DockBottom
            Case "DockTopAutoHideToolStripMenuItem"
                Me.DockState = WeifenLuo.WinFormsUI.Docking.DockState.DockTopAutoHide
            Case "DockTopToolStripMenuItem"
                Me.DockState = WeifenLuo.WinFormsUI.Docking.DockState.DockTop
            Case "HiddenToolStripMenuItem"
                Me.DockState = WeifenLuo.WinFormsUI.Docking.DockState.Hidden
        End Select

    End Sub

    Private Sub DepurarObjetoToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles DepurarObjetoToolStripMenuItem.Click

        If Not FlowsheetSurface.SelectedObject Is Nothing Then
            If Flowsheet.Collections.FlowsheetObjectCollection.ContainsKey(Flowsheet.FormSurface.FlowsheetSurface.SelectedObject.Name) Then
                Dim myobj As SharedClasses.UnitOperations.BaseClass = Flowsheet.Collections.FlowsheetObjectCollection(Flowsheet.FormSurface.FlowsheetSurface.SelectedObject.Name)

                FormMain.AnalyticsProvider?.RegisterEvent("Debugging Object", myobj.GetDisplayName(), Nothing)

                Dim frm As New FormTextBox
                With frm
                    .TextBox1.Text = "Please wait, debugging object..."
                    .TextBox1.SelectionStart = 0
                    .TextBox1.SelectionLength = 0
                    .TextBox1.ScrollBars = ScrollBars.Vertical
                    .Text = DWSIM.App.GetLocalString("Debugging") & " " & myobj.GraphicObject.Tag & "..."
                    .StartPosition = FormStartPosition.CenterScreen
                End With
                frm.Show(Me.Flowsheet)

                Task.Factory.StartNew(Function()
                                          Return myobj.GetDebugReport()
                                      End Function).ContinueWith(Sub(t)
                                                                     frm.UIThread(Sub()
                                                                                      frm.TextBox1.Text = t.Result
                                                                                      frm.TextBox1.SelectionStart = 0
                                                                                      frm.TextBox1.SelectionLength = 0
                                                                                  End Sub)
                                                                 End Sub, TaskContinuationOptions.ExecuteSynchronously)

            End If
        End If
    End Sub

    Private Sub AtivadoToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles AtivadoToolStripMenuItem.Click
        FlowsheetSurface.SelectedObject.Active = Me.AtivadoToolStripMenuItem.Checked
        Me.Flowsheet.UpdateOpenEditForms()
    End Sub

    Private Sub SplitToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles SplitToolStripMenuItem.Click

        Flowsheet.RegisterSnapshot(SnapshotType.ObjectAddedOrRemoved)
        Try
            Dim stream = FlowsheetSurface.SelectedObject
            Dim newstream = CloneObject(stream)
            newstream.CreateConnectors(1, 1)
            newstream.Status = stream.Status

            Dim objfrom As GraphicObject, fromidx As Integer
            If stream.InputConnectors(0).IsAttached Then
                objfrom = stream.InputConnectors(0).AttachedConnector.AttachedFrom
                fromidx = stream.InputConnectors(0).AttachedConnector.AttachedFromConnectorIndex
                Flowsheet.DisconnectObjects(objfrom, stream)
                Flowsheet.ConnectObject(objfrom, newstream, fromidx)
            End If

            newstream.FlippedH = stream.FlippedH

        Catch ex As Exception
        End Try

    End Sub

    Private Sub MergeStreamsToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles MergeStreamsToolStripMenuItem.Click

        Flowsheet.RegisterSnapshot(SnapshotType.ObjectAddedOrRemoved)
        Try
            Dim stream1 = FlowsheetSurface.SelectedObjects.Values.ElementAt(0)
            Dim stream2 = FlowsheetSurface.SelectedObjects.Values.ElementAt(1)

            If stream1.OutputConnectors(0).IsAttached Then
                stream1 = FlowsheetSurface.SelectedObjects.Values.ElementAt(1)
                stream2 = FlowsheetSurface.SelectedObjects.Values.ElementAt(0)
            End If

            Dim objto As GraphicObject, toidx As Integer
            If stream2.OutputConnectors(0).IsAttached Then
                objto = stream2.OutputConnectors(0).AttachedConnector.AttachedTo
                toidx = stream2.OutputConnectors(0).AttachedConnector.AttachedToConnectorIndex
                Flowsheet.DisconnectObjects(stream2, objto)
                Flowsheet.DeleteObject(stream2.Tag, False)
                Flowsheet.ConnectObjects(stream1, objto, 0, toidx)
            End If
        Catch ex As Exception
        End Try

    End Sub

    Private Sub ToolStripButton2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ToolStripButton2.Click
        If tsbControlPanelMode.Checked Then Exit Sub
        FlowsheetSurface.Zoom += 0.05
        Me.TSTBZoom.Text = Format(Flowsheet.FormSurface.FlowsheetSurface.Zoom, "#%")
        SplitContainerHorizontal.Panel1.Refresh()
    End Sub

    Private Sub ToolStripButton1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ToolStripButton1.Click
        If tsbControlPanelMode.Checked Then Exit Sub
        FlowsheetSurface.Zoom -= 0.05
        If FlowsheetSurface.Zoom < 0.05 Then FlowsheetSurface.Zoom = 0.05
        Me.TSTBZoom.Text = Format(FlowsheetSurface.Zoom, "#%")
        SplitContainerHorizontal.Panel1.Refresh()
    End Sub

    Private Sub ToolStripButton20_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ToolStripButton20.Click
        If tsbControlPanelMode.Checked Then Exit Sub
        FlowsheetSurface.ZoomAll(FControl.Width, FControl.Height)
        FlowsheetSurface.ZoomAll(FControl.Width, FControl.Height)
        FlowsheetSurface.Center(FControl.Width, FControl.Height)
        Application.DoEvents()
        Me.TSTBZoom.Text = Format(FlowsheetSurface.Zoom, "#%")
        SplitContainerHorizontal.Panel1.Refresh()
    End Sub

    Private Sub ToolStripButton3_Click(sender As System.Object, e As System.EventArgs) Handles ToolStripButton3.Click
        If tsbControlPanelMode.Checked Then Exit Sub
        FlowsheetSurface.Zoom = 1
        FlowsheetSurface.Center(FControl.Width, FControl.Height)
        Me.TSTBZoom.Text = Format(FlowsheetSurface.Zoom, "#%")
        SplitContainerHorizontal.Panel1.Refresh()
    End Sub

    Private Sub TSTBZoom_KeyPress(ByVal sender As System.Object, ByVal e As System.Windows.Forms.KeyEventArgs) Handles TSTBZoom.KeyDown
        If tsbControlPanelMode.Checked Then Exit Sub
        If e.KeyCode = Keys.Enter Then
            FlowsheetSurface.Zoom = Convert.ToInt32(Me.TSTBZoom.Text.Replace("%", "")) / 100
            Me.TSTBZoom.Text = Format(FlowsheetSurface.Zoom, "#%")
            SplitContainerHorizontal.Panel1.Refresh()
        End If
    End Sub

    Private Sub ToolStripButton6_Click_1(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles tsbConfigPage.Click
        pageSetup.ShowDialog()
    End Sub

    Private Sub ToolStripButton10_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles tsbPrint.Click
        PreviewDialog.ShowDialog()
    End Sub

    Private Sub ToolStripButton11_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles tsbConfigPrinter.Click
        setupPrint.ShowDialog()
    End Sub

    Private Sub tsbCutObj_Click(sender As Object, e As EventArgs) Handles tsbCutObj.Click
        Flowsheet.CutObjects()
        tsbPasteObj.Enabled = True
    End Sub

    Private Sub tsbCopyObj_Click(sender As Object, e As EventArgs) Handles tsbCopyObj.Click
        Flowsheet.CopyObjects()
        tsbPasteObj.Enabled = True
    End Sub

    Private Sub tsbPasteObj_Click(sender As Object, e As EventArgs) Handles tsbPasteObj.Click
        Flowsheet.PasteObjects()
    End Sub

    Private Sub TSBTexto_Click(sender As Object, e As EventArgs) Handles TSBTexto.Click
        Flowsheet.TSBTexto_Click(sender, e)
    End Sub

    Private Sub ToolStripButton4_Click(sender As Object, e As EventArgs) Handles ToolStripButton4.Click
        Flowsheet.ToolStripButton4_Click(sender, e)
    End Sub

    Private Sub ToolStripButton6_Click(sender As Object, e As EventArgs) Handles ToolStripButton6.Click
        Flowsheet.ToolStripButton6_Click(sender, e)
    End Sub

    Private Sub ToolStripButton19_Click(sender As Object, e As EventArgs) Handles ToolStripButton19.Click
        Flowsheet.ToolStripButton19_Click(sender, e)
    End Sub

    Private Sub FlowsheetSurface_SkiaSharp_Shown(sender As Object, e As EventArgs) Handles Me.Shown

        Try
            FlowsheetSurface.ZoomAll(SplitContainerHorizontal.Panel1.Width, SplitContainerHorizontal.Panel1.Height)
            FlowsheetSurface.ShowGrid = Flowsheet.Options.FlowsheetDisplayGrid
            FlowsheetSurface.SnapToGrid = Flowsheet.Options.FlowsheetSnapToGrid
            FlowsheetSurface.MultiSelectMode = Flowsheet.Options.FlowsheetMultiSelectMode
            tsbDisplayGrid.Checked = Flowsheet.Options.FlowsheetDisplayGrid
            tsbSnapObjectsToGrid.Checked = Flowsheet.Options.FlowsheetSnapToGrid
            tsbMultiSelectMode.Checked = Flowsheet.Options.FlowsheetSnapToGrid
        Catch ex As Exception
        End Try

        TSTBZoom.Text = Format(Flowsheet.FormSurface.FlowsheetSurface.Zoom, "#%")

        SplitContainerVertical.SplitterDistance = SplitContainerVertical.Width - 350 * Settings.DpiScale

        FormMain.TranslateFormFunction?.Invoke(Me)

    End Sub

    Public Sub FlowsheetDesignSurface_SelectionChanged_New(ByVal sender As Object, ByVal e As EventArgs)

        If Not FlowsheetSurface.SelectedObject Is Nothing Then

            If Not FlowsheetSurface.SelectedObject.IsConnector Then

                If Not FlowsheetSurface.SelectedObject Is Nothing Then

                    If FlowsheetSurface.SelectedObject.IsConnector Then

                        FlowsheetSurface.SelectedObject = Nothing

                    End If

                End If

                Flowsheet.ChangeEditMenuStatus(True)

            Else

                FlowsheetSurface.SelectedObject = Nothing

            End If

        Else

            Flowsheet.ChangeEditMenuStatus(False)

        End If

    End Sub

    Private Sub ToolStripMenuItem6_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ToolStripMenuItem6.Click
        If DWSIM.App.IsRunningOnMono Then
            MessageBox.Show(DWSIM.App.GetLocalString("Unsupported_Feature"), "DWSIM", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)
        Else
            Flowsheet.RegisterSnapshot(SnapshotType.ObjectLayout)
            If FlowsheetSurface.SelectedObject.Rotation + 90 >= 360 Then
                FlowsheetSurface.SelectedObject.Rotation = Math.Truncate((FlowsheetSurface.SelectedObject.Rotation + 90) / 360)
            Else
                FlowsheetSurface.SelectedObject.Rotation = FlowsheetSurface.SelectedObject.Rotation + 90
            End If
            SplitContainerHorizontal.Panel1.Invalidate()
        End If
    End Sub

    Private Sub BToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles BToolStripMenuItem.Click
        If DWSIM.App.IsRunningOnMono Then
            MessageBox.Show(DWSIM.App.GetLocalString("Unsupported_Feature"), "DWSIM", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)
        Else
            Flowsheet.RegisterSnapshot(SnapshotType.ObjectLayout)
            If FlowsheetSurface.SelectedObject.Rotation + 180 >= 360 Then
                FlowsheetSurface.SelectedObject.Rotation = Math.Truncate((FlowsheetSurface.SelectedObject.Rotation + 180) / 360)
            Else
                FlowsheetSurface.SelectedObject.Rotation = FlowsheetSurface.SelectedObject.Rotation + 180
            End If
            SplitContainerHorizontal.Panel1.Invalidate()
        End If
    End Sub

    Private Sub ToolStripMenuItem7_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ToolStripMenuItem7.Click
        If DWSIM.App.IsRunningOnMono Then
            MessageBox.Show(DWSIM.App.GetLocalString("Unsupported_Feature"), "DWSIM", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)
        Else
            Flowsheet.RegisterSnapshot(SnapshotType.ObjectLayout)
            If FlowsheetSurface.SelectedObject.Rotation + 270 >= 360 Then
                FlowsheetSurface.SelectedObject.Rotation = Math.Truncate((FlowsheetSurface.SelectedObject.Rotation + 270) / 360)
            Else
                FlowsheetSurface.SelectedObject.Rotation = FlowsheetSurface.SelectedObject.Rotation + 270
            End If
            SplitContainerHorizontal.Panel1.Invalidate()
        End If
    End Sub

    Private Sub ToolStripMenuItem11_Click(sender As Object, e As EventArgs) Handles ToolStripMenuItem11.Click
        If DWSIM.App.IsRunningOnMono Then
            MessageBox.Show(DWSIM.App.GetLocalString("Unsupported_Feature"), "DWSIM", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)
        Else
            Flowsheet.RegisterSnapshot(SnapshotType.ObjectLayout)
            FlowsheetSurface.SelectedObject.Rotation = 0
            Me.Invalidate()
        End If
    End Sub

    Private Sub ToolStripMenuItem12_Click(sender As Object, e As EventArgs) Handles ToolStripMenuItem12.Click
        If DWSIM.App.IsRunningOnMono Then
            MessageBox.Show(DWSIM.App.GetLocalString("Unsupported_Feature"), "DWSIM", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)
        Else
            Flowsheet.RegisterSnapshot(SnapshotType.ObjectLayout)
            FlowsheetSurface.SelectedObject.Rotation = 90
            Me.Invalidate()
        End If
    End Sub

    Private Sub ToolStripMenuItem13_Click(sender As Object, e As EventArgs) Handles ToolStripMenuItem13.Click
        If DWSIM.App.IsRunningOnMono Then
            MessageBox.Show(DWSIM.App.GetLocalString("Unsupported_Feature"), "DWSIM", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)
        Else
            Flowsheet.RegisterSnapshot(SnapshotType.ObjectLayout)
            FlowsheetSurface.SelectedObject.Rotation = 180
            Me.Invalidate()
        End If
    End Sub

    Private Sub HorizontalmenteToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles HorizontalmenteToolStripMenuItem.Click
        Flowsheet.RegisterSnapshot(SnapshotType.ObjectLayout)
        If Me.HorizontalmenteToolStripMenuItem.Checked Then
            FlowsheetSurface.SelectedObject.FlippedH = True
        Else
            FlowsheetSurface.SelectedObject.FlippedH = False
        End If
        Flowsheet.UpdateInterface()
    End Sub

    Private Sub ToolStripMenuItem14_Click(sender As Object, e As EventArgs) Handles ToolStripMenuItem14.Click
        If DWSIM.App.IsRunningOnMono Then
            MessageBox.Show(DWSIM.App.GetLocalString("Unsupported_Feature"), "DWSIM", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)
        Else
            Flowsheet.RegisterSnapshot(SnapshotType.ObjectLayout)
            FlowsheetSurface.SelectedObject.Rotation = 270
            Me.Invalidate()
        End If
    End Sub

    Private Sub CopyAsImageToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles CopyAsImageToolStripMenuItem.Click
        CopyAsImage(1)
    End Sub

    Sub CopyAsImage(Zoom As Integer)

        FormMain.AnalyticsProvider?.RegisterEvent("Results Viewing", "Export PFD as Image", Nothing)

        Using bmp As New SKBitmap(SplitContainerHorizontal.Panel1.Width * Zoom, SplitContainerHorizontal.Panel1.Height * Zoom)
            Using canvas As New SKCanvas(bmp)
                canvas.Scale(Zoom)
                FlowsheetSurface.UpdateCanvas(canvas)
                Dim d = SKImage.FromBitmap(bmp).Encode(SKEncodedImageFormat.Png, 100)
                Using str As New IO.MemoryStream
                    d.SaveTo(str)
                    Clipboard.SetImage(Image.FromStream(str))
                    Flowsheet.ShowMessage("The flowsheet was copied as an image to the clipboard.", Interfaces.IFlowsheet.MessageType.Information)
                End Using
            End Using
        End Using

    End Sub

    Private Sub tsbSnapObjectsToGrid_Click(sender As Object, e As EventArgs) Handles tsbSnapObjectsToGrid.CheckedChanged
        FlowsheetSurface.SnapToGrid = tsbSnapObjectsToGrid.Checked
        Flowsheet.Options.FlowsheetSnapToGrid = tsbSnapObjectsToGrid.Checked
    End Sub

    Private Sub tsbDisplayGrid_Click(sender As Object, e As EventArgs) Handles tsbDisplayGrid.CheckedChanged
        FlowsheetSurface.ShowGrid = tsbDisplayGrid.Checked
        Flowsheet.Options.FlowsheetDisplayGrid = tsbDisplayGrid.Checked
    End Sub

    Private Sub tsbMultiSelectMode_CheckedChanged(sender As Object, e As EventArgs) Handles tsbMultiSelectMode.CheckedChanged
        FlowsheetSurface.MultiSelectMode = tsbMultiSelectMode.Checked
        tsbAlignTops.Enabled = tsbMultiSelectMode.Checked
        tsbAlignBottoms.Enabled = tsbMultiSelectMode.Checked
        tsbAlignCenters.Enabled = tsbMultiSelectMode.Checked
        tsbAlignHorizontal.Enabled = tsbMultiSelectMode.Checked
        tsbAlignVertical.Enabled = tsbMultiSelectMode.Checked
        tsbAlignLefts.Enabled = tsbMultiSelectMode.Checked
        tsbAlignMiddles.Enabled = tsbMultiSelectMode.Checked
        tsbAlignRights.Enabled = tsbMultiSelectMode.Checked
    End Sub

    Private Sub tsbAlign_Click(sender As Object, e As EventArgs) Handles tsbAlignBottoms.Click, tsbAlignCenters.Click, tsbAlignHorizontal.Click,
                                                                        tsbAlignLefts.Click, tsbAlignMiddles.Click, tsbAlignRights.Click,
                                                                        tsbAlignTops.Click, tsbAlignVertical.Click

        Dim tsb As ToolStripButton = DirectCast(sender, ToolStripButton)

        Dim direction As Drawing.SkiaSharp.GraphicsSurface.AlignDirection

        If tsb.Name.Contains("Lefts") Then
            direction = Drawing.SkiaSharp.GraphicsSurface.AlignDirection.Lefts
        ElseIf tsb.Name.Contains("Centers") Then
            direction = Drawing.SkiaSharp.GraphicsSurface.AlignDirection.Centers
        ElseIf tsb.Name.Contains("Rights") Then
            direction = Drawing.SkiaSharp.GraphicsSurface.AlignDirection.Rights
        ElseIf tsb.Name.Contains("Tops") Then
            direction = Drawing.SkiaSharp.GraphicsSurface.AlignDirection.Tops
        ElseIf tsb.Name.Contains("Middles") Then
            direction = Drawing.SkiaSharp.GraphicsSurface.AlignDirection.Middles
        ElseIf tsb.Name.Contains("Bottoms") Then
            direction = Drawing.SkiaSharp.GraphicsSurface.AlignDirection.Bottoms
        ElseIf tsb.Name.Contains("Vertical") Then
            direction = Drawing.SkiaSharp.GraphicsSurface.AlignDirection.EqualizeVertical
        ElseIf tsb.Name.Contains("Horizontal") Then
            direction = Drawing.SkiaSharp.GraphicsSurface.AlignDirection.EqualizeHorizontal
        End If

        FlowsheetSurface.AlignSelectedObjects(direction)

        Me.Refresh()

    End Sub

    Private Sub ToolStripButton12_Click(sender As Object, e As EventArgs) Handles ToolStripButton12.Click
        Flowsheet.RectangleToolStripMenuItem_Click(sender, e)
    End Sub

    Private Sub TSBtabela_Click(sender As Object, e As EventArgs) Handles TSBtabela.Click
        Flowsheet.FiguraToolStripMenuItem_Click(sender, e)
    End Sub

    Private Sub CopiarComoImagem200ToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles CopiarComoImagem200ToolStripMenuItem.Click
        CopyAsImage(2)
    End Sub

    Private Sub CopiarComoImagem300ToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles CopiarComoImagem300ToolStripMenuItem.Click
        CopyAsImage(3)
    End Sub

    Private Sub designSurfacePrintDocument_PrintPage(sender As Object, e As Printing.PrintPageEventArgs) Handles designSurfacePrintDocument.PrintPage
        Dim dpi As Double = Me.CreateGraphics.DpiX
        Dim bounds = designSurfacePrintDocument.PrinterSettings.DefaultPageSettings.Bounds
        Dim margins = designSurfacePrintDocument.PrinterSettings.DefaultPageSettings.Margins
        Dim prevzoom = FlowsheetSurface.Zoom
        Using bmp As New SKBitmap(CDbl(bounds.Width), CDbl(bounds.Height))
            Using canvas As New SKCanvas(bmp)
                FlowsheetSurface.ZoomAll(bounds.Width * 0.9, bounds.Height * 0.9)
                FlowsheetSurface.UpdateCanvas(canvas)
                Dim d = SKImage.FromBitmap(bmp).Encode(SKEncodedImageFormat.Png, 100)
                Using str As New IO.MemoryStream
                    d.SaveTo(str)
                    e.Graphics.DrawImage(New Bitmap(str), 0, 0)
                End Using
            End Using
        End Using
        FlowsheetSurface.Zoom = prevzoom
    End Sub

    Private Sub SplitAndInsertRecycleMenuItem_Click(sender As Object, e As EventArgs) Handles SplitAndInsertRecycleMenuItem.Click

        Flowsheet.RegisterSnapshot(SnapshotType.ObjectAddedOrRemoved)
        Try

            Dim stream = FlowsheetSurface.SelectedObject
            Dim newstream = CloneObject(stream)
            newstream.CreateConnectors(1, 1)
            newstream.Status = stream.Status

            Dim x = stream.X
            Dim y = stream.Y

            Dim objfrom As GraphicObject, fromidx As Integer

            If stream.InputConnectors(0).IsAttached Then
                objfrom = stream.InputConnectors(0).AttachedConnector.AttachedFrom
                fromidx = stream.InputConnectors(0).AttachedConnector.AttachedFromConnectorIndex
                Flowsheet.DisconnectObjects(objfrom, stream)
                Flowsheet.ConnectObject(objfrom, newstream, fromidx)
            End If

            Dim id As String, obj As GraphicObject

            If stream.ObjectType = ObjectType.MaterialStream Then
                id = AddObjectToSurface(ObjectType.OT_Recycle, x, y, False)
            Else
                id = AddObjectToSurface(ObjectType.OT_EnergyRecycle, x, y, False)
            End If
            obj = Flowsheet.SimulationObjects(id).GraphicObject
            obj.CreateConnectors(1, 1)
            obj.Calculated = True
            obj.Owner.Calculated = True

            Flowsheet.ConnectObjects(newstream, obj, 0, 0)
            Flowsheet.ConnectObjects(obj, stream, 0, 0)

            If Not stream.FlippedH Then
                newstream.X = x - 50
                newstream.Y = y
                stream.X = x + 50
                stream.Y = y
            Else
                newstream.X = x + 50
                newstream.Y = y
                newstream.FlippedH = True
                stream.X = x - 50
                stream.Y = y
                obj.FlippedH = True
            End If

        Catch ex As Exception

        End Try

    End Sub

    Private Sub EditarAparênciaToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles EditAppearanceToolStripMenuItem.Click

        Flowsheet.RegisterSnapshot(SnapshotType.ObjectLayout)

        If FlowsheetSurface.SelectedObject.Editor Is Nothing OrElse DirectCast(FlowsheetSurface.SelectedObject.Editor, Form).IsDisposed Then
            Dim f As New FormEditGraphicObject() With {.gobj = FlowsheetSurface.SelectedObject, .fs = FlowsheetSurface, .flowsheet = Me.Flowsheet}
            FlowsheetSurface.SelectedObject.Editor = f
            f.Show(Flowsheet.dckPanel)
        Else
            DirectCast(FlowsheetSurface.SelectedObject.Editor, Form).Activate()
        End If

    End Sub

    Private Sub tstbSearch_TextChanged(sender As Object, e As EventArgs) Handles tstbSearch.TextChanged

        Dim obj = Flowsheet.GetFlowsheetGraphicObject(tstbSearch.Text)
        If Not obj Is Nothing Then
            Try
                Dim center As Point = New Point(SplitContainerHorizontal.Panel1.Width / 2, SplitContainerHorizontal.Panel1.Height / 2)
                FlowsheetSurface.OffsetAll(center.X / FlowsheetSurface.Zoom - obj.X, center.Y / FlowsheetSurface.Zoom - obj.Y)
                FlowsheetSurface.SelectedObject = obj
                FControl.Invalidate()
                FControl.Invalidate()
            Catch ex As Exception
            End Try
        End If

    End Sub

    Private Sub SplitAndInsertValveTSMI_Click(sender As Object, e As EventArgs) Handles SplitAndInsertValveTSMI.Click

        Flowsheet.RegisterSnapshot(SnapshotType.ObjectAddedOrRemoved)
        Try

            Dim stream = FlowsheetSurface.SelectedObject
            Dim newstream = CloneObject(stream)
            newstream.CreateConnectors(1, 1)
            newstream.Status = stream.Status

            Dim istream = DirectCast(Flowsheet.SimulationObjects(stream.Name), Thermodynamics.Streams.MaterialStream)
            Dim ostream = DirectCast(Flowsheet.SimulationObjects(newstream.Name), Thermodynamics.Streams.MaterialStream)

            ostream.Phases(0).Properties.pressure = istream.Phases(0).Properties.pressure.GetValueOrDefault * 0.95

            Dim x = stream.X
            Dim y = stream.Y

            Dim objto As GraphicObject, toidx As Integer

            If stream.OutputConnectors(0).IsAttached Then
                objto = stream.OutputConnectors(0).AttachedConnector.AttachedTo
                toidx = stream.OutputConnectors(0).AttachedConnector.AttachedToConnectorIndex
                Flowsheet.DisconnectObjects(stream, objto)
                Flowsheet.ConnectObject(newstream, objto, 0, toidx)
            End If

            Dim id As String, obj As GraphicObject

            id = AddObjectToSurface(ObjectType.Valve, x, y, False)
            Dim valve = DirectCast(Flowsheet.SimulationObjects(id), Valve)
            valve.CalcMode = Valve.CalculationMode.Kv_General
            valve.Kv = 2.0
            valve.EnableOpeningKvRelationship = True
            valve.OpeningPct = 50
            valve.PercentOpeningVersusPercentKvExpression = "OP"
            valve.DynamicsOnly = True

            obj = Flowsheet.SimulationObjects(id).GraphicObject
            obj.CreateConnectors(1, 1)
            obj.Calculated = True
            obj.Owner.Calculated = True

            Flowsheet.ConnectObjects(stream, obj, 0, 0)
            Flowsheet.ConnectObjects(obj, newstream, 0, 0)

            valve.CalculateKv()

            If Not stream.FlippedH Then
                stream.X = x - 50
                stream.Y = y
                newstream.X = x + 50
                newstream.Y = y
            Else
                stream.X = x + 50
                stream.Y = y
                stream.FlippedH = True
                newstream.X = x - 50
                newstream.Y = y
                obj.FlippedH = True
            End If

        Catch ex As Exception

        End Try


    End Sub

    Private Sub btnUp_Click(sender As Object, e As EventArgs) Handles btnUp.Click

        Dim s As Size = SplitContainerHorizontal.Panel1.Size
        Dim z = FlowsheetSurface.Zoom

        FlowsheetSurface.OffsetAll(0, s.Height / z)
        FControl.Invalidate()

    End Sub

    Private Sub btnDown_Click(sender As Object, e As EventArgs) Handles btnDown.Click

        Dim s As Size = SplitContainerHorizontal.Panel1.Size
        Dim z = FlowsheetSurface.Zoom

        FlowsheetSurface.OffsetAll(0, -s.Height / z)
        FControl.Invalidate()

    End Sub

    Private Sub btnLeft_Click(sender As Object, e As EventArgs) Handles btnLeft.Click

        Dim s As Size = SplitContainerHorizontal.Panel1.Size
        Dim z = FlowsheetSurface.Zoom

        FlowsheetSurface.OffsetAll(s.Width / z, 0)
        FControl.Invalidate()

    End Sub

    Private Sub btnRight_Click(sender As Object, e As EventArgs) Handles btnRight.Click

        Dim s As Size = SplitContainerHorizontal.Panel1.Size
        Dim z = FlowsheetSurface.Zoom

        FlowsheetSurface.OffsetAll(-s.Width / z, 0)
        FControl.Invalidate()

    End Sub

    Private Sub tstbSearch_GotFocus(sender As Object, e As EventArgs) Handles tstbSearch.GotFocus

        tstbSearch.AutoCompleteCustomSource = New AutoCompleteStringCollection()
        tstbSearch.AutoCompleteCustomSource.Clear()
        tstbSearch.AutoCompleteCustomSource.AddRange(Flowsheet.GraphicObjects.Select(Function(x) x.Value.Tag).ToArray)

    End Sub

    Private Sub LayoutAutomaticoToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles LayoutAutomaticoToolStripMenuItem.Click

        Flowsheet.RegisterSnapshot(SnapshotType.ObjectLayout)

        FlowsheetSurface.AutoArrange()
        FlowsheetSurface.ZoomAll(SplitContainerHorizontal.Panel1.Width, SplitContainerHorizontal.Panel1.Height)
        FlowsheetSurface.ZoomAll(SplitContainerHorizontal.Panel1.Width, SplitContainerHorizontal.Panel1.Height)
        FControl.Invalidate()
        RestaurarLayoutToolStripMenuItem.Enabled = True

    End Sub

    Private Sub RestaurarLayoutToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles RestaurarLayoutToolStripMenuItem.Click

        FlowsheetSurface.RestoreLayout()
        FlowsheetSurface.ZoomAll(SplitContainerHorizontal.Panel1.Width, SplitContainerHorizontal.Panel1.Height)
        FlowsheetSurface.ZoomAll(SplitContainerHorizontal.Panel1.Width, SplitContainerHorizontal.Panel1.Height)
        FControl.Invalidate()
        RestaurarLayoutToolStripMenuItem.Enabled = False

    End Sub

    Private Sub ToolStripMenuItem1_Click(sender As Object, e As EventArgs)
        If SplitContainerVertical.Panel2Collapsed Then
            SplitContainerHorizontal.Panel2.Controls.Remove(SimObjPanel)
            SplitContainerVertical.Panel2.Controls.Add(SimObjPanel)
            SplitContainerVertical.SplitterDistance = (SplitContainerVertical.Width - 160) * GlobalSettings.Settings.DpiScale
            SplitContainerVertical.Panel2Collapsed = False
            SplitContainerHorizontal.Panel2Collapsed = True
        Else
            SplitContainerVertical.Panel2.Controls.Remove(SimObjPanel)
            SplitContainerHorizontal.Panel2.Controls.Add(SimObjPanel)
            SplitContainerHorizontal.SplitterDistance = (SplitContainerVertical.Height - 100) * GlobalSettings.Settings.DpiScale
            SplitContainerVertical.Panel2Collapsed = True
            SplitContainerHorizontal.Panel2Collapsed = False
        End If
    End Sub

    Private Sub ToolStripComboBox1_Click(sender As Object, e As EventArgs) Handles tsbColorTheme.SelectedIndexChanged
        Try
            Flowsheet.Options.FlowsheetColorTheme = tsbColorTheme.SelectedIndex
            FlowsheetSurface.UpdateColorTheme()
            FControl.Invalidate()
        Catch ex As Exception
        End Try
    End Sub

    Private Sub tbFontSize_TextChanged(sender As Object, e As EventArgs) Handles tbFontSize.TextChanged
        If tbFontSize.Text.IsValidDouble() And Loaded Then
            Flowsheet.FlowsheetOptions.LabelFontSize = Double.Parse(tbFontSize.Text)
            For Each obj In FlowsheetSurface.DrawingObjects
                If TypeOf obj Is ShapeGraphic Then
                    DirectCast(obj, ShapeGraphic).FontSize = Flowsheet.FlowsheetOptions.LabelFontSize
                End If
            Next
            FControl.Invalidate()
        End If
    End Sub

    Private Sub ToolStripButton5_Click(sender As Object, e As EventArgs) Handles ToolStripButton5.Click

        Dim fset As New FormDefineFonts With {.Flowsheet = Flowsheet, .FlowsheetSurface = FlowsheetSurface}
        fset.ShowDialog(Me)

    End Sub

    Private Sub tsmiInvertVertically_Click(sender As Object, e As EventArgs) Handles tsmiInvertVertically.Click
        Flowsheet.RegisterSnapshot(SnapshotType.ObjectLayout)
        If Me.tsmiInvertVertically.Checked Then
            FlowsheetSurface.SelectedObject.FlippedV = True
        Else
            FlowsheetSurface.SelectedObject.FlippedV = False
        End If
        Flowsheet.UpdateInterface()
    End Sub

    Private Sub tscbAddObjectsWithStreams_SelectedIndexChanged(sender As Object, e As EventArgs) Handles tscbAddObjectsWithStreams.SelectedIndexChanged
        Flowsheet.FlowsheetOptions.AddObjectsWithStreams = tscbAddObjectsWithStreams.SelectedIndex
    End Sub

    Private Sub ExportarParaPDFToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles ExportarParaPDFToolStripMenuItem.Click

        FormMain.AnalyticsProvider?.RegisterEvent("Results Viewing", "Export PFD to PDF", Nothing)

        Dim filePickerForm As IFilePicker = FilePickerService.GetInstance().GetFilePicker()

        Dim handler As IVirtualFile = filePickerForm.ShowSaveDialog(
            New List(Of FilePickerAllowedType) From {New FilePickerAllowedType("PDF File", "*.pdf")})

        If handler IsNot Nothing Then
            Using stream As New IO.MemoryStream()
                Dim skstream = New SKManagedWStream(stream)
                Using document = SKDocument.CreatePdf(stream)
                    Dim canvas = document.BeginPage(SplitContainerHorizontal.Panel1.Width, SplitContainerHorizontal.Panel1.Height)
                    FlowsheetSurface.UpdateCanvas(canvas)
                    document.EndPage()
                    document.Close()
                End Using
                handler.Write(stream)
            End Using
            Flowsheet.ShowMessage(String.Format("Flowsheet exported successfully to {0}.", handler.FullPath), Interfaces.IFlowsheet.MessageType.Information)
        End If

    End Sub

    Private Sub ExportarParaSVGToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles ExportarParaSVGToolStripMenuItem.Click

        FormMain.AnalyticsProvider?.RegisterEvent("Results Viewing", "Export PFD to SVG", Nothing)

        Dim filePickerForm As IFilePicker = FilePickerService.GetInstance().GetFilePicker()

        Dim handler As IVirtualFile = filePickerForm.ShowSaveDialog(
            New List(Of FilePickerAllowedType) From {New FilePickerAllowedType("SVG File", "*.svg")})

        If handler IsNot Nothing Then
            Using stream As New IO.MemoryStream()
                Dim skstream = New SKManagedWStream(stream)
                Dim writer = New SKXmlStreamWriter(skstream)
                Using canvas = SKSvgCanvas.Create(SKRect.Create(SplitContainerHorizontal.Panel1.Width, SplitContainerHorizontal.Panel1.Height), writer)
                    FlowsheetSurface.UpdateCanvas(canvas)
                End Using
                handler.Write(stream)
            End Using
            Flowsheet.ShowMessage(String.Format("Flowsheet exported successfully to {0}.", handler.FullPath), Interfaces.IFlowsheet.MessageType.Information)
        End If

    End Sub

    Private Sub tsmiHeatMap_Click(sender As Object, e As EventArgs) Handles tsmiHeatMap.Click
        Dim fhm As New FormHeatMaps()
        fhm.ShowDialog(Me)
    End Sub

    Private Sub tsmiLiveFlow_Click(sender As Object, e As EventArgs) Handles tsmiLiveFlow.Click
        Dim flf As New FormLiveFlows()
        flf.ShowDialog(Me)
    End Sub

    Private Sub btnGetLocation_Click(sender As Object, e As EventArgs) Handles btnGetLocation.Click

        Dim watcher As New GeoCoordinateWatcher()

        AddHandler watcher.PositionChanged, Sub(s2, e2)

                                                UIThread(Sub()

                                                             Dim coord = watcher.Position.Location

                                                             If Not coord.IsUnknown Then

                                                                 tbCurrentLocation.Text = coord.Latitude.ToString() + ", " + coord.Longitude.ToString()

                                                             Else

                                                                 tbCurrentLocation.Text = "N/A"

                                                             End If

                                                         End Sub)

                                            End Sub

        watcher.TryStart(False, TimeSpan.FromSeconds(10))

    End Sub

    Private Sub tbAmbientTemperature_TextChanged(sender As Object, e As EventArgs) Handles tbAmbientTemperature.TextChanged
        UpdateCurrentWeather()
    End Sub

    Private Sub cbWeather_SelectedIndexChanged(sender As Object, e As EventArgs)
        UpdateCurrentWeather()
    End Sub

    Private Sub UpdateCurrentWeather()
        If Loaded Then
            Try
                Flowsheet.Options.CurrentWeather.Temperature_C = tbAmbientTemperature.Text
            Catch ex As Exception
            End Try
            Try
                Flowsheet.Options.CurrentWeather.WindSpeed_km_h = tbWindSpeed.Text
            Catch ex As Exception
            End Try
            Try
                Flowsheet.Options.CurrentWeather.RelativeHumidity_pct = tbHumidity.Text
            Catch ex As Exception
            End Try
            Try
                Flowsheet.Options.CurrentWeather.SolarIrradiation_kWh_m2 = tbSolarIrradiation.Text
            Catch ex As Exception
            End Try
            Try
                Flowsheet.Options.CurrentWeather.AtmosphericPressure_Pa = tbAtmPress.Text * 1000.0
            Catch ex As Exception
            End Try
        End If
    End Sub

    Private Sub tbWindSpeed_TextChanged(sender As Object, e As EventArgs) Handles tbWindSpeed.TextChanged
        UpdateCurrentWeather()
    End Sub

    Private Sub tbHumidity_TextChanged(sender As Object, e As EventArgs) Handles tbHumidity.TextChanged
        UpdateCurrentWeather()
    End Sub

    Private Sub tbSolarIrradiation_TextChanged(sender As Object, e As EventArgs) Handles tbSolarIrradiation.TextChanged
        UpdateCurrentWeather()
    End Sub

    Private Sub btnGetWeather_Click(sender As Object, e As EventArgs) Handles btnGetWeather.Click

        If Flowsheet.WeatherProvider IsNot Nothing Then

            Try

                Dim data = Flowsheet.WeatherProvider.GetCurrentWeather(tbCurrentLocation.Text.Split(",")(0).Trim().ToDoubleFromInvariant(),
                                                                   tbCurrentLocation.Text.Split(",")(1).Trim().ToDoubleFromInvariant())

                Flowsheet.Options.CurrentWeather = data

                ReadWeather(data)

            Catch ex As Exception

                MessageBox.Show("Error getting current weather: " + ex.Message, "Error", MessageBoxButtons.OK, MessageBoxIcon.Error)

            End Try

        End If

    End Sub

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click

        PanelWeather.Visible = False
        My.Settings.WeatherPanelVisible = False

    End Sub

    Private Sub tsbControlPanelMode_CheckedChanged(sender As Object, e As EventArgs) Handles tsbControlPanelMode.CheckedChanged

        If tsbControlPanelMode.Checked Then
            btnDown.Visible = True
            btnUp.Visible = True
            btnLeft.Visible = True
            btnRight.Visible = True
            btnUp.Height = 24
            btnDown.Height = 24
            btnRight.Height = 24
            btnLeft.Height = 24
            FlowsheetSurface.ControlPanelMode = True
            SplitContainerHorizontal.Panel2Collapsed = True
        Else
            btnDown.Visible = False
            btnUp.Visible = False
            btnLeft.Visible = False
            btnRight.Visible = False
            btnUp.Height = 1
            btnDown.Height = 1
            btnRight.Height = 1
            btnLeft.Height = 1
            FlowsheetSurface.ControlPanelMode = False
            SplitContainerHorizontal.Panel2Collapsed = False
        End If
        FControl.Invalidate()

    End Sub

    Public Sub tsmiNaturalLayout_Click(sender As Object, e As EventArgs) Handles tsmiNaturalLayout.Click

        Flowsheet.RegisterSnapshot(SnapshotType.ObjectLayout)

        Try
            FlowsheetSurface.ApplyNaturalLayout(FlowsheetSolver.FlowsheetSolver.GetSolvingList(Flowsheet, False)(0), 75)
            FlowsheetSurface.ZoomAll(SplitContainerHorizontal.Panel1.Width, SplitContainerHorizontal.Panel1.Height)
            FlowsheetSurface.ZoomAll(SplitContainerHorizontal.Panel1.Width, SplitContainerHorizontal.Panel1.Height)
            FControl.Invalidate()
            RestaurarLayoutToolStripMenuItem.Enabled = True
        Catch ex As Exception
            MessageBox.Show(Flowsheet.GetTranslatedString1("Error applying natural layout to flowsheet: " + ex.Message), Flowsheet.GetTranslatedString1("Error"), MessageBoxButtons.OK, MessageBoxIcon.Error)
        End Try

    End Sub

    Private Sub FlowsheetSurface_SkiaSharp_KeyDown(sender As Object, e As KeyEventArgs) Handles Me.KeyDown

        HandleKeyDown(e)

    End Sub

    Private Sub PasteObjectTSMI_Click(sender As Object, e As EventArgs) Handles PasteObjectTSMI.Click
        Flowsheet.RegisterSnapshot(SnapshotType.ObjectAddedOrRemoved)
        Flowsheet.PasteObjects()
    End Sub

    Private Sub CutTSMI_Click(sender As Object, e As EventArgs) Handles CutTSMI.Click
        Flowsheet.RegisterSnapshot(SnapshotType.ObjectAddedOrRemoved)
        Flowsheet.CutObjects()
    End Sub

    Private Sub CopyTSMI_Click(sender As Object, e As EventArgs) Handles CopyTSMI.Click
        Flowsheet.CopyObjects()
    End Sub

    Private Sub tsmiCopyObjID_Click(sender As Object, e As EventArgs) Handles tsmiCopyObjID.Click

        Clipboard.SetText(FlowsheetSurface.SelectedObject.Name)
        Flowsheet.ShowMessage(String.Format(Flowsheet.GetTranslatedString1("Object's ID copied to clipboard. [{0}: {1}]"), FlowsheetSurface.SelectedObject.Tag, FlowsheetSurface.SelectedObject.Name), IFlowsheet.MessageType.Information)

    End Sub

    Public Sub ReleaseResources()

        FControl.FlowsheetObject = Nothing
        FControl.FlowsheetSurface = Nothing
        Flowsheet = Nothing
        FlowsheetSurface.Flowsheet = Nothing
        FlowsheetSurface = Nothing

        'unload context menu extenders
        For Each extender In My.Application.MainWindowForm.Extenders.Values
            Try
                If extender.Level = ExtenderLevel.FlowsheetWindow Then
                    For Each item In extender.Collection
                        If TypeOf item Is IExtender3 Then
                            DirectCast(item, IExtender3).ReleaseResources()
                        Else
                            item.SetFlowsheet(Nothing)
                        End If
                    Next
                End If
            Catch ex As Exception
            End Try
        Next

        Dim i = 0
        For Each tsmi In TSMenuItems
            RemoveHandler tsmi.Click, Handlers(i)
            i += 1
        Next

        Handlers.Clear()
        TSMenuItems.Clear()

    End Sub

End Class
