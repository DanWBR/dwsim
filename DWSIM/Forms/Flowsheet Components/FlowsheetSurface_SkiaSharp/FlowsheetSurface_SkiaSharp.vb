Imports DWSIM.Drawing.SkiaSharp.GraphicObjects
Imports WeifenLuo.WinFormsUI.Docking
Imports System.Linq
Imports System.Threading.Tasks
Imports DWSIM.UnitOperations
Imports DWSIM.SharedClasses.DWSIM.Flowsheet
Imports DWSIM.Drawing.SkiaSharp.GraphicObjects.Shapes
Imports SkiaSharp

Public Class FlowsheetSurface_SkiaSharp

    Inherits DockContent

    Private m_connecting As Boolean = False

    Public Flowsheet As FormFlowsheet

    Public m_startobj, m_endobj As GraphicObject

    Public ticks As Integer

    Public SimObjPanel As SimulationObjectsPanel

    Public FlowsheetSurface As Drawing.SkiaSharp.GraphicsSurface

    Public FControl As Control

    Public Sub New()

        ' This call is required by the designer.
        InitializeComponent()

        If My.Settings.FlowsheetRenderer = 0 Then
            Dim fscontrol As New FlowsheetSurfaceControl
            fscontrol.Dock = DockStyle.Fill
            fscontrol.FlowsheetObject = Flowsheet
            FlowsheetSurface = fscontrol.FlowsheetSurface
            FControl = fscontrol
        Else
            Dim fscontrol As New FlowsheetSurfaceGLControl
            fscontrol.Dock = DockStyle.Fill
            fscontrol.FlowsheetObject = Flowsheet
            FlowsheetSurface = fscontrol.FlowsheetSurface
            FControl = fscontrol
        End If

    End Sub

    Private Sub frmSurface_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load

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

        If My.Settings.FlowsheetRenderer = 0 Then
            DirectCast(FControl, FlowsheetSurfaceControl).FlowsheetObject = Flowsheet
        Else
            DirectCast(FControl, FlowsheetSurfaceGLControl).FlowsheetObject = Flowsheet
        End If

        SplitContainer1.Panel1.Controls.Add(FControl)

        SimObjPanel = New SimulationObjectsPanel() With {.Dock = DockStyle.Fill, .Flowsheet = Flowsheet}

        SplitContainer1.Panel2.Controls.Add(SimObjPanel)

        SplitContainer1.Panel2MinSize *= GlobalSettings.Settings.DpiScale

        AddHandler CopyFromTSMI.DropDownItemClicked, AddressOf MaterialStreamClickHandler

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

                If FlowsheetSurface.SelectedObject.ObjectType = ObjectType.MaterialStream Then

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

        Dim obj1 As Thermodynamics.Streams.MaterialStream = Flowsheet.Collections.FlowsheetObjectCollection(FlowsheetSurface.SelectedObject.Name)

        Dim obj2 As Thermodynamics.Streams.MaterialStream = Flowsheet.GetFlowsheetSimulationObject(e.ClickedItem.Text)

        obj1.Assign(obj2)

        CMS_Sel.Hide()

        Application.DoEvents()

        FlowsheetSolver.FlowsheetSolver.CalculateObject(Flowsheet, obj1.Name)

    End Sub

    Private Sub ToolStripMenuItem5_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)
        PreviewDialog.ShowDialog()
    End Sub

    Private Sub Timer1_Tick(ByVal sender As Object, ByVal e As System.EventArgs) Handles Timer1.Tick
        Me.ticks += 1
    End Sub

    Public Sub ClonarToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ClonarToolStripMenuItem.Click
        CloneObject(FlowsheetSurface.SelectedObject)
    End Sub

    Public Function CloneObject(gobj As GraphicObject) As GraphicObject

        Flowsheet = My.Application.ActiveSimulation

        Dim obj As SharedClasses.UnitOperations.BaseClass = Flowsheet.Collections.FlowsheetObjectCollection(gobj.Name)
        Dim newobj As SharedClasses.UnitOperations.BaseClass = obj.Clone

        newobj.GraphicObject = gobj.Clone
        newobj.GraphicObject.Owner = newobj

        Dim searchtext As String = gobj.Tag.Split("(")(0).Trim()

        Dim objcount As Integer = (From go As GraphicObject In FlowsheetSurface.DrawingObjects Select go Where go.Tag.Contains(searchtext)).Count

        Dim mpx = FlowsheetSurface.SelectedObject.X + FlowsheetSurface.SelectedObject.Width * 1.1
        Dim mpy = FlowsheetSurface.SelectedObject.Y + FlowsheetSurface.SelectedObject.Height * 1.1

        Select Case FlowsheetSurface.SelectedObject.ObjectType

            Case ObjectType.External

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
            Case ObjectType.ReboiledAbsorber
                Dim myDWOBJ As ReboiledAbsorber = CType(newobj, ReboiledAbsorber)
                With myDWOBJ.GraphicObject
                    .Calculated = False
                    .Name = "RBA-" & Guid.NewGuid.ToString
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
            Case ObjectType.RefluxedAbsorber
                Dim myDWOBJ As RefluxedAbsorber = CType(newobj, RefluxedAbsorber)
                With myDWOBJ.GraphicObject
                    .Calculated = False
                    .Name = "RFA-" & Guid.NewGuid.ToString
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
        End Select

        SplitContainer1.Panel1.Invalidate()

        newobj.SetFlowsheet(Flowsheet)

        Return newobj.GraphicObject

    End Function

    Private Sub CMS_ItemsToDisconnect_ItemClicked(ByVal sender As System.Object, ByVal e As System.Windows.Forms.ToolStripItemClickedEventArgs) Handles CMS_ItemsToDisconnect.ItemClicked

        Me.Flowsheet.DisconnectObject(FlowsheetSurface.SelectedObject, FormFlowsheet.SearchSurfaceObjectsByTag(e.ClickedItem.Text, FlowsheetSurface), True)

    End Sub

    Private Sub CMS_ItemsToConnect_ItemClicked(ByVal sender As System.Object, ByVal e As System.Windows.Forms.ToolStripItemClickedEventArgs) Handles CMS_ItemsToConnect.ItemClicked

        Call Me.Flowsheet.ConnectObject(FlowsheetSurface.SelectedObject, FormFlowsheet.SearchSurfaceObjectsByTag(e.ClickedItem.Text, FlowsheetSurface))

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

        For Each obj In Me.Flowsheet.Collections.FlowsheetObjectCollection.Values
            If obj.GraphicObject.Tag <> refobj.Tag Then
                If obj.GraphicObject.ObjectType <> ObjectType.GO_Text And
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
        Call Me.Flowsheet.DeleteSelectedObject(sender, e, FlowsheetSurface.SelectedObject)
    End Sub

    Public Function AddObjectToSurface(ByVal type As ObjectType, ByVal x As Integer, ByVal y As Integer, chemsep As Boolean, Optional ByVal tag As String = "", Optional ByVal id As String = "", Optional ByVal uoobj As Interfaces.IExternalUnitOperation = Nothing) As String

        If Flowsheet Is Nothing Then Flowsheet = My.Application.ActiveSimulation

        Dim gObj As GraphicObject = Nothing
        Dim fillclr As SKColor = SKColors.White
        Dim lineclr As SKColor = SKColors.SteelBlue
        Dim mpx = x '- SplitContainer1.SplitterDistance
        Dim mpy = y '- ToolStripContainer1.TopToolStripPanel.Height

        Flowsheet.WriteToLog(DWSIM.App.GetLocalTipString("FLSH004"), Color.Black, MessageType.Tip)
        Flowsheet.WriteToLog(DWSIM.App.GetLocalTipString("FLSH006"), Color.Black, MessageType.Tip)

        Select Case type

            Case ObjectType.External

                Dim myNode As New ExternalUnitOperationGraphic(mpx, mpy, 40, 40)
                myNode.Tag = uoobj.Prefix & Format(Flowsheet.Collections.FlowsheetObjectCollection.Count, "00#")
                If tag <> "" Then myNode.Tag = tag
                gObj = myNode
                gObj.Name = Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                DirectCast(uoobj, Interfaces.ISimulationObject).Name = gObj.Name
                Flowsheet.Collections.GraphicObjectCollection.Add(gObj.Name, myNode)
                DirectCast(uoobj, Interfaces.ISimulationObject).GraphicObject = myNode
                myNode.CreateConnectors(0, 0)
                Flowsheet.Collections.FlowsheetObjectCollection.Add(myNode.Name, uoobj)

            Case ObjectType.OT_Adjust

                Dim myNode As New AdjustGraphic(mpx, mpy, 20, 20)
                myNode.LineWidth = 2
                myNode.Fill = True
                myNode.FillColor = fillclr
                myNode.LineColor = lineclr
                myNode.Tag = "ADJ-" & Format(Flowsheet.Collections.FlowsheetObjectCollection.Count, "00#")
                If tag <> "" Then myNode.Tag = tag
                gObj = myNode
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
                myNode.Tag = "SPEC-" & Format(Flowsheet.Collections.FlowsheetObjectCollection.Count, "00#")
                If tag <> "" Then myNode.Tag = tag
                gObj = myNode
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
                myNode.Tag = "REC-" & Format(Flowsheet.Collections.FlowsheetObjectCollection.Count, "00#")
                If tag <> "" Then myNode.Tag = tag
                gObj = myNode
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
                myNode.Tag = "EREC-" & Format(Flowsheet.Collections.FlowsheetObjectCollection.Count, "00#")
                If tag <> "" Then myNode.Tag = tag
                gObj = myNode
                gObj.Name = "EREC-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                Flowsheet.Collections.GraphicObjectCollection.Add(gObj.Name, myNode)
                Dim myADJ As EnergyRecycle = New EnergyRecycle(myNode.Name, "EnergyRecycle")
                myADJ.GraphicObject = myNode
                Flowsheet.Collections.FlowsheetObjectCollection.Add(myNode.Name, myADJ)

            Case ObjectType.NodeIn

                Dim myNode As New MixerGraphic(mpx, mpy, 20, 20)
                myNode.LineWidth = 2
                myNode.Fill = True
                myNode.FillColor = fillclr
                myNode.LineColor = lineclr
                myNode.Tag = "MIX-" & Format(Flowsheet.Collections.FlowsheetObjectCollection.Count, "00#")
                If tag <> "" Then myNode.Tag = tag
                gObj = myNode
                gObj.Name = "MIST-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                Flowsheet.Collections.GraphicObjectCollection.Add(gObj.Name, myNode)
                Dim myCOMIX As Mixer = New Mixer(myNode.Name, "Misturador")
                myCOMIX.GraphicObject = myNode
                Flowsheet.Collections.FlowsheetObjectCollection.Add(myNode.Name, myCOMIX)

            Case ObjectType.NodeOut

                Dim myNodeo As New SplitterGraphic(mpx, mpy, 20, 20)
                myNodeo.LineWidth = 2
                myNodeo.Fill = True
                myNodeo.FillColor = fillclr
                myNodeo.LineColor = lineclr
                myNodeo.Tag = "SPLT-" & Format(Flowsheet.Collections.FlowsheetObjectCollection.Count, "00#")
                If tag <> "" Then myNodeo.Tag = tag
                gObj = myNodeo
                gObj.Name = "DIV-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                Flowsheet.Collections.GraphicObjectCollection.Add(gObj.Name, myNodeo)
                'OBJETO DWSIM
                Dim myCOSP As UnitOperations.UnitOperations.Splitter = New UnitOperations.UnitOperations.Splitter(myNodeo.Name, "Divisor")
                myCOSP.GraphicObject = myNodeo
                Flowsheet.Collections.FlowsheetObjectCollection.Add(myNodeo.Name, myCOSP)

            Case ObjectType.Pump

                Dim myPump As New PumpGraphic(mpx, mpy, 25, 25)
                myPump.LineWidth = 2
                myPump.Fill = True
                myPump.FillColor = fillclr
                myPump.LineColor = lineclr
                myPump.Tag = "PUMP-" & Format(Flowsheet.Collections.FlowsheetObjectCollection.Count, "00#")
                If tag <> "" Then myPump.Tag = tag
                gObj = myPump
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
                myTank.Tag = "TANK-" & Format(Flowsheet.Collections.FlowsheetObjectCollection.Count, "00#")
                If tag <> "" Then myTank.Tag = tag
                gObj = myTank
                gObj.Name = "TQ-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                Flowsheet.Collections.GraphicObjectCollection.Add(gObj.Name, myTank)
                'OBJETO DWSIM
                Dim myCOTK As Tank = New Tank(myTank.Name, "Tanque")
                myCOTK.GraphicObject = myTank
                Flowsheet.Collections.FlowsheetObjectCollection.Add(myTank.Name, myCOTK)

            Case ObjectType.Vessel

                Dim myVessel As New VesselGraphic(mpx, mpy, 50, 70)
                myVessel.LineWidth = 2
                myVessel.Fill = True
                myVessel.FillColor = fillclr
                myVessel.LineColor = lineclr
                myVessel.Tag = "SEP-" & Format(Flowsheet.Collections.FlowsheetObjectCollection.Count, "00#")
                If tag <> "" Then myVessel.Tag = tag
                gObj = myVessel
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
                myMStr.Tag = "MSTR-" & Format(Flowsheet.Collections.FlowsheetObjectCollection.Count, "00#")
                If tag <> "" Then myMStr.Tag = tag
                gObj = myMStr
                gObj.Name = "MAT-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                Flowsheet.Collections.GraphicObjectCollection.Add(gObj.Name, myMStr)
                'OBJETO DWSIM
                Dim myCOMS As Thermodynamics.Streams.MaterialStream = New Thermodynamics.Streams.MaterialStream(myMStr.Name, "CorrentedeMatria", Flowsheet, Nothing)
                myCOMS.GraphicObject = myMStr
                Flowsheet.AddComponentsRows(myCOMS)
                Flowsheet.Collections.FlowsheetObjectCollection.Add(myCOMS.Name, myCOMS)

                Flowsheet.WriteToLog(DWSIM.App.GetLocalTipString("MSTR001"), Color.Black, MessageType.Tip)
                Flowsheet.WriteToLog(DWSIM.App.GetLocalTipString("MSTR002"), Color.Black, MessageType.Tip)

            Case ObjectType.EnergyStream

                Dim myMStr As New EnergyStreamGraphic(mpx, mpy, 20, 20)
                myMStr.LineWidth = 2
                myMStr.Fill = True
                myMStr.Tag = "ESTR-" & Format(Flowsheet.Collections.FlowsheetObjectCollection.Count, "00#")
                If tag <> "" Then myMStr.Tag = tag
                gObj = myMStr
                gObj.Name = "EN-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                Flowsheet.Collections.GraphicObjectCollection.Add(gObj.Name, myMStr)
                'OBJETO DWSIM
                Dim myCOES As EnergyStream = New Streams.EnergyStream(myMStr.Name, "CorrentedeEnergia")
                myCOES.GraphicObject = myMStr
                Flowsheet.Collections.FlowsheetObjectCollection.Add(myCOES.Name, myCOES)

            Case ObjectType.Compressor

                Dim myComp As New CompressorGraphic(mpx, mpy, 25, 25)
                myComp.LineWidth = 2
                myComp.Fill = True
                myComp.FillColor = fillclr
                myComp.LineColor = lineclr
                myComp.Tag = "COMP-" & Format(Flowsheet.Collections.FlowsheetObjectCollection.Count, "00#")
                If tag <> "" Then myComp.Tag = tag
                gObj = myComp
                gObj.Name = "COMP-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                Flowsheet.Collections.GraphicObjectCollection.Add(gObj.Name, myComp)
                'OBJETO DWSIM
                Dim myCOCP As Compressor = New Compressor(myComp.Name, "CompressorAdiabtico")
                myCOCP.GraphicObject = myComp
                Flowsheet.Collections.FlowsheetObjectCollection.Add(myComp.Name, myCOCP)

            Case ObjectType.Expander

                Dim myComp As New TurbineGraphic(mpx, mpy, 25, 25)
                myComp.LineWidth = 2
                myComp.Fill = True
                myComp.FillColor = fillclr
                myComp.LineColor = lineclr
                myComp.Tag = "EXP-" & Format(Flowsheet.Collections.FlowsheetObjectCollection.Count, "00#")
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
                myCool.Tag = "COOL-" & Format(Flowsheet.Collections.FlowsheetObjectCollection.Count, "00#")
                If tag <> "" Then myCool.Tag = tag
                gObj = myCool
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
                myHeat.Tag = "HEAT-" & Format(Flowsheet.Collections.FlowsheetObjectCollection.Count, "00#")
                If tag <> "" Then myHeat.Tag = tag
                gObj = myHeat
                gObj.Name = "AQ-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                Flowsheet.Collections.GraphicObjectCollection.Add(gObj.Name, myHeat)
                'OBJETO DWSIM
                Dim myCOCL As Heater = New Heater(myHeat.Name, "Aquecedor")
                myCOCL.GraphicObject = myHeat
                Flowsheet.Collections.FlowsheetObjectCollection.Add(myHeat.Name, myCOCL)

            Case ObjectType.Pipe

                Dim myPipe As New PipeSegmentGraphic(mpx, mpy, 50, 10)
                myPipe.LineWidth = 2
                myPipe.Fill = True
                myPipe.FillColor = fillclr
                myPipe.LineColor = lineclr
                myPipe.Tag = "PIPE-" & Format(Flowsheet.Collections.FlowsheetObjectCollection.Count, "00#")
                If tag <> "" Then myPipe.Tag = tag
                gObj = myPipe
                gObj.Name = "TUB-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                Flowsheet.Collections.GraphicObjectCollection.Add(gObj.Name, myPipe)
                'OBJETO DWSIM
                Dim myCOPIPE As Pipe = New Pipe(myPipe.Name, "Tubulao")
                myCOPIPE.GraphicObject = myPipe
                Flowsheet.Collections.FlowsheetObjectCollection.Add(myPipe.Name, myCOPIPE)

            Case ObjectType.Valve

                Dim myValve As New ValveGraphic(mpx, mpy, 20, 20)
                myValve.LineWidth = 2
                myValve.Fill = True
                myValve.FillColor = fillclr
                myValve.LineColor = lineclr
                myValve.Tag = "VALV-" & Format(Flowsheet.Collections.FlowsheetObjectCollection.Count, "00#")
                If tag <> "" Then myValve.Tag = tag
                gObj = myValve
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
                myRconv.Tag = "RC-" & Format(Flowsheet.Collections.FlowsheetObjectCollection.Count, "00#")
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
                myReq.Tag = "RE-" & Format(Flowsheet.Collections.FlowsheetObjectCollection.Count, "00#")
                If tag <> "" Then myReq.Tag = tag
                gObj = myReq
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
                myRgibbs.Tag = "RG-" & Format(Flowsheet.Collections.FlowsheetObjectCollection.Count, "00#")
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
                myRcstr.Tag = "CSTR-" & Format(Flowsheet.Collections.FlowsheetObjectCollection.Count, "00#")
                If tag <> "" Then myRcstr.Tag = tag
                gObj = myRcstr
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
                myRpfr.Tag = "PFR-" & Format(Flowsheet.Collections.FlowsheetObjectCollection.Count, "00#")
                If tag <> "" Then myRpfr.Tag = tag
                gObj = myRpfr
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
                myHeatExchanger.LineWidth = 2
                myHeatExchanger.Fill = True
                myHeatExchanger.FillColor = fillclr
                myHeatExchanger.LineColor = lineclr
                myHeatExchanger.Tag = "HE-" & Format(Flowsheet.Collections.FlowsheetObjectCollection.Count, "00#")
                If tag <> "" Then myHeatExchanger.Tag = tag
                gObj = myHeatExchanger
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
                mySC.Tag = "SC-" & Format(Flowsheet.Collections.FlowsheetObjectCollection.Count, "00#")
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
                mySC.Tag = "DC-" & Format(Flowsheet.Collections.FlowsheetObjectCollection.Count, "00#")
                If tag <> "" Then mySC.Tag = tag
                gObj = mySC
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
                mySC.Tag = "ABS-" & Format(Flowsheet.Collections.FlowsheetObjectCollection.Count, "00#")
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
                myCSep.Tag = "CS-" & Format(Flowsheet.Collections.FlowsheetObjectCollection.Count, "00#")
                If tag <> "" Then myCSep.Tag = tag
                gObj = myCSep
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
                myCSep.Tag = "SS-" & Format(Flowsheet.Collections.FlowsheetObjectCollection.Count, "00#")
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
                myCSep.Tag = "FT-" & Format(Flowsheet.Collections.FlowsheetObjectCollection.Count, "00#")
                If tag <> "" Then myCSep.Tag = tag
                gObj = myCSep
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
                myOPL.Tag = "OP-" & Format(Flowsheet.Collections.FlowsheetObjectCollection.Count, "00#")
                If tag <> "" Then myOPL.Tag = tag
                gObj = myOPL
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
                myCUO.Tag = "UO-" & Format(Flowsheet.Collections.FlowsheetObjectCollection.Count, "00#")
                If tag <> "" Then myCUO.Tag = tag
                gObj = myCUO
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
                myEUO.Tag = "EXL-" & Format(Flowsheet.Collections.FlowsheetObjectCollection.Count, "00#")
                If tag <> "" Then myEUO.Tag = tag
                gObj = myEUO
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
                myEUO.Tag = "FS-" & Format(Flowsheet.Collections.FlowsheetObjectCollection.Count, "00#")
                If tag <> "" Then myEUO.Tag = tag
                gObj = myEUO
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
                myCUO.Tag = "COUO-" & Format(Flowsheet.Collections.FlowsheetObjectCollection.Count, "00#")
                If tag <> "" Then myCUO.Tag = tag
                gObj = myCUO
                gObj.Name = "COUO-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                Flowsheet.Collections.GraphicObjectCollection.Add(gObj.Name, myCUO)
                'OBJETO DWSIM
                If chemsep Then
                    gObj.Tag = "CSCOL-" & Format(Flowsheet.Collections.FlowsheetObjectCollection.Count, "00#")
                    DirectCast(gObj, CAPEOPENGraphic).ChemSep = True
                    gObj.Width = 144
                    gObj.Height = 180
                End If
                Dim myCOCUO As CapeOpenUO = New CapeOpenUO(myCUO.Name, "CapeOpenUnitOperation", gObj, chemsep)
                myCOCUO.GraphicObject = myCUO
                Flowsheet.Collections.FlowsheetObjectCollection.Add(myCUO.Name, myCOCUO)

                Flowsheet.WriteToLog(DWSIM.App.GetLocalTipString("CAPE001"), Color.Black, MessageType.Tip)

        End Select

        If Not gObj Is Nothing Then
            gObj.Owner = Me.Flowsheet.SimulationObjects(gObj.Name)
            Me.Flowsheet.SimulationObjects(gObj.Name).SetFlowsheet(Flowsheet)
            FlowsheetSurface.DrawingObjects.Add(gObj)
            SplitContainer1.Panel1.Invalidate()
            For Each obj In Me.Flowsheet.SimulationObjects.Values
                obj.UpdateEditForm()
                EditorTooltips.Update(obj, Flowsheet)
            Next
            If TypeOf gObj.Owner Is Thermodynamics.Streams.MaterialStream Then
                gObj.CreateConnectors(1, 1)
                If Flowsheet.Visible Then FlowsheetSolver.FlowsheetSolver.CalculateObject(Me.Flowsheet, gObj.Name)
            End If
            Application.DoEvents()
            If My.Application.PushUndoRedoAction Then Flowsheet.AddUndoRedoAction(New SharedClasses.UndoRedoAction() With {.AType = UndoRedoActionType.ObjectAdded,
                                     .ObjID = gObj.Name,
                                     .NewValue = gObj,
                                     .Name = String.Format(DWSIM.App.GetLocalString("UndoRedo_ObjectAdded"), gObj.Tag)})
        End If

        SplitContainer1.Panel1.Cursor = Cursors.Arrow

        Return gObj.Name

    End Function

    Sub AddObject(objname As String, x As Integer, y As Integer, Optional ByVal cl As Interfaces.Enums.SimulationObjectClass = SimulationObjectClass.Other)

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
        End Select

        AddObjectToSurface(tobj, x, y, chemsep)

    End Sub

    Public calcstart As Date

    Public Sub RecalcularToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles RecalcularToolStripMenuItem.Click

        Me.Flowsheet.tsmiRecalc_Click(sender, e)

    End Sub

    Private Sub CopiarDadosParaareaDeTransferenciaToolStripMenuItem_Click(sender As System.Object, e As System.EventArgs) Handles CopiarDadosParaareaDeTransferenciaToolStripMenuItem.Click

        Me.Flowsheet.tsmiExportData_Click(sender, e)

    End Sub

    Private Sub ExibirTudoToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles ExibirTudoToolStripMenuItem.Click
        FlowsheetSurface.ZoomAll(SplitContainer1.Panel1.Width, SplitContainer1.Panel1.Height)
        FlowsheetSurface.ZoomAll(SplitContainer1.Panel1.Width, SplitContainer1.Panel1.Height)
        Me.Invalidate()
    End Sub

    Private Sub ZoomPadrao100ToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles ZoomPadrao100ToolStripMenuItem.Click
        FlowsheetSurface.Zoom = 1
        Me.Invalidate()
    End Sub

    Private Sub CentralizarToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles CentralizarToolStripMenuItem.Click
        FlowsheetSurface.Center()
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

        Try
            My.Application.PushUndoRedoAction = False
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
        Finally
            My.Application.PushUndoRedoAction = True
        End Try

    End Sub

    Private Sub MergeStreamsToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles MergeStreamsToolStripMenuItem.Click

        Try
            My.Application.PushUndoRedoAction = False
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
        Finally
            My.Application.PushUndoRedoAction = True
        End Try

    End Sub

    Private Sub ToolStripButton2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ToolStripButton2.Click
        FlowsheetSurface.Zoom += 0.05
        Me.TSTBZoom.Text = Format(Flowsheet.FormSurface.FlowsheetSurface.Zoom, "#%")
        SplitContainer1.Panel1.Refresh()
    End Sub

    Private Sub ToolStripButton1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ToolStripButton1.Click
        FlowsheetSurface.Zoom -= 0.05
        If FlowsheetSurface.Zoom < 0.05 Then FlowsheetSurface.Zoom = 0.05
        Me.TSTBZoom.Text = Format(FlowsheetSurface.Zoom, "#%")
        SplitContainer1.Panel1.Refresh()
    End Sub

    Private Sub ToolStripButton18_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)

        If Flowsheet.SaveFileDialog1.ShowDialog = Windows.Forms.DialogResult.OK Then
            Dim rect As Rectangle = New Rectangle(0, 0, SplitContainer1.Panel1.Width - 14, SplitContainer1.Panel1.Height - 14)
            Dim img As Image = New Bitmap(rect.Width, rect.Height)
            SplitContainer1.Panel1.DrawToBitmap(img, SplitContainer1.Panel1.Bounds)
            img.Save(Flowsheet.SaveFileDialog1.FileName, Imaging.ImageFormat.Png)
            img.Dispose()
        End If

    End Sub

    Private Sub ToolStripButton20_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ToolStripButton20.Click
        FlowsheetSurface.ZoomAll(SplitContainer1.Panel1.Width - 14, SplitContainer1.Panel1.Height - 14)
        Application.DoEvents()
        FlowsheetSurface.ZoomAll(SplitContainer1.Panel1.Width - 14, SplitContainer1.Panel1.Height - 14)
        Application.DoEvents()
        Me.TSTBZoom.Text = Format(FlowsheetSurface.Zoom, "#%")
        SplitContainer1.Panel1.Refresh()
    End Sub

    Private Sub ToolStripButton3_Click(sender As System.Object, e As System.EventArgs) Handles ToolStripButton3.Click
        FlowsheetSurface.Zoom = 1
        Me.TSTBZoom.Text = Format(FlowsheetSurface.Zoom, "#%")
        SplitContainer1.Panel1.Refresh()
    End Sub

    Private Sub TSTBZoom_KeyPress(ByVal sender As System.Object, ByVal e As System.Windows.Forms.KeyEventArgs) Handles TSTBZoom.KeyDown
        If e.KeyCode = Keys.Enter Then
            FlowsheetSurface.Zoom = Convert.ToInt32(Me.TSTBZoom.Text.Replace("%", "")) / 100
            Me.TSTBZoom.Text = Format(FlowsheetSurface.Zoom, "#%")
            SplitContainer1.Panel1.Refresh()
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
            FlowsheetSurface.ZoomAll(SplitContainer1.Panel1.Width, SplitContainer1.Panel1.Height)
            FlowsheetSurface.ShowGrid = Flowsheet.Options.FlowsheetDisplayGrid
            FlowsheetSurface.SnapToGrid = Flowsheet.Options.FlowsheetSnapToGrid
            FlowsheetSurface.MultiSelectMode = Flowsheet.Options.FlowsheetMultiSelectMode
            tsbDisplayGrid.Checked = Flowsheet.Options.FlowsheetDisplayGrid
            tsbSnapObjectsToGrid.Checked = Flowsheet.Options.FlowsheetSnapToGrid
            tsbMultiSelectMode.Checked = Flowsheet.Options.FlowsheetSnapToGrid
        Catch ex As Exception
        End Try
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
            If FlowsheetSurface.SelectedObject.Rotation + 90 >= 360 Then
                FlowsheetSurface.SelectedObject.Rotation = Math.Truncate((FlowsheetSurface.SelectedObject.Rotation + 90) / 360)
            Else
                FlowsheetSurface.SelectedObject.Rotation = FlowsheetSurface.SelectedObject.Rotation + 90
            End If
            SplitContainer1.Panel1.Invalidate()
        End If
    End Sub

    Private Sub BToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles BToolStripMenuItem.Click
        If DWSIM.App.IsRunningOnMono Then
            MessageBox.Show(DWSIM.App.GetLocalString("Unsupported_Feature"), "DWSIM", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)
        Else
            If FlowsheetSurface.SelectedObject.Rotation + 180 >= 360 Then
                FlowsheetSurface.SelectedObject.Rotation = Math.Truncate((FlowsheetSurface.SelectedObject.Rotation + 180) / 360)
            Else
                FlowsheetSurface.SelectedObject.Rotation = FlowsheetSurface.SelectedObject.Rotation + 180
            End If
            SplitContainer1.Panel1.Invalidate()
        End If
    End Sub

    Private Sub ToolStripMenuItem7_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ToolStripMenuItem7.Click
        If DWSIM.App.IsRunningOnMono Then
            MessageBox.Show(DWSIM.App.GetLocalString("Unsupported_Feature"), "DWSIM", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)
        Else
            If FlowsheetSurface.SelectedObject.Rotation + 270 >= 360 Then
                FlowsheetSurface.SelectedObject.Rotation = Math.Truncate((FlowsheetSurface.SelectedObject.Rotation + 270) / 360)
            Else
                FlowsheetSurface.SelectedObject.Rotation = FlowsheetSurface.SelectedObject.Rotation + 270
            End If
            SplitContainer1.Panel1.Invalidate()
        End If
    End Sub

    Private Sub ToolStripMenuItem11_Click(sender As Object, e As EventArgs) Handles ToolStripMenuItem11.Click
        If DWSIM.App.IsRunningOnMono Then
            MessageBox.Show(DWSIM.App.GetLocalString("Unsupported_Feature"), "DWSIM", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)
        Else
            FlowsheetSurface.SelectedObject.Rotation = 0
            Me.Invalidate()
        End If
    End Sub

    Private Sub ToolStripMenuItem12_Click(sender As Object, e As EventArgs) Handles ToolStripMenuItem12.Click
        If DWSIM.App.IsRunningOnMono Then
            MessageBox.Show(DWSIM.App.GetLocalString("Unsupported_Feature"), "DWSIM", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)
        Else
            FlowsheetSurface.SelectedObject.Rotation = 90
            Me.Invalidate()
        End If
    End Sub

    Private Sub ToolStripMenuItem13_Click(sender As Object, e As EventArgs) Handles ToolStripMenuItem13.Click
        If DWSIM.App.IsRunningOnMono Then
            MessageBox.Show(DWSIM.App.GetLocalString("Unsupported_Feature"), "DWSIM", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)
        Else
            FlowsheetSurface.SelectedObject.Rotation = 180
            Me.Invalidate()
        End If
    End Sub

    Private Sub HorizontalmenteToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles HorizontalmenteToolStripMenuItem.Click
        If Me.HorizontalmenteToolStripMenuItem.Checked Then
            FlowsheetSurface.SelectedObject.FlippedH = True
        Else
            FlowsheetSurface.SelectedObject.FlippedH = False
        End If
        SplitContainer1.Panel1.Invalidate()
    End Sub

    Private Sub ToolStripMenuItem14_Click(sender As Object, e As EventArgs) Handles ToolStripMenuItem14.Click
        If DWSIM.App.IsRunningOnMono Then
            MessageBox.Show(DWSIM.App.GetLocalString("Unsupported_Feature"), "DWSIM", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)
        Else
            FlowsheetSurface.SelectedObject.Rotation = 270
            Me.Invalidate()
        End If
    End Sub

    Private Sub CopyAsImageToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles CopyAsImageToolStripMenuItem.Click
        CopyAsImage(1)
    End Sub

    Sub CopyAsImage(Zoom As Integer)

        Using bmp As New SKBitmap(SplitContainer1.Panel1.Width * Zoom, SplitContainer1.Panel1.Height * Zoom)
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

        SplitContainer1.Panel1.Invalidate()
        SplitContainer1.Panel1.Invalidate()

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

        Try

            My.Application.PushUndoRedoAction = False

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

        Finally
            My.Application.PushUndoRedoAction = True
        End Try

    End Sub

    Private Sub EditarAparênciaToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles EditAppearanceToolStripMenuItem.Click

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
                Dim center As Point = New Point(SplitContainer1.Panel1.Width / 2, SplitContainer1.Panel1.Height / 2)
                FlowsheetSurface.OffsetAll(center.X / FlowsheetSurface.Zoom - obj.X, center.Y / FlowsheetSurface.Zoom - obj.Y)
                FlowsheetSurface.SelectedObject = obj
                FControl.Invalidate()
                FControl.Invalidate()
            Catch ex As Exception
            End Try
        End If

    End Sub

    Private Sub tstbSearch_GotFocus(sender As Object, e As EventArgs) Handles tstbSearch.GotFocus

        tstbSearch.AutoCompleteCustomSource = New AutoCompleteStringCollection()
        tstbSearch.AutoCompleteCustomSource.Clear()
        tstbSearch.AutoCompleteCustomSource.AddRange(Flowsheet.GraphicObjects.Select(Function(x) x.Value.Tag).ToArray)

    End Sub
End Class
