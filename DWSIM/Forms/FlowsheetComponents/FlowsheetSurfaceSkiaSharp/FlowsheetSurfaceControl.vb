Imports DWSIM.Drawing.SkiaSharp
Imports DWSIM.Drawing.SkiaSharp.GraphicObjects
Imports DWSIM.Interfaces
Imports DWSIM.UnitOperations
Imports SkiaSharp.Views.Desktop

Public Class FlowsheetSurfaceControl

    Inherits SkiaSharp.Views.Desktop.SKControl

    Public WithEvents FlowsheetSurface As New Drawing.SkiaSharp.GraphicsSurface

    Public FlowsheetObject As FormFlowsheet

    Public Event ObjectSelected(ByVal sender As FormFlowsheet)

    Public Shared SingleClickAction As Action(Of IFlowsheet, MouseEventArgs)
    Public Shared DoubleClickAction As Action(Of IFlowsheet, MouseEventArgs)

    Public Sub New()

        ' This call is required by the designer.
        InitializeComponent()

        ' Add any initialization after the InitializeComponent() call.
        AllowDrop = True

        FlowsheetSurface.InvalidateCallback = Sub()
                                                  FlowsheetObject.UpdateInterface()
                                              End Sub

    End Sub

    Protected Overrides Sub OnPaintSurface(e As SKPaintSurfaceEventArgs)

        MyBase.OnPaintSurface(e)

        If FlowsheetSurface IsNot Nothing Then FlowsheetSurface.UpdateSurface(e.Surface)

    End Sub

    Private Sub FlowsheetSurfaceControl_MouseUp(sender As Object, e As MouseEventArgs) Handles Me.MouseUp
        FlowsheetSurface.InputRelease()
        Invalidate()
        Invalidate()
    End Sub

    Private Sub FlowsheetSurfaceControl_MouseMove(sender As Object, e As MouseEventArgs) Handles Me.MouseMove
        FlowsheetSurface.InputMove(e.X, e.Y)
        Invalidate()
        Invalidate()
    End Sub

    Private Sub FlowsheetSurfaceControl_MouseDown(sender As Object, e As MouseEventArgs) Handles Me.MouseDown

        FlowsheetObject.RegisterSnapshot(SnapshotType.ObjectLayout)
        FlowsheetSurface.InputPress(e.X, e.Y)
        Invalidate()
        Invalidate()

    End Sub

    Private Sub FlowsheetSurfaceControl_DragEnter(sender As Object, e As DragEventArgs) Handles Me.DragEnter
        e.Effect = DragDropEffects.All
    End Sub

    Private Sub FlowsheetSurfaceControl_MouseDoubleClick(sender As Object, e As MouseEventArgs) Handles Me.MouseDoubleClick

        If FlowsheetObject.BidirectionalSolver IsNot Nothing AndAlso FlowsheetObject.BidirectionalSolver.Activated Then
            FlowsheetObject.BidirectionalSolver.ObjectDoubleClickAction(sender, e)
        ElseIf DoubleClickAction IsNot Nothing Then
            DoubleClickAction?.Invoke(FlowsheetObject, e)
        Else
            DoubleClickHandler(sender, e)
        End If

    End Sub

    Public Sub DoubleClickHandler(sender As Object, e As MouseEventArgs)

        Dim obj = FlowsheetSurface.SelectedObject

        If (obj Is Nothing) Then

            FlowsheetObject.RegisterSnapshot(SnapshotType.ObjectLayout)
            FlowsheetSurface.ZoomAll(Width, Height)
            FlowsheetSurface.Center(Width, Height)
            FlowsheetObject.FormSurface.TSTBZoom.Text = FlowsheetSurface.Zoom.ToString("###%")
            Invalidate()
            Invalidate()

        Else

            Select Case Me.FlowsheetSurface.SelectedObject.ObjectType
                Case ObjectType.GO_Button
                    Dim btn = DirectCast(FlowsheetSurface.SelectedObject, Drawing.SkiaSharp.GraphicObjects.Shapes.ButtonGraphic)
                    Dim f As New FormEditFlowsheetButton() With {.Flowsheet = FlowsheetObject, .ButtonObject = btn}
                    f.ShowDialog(Me)
                Case ObjectType.GO_HTMLText
                    Dim rtg = DirectCast(FlowsheetSurface.SelectedObject, Drawing.SkiaSharp.GraphicObjects.HTMLTextGraphic)
                    Dim f As New FormHTMLEditor()
                    f.Editor1.Html = rtg.Text
                    f.ShowDialog(Me)
                    rtg.Text = f.Editor1.Html
                Case ObjectType.GO_Table
                    Dim f As New FormConfigurePropertyTable() With {.Table = FlowsheetSurface.SelectedObject}
                    f.ShowDialog(Me)
                Case ObjectType.GO_SpreadsheetTable
                    Dim f As New FormConfigureSpreadsheetTable() With {.Table = FlowsheetSurface.SelectedObject}
                    f.ShowDialog(Me)
                Case ObjectType.GO_MasterTable
                    Dim f As New FormConfigureMasterTable() With {.Table = FlowsheetSurface.SelectedObject}
                    f.ShowDialog(Me)
                Case ObjectType.GO_Chart
                    Dim f As New FormConfigureChartObject() With {.Chart = FlowsheetSurface.SelectedObject}
                    f.ShowDialog(Me)
                Case ObjectType.FlowsheetUO
                    Dim myobj As UnitOperations.UnitOperations.Flowsheet = FlowsheetObject.SimulationObjects(FlowsheetSurface.SelectedObject.Name)
                    If My.Computer.Keyboard.ShiftKeyDown Then
                        Dim viewform As New UnitOperations.EditingForm_Flowsheet_Viewer
                        With viewform
                            .Text = FlowsheetSurface.SelectedObject.Tag
                            .fsuo = myobj
                            .ShowDialog()
                            .Dispose()
                        End With
                        viewform = Nothing
                    Else
                        If myobj.Initialized Then
                            Dim viewform As New UnitOperations.EditingForm_Flowsheet_Viewer
                            With viewform
                                .Text = FlowsheetSurface.SelectedObject.Tag
                                .fsuo = myobj
                                .Show(FlowsheetObject.dckPanel)
                            End With
                        Else
                            Dim viewform As New UnitOperations.EditingForm_Flowsheet_Editor
                            With viewform
                                .Text = FlowsheetSurface.SelectedObject.Tag
                                .fsuo = myobj
                                .ShowDialog()
                                .Dispose()
                            End With
                            viewform = Nothing
                        End If
                    End If
                Case ObjectType.CapeOpenUO
                    Dim myobj As CapeOpenUO = FlowsheetObject.SimulationObjects(FlowsheetSurface.SelectedObject.Name)
                    myobj.Edit()
                Case ObjectType.CustomUO
                    Dim myobj As CustomUO = FlowsheetObject.SimulationObjects(FlowsheetSurface.SelectedObject.Name)
                    If Not DWSIM.App.IsRunningOnMono Then
                        Dim f As New EditingForm_CustomUO_ScriptEditor With {.ScriptUO = myobj}
                        myobj.FlowSheet.DisplayForm(f)
                    Else
                        Dim f As New EditingForm_CustomUO_ScriptEditor_Mono With {.ScriptUO = myobj}
                        myobj.FlowSheet.DisplayForm(f)
                    End If
            End Select

            If My.Settings.DoubleClickToEdit Then
                If FlowsheetObject.SimulationObjects.ContainsKey(FlowsheetSurface.SelectedObject.Name) Then
                    If Not My.Settings.EnableMultipleObjectEditors Then
                        For Each sobj In FlowsheetObject.SimulationObjects.Values
                            sobj.CloseEditForm()
                            If FlowsheetObject.DynamicMode Then sobj.CloseDynamicsEditForm()
                        Next
                    End If
                    FlowsheetObject.SimulationObjects(FlowsheetSurface.SelectedObject.Name).DisplayEditForm()
                    If FlowsheetObject.DynamicMode And FlowsheetObject.SimulationObjects(FlowsheetSurface.SelectedObject.Name).HasPropertiesForDynamicMode Then
                        FlowsheetObject.SimulationObjects(FlowsheetSurface.SelectedObject.Name).DisplayDynamicsEditForm()
                    End If
                    EditorTooltips.Update(FlowsheetObject.SimulationObjects(FlowsheetSurface.SelectedObject.Name), FlowsheetObject)
                    If FlowsheetObject.Options.DisplayUserDefinedPropertiesEditor Then
                        FlowsheetObject.SimulationObjects(FlowsheetSurface.SelectedObject.Name).DisplayExtraPropertiesEditForm()
                    End If
                End If
            End If

            obj?.DoubleClickAction?.Invoke(obj)

        End If


    End Sub

    Private Sub FlowsheetSurfaceControl_MouseWheel(sender As Object, e As MouseEventArgs) Handles Me.MouseWheel
        FlowsheetObject.RegisterSnapshot(SnapshotType.ObjectLayout)
        Dim oldzoom = FlowsheetSurface.Zoom
        FlowsheetSurface.Zoom += e.Delta / 4 / 100.0
        If FlowsheetSurface.Zoom < 0.05 Then FlowsheetSurface.Zoom = 0.05
        FlowsheetObject.FormSurface.TSTBZoom.Text = FlowsheetSurface.Zoom.ToString("###%")
        FlowsheetObject.FormSurface.FlowsheetSurface.CenterTo(oldzoom, e.Location.X, e.Location.Y, FlowsheetObject.FormSurface.SplitContainerHorizontal.Panel1.Width,
                                                            FlowsheetObject.FormSurface.SplitContainerHorizontal.Panel1.Height)
        Invalidate()
        Invalidate()
    End Sub

#Region "Events"

    Public Sub FlowsheetDesignSurface_MouseUp(ByVal sender As Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles Me.MouseUp

        If FlowsheetObject.BidirectionalSolver IsNot Nothing AndAlso FlowsheetObject.BidirectionalSolver.Activated Then
            FlowsheetObject.BidirectionalSolver.ObjectClickAction(sender, e)
        ElseIf SingleClickAction IsNot Nothing Then
            SingleClickAction.Invoke(FlowsheetObject, e)
        Else
            SingleClickHandler(sender, e)
        End If

    End Sub

    Public Sub SingleClickHandler(sender As Object, e As MouseEventArgs)


        If e.Button = Windows.Forms.MouseButtons.Left Then

            If Not Me.FlowsheetSurface.SelectedObject Is Nothing Then

                If My.Settings.CloseFormsOnDeselecting Then
                    For Each obj In FlowsheetObject.SimulationObjects.Values
                        If Not obj.GraphicObject Is FlowsheetSurface.SelectedObject Then
                            obj.CloseEditForm()
                            If FlowsheetObject.DynamicMode Then obj.CloseDynamicsEditForm()
                        End If
                    Next
                End If

                If FlowsheetObject.SimulationObjects.ContainsKey(FlowsheetSurface.SelectedObject.Name) Then

                    If Not FlowsheetSurface.ControlPanelMode Then
                        If Not My.Settings.EnableMultipleObjectEditors Then
                            For Each obj In FlowsheetObject.SimulationObjects.Values
                                obj.CloseEditForm()
                                If FlowsheetObject.DynamicMode Then obj.CloseDynamicsEditForm()
                            Next
                        End If
                        If Not My.Settings.DoubleClickToEdit Then
                            FlowsheetObject.SimulationObjects(FlowsheetSurface.SelectedObject.Name).DisplayEditForm()
                            If FlowsheetObject.DynamicMode And FlowsheetObject.SimulationObjects(FlowsheetSurface.SelectedObject.Name).HasPropertiesForDynamicMode Then
                                FlowsheetObject.SimulationObjects(FlowsheetSurface.SelectedObject.Name).DisplayDynamicsEditForm()
                            End If
                            EditorTooltips.Update(FlowsheetObject.SimulationObjects(FlowsheetSurface.SelectedObject.Name), FlowsheetObject)
                            If FlowsheetObject.Options.DisplayUserDefinedPropertiesEditor Then
                                FlowsheetObject.SimulationObjects(FlowsheetSurface.SelectedObject.Name).DisplayExtraPropertiesEditForm()
                            End If
                        End If
                    End If

                    Focus()

                End If

            End If

            'new
            FlowsheetObject.FormSurface.FlowsheetDesignSurface_SelectionChanged_New(sender, New EventArgs)

        ElseIf e.Button = Windows.Forms.MouseButtons.Right Then

            If Not FlowsheetSurface.SelectedObject Is Nothing Then

                FlowsheetObject.FormSurface.CMS_Sel.Items("TSMI_Label").Text = FlowsheetSurface.SelectedObject.Tag
                FlowsheetObject.FormSurface.CMS_Sel.Show(MousePosition)

            Else

                FlowsheetObject.FormSurface.CMS_NoSel.Show(MousePosition)

            End If

        End If

        RaiseEvent ObjectSelected(FlowsheetObject)


    End Sub

    Private Sub FlowsheetDesignSurface_DragDrop(sender As Object, e As DragEventArgs) Handles Me.DragDrop

        If e.Effect = DragDropEffects.All Then

            Dim pt = PointToClient(New Point(e.X, e.Y))

            Dim obj As Object() = Nothing
            obj = e.Data.GetData("System.Object[]")

            Dim t As Type = Nothing
            Dim c As Interfaces.Enums.SimulationObjectClass

            t = obj(0)

            Dim pdfpath, scriptpath As String
            pdfpath = ""
            scriptpath = ""

            If TypeOf obj(1) Is Interfaces.Enums.SimulationObjectClass Then
                c = obj(1)
            Else
                c = obj(1)(0)
                scriptpath = obj(1)(1)
                pdfpath = obj(1)(2)
            End If

            If c = SimulationObjectClass.None Then

                If obj.Length = 4 Then

                    Dim name As String = obj(2)
                    Dim otype = obj(3)

                    ProFeatures.Functions.CreateTransitionObject(FlowsheetObject, name, "Unit Operation", "Add", otype, New Double() {e.X / FlowsheetSurface.Zoom, e.Y / FlowsheetSurface.Zoom})

                    ProFeatures.Functions.DisplayTransitionForm(FlowsheetObject, name & " Unit Operation")

                    Exit Sub

                Else

                    Exit Sub

                End If

            End If

            Console.WriteLine(t.Name)

            If scriptpath <> "" Then

                Dim uobjid = FlowsheetObject.FormSurface.AddObjectToSurface(ObjectType.CustomUO, pt.X / FlowsheetSurface.Zoom, pt.Y / FlowsheetSurface.Zoom, False, "", "")
                Dim suo = DirectCast(FlowsheetObject.SimulationObjects(uobjid), CustomUO)
                suo.ScriptText = IO.File.ReadAllText(scriptpath)

                FormMain.AnalyticsProvider?.RegisterEvent("Added Object", "FOSSEE Custom Unit Operation: " + scriptpath, Nothing)

                Process.Start(pdfpath)

            ElseIf t.GetInterface("DWSIM.Interfaces.IExternalUnitOperation", True) Is Nothing Then

                FormMain.AnalyticsProvider?.RegisterEvent("Added Object", t.Name, Nothing)

                FlowsheetObject.FormSurface.AddObject(t.Name, pt.X / FlowsheetSurface.Zoom, pt.Y / FlowsheetSurface.Zoom, c, True)

            Else

                FormMain.AnalyticsProvider?.RegisterEvent("Added Object", "External: " + t.FullName, Nothing)

                FlowsheetObject.FormSurface.AddObjectToSurface(ObjectType.External, pt.X / FlowsheetSurface.Zoom, pt.Y / FlowsheetSurface.Zoom, False, "", "", Activator.CreateInstance(t))

            End If

        End If
    End Sub

    Public Sub FlowsheetDesignSurface_KeyDown(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyEventArgs) Handles Me.KeyDown

        If e.KeyCode = Keys.E And e.Control Then

        ElseIf e.KeyCode = Keys.F5 Then
            FlowsheetObject.tsbCalc.PerformClick()
        ElseIf e.KeyCode = Keys.F6 Then
            FlowsheetObject.tsbAtivar.Checked = Not FlowsheetObject.tsbAtivar.Checked
            FlowsheetObject.tsbAtivar_CheckedChanged(sender, e)
        ElseIf e.KeyCode = Keys.F7 Then
            FlowsheetObject.tsbSimultAdjustSolver.Checked = Not FlowsheetObject.tsbSimultAdjustSolver.Checked
            FlowsheetObject.tsbSimultAdjustSolver_CheckedChanged(sender, e)
        ElseIf e.KeyCode = Keys.Pause Then
            FlowsheetObject.tsbAbortCalc_Click(sender, e)
        ElseIf e.KeyCode = Keys.X And e.Control Then
            FlowsheetObject.tsmiCut_Click(Me, New EventArgs)
        ElseIf e.KeyCode = Keys.C And e.Control Then
            FlowsheetObject.tsmiCopy_Click(Me, New EventArgs)
        ElseIf e.KeyCode = Keys.V And e.Control Then
            FlowsheetObject.tsmiPaste_Click(Me, New EventArgs)
        ElseIf e.KeyCode = Keys.Delete Then
            FlowsheetObject.tsmiRemoveSelected_Click(Me, New EventArgs)
        End If

        If Not FlowsheetSurface.SelectedObject Is Nothing Then
            For Each go As GraphicObject In FlowsheetSurface.SelectedObjects.Values
                If e.KeyCode = Keys.Up Then
                    If e.Modifiers = Keys.Control Then
                        go.Y = go.Y - 1
                    Else
                        go.Y = go.Y - 5
                    End If
                ElseIf e.KeyCode = Keys.Down Then
                    If e.Modifiers = Keys.Control Then
                        go.Y = go.Y + 1
                    Else
                        go.Y = go.Y + 5
                    End If
                ElseIf e.KeyCode = Keys.Left Then
                    If e.Modifiers = Keys.Control Then
                        go.X = go.X - 1
                    Else
                        go.X = go.X - 5
                    End If
                ElseIf e.KeyCode = Keys.Right Then
                    If e.Modifiers = Keys.Control Then
                        go.X = go.X + 1
                    Else
                        go.X = go.X + 5
                    End If
                End If
            Next
            Me.Invalidate()
        End If

    End Sub

#End Region

End Class
