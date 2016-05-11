Imports DWSIM.DrawingTools.GraphicObjects
Imports System.Collections.Generic
Imports System.ComponentModel
Imports WeifenLuo.WinFormsUI.Docking

Imports DWSIM.FlowsheetSolver
Imports System.Drawing.Drawing2D
Imports System.Linq
Imports System.Threading.Tasks
Imports DWSIM.GraphicObjects
Imports DWSIM.Interfaces.Enums.GraphicObjects
Imports DWSIM.UnitOperations

Public Class FlowsheetSurface
    Inherits DockContent

    Private m_connecting As Boolean = False
    Private m_stPoint As New Drawing.Point

    Public Flowsheet As FormFlowsheet

    Public m_startobj, m_endobj As GraphicObject

    Public m_qt As QuickTableGraphic

    Public ticks As Integer

    Public Event ObjectSelected(ByVal sender As FormFlowsheet)

    Public Function ReturnForm(ByVal str As String) As IDockContent

        If str = Me.ToString Then
            Return Me
        Else
            Return Nothing
        End If

    End Function

    Public Sub SetupGraphicsSurface()

        'load up the design surface with the default bounds and margins
        Dim defSettings As Printing.PageSettings = _
            designSurfacePrintDocument.DefaultPageSettings
        With defSettings

            Dim bounds As Rectangle = .Bounds
            Dim horizRes As Integer = .PrinterResolution.X

            Dim vertRes As Integer = .PrinterResolution.Y

            Me.FlowsheetDesignSurface.SurfaceBounds = bounds

            'Me.FlowsheetDesignSurface.GridSize = 50
            Me.FlowsheetDesignSurface.SurfaceMargins = _
                New Rectangle(bounds.Left + .Margins.Left, _
                    bounds.Top + .Margins.Top, _
                    bounds.Width - .Margins.Left - .Margins.Right, _
                    bounds.Height - .Margins.Top - .Margins.Bottom)
        End With


    End Sub

    Private Sub designSurfacePrintDocument_PrintPage(ByVal sender As System.Object, _
            ByVal e As System.Drawing.Printing.PrintPageEventArgs) _
            Handles designSurfacePrintDocument.PrintPage
        Dim drawobj As GraphicObjectCollection = Me.FlowsheetDesignSurface.drawingObjects
        Me.FlowsheetDesignSurface.drawingObjects.PrintObjects(e.Graphics, -FlowsheetDesignSurface.HorizontalScroll.Value / FlowsheetDesignSurface.Zoom, -FlowsheetDesignSurface.VerticalScroll.Value / FlowsheetDesignSurface.Zoom)
    End Sub

    Private Sub frmSurface_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load

        If TypeOf Me.ParentForm Is FormFlowsheet Then
            Flowsheet = Me.ParentForm
        ElseIf Flowsheet Is Nothing Then
            Flowsheet = My.Application.ActiveSimulation
        End If

        AddHandler CopyFromTSMI.DropDownItemClicked, AddressOf MaterialStreamClickHandler

    End Sub

    Public Sub UpdateSelectedObject()

        If Not Me.FlowsheetDesignSurface.SelectedObject Is Nothing Then

            If Flowsheet.SimulationObjects.ContainsKey(Me.FlowsheetDesignSurface.SelectedObject.Name) Then

                Flowsheet.SimulationObjects(Me.FlowsheetDesignSurface.SelectedObject.Name).UpdateEditForm()

            End If

        End If

    End Sub

    Private Sub FlowsheetDesignSurface_KeyDown(ByVal sender As Object, ByVal e As System.Windows.Forms.KeyEventArgs) Handles FlowsheetDesignSurface.KeyDown

        If e.KeyCode = Keys.E And e.Control Then

        ElseIf e.KeyCode = Keys.F5 Then
            If My.Computer.Keyboard.ShiftKeyDown Then
                Flowsheet.tsbDesat.Checked = False
                Flowsheet.tsbAtivar.Checked = True
                GlobalSettings.Settings.CalculatorActivated = True
            End If
            FlowsheetSolver.FlowsheetSolver.SolveFlowsheet(Me.Flowsheet, My.Settings.SolverMode)
        ElseIf e.KeyCode = Keys.F6 Then
            If Not Me.FlowsheetDesignSurface.SelectedObject Is Nothing Then RecalcularToolStripMenuItem_Click(sender, e)
        ElseIf e.KeyCode = Keys.D And e.Control Then
            Flowsheet.tsbAtivar.Checked = False
            Flowsheet.tsbDesat.Checked = True
            GlobalSettings.Settings.CalculatorActivated = False
         ElseIf e.KeyCode = Keys.A And e.Control Then
            Flowsheet.tsbDesat.Checked = False
            Flowsheet.tsbAtivar.Checked = True
            GlobalSettings.Settings.CalculatorActivated = True
            Flowsheet.WriteToLog(DWSIM.App.GetLocalString("Calculadorativado"), Color.DimGray, DWSIM.Flowsheet.MessageType.Information)
        ElseIf e.KeyCode = Keys.X And e.Control Then
            Flowsheet.tsmiCut_Click(Me, New EventArgs)
        ElseIf e.KeyCode = Keys.C And e.Control Then
            Flowsheet.tsmiCopy_Click(Me, New EventArgs)
        ElseIf e.KeyCode = Keys.V And e.Control Then
            Flowsheet.tsmiPaste_Click(Me, New EventArgs)
        ElseIf e.KeyCode = Keys.Delete Then
            Flowsheet.tsmiRemoveSelected_Click(Me, New EventArgs)
        End If

        If Not Me.FlowsheetDesignSurface.SelectedObject Is Nothing Then
            For Each go As GraphicObject In Me.FlowsheetDesignSurface.SelectedObjects.Values
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
            Me.FlowsheetDesignSurface.Invalidate()
        Else
            If e.KeyCode = Keys.Up Then
                If Me.FlowsheetDesignSurface.VerticalScroll.Value > 4 * Me.FlowsheetDesignSurface.VerticalScroll.SmallChange Then
                    Me.FlowsheetDesignSurface.VerticalScroll.Value -= 4 * Me.FlowsheetDesignSurface.VerticalScroll.SmallChange
                Else
                    Me.FlowsheetDesignSurface.VerticalScroll.Value = 0
                End If
            ElseIf e.KeyCode = Keys.Down Then
                Me.FlowsheetDesignSurface.VerticalScroll.Value += 4 * Me.FlowsheetDesignSurface.VerticalScroll.SmallChange
            ElseIf e.KeyCode = Keys.Left Then
                If Me.FlowsheetDesignSurface.HorizontalScroll.Value > 4 * Me.FlowsheetDesignSurface.HorizontalScroll.SmallChange Then
                    Me.FlowsheetDesignSurface.HorizontalScroll.Value -= 4 * Me.FlowsheetDesignSurface.HorizontalScroll.SmallChange
                Else
                    Me.FlowsheetDesignSurface.HorizontalScroll.Value = 0
                End If
            ElseIf e.KeyCode = Keys.Right Then
                Me.FlowsheetDesignSurface.HorizontalScroll.Value += 4 * Me.FlowsheetDesignSurface.HorizontalScroll.SmallChange
            End If
            Me.FlowsheetDesignSurface.Invalidate()
            Me.FlowsheetDesignSurface.Invalidate()
        End If

    End Sub

    Private Sub FlowsheetDesignSurface_SelectionChanged(ByVal sender As Object, _
            ByVal e As DrawingTools.SelectionChangedEventArgs) Handles FlowsheetDesignSurface.SelectionChanged

        If Not e.SelectedObject Is Nothing Then

            If Not e.SelectedObject.IsConnector Then

                If Not Me.FlowsheetDesignSurface.SelectedObject Is Nothing Then

                    If Me.FlowsheetDesignSurface.SelectedObject.IsConnector Then

                        Me.FlowsheetDesignSurface.SelectedObject = Nothing

                    End If

                End If

                Flowsheet.ChangeEditMenuStatus(True)

            Else

                Me.FlowsheetDesignSurface.SelectedObject = Nothing

            End If

        Else

            Flowsheet.ChangeEditMenuStatus(False)

        End If

        If My.Settings.CloseFormsOnDeselecting Then
            For Each obj In Flowsheet.SimulationObjects.Values
                If Not obj.GraphicObject Is FlowsheetDesignSurface.SelectedObject Then
                    obj.CloseEditForm()
                End If
            Next
        End If

    End Sub

    Private Sub ToolStripMenuItem6_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ToolStripMenuItem6.Click
        If DWSIM.App.IsRunningOnMono Then
            MessageBox.Show(DWSIM.App.GetLocalString("Unsupported_Feature"), "DWSIM", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)
        Else
            If Me.FlowsheetDesignSurface.SelectedObject.Rotation + 90 >= 360 Then
                Me.FlowsheetDesignSurface.SelectedObject.Rotation = Math.Truncate((Me.FlowsheetDesignSurface.SelectedObject.Rotation + 90) / 360)
            Else
                Me.FlowsheetDesignSurface.SelectedObject.Rotation = Me.FlowsheetDesignSurface.SelectedObject.Rotation + 90
            End If
            Me.FlowsheetDesignSurface.Invalidate()
        End If
    End Sub

    Private Sub BToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles BToolStripMenuItem.Click
        If DWSIM.App.IsRunningOnMono Then
            MessageBox.Show(DWSIM.App.GetLocalString("Unsupported_Feature"), "DWSIM", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)
        Else
            If Me.FlowsheetDesignSurface.SelectedObject.Rotation + 180 >= 360 Then
                Me.FlowsheetDesignSurface.SelectedObject.Rotation = Math.Truncate((Me.FlowsheetDesignSurface.SelectedObject.Rotation + 180) / 360)
            Else
                Me.FlowsheetDesignSurface.SelectedObject.Rotation = Me.FlowsheetDesignSurface.SelectedObject.Rotation + 180
            End If
            Me.FlowsheetDesignSurface.Invalidate()
        End If
    End Sub

    Private Sub ToolStripMenuItem7_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ToolStripMenuItem7.Click
        If DWSIM.App.IsRunningOnMono Then
            MessageBox.Show(DWSIM.App.GetLocalString("Unsupported_Feature"), "DWSIM", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)
        Else
            If Me.FlowsheetDesignSurface.SelectedObject.Rotation + 270 >= 360 Then
                Me.FlowsheetDesignSurface.SelectedObject.Rotation = Math.Truncate((Me.FlowsheetDesignSurface.SelectedObject.Rotation + 270) / 360)
            Else
                Me.FlowsheetDesignSurface.SelectedObject.Rotation = Me.FlowsheetDesignSurface.SelectedObject.Rotation + 270
            End If
            Me.FlowsheetDesignSurface.Invalidate()
        End If
    End Sub

    Private Sub FlowsheetDesignSurface_StatusUpdate(ByVal sender As Object, ByVal e As DrawingTools.StatusUpdateEventArgs) Handles FlowsheetDesignSurface.StatusUpdate
        Flowsheet.TSTBZoom.Text = Format(FlowsheetDesignSurface.Zoom, "#%")
    End Sub

    Private Sub FlowsheetDesignSurface_MouseClick(ByVal sender As Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles FlowsheetDesignSurface.MouseDown

        If e.Button = Windows.Forms.MouseButtons.Left Then

            If Me.FlowsheetDesignSurface.QuickConnect And My.Computer.Keyboard.CtrlKeyDown Then
                Dim mousePT As New Drawing.Point(Flowsheet.gscTogoc(e.X, e.Y))
                Dim mpx = mousePT.X
                Dim mpy = mousePT.Y
                Me.m_stPoint = mousePT
                Dim myCTool As New ConnectToolGraphic(mousePT.X, mousePT.Y)
                myCTool.Name = "CTOOL1234567890"
                myCTool.Width = mousePT.X
                myCTool.Height = mousePT.Y
                Me.m_startobj = Me.FlowsheetDesignSurface.drawingObjects.FindObjectAtPoint(mousePT.ToDTPoint)
                Me.FlowsheetDesignSurface.drawingObjects.Add(myCTool)
                Me.m_connecting = True
            Else
                Me.FlowsheetDesignSurface.SelectRectangle = True
            End If

            Me.FlowsheetDesignSurface.Invalidate()

        End If

    End Sub

    Public Sub FlowsheetDesignSurface_MouseUp(ByVal sender As Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles FlowsheetDesignSurface.MouseUp

        If Me.m_connecting Then

            Me.m_connecting = False

            Me.FlowsheetDesignSurface.drawingObjects.Remove(Me.FlowsheetDesignSurface.drawingObjects.FindObjectWithName("CTOOL1234567890"))

            Me.m_endobj = Me.FlowsheetDesignSurface.drawingObjects.FindObjectAtPoint(Flowsheet.gscTogoc(e.X, e.Y).ToDTPoint)

            Me.FlowsheetDesignSurface.SelectRectangle = True
            Me.FlowsheetDesignSurface.Invalidate()

            If Not m_startobj Is Nothing And Not m_endobj Is Nothing Then
                If m_startobj.Name <> m_endobj.Name Then
                    Try
                        Flowsheet.ConnectObject(Me.m_startobj, Me.m_endobj)
                    Catch ex As Exception
                        Flowsheet.WriteToLog(ex.Message.ToString, Color.Red, DWSIM.Flowsheet.MessageType.GeneralError)
                    End Try
                End If
            End If
        End If

        If e.Button = Windows.Forms.MouseButtons.Left Then

            If Not Flowsheet.ClickedToolStripMenuItem Is Nothing Then
                If Flowsheet.InsertingObjectToPFD Then

                    Dim gObj As GraphicObject = Nothing
                    Dim fillclr As Color = Color.WhiteSmoke
                    Dim lineclr As Color = Color.Red
                    Dim mousePT As Drawing.Point = Flowsheet.gscTogoc(e.X, e.Y)
                    Dim mpx = mousePT.X '- SplitContainer1.SplitterDistance
                    Dim mpy = mousePT.Y '- ToolStripContainer1.TopToolStripPanel.Height
                    Dim tobj As ObjectType = ObjectType.Nenhum

                    Select Case Flowsheet.ClickedToolStripMenuItem.Name

                        Case "TSMIMaterialStream"
                            tobj = ObjectType.MaterialStream
                        Case "TSMIEnergyStream"
                            tobj = ObjectType.EnergyStream
                        Case "TSMIMixer"
                            tobj = ObjectType.NodeIn
                        Case "TSMISplitter"
                            tobj = ObjectType.NodeOut
                        Case "TSMICompressor"
                            tobj = ObjectType.Compressor
                        Case "TSMIExpander"
                            tobj = ObjectType.Expander
                        Case "TSMIPump"
                            tobj = ObjectType.Pump
                        Case "TSMIPipe"
                            tobj = ObjectType.Pipe
                        Case "TSMIValve"
                            tobj = ObjectType.Valve
                        Case "TSMISeparator"
                            tobj = ObjectType.Vessel
                        Case "TSMIHeater"
                            tobj = ObjectType.Heater
                        Case "TSMICooler"
                            tobj = ObjectType.Cooler
                        Case "TSMIOrificePlate"
                            tobj = ObjectType.OrificePlate
                        Case "TSMIComponentSeparator"
                            tobj = ObjectType.ComponentSeparator
                        Case "TSMIHeatExchanger"
                            tobj = ObjectType.HeatExchanger
                        Case "TSMITank"
                            tobj = ObjectType.Tank
                        Case "TSMIColShortcut"
                            tobj = ObjectType.ShortcutColumn
                        Case "TSMIColDist"
                            tobj = ObjectType.DistillationColumn
                        Case "TSMIColAbs"
                            tobj = ObjectType.AbsorptionColumn
                        Case "TSMIColAbsReb"
                            tobj = ObjectType.ReboiledAbsorber
                        Case "TSMIColAbsCond"
                            tobj = ObjectType.RefluxedAbsorber
                        Case "TSMIReactorConv"
                            tobj = ObjectType.RCT_Conversion
                        Case "TSMIReactorEquilibrium"
                            tobj = ObjectType.RCT_Equilibrium
                        Case "TSMIReactorGibbs"
                            tobj = ObjectType.RCT_Gibbs
                        Case "TSMIReactorCSTR"
                            tobj = ObjectType.RCT_CSTR
                        Case "TSMIReactorPFR"
                            tobj = ObjectType.RCT_PFR
                        Case "TSMIRecycle"
                            tobj = ObjectType.OT_Recycle
                        Case "TSMIEnergyRecycle"
                            tobj = ObjectType.OT_EnergyRecycle
                        Case "TSMIAdjust"
                            tobj = ObjectType.OT_Adjust
                        Case "TSMISpecification"
                            tobj = ObjectType.OT_Spec
                        Case "TSMICUO"
                            tobj = ObjectType.CustomUO
                        Case "TSMIExcelUO"
                            tobj = ObjectType.ExcelUO
                        Case "TSMICOUO"
                            tobj = ObjectType.CapeOpenUO
                        Case "TSMISolidsSeparator"
                            tobj = ObjectType.SolidSeparator
                        Case "TSMIFilter"
                            tobj = ObjectType.Filter
                        Case "TSMIFlowsheet"
                            tobj = ObjectType.FlowsheetUO
                    End Select

                    AddObjectToSurface(tobj, mpx, mpy)

                    Flowsheet.ClickedToolStripMenuItem = Nothing
                    Flowsheet.InsertingObjectToPFD = False

                    If tobj = ObjectType.MaterialStream Then

                    End If

                End If

            End If

            If Not Me.FlowsheetDesignSurface.SelectedObject Is Nothing Then

                If Flowsheet.SimulationObjects.ContainsKey(Me.FlowsheetDesignSurface.SelectedObject.Name) Then

                    Flowsheet.SimulationObjects(Me.FlowsheetDesignSurface.SelectedObject.Name).DisplayEditForm()

                Else

                    'Me.FlowsheetDesignSurface.SelectedObject = Nothing

                End If

            End If

        ElseIf e.Button = Windows.Forms.MouseButtons.Right Then

            If Not Me.FlowsheetDesignSurface.SelectedObject Is Nothing Then

                Me.CMS_Sel.Items("TSMI_Label").Text = Me.FlowsheetDesignSurface.SelectedObject.Tag
                Me.CMS_Sel.Show(MousePosition)

            Else

                Me.CMS_NoSel.Show(MousePosition)

            End If

        End If

        RaiseEvent ObjectSelected(Flowsheet)

    End Sub

    Private Sub FlowsheetDesignSurface_MouseMove(ByVal sender As Object, ByVal e As System.Windows.Forms.MouseEventArgs) Handles FlowsheetDesignSurface.MouseMove

        Dim px As Drawing.Point = Flowsheet.gscTogoc(e.Location.X, e.Location.Y)
        Dim px2 As Drawing.Point = Flowsheet.gscTogoc(e.Location.X + 20, e.Location.Y + 20)

        If Me.m_connecting Then

            Dim myCTool As ConnectToolGraphic = Me.FlowsheetDesignSurface.drawingObjects.FindObjectWithName("CTOOL1234567890")

            Dim mousePT As New Drawing.Point(Flowsheet.gscTogoc(e.X, e.Y))

            myCTool.Width = mousePT.X
            myCTool.Height = mousePT.Y

        ElseIf Not FlowsheetDesignSurface.dragging Then

            Dim gobj As GraphicObject = Me.FlowsheetDesignSurface.drawingObjects.FindObjectAtPoint(px.ToDTPoint)

            If Not gobj Is Nothing Then

                If Me.m_qt Is Nothing And Not _
                        gobj.ObjectType = ObjectType.GO_FloatingTable And Not _
                        gobj.ObjectType = ObjectType.GO_MasterTable And Not _
                        gobj.ObjectType = ObjectType.GO_SpreadsheetTable And Not _
                        gobj.ObjectType = ObjectType.GO_Table And Not _
                        gobj.ObjectType = ObjectType.GO_Image And Not _
                        gobj.ObjectType = ObjectType.GO_Text And Not _
                        gobj.ObjectType = ObjectType.GO_Rectangle And Not _
                        gobj.ObjectType = ObjectType.Nenhum Then

                    If gobj.Calculated Then

                        If Flowsheet.Collections.FlowsheetObjectCollection.ContainsKey(gobj.Name) Then

                            Dim obj = Flowsheet.SimulationObjects(gobj.Name)

                            Dim tabela As New QuickTableGraphic(obj, px2.X + 5, px2.Y + 5)
                            tabela.Owner = obj
                            tabela.Tag = obj.Name
                            tabela.Name = "QTAB-" & Guid.NewGuid.ToString
                            tabela.HeaderText = gobj.Tag
                            tabela.AdditionalInfo = Me.FlowsheetDesignSurface.Zoom
                            Me.m_qt = tabela
                            If Not Me.m_qt Is Nothing Then
                                If FlowsheetDesignSurface.AutoScrollPosition.X + px2.X * FlowsheetDesignSurface.Zoom + m_qt.Width * FlowsheetDesignSurface.Zoom > FlowsheetDesignSurface.ClientRectangle.Width Then
                                    px2.X -= 50 + m_qt.Width / FlowsheetDesignSurface.Zoom
                                End If
                                If FlowsheetDesignSurface.AutoScrollPosition.Y + px2.Y * FlowsheetDesignSurface.Zoom + m_qt.Height * FlowsheetDesignSurface.Zoom > FlowsheetDesignSurface.ClientRectangle.Height Then
                                    px2.Y -= 50 + m_qt.Height / FlowsheetDesignSurface.Zoom
                                End If
                                Me.m_qt.SetPosition(px2.ToDTPoint)
                            End If
                            Me.FlowsheetDesignSurface.drawingObjects.Add(tabela)
                            Me.ticks = 0

                        Else

                            Me.m_qt.AdditionalInfo = Me.FlowsheetDesignSurface.Zoom

                            If Not Me.m_qt Is Nothing Then
                                If FlowsheetDesignSurface.AutoScrollPosition.X + px2.X * FlowsheetDesignSurface.Zoom + m_qt.Width * FlowsheetDesignSurface.Zoom > FlowsheetDesignSurface.ClientRectangle.Width Then
                                    px2.X -= 50 + m_qt.Width / FlowsheetDesignSurface.Zoom
                                End If
                                If FlowsheetDesignSurface.AutoScrollPosition.Y + px2.Y * FlowsheetDesignSurface.Zoom + m_qt.Height * FlowsheetDesignSurface.Zoom > FlowsheetDesignSurface.ClientRectangle.Height Then
                                    px2.Y -= 50 + m_qt.Height / FlowsheetDesignSurface.Zoom
                                End If
                                Me.m_qt.SetPosition(px2.ToDTPoint)
                            End If

                        End If

                    End If

                ElseIf gobj.ObjectType = ObjectType.GO_FloatingTable Then

                    If Me.FlowsheetDesignSurface.drawingObjects.Contains(Me.m_qt) Then
                        Me.FlowsheetDesignSurface.drawingObjects.Remove(Me.m_qt)
                    End If
                    Me.m_qt = Nothing
                    Me.ticks = 0

                End If

            Else

                Try
                    If Me.FlowsheetDesignSurface.drawingObjects.Contains(Me.m_qt) Then
                        Me.FlowsheetDesignSurface.drawingObjects.Remove(Me.m_qt)
                    End If
                    Me.m_qt = Nothing
                    Me.ticks = 0
                Catch ex As Exception
                    Console.WriteLine(ex.Message)
                End Try

            End If

        Else

            Try
                If Me.FlowsheetDesignSurface.drawingObjects.Contains(Me.m_qt) Then
                    Me.FlowsheetDesignSurface.drawingObjects.Remove(Me.m_qt)
                End If
                Me.m_qt = Nothing
                Me.ticks = 0
            Catch ex As Exception
                Console.WriteLine(ex.Message)
            End Try

        End If

        'If Not Me.m_qt Is Nothing Then Me.m_qt.SetPosition(px2)
        Me.FlowsheetDesignSurface.Invalidate()

    End Sub

    Private Sub CMS_Sel_Enter(ByVal sender As Object, ByVal e As System.EventArgs) Handles CMS_Sel.Opened

        Dim naoLimparListaDeDesconectar As Boolean = False

        Me.CMS_ItemsToDisconnect.Items.Clear()
        Me.CMS_ItemsToConnect.Items.Clear()
        Me.CopyFromTSMI.DropDownItems.Clear()

        Me.DesconectarDeToolStripMenuItem.Visible = False
        Me.ConectarAToolStripMenuItem.Visible = False
        Me.ToolStripSeparator3.Visible = False
        Me.CopyFromTSMI.Visible = False
      
        DepurarObjetoToolStripMenuItem.Visible = Flowsheet.Collections.FlowsheetObjectCollection.ContainsKey(Me.FlowsheetDesignSurface.SelectedObject.Name)

        If Me.FlowsheetDesignSurface.SelectedObject.ObjectType <> ObjectType.GO_Image And _
            Me.FlowsheetDesignSurface.SelectedObject.ObjectType <> ObjectType.GO_Table And _
            Me.FlowsheetDesignSurface.SelectedObject.ObjectType <> ObjectType.GO_MasterTable And _
            Me.FlowsheetDesignSurface.SelectedObject.ObjectType <> ObjectType.GO_SpreadsheetTable And _
            Me.FlowsheetDesignSurface.SelectedObject.ObjectType <> ObjectType.GO_FloatingTable And _
            Me.FlowsheetDesignSurface.SelectedObject.ObjectType <> ObjectType.DistillationColumn And _
            Me.FlowsheetDesignSurface.SelectedObject.ObjectType <> ObjectType.AbsorptionColumn And _
            Me.FlowsheetDesignSurface.SelectedObject.ObjectType <> ObjectType.ReboiledAbsorber And _
            Me.FlowsheetDesignSurface.SelectedObject.ObjectType <> ObjectType.RefluxedAbsorber And _
            Me.FlowsheetDesignSurface.SelectedObject.ObjectType <> ObjectType.GO_Rectangle And _
            Me.FlowsheetDesignSurface.SelectedObject.ObjectType <> ObjectType.GO_Text Then

            Me.RecalcularToolStripMenuItem.Visible = True
            Me.ToolStripSeparator6.Visible = True
            Me.ClonarToolStripMenuItem.Visible = True
            Me.ExcluirToolStripMenuItem.Visible = True
            Me.HorizontalmenteToolStripMenuItem.Visible = True

            Try

                Dim obj As SharedClasses.UnitOperations.BaseClass = Flowsheet.Collections.FlowsheetObjectCollection(Me.FlowsheetDesignSurface.SelectedObject.Name)

                If Me.IsObjectDownstreamConnectable(obj.GraphicObject.Tag) Then
                    Dim arr As ArrayList = Me.ReturnDownstreamConnectibles(obj.GraphicObject.Tag)
                    Me.CMS_ItemsToConnect.Items.Clear()
                    If arr.Count <> 0 Then
                        Dim i As Integer = 0
                        Do
                            Me.CMS_ItemsToConnect.Items.Add(arr(i))
                            i = i + 1
                            Me.ConectarAToolStripMenuItem.Visible = True
                            Me.ToolStripSeparator3.Visible = True
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
                        Me.ToolStripSeparator3.Visible = True
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
                        Me.ToolStripSeparator3.Visible = True
                        Me.DesconectarDeToolStripMenuItem.DropDown = Me.CMS_ItemsToDisconnect
                    End If
                End If

                If obj.GraphicObject.FlippedH Then
                    Me.HorizontalmenteToolStripMenuItem.Checked = True
                Else
                    Me.HorizontalmenteToolStripMenuItem.Checked = False
                End If

                If Me.FlowsheetDesignSurface.SelectedObject.ObjectType = ObjectType.MaterialStream Then

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
                        For Each mstr As Thermodynamics.Streams.MaterialStream In Flowsheet.Collections.FlowsheetObjectCollection.Values
                            If mstr.GraphicObject.Tag <> obj.GraphicObject.Tag Then
                                Dim newtsmi As New ToolStripMenuItem(mstr.GraphicObject.Tag)
                                If mstr.GraphicObject.Calculated Then CopyFromTSMI.DropDownItems.Add(newtsmi)
                            End If
                        Next
                    End If

                End If

            Catch ex As Exception
                CMS_Sel.Hide()
            End Try

        ElseIf Me.FlowsheetDesignSurface.SelectedObject.ObjectType = ObjectType.AbsorptionColumn Or _
        Me.FlowsheetDesignSurface.SelectedObject.ObjectType = ObjectType.DistillationColumn Or _
        Me.FlowsheetDesignSurface.SelectedObject.ObjectType = ObjectType.ReboiledAbsorber Or _
        Me.FlowsheetDesignSurface.SelectedObject.ObjectType = ObjectType.RefluxedAbsorber Then

            Me.RecalcularToolStripMenuItem.Visible = True
            Me.ToolStripSeparator6.Visible = True

            Me.ConectarAToolStripMenuItem.Visible = False
            Me.DesconectarDeToolStripMenuItem.Visible = False
            Me.ClonarToolStripMenuItem.Visible = True
            Me.ExcluirToolStripMenuItem.Visible = True
            Me.HorizontalmenteToolStripMenuItem.Visible = True
            Dim obj As SharedClasses.UnitOperations.BaseClass = Flowsheet.Collections.FlowsheetObjectCollection(Me.FlowsheetDesignSurface.SelectedObject.Name)

            If obj.GraphicObject.FlippedH Then
                Me.HorizontalmenteToolStripMenuItem.Checked = True
            Else
                Me.HorizontalmenteToolStripMenuItem.Checked = False
            End If

        Else

            Me.TSMI_Label.Text = "Tabela"
            Me.ClonarToolStripMenuItem.Visible = False
            Me.HorizontalmenteToolStripMenuItem.Visible = False
            Me.ExcluirToolStripMenuItem.Visible = False
            Me.RecalcularToolStripMenuItem.Visible = False
            Me.ToolStripSeparator6.Visible = False

        End If
        'Me.InverterToolStripMenuItem.Visible = False

    End Sub

    Sub MaterialStreamClickHandler(ByVal sender As System.Object, ByVal e As ToolStripItemClickedEventArgs)

        Dim obj1 As Thermodynamics.Streams.MaterialStream = Flowsheet.Collections.FlowsheetObjectCollection(Me.FlowsheetDesignSurface.SelectedObject.Name)

        Dim obj2 As Thermodynamics.Streams.MaterialStream = Flowsheet.GetFlowsheetSimulationObject(e.ClickedItem.Text)

        obj1.Assign(obj2)

        CMS_Sel.Hide()

        Application.DoEvents()

        FlowsheetSolver.FlowsheetSolver.CalculateObject(Flowsheet, obj1.Name)

    End Sub

    Private Sub ToolStripMenuItem5_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ToolStripMenuItem5.Click
        PreviewDialog.ShowDialog()
    End Sub

    Private Sub Timer1_Tick(ByVal sender As Object, ByVal e As System.EventArgs) Handles Timer1.Tick
        Me.ticks += 1
    End Sub

    Public Sub ClonarToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ClonarToolStripMenuItem.Click
        CloneObject(Me.FlowsheetDesignSurface.SelectedObject)
    End Sub

    Public Sub CloneObject(gobj As GraphicObject)

        Flowsheet = My.Application.ActiveSimulation

        Dim obj As SharedClasses.UnitOperations.BaseClass = Flowsheet.Collections.FlowsheetObjectCollection(gobj.Name)
        Dim newobj As SharedClasses.UnitOperations.BaseClass = obj.Clone

        Dim searchtext As String = gobj.Tag.Split("(")(0).Trim()

        Dim objcount As Integer = (From go As GraphicObject In Me.FlowsheetDesignSurface.drawingObjects Select go Where go.Tag.Contains(searchtext)).Count

        Dim mpx = Me.FlowsheetDesignSurface.SelectedObject.X + Me.FlowsheetDesignSurface.SelectedObject.Width * 1.1
        Dim mpy = Me.FlowsheetDesignSurface.SelectedObject.Y + Me.FlowsheetDesignSurface.SelectedObject.Height * 1.1

        Select Case Me.FlowsheetDesignSurface.SelectedObject.ObjectType

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
                Me.FlowsheetDesignSurface.drawingObjects.Add(myDWOBJ.GraphicObject)
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
                Me.FlowsheetDesignSurface.drawingObjects.Add(myDWOBJ.GraphicObject)
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
                Me.FlowsheetDesignSurface.drawingObjects.Add(myDWOBJ.GraphicObject)
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
                Me.FlowsheetDesignSurface.drawingObjects.Add(myDWOBJ.GraphicObject)
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
                Me.FlowsheetDesignSurface.drawingObjects.Add(myDWOBJ.GraphicObject)
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
                Me.FlowsheetDesignSurface.drawingObjects.Add(myDWOBJ.GraphicObject)
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
                Me.FlowsheetDesignSurface.drawingObjects.Add(myDWOBJ.GraphicObject)
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
                Me.FlowsheetDesignSurface.drawingObjects.Add(myDWOBJ.GraphicObject)
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
                Me.FlowsheetDesignSurface.drawingObjects.Add(myDWOBJ.GraphicObject)
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
                Me.FlowsheetDesignSurface.drawingObjects.Add(myDWOBJ.GraphicObject)
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
                Me.FlowsheetDesignSurface.drawingObjects.Add(myDWOBJ.GraphicObject)
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
                Me.FlowsheetDesignSurface.drawingObjects.Add(myDWOBJ.GraphicObject)
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
                Me.FlowsheetDesignSurface.drawingObjects.Add(myDWOBJ.GraphicObject)
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
                Me.FlowsheetDesignSurface.drawingObjects.Add(myDWOBJ.GraphicObject)
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
                Me.FlowsheetDesignSurface.drawingObjects.Add(myDWOBJ.GraphicObject)
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
                Me.FlowsheetDesignSurface.drawingObjects.Add(myDWOBJ.GraphicObject)
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
                Me.FlowsheetDesignSurface.drawingObjects.Add(myDWOBJ.GraphicObject)
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
                Me.FlowsheetDesignSurface.drawingObjects.Add(myDWOBJ.GraphicObject)
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
                Me.FlowsheetDesignSurface.drawingObjects.Add(myDWOBJ.GraphicObject)
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
                Me.FlowsheetDesignSurface.drawingObjects.Add(myDWOBJ.GraphicObject)
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
                Me.FlowsheetDesignSurface.drawingObjects.Add(myDWOBJ.GraphicObject)
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
                Me.FlowsheetDesignSurface.drawingObjects.Add(myDWOBJ.GraphicObject)
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
                Me.FlowsheetDesignSurface.drawingObjects.Add(myDWOBJ.GraphicObject)
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
                Me.FlowsheetDesignSurface.drawingObjects.Add(myDWOBJ.GraphicObject)
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
                Me.FlowsheetDesignSurface.drawingObjects.Add(myDWOBJ.GraphicObject)
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
                Me.FlowsheetDesignSurface.drawingObjects.Add(myDWOBJ.GraphicObject)
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
                Me.FlowsheetDesignSurface.drawingObjects.Add(myDWOBJ.GraphicObject)
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
                Me.FlowsheetDesignSurface.drawingObjects.Add(myDWOBJ.GraphicObject)
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
                Me.FlowsheetDesignSurface.drawingObjects.Add(myDWOBJ.GraphicObject)
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
                Me.FlowsheetDesignSurface.drawingObjects.Add(myDWOBJ.GraphicObject)
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
                Me.FlowsheetDesignSurface.drawingObjects.Add(myDWOBJ.GraphicObject)
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
                Me.FlowsheetDesignSurface.drawingObjects.Add(myDWOBJ.GraphicObject)
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
                Me.FlowsheetDesignSurface.drawingObjects.Add(myDWOBJ.GraphicObject)
            Case ObjectType.CapeOpenUO, ObjectType.FlowsheetUO
                MessageBox.Show("Cloning is not supported by CAPE-OPEN/Flowsheet Unit Operations.", "Error", MessageBoxButtons.OK, MessageBoxIcon.Error)
        End Select
        Me.FlowsheetDesignSurface.Invalidate()

    End Sub

    Private Sub CMS_ItemsToDisconnect_ItemClicked(ByVal sender As System.Object, ByVal e As System.Windows.Forms.ToolStripItemClickedEventArgs) Handles CMS_ItemsToDisconnect.ItemClicked

        Me.Flowsheet.DisconnectObject(Me.FlowsheetDesignSurface.SelectedObject, FormFlowsheet.SearchSurfaceObjectsByTag(e.ClickedItem.Text, Me.FlowsheetDesignSurface), True)

    End Sub

    Private Sub CMS_ItemsToConnect_ItemClicked(ByVal sender As System.Object, ByVal e As System.Windows.Forms.ToolStripItemClickedEventArgs) Handles CMS_ItemsToConnect.ItemClicked

        Call Me.Flowsheet.ConnectObject(Me.FlowsheetDesignSurface.SelectedObject, FormFlowsheet.SearchSurfaceObjectsByTag(e.ClickedItem.Text, Me.FlowsheetDesignSurface))

    End Sub

    Function IsObjectDownstreamConnectable(ByVal objTag As String) As Boolean

        Dim obj As GraphicObject = FormFlowsheet.SearchSurfaceObjectsByTag(objTag, Me.FlowsheetDesignSurface)

        If Not obj Is Nothing Then

            Dim cp As ConnectionPoint
            For Each cp In obj.OutputConnectors
                If cp.IsAttached = False Then Return True
            Next

        End If

        Return False

    End Function

    Function IsObjectUpstreamConnectable(ByVal objTag As String) As Boolean

        Dim obj As GraphicObject = FormFlowsheet.SearchSurfaceObjectsByTag(objTag, Me.FlowsheetDesignSurface)

        If Not obj Is Nothing Then

            Dim cp As ConnectionPoint
            For Each cp In obj.InputConnectors
                If cp.IsAttached = False Then Return True
            Next

        End If

        Return False

    End Function

    Function ReturnDownstreamConnectibles(ByVal objtag As String)

        Dim refobj As GraphicObject = FormFlowsheet.SearchSurfaceObjectsByTag(objtag, Me.FlowsheetDesignSurface)

        Dim obj As SharedClasses.UnitOperations.BaseClass
        Dim cp As ConnectionPoint

        Dim conables As New ArrayList

        For Each obj In Me.Flowsheet.Collections.FlowsheetObjectCollection.Values
            If obj.GraphicObject.Tag <> refobj.Tag Then
                If obj.GraphicObject.ObjectType <> ObjectType.GO_Text And _
                    obj.GraphicObject.ObjectType <> ObjectType.GO_FloatingTable And _
                    obj.GraphicObject.ObjectType <> ObjectType.GO_MasterTable And _
                    obj.GraphicObject.ObjectType <> ObjectType.GO_SpreadsheetTable And _
                    obj.GraphicObject.ObjectType <> ObjectType.GO_Table And _
                    obj.GraphicObject.ObjectType <> ObjectType.GO_Rectangle And _
                    obj.GraphicObject.ObjectType <> ObjectType.OT_Adjust And _
                    obj.GraphicObject.ObjectType <> ObjectType.OT_Spec And _
                    obj.GraphicObject.ObjectType <> ObjectType.DistillationColumn And _
                    obj.GraphicObject.ObjectType <> ObjectType.AbsorptionColumn And _
                    obj.GraphicObject.ObjectType <> ObjectType.RefluxedAbsorber And _
                    obj.GraphicObject.ObjectType <> ObjectType.ReboiledAbsorber And _
                    obj.GraphicObject.ObjectType <> ObjectType.Nenhum Then

                    If refobj.ObjectType = ObjectType.MaterialStream Then
                        For Each cp In obj.GraphicObject.InputConnectors
                            If Not cp.IsAttached And Not conables.Contains(obj.GraphicObject.Tag) And Not _
                            obj.GraphicObject.ObjectType = ObjectType.MaterialStream And Not _
                            obj.GraphicObject.ObjectType = ObjectType.EnergyStream And _
                            cp.Type = ConType.ConIn Then conables.Add(obj.GraphicObject.Tag)
                        Next
                    ElseIf refobj.ObjectType = ObjectType.EnergyStream Then
                        If obj.GraphicObject.ObjectType <> ObjectType.Heater And _
                        obj.GraphicObject.ObjectType <> ObjectType.Pump And _
                        obj.GraphicObject.ObjectType <> ObjectType.Compressor And _
                        obj.GraphicObject.ObjectType <> ObjectType.MaterialStream Then
                            cp = obj.GraphicObject.EnergyConnector
                            If Not cp.IsAttached And Not conables.Contains(obj.GraphicObject.Tag) Then conables.Add(obj.GraphicObject.Tag)
                        ElseIf obj.GraphicObject.ObjectType = ObjectType.MaterialStream Then

                        Else
                            For Each cp In obj.GraphicObject.InputConnectors
                                If Not cp.IsAttached And Not conables.Contains(obj.GraphicObject.Tag) And Not _
                                obj.GraphicObject.ObjectType = ObjectType.MaterialStream And Not _
                                obj.GraphicObject.ObjectType = ObjectType.EnergyStream And _
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

        Dim obj As GraphicObject = FormFlowsheet.SearchSurfaceObjectsByTag(objTag, Me.FlowsheetDesignSurface)

        Dim conables As New ArrayList

        If Not obj Is Nothing Then

            Dim cp As ConnectionPoint
            For Each cp In obj.OutputConnectors
                If cp.AttachedConnector.AttachedTo.ObjectType <> ObjectType.AbsorptionColumn And cp.AttachedConnector.AttachedTo.ObjectType <> ObjectType.DistillationColumn And _
                cp.AttachedConnector.AttachedTo.ObjectType <> ObjectType.RefluxedAbsorber And cp.AttachedConnector.AttachedTo.ObjectType <> ObjectType.ReboiledAbsorber Then
                    If cp.IsAttached = True And Not conables.Contains(cp.AttachedConnector.AttachedTo.Tag) Then conables.Add(cp.AttachedConnector.AttachedTo.Tag)
                End If
            Next
        End If

        Return conables

    End Function

    Function ReturnUpstreamDisconnectables(ByVal objTag As String) As ArrayList

        Dim obj As GraphicObject = FormFlowsheet.SearchSurfaceObjectsByTag(objTag, Me.FlowsheetDesignSurface)

        Dim conables As New ArrayList

        If Not obj Is Nothing Then

            Dim cp As ConnectionPoint
            For Each cp In obj.InputConnectors
                If cp.AttachedConnector.AttachedTo.ObjectType <> ObjectType.AbsorptionColumn And cp.AttachedConnector.AttachedTo.ObjectType <> ObjectType.DistillationColumn And _
                cp.AttachedConnector.AttachedTo.ObjectType <> ObjectType.RefluxedAbsorber And cp.AttachedConnector.AttachedTo.ObjectType <> ObjectType.ReboiledAbsorber Then
                    If cp.IsAttached = True And Not conables.Contains(cp.AttachedConnector.AttachedFrom.Tag) Then conables.Add(cp.AttachedConnector.AttachedFrom.Tag)
                End If
            Next

        End If

        Return conables

    End Function

    Private Sub ExcluirToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ExcluirToolStripMenuItem.Click
        Call Me.Flowsheet.DeleteSelectedObject(sender, e, Me.FlowsheetDesignSurface.SelectedObject)
    End Sub

    Private Sub HorizontalmenteToolStripMenuItem_Click_1(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles HorizontalmenteToolStripMenuItem.Click
        If Me.HorizontalmenteToolStripMenuItem.Checked Then
            Me.FlowsheetDesignSurface.SelectedObject.FlippedH = True
        Else
            Me.FlowsheetDesignSurface.SelectedObject.FlippedH = False
        End If
        Me.FlowsheetDesignSurface.Invalidate()
    End Sub

    Public Function AddObjectToSurface(ByVal type As ObjectType, ByVal x As Integer, ByVal y As Integer, Optional ByVal tag As String = "", Optional ByVal id As String = "") As String

        Flowsheet = My.Application.ActiveSimulation

        Dim gObj As GraphicObject = Nothing
        Dim fillclr As Color = Color.WhiteSmoke
        Dim lineclr As Color = Color.Red
        Dim mpx = x '- SplitContainer1.SplitterDistance
        Dim mpy = y '- ToolStripContainer1.TopToolStripPanel.Height

        Flowsheet.WriteToLog(DWSIM.App.GetLocalTipString("FLSH004"), Color.Black, DWSIM.Flowsheet.MessageType.Tip)
        Flowsheet.WriteToLog(DWSIM.App.GetLocalTipString("FLSH006"), Color.Black, DWSIM.Flowsheet.MessageType.Tip)

        Select Case type

            Case ObjectType.OT_Adjust

                Dim myNode As New AdjustGraphic(mpx, mpy, 20, 20, 0)
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
                Flowsheet.WriteToLog(DWSIM.App.GetLocalTipString("ADJT001"), Color.Black, DWSIM.Flowsheet.MessageType.Tip)

            Case ObjectType.OT_Spec

                Dim myNode As New SpecGraphic(mpx, mpy, 20, 20, 0)
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

                Dim myNode As New RecycleGraphic(mpx, mpy, 20, 20, 0)
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

                Flowsheet.WriteToLog(DWSIM.App.GetLocalTipString("RECY001"), Color.Black, DWSIM.Flowsheet.MessageType.Tip)

            Case ObjectType.OT_EnergyRecycle

                Dim myNode As New EnergyRecycleGraphic(mpx, mpy, 20, 20, 0)
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

                Dim myNode As New NodeInGraphic(mpx, mpy, 20, 20, 0)
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

                Dim myNodeo As New NodeOutGraphic(mpx, mpy, 20, 20, 0)
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

                Dim myPump As New PumpGraphic(mpx, mpy, 25, 25, 0)
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

                Dim myTank As New TankGraphic(mpx, mpy, 50, 50, 0)
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

                Dim myVessel As New VesselGraphic(mpx, mpy, 50, 70, 0)
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

            Case ObjectType.TPVessel

                Dim myVessel As New TPVesselGraphic(mpx, mpy, 50, 50, 0)
                myVessel.LineWidth = 2
                myVessel.Fill = True
                myVessel.FillColor = fillclr
                myVessel.LineColor = lineclr
                myVessel.Tag = "SEPTF-" & Format(Flowsheet.Collections.FlowsheetObjectCollection.Count, "00#")
                If tag <> "" Then myVessel.Tag = tag
                gObj = myVessel
                gObj.Name = "SEPTF-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                Flowsheet.Collections.GraphicObjectCollection.Add(gObj.Name, myVessel)

            Case ObjectType.MaterialStream

                Dim myMStr As New MaterialStreamGraphic(mpx, mpy, 20, 20, 0)
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

                Flowsheet.WriteToLog(DWSIM.App.GetLocalTipString("MSTR001"), Color.Black, DWSIM.Flowsheet.MessageType.Tip)
                Flowsheet.WriteToLog(DWSIM.App.GetLocalTipString("MSTR002"), Color.Black, DWSIM.Flowsheet.MessageType.Tip)
                Flowsheet.WriteToLog(DWSIM.App.GetLocalTipString("MSTR003"), Color.Black, DWSIM.Flowsheet.MessageType.Tip)

            Case ObjectType.EnergyStream

                Dim myMStr As New EnergyStreamGraphic(mpx, mpy, 20, 20, 0)
                myMStr.LineWidth = 2
                myMStr.Fill = True
                myMStr.FillColor = Color.LightYellow
                myMStr.LineColor = lineclr
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

                Dim myComp As New CompressorGraphic(mpx, mpy, 25, 25, 0)
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

                Dim myComp As New TurbineGraphic(mpx, mpy, 25, 25, 0)
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

                Dim myCool As New CoolerGraphic(mpx, mpy, 25, 25, 0)
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

                Dim myHeat As New HeaterGraphic(mpx, mpy, 25, 25, 0)
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

                Dim myPipe As New PipeGraphic(mpx, mpy, 50, 10, 0)
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

                Dim myValve As New ValveGraphic(mpx, mpy, 20, 20, 0)
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

                Dim myRconv As New ReactorConversionGraphic(mpx, mpy, 50, 50, 0)
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

                Flowsheet.WriteToLog(DWSIM.App.GetLocalTipString("RCON001"), Color.Black, DWSIM.Flowsheet.MessageType.Tip)

            Case ObjectType.RCT_Equilibrium

                Dim myReq As New ReactorEquilibriumGraphic(mpx, mpy, 50, 50, 0)
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

                Flowsheet.WriteToLog(DWSIM.App.GetLocalTipString("REQL001"), Color.Black, DWSIM.Flowsheet.MessageType.Tip)

            Case ObjectType.RCT_Gibbs

                Dim myRgibbs As New ReactorGibbsGraphic(mpx, mpy, 50, 50, 0)
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

                Flowsheet.WriteToLog(DWSIM.App.GetLocalTipString("RGIB001"), Color.Black, DWSIM.Flowsheet.MessageType.Tip)

            Case ObjectType.RCT_CSTR

                Dim myRcstr As New ReactorCSTRGraphic(mpx, mpy, 50, 50, 0)
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

                Flowsheet.WriteToLog(DWSIM.App.GetLocalTipString("CSTR001"), Color.Black, DWSIM.Flowsheet.MessageType.Tip)

            Case ObjectType.RCT_PFR

                Dim myRpfr As New ReactorPFRGraphic(mpx, mpy, 70, 20, 0)
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

                Flowsheet.WriteToLog(DWSIM.App.GetLocalTipString("PFR001"), Color.Black, DWSIM.Flowsheet.MessageType.Tip)

            Case ObjectType.HeatExchanger

                Dim myHeatExchanger As New HeatExchangerGraphic(mpx, mpy, 30, 30, 0)
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

                Dim mySC As New ShorcutColumnGraphic(mpx, mpy, 144, 180, 0)
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

                Dim mySC As New DistillationColumnGraphic(mpx, mpy, 144, 180, 0)
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

                Flowsheet.WriteToLog(DWSIM.App.GetLocalTipString("DCOL001"), Color.Black, DWSIM.Flowsheet.MessageType.Tip)
                Flowsheet.WriteToLog(DWSIM.App.GetLocalTipString("DCOL002"), Color.Black, DWSIM.Flowsheet.MessageType.Tip)
                Flowsheet.WriteToLog(DWSIM.App.GetLocalTipString("DCOL003"), Color.Black, DWSIM.Flowsheet.MessageType.Tip)

            Case ObjectType.AbsorptionColumn

                Dim mySC As New AbsorptionColumnGraphic(mpx, mpy, 144, 180, 0)
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

                Flowsheet.WriteToLog(DWSIM.App.GetLocalTipString("DCOL001"), Color.Black, DWSIM.Flowsheet.MessageType.Tip)
                Flowsheet.WriteToLog(DWSIM.App.GetLocalTipString("DCOL002"), Color.Black, DWSIM.Flowsheet.MessageType.Tip)
                Flowsheet.WriteToLog(DWSIM.App.GetLocalTipString("DCOL003"), Color.Black, DWSIM.Flowsheet.MessageType.Tip)

            Case ObjectType.ReboiledAbsorber

                Dim mySC As New ReboiledAbsorberGraphic(mpx, mpy, 144, 180, 0)
                mySC.LineWidth = 2
                mySC.Fill = True
                mySC.FillColor = fillclr
                mySC.LineColor = lineclr
                mySC.Tag = "RBA-" & Format(Flowsheet.Collections.FlowsheetObjectCollection.Count, "00#")
                If tag <> "" Then mySC.Tag = tag
                gObj = mySC
                gObj.Name = "RBA-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                Flowsheet.Collections.GraphicObjectCollection.Add(gObj.Name, mySC)
                'OBJETO DWSIM
                Dim myCOSC As ReboiledAbsorber = New ReboiledAbsorber(mySC.Name, "ReboiledAbsorber", Flowsheet)
                myCOSC.GraphicObject = mySC
                Flowsheet.Collections.FlowsheetObjectCollection.Add(mySC.Name, myCOSC)

                Flowsheet.WriteToLog(DWSIM.App.GetLocalTipString("DCOL001"), Color.Black, DWSIM.Flowsheet.MessageType.Tip)
                Flowsheet.WriteToLog(DWSIM.App.GetLocalTipString("DCOL002"), Color.Black, DWSIM.Flowsheet.MessageType.Tip)
                Flowsheet.WriteToLog(DWSIM.App.GetLocalTipString("DCOL003"), Color.Black, DWSIM.Flowsheet.MessageType.Tip)

            Case ObjectType.RefluxedAbsorber

                Dim mySC As New RefluxedAbsorberGraphic(mpx, mpy, 144, 180, 0)
                mySC.LineWidth = 2
                mySC.Fill = True
                mySC.FillColor = fillclr
                mySC.LineColor = lineclr
                mySC.Tag = "RFA-" & Format(Flowsheet.Collections.FlowsheetObjectCollection.Count, "00#")
                If tag <> "" Then mySC.Tag = tag
                gObj = mySC
                gObj.Name = "RFA-" & Guid.NewGuid.ToString
                If id <> "" Then gObj.Name = id
                Flowsheet.Collections.GraphicObjectCollection.Add(gObj.Name, mySC)
                'OBJETO DWSIM
                Dim myCOSC As RefluxedAbsorber = New RefluxedAbsorber(mySC.Name, "RefluxedAbsorber", Flowsheet)
                myCOSC.GraphicObject = mySC
                Flowsheet.Collections.FlowsheetObjectCollection.Add(mySC.Name, myCOSC)

                Flowsheet.WriteToLog(DWSIM.App.GetLocalTipString("DCOL001"), Color.Black, DWSIM.Flowsheet.MessageType.Tip)
                Flowsheet.WriteToLog(DWSIM.App.GetLocalTipString("DCOL002"), Color.Black, DWSIM.Flowsheet.MessageType.Tip)
                Flowsheet.WriteToLog(DWSIM.App.GetLocalTipString("DCOL003"), Color.Black, DWSIM.Flowsheet.MessageType.Tip)

            Case ObjectType.ComponentSeparator

                Dim myCSep As New ComponentSeparatorGraphic(mpx, mpy, 50, 50, 0)
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

                Dim myCSep As New SolidSeparatorGraphic(mpx, mpy, 50, 50, 0)
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

                Dim myCSep As New FilterGraphic(mpx, mpy, 50, 50, 0)
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

                Dim myOPL As New OrificePlateGraphic(mpx, mpy, 25, 25, 0)
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

                Dim myCUO As New CustomUOGraphic(mpx, mpy, 25, 25, 0)
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

                Flowsheet.WriteToLog(DWSIM.App.GetLocalTipString("CSUO001"), Color.Black, DWSIM.Flowsheet.MessageType.Tip)

            Case ObjectType.ExcelUO

                Dim myEUO As New ExcelUOGraphic(mpx, mpy, 25, 25, 0)
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

                Dim myEUO As New FlowsheetUOGraphic(mpx, mpy, 25, 25, 0)
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
                Dim myCOEUO As Flowsheet = New Flowsheet(myEUO.Name, "FlowsheetUnitOp")
                myCOEUO.GraphicObject = myEUO
                Flowsheet.Collections.FlowsheetObjectCollection.Add(myEUO.Name, myCOEUO)

            Case ObjectType.CapeOpenUO

                Dim myCUO As New CapeOpenUOGraphic(mpx, mpy, 40, 40, 0)
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
                Dim myCOCUO As CapeOpenUO = New CapeOpenUO(myCUO.Name, "CapeOpenUnitOperation", gObj)
                myCOCUO.GraphicObject = myCUO
                Flowsheet.Collections.FlowsheetObjectCollection.Add(myCUO.Name, myCOCUO)

                Flowsheet.WriteToLog(DWSIM.App.GetLocalTipString("CAPE001"), Color.Black, DWSIM.Flowsheet.MessageType.Tip)

        End Select

        If Not gObj Is Nothing Then
            gObj.Owner = Me.Flowsheet.SimulationObjects(gObj.Name)
            Me.Flowsheet.SimulationObjects(gObj.Name).SetFlowsheet(Flowsheet)
            Me.FlowsheetDesignSurface.drawingObjects.Add(gObj)
            Me.FlowsheetDesignSurface.Invalidate()
            Application.DoEvents()
            If My.Application.PushUndoRedoAction Then Flowsheet.AddUndoRedoAction(New DWSIM.Flowsheet.UndoRedoAction() With {.AType = DWSIM.Flowsheet.UndoRedoActionType.ObjectAdded,
                                     .ObjID = gObj.Name,
                                     .NewValue = gObj,
                                     .Name = String.Format(DWSIM.App.GetLocalString("UndoRedo_ObjectAdded"), gObj.Tag)})
        End If

        Me.FlowsheetDesignSurface.Cursor = Cursors.Arrow

        Return gObj.Name

    End Function

    Private Sub FlowsheetDesignSurface_DragEnter(ByVal sender As System.Object, ByVal e As System.Windows.Forms.DragEventArgs) Handles FlowsheetDesignSurface.DragEnter
        Dim i As Integer
        For i = 0 To e.Data.GetFormats().Length - 1
            Dim format = e.Data.GetFormats()(i)
            If format.Equals("System.RuntimeType") Or format.Equals("System.MonoType") Then
                e.Effect = DragDropEffects.All
            End If
        Next
    End Sub

    Private Sub FlowsheetDesignSurface_DragDrop(ByVal sender As System.Object, ByVal e As System.Windows.Forms.DragEventArgs) Handles FlowsheetDesignSurface.DragDrop

        If e.Effect = DragDropEffects.All Then
            Dim obj As Type = Nothing
            If DWSIM.App.IsRunningOnMono Then
                obj = e.Data.GetData("System.MonoType")
            Else
                obj = e.Data.GetData("System.RuntimeType")
            End If
            Dim tobj As ObjectType = ObjectType.Nenhum
            Dim p As Drawing.Point = Me.FlowsheetDesignSurface.PointToClient(New Drawing.Point(e.X, e.Y))
            Dim mousePT As Drawing.Point = Flowsheet.gscTogoc(p.X, p.Y)
            Dim mpx = mousePT.X - 40
            Dim mpy = mousePT.Y - 40
            Select Case obj.Name
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

            AddObjectToSurface(tobj, mpx, mpy)

            If tobj = ObjectType.MaterialStream And Not My.Computer.Keyboard.ShiftKeyDown Then

            End If

        End If

    End Sub

    Public calcstart As Date

    Public Sub RecalcularToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles RecalcularToolStripMenuItem.Click

        Me.Flowsheet.tsmiRecalc_Click(sender, e)

    End Sub

    Private Sub ToolStripMenuItem8_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ToolStripMenuItem8.Click
        DrawToBitmapScaled(2)
    End Sub

    Private Sub ToolStripMenuItem4_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ToolStripMenuItem4.Click
        DrawToBitmapScaled(1)
    End Sub

    Private Sub ToolStripMenuItem1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ToolStripMenuItem1.Click
        DrawToBitmapScaled(0.5)
    End Sub

    Private Sub ToolStripMenuItem10_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ToolStripMenuItem10.Click
        DrawToBitmapScaled(3)
    End Sub

    Sub DrawToBitmapScaled(ByVal scale As Double)

        If DWSIM.App.IsRunningOnMono Then
            MessageBox.Show(DWSIM.App.GetLocalString("Unsupported_Feature"), "DWSIM", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)
            Exit Sub
        End If

        Dim rect As Rectangle = New Rectangle(0, 0, scale * (Me.FlowsheetDesignSurface.Width - 14), scale * (Me.FlowsheetDesignSurface.Height - 14))
        Dim img As Image = New Bitmap(rect.Width, rect.Height)
        Dim g As Graphics = Graphics.FromImage(img)

        Try
            g.SmoothingMode = SmoothingMode.Default
            'get the dpi settings of the graphics context,
            'for example; 96dpi on screen, 600dpi for the printer
            'used to adjust grid and margin sizing.
            Me.FlowsheetDesignSurface.m_HorizRes = g.DpiX
            Me.FlowsheetDesignSurface.m_VertRes = g.DpiY

            Me.FlowsheetDesignSurface.DrawGrid(g)

            'handle the possibility that the viewport is scrolled,
            'adjust my origin coordintates to compensate
            Dim pt As Drawing.Point = Me.FlowsheetDesignSurface.AutoScrollPosition
            g.TranslateTransform(pt.X * scale, pt.Y * scale)

            'draw the actual objects onto the page, on top of the grid

            For Each gr As GraphicObject In Me.FlowsheetDesignSurface.SelectedObjects.Values
                Me.FlowsheetDesignSurface.drawingObjects.DrawSelectedObject(g, gr, scale * Me.FlowsheetDesignSurface.Zoom)
            Next

            With Me.FlowsheetDesignSurface.drawingObjects
                'pass the graphics resolution onto the objects
                'so that images and other objects can be sized
                'correct taking the dpi into consideration.
                .HorizontalResolution = g.DpiX
                .VerticalResolution = g.DpiY
                'doesn't really draw the selected object, but instead the
                'selection indicator, a dotted outline around the selected object
                .DrawObjects(g, scale * Me.FlowsheetDesignSurface.Zoom, False)
                If Not Me.FlowsheetDesignSurface.SelectedObject Is Nothing Then
                    If Not Me.FlowsheetDesignSurface.SelectedObjects.ContainsKey(Me.FlowsheetDesignSurface.SelectedObject.Name) Then
                        .DrawSelectedObject(g, Me.FlowsheetDesignSurface.SelectedObject, scale * Me.FlowsheetDesignSurface.Zoom)
                    End If
                End If
            End With

            Clipboard.SetImage(img)

            Me.Flowsheet.WriteToLog("Image created and sent to clipboard sucessfully.", Color.Blue, DWSIM.Flowsheet.MessageType.Information)

        Catch ex As Exception

            Me.Flowsheet.WriteToLog("Error capturing flowsheet snapshot: " & ex.ToString, Color.Red, DWSIM.Flowsheet.MessageType.GeneralError)

        Finally

            img.Dispose()
            g.Dispose()

        End Try


    End Sub

    Public Sub New()

        ' This call is required by the designer.
        InitializeComponent()

    End Sub

    'Private Sub FlowsheetDesignSurface_MouseDoubleClick(sender As Object, e As System.Windows.Forms.MouseEventArgs) Handles FlowsheetDesignSurface.MouseDoubleClick

    '    If Not Me.FlowsheetDesignSurface.SelectedObject Is Nothing Then
    '        Select Case Me.FlowsheetDesignSurface.SelectedObject.ObjectType
    '            Case ObjectType.MaterialStream

    '            Case ObjectType.FlowsheetUO
    '                Dim myobj As UnitOperations.UnitOperations.Flowsheet = Flowsheet.Collections.FlowsheetObjectCollection(Flowsheet.FormSurface.FlowsheetDesignSurface.SelectedObject.Name)
    '                If My.Computer.Keyboard.ShiftKeyDown Then
    '                    Dim viewform As New FlowsheetUOEditorForm
    '                    With viewform
    '                        .Text = Flowsheet.FormSurface.FlowsheetDesignSurface.SelectedObject.Tag
    '                        .fsuo = myobj
    '                        .ShowDialog()
    '                        .Dispose()
    '                    End With
    '                    viewform = Nothing
    '                Else
    '                    If myobj.Initialized Then
    '                        Dim viewform As New FlowsheetUOViewerForm
    '                        With viewform
    '                            .Text = Flowsheet.FormSurface.FlowsheetDesignSurface.SelectedObject.Tag
    '                            .fsuo = myobj
    '                            .Show(Flowsheet.dckPanel)
    '                        End With
    '                    Else
    '                        Dim viewform As New FlowsheetUOEditorForm
    '                        With viewform
    '                            .Text = Flowsheet.FormSurface.FlowsheetDesignSurface.SelectedObject.Tag
    '                            .fsuo = myobj
    '                            .ShowDialog()
    '                            .Dispose()
    '                        End With
    '                        viewform = Nothing
    '                    End If
    '                End If
    '            Case ObjectType.CapeOpenUO
    '                Dim myobj As CapeOpenUO = Flowsheet.Collections.FlowsheetObjectCollection(Flowsheet.FormSurface.FlowsheetDesignSurface.SelectedObject.Name)
    '                myobj.Edit(Me, New EventArgs)
    '            Case ObjectType.ExcelUO
    '                Dim myobj As ExcelUO = Flowsheet.Collections.FlowsheetObjectCollection(Flowsheet.FormSurface.FlowsheetDesignSurface.SelectedObject.Name)
    '                If My.Computer.Keyboard.ShiftKeyDown Then
    '                    Dim selectionControl As New ExcelUOEditorForm
    '                    selectionControl.Text = Flowsheet.FormSurface.FlowsheetDesignSurface.SelectedObject.Tag & " - " & "Excel UO Specification"
    '                    selectionControl.TbFileName.Text = myobj.Filename
    '                    selectionControl.ShowDialog()
    '                    myobj.Filename = selectionControl.TbFileName.Text
    '                Else
    '                    If myobj.Filename <> "" Then
    '                        If My.Computer.FileSystem.FileExists(myobj.Filename) Then
    '                            If Not DWSIM.App.IsRunningOnMono Then
    '                                Try
    '                                    Process.Start(myobj.Filename)
    '                                Catch ex As Exception
    '                                End Try
    '                            Else
    '                                Try
    '                                    Process.Start(New ProcessStartInfo("xdg-open", myobj.Filename) With {.UseShellExecute = False})
    '                                Catch ex As Exception

    '                                End Try
    '                            End If
    '                        Else
    '                            MessageBox.Show(DWSIM.App.GetLocalString("Oarquivonoexisteoufo"), DWSIM.App.GetLocalString("Erroaoabrirarquivo"), MessageBoxButtons.OK, MessageBoxIcon.Error)
    '                        End If
    '                    End If
    '                End If
    '            Case ObjectType.CustomUO
    '                Dim myobj As CustomUO = Flowsheet.Collections.FlowsheetObjectCollection(Flowsheet.FormSurface.FlowsheetDesignSurface.SelectedObject.Name)
    '                If Not DWSIM.App.IsRunningOnMono Then
    '                    Dim selectionControl As New ScriptEditorForm
    '                    selectionControl.scripttext = myobj.ScriptText
    '                    selectionControl.fontname = myobj.FontName
    '                    selectionControl.fontsize = myobj.FontSize
    '                    selectionControl.includes = myobj.Includes
    '                    selectionControl.highlightspaces = myobj.HighlightSpaces
    '                    selectionControl.Text = Flowsheet.FormSurface.FlowsheetDesignSurface.SelectedObject.Tag & " - " & DWSIM.App.GetLocalString("ScriptEditor")
    '                    selectionControl.ShowDialog(Me)
    '                    myobj.FontName = selectionControl.tscb1.SelectedItem
    '                    myobj.FontSize = selectionControl.tscb2.SelectedItem
    '                    myobj.Includes = selectionControl.includes
    '                    myobj.ScriptText = selectionControl.scripttext
    '                    myobj.HighlightSpaces = selectionControl.highlightspaces
    '                    selectionControl.Dispose()
    '                    selectionControl = Nothing
    '                Else
    '                    Dim selectionControl As New ScriptEditorFormMono
    '                    selectionControl.scripttext = myobj.ScriptText
    '                    selectionControl.fontname = myobj.FontName
    '                    selectionControl.fontsize = myobj.FontSize
    '                    selectionControl.includes = myobj.Includes
    '                    selectionControl.Text = Flowsheet.FormSurface.FlowsheetDesignSurface.SelectedObject.Tag & " - " & DWSIM.App.GetLocalString("ScriptEditor")
    '                    selectionControl.ShowDialog()
    '                    myobj.FontName = selectionControl.tscb1.SelectedItem
    '                    myobj.FontSize = selectionControl.tscb2.SelectedItem
    '                    myobj.Includes = selectionControl.includes
    '                    myobj.ScriptText = selectionControl.txtScript.Text
    '                    selectionControl.Dispose()
    '                    selectionControl = Nothing
    '                End If
    '            Case ObjectType.OT_Adjust
    '                Dim selectionControl As New UI_AdjControlPanelForm
    '                selectionControl.ShowDialog()
    '                selectionControl.Dispose()
    '                selectionControl = Nothing
    '            Case ObjectType.OT_Spec
    '                Dim selectionControl As New UI_SpecControlPanelForm
    '                selectionControl.ShowDialog()
    '                selectionControl.Dispose()
    '                selectionControl = Nothing
    '            Case ObjectType.AbsorptionColumn, ObjectType.DistillationColumn, ObjectType.ReboiledAbsorber, ObjectType.RefluxedAbsorber
    '                Dim myobj As Column = Flowsheet.Collections.FlowsheetObjectCollection(Flowsheet.FormSurface.FlowsheetDesignSurface.SelectedObject.Name)
    '                If My.Computer.Keyboard.ShiftKeyDown Then
    '                    Dim selectionControl As New UIConnectionsEditorForm
    '                    selectionControl.ShowDialog()
    '                    selectionControl.Dispose()
    '                    selectionControl = Nothing
    '                ElseIf My.Computer.Keyboard.CtrlKeyDown Then
    '                    Dim selectionControl As New UIInitialEstimatesEditorForm
    '                    selectionControl.ShowDialog()
    '                    selectionControl.Dispose()
    '                    selectionControl = Nothing
    '                ElseIf My.Computer.Keyboard.AltKeyDown Then
    '                    Dim selectionControl As New UIStagesEditorForm
    '                    selectionControl.ShowDialog()
    '                    selectionControl.Dispose()
    '                    selectionControl = Nothing
    '                Else
    '                    If myobj.Calculated Then
    '                        Dim selectionControl As New UIResultsForm
    '                        selectionControl.form = myobj.FlowSheet
    '                        selectionControl.ShowDialog()
    '                        selectionControl.Dispose()
    '                        selectionControl = Nothing
    '                    End If
    '                End If
    '            Case ObjectType.Pipe
    '                Dim myobj As Pipe = Flowsheet.Collections.FlowsheetObjectCollection(Flowsheet.FormSurface.FlowsheetDesignSurface.SelectedObject.Name)
    '                If My.Computer.Keyboard.ShiftKeyDown Then
    '                    Dim selectionControl As New ThermalProfileEditorForm
    '                    selectionControl.ThermalProfile = myobj.ThermalProfile
    '                    selectionControl.ShowDialog()
    '                    myobj.ThermalProfile = selectionControl.ThermalProfile
    '                    selectionControl.Dispose()
    '                    selectionControl = Nothing
    '                ElseIf My.Computer.Keyboard.CtrlKeyDown Then
    '                    If myobj.Calculated Then
    '                        Dim selectionControl As New FormGraph
    '                        selectionControl.Profile = myobj.Profile
    '                        selectionControl.ShowDialog()
    '                        selectionControl.Dispose()
    '                        selectionControl = Nothing
    '                    End If
    '                ElseIf My.Computer.Keyboard.AltKeyDown Then
    '                    If myobj.Calculated Then
    '                        Dim selectionControl As New FormTable
    '                        selectionControl.Profile = myobj.Profile
    '                        selectionControl.ShowDialog()
    '                        selectionControl.Dispose()
    '                        selectionControl = Nothing
    '                    End If
    '                Else
    '                    Dim selectionControl As New PipeEditorForm
    '                    selectionControl.PipeEditor1.SystemOfUnits = My.Application.ActiveSimulation.Options.SelectedUnitSystem
    '                    selectionControl.PipeEditor1.NumberFormat = My.Application.ActiveSimulation.Options.NumberFormat
    '                    selectionControl.PipeEditor1.Profile = myobj.Profile
    '                    selectionControl.ShowDialog()
    '                    myobj.Profile = selectionControl.PipeEditor1.Profile
    '                    selectionControl.Dispose()
    '                    selectionControl = Nothing
    '                End If
    '            Case ObjectType.RCT_PFR
    '                Dim myobj As Reactor_PFR = Flowsheet.Collections.FlowsheetObjectCollection(Flowsheet.FormSurface.FlowsheetDesignSurface.SelectedObject.Name)
    '                If myobj.Calculated Then
    '                    Dim selectionControl As New FormGraphPFR
    '                    selectionControl.form = myobj.FlowSheet
    '                    selectionControl.Points = myobj.points
    '                    selectionControl.ShowDialog()
    '                    selectionControl.Dispose()
    '                    selectionControl = Nothing
    '                End If
    '            Case ObjectType.RCT_Gibbs
    '                Dim myobj As Reactor_Gibbs = Flowsheet.Collections.FlowsheetObjectCollection(Flowsheet.FormSurface.FlowsheetDesignSurface.SelectedObject.Name)
    '                If My.Computer.Keyboard.ShiftKeyDown Then
    '                    Dim selectionControl As New ElementMatrixEditorForm
    '                    selectionControl.elmat = myobj.ElementMatrix
    '                    selectionControl.Text = myobj.GraphicObject.Tag & " - " & DWSIM.App.GetLocalString("RGEditElementMatrix")
    '                    selectionControl.ShowDialog()
    '                    myobj.ElementMatrix = selectionControl.elmat
    '                    selectionControl.Dispose()
    '                    selectionControl = Nothing
    '                Else
    '                    Dim selectionControl As New GibbsInitialEstimatesEditorForm
    '                    selectionControl.ie = myobj.InitialEstimates
    '                    selectionControl.gr = myobj
    '                    selectionControl.Text = myobj.GraphicObject.Tag & " - " & selectionControl.Text
    '                    selectionControl.form = myobj.FlowSheet
    '                    If selectionControl.gr.GraphicObject.InputConnectors(0).IsAttached Then
    '                        selectionControl.inlet = myobj.FlowSheet.Collections.FlowsheetObjectCollection(selectionControl.gr.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Name)
    '                    End If
    '                    If selectionControl.gr.GraphicObject.InputConnectors(0).IsAttached Then
    '                        selectionControl.inlet = myobj.FlowSheet.Collections.FlowsheetObjectCollection(selectionControl.gr.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Name)
    '                    End If
    '                    If selectionControl.gr.GraphicObject.OutputConnectors(0).IsAttached Then
    '                        selectionControl.outletv = myobj.FlowSheet.Collections.FlowsheetObjectCollection(selectionControl.gr.GraphicObject.OutputConnectors(0).AttachedConnector.AttachedTo.Name)
    '                    End If
    '                    If selectionControl.gr.GraphicObject.OutputConnectors(1).IsAttached Then
    '                        selectionControl.outletl = myobj.FlowSheet.Collections.FlowsheetObjectCollection(selectionControl.gr.GraphicObject.OutputConnectors(1).AttachedConnector.AttachedTo.Name)
    '                    End If
    '                    selectionControl.ShowDialog()
    '                    myobj.InitialEstimates = selectionControl.ie
    '                    selectionControl.Dispose()
    '                    selectionControl = Nothing
    '                End If
    '        End Select
    '    End If

    'End Sub

    Private Sub CopiarDadosParaareaDeTransferenciaToolStripMenuItem_Click(sender As System.Object, e As System.EventArgs) Handles CopiarDadosParaareaDeTransferenciaToolStripMenuItem.Click

        Me.Flowsheet.tsmiExportData_Click(sender, e)

    End Sub

    'Private Sub RestoreTSMI_Click(sender As Object, e As EventArgs) Handles RestoreTSMI.Click

    '    If Not Me.FlowsheetDesignSurface.SelectedObject Is Nothing Then

    '        If Me.FlowsheetDesignSurface.SelectedObject.ObjectType = ObjectType.MaterialStream Then

    '            Dim mystr As DWSIM.Thermodynamics.Streams.MaterialStream = Flowsheet.Collections.FlowsheetObjectCollection(Flowsheet.FormSurface.FlowsheetDesignSurface.SelectedObject.Name)

    '            If Not mystr.GraphicObject.InputConnectors(0).IsAttached Then

    '                'assign default values for temperature, pressure and mass flow
    '                mystr.Phases(0).Properties.temperature = 298.15
    '                mystr.Phases(0).Properties.pressure = 101325
    '                mystr.Phases(0).Properties.massflow = 1

    '                mystr.EqualizeOverallComposition()

    '                Application.DoEvents()
    '                CalculateMaterialStream(Flowsheet, mystr)
    '                Application.DoEvents()
    '                Call Flowsheet.FormSurface.UpdateSelectedObject()
    '                Application.DoEvents()
    '                Call Flowsheet.FormSurface.FlowsheetDesignSurface.Invalidate()
    '                Application.DoEvents()
    '                ProcessCalculationQueue(Flowsheet)
    '                Application.DoEvents()

    '            ElseIf mystr.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.ObjectType = ObjectType.OT_Recycle Then

    '                'assign default values for temperature, pressure and mass flow
    '                mystr.Phases(0).Properties.temperature = 298.15
    '                mystr.Phases(0).Properties.pressure = 101325
    '                mystr.Phases(0).Properties.massflow = 1

    '                mystr.EqualizeOverallComposition()

    '                Application.DoEvents()
    '                CalculateMaterialStream(Flowsheet, mystr)
    '                Application.DoEvents()
    '                Call Flowsheet.FormSurface.UpdateSelectedObject()
    '                Application.DoEvents()
    '                Call Flowsheet.FormSurface.FlowsheetDesignSurface.Invalidate()
    '                Application.DoEvents()
    '                ProcessCalculationQueue(Flowsheet)
    '                Application.DoEvents()

    '            Else

    '                MessageBox.Show("The selected Material Stream is read-only.", "Information", MessageBoxButtons.OK, MessageBoxIcon.Information)

    '            End If

    '        End If
    '    End If
    'End Sub

    Private Sub ExibirTudoToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles ExibirTudoToolStripMenuItem.Click
        Me.FlowsheetDesignSurface.ZoomAll()
        Me.FlowsheetDesignSurface.ZoomAll()
        Me.Invalidate()
    End Sub

    Private Sub ZoomPadrao100ToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles ZoomPadrao100ToolStripMenuItem.Click
        Me.FlowsheetDesignSurface.Zoom = 1
        Me.Invalidate()
    End Sub

    Private Sub CentralizarToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles CentralizarToolStripMenuItem.Click
        Me.FlowsheetDesignSurface.Center()
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

    Private Sub ToolStripMenuItem11_Click(sender As Object, e As EventArgs) Handles ToolStripMenuItem11.Click
        If DWSIM.App.IsRunningOnMono Then
            MessageBox.Show(DWSIM.App.GetLocalString("Unsupported_Feature"), "DWSIM", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)
        Else
            Me.FlowsheetDesignSurface.SelectedObject.Rotation = 0
            Me.Invalidate()
        End If
    End Sub

    Private Sub ToolStripMenuItem12_Click(sender As Object, e As EventArgs) Handles ToolStripMenuItem12.Click
        If DWSIM.App.IsRunningOnMono Then
            MessageBox.Show(DWSIM.App.GetLocalString("Unsupported_Feature"), "DWSIM", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)
        Else
            Me.FlowsheetDesignSurface.SelectedObject.Rotation = 90
            Me.Invalidate()
        End If
    End Sub

    Private Sub ToolStripMenuItem13_Click(sender As Object, e As EventArgs) Handles ToolStripMenuItem13.Click
        If DWSIM.App.IsRunningOnMono Then
            MessageBox.Show(DWSIM.App.GetLocalString("Unsupported_Feature"), "DWSIM", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)
        Else
            Me.FlowsheetDesignSurface.SelectedObject.Rotation = 180
            Me.Invalidate()
        End If
    End Sub

    Private Sub ToolStripMenuItem14_Click(sender As Object, e As EventArgs) Handles ToolStripMenuItem14.Click
        If DWSIM.App.IsRunningOnMono Then
            MessageBox.Show(DWSIM.App.GetLocalString("Unsupported_Feature"), "DWSIM", MessageBoxButtons.OK, MessageBoxIcon.Exclamation)
        Else
            Me.FlowsheetDesignSurface.SelectedObject.Rotation = 270
            Me.Invalidate()
        End If
    End Sub

    Private Sub DepurarObjetoToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles DepurarObjetoToolStripMenuItem.Click
        If Not Me.FlowsheetDesignSurface.SelectedObject Is Nothing Then
            If Flowsheet.Collections.FlowsheetObjectCollection.ContainsKey(Flowsheet.FormSurface.FlowsheetDesignSurface.SelectedObject.Name) Then
                Dim myobj As SharedClasses.UnitOperations.BaseClass = Flowsheet.Collections.FlowsheetObjectCollection(Flowsheet.FormSurface.FlowsheetDesignSurface.SelectedObject.Name)
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
                                                                     frm.TextBox1.Text = t.Result
                                                                     frm.TextBox1.SelectionStart = 0
                                                                     frm.TextBox1.SelectionLength = 0
                                                                 End Sub, TaskContinuationOptions.ExecuteSynchronously)

            End If
        End If
    End Sub

    Private Sub FlowsheetDesignSurface_MouseDoubleClick(sender As Object, e As Windows.Forms.MouseEventArgs) Handles FlowsheetDesignSurface.MouseDoubleClick
        If Not Me.FlowsheetDesignSurface.SelectedObject Is Nothing Then
            Select Case Me.FlowsheetDesignSurface.SelectedObject.ObjectType
                Case ObjectType.GO_Table
                    Dim f As New FormConfigurePropertyTable() With {.Table = Me.FlowsheetDesignSurface.SelectedObject}
                    f.ShowDialog(Me)
                Case ObjectType.GO_SpreadsheetTable
                    Dim f As New FormConfigureSpreadsheetTable() With {.Table = Me.FlowsheetDesignSurface.SelectedObject}
                    f.ShowDialog(Me)
                Case ObjectType.GO_MasterTable
                    Dim f As New FormConfigureMasterTable() With {.Table = Me.FlowsheetDesignSurface.SelectedObject}
                    f.ShowDialog(Me)
            End Select
        End If
    End Sub

    Private Sub EditarAparênciaToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles EditAppearanceToolStripMenuItem.Click
        Dim f As New FormEditGraphicObject() With {.gobj = Me.FlowsheetDesignSurface.SelectedObject}
        f.Show(Flowsheet.dckPanel)
    End Sub

End Class
