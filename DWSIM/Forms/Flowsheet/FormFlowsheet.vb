'    Copyright 2008-2016 Daniel Wagner O. de Medeiros
'
'    This file is part of DWSIM.
'
'    DWSIM is free software: you can redistribute it and/or modify
'    it under the terms of the GNU General Public License as published by
'    the Free Software Foundation, either version 3 of the License, or
'    (at your option) any later version.
'
'    DWSIM is distributed in the hope that it will be useful,
'    but WITHOUT ANY WARRANTY; without even the implied warranty of
'    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
'    GNU General Public License for more details.
'
'    You should have received a copy of the GNU General Public License
'    along with DWSIM.  If not, see <http://www.gnu.org/licenses/>.

Imports DWSIM.Drawing.SkiaSharp.GraphicObjects
Imports System.ComponentModel
Imports WeifenLuo.WinFormsUI
Imports System.Linq
Imports System.IO
Imports WeifenLuo.WinFormsUI.Docking
Imports System.Globalization
Imports System.Reflection
Imports DWSIM.Interfaces
Imports DWSIM.Interfaces.Interfaces2
Imports System.Runtime.InteropServices
Imports System.Dynamic
Imports DWSIM.Drawing.SkiaSharp.GraphicObjects.Tables
Imports DWSIM.Thermodynamics.BaseClasses
Imports DWSIM.Thermodynamics.PropertyPackages.Auxiliary

<ComSourceInterfaces(GetType(Interfaces.IFlowsheetNewMessageSentEvent)), ClassInterface(ClassInterfaceType.AutoDual)>
<System.Serializable()>
<Guid(FormFlowsheet.ClassId)>
Public Class FormFlowsheet

    Inherits Form

    'CAPE-OPEN PME/COSE Interfaces
    Implements CapeOpen.ICapeCOSEUtilities, CapeOpen.ICapeMaterialTemplateSystem, CapeOpen.ICapeDiagnostic,
                CapeOpen.ICapeFlowsheetMonitoring, CapeOpen.ICapeSimulationContext, CapeOpen.ICapeIdentification

    'DWSIM IFlowsheet interface
    Implements Interfaces.IFlowsheet, Interfaces.IFlowsheetBag, Interfaces.IFlowsheetGUI, Interfaces.IFlowsheetCalculationQueue

    Public Shadows Const ClassId As String = "0294AA84-9269-46CE-A854-BEF64539287B"
    Public Shadows Const InterfaceId As String = "F405F679-7C8F-4737-BE58-738624220B7D"
    Public Shadows Const EventsId As String = "5E0BA6EE-9025-4C33-A896-E061F32E93BF"

#Region "    Variable Declarations "

    Public Property ExtraProperties As New ExpandoObject Implements IFlowsheet.ExtraProperties

    Public Property MasterFlowsheet As IFlowsheet = Nothing Implements IFlowsheet.MasterFlowsheet
    <Xml.Serialization.XmlIgnore> Public Property MasterUnitOp As ISimulationObject = Nothing Implements IFlowsheet.MasterUnitOp
    Public Property RedirectMessages As Boolean = False Implements IFlowsheet.RedirectMessages

    Public FrmStSim1 As New FormSimulSettings
    Public FrmPCBulk As New FormPCBulk
    Public FrmReport As New FormReportConfig

    Public FrmReacMan As FormReacManager

    Public m_IsLoadedFromFile As Boolean = False
    Public m_overrideCloseQuestion As Boolean = False

    Public FormSurface As New FlowsheetSurface_SkiaSharp
    Public FormLog As New LogPanel
    Public FormMatList As New MaterialStreamPanel
    Public FormSpreadsheet As New FormNewSpreadsheet
    Public FormCharts As New FormCharts

    Public FormProps As New frmProps

    Public FormCOReports As New COReportsPanel
    Public FormWatch As New WatchPanel

    Public FormSensAnalysis0 As New FormSensAnalysis
    Public FormOptimization0 As New FormOptimization

    Public WithEvents Options As New SharedClasses.DWSIM.Flowsheet.FlowsheetVariables

    Public Property CalculationQueue As Generic.Queue(Of ICalculationArgs) Implements IFlowsheetCalculationQueue.CalculationQueue

    Public ScriptCollection As Dictionary(Of String, IScript)

    Public ChartCollection As New Dictionary(Of String, IChart)

    Public CheckedToolstripButton As ToolStripButton
    Public ClickedToolStripMenuItem As ToolStripMenuItem
    Public InsertingObjectToPFD As Boolean = False

    Public prevcolor1, prevcolor2 As Color

    Public Collections As New SharedClasses.DWSIM.Flowsheet.ObjectCollection

    Public ID As String

    Private QuestionID As Integer = -1

    Private loaded As Boolean = False

    Public UndoStack As New Stack(Of UndoRedoAction)
    Public RedoStack As New Stack(Of UndoRedoAction)

    Private listeningaction As Action(Of String, Interfaces.IFlowsheet.MessageType)

    Public Property SupressMessages As Boolean = False

#End Region

#Region "    Form Event Handlers "

    Public Sub New()

        ' This call is required by the Windows Form Designer.
        InitializeComponent()

        ' Add any initialization after the InitializeComponent() call.
        ID = Guid.NewGuid().ToString

        If Not DWSIM.App.IsRunningOnMono Then
            Dim theme As New VS2012LightTheme()
            theme.Apply(Me.dckPanel)
        Else
            Me.dckPanel.Skin.DockPaneStripSkin.TextFont = SystemFonts.DefaultFont
        End If

        Options.ReactionSets.Add("DefaultSet", New ReactionSet("DefaultSet", DWSIM.App.GetLocalString("Rxn_DefaultSetName"), DWSIM.App.GetLocalString("Rxn_DefaultSetDesc")))

        Dim fa As New Thermodynamics.PropertyPackages.Auxiliary.FlashAlgorithms.NestedLoops()
        fa.Tag = fa.Name & " (1)"

        Options.FlashAlgorithms.Add(fa)

    End Sub

    Public Sub SetActive()

        My.Application.ActiveSimulation = Me

    End Sub
    Private Sub FormChild_Activated(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Activated

        SetActive()

    End Sub

    Private Sub FormChild_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load

        FormCharts.Flowsheet = Me
        FormSpreadsheet.Flowsheet = Me

        Me.MdiParent = FormMain

        If DWSIM.App.IsRunningOnMono Then
            'Me.FlowLayoutPanel1.AutoSize = False
            'Me.FlowLayoutPanel1.Height = 50
            Me.MenuStrip1.Visible = False
            Me.CAPEOPENFlowsheetMonitoringObjectsMOsToolStripMenuItem.Visible = False
            'Me.WindowState = FormWindowState.Maximized
        Else
            'FormObjList = New frmObjList
            Me.MenuStrip1.Visible = False
            Me.WindowState = FormWindowState.Normal
        End If

        If GlobalSettings.Settings.OldUI Then
            showflowsheettoolstripmenuitem.Checked = My.Settings.ShowFlowsheetToolStrip
            showunitstoolstripmenuitem.Checked = My.Settings.ShowUnitsToolStrip
        End If

        Me.COObjTSMI.Checked = Me.Options.FlowsheetShowCOReportsWindow
        Me.ExibirListaDeItensACalcularToolStripMenuItem.Checked = Me.Options.FlowsheetShowCalculationQueue
        Me.varpaneltsmi.Checked = Me.Options.FlowsheetShowWatchWindow

        Dim rand As New Random
        Dim str As String = rand.Next(10000000, 99999999)

        Me.Options.BackupFileName = str & ".dwbcs"

        Me.FormSurface.TSTBZoom.Text = Format(Me.FormSurface.FlowsheetSurface.Zoom, "#%")

        If GlobalSettings.Settings.CalculatorActivated Then
            Me.tsbAtivar.Checked = True
        Else
            Me.tsbAtivar.Checked = False
        End If

        Me.tsbSimultAdjustSolver.Checked = Me.FlowsheetOptions.SimultaneousAdjustSolverEnabled

        Me.FormSurface.tsbSnapObjectsToGrid.Checked = Me.Options.FlowsheetSnapToGrid
        'Me.FormSurface.FlowsheetDesignSurface.SnapToGrid = Me.Options.FlowsheetSnapToGrid
        Me.FormSurface.ToolStripButton17.Checked = Me.Options.FlowsheetQuickConnect

        If Me.ScriptCollection Is Nothing Then Me.ScriptCollection = New Dictionary(Of String, Interfaces.IScript)

        My.Application.ActiveSimulation = Me

        If Not Me.m_IsLoadedFromFile Then

            For Each item In FormMain.aTypeList.OrderBy(Function(x) x.Name)
                If Not item.IsAbstract Then
                    Dim obj = DirectCast(Activator.CreateInstance(item), Interfaces.ISimulationObject)
                    obj.SetFlowsheet(Me)
                    Me.FlowsheetOptions.VisibleProperties.Add(item.Name, obj.GetDefaultProperties.ToList)
                    obj = Nothing
                End If
            Next

            If Not DWSIM.App.IsRunningOnMono Then
                Me.Options.SimulationAuthor = My.User.Name
            Else
                Me.Options.SimulationAuthor = "user"
            End If

            For Each pp As PropertyPackages.PropertyPackage In Me.Options.PropertyPackages.Values
                'If pp.ConfigForm Is Nothing Then pp.ReconfigureConfigForm()
            Next

            Me.Options.NotSelectedComponents = New Dictionary(Of String, Interfaces.ICompoundConstantProperties)

            Dim tmpc As BaseClasses.ConstantProperties
            For Each tmpc In FormMain.AvailableComponents.Values
                Dim newc As New BaseClasses.ConstantProperties
                newc = tmpc
                Me.Options.NotSelectedComponents.Add(tmpc.Name, newc)
            Next

            Dim Frm = ParentForm

            ' Set DockPanel properties
            dckPanel.ActiveAutoHideContent = Nothing
            dckPanel.Parent = Me

            FormLog.DockPanel = Nothing
            FormMatList.DockPanel = Nothing
            FormSpreadsheet.DockPanel = Nothing
            FormWatch.DockPanel = Nothing
            FormSurface.DockPanel = Nothing
            FormCharts.DockPanel = Nothing

            Dim myfile As String = Path.Combine(My.Application.Info.DirectoryPath, "layout.xml")
            dckPanel.LoadFromXml(myfile, New DeserializeDockContent(AddressOf ReturnForm))

            FormLog.Show(dckPanel)
            FormSurface.Show(dckPanel)
            FormMatList.Show(FormSurface.Pane, Nothing)
            FormSpreadsheet.Show(FormSurface.Pane, Nothing)
            FormCharts.Show(FormSurface.Pane, Nothing)
            FormWatch.Show(dckPanel)
            FormProps.Show(dckPanel, DockState.DockLeft)

            FormSurface.Activate()

            dckPanel.BringToFront()
            dckPanel.UpdateDockWindowZOrder(DockStyle.Fill, True)

            Me.Invalidate()
            Application.DoEvents()

        End If

        Me.UpdateFormText()

        'load plugins
        CreatePluginsList()

        loaded = True

        If My.Settings.ObjectEditor = 0 Then FormProps.Hide()

        Dim fs As New FormScript
        fs.fc = Me
        fs.Show(Me.dckPanel)

        FormSurface.Activate()

    End Sub

    Function ReturnForm(ByVal str As String) As IDockContent
        Select Case str
            Case "DWSIM.LogPanel", "DWSIM.frmLog"
                Return Me.FormLog
            Case "DWSIM.MaterialStreamPanel", "DWSIM.frmMatList"
                Return Me.FormMatList
            Case "DWSIM.FlowsheetSurface", "DWSIM.frmSurface"
                Return Me.FormSurface
            Case "DWSIM.SpreadsheetForm"
                Return Me.FormSpreadsheet
            Case "DWSIM.WatchPanel", "DWSIM.frmWatch"
                Return Me.FormWatch
            Case "DWSIM.frmProps"
                Return Me.FormProps
            Case "DWSIM.FormCharts"
                Return Me.FormCharts
        End Select
        Return Nothing
    End Function
    Public Sub FormChild_Shown(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Shown

        If Not Me.m_IsLoadedFromFile Then

            Me.Invalidate()
            Application.DoEvents()
            Application.DoEvents()

            If Not DWSIM.App.IsRunningOnMono Then
                Dim fw As New FormSimulWizard
                With fw
                    .StartPosition = FormStartPosition.CenterScreen
                    .WindowState = FormWindowState.Normal
                    .ShowDialog(Me)
                    If .switch Then
                        With Me.FrmStSim1
                            .WindowState = FormWindowState.Normal
                            .StartPosition = FormStartPosition.CenterScreen
                            .ShowDialog(Me)
                        End With
                    End If
                End With
            Else
                With Me.FrmStSim1
                    .WindowState = FormWindowState.Normal
                    .StartPosition = FormStartPosition.CenterScreen
                    .ShowDialog(Me)
                End With
            End If

        End If

        If DWSIM.App.IsRunningOnMono Then
            FormMain.ToolStripButton1.Enabled = True
            FormMain.SaveAllToolStripButton.Enabled = True
            FormMain.SaveToolStripButton.Enabled = True
            FormMain.SaveToolStripMenuItem.Enabled = True
            FormMain.SaveAllToolStripMenuItem.Enabled = True
            FormMain.SaveAsToolStripMenuItem.Enabled = True
            FormMain.ToolStripButton1.Enabled = True
            FormMain.CloseAllToolstripMenuItem.Enabled = True
        End If

        Me.ProcessScripts(Enums.Scripts.EventType.SimulationOpened, Enums.Scripts.ObjectType.Simulation, "")

        WriteToLog(DWSIM.App.GetLocalTipString("FLSH003"), Color.Black, SharedClasses.DWSIM.Flowsheet.MessageType.Tip)
        WriteToLog(DWSIM.App.GetLocalTipString("FLSH001"), Color.Black, SharedClasses.DWSIM.Flowsheet.MessageType.Tip)
        WriteToLog(DWSIM.App.GetLocalTipString("FLSH002"), Color.Black, SharedClasses.DWSIM.Flowsheet.MessageType.Tip)
        WriteToLog(DWSIM.App.GetLocalTipString("FLSH005"), Color.Black, SharedClasses.DWSIM.Flowsheet.MessageType.Tip)
        WriteToLog(DWSIM.App.GetLocalTipString("FLSH008"), Color.Black, SharedClasses.DWSIM.Flowsheet.MessageType.Tip)

        FormSurface.FlowsheetSurface.DrawFloatingTable = Options.DisplayFloatingPropertyTables
        FormSurface.FlowsheetSurface.DrawPropertyList = Options.DisplayCornerPropertyList

        FormSurface.FlowsheetSurface.ZoomAll(FormSurface.SplitContainer1.Panel1.Width, FormSurface.SplitContainer1.Panel1.Height)
        FormSurface.FlowsheetSurface.ZoomAll(FormSurface.SplitContainer1.Panel1.Width, FormSurface.SplitContainer1.Panel1.Height)

        Me.FormLog.Grid1.Sort(Me.FormLog.Grid1.Columns(1), ListSortDirection.Descending)

    End Sub

    Private Sub FormChild2_FormClosed(ByVal sender As Object, ByVal e As System.Windows.Forms.FormClosedEventArgs) Handles Me.FormClosed

        ToolStripManager.RevertMerge(FormMain.ToolStrip1, ToolStrip1)

        Me.ProcessScripts(Enums.Scripts.EventType.SimulationClosed, Enums.Scripts.ObjectType.Simulation, "")

        If My.Application.ActiveSimulation Is Me Then
            My.Application.ActiveSimulation = Nothing
        End If

        'dispose objects
        For Each obj As SharedClasses.UnitOperations.BaseClass In Me.Collections.FlowsheetObjectCollection.Values
            If obj.disposedValue = False Then obj.Dispose()
        Next

        If GlobalSettings.Settings.OldUI Then

            Dim path As String = My.Settings.BackupFolder + System.IO.Path.DirectorySeparatorChar + Me.Options.BackupFileName

            If My.Settings.BackupFiles.Contains(path) Then
                My.Settings.BackupFiles.Remove(path)
                If Not DWSIM.App.IsRunningOnMono Then My.Settings.Save()
                Try
                    If File.Exists(path) Then File.Delete(path)
                Catch ex As Exception
                End Try
            End If

        End If

        Dim cnt As Integer = FormMain.MdiChildren.Length

        If cnt = 0 Then

            FormMain.ToolStripButton1.Enabled = False
            FormMain.SaveAllToolStripButton.Enabled = False
            FormMain.SaveToolStripButton.Enabled = False
            FormMain.SaveToolStripMenuItem.Enabled = False
            FormMain.SaveAllToolStripMenuItem.Enabled = False
            FormMain.SaveAsToolStripMenuItem.Enabled = False
            FormMain.ToolStripButton1.Enabled = False

        Else

            FormMain.ToolStripButton1.Enabled = True
            FormMain.SaveAllToolStripButton.Enabled = True
            FormMain.SaveToolStripButton.Enabled = True
            FormMain.SaveToolStripMenuItem.Enabled = True
            FormMain.SaveAllToolStripMenuItem.Enabled = True
            FormMain.SaveAsToolStripMenuItem.Enabled = True
            FormMain.ToolStripButton1.Enabled = True

        End If

        'garbage collection (frees unused memory)
        System.GC.Collect()
        System.GC.WaitForPendingFinalizers()
        System.GC.Collect()
        System.GC.WaitForPendingFinalizers()

    End Sub

    Private Sub FormChild2_FormClosing(ByVal sender As Object, ByVal e As System.Windows.Forms.FormClosingEventArgs) Handles Me.FormClosing

        If Me.m_overrideCloseQuestion = False Then

            Dim x = MessageBox.Show(DWSIM.App.GetLocalString("Desejasalvarasaltera"), DWSIM.App.GetLocalString("Fechando") & " " & Me.Options.SimulationName & " (" & System.IO.Path.GetFileName(Me.Options.FilePath) & ") ...", MessageBoxButtons.YesNoCancel, MessageBoxIcon.Question)

            If x = MsgBoxResult.Yes Then

                FormMain.SaveFile(False)
                Me.m_overrideCloseQuestion = True
                Me.Close()

            ElseIf x = MsgBoxResult.Cancel Then

                FormMain.CancelClosing = True
                e.Cancel = True

            Else

                Me.m_overrideCloseQuestion = True
                Me.Close()

            End If

        End If

    End Sub

#End Region

#Region "    Functions "

    Public Sub SolveFlowsheet2()

        tsbCalcF_Click(Me, New EventArgs)

    End Sub

    Sub UpdateFormText()
        If File.Exists(Me.Options.FilePath) Then
            Me.Text = IO.Path.GetFileNameWithoutExtension(Me.Options.FilePath) & " (" & Me.Options.FilePath & ")"
        Else
            Me.Text = Me.Options.SimulationName
        End If
    End Sub

    Public Sub ProcessScripts(ByVal sourceevent As Scripts.EventType, ByVal sourceobj As Scripts.ObjectType, ByVal sourceobjname As String) Implements IFlowsheetGUI.ProcessScripts, IFlowsheet.ProcessScripts

        Me.UIThread(Sub()
                        If Not Me.ScriptCollection Is Nothing Then
                            For Each scr As Script In Me.ScriptCollection.Values
                                If scr.Linked And scr.LinkedEventType = sourceevent And scr.LinkedObjectType = sourceobj And scr.LinkedObjectName = sourceobjname Then
                                    If My.Application.CommandLineMode Then
                                        Console.WriteLine()
                                        Console.WriteLine("Running script '" & scr.Title & "' for event '" & scr.LinkedEventType.ToString & "', linked to '" & Me.Collections.FlowsheetObjectCollection(scr.LinkedObjectName).GraphicObject.Tag & "'...")
                                        Console.WriteLine()
                                    Else
                                        If scr.LinkedObjectName <> "" Then
                                            Me.WriteToLog("Running script '" & scr.Title & "' for event '" & scr.LinkedEventType.ToString & "', linked to '" & Me.Collections.FlowsheetObjectCollection(scr.LinkedObjectName).GraphicObject.Tag & "'...", Color.Blue, SharedClasses.DWSIM.Flowsheet.MessageType.Information)
                                        Else
                                            Me.WriteToLog("Running script '" & scr.Title & "' for event '" & scr.LinkedEventType.ToString & "'", Color.Blue, SharedClasses.DWSIM.Flowsheet.MessageType.Information)
                                        End If
                                    End If
                                    If scr.PythonInterpreter = Enums.Scripts.Interpreter.IronPython Then
                                        FormScript.RunScript_IronPython(scr.ScriptText, Me, Nothing)
                                    Else
                                        FormScript.RunScript_PythonNET(scr.ScriptText, Me)
                                    End If
                                End If
                            Next
                        Else
                            Me.ScriptCollection = New Dictionary(Of String, Interfaces.IScript)
                        End If
                    End Sub)

    End Sub

    Public Sub AddUnitSystem(ByVal su As SystemsOfUnits.Units)

        If Not My.Application.UserUnitSystems.ContainsKey(su.Name) Then
            My.Application.UserUnitSystems.Add(su.Name, su)
            FormMain.AvailableUnitSystems.Add(su.Name, su)
            Me.FrmStSim1.ComboBox2.Items.Add(su.Name)
        Else
            MessageBox.Show("Please input a different name for the unit system.")
        End If

    End Sub

    Public Sub AddComponentsRows(ByVal MaterialStream As IMaterialStream) Implements IFlowsheet.AddCompoundsToMaterialStream
        If Me.Options.SelectedComponents.Count = 0 Then
            MessageBox.Show(DWSIM.App.GetLocalString("Nohcomponentesaadici"))
        Else
            Dim comp As BaseClasses.ConstantProperties
            For Each phase As BaseClasses.Phase In MaterialStream.Phases.Values
                For Each comp In Me.Options.SelectedComponents.Values
                    With phase
                        .Compounds.Add(comp.Name, New BaseClasses.Compound(comp.Name, ""))
                        .Compounds(comp.Name).ConstantProperties = comp
                    End With
                Next
            Next
            DirectCast(MaterialStream, Streams.MaterialStream).EqualizeOverallComposition()
            DirectCast(MaterialStream, Streams.MaterialStream).CalcOverallCompMassFractions()
        End If
    End Sub

    Public Function FT(ByRef prop As String, ByVal unit As String)
        Return prop & " (" & unit & ")"
    End Function

    Public Enum ID_Type
        Name
        Tag
    End Enum

    Public Shared Function SearchSurfaceObjectsByName(ByVal Name As String, ByVal Surface As Drawing.SkiaSharp.GraphicsSurface) As GraphicObject

        Dim gObj As GraphicObject = Nothing
        Dim gObj2 As GraphicObject = Nothing
        For Each gObj In Surface.DrawingObjects
            If gObj.Name.ToString = Name Then
                gObj2 = gObj
                Exit For
            End If
        Next
        Return gObj2

    End Function

    Public Shared Function SearchSurfaceObjectsByTag(ByVal Name As String, ByVal Surface As Drawing.SkiaSharp.GraphicsSurface) As GraphicObject

        Dim gObj As GraphicObject = Nothing
        Dim gObj2 As GraphicObject = Nothing
        For Each gObj In Surface.DrawingObjects
            If gObj.Tag.ToString = Name Then
                gObj2 = gObj
                Exit For
            End If
        Next
        Return gObj2

    End Function

    Public Function GetFlowsheetGraphicObject(ByVal tag As String) As GraphicObject

        Dim gObj As GraphicObject = Nothing
        Dim gObj2 As GraphicObject = Nothing
        For Each gObj In Me.FormSurface.FlowsheetSurface.DrawingObjects
            If gObj.Tag.ToString.ToLower = tag.ToLower And Not gObj.IsConnector Then
                gObj2 = gObj
                Exit For
            End If
        Next

        Return gObj2

    End Function

    Public Function GetFlowsheetSimulationObject(ByVal tag As String) As SharedClasses.UnitOperations.BaseClass

        For Each obj As SharedClasses.UnitOperations.BaseClass In Me.Collections.FlowsheetObjectCollection.Values
            If obj.GraphicObject.Tag = tag Then
                Return obj
            End If
        Next

        Return Nothing

    End Function

    Public Sub WriteToLog(ByVal texto As String, ByVal cor As Color, ByVal tipo As SharedClasses.DWSIM.Flowsheet.MessageType, Optional ByVal exceptionID As String = "")

        If Not SupressMessages Then

            If texto.Trim <> "" Then

            Dim frsht As FormFlowsheet
            If Not Me.MasterFlowsheet Is Nothing And Me.RedirectMessages Then
                frsht = Me.MasterFlowsheet
                texto = "[" & Me.MasterUnitOp.GraphicObject.Tag & "] " & texto
            Else
                frsht = Me
            End If

            If listeningaction IsNot Nothing Then
                listeningaction(texto, tipo)
            End If

            Message = texto

            frsht.UIThread(New System.Action(Sub()
                                                 RaiseEvent NewMessageSent(texto)
                                             End Sub))

            If frsht.Visible Then

                frsht.UIThread(New System.Action(Sub()

                                                     Dim showtips As Boolean = True
                                                     If GlobalSettings.Settings.OldUI Then
                                                         showtips = My.Settings.ShowTips
                                                     End If

                                                     If Not My.Application.CommandLineMode Then

                                                         Dim frlog = frsht.FormLog

                                                         Dim img As Bitmap
                                                         Dim strtipo As String
                                                         Select Case tipo
                                                             Case SharedClasses.DWSIM.Flowsheet.MessageType.Warning
                                                                 img = My.Resources._error
                                                                 strtipo = DWSIM.App.GetLocalString("Aviso")
                                                             Case SharedClasses.DWSIM.Flowsheet.MessageType.GeneralError
                                                                 img = My.Resources.exclamation
                                                                 strtipo = DWSIM.App.GetLocalString("Erro")
                                                             Case SharedClasses.DWSIM.Flowsheet.MessageType.Tip
                                                                 If Not showtips Then Exit Sub
                                                                 img = My.Resources.lightbulb
                                                                 strtipo = DWSIM.App.GetLocalString("Dica")
                                                             Case Else
                                                                 img = My.Resources.information
                                                                 strtipo = DWSIM.App.GetLocalString("Mensagem")
                                                         End Select

                                                         frlog.Grid1.Rows.Insert(0, New Object() {img, frlog.Grid1.Rows.Count, Date.Now, strtipo, texto})

                                                         If frlog.Grid1.Rows.Count > 0 Then
                                                             frlog.Grid1.Rows(0).Cells("Info").Tag = exceptionID
                                                             frlog.Grid1.Rows(0).Cells("Mensagem").Style.ForeColor = cor
                                                             frlog.Grid1.ClearSelection()
                                                             Try
                                                                 frlog.Grid1.FirstDisplayedScrollingRowIndex = 0
                                                             Catch ex As Exception
                                                             End Try
                                                         End If

                                                     End If

                                                 End Sub))

            End If

        End If

        End If

    End Sub

    Public Sub WriteMessage(ByVal message As String)
        WriteToLog(message, Color.Black, SharedClasses.DWSIM.Flowsheet.MessageType.Information)
    End Sub

    Public Sub CheckCollections()

        If Collections.GraphicObjectCollection Is Nothing Then Collections.GraphicObjectCollection = New Dictionary(Of String, IGraphicObject)

        If Collections.FlowsheetObjectCollection Is Nothing Then Collections.FlowsheetObjectCollection = New Dictionary(Of String, SharedClasses.UnitOperations.BaseClass)

        If Collections.OPT_SensAnalysisCollection Is Nothing Then Collections.OPT_SensAnalysisCollection = New List(Of SharedClasses.Flowsheet.Optimization.SensitivityAnalysisCase)

        If Collections.OPT_OptimizationCollection Is Nothing Then Collections.OPT_OptimizationCollection = New List(Of SharedClasses.Flowsheet.Optimization.OptimizationCase)

    End Sub

#End Region

#Region "    Click Event Handlers "

    Public Sub tsbAtivar_CheckedChanged(sender As Object, e As EventArgs) Handles tsbAtivar.CheckedChanged
        GlobalSettings.Settings.CalculatorActivated = tsbAtivar.Checked
        tsbCalc.Enabled = tsbAtivar.Checked
        tsbCalcF.Enabled = tsbAtivar.Checked
        tsbAbortCalc.Enabled = tsbAtivar.Checked
        tsbSimultAdjustSolver.Enabled = tsbAtivar.Checked
    End Sub

    Private Sub ToolStripButton3_Click(sender As Object, e As EventArgs) Handles tsbAbortCalc.Click
        GlobalSettings.Settings.CalculatorStopRequested = True
        If GlobalSettings.Settings.TaskCancellationTokenSource IsNot Nothing Then
            GlobalSettings.Settings.TaskCancellationTokenSource.Cancel()
        End If
    End Sub

    Public Sub tsbSimultAdjustSolver_CheckedChanged(sender As Object, e As EventArgs) Handles tsbSimultAdjustSolver.CheckedChanged
        Me.FlowsheetOptions.SimultaneousAdjustSolverEnabled = tsbSimultAdjustSolver.Checked
    End Sub

    Private Sub ConsoleOutputTSMI_Click(sender As Object, e As EventArgs) Handles ConsoleOutputTSMI.Click
        If Calculator.ExcelLogForm Is Nothing OrElse Calculator.ExcelLogForm.IsDisposed Then
            Calculator.ExcelLogForm = New LogForm
            Dim txtwriter = New ConsoleRedirection.TextBoxStreamWriter(Calculator.ExcelLogForm.TextBox1)
            Console.SetOut(txtwriter)
        End If
        Calculator.ExcelLogForm.Show()
    End Sub

    Private Sub tsbCalcF_Click(sender As Object, e As EventArgs) Handles tsbCalcF.Click
        GlobalSettings.Settings.TaskCancellationTokenSource = Nothing
        GlobalSettings.Settings.CalculatorBusy = False
        My.Application.ActiveSimulation = Me
        FlowsheetSolver.FlowsheetSolver.SolveFlowsheet(Me, My.Settings.SolverMode, Nothing, False, False, Nothing, Nothing,
                                                        Sub()
                                                            If My.Settings.ObjectEditor = 1 Then
                                                                Me.UIThread(Sub()
                                                                                Me.FormSurface.Flowsheet = Me
                                                                                Me.FormSurface.UpdateSelectedObject()
                                                                            End Sub)
                                                            End If
                                                        End Sub, My.Computer.Keyboard.ShiftKeyDown And My.Computer.Keyboard.AltKeyDown)
    End Sub

    Public Sub RectangleToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles RectangleToolStripMenuItem.Click
        Dim myTextObject As New Shapes.RectangleGraphic(New SkiaSharp.SKPoint(10, 10), "TEXT")
        Dim gObj As GraphicObject = Nothing
        gObj = myTextObject
        gObj.Name = "RECT-" & Guid.NewGuid.ToString
        gObj.Tag = "RECT" & ((From t As GraphicObject In Me.FormSurface.FlowsheetSurface.DrawingObjects Select t Where t.ObjectType = ObjectType.GO_Text).Count + 1).ToString
        gObj.ObjectType = ObjectType.GO_Rectangle
        Me.FormSurface.FlowsheetSurface.DrawingObjects.Add(gObj)
        Me.FormSurface.Invalidate()
    End Sub

    Public Sub FiguraToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles FiguraToolStripMenuItem.Click
        With Me.OpenFileName
            .CheckFileExists = True
            .CheckPathExists = True
            .Title = DWSIM.App.GetLocalString("Adicionarfigura")
            .Filter = "Images|*.bmp;*.jpg;*.png;*.gif"
            .AddExtension = True
            .Multiselect = False
            .RestoreDirectory = True
            Dim res As DialogResult = .ShowDialog
            If res = Windows.Forms.DialogResult.OK Then
                Dim img = SkiaSharp.Views.Desktop.Extensions.ToSKImage(Bitmap.FromFile(.FileName))
                Dim gObj As GraphicObject = Nothing
                If Not img Is Nothing Then
                    Dim myEmbeddedImage As New Shapes.EmbeddedImageGraphic(100, 100, img)
                    gObj = myEmbeddedImage
                    gObj.Width = img.Width
                    gObj.Height = img.Height
                    gObj.Tag = DWSIM.App.GetLocalString("FIGURA") & Guid.NewGuid.ToString
                    gObj.AutoSize = True
                End If
                Me.FormSurface.FlowsheetSurface.DrawingObjects.Add(gObj)
                Me.FormSurface.Invalidate()
            End If
        End With

        FormSurface.TSBtabela.Checked = False

    End Sub

    Private Sub GraficoToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles GraficoToolStripMenuItem.Click
        Dim myobj As New Charts.OxyPlotGraphic(30, 30)
        myobj.Name = "CHART-" & Guid.NewGuid.ToString
        myobj.Tag = "CHART" & ((From t As GraphicObject In Me.FormSurface.FlowsheetSurface.DrawingObjects Select t Where t.ObjectType = ObjectType.GO_Chart).Count + 1).ToString
        myobj.Height = 400
        myobj.Width = 500
        myobj.Flowsheet = Me
        Me.FormSurface.FlowsheetSurface.AddObject(myobj)
        Me.FormSurface.Invalidate()
    End Sub

    Private Sub AssistenteDeCriacaoDeSubstânciasToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles CompoundCreatorWizardTSMI.Click

        FrmStSim1.loaded = False

        Dim wform As New UI.Desktop.Editors.CompoundCreatorWizard(Me)
        wform.SetupAndDisplayPage(1)

        'start dispatcher for WPF Interop
        If Not GlobalSettings.Settings.IsRunningOnMono Then System.Windows.Threading.Dispatcher.Run()

    End Sub

    Private Sub ToolStripSplitButton1_ButtonClick(sender As Object, e As EventArgs)
        If DWSIM.App.IsRunningOnMono Then
            Me.FrmStSim1 = New FormSimulSettings()
            Me.FrmStSim1.Show(Me.dckPanel)
        Else
            Me.FrmStSim1.Show(Me.dckPanel)
        End If
    End Sub

    Private Sub InspetorDeSolucoesToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles InspectorTSMI.Click

        Dim iform As New Inspector.Window
        iform.Show()

    End Sub

    Private Sub ToolStripButton1_Click(sender As Object, e As EventArgs) Handles ToolStripButton1.Click
        If DWSIM.App.IsRunningOnMono Then
            Me.FrmStSim1 = New FormSimulSettings()
            Me.FrmStSim1.Show(Me.dckPanel)
        Else
            Me.FrmStSim1.Show(Me.dckPanel)
        End If
    End Sub

    Private Sub ToolStripButton2_Click(sender As Object, e As EventArgs) Handles tsbCalc.Click
        GlobalSettings.Settings.TaskCancellationTokenSource = Nothing
        My.Application.ActiveSimulation = Me
        If My.Computer.Keyboard.ShiftKeyDown Then GlobalSettings.Settings.CalculatorBusy = False
        FlowsheetSolver.FlowsheetSolver.SolveFlowsheet(Me, My.Settings.SolverMode, Nothing, False, False, Nothing, Nothing,
                                                        Sub()
                                                            If My.Settings.ObjectEditor = 1 Then
                                                                Me.UIThread(Sub()
                                                                                Me.FormSurface.Flowsheet = Me
                                                                                Me.FormSurface.UpdateSelectedObject()
                                                                            End Sub)
                                                            End If
                                                        End Sub, My.Computer.Keyboard.CtrlKeyDown And My.Computer.Keyboard.AltKeyDown)
    End Sub

    Private Sub Button2_Click(sender As Object, e As EventArgs) Handles Button2.Click
        Process.Start("http://dwsim.inforside.com.br/wiki/index.php?title=Mobile_Compatibility_Mode")
    End Sub

    Private Sub UtilitiesTSMI_Click(sender As Object, e As EventArgs) Handles UtilitiesTSMI.DropDownOpening

        UtilitiesTSMI.DropDownItems.Clear()

        Application.DoEvents()

        UtilitiesTSMI.DropDownItems.Add(TSMIAddUtility)

        For Each obj In Me.SimulationObjects.Values
            For Each attchu In obj.AttachedUtilities
                Dim tsmi As New ToolStripMenuItem
                With tsmi
                    .Text = obj.GraphicObject.Tag & " / " & attchu.Name
                    .Image = My.Resources.cog
                End With
                AddHandler tsmi.Click, Sub()
                                           Dim f = DirectCast(attchu, DockContent)
                                           If f.Visible Then
                                               f.Select()
                                           Else
                                               obj.GetFlowsheet.DisplayForm(f)
                                           End If
                                       End Sub
                UtilitiesTSMI.DropDownItems.Add(tsmi)
            Next
        Next

    End Sub

    Private Sub FormFlowsheet_HelpRequested(sender As System.Object, hlpevent As System.Windows.Forms.HelpEventArgs) Handles MyBase.HelpRequested

        Dim obj As GraphicObject = Me.FormSurface.FlowsheetSurface.SelectedObject

        If obj Is Nothing Then
            DWSIM.App.HelpRequested("Frame.htm")
        Else
            Select Case obj.ObjectType
                Case ObjectType.MaterialStream
                    DWSIM.App.HelpRequested("SO_Material_Stream.htm")
                Case ObjectType.EnergyStream
                    DWSIM.App.HelpRequested("SO_Energy_Stream.htm")
                Case ObjectType.NodeIn
                    DWSIM.App.HelpRequested("SO_Mixer.htm")
                Case ObjectType.NodeOut
                    DWSIM.App.HelpRequested("SO_Splitter.htm")
                Case ObjectType.Vessel
                    DWSIM.App.HelpRequested("SO_Separator.htm")
                Case ObjectType.Tank
                    DWSIM.App.HelpRequested("SO_Tank.htm")
                Case ObjectType.Pipe
                    DWSIM.App.HelpRequested("SO_Pipe_Segment.htm")
                Case ObjectType.Valve
                    DWSIM.App.HelpRequested("SO_Valve.htm")
                Case ObjectType.Pump
                    DWSIM.App.HelpRequested("SO_Pump.htm")
                Case ObjectType.Compressor
                    DWSIM.App.HelpRequested("SO_Compressor.htm")
                Case ObjectType.Expander
                    DWSIM.App.HelpRequested("SO_Expander.htm")
                Case ObjectType.Heater
                    DWSIM.App.HelpRequested("SO_Heater.htm")
                Case ObjectType.Cooler
                    DWSIM.App.HelpRequested("SO_Cooler.htm")
                Case ObjectType.HeatExchanger
                    DWSIM.App.HelpRequested("SO_Heatexchanger.htm")
                Case ObjectType.ShortcutColumn
                    DWSIM.App.HelpRequested("SO_Shortcut_Column.htm")
                Case ObjectType.DistillationColumn
                    DWSIM.App.HelpRequested("SO_Rigorous_Column.htm")
                Case ObjectType.AbsorptionColumn
                    DWSIM.App.HelpRequested("NoHelp.htm") 'no topic yet
                Case ObjectType.ReboiledAbsorber
                    DWSIM.App.HelpRequested("NoHelp.htm") 'no topic yet
                Case ObjectType.RefluxedAbsorber
                    DWSIM.App.HelpRequested("NoHelp.htm") 'no topic yet
                Case ObjectType.ComponentSeparator
                    DWSIM.App.HelpRequested("SO_CompSep.htm")
                Case ObjectType.OrificePlate
                    DWSIM.App.HelpRequested("SO_OrificePlate.htm")
                Case ObjectType.CustomUO
                    DWSIM.App.HelpRequested("SO_CustomUO.htm")
                Case ObjectType.ExcelUO
                    DWSIM.App.HelpRequested("SO_ExcelUO.htm")
                Case ObjectType.FlowsheetUO
                    DWSIM.App.HelpRequested("SO_FlowsheetUO.htm")
                Case ObjectType.SolidSeparator
                    DWSIM.App.HelpRequested("SO_SolidSeparator.htm")
                Case ObjectType.Filter
                    DWSIM.App.HelpRequested("SO_CakeFilter.htm")
                Case ObjectType.RCT_Conversion, ObjectType.RCT_CSTR, ObjectType.RCT_Equilibrium, ObjectType.RCT_Gibbs, ObjectType.RCT_PFR
                    DWSIM.App.HelpRequested("SO_Reactor.htm")
                Case ObjectType.OT_Recycle, ObjectType.OT_EnergyRecycle
                    DWSIM.App.HelpRequested("SO_Recycle.htm")
                Case ObjectType.OT_Adjust
                    DWSIM.App.HelpRequested("SO_Adjust.htm")
                Case ObjectType.OT_Spec
                    DWSIM.App.HelpRequested("SO_Specification.htm")
                Case ObjectType.GO_Text
                    DWSIM.App.HelpRequested("GO_Textbox.htm")
                Case ObjectType.GO_Image
                    DWSIM.App.HelpRequested("GO_Picture.htm")
                Case ObjectType.GO_MasterTable
                    DWSIM.App.HelpRequested("GO_MasterPropertyTable.htm")
                Case ObjectType.GO_SpreadsheetTable
                    DWSIM.App.HelpRequested("GO_SpreadsheetTable.htm")
                Case Else
                    DWSIM.App.HelpRequested("Frame.htm")
            End Select
        End If

    End Sub

    Private Sub Restorelayout(sender As Object, e As EventArgs) Handles RestoreLayoutTSMI.Click

        FormLog.DockState = DockState.DockBottom
        FormMatList.DockState = DockState.Document
        FormSpreadsheet.DockState = DockState.Document
        FormSurface.DockState = DockState.Document
        FormLog.DockState = DockState.DockBottom

    End Sub

    Sub UpdateToolstripItemVisibility()

        Dim isenabled As Boolean = Me.FormSurface.FlowsheetSurface.SelectedObjects.Count > 0

        tsmiCut.Enabled = isenabled
        tsmiCopy.Enabled = isenabled
        tsmiPaste.Enabled = isenabled
        tsmiCloneSelected.Enabled = isenabled
        tsmiExportData.Enabled = isenabled
        tsmiRemoveSelected.Enabled = isenabled
        tsmiRecalc.Enabled = isenabled

    End Sub

    Public Sub tsmiUndo_Click(sender As Object, e As EventArgs) Handles tsmiUndo.Click
        tsbUndo_Click(sender, e)
    End Sub

    Public Sub tsmiRedo_Click(sender As Object, e As EventArgs) Handles tsmiRedo.Click
        tsbRedo_Click(sender, e)
    End Sub

    Public Sub tsmiCut_Click(sender As Object, e As EventArgs) Handles tsmiCut.Click
        CutObjects()
    End Sub

    Public Sub tsmiCopy_Click(sender As Object, e As EventArgs) Handles tsmiCopy.Click
        CopyObjects()
    End Sub

    Public Sub tsmiPaste_Click(sender As Object, e As EventArgs) Handles tsmiPaste.Click
        PasteObjects()
    End Sub

    Public Sub tsmiRemoveSelected_Click(sender As Object, e As EventArgs) Handles tsmiRemoveSelected.Click
        Dim n As Integer = Me.FormSurface.FlowsheetSurface.SelectedObjects.Count
        If n > 1 Then
            If MessageBox.Show("Delete " & n & " objects?", "Mass delete", MessageBoxButtons.YesNo) = Windows.Forms.DialogResult.Yes Then
                Dim indexes As New ArrayList
                For Each gobj As GraphicObject In Me.FormSurface.FlowsheetSurface.SelectedObjects.Values
                    indexes.Add(gobj.Tag)
                Next
                For Each s As String In indexes
                    Dim gobj As GraphicObject
                    gobj = GetFlowsheetGraphicObject(s)
                    If Not gobj Is Nothing Then
                        DeleteSelectedObject(sender, e, gobj, False)
                        Me.FormSurface.FlowsheetSurface.SelectedObjects.Remove(gobj.Name)
                    End If
                Next
            End If
        ElseIf n = 1 Then
            DeleteSelectedObject(sender, e, Me.FormSurface.FlowsheetSurface.SelectedObject)
        End If
    End Sub

    Public Sub tsmiCloneSelected_Click(sender As Object, e As EventArgs) Handles tsmiCloneSelected.Click
        FormSurface.CloneObject(FormSurface.FlowsheetSurface.SelectedObject)
    End Sub

    Public Sub tsmiRecalc_Click(sender As Object, e As EventArgs) Handles tsmiRecalc.Click

        If Not Me.FormSurface.FlowsheetSurface.SelectedObject Is Nothing Then

            Dim obj As SharedClasses.UnitOperations.BaseClass = Collections.FlowsheetObjectCollection(Me.FormSurface.FlowsheetSurface.SelectedObject.Name)

            'Call function to calculate flowsheet
            Dim objargs As New CalculationArgs
            With objargs
                .Calculated = False
                .Tag = obj.GraphicObject.Tag
                .Name = obj.Name
                .ObjectType = obj.GraphicObject.ObjectType
                .Sender = "PropertyGrid"
            End With

            CalculationQueue.Enqueue(objargs)

            FlowsheetSolver.FlowsheetSolver.SolveFlowsheet(Me, My.Settings.SolverMode, , True)

        End If

    End Sub

    Public Sub tsmiExportData_Click(sender As Object, e As EventArgs) Handles tsmiExportData.Click
        'copy all simulation properties from the selected object to clipboard
        Try
            Select Case Me.FormSurface.FlowsheetSurface.SelectedObject.ObjectType
                Case ObjectType.GO_MasterTable
                    Clipboard.SetText(DirectCast(Me.FormSurface.FlowsheetSurface.SelectedObject, Drawing.SkiaSharp.GraphicObjects.Tables.MasterTableGraphic).ClipboardData)
                Case ObjectType.GO_SpreadsheetTable
                    Clipboard.SetText(DirectCast(Me.FormSurface.FlowsheetSurface.SelectedObject, Drawing.SkiaSharp.GraphicObjects.Tables.SpreadsheetTableGraphic).ClipboardData)
                Case ObjectType.GO_Table
                    Clipboard.SetText(DirectCast(Me.FormSurface.FlowsheetSurface.SelectedObject, Drawing.SkiaSharp.GraphicObjects.Tables.TableGraphic).ClipboardData)
                Case ObjectType.GO_Chart
                    Clipboard.SetText(DirectCast(Me.FormSurface.FlowsheetSurface.SelectedObject, Drawing.SkiaSharp.GraphicObjects.Charts.OxyPlotGraphic).ClipboardData)
                Case Else
                    Collections.FlowsheetObjectCollection(Me.FormSurface.FlowsheetSurface.SelectedObject.Name).CopyDataToClipboard(Options.SelectedUnitSystem, Options.NumberFormat)
            End Select
        Catch ex As Exception
            WriteToLog("Error copying data to clipboard: " & ex.ToString, Color.Red, SharedClasses.DWSIM.Flowsheet.MessageType.GeneralError)
        End Try
    End Sub

    Public Sub TSBTexto_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles TextoToolStripMenuItem.Click
        Dim myTextObject As New TextGraphic(30, 30,
            DWSIM.App.GetLocalString("caixa_de_texto"))
        Dim gObj As GraphicObject = Nothing
        gObj = myTextObject
        gObj.Name = "TEXT-" & Guid.NewGuid.ToString
        gObj.Tag = "TEXT" & ((From t As GraphicObject In Me.FormSurface.FlowsheetSurface.DrawingObjects Select t Where t.ObjectType = ObjectType.GO_Text).Count + 1).ToString
        gObj.AutoSize = True
        gObj.ObjectType = ObjectType.GO_Text
        Me.FormSurface.FlowsheetSurface.DrawingObjects.Add(gObj)
        Me.FormSurface.Invalidate()

    End Sub

    Public Sub ToolStripButton19_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles TabelaDePropriedatesMestraToolStripMenuItem.Click
        Dim myMasterTable As New Drawing.SkiaSharp.GraphicObjects.Tables.MasterTableGraphic(30, 30)
        Dim gObj As GraphicObject = Nothing
        myMasterTable.Flowsheet = Me
        gObj = myMasterTable
        gObj.Name = "MASTERTABLE-" & Guid.NewGuid.ToString
        gObj.AutoSize = True
        gObj.ObjectType = ObjectType.GO_MasterTable
        Me.FormSurface.FlowsheetSurface.DrawingObjects.Add(gObj)
        Me.FormSurface.Invalidate()
    End Sub

    Public Sub ToolStripButton4_Click(sender As Object, e As EventArgs) Handles TabelaDePropriedadesPlanilhaToolStripMenuItem.Click
        Dim mySpreadsheetTable As New SpreadsheetTableGraphic(30, 30)
        Dim gObj As GraphicObject = Nothing
        mySpreadsheetTable.Flowsheet = Me
        gObj = mySpreadsheetTable
        gObj.Name = "STABLE-" & Guid.NewGuid.ToString
        gObj.Tag = "Spreadsheet Table"
        gObj.AutoSize = True
        gObj.ObjectType = ObjectType.GO_SpreadsheetTable
        Me.FormSurface.FlowsheetSurface.DrawingObjects.Add(gObj)
        Me.FormSurface.Invalidate()
    End Sub

    Private Sub FecharSimulacaoAtualToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles CloseToolStripMenuItem.Click
        Me.Close()
    End Sub

    Public Sub tsbAbortCalc_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)
        GlobalSettings.Settings.CalculatorStopRequested = True
        If GlobalSettings.Settings.TaskCancellationTokenSource IsNot Nothing Then
            GlobalSettings.Settings.TaskCancellationTokenSource.Cancel()
        End If
    End Sub

    Public Sub tsbCalc_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)
        GlobalSettings.Settings.TaskCancellationTokenSource = Nothing
        If My.Computer.Keyboard.ShiftKeyDown Then GlobalSettings.Settings.CalculatorBusy = False
        If My.Computer.Keyboard.CtrlKeyDown And My.Computer.Keyboard.AltKeyDown Then
            FlowsheetSolver.FlowsheetSolver.SolveFlowsheet(Me, My.Settings.SolverMode, ChangeCalcOrder:=True)
        Else
            FlowsheetSolver.FlowsheetSolver.SolveFlowsheet(Me, My.Settings.SolverMode)
        End If
    End Sub

    Private Sub AnaliseDeSensibilidadeToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles AnaliseDeSensibilidadeToolStripMenuItem.Click
        Me.FormSensAnalysis0 = New FormSensAnalysis
        Me.FormSensAnalysis0.Show(Me.dckPanel)
    End Sub

    Private Sub MultivariateOptimizerToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MultivariateOptimizerToolStripMenuItem.Click
        Me.FormOptimization0 = New FormOptimization
        Me.FormOptimization0.Show(Me.dckPanel)
    End Sub

    Private Sub CaracterizacaoDePetroleosFracoesC7ToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles CaracterizacaoDePetroleosFracoesC7ToolStripMenuItem.Click
        Me.FrmPCBulk.ShowDialog(Me)
    End Sub

    Private Sub CaracterizacaoDePetroleosCurvasDeDestilacaoToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles CaracterizacaoDePetroleosCurvasDeDestilacaoToolStripMenuItem.Click
        Dim frmdc As New DCCharacterizationWizard
        frmdc.ShowDialog(Me)
    End Sub

    Private Sub ExibirRelatoriosDosObjetosCAPEOPENToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles COObjTSMI.CheckedChanged
        If COObjTSMI.Checked Then
            FormCOReports.Show(dckPanel)
        Else
            FormCOReports.Hide()
        End If
        Me.Options.FlowsheetShowCOReportsWindow = COObjTSMI.Checked
    End Sub

    Private Sub PainelDeVariaveisToolStripMenuItem_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles varpaneltsmi.CheckedChanged
        If varpaneltsmi.Checked Then
            FormWatch.Show(dckPanel)
        Else
            FormWatch.Hide()
        End If
        Me.Options.FlowsheetShowWatchWindow = varpaneltsmi.Checked
    End Sub



    Private Sub SimulationConfig_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles tsmiConfigSimulation.Click
        If DWSIM.App.IsRunningOnMono Then
            Me.FrmStSim1 = New FormSimulSettings()
            Me.FrmStSim1.Show(Me.dckPanel)
        Else
            Me.FrmStSim1.Show(Me.dckPanel)
        End If
    End Sub

    Private Sub GerarRelatorioToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles GerarRelatorioToolStripMenuItem.Click
        FrmReport = New FormReportConfig
        Me.FrmReport.Show(Me)
    End Sub


    Private Sub CAPEOPENFlowsheetMonitoringObjectsMOsToolStripMenuItem_MouseHover(ByVal sender As Object, ByVal e As System.EventArgs) Handles CAPEOPENFlowsheetMonitoringObjectsMOsToolStripMenuItem.MouseHover

        If Me.CAPEOPENFlowsheetMonitoringObjectsMOsToolStripMenuItem.DropDownItems.Count = 0 Then

            Dim tsmi As New ToolStripMenuItem
            With tsmi
                .Text = "Please wait..."
                .DisplayStyle = ToolStripItemDisplayStyle.Text
                .AutoToolTip = False
            End With
            Me.CAPEOPENFlowsheetMonitoringObjectsMOsToolStripMenuItem.DropDownItems.Add(tsmi)

            Application.DoEvents()

            If FormMain.COMonitoringObjects.Count = 0 Then
                FormMain.SearchCOMOs()
            End If

            Me.CAPEOPENFlowsheetMonitoringObjectsMOsToolStripMenuItem.DropDownItems.Clear()

            tsmi = Nothing

            Application.DoEvents()

            'load CAPE-OPEN Flowsheet Monitoring Objects
            CreateCOMOList()

        End If


    End Sub



    Private Sub GerenciadorDeAmostrasDePetroleoToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles GerenciadorDeAmostrasDePetroleoToolStripMenuItem.Click
        Dim frmam As New FormAssayManager
        frmam.ShowDialog(Me)
        Try
            frmam.Close()
        Catch ex As Exception

        End Try
    End Sub

    Sub ChangeEditMenuStatus(status As Boolean)

        tsmiCut.Enabled = status
        tsmiCopy.Enabled = status
        tsmiPaste.Enabled = status
        tsmiRecalc.Enabled = status
        tsmiCloneSelected.Enabled = status
        tsmiRemoveSelected.Enabled = status
        tsmiExportData.Enabled = status
        FormSurface.tsbCutObj.Enabled = status
        FormSurface.tsbCopyObj.Enabled = status
        FormSurface.tsbPasteObj.Enabled = status

    End Sub


    Public Sub ToolStripButton6_Click(sender As Object, e As EventArgs) Handles TabelaDePropriedadesToolStripMenuItem.Click
        Dim myPropertyTable As New TableGraphic(30, 30)
        Dim gObj As GraphicObject = Nothing
        myPropertyTable.Flowsheet = Me
        gObj = myPropertyTable
        gObj.Name = "PROPERTYTABLE-" & Guid.NewGuid.ToString
        gObj.Tag = "PROPERTYTABLE-" & Guid.NewGuid.ToString
        gObj.AutoSize = True
        Me.FormSurface.FlowsheetSurface.DrawingObjects.Add(gObj)
        Me.FormSurface.Invalidate()
    End Sub

    Private Sub BlocoDeSimulacaoToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles BlocoDeSimulacaoToolStripMenuItem.Click
        Dim f As New FormAddFlowsheetObject() With {.Flowsheet = Me}
        f.ShowDialog(Me)
    End Sub

    Private Sub tsmiCloseOpenedEditors_Click(sender As Object, e As EventArgs) Handles tsmiCloseOpenedEditors.Click

        Me.UIThreadInvoke(Sub()
                              For Each obj In Me.SimulationObjects.Values
                                  obj.CloseEditForm()
                              Next
                          End Sub)

    End Sub

#End Region

#Region "    Connect/Disconnect Objects "

    Public Sub DeleteSelectedObject(ByVal sender As System.Object, ByVal e As System.EventArgs, gobj As GraphicObject, Optional ByVal confirmation As Boolean = True, Optional ByVal triggercalc As Boolean = False)

        If Not gobj Is Nothing Then
            Dim SelectedObj As GraphicObject = gobj
            Dim namesel As String = SelectedObj.Name
            If Not gobj.IsConnector Then
                Dim msgresult As MsgBoxResult
                If confirmation Then
                    If SelectedObj.ObjectType = ObjectType.GO_Image Then
                        msgresult = MessageBox.Show(DWSIM.App.GetLocalString("Excluirafiguraseleci"), DWSIM.App.GetLocalString("Excluirobjeto"), MessageBoxButtons.YesNo, MessageBoxIcon.Question)
                    ElseIf SelectedObj.ObjectType = ObjectType.GO_Rectangle Then
                        msgresult = MessageBox.Show(DWSIM.App.GetLocalString("Deleterectangle"), DWSIM.App.GetLocalString("Excluirobjeto"), MessageBoxButtons.YesNo, MessageBoxIcon.Question)
                    ElseIf SelectedObj.ObjectType = ObjectType.GO_Text Then
                        msgresult = MessageBox.Show(DWSIM.App.GetLocalString("Excluiracaixadetexto"), DWSIM.App.GetLocalString("Excluirobjeto"), MessageBoxButtons.YesNo, MessageBoxIcon.Question)
                    ElseIf SelectedObj.ObjectType = ObjectType.GO_MasterTable Then
                        msgresult = MessageBox.Show(DWSIM.App.GetLocalString("Excluir") & DirectCast(gobj, MasterTableGraphic).HeaderText & "?", DWSIM.App.GetLocalString("Excluirobjeto"), MessageBoxButtons.YesNo, MessageBoxIcon.Question)
                    ElseIf SelectedObj.ObjectType = ObjectType.GO_Table Then
                        msgresult = MessageBox.Show(DWSIM.App.GetLocalString("Excluir") & DirectCast(gobj, TableGraphic).HeaderText & "?", DWSIM.App.GetLocalString("Excluirobjeto"), MessageBoxButtons.YesNo, MessageBoxIcon.Question)
                    Else
                        msgresult = MessageBox.Show(DWSIM.App.GetLocalString("Excluir") & gobj.Tag & "?", DWSIM.App.GetLocalString("Excluirobjeto"), MessageBoxButtons.YesNo, MessageBoxIcon.Question)
                    End If
                Else
                    msgresult = MsgBoxResult.Yes
                End If
                If msgresult = MsgBoxResult.Yes Then

                    'close opened editor

                    If SelectedObj.Editor IsNot Nothing Then
                        If Not DirectCast(SelectedObj.Editor, Form).IsDisposed Then DirectCast(SelectedObj.Editor, Form).Close()
                        SelectedObj.Editor = Nothing
                    End If

                    If SelectedObj.IsEnergyStream Then

                        Dim InCon, OutCon As ConnectionPoint
                        For Each InCon In gobj.InputConnectors
                            If InCon.IsAttached = True Then DisconnectObject(InCon.AttachedConnector.AttachedFrom, gobj, False)
                        Next
                        gobj = SelectedObj
                        For Each OutCon In gobj.OutputConnectors
                            If OutCon.IsAttached = True Then DisconnectObject(gobj, OutCon.AttachedConnector.AttachedTo, False)
                        Next
                        gobj = SelectedObj

                        If My.Application.PushUndoRedoAction Then AddUndoRedoAction(New UndoRedoAction() With {.AType = UndoRedoActionType.ObjectRemoved,
                                             .NewValue = gobj,
                                             .OldValue = Me.Collections.FlowsheetObjectCollection(namesel).SaveData(),
                                             .Name = String.Format(DWSIM.App.GetLocalString("UndoRedo_ObjectRemoved"), gobj.Tag)})
                        'DWSIM
                        Me.Collections.FlowsheetObjectCollection(namesel).CloseEditForm()
                        Me.Collections.FlowsheetObjectCollection(namesel).Dispose()
                        Me.Collections.FlowsheetObjectCollection.Remove(namesel)
                        Me.Collections.GraphicObjectCollection.Remove(namesel)
                        Me.FormSurface.FlowsheetSurface.DeleteSelectedObject(gobj)

                    Else

                        If SelectedObj.ObjectType = ObjectType.GO_Image Then
                            Me.FormSurface.FlowsheetSurface.DeleteSelectedObject(gobj)
                        ElseIf SelectedObj.ObjectType = ObjectType.GO_Rectangle Then
                            Me.FormSurface.FlowsheetSurface.DeleteSelectedObject(gobj)
                        ElseIf SelectedObj.ObjectType = ObjectType.GO_Table Then
                            Me.FormSurface.FlowsheetSurface.DeleteSelectedObject(gobj)
                        ElseIf SelectedObj.ObjectType = ObjectType.GO_MasterTable Then
                            Me.FormSurface.FlowsheetSurface.DeleteSelectedObject(gobj)
                        ElseIf SelectedObj.ObjectType = ObjectType.GO_Text Then
                            Me.FormSurface.FlowsheetSurface.DeleteSelectedObject(gobj)
                        ElseIf SelectedObj.ObjectType = ObjectType.GO_FloatingTable Then
                            Me.FormSurface.FlowsheetSurface.DeleteSelectedObject(gobj)
                        ElseIf SelectedObj.ObjectType = ObjectType.GO_SpreadsheetTable Then
                            Me.FormSurface.FlowsheetSurface.DeleteSelectedObject(gobj)
                        ElseIf SelectedObj.ObjectType = ObjectType.GO_Chart Then
                            Me.FormSurface.FlowsheetSurface.DeleteSelectedObject(gobj)
                        Else

                            Dim obj As SharedClasses.UnitOperations.BaseClass = Me.Collections.FlowsheetObjectCollection(SelectedObj.Name)

                            gobj = SelectedObj

                            If gobj.EnergyConnector.IsAttached = True Then DisconnectObject(gobj, gobj.EnergyConnector.AttachedConnector.AttachedTo, False)

                            Dim InCon, OutCon As ConnectionPoint
                            For Each InCon In gobj.InputConnectors
                                Try
                                    If InCon.IsAttached = True Then DisconnectObject(InCon.AttachedConnector.AttachedFrom, gobj, False)
                                Catch ex As Exception

                                End Try
                            Next
                            gobj = SelectedObj
                            For Each OutCon In gobj.OutputConnectors
                                Try
                                    If OutCon.IsAttached = True Then DisconnectObject(gobj, OutCon.AttachedConnector.AttachedTo, False)
                                Catch ex As Exception

                                End Try
                            Next

                            gobj = SelectedObj

                            If My.Application.PushUndoRedoAction Then AddUndoRedoAction(New UndoRedoAction() With {.AType = UndoRedoActionType.ObjectRemoved,
                                                                         .NewValue = gobj,
                                                                         .OldValue = Me.Collections.FlowsheetObjectCollection(namesel).SaveData(),
                                                                         .Name = String.Format(DWSIM.App.GetLocalString("UndoRedo_ObjectRemoved"), gobj.Tag)})

                            If gobj.ObjectType = ObjectType.OT_Spec Then
                                Dim specobj As Spec = Me.Collections.FlowsheetObjectCollection(namesel)
                                If Me.Collections.FlowsheetObjectCollection.ContainsKey(specobj.TargetObjectData.ID) Then
                                    Me.Collections.FlowsheetObjectCollection(specobj.TargetObjectData.ID).IsSpecAttached = False
                                    Me.Collections.FlowsheetObjectCollection(specobj.TargetObjectData.ID).AttachedSpecId = ""
                                End If
                                If Me.Collections.FlowsheetObjectCollection.ContainsKey(specobj.SourceObjectData.ID) Then
                                    Me.Collections.FlowsheetObjectCollection(specobj.SourceObjectData.ID).IsSpecAttached = False
                                    Me.Collections.FlowsheetObjectCollection(specobj.SourceObjectData.ID).AttachedSpecId = ""
                                End If
                            ElseIf gobj.ObjectType = ObjectType.OT_Adjust Then
                                Dim adjobj As Adjust = Me.Collections.FlowsheetObjectCollection(namesel)
                                If Me.Collections.FlowsheetObjectCollection.ContainsKey(adjobj.ManipulatedObjectData.ID) Then
                                    Me.Collections.FlowsheetObjectCollection(adjobj.ManipulatedObjectData.ID).IsAdjustAttached = False
                                    Me.Collections.FlowsheetObjectCollection(adjobj.ManipulatedObjectData.ID).AttachedAdjustId = ""
                                End If
                                If Me.Collections.FlowsheetObjectCollection.ContainsKey(adjobj.ControlledObjectData.ID) Then
                                    Me.Collections.FlowsheetObjectCollection(adjobj.ControlledObjectData.ID).IsAdjustAttached = False
                                    Me.Collections.FlowsheetObjectCollection(adjobj.ControlledObjectData.ID).AttachedAdjustId = ""
                                End If
                                If Me.Collections.FlowsheetObjectCollection.ContainsKey(adjobj.ReferencedObjectData.ID) Then
                                    Me.Collections.FlowsheetObjectCollection(adjobj.ReferencedObjectData.ID).IsAdjustAttached = False
                                    Me.Collections.FlowsheetObjectCollection(adjobj.ReferencedObjectData.ID).AttachedAdjustId = ""
                                End If
                            End If

                            'dispose object
                            Me.Collections.FlowsheetObjectCollection(namesel).CloseEditForm()
                            Me.Collections.FlowsheetObjectCollection(namesel).Dispose()

                            Me.Collections.FlowsheetObjectCollection.Remove(namesel)
                            Me.Collections.GraphicObjectCollection.Remove(namesel)

                            Me.FormSurface.FlowsheetSurface.DeleteSelectedObject(gobj)

                        End If

                    End If

                    For Each obj In Me.SimulationObjects.Values
                        obj.UpdateEditForm()
                    Next

                End If

            End If

        End If

    End Sub

    Public Sub DeleteObject(ByVal tag As String, Optional ByVal confirmation As Boolean = True)

        Dim gobj As GraphicObject = Me.GetFlowsheetGraphicObject(tag)

        If Not gobj Is Nothing Then
            Me.FormSurface.FlowsheetSurface.SelectedObject = gobj
            Me.DeleteSelectedObject(Me, New EventArgs(), gobj, confirmation)
        End If

    End Sub

    Public Sub DisconnectObject(ByRef gObjFrom As GraphicObject, ByRef gObjTo As GraphicObject, Optional ByVal triggercalc As Boolean = False)

        Me.WriteToLog(DWSIM.App.GetLocalTipString("FLSH007"), Color.Black, SharedClasses.DWSIM.Flowsheet.MessageType.Tip)

        Dim conObj As ConnectorGraphic = Nothing
        Dim SelObj As GraphicObject = gObjFrom
        Dim ObjToDisconnect As GraphicObject = Nothing
        Dim gobj1 As GraphicObject = Nothing
        Dim gobj2 As GraphicObject = Nothing
        ObjToDisconnect = gObjTo
        Dim i1, i2 As Integer
        If Not ObjToDisconnect Is Nothing Then
            Dim conptObj As ConnectionPoint = Nothing
            For Each conptObj In SelObj.InputConnectors
                If conptObj.IsAttached = True Then
                    If Not conptObj.AttachedConnector Is Nothing Then
                        If conptObj.AttachedConnector.AttachedFrom.Name.ToString = ObjToDisconnect.Name.ToString Then
                            i1 = conptObj.AttachedConnector.AttachedFromConnectorIndex
                            i2 = conptObj.AttachedConnector.AttachedToConnectorIndex
                            gobj1 = gObjTo
                            gobj2 = gObjFrom
                            conptObj.AttachedConnector.AttachedFrom.OutputConnectors(conptObj.AttachedConnector.AttachedFromConnectorIndex).IsAttached = False
                            conptObj.AttachedConnector.AttachedFrom.OutputConnectors(conptObj.AttachedConnector.AttachedFromConnectorIndex).AttachedConnector = Nothing
                            Me.FormSurface.FlowsheetSurface.SelectedObjects.Clear()
                            conptObj.IsAttached = False
                            Me.FormSurface.FlowsheetSurface.DeleteSelectedObject(conptObj.AttachedConnector)
                        End If
                    End If
                End If
            Next
            For Each conptObj In SelObj.OutputConnectors
                If conptObj.IsAttached = True Then
                    If Not conptObj.AttachedConnector Is Nothing Then
                        If conptObj.AttachedConnector.AttachedTo.Name.ToString = ObjToDisconnect.Name.ToString Then
                            i1 = conptObj.AttachedConnector.AttachedFromConnectorIndex
                            i2 = conptObj.AttachedConnector.AttachedToConnectorIndex
                            gobj1 = gObjFrom
                            gobj2 = gObjTo
                            conptObj.AttachedConnector.AttachedTo.InputConnectors(conptObj.AttachedConnector.AttachedToConnectorIndex).IsAttached = False
                            conptObj.AttachedConnector.AttachedTo.InputConnectors(conptObj.AttachedConnector.AttachedToConnectorIndex).AttachedConnector = Nothing
                            conptObj.IsAttached = False
                            Me.FormSurface.FlowsheetSurface.DeleteSelectedObject(conptObj.AttachedConnector)
                        End If
                    End If
                End If
            Next
            If SelObj.EnergyConnector.IsAttached = True Then
                If SelObj.EnergyConnector.AttachedConnector.AttachedTo.Name.ToString = ObjToDisconnect.Name.ToString Then
                    i1 = SelObj.EnergyConnector.AttachedConnector.AttachedFromConnectorIndex
                    i2 = SelObj.EnergyConnector.AttachedConnector.AttachedToConnectorIndex
                    gobj1 = SelObj
                    gobj2 = ObjToDisconnect
                    SelObj.EnergyConnector.AttachedConnector.AttachedTo.InputConnectors(SelObj.EnergyConnector.AttachedConnector.AttachedToConnectorIndex).IsAttached = False
                    SelObj.EnergyConnector.AttachedConnector.AttachedTo.InputConnectors(SelObj.EnergyConnector.AttachedConnector.AttachedToConnectorIndex).AttachedConnector = Nothing
                    SelObj.EnergyConnector.IsAttached = False
                    Me.FormSurface.FlowsheetSurface.DeleteSelectedObject(SelObj.EnergyConnector.AttachedConnector)
                End If
            End If
        End If

        If My.Application.PushUndoRedoAction Then AddUndoRedoAction(New UndoRedoAction() With {.AType = UndoRedoActionType.FlowsheetObjectDisconnected,
                                     .ObjID = gobj1.Name,
                                     .ObjID2 = gobj2.Name,
                                     .OldValue = i1,
                                     .NewValue = i2,
                                     .Name = String.Format(DWSIM.App.GetLocalString("UndoRedo_ObjectDisconnected"), gobj1.Tag, gobj2.Tag)})

        'If triggercalc Then ProcessCalculationQueue(Me, Nothing, False, False) Else Me.CalculationQueue.Clear()

    End Sub

    Public Sub ConnectObject(ByRef gObjFrom As GraphicObject, ByRef gObjTo As GraphicObject, Optional ByVal fidx As Integer = -1, Optional ByVal tidx As Integer = -1)

        Me.WriteToLog(DWSIM.App.GetLocalTipString("FLSH007"), Color.Black, SharedClasses.DWSIM.Flowsheet.MessageType.Tip)

        Me.FormSurface.FlowsheetSurface.ConnectObject(gObjFrom, gObjTo, fidx, tidx)

    End Sub

#End Region

#Region "    Plugin/CAPE-OPEN MO Management "

    Private Sub CreatePluginsList()

        'process plugin list

        For Each iplugin As Interfaces.IUtilityPlugin In My.Application.UtilityPlugins.Values

            Dim tsmi As New ToolStripMenuItem
            With tsmi
                .Text = iplugin.Name
                .Tag = iplugin.UniqueID
                .Image = My.Resources.plugin
                .DisplayStyle = ToolStripItemDisplayStyle.ImageAndText
            End With
            Me.PluginsToolStripMenuItem.DropDownItems.Add(tsmi)
            AddHandler tsmi.Click, AddressOf Me.PluginClick
        Next

    End Sub

    Private Sub PluginClick(ByVal sender As System.Object, ByVal e As System.EventArgs)

        Dim tsmi As ToolStripMenuItem = CType(sender, ToolStripMenuItem)

        Dim myUPlugin As Interfaces.IUtilityPlugin = My.Application.UtilityPlugins.Item(tsmi.Tag)

        myUPlugin.SetFlowsheet(Me)
        Select Case myUPlugin.DisplayMode
            Case Interfaces.IUtilityPlugin.DispMode.Normal
                myUPlugin.UtilityForm.Show(Me)
            Case Interfaces.IUtilityPlugin.DispMode.Modal
                myUPlugin.UtilityForm.ShowDialog(Me)
            Case Interfaces.IUtilityPlugin.DispMode.Dockable
                CType(myUPlugin.UtilityForm, Docking.DockContent).Show(Me.dckPanel)
        End Select

    End Sub

    Private Sub CreateCOMOList()

        'process plugin list

        For Each icomo As UnitOperations.UnitOperations.Auxiliary.CapeOpen.CapeOpenUnitOpInfo In FormMain.COMonitoringObjects.Values

            Dim tsmi As New ToolStripMenuItem
            With tsmi
                .Text = icomo.Name
                .Tag = icomo.TypeName
                .Image = My.Resources.colan2
                .DisplayStyle = ToolStripItemDisplayStyle.ImageAndText
                .AutoToolTip = False
            End With
            With icomo
                tsmi.ToolTipText = "TypeName: " & vbTab & .TypeName & vbCrLf &
                                    "Version: " & vbTab & vbTab & .Version & vbCrLf &
                                    "Vendor URL: " & vbTab & .VendorURL & vbCrLf &
                                    "About: " & vbTab & vbTab & .AboutInfo
            End With
            Me.CAPEOPENFlowsheetMonitoringObjectsMOsToolStripMenuItem.DropDownItems.Add(tsmi)
            AddHandler tsmi.Click, AddressOf Me.COMOClick
        Next

    End Sub

    Private Sub COMOClick(ByVal sender As System.Object, ByVal e As System.EventArgs)

        Dim tsmi As ToolStripMenuItem = CType(sender, ToolStripMenuItem)

        Dim myCOMO As UnitOperations.UnitOperations.Auxiliary.CapeOpen.CapeOpenUnitOpInfo = FormMain.COMonitoringObjects.Item(tsmi.Tag)

        Dim _como As Object = Nothing
        Try
            Dim t As Type = Type.GetTypeFromProgID(myCOMO.TypeName)
            _como = Activator.CreateInstance(t)
            If TryCast(_como, CapeOpen.ICapeUtilities) IsNot Nothing Then
                If TryCast(_como, IPersistStreamInit) IsNot Nothing Then
                    CType(_como, IPersistStreamInit).InitNew()
                End If
                With CType(_como, CapeOpen.ICapeUtilities)
                    .Initialize()
                    .simulationContext = Me
                    .Edit()
                End With
            End If
        Catch ex As Exception
            Me.WriteToLog("Error creating CAPE-OPEN Flowsheet Monitoring Object: " & ex.ToString, Color.Red, SharedClasses.DWSIM.Flowsheet.MessageType.GeneralError)
        Finally
            If TryCast(_como, CapeOpen.ICapeUtilities) IsNot Nothing Then
                With CType(_como, CapeOpen.ICapeUtilities)
                    .Terminate()
                End With
            End If
        End Try

    End Sub

#End Region

#Region "    CAPE-OPEN COSE/PME Methods and Properties "

    Public Function NamedValue(ByVal value As String) As Object Implements CapeOpen.ICapeCOSEUtilities.NamedValue

        Return NamedValueList()

    End Function

    Public ReadOnly Property NamedValueList() As Object Implements CapeOpen.ICapeCOSEUtilities.NamedValueList
        Get
            Return New String() {Nothing}
        End Get
    End Property

    Public Sub LogMessage(ByVal message As String) Implements CapeOpen.ICapeDiagnostic.LogMessage
        Me.WriteMessage(message)
    End Sub

    Public Sub PopUpMessage(ByVal message As String) Implements CapeOpen.ICapeDiagnostic.PopUpMessage
        MessageBox.Show(message)
    End Sub

    Public Function CreateMaterialTemplate(ByVal materialTemplateName As String) As Object Implements CapeOpen.ICapeMaterialTemplateSystem.CreateMaterialTemplate
        For Each pp As Thermodynamics.PropertyPackages.PropertyPackage In Me.Options.PropertyPackages.Values
            If materialTemplateName = pp.ComponentName Then
                Dim mat As New Streams.MaterialStream("temporary stream", "temporary stream", Me, pp)
                Me.AddComponentsRows(mat)
                Return mat
                Exit For
            Else
                Return Nothing
            End If
        Next
        Return Nothing
    End Function

    Public ReadOnly Property MaterialTemplates() As Object Implements CapeOpen.ICapeMaterialTemplateSystem.MaterialTemplates
        Get
            Dim pps As New ArrayList
            For Each p As Thermodynamics.PropertyPackages.PropertyPackage In Me.Options.PropertyPackages.Values
                pps.Add(p.ComponentName)
            Next
            Dim arr2(pps.Count - 1) As String
            Array.Copy(pps.ToArray, arr2, pps.Count)
            Return arr2
        End Get
    End Property

    Public Function GetStreamCollection() As Object Implements CapeOpen.ICapeFlowsheetMonitoring.GetStreamCollection
        Dim _col As New CCapeCollection
        For Each o As SharedClasses.UnitOperations.BaseClass In Me.Collections.FlowsheetObjectCollection.Values
            If TryCast(o, CapeOpen.ICapeThermoMaterialObject) IsNot Nothing Then
                'object is a CAPE-OPEN Material Object
                _col._icol.Add(o)
            ElseIf TryCast(o, CapeOpen.ICapeCollection) IsNot Nothing Then
                'object is a CAPE-OPEN Energy Object
                _col._icol.Add(o)
            End If
        Next
        Return _col
    End Function

    Public Function GetUnitOperationCollection() As Object Implements CapeOpen.ICapeFlowsheetMonitoring.GetUnitOperationCollection
        Dim _col As New CCapeCollection
        For Each o As SharedClasses.UnitOperations.BaseClass In Me.Collections.FlowsheetObjectCollection.Values
            If TryCast(o, CapeOpen.ICapeUnit) IsNot Nothing Then
                'object is a CAPE-OPEN Unit Operation
                _col._icol.Add(o)
            End If
        Next
        Return _col
    End Function

    Public ReadOnly Property SolutionStatus() As CapeOpen.CapeSolutionStatus Implements CapeOpen.ICapeFlowsheetMonitoring.SolutionStatus
        Get
            Return CapeOpen.CapeSolutionStatus.CAPE_SOLVED
        End Get
    End Property

    Public ReadOnly Property ValStatus() As CapeOpen.CapeValidationStatus Implements CapeOpen.ICapeFlowsheetMonitoring.ValStatus
        Get
            Return CapeOpen.CapeValidationStatus.CAPE_VALID
        End Get
    End Property

    Public Property ComponentDescription() As String Implements CapeOpen.ICapeIdentification.ComponentDescription
        Get
            Return Me.Options.SimulationComments
        End Get
        Set(ByVal value As String)
            Me.Options.SimulationComments = value
        End Set
    End Property

    Public Property ComponentName() As String Implements CapeOpen.ICapeIdentification.ComponentName
        Get
            Return Me.Options.SimulationName
        End Get
        Set(ByVal value As String)
            Me.Options.SimulationName = value
        End Set
    End Property

#End Region

#Region "    Script Timers"

    Private Sub TimerScripts1_Tick(sender As Object, e As EventArgs) Handles TimerScripts1.Tick
        Me.ProcessScripts(Enums.Scripts.EventType.SimulationTimer1, Enums.Scripts.ObjectType.Simulation, "")
    End Sub

    Private Sub TimerScripts5_Tick(sender As Object, e As EventArgs) Handles TimerScripts5.Tick
        Me.ProcessScripts(Enums.Scripts.EventType.SimulationTimer5, Enums.Scripts.ObjectType.Simulation, "")
    End Sub

    Private Sub TimerScripts15_Tick(sender As Object, e As EventArgs) Handles TimerScripts15.Tick
        Me.ProcessScripts(Enums.Scripts.EventType.SimulationTimer15, Enums.Scripts.ObjectType.Simulation, "")
    End Sub

    Private Sub TimerScripts30_Tick(sender As Object, e As EventArgs) Handles TimerScripts30.Tick
        Me.ProcessScripts(Enums.Scripts.EventType.SimulationTimer30, Enums.Scripts.ObjectType.Simulation, "")
    End Sub

    Private Sub TimerScripts60_Tick(sender As Object, e As EventArgs) Handles TimerScripts60.Tick
        Me.ProcessScripts(Enums.Scripts.EventType.SimulationTimer60, Enums.Scripts.ObjectType.Simulation, "")
    End Sub

#End Region

#Region "    Question Box"

    Private Sub QuestionBox_Button1_Click(sender As Object, e As EventArgs) Handles QuestionBox_Button1.Click
        Me.QuestionBox_Panel.Visible = False
        Select Case QuestionID
            Case 0 'question about adding or not a new user-defined unit from the simulation file
                AddUnitSystem(Me.Options.SelectedUnitSystem)
        End Select
    End Sub

    Private Sub QuestionBox_Button2_Click(sender As Object, e As EventArgs) Handles QuestionBox_Button2.Click
        Me.QuestionBox_Panel.Visible = False
    End Sub

    Sub ShowQuestionPanel(ByVal icon As MessageBoxIcon, ByVal question As String, ByVal button1visible As Boolean, ByVal button1text As String, ByVal button2visible As Boolean, ByVal button2text As String)

        Me.QuestionBox_Panel.Visible = True

        Select Case icon
            Case MessageBoxIcon.Information
                QuestionBox_PictureBox1.Image = My.Resources.information
            Case MessageBoxIcon.Error
                QuestionBox_PictureBox1.Image = My.Resources.cross
            Case MessageBoxIcon.Exclamation
                QuestionBox_PictureBox1.Image = My.Resources.exclamation
            Case MessageBoxIcon.Question
                QuestionBox_PictureBox1.Image = My.Resources.help
            Case MessageBoxIcon.Warning
                QuestionBox_PictureBox1.Image = My.Resources._error
        End Select

        QuestionBox_Label1.Text = question

        QuestionBox_Button1.Visible = button1visible
        QuestionBox_Button2.Visible = button2visible

        QuestionBox_Button1.Text = button1text
        QuestionBox_Button2.Text = button2text

    End Sub

#End Region

#Region "    Cut/Copy/Paste Objects"

    Sub CutObjects(Optional ByVal addundo As Boolean = True)

        CopyObjects()

        My.Application.PushUndoRedoAction = False

        If addundo Then AddUndoRedoAction(New UndoRedoAction() With {.AType = UndoRedoActionType.CutObjects,
                                     .NewValue = Clipboard.GetText,
                                     .OldValue = Me.FormSurface.FlowsheetSurface.SelectedObjects.Values.ToList,
                                     .Name = DWSIM.App.GetLocalString("UndoRedo_Cut")})

        Dim indexes As New ArrayList
        For Each gobj As GraphicObject In Me.FormSurface.FlowsheetSurface.SelectedObjects.Values
            indexes.Add(gobj.Tag)
        Next
        For Each s As String In indexes
            Dim gobj As GraphicObject
            gobj = Me.GetFlowsheetGraphicObject(s)
            If Not gobj Is Nothing Then
                Me.DeleteSelectedObject(Me, New EventArgs(), gobj, False)
                Me.FormSurface.FlowsheetSurface.SelectedObjects.Remove(gobj.Name)
            End If
        Next

        My.Application.PushUndoRedoAction = True

    End Sub

    Sub CopyObjects()

        Dim xdoc As New XDocument()
        Dim xel As XElement

        Dim ppackages As New List(Of String)

        Dim ci As CultureInfo = CultureInfo.InvariantCulture

        xdoc.Add(New XElement("DWSIM_Simulation_Data"))

        xdoc.Element("DWSIM_Simulation_Data").Add(New XElement("SimulationObjects"))
        xel = xdoc.Element("DWSIM_Simulation_Data").Element("SimulationObjects")

        For Each so As SharedClasses.UnitOperations.BaseClass In Collections.FlowsheetObjectCollection.Values
            If so.GraphicObject.Selected Then
                xel.Add(New XElement("SimulationObject", {so.SaveData().ToArray()}))
                If TypeOf so Is Streams.MaterialStream Then
                    If Not ppackages.Contains(DirectCast(so, Streams.MaterialStream).PropertyPackage.Name) Then
                        ppackages.Add(DirectCast(so, Streams.MaterialStream).PropertyPackage.Name)
                    End If
                ElseIf TypeOf so Is UnitOpBaseClass Then
                    If Not ppackages.Contains(DirectCast(so, UnitOpBaseClass).PropertyPackage.Name) Then
                        ppackages.Add(DirectCast(so, UnitOpBaseClass).PropertyPackage.Name)
                    End If
                End If
            End If
        Next

        xdoc.Element("DWSIM_Simulation_Data").Add(New XElement("GraphicObjects"))
        xel = xdoc.Element("DWSIM_Simulation_Data").Element("GraphicObjects")

        For Each go As GraphicObject In FormSurface.FlowsheetSurface.DrawingObjects
            If Not go.IsConnector And go.Selected Then xel.Add(New XElement("GraphicObject", go.SaveData().ToArray()))
        Next

        xdoc.Element("DWSIM_Simulation_Data").Add(New XElement("PropertyPackages"))
        xel = xdoc.Element("DWSIM_Simulation_Data").Element("PropertyPackages")

        For Each pp In Options.PropertyPackages
            Dim createdms As Boolean = False
            If pp.Value.CurrentMaterialStream Is Nothing Then
                Dim ms As New Streams.MaterialStream("", "", Me, pp.Value)
                AddComponentsRows(ms)
                pp.Value.CurrentMaterialStream = ms
                createdms = True
            End If
            xel.Add(New XElement("PropertyPackage", {New XElement("ID", pp.Key),
                                                        pp.Value.SaveData().ToArray()}))
            If createdms Then pp.Value.CurrentMaterialStream = Nothing
        Next

        xdoc.Element("DWSIM_Simulation_Data").Add(New XElement("Compounds"))
        xel = xdoc.Element("DWSIM_Simulation_Data").Element("Compounds")

        For Each cp As ConstantProperties In Options.SelectedComponents.Values
            xel.Add(New XElement("Compound", cp.SaveData().ToArray()))
        Next

        Clipboard.SetText(xdoc.ToString)

    End Sub

    Sub PasteObjects(Optional ByVal addundo As Boolean = True)

        My.Application.PushUndoRedoAction = False

        Dim pkey As String = New Random().Next().ToString & "_"

        Dim ci As CultureInfo = CultureInfo.InvariantCulture

        Dim excs As New Concurrent.ConcurrentBag(Of Exception)

        Dim xdoc As XDocument = XDocument.Parse(Clipboard.GetText())

        Dim data As List(Of XElement) = xdoc.Element("DWSIM_Simulation_Data").Element("GraphicObjects").Elements.ToList

        FormMain.AddGraphicObjects(Me, data, excs, pkey, 40, True)

        Dim pp As New PropertyPackages.RaoultPropertyPackage()

        If My.Settings.ClipboardCopyMode_Compounds = 1 Then

            data = xdoc.Element("DWSIM_Simulation_Data").Element("Compounds").Elements.ToList

            Dim complist As New List(Of ConstantProperties)

            For Each xel As XElement In data
                Dim obj As New ConstantProperties
                obj.LoadData(xel.Elements.ToList)
                complist.Add(obj)
            Next

            Dim idx As Integer

            If Not Me.FrmStSim1.initialized Then Me.FrmStSim1.Init()

            For Each comp In complist
                If Not Me.Options.SelectedComponents.ContainsKey(comp.Name) Then
                    If Not Me.Options.NotSelectedComponents.ContainsKey(comp.Name) Then
                        idx = Me.FrmStSim1.AddCompToGrid(comp)
                    Else
                        For Each r As DataGridViewRow In Me.FrmStSim1.ogc1.Rows
                            If r.Cells(0).Value = comp.Name Then
                                idx = r.Index
                                Exit For
                            End If
                        Next
                    End If
                    Me.FrmStSim1.AddCompToSimulation(comp.Name)
                End If
            Next

        End If

        If My.Settings.ClipboardCopyMode_PropertyPackages = 1 Then

            data = xdoc.Element("DWSIM_Simulation_Data").Element("PropertyPackages").Elements.ToList

            For Each xel As XElement In data
                Try
                    Dim obj As Thermodynamics.PropertyPackages.PropertyPackage = pp.ReturnInstance(xel.Element("Type").Value)
                    obj.LoadData(xel.Elements.ToList)
                    obj.UniqueID = pkey & obj.UniqueID
                    obj.Tag = obj.Tag & " (C)"
                    obj.Flowsheet = Me
                    Me.Options.PropertyPackages.Add(obj.UniqueID, obj)
                Catch ex As Exception
                    excs.Add(New Exception("Error Loading Property Package Information", ex))
                End Try
            Next

        End If

        data = xdoc.Element("DWSIM_Simulation_Data").Element("SimulationObjects").Elements.ToList

        FormSurface.FlowsheetSurface.SelectedObject = Nothing
        FormSurface.FlowsheetSurface.SelectedObjects.Clear()

        Dim objlist As New Concurrent.ConcurrentBag(Of SharedClasses.UnitOperations.BaseClass)

        Dim compoundstoremove As New List(Of String)

        For Each xel As XElement In data
            Dim id As String = pkey & xel.<Name>.Value
            Dim obj As SharedClasses.UnitOperations.BaseClass = Nothing
            If xel.Element("Type").Value.Contains("MaterialStream") Then
                obj = pp.ReturnInstance(xel.Element("Type").Value)
            Else
                obj = UnitOperations.Resolver.ReturnInstance(xel.Element("Type").Value)
            End If
            Dim gobj As GraphicObject = (From go As GraphicObject In
                                FormSurface.FlowsheetSurface.DrawingObjects Where go.Name = id).SingleOrDefault
            obj.GraphicObject = gobj
            gobj.Owner = obj
            obj.SetFlowsheet(Me)
            If Not gobj Is Nothing Then
                obj.LoadData(xel.Elements.ToList)
                If TypeOf obj Is Streams.MaterialStream Then
                    If My.Settings.ClipboardCopyMode_Compounds = 0 Then
                        For Each subst As Compound In DirectCast(obj, Streams.MaterialStream).Phases(0).Compounds.Values
                            If Not Options.SelectedComponents.ContainsKey(subst.Name) And Not compoundstoremove.Contains(subst.Name) Then compoundstoremove.Add(subst.Name)
                        Next
                    End If
                    For Each phase As BaseClasses.Phase In DirectCast(obj, Streams.MaterialStream).Phases.Values
                        For Each c As ConstantProperties In Options.SelectedComponents.Values
                            If Not phase.Compounds.ContainsKey(c.Name) Then phase.Compounds.Add(c.Name, New Compound(c.Name, "") With {.ConstantProperties = c})
                            phase.Compounds(c.Name).ConstantProperties = c
                        Next
                    Next
                End If
            End If
            If My.Settings.ClipboardCopyMode_PropertyPackages = 1 Then
                If TypeOf obj Is Streams.MaterialStream Then
                    DirectCast(obj, Streams.MaterialStream).PropertyPackage = Me.Options.PropertyPackages(pkey & DirectCast(obj, Streams.MaterialStream)._ppid)
                ElseIf TypeOf obj Is UnitOpBaseClass Then
                    If DirectCast(obj, UnitOpBaseClass)._ppid <> "" Then
                        DirectCast(obj, UnitOpBaseClass).PropertyPackage = Me.Options.PropertyPackages(pkey & DirectCast(obj, UnitOpBaseClass)._ppid)
                    End If
                End If
            End If
            objlist.Add(obj)
        Next

        If My.Settings.ClipboardCopyMode_Compounds = 0 Then

            For Each obj As SharedClasses.UnitOperations.BaseClass In objlist
                If TypeOf obj Is Streams.MaterialStream Then
                    For Each phase As BaseClasses.Phase In DirectCast(obj, Streams.MaterialStream).Phases.Values
                        For Each comp In compoundstoremove
                            phase.Compounds.Remove(comp)
                        Next
                    Next
                End If
            Next

        End If

        pp.Dispose()
        pp = Nothing

        FormMain.AddSimulationObjects(Me, objlist, excs, pkey)

        For Each obj In objlist
            If FormSurface.FlowsheetSurface.SelectedObject Is Nothing Then FormSurface.FlowsheetSurface.SelectedObject = obj.GraphicObject
            FormSurface.FlowsheetSurface.SelectedObjects.Add(obj.Name, obj.GraphicObject)
        Next

        If addundo Then AddUndoRedoAction(New UndoRedoAction() With {.AType = UndoRedoActionType.PasteObjects,
                                     .OldValue = Clipboard.GetText,
                                     .NewValue = Me.FormSurface.FlowsheetSurface.SelectedObjects.Values.ToList,
                                     .Name = DWSIM.App.GetLocalString("UndoRedo_Paste")})

        My.Application.PushUndoRedoAction = True

    End Sub

#End Region

#Region "    Undo/Redo Handlers"

    Sub ProcessAction(act As UndoRedoAction, undo As Boolean)

        Try

            Dim pval As Object = Nothing

            If undo Then pval = act.OldValue Else pval = act.NewValue

            Select Case act.AType

                Case UndoRedoActionType.SimulationObjectPropertyChanged

                    Dim fobj = Me.Collections.FlowsheetObjectCollection(act.ObjID)

                    If fobj.GetProperties(Interfaces.Enums.PropertyType.ALL).Contains(act.PropertyName) Then
                        'Property is listed, set using SetProperty
                        fobj.SetPropertyValue(act.PropertyName, pval, act.Tag)
                    Else
                        'Property not listed, set using Reflection
                        Dim method As FieldInfo = fobj.GetType().GetField(act.PropertyName)
                        If Not method Is Nothing Then
                            method.SetValue(fobj, pval)
                        Else
                            fobj.GetType().GetProperty(act.PropertyName).SetValue(fobj, pval, Nothing)
                        End If
                    End If

                Case UndoRedoActionType.FlowsheetObjectPropertyChanged

                    Dim gobj = FormFlowsheet.SearchSurfaceObjectsByName(act.ObjID, FormSurface.FlowsheetSurface)

                    'Property not listed, set using Reflection
                    Dim method As FieldInfo = gobj.GetType().GetField(act.PropertyName)
                    If Not method Is Nothing Then
                        method.SetValue(gobj, pval)
                    Else
                        gobj.GetType().GetProperty(act.PropertyName).SetValue(gobj, pval, Nothing)
                    End If

                Case UndoRedoActionType.CompoundAdded

                    If undo Then
                        Me.FrmStSim1.RemoveCompFromSimulation(act.ObjID)
                    Else
                        Me.FrmStSim1.AddCompToSimulation(act.ObjID)
                    End If

                Case UndoRedoActionType.CompoundRemoved

                    If undo Then
                        Me.FrmStSim1.AddCompToSimulation(act.ObjID)
                    Else
                        Me.FrmStSim1.RemoveCompFromSimulation(act.ObjID)
                    End If

                Case UndoRedoActionType.ObjectAdded

                    Dim gobj1 = DirectCast(act.NewValue, GraphicObject)

                    If undo Then
                        DeleteObject(gobj1.Tag, False)
                    Else
                        FormSurface.AddObjectToSurface(gobj1.ObjectType, gobj1.X, gobj1.Y, gobj1.Tag, gobj1.Name)
                    End If

                Case UndoRedoActionType.ObjectRemoved

                    Dim gobj1 = DirectCast(act.NewValue, GraphicObject)

                    If undo Then
                        Collections.FlowsheetObjectCollection(FormSurface.AddObjectToSurface(gobj1.ObjectType, gobj1.X, gobj1.Y, gobj1.Tag, gobj1.Name)).LoadData(act.OldValue)
                        FormFlowsheet.SearchSurfaceObjectsByName(gobj1.Name, Me.FormSurface.FlowsheetSurface).LoadData(gobj1.SaveData)
                        If gobj1.ObjectType = ObjectType.MaterialStream Then
                            For Each phase As BaseClasses.Phase In DirectCast(Collections.FlowsheetObjectCollection(gobj1.Name), Streams.MaterialStream).Phases.Values
                                For Each c As ConstantProperties In Options.SelectedComponents.Values
                                    phase.Compounds(c.Name).ConstantProperties = c
                                Next
                            Next
                        End If
                    Else
                        DeleteObject(gobj1.Tag, False)
                    End If

                Case UndoRedoActionType.FlowsheetObjectConnected

                    Dim gobj1 = FormFlowsheet.SearchSurfaceObjectsByName(act.ObjID, FormSurface.FlowsheetSurface)
                    Dim gobj2 = FormFlowsheet.SearchSurfaceObjectsByName(act.ObjID2, FormSurface.FlowsheetSurface)

                    If undo Then
                        DisconnectObject(gobj1, gobj2)
                    Else
                        ConnectObject(gobj1, gobj2, act.OldValue, act.NewValue)
                    End If

                Case UndoRedoActionType.FlowsheetObjectDisconnected

                    Dim gobj1 = FormFlowsheet.SearchSurfaceObjectsByName(act.ObjID, FormSurface.FlowsheetSurface)
                    Dim gobj2 = FormFlowsheet.SearchSurfaceObjectsByName(act.ObjID2, FormSurface.FlowsheetSurface)

                    If undo Then
                        ConnectObject(gobj1, gobj2, act.OldValue, act.NewValue)
                    Else
                        DisconnectObject(gobj1, gobj2)
                    End If

                Case UndoRedoActionType.PropertyPackageAdded

                    If undo Then
                        Me.Options.PropertyPackages.Remove(act.ObjID)
                    Else
                        Dim pp As Thermodynamics.PropertyPackages.PropertyPackage = DirectCast(act.NewValue, Thermodynamics.PropertyPackages.PropertyPackage)
                        Me.Options.PropertyPackages.Add(pp.UniqueID, pp)
                    End If

                Case UndoRedoActionType.PropertyPackageRemoved

                    If undo Then
                        Dim pp As Thermodynamics.PropertyPackages.PropertyPackage = DirectCast(act.NewValue, Thermodynamics.PropertyPackages.PropertyPackage)
                        Me.Options.PropertyPackages.Add(pp.UniqueID, pp)
                    Else
                        Me.Options.PropertyPackages.Remove(act.ObjID)
                    End If

                Case UndoRedoActionType.PropertyPackagePropertyChanged

                    Dim pp As Thermodynamics.PropertyPackages.PropertyPackage = DirectCast(act.Tag, Thermodynamics.PropertyPackages.PropertyPackage)

                    If act.PropertyName = "PR_IP" Then
                        Dim prip As PengRobinson = pp.GetType.GetField("m_pr").GetValue(pp)
                        prip.InteractionParameters(act.ObjID)(act.ObjID2).kij = pval
                    ElseIf act.PropertyName = "PRSV2_KIJ" Then
                        Dim prip As PRSV2 = pp.GetType.GetField("m_pr").GetValue(pp)
                        prip.InteractionParameters(act.ObjID)(act.ObjID2).kij = pval
                    ElseIf act.PropertyName = "PRSV2_KJI Then" Then
                        Dim prip As PRSV2 = pp.GetType.GetField("m_pr").GetValue(pp)
                        prip.InteractionParameters(act.ObjID)(act.ObjID2).kji = pval
                    ElseIf act.PropertyName = "PRSV2VL_KIJ" Then
                        Dim prip As PRSV2VL = pp.GetType.GetField("m_pr").GetValue(pp)
                        prip.InteractionParameters(act.ObjID)(act.ObjID2).kij = pval
                    ElseIf act.PropertyName = "PRSV2VL_KJI Then" Then
                        Dim prip As PRSV2VL = pp.GetType.GetField("m_pr").GetValue(pp)
                        prip.InteractionParameters(act.ObjID)(act.ObjID2).kji = pval
                    ElseIf act.PropertyName = "LK_IP" Then
                        Dim prip As LeeKeslerPlocker = pp.GetType.GetField("m_lk").GetValue(pp)
                        prip.InteractionParameters(act.ObjID)(act.ObjID2).kij = pval
                    ElseIf act.PropertyName.Contains("NRTL") Then
                        Dim nrtlip As NRTL = pp.GetType.GetProperty("m_uni").GetValue(pp, Nothing)
                        Select Case act.PropertyName
                            Case "NRTL_A12"
                                nrtlip.InteractionParameters(act.ObjID)(act.ObjID2).A12 = pval
                            Case "NRTL_A21"
                                nrtlip.InteractionParameters(act.ObjID)(act.ObjID2).A21 = pval
                            Case "NRTL_B12"
                                nrtlip.InteractionParameters(act.ObjID)(act.ObjID2).B12 = pval
                            Case "NRTL_B21"
                                nrtlip.InteractionParameters(act.ObjID)(act.ObjID2).B21 = pval
                            Case "NRTL_C12"
                                nrtlip.InteractionParameters(act.ObjID)(act.ObjID2).C12 = pval
                            Case "NRTL_C21"
                                nrtlip.InteractionParameters(act.ObjID)(act.ObjID2).C21 = pval
                            Case "NRTL_alpha12"
                                nrtlip.InteractionParameters(act.ObjID)(act.ObjID2).alpha12 = pval
                        End Select
                    ElseIf act.PropertyName.Contains("UNIQUAC") Then
                        Dim uniquacip As UNIQUAC = pp.GetType.GetProperty("m_uni").GetValue(pp, Nothing)
                        Select Case act.PropertyName
                            Case "UNIQUAC_A12"
                                uniquacip.InteractionParameters(act.ObjID)(act.ObjID2).A12 = pval
                            Case "UNIQUAC_A21"
                                uniquacip.InteractionParameters(act.ObjID)(act.ObjID2).A21 = pval
                            Case "UNIQUAC_B12"
                                uniquacip.InteractionParameters(act.ObjID)(act.ObjID2).B12 = pval
                            Case "UNIQUAC_B21"
                                uniquacip.InteractionParameters(act.ObjID)(act.ObjID2).B21 = pval
                            Case "UNIQUAC_C12"
                                uniquacip.InteractionParameters(act.ObjID)(act.ObjID2).C12 = pval
                            Case "UNIQUAC_C21"
                                uniquacip.InteractionParameters(act.ObjID)(act.ObjID2).C21 = pval
                        End Select
                    End If

                Case UndoRedoActionType.SystemOfUnitsAdded

                    Dim su = DirectCast(act.NewValue, SystemsOfUnits.Units)

                    If undo Then
                        Me.FrmStSim1.ComboBox2.SelectedIndex = 0
                        Me.Options.AvailableUnitSystems.Remove(su.Name)
                    Else
                        Me.Options.AvailableUnitSystems.Add(su.Name, su)
                    End If

                Case UndoRedoActionType.SystemOfUnitsRemoved

                    Dim su = DirectCast(act.NewValue, SystemsOfUnits.Units)

                    If undo Then
                        Me.Options.AvailableUnitSystems.Add(su.Name, su)
                    Else
                        Me.FrmStSim1.ComboBox2.SelectedIndex = 0
                        Me.Options.AvailableUnitSystems.Remove(su.Name)
                    End If

                Case UndoRedoActionType.SystemOfUnitsChanged

                    Dim sobj = FormMain.AvailableUnitSystems(act.ObjID)

                    'Property not listed, set using Reflection
                    Dim method As FieldInfo = sobj.GetType().GetField(act.ObjID2)
                    method.SetValue(sobj, pval)

                    FrmStSim1.ComboBox2_SelectedIndexChanged(Me, New EventArgs)

                Case UndoRedoActionType.CutObjects

                    Dim xmldata As String, objlist As List(Of GraphicObject)
                    If undo Then
                        xmldata = act.NewValue
                        Clipboard.SetText(xmldata)
                        PasteObjects(False)
                    Else
                        objlist = act.OldValue
                        For Each obj In objlist
                            DeleteSelectedObject(Me, New EventArgs, obj, False)
                        Next
                    End If

                Case UndoRedoActionType.PasteObjects

                    Dim xmldata As String, objlist As List(Of GraphicObject)
                    If undo Then
                        objlist = act.NewValue
                        For Each obj In objlist
                            DeleteSelectedObject(Me, New EventArgs, obj, False)
                        Next
                    Else
                        xmldata = act.OldValue
                        Clipboard.SetText(xmldata)
                        PasteObjects(False)
                    End If

                Case UndoRedoActionType.SpreadsheetCellChanged

                    Dim cell = FormSpreadsheet.GetCellValue(act.ObjID)
                    If undo Then
                        cell.Data = act.OldValue(0)
                        cell.Tag = act.OldValue(1)
                    Else
                        cell.Data = act.NewValue(0)
                        cell.Tag = act.NewValue(1)
                    End If
                    FormSpreadsheet.Spreadsheet.Worksheets(0).Recalculate()

            End Select

            Me.FormSurface.UpdateSelectedObject()

        Catch ex As Exception

            WriteToLog(ex.ToString(), Color.Red, SharedClasses.DWSIM.Flowsheet.MessageType.GeneralError)

        End Try

    End Sub

    Sub AddUndoRedoAction(act As Interfaces.IUndoRedoAction) Implements Interfaces.IFlowsheet.AddUndoRedoAction

        If Me.MasterFlowsheet Is Nothing Then

            act.ID = Guid.NewGuid().ToString

            UndoStack.Push(act)

            RedoStack.Clear()

            tsbUndo.Enabled = True
            tsmiUndo.Enabled = True
            tsbRedo.Enabled = False
            tsmiRedo.Enabled = False
            tsbRedo.Text = DWSIM.App.GetLocalString("Redo")
            tsmiRedo.Text = DWSIM.App.GetLocalString("Redo")

            PopulateUndoRedoItems()

        End If

    End Sub

    Public Sub tsbUndo_Click(sender As Object, e As EventArgs)

        UndoActions(tsbUndo.DropDownItems(0), e)

    End Sub

    Public Sub tsbRedo_Click(sender As Object, e As EventArgs)

        RedoActions(tsbRedo.DropDownItems(0), e)

    End Sub

    Public Sub ProcessUndo()

        If UndoStack.Count > 0 Then
            Dim act = UndoStack.Pop()
            My.Application.PushUndoRedoAction = False
            ProcessAction(act, True)
            My.Application.PushUndoRedoAction = True
            RedoStack.Push(act)
            tsbRedo.Enabled = True
            tsmiRedo.Enabled = True
        End If

        If UndoStack.Count = 0 Then
            tsbUndo.Enabled = False
            tsmiUndo.Enabled = False
            tsbUndo.Text = DWSIM.App.GetLocalString("Undo")
            tsmiUndo.Text = DWSIM.App.GetLocalString("Undo")
        End If

    End Sub

    Public Sub ProcessRedo()

        If RedoStack.Count > 0 Then
            Dim act = RedoStack.Pop()
            My.Application.PushUndoRedoAction = False
            ProcessAction(act, False)
            My.Application.PushUndoRedoAction = True
            UndoStack.Push(act)
            tsbUndo.Enabled = True
            tsmiUndo.Enabled = True
        End If

        If RedoStack.Count = 0 Then
            tsbRedo.Enabled = False
            tsmiRedo.Enabled = False
            tsbRedo.Text = DWSIM.App.GetLocalString("Redo")
            tsmiRedo.Text = DWSIM.App.GetLocalString("Redo")
        End If

    End Sub

    Private Sub PopulateUndoRedoItems()

        Dim count As Integer

        tsbUndo.DropDownItems.Clear()
        count = 0
        For Each act In UndoStack
            If count = 0 Then
                tsmiUndo.Text = DWSIM.App.GetLocalString("Undo") & " " & act.Name
                tsbUndo.Text = DWSIM.App.GetLocalString("Undo") & " " & act.Name
            End If
            Dim tsmi As New ToolStripMenuItem(act.Name, My.Resources.undo_161, AddressOf UndoActions) With {.Tag = act.ID}
            AddHandler tsmi.MouseEnter, AddressOf tsbUndo_MouseEnter
            tsbUndo.DropDownItems.Add(tsmi)
            count += 1
            If count > 15 Then Exit For
        Next

        tsbRedo.DropDownItems.Clear()
        count = 0
        For Each act In RedoStack
            If count = 0 Then
                tsmiRedo.Text = DWSIM.App.GetLocalString("Redo") & " " & act.Name
                tsbRedo.Text = DWSIM.App.GetLocalString("Redo") & " " & act.Name
            End If
            Dim tsmi As New ToolStripMenuItem(act.Name, My.Resources.redo_16, AddressOf RedoActions) With {.Tag = act.ID}
            AddHandler tsmi.MouseEnter, AddressOf tsbRedo_MouseEnter
            tsbRedo.DropDownItems.Add(tsmi)
            count += 1
            If count > 15 Then Exit For
        Next

    End Sub

    Sub UndoActions(sender As Object, e As EventArgs)

        Dim actID = DirectCast(sender, ToolStripMenuItem).Tag
        Dim act As UndoRedoAction
        Do
            If UndoStack.Count = 0 Then Exit Do
            act = UndoStack.Peek
            ProcessUndo()
        Loop Until actID = act.ID

        PopulateUndoRedoItems()

        If My.Settings.UndoRedo_RecalculateFlowsheet Then FlowsheetSolver.FlowsheetSolver.SolveFlowsheet(Me, My.Settings.SolverMode)

    End Sub

    Sub RedoActions(sender As Object, e As EventArgs)

        Dim actID = DirectCast(sender, ToolStripMenuItem).Tag
        Dim act As UndoRedoAction
        Do
            If RedoStack.Count = 0 Then Exit Do
            act = RedoStack.Peek
            ProcessRedo()
        Loop Until actID = act.ID

        PopulateUndoRedoItems()

        If My.Settings.UndoRedo_RecalculateFlowsheet Then FlowsheetSolver.FlowsheetSolver.SolveFlowsheet(Me, My.Settings.SolverMode)

    End Sub

    Private Sub tsbUndo_MouseEnter(sender As Object, e As EventArgs)

        If TypeOf sender Is ToolStripMenuItem Then
            Dim hovereditem As ToolStripMenuItem = DirectCast(sender, ToolStripMenuItem)
            For Each tsmi As ToolStripMenuItem In tsbUndo.DropDownItems
                tsmi.Checked = False
            Next
            For Each tsmi As ToolStripMenuItem In tsbUndo.DropDownItems
                tsmi.Checked = True
                If tsmi Is hovereditem Then Exit For
            Next
        End If

    End Sub

    Private Sub tsbRedo_MouseEnter(sender As Object, e As EventArgs)

        If TypeOf sender Is ToolStripMenuItem Then
            Dim hovereditem As ToolStripMenuItem = DirectCast(sender, ToolStripMenuItem)
            For Each tsmi As ToolStripMenuItem In tsbRedo.DropDownItems
                tsmi.Checked = False
            Next
            For Each tsmi As ToolStripMenuItem In tsbRedo.DropDownItems
                tsmi.Checked = True
                If tsmi Is hovereditem Then Exit For
            Next
        End If

    End Sub

#End Region

#Region "    IFlowsheet Implementation"

    Private Delegate Sub NewMessageSentEventHandler(ByVal message As String)
    Private Event NewMessageSent As NewMessageSentEventHandler
    Public Event StatusChanged()

    Public Function GetFlowsheetSurfaceWidth() As Integer Implements IFlowsheet.GetFlowsheetSurfaceWidth
        Return FormSurface.SplitContainer1.Panel1.Width
    End Function


    Public Function GetFlowsheetSurfaceHeight() As Integer Implements IFlowsheet.GetFlowsheetSurfaceHeight
        Return FormSurface.SplitContainer1.Panel1.Height
    End Function

    Public Function ChangeCalculationOrder(objects As List(Of String)) As List(Of String) Implements IFlowsheet.ChangeCalculationOrder

        Dim frm As New SharedClasses.FormCustomCalcOrder
        frm.Flowsheet = Me
        frm.ItemList = objects
        frm.ShowDialog(Me)

        Return frm.NewItemList

    End Function

    Public Property AvailableFlashAlgorithms As Dictionary(Of String, IFlashAlgorithm) Implements IFlowsheet.AvailableFlashAlgorithms
        Get
            Throw New NotImplementedException()
        End Get
        Set(value As Dictionary(Of String, IFlashAlgorithm))
            Throw New NotImplementedException()
        End Set
    End Property

    Public Property AvailablePropertyPackages As Dictionary(Of String, IPropertyPackage) Implements IFlowsheet.AvailablePropertyPackages
        Get
            Throw New NotImplementedException()
        End Get
        Set(value As Dictionary(Of String, IPropertyPackage))
            Throw New NotImplementedException()
        End Set
    End Property

    Public Property AvailableSystemsOfUnits As List(Of IUnitsOfMeasure) Implements IFlowsheet.AvailableSystemsOfUnits
        Get
            Return FormMain.AvailableUnitSystems.Values.Select(Function(x) DirectCast(x, IUnitsOfMeasure)).ToList()
        End Get
        Set(value As List(Of IUnitsOfMeasure))
            Throw New NotImplementedException
        End Set
    End Property

    Public Property Scripts As Dictionary(Of String, IScript) Implements IFlowsheet.Scripts
        Get
            Return ScriptCollection
        End Get
        Set(value As Dictionary(Of String, IScript))
            ScriptCollection = value
        End Set
    End Property

    Public Function GetSpreadsheetData(range As String) As List(Of String()) Implements IFlowsheet.GetSpreadsheetData

        Dim firstcolumn, firstrow, lastcolumn, lastrow As Integer
        Dim firstcell, lastcell As String

        firstcell = range.Split(":")(0)
        lastcell = range.Split(":")(1)

        firstrow = FormSpreadsheet.GetCellValue(firstcell).Row
        firstcolumn = FormSpreadsheet.GetCellValue(firstcell).Column

        lastrow = FormSpreadsheet.GetCellValue(lastcell).Row
        lastcolumn = FormSpreadsheet.GetCellValue(lastcell).Column

        Dim data As New List(Of String())

        Dim i, j As Integer

        Dim grid = FormSpreadsheet.Spreadsheet.Worksheets(0)

        For i = firstrow To lastrow
            Dim sublist = New List(Of String)
            For j = firstcolumn To lastcolumn
                Dim val = grid.Cells(i, j).Data
                If val Is Nothing Then
                    sublist.Add("")
                Else
                    sublist.Add(val.ToString())
                End If
            Next
            data.Add(sublist.ToArray)
        Next

        Return data

    End Function

    Public Function GetApplicationObject() As Object Implements IFlowsheet.GetApplicationObject
        Return My.Application
    End Function

    Public Property GraphicObjects As Dictionary(Of String, Interfaces.IGraphicObject) Implements Interfaces.IFlowsheet.GraphicObjects, IFlowsheetBag.GraphicObjects
        Get
            Return Collections.GraphicObjectCollection.ToDictionary(Of String, IGraphicObject)(Function(k) k.Key, Function(k) k.Value)
        End Get
        Set(value As Dictionary(Of String, Interfaces.IGraphicObject))

        End Set
    End Property

    Public Property SimulationObjects As Dictionary(Of String, Interfaces.ISimulationObject) Implements Interfaces.IFlowsheet.SimulationObjects, IFlowsheetBag.SimulationObjects
        Get
            Return Collections.FlowsheetObjectCollection.ToDictionary(Of String, ISimulationObject)(Function(k) k.Key, Function(k) k.Value)
        End Get
        Set(value As Dictionary(Of String, Interfaces.ISimulationObject))

        End Set
    End Property

    Public Property Reactions As Dictionary(Of String, Interfaces.IReaction) Implements Interfaces.IFlowsheet.Reactions, IFlowsheetBag.Reactions
        Get
            Return Options.Reactions
        End Get
        Set(value As Dictionary(Of String, Interfaces.IReaction))

        End Set
    End Property

    Public Property ReactionSets As Dictionary(Of String, Interfaces.IReactionSet) Implements Interfaces.IFlowsheet.ReactionSets, IFlowsheetBag.ReactionSets
        Get
            Return Options.ReactionSets
        End Get
        Set(value As Dictionary(Of String, Interfaces.IReactionSet))

        End Set
    End Property

    Public Sub ShowMessage(text As String, mtype As Interfaces.IFlowsheet.MessageType, Optional ByVal exceptionID As String = "") Implements Interfaces.IFlowsheet.ShowMessage, IFlowsheetGUI.ShowMessage
        Select Case mtype
            Case Interfaces.IFlowsheet.MessageType.Information
                WriteToLog(text, Color.Blue, SharedClasses.DWSIM.Flowsheet.MessageType.Information)
            Case Interfaces.IFlowsheet.MessageType.GeneralError
                WriteToLog(text, Color.Red, SharedClasses.DWSIM.Flowsheet.MessageType.GeneralError, exceptionID)
            Case Interfaces.IFlowsheet.MessageType.Warning
                WriteToLog(text, Color.OrangeRed, SharedClasses.DWSIM.Flowsheet.MessageType.Warning)
            Case Interfaces.IFlowsheet.MessageType.Tip
                WriteToLog(text, Color.Blue, SharedClasses.DWSIM.Flowsheet.MessageType.Tip)
            Case Interfaces.IFlowsheet.MessageType.Other
                WriteToLog(text, Color.Black, SharedClasses.DWSIM.Flowsheet.MessageType.Information)
        End Select
    End Sub

    Public Sub CheckStatus() Implements Interfaces.IFlowsheet.CheckStatus, IFlowsheetGUI.CheckStatus
        Application.DoEvents()
        FlowsheetSolver.FlowsheetSolver.CheckCalculatorStatus()
    End Sub

    Public Function GetTranslatedString(text As String, locale As String) As String Implements Interfaces.IFlowsheet.GetTranslatedString, IFlowsheetGUI.GetTranslatedString

        Return GetTranslatedString1(text)

    End Function

    Public Sub ShowDebugInfo(text As String, level As Integer) Implements Interfaces.IFlowsheet.ShowDebugInfo, IFlowsheetGUI.ShowDebugInfo

        DWSIM.App.WriteToConsole(text, level)

    End Sub

    Public ReadOnly Property FlowsheetOptions As Interfaces.IFlowsheetOptions Implements Interfaces.IFlowsheet.FlowsheetOptions
        Get
            Return Options
        End Get
    End Property

    Public Function GetTranslatedString1(text As String) As String Implements IFlowsheet.GetTranslatedString, IFlowsheetGUI.GetTranslatedString
        Dim returntext As String = text
        returntext = DWSIM.App.GetLocalString(text)
        If returntext <> text Then Return returntext
        returntext = DWSIM.App.GetPropertyName(text)
        If returntext <> text Then Return returntext
        returntext = DWSIM.App.GetLocalTipString(text)
        Return returntext
    End Function

    Public Property PropertyPackages As Dictionary(Of String, IPropertyPackage) Implements IFlowsheet.PropertyPackages, IFlowsheetBag.PropertyPackages
        Get
            Return Options.PropertyPackages
        End Get
        Set(value As Dictionary(Of String, IPropertyPackage))

        End Set
    End Property

    Public Function GetFlowsheetSimulationObject1(tag As String) As ISimulationObject Implements IFlowsheet.GetFlowsheetSimulationObject
        Return Me.GetFlowsheetSimulationObject(tag)
    End Function

    Public Property SelectedCompounds As Dictionary(Of String, ICompoundConstantProperties) Implements IFlowsheet.SelectedCompounds, IFlowsheetBag.Compounds
        Get
            Select Case Options.CompoundOrderingMode
                Case CompoundOrdering.CAS_ASC
                    Return Options.SelectedComponents.OrderBy(Function(c) c.Value.CAS_Number).ToDictionary(Of String, ICompoundConstantProperties)(Function(k) k.Key, Function(k) k.Value)
                Case CompoundOrdering.CAS_DESC
                    Return Options.SelectedComponents.OrderByDescending(Function(c) c.Value.CAS_Number).ToDictionary(Of String, ICompoundConstantProperties)(Function(k) k.Key, Function(k) k.Value)
                Case CompoundOrdering.MW_ASC
                    Return Options.SelectedComponents.OrderBy(Function(c) c.Value.Molar_Weight).ToDictionary(Of String, ICompoundConstantProperties)(Function(k) k.Key, Function(k) k.Value)
                Case CompoundOrdering.MW_DESC
                    Return Options.SelectedComponents.OrderByDescending(Function(c) c.Value.Molar_Weight).ToDictionary(Of String, ICompoundConstantProperties)(Function(k) k.Key, Function(k) k.Value)
                Case CompoundOrdering.Name_ASC
                    Return Options.SelectedComponents.OrderBy(Function(c) c.Value.Name).ToDictionary(Of String, ICompoundConstantProperties)(Function(k) k.Key, Function(k) k.Value)
                Case CompoundOrdering.Name_DESC
                    Return Options.SelectedComponents.OrderByDescending(Function(c) c.Value.Name).ToDictionary(Of String, ICompoundConstantProperties)(Function(k) k.Key, Function(k) k.Value)
                Case CompoundOrdering.NBP_ASC
                    Return Options.SelectedComponents.OrderBy(Function(c) c.Value.NBP.GetValueOrDefault).ToDictionary(Of String, ICompoundConstantProperties)(Function(k) k.Key, Function(k) k.Value)
                Case CompoundOrdering.NBP_DESC
                    Return Options.SelectedComponents.OrderByDescending(Function(c) c.Value.NBP.GetValueOrDefault).ToDictionary(Of String, ICompoundConstantProperties)(Function(k) k.Key, Function(k) k.Value)
                Case CompoundOrdering.TAG_ASC
                    Return Options.SelectedComponents.OrderBy(Function(c) c.Value.Tag).ToDictionary(Of String, ICompoundConstantProperties)(Function(k) k.Key, Function(k) k.Value)
                Case CompoundOrdering.TAG_DESC
                    Return Options.SelectedComponents.OrderByDescending(Function(c) c.Value.Tag).ToDictionary(Of String, ICompoundConstantProperties)(Function(k) k.Key, Function(k) k.Value)
                Case Else
                    Return Options.SelectedComponents
            End Select
        End Get
        Set(value As Dictionary(Of String, ICompoundConstantProperties))
            Options.SelectedComponents = value
        End Set
    End Property

    Public Function GetSelectedFlowsheetSimulationObject(tag As String) As ISimulationObject Implements IFlowsheet.GetSelectedFlowsheetSimulationObject
        Dim selobj = Me.FormSurface.FlowsheetSurface.SelectedObject
        If selobj Is Nothing Then
            Return Nothing
        Else
            If tag Is Nothing OrElse tag = "" Then
                If SimulationObjects.ContainsKey(selobj.Name) Then Return SimulationObjects(selobj.Name) Else Return Nothing
            Else
                Return SimulationObjects.Values.Where(Function(x) x.GraphicObject.Tag = tag).FirstOrDefault
            End If
        End If
    End Function

    Public Sub DisplayForm(form As Object) Implements IFlowsheet.DisplayForm
        Dim cnt = TryCast(form, DockContent)
        If Not cnt Is Nothing Then
            If cnt.ShowHint = DockState.DockLeft Or cnt.ShowHint = DockState.DockLeftAutoHide Then
                dckPanel.DockLeftPortion = cnt.Width
            ElseIf cnt.ShowHint = DockState.DockRight Or cnt.ShowHint = DockState.DockRightAutoHide Then
                dckPanel.DockRightPortion = cnt.Width
            ElseIf cnt.ShowHint = DockState.Float Then
                dckPanel.DefaultFloatWindowSize = New Size(cnt.Width, cnt.Height)
            End If
            cnt.Show(Me.dckPanel)
            Try
                Dim editors = dckPanel.Contents.Where(Function(d) TypeOf d Is DockContent).Where(Function(d2) DirectCast(d2, DockContent).Tag = "ObjectEditor").ToList
                If editors.Count > 4 Then
                    DirectCast(editors(0), DockContent).Close()
                End If
            Catch ex As Exception
            End Try
        Else
            DirectCast(form, Form).Show(Me)
        End If
    End Sub

    Public Sub ConnectObjects(gobjfrom As IGraphicObject, gobjto As IGraphicObject, fromidx As Integer, toidx As Integer) Implements IFlowsheet.ConnectObjects
        ConnectObject(gobjfrom, gobjto, fromidx, toidx)
        UpdateOpenEditForms()
    End Sub

    Public Sub DisconnectObjects(gobjfrom As IGraphicObject, gobjto As IGraphicObject) Implements IFlowsheet.DisconnectObjects
        DisconnectObject(gobjfrom, gobjto, False)
        UpdateOpenEditForms()
    End Sub

    Public Function GetFlowsheetBag() As IFlowsheetBag Implements IFlowsheet.GetFlowsheetBag

        Dim fbag As New SharedClasses.Flowsheet.FlowsheetBag(SimulationObjects, GraphicObjects, SelectedCompounds, PropertyPackages, Reactions, ReactionSets)

        Return fbag

    End Function

    Public Property FilePath As String Implements IFlowsheet.FilePath
        Get
            Return Me.Options.FilePath
        End Get
        Set(value As String)
            Me.Options.FilePath = value
        End Set
    End Property

    Public Sub SaveToXML(file As String) Implements IFlowsheetBag.SaveToXML
        FormMain.SaveXML(file, Me)
    End Sub

    Public Sub UpdateProcessData(xdoc As XDocument) Implements IFlowsheetBag.UpdateProcessData

    End Sub

    Public Function AddObject(t As ObjectType, xcoord As Integer, ycoord As Integer, tag As String) As Interfaces.ISimulationObject Implements IFlowsheet.AddObject
        Dim id = Me.FormSurface.AddObjectToSurface(t, xcoord, ycoord, False, tag)
        FormSurface.FControl?.Refresh()
        FormSurface.FControl?.Refresh()
        Return Me.SimulationObjects(id)
    End Function

    Public Sub RequestCalculation(Optional sender As ISimulationObject = Nothing, Optional changecalcorder As Boolean = False) Implements IFlowsheet.RequestCalculation

        If Not sender Is Nothing Then
            FlowsheetSolver.FlowsheetSolver.CalculateObject(Me, sender.Name)
        Else
            FlowsheetSolver.FlowsheetSolver.SolveFlowsheet(Me, Settings.SolverMode)
        End If
        FormSurface.Invalidate()

    End Sub

    Public Function GetUtility(uttype As Enums.FlowsheetUtility) As IAttachedUtility Implements IFlowsheet.GetUtility
        Select Case uttype
            Case FlowsheetUtility.NaturalGasHydrates
                Return New AttachedUtilityClass() With {.InternalUtility = New FormHYD}
            Case FlowsheetUtility.PetroleumProperties
                Return New AttachedUtilityClass() With {.InternalUtility = New FrmColdProperties}
            Case FlowsheetUtility.PhaseEnvelope
                Return New AttachedUtilityClass() With {.InternalUtility = New FormPhEnv}
            Case FlowsheetUtility.PhaseEnvelopeBinary
                Return New AttachedUtilityClass() With {.InternalUtility = New FormBinEnv}
            Case FlowsheetUtility.PhaseEnvelopeTernary
                Return New AttachedUtilityClass() With {.InternalUtility = New FormLLEDiagram}
            Case FlowsheetUtility.PSVSizing
                Return New AttachedUtilityClass() With {.InternalUtility = New FrmPsvSize}
            Case FlowsheetUtility.PureCompoundProperties
                Return New AttachedUtilityClass() With {.InternalUtility = New FormPureComp}
            Case FlowsheetUtility.SeparatorSizing
                Return New AttachedUtilityClass() With {.InternalUtility = New FrmDAVP}
            Case FlowsheetUtility.TrueCriticalPoint
                Return New AttachedUtilityClass() With {.InternalUtility = New FrmCritpt}
        End Select
        Throw New ArgumentException
    End Function

    Public Sub UpdateOpenEditForms() Implements IFlowsheet.UpdateOpenEditForms

        Me.UIThreadInvoke(Sub()
                              For Each obj In SimulationObjects.Values
                                  obj.UpdateEditForm()
                                  EditorTooltips.Update(obj, Me)
                                  obj.AttachedUtilities.ForEach(Sub(x) x.Populate())
                              Next
                          End Sub)

    End Sub

    Public Function GetSurface() As Object Implements IFlowsheetBag.GetSurface, IFlowsheet.GetSurface
        Return Me.FormSurface
    End Function

    Public Function GetNewInstance() As IFlowsheet Implements IFlowsheet.GetNewInstance
        Dim fs As New FormFlowsheet()
        fs.Options.VisibleProperties = Me.Options.VisibleProperties
        fs.FormSpreadsheet.Initialize()
        Return fs
    End Function

    Public Sub AddGraphicObject(obj As IGraphicObject) Implements IFlowsheet.AddGraphicObject
        Me.FormSurface.FlowsheetSurface.DrawingObjects.Add(obj)
        Me.Collections.GraphicObjectCollection.Add(obj.Name, obj)
    End Sub

    Public Sub AddSimulationObject(obj As ISimulationObject) Implements IFlowsheet.AddSimulationObject
        Me.Collections.FlowsheetObjectCollection.Add(obj.Name, obj)
    End Sub

    Public Sub AddPropertyPackage(obj As IPropertyPackage) Implements IFlowsheet.AddPropertyPackage
        Me.Options.PropertyPackages.Add(obj.UniqueID, obj)
    End Sub

    Public Sub UpdateInterface() Implements IFlowsheetGUI.UpdateInterface, IFlowsheet.UpdateInterface

        Me.UIThread(Sub()
                        Me.FormSurface.Refresh()
                    End Sub)
    End Sub

    Public ReadOnly Property UtilityPlugins As Dictionary(Of String, IUtilityPlugin) Implements IFlowsheet.UtilityPlugins
        Get
            Return My.Application.UtilityPlugins
        End Get
    End Property

    Public Function GetDockPanel() As Object Implements IFlowsheet.GetDockPanel
        Return dckPanel
    End Function


    Private Sub AdicionarUtilitárioToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles TSMIAddUtility.Click

        Dim f As New FormAddUtility() With {.Flowsheet = Me}

        f.ShowDialog(Me)

    End Sub

    Private Sub PropriedadesDasSubstanciasToolStripMenuItem_Click(sender As Object, e As EventArgs) Handles PropriedadesDasSubstanciasToolStripMenuItem.Click
        Dim frmpc As New FormPureComp With {.Flowsheet = Me}
        frmpc.ShowDialog(Me)
    End Sub

    Public Sub UpdateInformation() Implements IFlowsheetGUI.UpdateInformation, IFlowsheet.UpdateInformation

        Me.UIThread(Sub()

                        If Me.Visible And Me.MasterFlowsheet Is Nothing Then

                            Me.FormWatch.UpdateList()

                            If Not Me.FormSpreadsheet Is Nothing Then
                                Me.FormSpreadsheet.EvaluateAll()
                            End If

                            'Application.DoEvents()

                        End If

                    End Sub)
    End Sub

    Public Sub UpdateSpreadsheet(act As Action) Implements IFlowsheet.UpdateSpreadsheet

        Try
            Me.UIThread(Sub()
                            If FormSpreadsheet IsNot Nothing Then Me.FormSpreadsheet.EvaluateAll()
                        End Sub)
        Catch ex As Exception
            WriteToLog("Error updating spreadsheet: " & ex.Message.ToString, Color.Red, SharedClasses.DWSIM.Flowsheet.MessageType.GeneralError)
        End Try

    End Sub

    Public Sub WriteSpreadsheetVariables(act As Action) Implements IFlowsheet.WriteSpreadsheetVariables
        Me.UIThread(Sub()
                        If FormSpreadsheet IsNot Nothing Then
                            Me.FormSpreadsheet.Flowsheet = Me
                            Me.FormSpreadsheet.WriteAll()
                        End If
                    End Sub)
    End Sub

    Public Sub SetMessageListener(act As Action(Of String, Interfaces.IFlowsheet.MessageType)) Implements IFlowsheet.SetMessageListener
        listeningaction = act
    End Sub

    Public Property MobileCompatibilityMode As Boolean = False Implements IFlowsheet.MobileCompatibilityMode

    Public Property Message As String = "" Implements IFlowsheet.Message

    Public Property SimulationObjectsArray As ISimulationObject() Implements IFlowsheetBag.SimulationObjectsArray
        Get
            Return SimulationObjects.Values.ToArray
        End Get
        Set(value As ISimulationObject())

        End Set
    End Property

    Public Property GraphicObjectsArray As IGraphicObject() Implements IFlowsheetBag.GraphicObjectsArray
        Get
            Return GraphicObjects.Values.ToArray
        End Get
        Set(value As IGraphicObject())

        End Set
    End Property

    Public Property CompoundsArray As ICompoundConstantProperties() Implements IFlowsheetBag.CompoundsArray
        Get
            Return SelectedCompounds.Values.ToArray
        End Get
        Set(value As ICompoundConstantProperties())

        End Set
    End Property

    Public Property PropertyPackagesArray As IPropertyPackage() Implements IFlowsheetBag.PropertyPackagesArray
        Get
            Return PropertyPackages.Values.ToArray
        End Get
        Set(value As IPropertyPackage())

        End Set
    End Property

    Public Property ReactionsArray As IReaction() Implements IFlowsheetBag.ReactionsArray
        Get
            Return Reactions.Values.ToArray
        End Get
        Set(value As IReaction())

        End Set
    End Property

    Public Property ReactionSetsArray As IReactionSet() Implements IFlowsheetBag.ReactionSetsArray
        Get
            Return ReactionSets.Values.ToArray
        End Get
        Set(value As IReactionSet())

        End Set
    End Property

    Public Property Solved As Boolean = False Implements IFlowsheet.Solved

    Public Property ErrorMessage As String = "" Implements IFlowsheet.ErrorMessage

    Public Sub RunCodeOnUIThread(act As Action) Implements IFlowsheet.RunCodeOnUIThread
        Invoke(act)
    End Sub

    Public Property AvailableCompounds As Dictionary(Of String, ICompoundConstantProperties) Implements IFlowsheet.AvailableCompounds
        Get
            Return FormMain.AvailableComponents
        End Get
        Set(value As Dictionary(Of String, ICompoundConstantProperties))
            FormMain.AvailableComponents = value
        End Set
    End Property

    Public Property Charts As Dictionary(Of String, IChart) Implements IFlowsheet.Charts
        Get
            If ChartCollection Is Nothing Then ChartCollection = New Dictionary(Of String, IChart)
            Return ChartCollection
        End Get
        Set(value As Dictionary(Of String, IChart))
            ChartCollection = value
        End Set
    End Property

    Public Sub DeleteSelectedObject1(sender As Object, e As EventArgs, gobj As IGraphicObject, Optional confirmation As Boolean = True, Optional triggercalc As Boolean = False) Implements IFlowsheet.DeleteSelectedObject

    End Sub

    Public Sub Initialize() Implements IFlowsheet.Initialize
        Throw New NotImplementedException()
    End Sub

    Public Sub LoadFromXML(xdoc As XDocument) Implements IFlowsheet.LoadFromXML
        Throw New NotImplementedException()
    End Sub

    Public Sub Reset() Implements IFlowsheet.Reset
        Throw New NotImplementedException()
    End Sub

    Public Function SaveToXML1() As XDocument Implements IFlowsheet.SaveToXML
        Throw New NotImplementedException()
    End Function

#End Region

End Class
