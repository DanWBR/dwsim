'    Copyright 2008-2015 Daniel Wagner O. de Medeiros
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

Imports Microsoft.Msdn.Samples.GraphicObjects
Imports System.Collections.Generic
Imports System.ComponentModel
Imports PropertyGridEx
Imports WeifenLuo.WinFormsUI
Imports System.Drawing
Imports System.Linq
Imports System.IO
Imports DWSIM.DWSIM.Flowsheet.FlowsheetSolver
Imports DWSIM.DWSIM.SimulationObjects.PropertyPackages
Imports Microsoft.Win32
Imports DWSIM.DWSIM.SimulationObjects
Imports DWSIM.DWSIM.Thermodynamics.BaseClasses
Imports System.Runtime.Serialization.Formatters.Binary
Imports DWSIM.DWSIM.Flowsheet
Imports DWSIM.DWSIM.GraphicObjects
Imports DWSIM.DWSIM.Extras
Imports WeifenLuo.WinFormsUI.Docking
Imports System.Globalization
Imports Microsoft.Msdn.Samples
Imports System.Reflection

<System.Serializable()> Public Class FormFlowsheet

    Inherits Form

    'CAPE-OPEN PME/COSE Interfaces
    Implements CapeOpen.ICapeCOSEUtilities, CapeOpen.ICapeMaterialTemplateSystem, CapeOpen.ICapeDiagnostic,  _
                CapeOpen.ICapeFlowsheetMonitoring, CapeOpen.ICapeSimulationContext, CapeOpen.ICapeIdentification

#Region "    Variable Declarations "

    Public Property MasterFlowsheet As FormFlowsheet = Nothing
    Public Property MasterUnitOp As UnitOps.Flowsheet = Nothing
    Public Property RedirectMessages As Boolean = False

    Public FrmStSim1 As New FormSimulSettings
    Public FrmPCBulk As New FormPCBulk
    Public FrmReport As New FormReportConfig

    Public m_IsLoadedFromFile As Boolean = False
    Public m_overrideCloseQuestion As Boolean = False
    Public m_simultadjustsolverenabled As Boolean = True

    Public FormSurface As New frmSurface
    Public FormProps As New frmProps
    Public FormLog As New frmLog
    Public FormMatList As New frmMatList
    Public FormSpreadsheet As New SpreadsheetForm

    Public FormOutput As New frmOutput
    Public FormQueue As New frmQueue
    Public FormCOReports As New frmCOReports
    Public FormWatch As New frmWatch

    Public FormCritPt As New FrmCritpt
    Public FormStabAn As New FrmStabAn
    Public FormHid As New FormHYD
    Public FormPE As New FormPhEnv
    Public FormLLEDiag As New FormLLEDiagram
    Public FormBE As New FormBinEnv
    Public FormPSVS As New FrmPsvSize
    Public FormVS As New FrmDAVP
    Public FormColdP As New FrmColdProperties

    Public FormSensAnalysis0 As New FormSensAnalysis
    Public FormOptimization0 As New FormOptimization

    Public FormCL As FormCLM

    Public WithEvents Options As New DWSIM.Flowsheet.FlowsheetVariables

    Public Conversor As New DWSIM.SystemsOfUnits.Converter

    Public CalculationQueue As Generic.Queue(Of DWSIM.Extras.StatusChangeEventArgs)

    Public FlowsheetStates As Dictionary(Of Date, FlowsheetState)

    Public PreviousSolutions As Dictionary(Of String, FlowsheetSolution)

    Public ScriptCollection As Dictionary(Of String, Script)

    Public CheckedToolstripButton As ToolStripButton
    Public ClickedToolStripMenuItem As ToolStripMenuItem
    Public InsertingObjectToPFD As Boolean = False

    Public prevcolor1, prevcolor2 As Color

    Public Collections As New DWSIM.Flowsheet.ObjectCollection

    Public ID As String

    Private QuestionID As Integer = -1

    Private loaded As Boolean = False

    Public UndoStack As New Stack(Of UndoRedoAction)
    Public RedoStack As New Stack(Of UndoRedoAction)

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

    End Sub

    Private Sub FormChild_Activated(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Activated
        My.Application.ActiveSimulation = Me
    End Sub

    Private Sub FormChild_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load

        If DWSIM.App.IsRunningOnMono Then
            'Me.FlowLayoutPanel1.AutoSize = False
            'Me.FlowLayoutPanel1.Height = 50
            Me.MenuStrip1.Visible = False
            Me.CAPEOPENFlowsheetMonitoringObjectsMOsToolStripMenuItem.Visible = False
            Me.WindowState = FormWindowState.Maximized
        Else
            'FormObjList = New frmObjList
            Me.MenuStrip1.Visible = False
            Me.WindowState = FormWindowState.Normal
        End If

        showflowsheettoolstripmenuitem.Checked = My.Settings.ShowFlowsheetToolStrip
        showsimulationtoolstripmenuitem.Checked = My.Settings.ShowSimulationToolStrip
        showunitstoolstripmenuitem.Checked = My.Settings.ShowUnitsToolStrip

        Me.COObjTSMI.Checked = Me.Options.FlowsheetShowCOReportsWindow
        Me.consoletsmi.Checked = Me.Options.FlowsheetShowConsoleWindow
        Me.ExibirListaDeItensACalcularToolStripMenuItem.Checked = Me.Options.FlowsheetShowCalculationQueue
        Me.varpaneltsmi.Checked = Me.Options.FlowsheetShowWatchWindow

        Dim rand As New Random
        Dim str As String = rand.Next(10000000, 99999999)

        Me.Options.BackupFileName = str & ".dwbcs"

        Me.CalculationQueue = New Generic.Queue(Of DWSIM.Extras.StatusChangeEventArgs)

        Me.TSTBZoom.Text = Format(Me.FormSurface.FlowsheetDesignSurface.Zoom, "#%")

        If Me.Options.CalculatorActivated Then
            Me.tsbAtivar.Checked = True
            Me.tsbDesat.Checked = False
        Else
            Me.tsbAtivar.Checked = False
            Me.tsbDesat.Checked = True
        End If

        Me.ToolStripButton16.Checked = Me.Options.FlowsheetSnapToGrid
        Me.ToolStripButton17.Checked = Me.Options.FlowsheetQuickConnect

        If Me.ScriptCollection Is Nothing Then Me.ScriptCollection = New Dictionary(Of String, Script)

        If Not Me.m_IsLoadedFromFile Then

            If Not DWSIM.App.IsRunningOnMono Then
                Me.Options.SimAutor = My.User.Name
            Else
                Me.Options.SimAutor = "user"
            End If

            For Each pp As DWSIM.SimulationObjects.PropertyPackages.PropertyPackage In Me.Options.PropertyPackages.Values
                If pp.ConfigForm Is Nothing Then pp.ReconfigureConfigForm()
            Next

            Me.Options.NotSelectedComponents = New Dictionary(Of String, DWSIM.Thermodynamics.BaseClasses.ConstantProperties)

            Dim tmpc As DWSIM.Thermodynamics.BaseClasses.ConstantProperties
            For Each tmpc In FormMain.AvailableComponents.Values
                Dim newc As New DWSIM.Thermodynamics.BaseClasses.ConstantProperties
                newc = tmpc
                Me.Options.NotSelectedComponents.Add(tmpc.Name, newc)
            Next

            Dim Frm = ParentForm

            ' Set DockPanel properties
            dckPanel.ActiveAutoHideContent = Nothing
            dckPanel.Parent = Me

            FormLog.DockPanel = Nothing
            FormProps.DockPanel = Nothing
            FormMatList.DockPanel = Nothing
            FormSpreadsheet.DockPanel = Nothing
            FormWatch.DockPanel = Nothing
            FormSurface.DockPanel = Nothing

            FormSpreadsheet.Show(dckPanel)
            FormMatList.Show(FormSpreadsheet.Pane, FormSpreadsheet)
            FormSurface.Show(FormSpreadsheet.Pane, Nothing)
            FormLog.Show(FormSurface.Pane, DockAlignment.Bottom, 0.2)
            FormProps.Show(dckPanel)

            dckPanel.BringToFront()
            dckPanel.UpdateDockWindowZOrder(DockStyle.Fill, True)

            Me.Invalidate()
            Application.DoEvents()

            Me.FormSurface.FlowsheetDesignSurface.Zoom = 1
            Me.FormSurface.FlowsheetDesignSurface.VerticalScroll.Maximum = 7000
            Me.FormSurface.FlowsheetDesignSurface.HorizontalScroll.Maximum = 10000
            Try
                Me.FormSurface.FlowsheetDesignSurface.VerticalScroll.Value = 3500
                Me.FormSurface.FlowsheetDesignSurface.HorizontalScroll.Value = 5000
            Catch ex As Exception
            End Try

        Else

            If Me.Collections.AdjustCollection Is Nothing Then
                Me.Collections.AdjustCollection = New Dictionary(Of String, AdjustGraphic)
                Me.Collections.CLCS_AdjustCollection = New Dictionary(Of String, DWSIM.SimulationObjects.SpecialOps.Adjust)
                Me.Collections.SpecCollection = New Dictionary(Of String, SpecGraphic)
                Me.Collections.CLCS_SpecCollection = New Dictionary(Of String, DWSIM.SimulationObjects.SpecialOps.Spec)
                Me.Collections.RecycleCollection = New Dictionary(Of String, RecycleGraphic)
                Me.Collections.CLCS_RecycleCollection = New Dictionary(Of String, DWSIM.SimulationObjects.SpecialOps.Recycle)
            End If

        End If

        Me.UpdateFormText()

        Dim array1(FormMain.AvailableUnitSystems.Count - 1) As String
        FormMain.AvailableUnitSystems.Keys.CopyTo(array1, 0)
        Me.ToolStripComboBoxUnitSystem.Items.Clear()
        Me.ToolStripComboBoxUnitSystem.Items.AddRange(array1)

        If Me.Options.SelectedUnitSystem.Name <> "" Then
            Me.ToolStripComboBoxUnitSystem.SelectedItem = Me.Options.SelectedUnitSystem.Name
        Else
            Me.ToolStripComboBoxUnitSystem.SelectedIndex = 0
        End If

        Me.ToolStripComboBoxNumberFormatting.SelectedItem = Me.Options.NumberFormat
        Me.ToolStripComboBoxNumberFractionFormatting.SelectedItem = Me.Options.FractionNumberFormat

        'load plugins
        CreatePluginsList()

        loaded = True

    End Sub

    Public Sub FormChild_Shown(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Shown

        If Not Me.m_IsLoadedFromFile Then

            Me.Invalidate()
            Application.DoEvents()
            Application.DoEvents()

            If Not DWSIM.App.IsRunningOnMono Then
                Dim fw As New FormConfigWizard
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

        Else

            Dim array1(FormMain.AvailableUnitSystems.Count - 1) As String
            FormMain.AvailableUnitSystems.Keys.CopyTo(array1, 0)
            Me.ToolStripComboBoxUnitSystem.Items.Clear()
            Me.ToolStripComboBoxUnitSystem.Items.AddRange(array1)

            If Me.ToolStripComboBoxUnitSystem.Items.Contains(Me.Options.SelectedUnitSystem.Name) Then
                Me.ToolStripComboBoxUnitSystem.SelectedItem = Me.Options.SelectedUnitSystem.Name
            Else
                If Me.Options.SelectedUnitSystem.Name <> "" Then
                    QuestionID = 0
                    ShowQuestionPanel(MessageBoxIcon.Question, DWSIM.App.GetLocalString("ConfirmAddUnitSystemFromSimulation"), True, DWSIM.App.GetLocalString("Sim"), True, DWSIM.App.GetLocalString("No"))
                Else
                    Me.ToolStripComboBoxUnitSystem.SelectedIndex = 0
                    Me.ToolStripComboBoxUnitSystem.SelectedItem = Me.Options.SelectedUnitSystem.Name
                End If
            End If
            Me.ToolStripComboBoxNumberFormatting.SelectedItem = Me.Options.NumberFormat
            Me.ToolStripComboBoxNumberFractionFormatting.SelectedItem = Me.Options.FractionNumberFormat

        End If

        Me.FormLog.Grid1.Sort(Me.FormLog.Grid1.Columns(1), ListSortDirection.Descending)

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

        My.Application.ActiveSimulation = Me

        Me.ProcessScripts(Script.EventType.SimulationOpened, Script.ObjectType.Simulation)

        WriteToLog(DWSIM.App.GetLocalTipString("FLSH003"), Color.Black, MessageType.Tip)
        WriteToLog(DWSIM.App.GetLocalTipString("FLSH001"), Color.Black, MessageType.Tip)
        WriteToLog(DWSIM.App.GetLocalTipString("FLSH002"), Color.Black, MessageType.Tip)
        WriteToLog(DWSIM.App.GetLocalTipString("FLSH005"), Color.Black, MessageType.Tip)

        If My.Settings.ShowWhatsNew Then
            If Not DWSIM.App.IsRunningOnMono Then
                Dim fwn As New FormWhatsNew
                fwn.Show()
            End If
        End If

    End Sub

    Private Sub FormChild2_FormClosed(ByVal sender As Object, ByVal e As System.Windows.Forms.FormClosedEventArgs) Handles Me.FormClosed

        Me.ProcessScripts(Script.EventType.SimulationClosed, Script.ObjectType.Simulation)

        If My.Application.ActiveSimulation Is Me Then
            My.Application.ActiveSimulation = Nothing
        End If

        'dispose objects
        For Each obj As DWSIM.SimulationObjects.UnitOperations.BaseClass In Me.Collections.FlowsheetObjectCollection.Values
            If obj.disposedValue = False Then obj.Dispose()
        Next

        Dim path As String = My.Settings.BackupFolder + System.IO.Path.DirectorySeparatorChar + Me.Options.BackupFileName

        If My.Settings.BackupFiles.Contains(path) Then
            My.Settings.BackupFiles.Remove(path)
            If Not DWSIM.App.IsRunningOnMono Then My.Settings.Save()
            Try
                If File.Exists(path) Then File.Delete(path)
            Catch ex As Exception
            End Try
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

            Dim x = MessageBox.Show(DWSIM.App.GetLocalString("Desejasalvarasaltera"), DWSIM.App.GetLocalString("Fechando") & " " & Me.Options.SimNome & " (" & System.IO.Path.GetFileName(Me.Options.FilePath) & ") ...", MessageBoxButtons.YesNoCancel, MessageBoxIcon.Question)

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

    Sub UpdateFormText()
        If File.Exists(Me.Options.FilePath) Then
            Me.Text = IO.Path.GetFileNameWithoutExtension(Me.Options.FilePath) & " (" & Me.Options.FilePath & ")"
        Else
            Me.Text = Me.Options.SimNome
        End If
    End Sub

    Public Sub ProcessScripts(ByVal sourceevent As DWSIM.Extras.Script.EventType, ByVal sourceobj As DWSIM.Extras.Script.ObjectType, Optional ByVal sourceobjname As String = "")

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
                                            Me.WriteToLog("Running script '" & scr.Title & "' for event '" & scr.LinkedEventType.ToString & "', linked to '" & Me.Collections.FlowsheetObjectCollection(scr.LinkedObjectName).GraphicObject.Tag & "'...", Color.Blue, MessageType.Information)
                                        Else
                                            Me.WriteToLog("Running script '" & scr.Title & "' for event '" & scr.LinkedEventType.ToString & "'", Color.Blue, MessageType.Information)
                                        End If
                                    End If
                                    FormScript.RunScript(scr.ScriptText, Me)
                                End If
                            Next
                        Else
                            Me.ScriptCollection = New Dictionary(Of String, Script)
                        End If
                    End Sub)

    End Sub

    Public Sub AddUnitSystem(ByVal su As DWSIM.SystemsOfUnits.Units)

        If Not My.Application.UserUnitSystems.ContainsKey(su.Name) Then
            My.Application.UserUnitSystems.Add(su.Name, su)
            FormMain.AvailableUnitSystems.Add(su.Name, su)
            Me.FrmStSim1.ComboBox2.Items.Add(su.Name)
            Me.ToolStripComboBoxUnitSystem.Items.Add(su.Name)
        Else
            MessageBox.Show("Please input a different name for the unit system.")
        End If

    End Sub

    Public Sub AddComponentsRows(ByRef MaterialStream As DWSIM.SimulationObjects.Streams.MaterialStream)
        If Me.Options.SelectedComponents.Count = 0 Then
            MessageBox.Show(DWSIM.App.GetLocalString("Nohcomponentesaadici"))
        Else
            Dim comp As DWSIM.Thermodynamics.BaseClasses.ConstantProperties
            For Each phase As DWSIM.Thermodynamics.BaseClasses.Phase In MaterialStream.Phases.Values
                For Each comp In Me.Options.SelectedComponents.Values
                    With phase
                        .Compounds.Add(comp.Name, New DWSIM.Thermodynamics.BaseClasses.Compound(comp.Name, ""))
                        .Compounds(comp.Name).ConstantProperties = comp
                    End With
                Next
            Next
        End If
    End Sub

    Public Function FT(ByRef prop As String, ByVal unit As String)
        Return prop & " (" & unit & ")"
    End Function

    Public Enum ID_Type
        Name
        Tag
    End Enum

    Public Shared Function SearchSurfaceObjectsByName(ByVal Name As String, ByVal Surface As Microsoft.Msdn.Samples.DesignSurface.GraphicsSurface) As GraphicObject

        Dim gObj As GraphicObject = Nothing
        Dim gObj2 As GraphicObject = Nothing
        For Each gObj In Surface.drawingObjects
            If gObj.Name.ToString = Name Then
                gObj2 = gObj
                Exit For
            End If
        Next
        Return gObj2

    End Function

    Public Shared Function SearchSurfaceObjectsByTag(ByVal Name As String, ByVal Surface As Microsoft.Msdn.Samples.DesignSurface.GraphicsSurface) As GraphicObject

        Dim gObj As GraphicObject = Nothing
        Dim gObj2 As GraphicObject = Nothing
        For Each gObj In Surface.drawingObjects
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
        For Each gObj In Me.FormSurface.FlowsheetDesignSurface.drawingObjects
            If gObj.Tag.ToString = tag Then
                gObj2 = gObj
                Exit For
            End If
        Next

        Return gObj2

    End Function

    Public Function GetFlowsheetSimulationObject(ByVal tag As String) As DWSIM.SimulationObjects.UnitOperations.BaseClass

        For Each obj As DWSIM.SimulationObjects.UnitOperations.BaseClass In Me.Collections.FlowsheetObjectCollection.Values
            If obj.GraphicObject.Tag = tag Then
                Return obj
            End If
        Next

        Return Nothing

    End Function

    Public Function gscTogoc(ByVal X As Integer, ByVal Y As Integer) As Point
        Dim myNewPoint As Point
        myNewPoint.X = Convert.ToInt32((X - Me.FormSurface.FlowsheetDesignSurface.AutoScrollPosition.X) / Me.FormSurface.FlowsheetDesignSurface.Zoom)
        myNewPoint.Y = Convert.ToInt32((Y - Me.FormSurface.FlowsheetDesignSurface.AutoScrollPosition.Y) / Me.FormSurface.FlowsheetDesignSurface.Zoom)
        Return myNewPoint
    End Function

    Public Sub WriteToLog(ByVal texto As String, ByVal cor As Color, ByVal tipo As DWSIM.Flowsheet.MessageType)

        If texto.Trim <> "" Then

            Dim frsht As FormFlowsheet
            If Not Me.MasterFlowsheet Is Nothing And Me.RedirectMessages Then
                frsht = Me.MasterFlowsheet
                texto = "[" & Me.MasterUnitOp.GraphicObject.Tag & "] " & texto
            Else
                frsht = Me
            End If

            If frsht.Visible Then

                frsht.UIThread(New System.Action(Sub()

                                                     If Not My.Application.CommandLineMode Then

                                                         Dim frlog = frsht.FormLog

                                                         Dim img As Bitmap
                                                         Dim strtipo As String
                                                         Select Case tipo
                                                             Case DWSIM.Flowsheet.MessageType.Warning
                                                                 img = My.Resources._error
                                                                 strtipo = DWSIM.App.GetLocalString("Aviso")
                                                             Case DWSIM.Flowsheet.MessageType.GeneralError
                                                                 img = My.Resources.exclamation
                                                                 strtipo = DWSIM.App.GetLocalString("Erro")
                                                             Case DWSIM.Flowsheet.MessageType.Tip
                                                                 If Not My.Settings.ShowTips Then Exit Sub
                                                                 img = My.Resources.lightbulb
                                                                 strtipo = DWSIM.App.GetLocalString("Dica")
                                                             Case Else
                                                                 img = My.Resources.information
                                                                 strtipo = DWSIM.App.GetLocalString("Mensagem")
                                                         End Select

                                                         If frlog.GridDT.Columns.Count < 4 Then
                                                             frlog.GridDT.Columns.Add("Imagem", GetType(Bitmap))
                                                             frlog.GridDT.Columns.Add("Data")
                                                             frlog.GridDT.Columns.Add("Tipo")
                                                             frlog.GridDT.Columns.Add("Mensagem")
                                                             frlog.GridDT.Columns.Add("Cor", GetType(Color))
                                                             frlog.GridDT.Columns.Add("Indice")
                                                         ElseIf frlog.GridDT.Columns.Count = 4 Then
                                                             frlog.GridDT.Columns.Add("Cor", GetType(Color))
                                                             frlog.GridDT.Columns.Add("Indice")
                                                         ElseIf frlog.GridDT.Columns.Count = 5 Then
                                                             frlog.GridDT.Columns.Add("Indice")
                                                         End If
                                                         frlog.GridDT.PrimaryKey = New DataColumn() {frlog.GridDT.Columns("Indice")}
                                                         With frlog.GridDT.Columns("Indice")
                                                             .AutoIncrement = True
                                                             .AutoIncrementSeed = 1
                                                             .AutoIncrementStep = 1
                                                             .Unique = True
                                                         End With

                                                         frlog.GridDT.Rows.Add(New Object() {img, Date.Now, strtipo, texto, cor, frlog.GridDT.Rows.Count})

                                                         If DWSIM.App.IsRunningOnMono Then
                                                             frlog.Grid1.Rows.Add(New Object() {img, frlog.GridDT.Rows.Count, Date.Now, strtipo, texto})
                                                         End If

                                                     Else

                                                         If Not frsht.FormCL Is Nothing Then
                                                             frsht.FormCL.LBLogMsg.Items.Insert(0, Date.Now.ToString & " " & texto)
                                                         End If

                                                     End If

                                                 End Sub))

            End If

        End If

    End Sub

    Public Sub WriteMessage(ByVal message As String)
        WriteToLog(message, Color.Black, DWSIM.Flowsheet.MessageType.Information)
    End Sub

    Public Sub UpdateStatusLabel(ByVal message As String)
        Me.FormSurface.LabelCalculator.Text = message
    End Sub

    Public Sub CheckCollections()

        'Creates all the graphic collections.
        If Collections.MixerCollection Is Nothing Then Collections.MixerCollection = New Dictionary(Of String, NodeInGraphic)
        If Collections.SplitterCollection Is Nothing Then Collections.SplitterCollection = New Dictionary(Of String, NodeOutGraphic)
        If Collections.MaterialStreamCollection Is Nothing Then Collections.MaterialStreamCollection = New Dictionary(Of String, MaterialStreamGraphic)
        If Collections.EnergyStreamCollection Is Nothing Then Collections.EnergyStreamCollection = New Dictionary(Of String, EnergyStreamGraphic)
        If Collections.PumpCollection Is Nothing Then Collections.PumpCollection = New Dictionary(Of String, PumpGraphic)
        If Collections.SeparatorCollection Is Nothing Then Collections.SeparatorCollection = New Dictionary(Of String, VesselGraphic)
        If Collections.CompressorCollection Is Nothing Then Collections.CompressorCollection = New Dictionary(Of String, CompressorGraphic)
        If Collections.PipeCollection Is Nothing Then Collections.PipeCollection = New Dictionary(Of String, PipeGraphic)
        If Collections.ValveCollection Is Nothing Then Collections.ValveCollection = New Dictionary(Of String, ValveGraphic)
        If Collections.CoolerCollection Is Nothing Then Collections.CoolerCollection = New Dictionary(Of String, CoolerGraphic)
        If Collections.HeaterCollection Is Nothing Then Collections.HeaterCollection = New Dictionary(Of String, HeaterGraphic)
        If Collections.TankCollection Is Nothing Then Collections.TankCollection = New Dictionary(Of String, TankGraphic)
        If Collections.ConnectorCollection Is Nothing Then Collections.ConnectorCollection = New Dictionary(Of String, ConnectorGraphic)
        If Collections.TPSeparatorCollection Is Nothing Then Collections.TPSeparatorCollection = New Dictionary(Of String, TPVesselGraphic)
        If Collections.TurbineCollection Is Nothing Then Collections.TurbineCollection = New Dictionary(Of String, TurbineGraphic)
        If Collections.MixerENCollection Is Nothing Then Collections.MixerENCollection = New Dictionary(Of String, NodeEnGraphic)
        If Collections.AdjustCollection Is Nothing Then Collections.AdjustCollection = New Dictionary(Of String, AdjustGraphic)
        If Collections.SpecCollection Is Nothing Then Collections.SpecCollection = New Dictionary(Of String, SpecGraphic)
        If Collections.RecycleCollection Is Nothing Then Collections.RecycleCollection = New Dictionary(Of String, RecycleGraphic)
        If Collections.ReactorConversionCollection Is Nothing Then Collections.ReactorConversionCollection = New Dictionary(Of String, ReactorConversionGraphic)
        If Collections.ReactorEquilibriumCollection Is Nothing Then Collections.ReactorEquilibriumCollection = New Dictionary(Of String, ReactorEquilibriumGraphic)
        If Collections.ReactorGibbsCollection Is Nothing Then Collections.ReactorGibbsCollection = New Dictionary(Of String, ReactorGibbsGraphic)
        If Collections.ReactorCSTRCollection Is Nothing Then Collections.ReactorCSTRCollection = New Dictionary(Of String, ReactorCSTRGraphic)
        If Collections.ReactorPFRCollection Is Nothing Then Collections.ReactorPFRCollection = New Dictionary(Of String, ReactorPFRGraphic)
        If Collections.HeatExchangerCollection Is Nothing Then Collections.HeatExchangerCollection = New Dictionary(Of String, HeatExchangerGraphic)
        If Collections.ShortcutColumnCollection Is Nothing Then Collections.ShortcutColumnCollection = New Dictionary(Of String, ShorcutColumnGraphic)
        If Collections.DistillationColumnCollection Is Nothing Then Collections.DistillationColumnCollection = New Dictionary(Of String, DistillationColumnGraphic)
        If Collections.AbsorptionColumnCollection Is Nothing Then Collections.AbsorptionColumnCollection = New Dictionary(Of String, AbsorptionColumnGraphic)
        If Collections.RefluxedAbsorberCollection Is Nothing Then Collections.RefluxedAbsorberCollection = New Dictionary(Of String, RefluxedAbsorberGraphic)
        If Collections.ReboiledAbsorberCollection Is Nothing Then Collections.ReboiledAbsorberCollection = New Dictionary(Of String, ReboiledAbsorberGraphic)
        If Collections.EnergyRecycleCollection Is Nothing Then Collections.EnergyRecycleCollection = New Dictionary(Of String, EnergyRecycleGraphic)
        If Collections.ComponentSeparatorCollection Is Nothing Then Collections.ComponentSeparatorCollection = New Dictionary(Of String, ComponentSeparatorGraphic)
        If Collections.OrificePlateCollection Is Nothing Then Collections.OrificePlateCollection = New Dictionary(Of String, OrificePlateGraphic)
        If Collections.CustomUOCollection Is Nothing Then Collections.CustomUOCollection = New Dictionary(Of String, CustomUOGraphic)
        If Collections.ExcelUOCollection Is Nothing Then Collections.ExcelUOCollection = New Dictionary(Of String, ExcelUOGraphic)
        If Collections.FlowsheetUOCollection Is Nothing Then Collections.FlowsheetUOCollection = New Dictionary(Of String, FlowsheetUOGraphic)
        If Collections.SolidsSeparatorCollection Is Nothing Then Collections.SolidsSeparatorCollection = New Dictionary(Of String, SolidSeparatorGraphic)
        If Collections.FilterCollection Is Nothing Then Collections.FilterCollection = New Dictionary(Of String, FilterGraphic)

        If Collections.FlowsheetObjectCollection Is Nothing Then Collections.FlowsheetObjectCollection = New Dictionary(Of String, DWSIM.SimulationObjects.UnitOperations.BaseClass)

        'Creates all the actual unit operations collections.
        If Collections.CLCS_MaterialStreamCollection Is Nothing Then Collections.CLCS_MaterialStreamCollection = New Dictionary(Of String, DWSIM.SimulationObjects.Streams.MaterialStream)
        If Collections.CLCS_EnergyStreamCollection Is Nothing Then Collections.CLCS_EnergyStreamCollection = New Dictionary(Of String, DWSIM.SimulationObjects.Streams.EnergyStream)
        If Collections.CLCS_PipeCollection Is Nothing Then Collections.CLCS_PipeCollection = New Dictionary(Of String, DWSIM.SimulationObjects.UnitOperations.Pipe)
        If Collections.CLCS_MixerCollection Is Nothing Then Collections.CLCS_MixerCollection = New Dictionary(Of String, DWSIM.SimulationObjects.UnitOperations.Mixer)
        If Collections.CLCS_SplitterCollection Is Nothing Then Collections.CLCS_SplitterCollection = New Dictionary(Of String, DWSIM.SimulationObjects.UnitOperations.Splitter)
        If Collections.CLCS_PumpCollection Is Nothing Then Collections.CLCS_PumpCollection = New Dictionary(Of String, DWSIM.SimulationObjects.UnitOperations.Pump)
        If Collections.CLCS_CompressorCollection Is Nothing Then Collections.CLCS_CompressorCollection = New Dictionary(Of String, DWSIM.SimulationObjects.UnitOperations.Compressor)
        If Collections.CLCS_ValveCollection Is Nothing Then Collections.CLCS_ValveCollection = New Dictionary(Of String, DWSIM.SimulationObjects.UnitOperations.Valve)
        If Collections.CLCS_VesselCollection Is Nothing Then Collections.CLCS_VesselCollection = New Dictionary(Of String, DWSIM.SimulationObjects.UnitOperations.Vessel)
        If Collections.CLCS_TurbineCollection Is Nothing Then Collections.CLCS_TurbineCollection = New Dictionary(Of String, DWSIM.SimulationObjects.UnitOperations.Expander)
        If Collections.CLCS_EnergyMixerCollection Is Nothing Then Collections.CLCS_EnergyMixerCollection = New Dictionary(Of String, DWSIM.SimulationObjects.UnitOperations.EnergyMixer)
        If Collections.CLCS_HeaterCollection Is Nothing Then Collections.CLCS_HeaterCollection = New Dictionary(Of String, DWSIM.SimulationObjects.UnitOperations.Heater)
        If Collections.CLCS_CoolerCollection Is Nothing Then Collections.CLCS_CoolerCollection = New Dictionary(Of String, DWSIM.SimulationObjects.UnitOperations.Cooler)
        If Collections.CLCS_TankCollection Is Nothing Then Collections.CLCS_TankCollection = New Dictionary(Of String, DWSIM.SimulationObjects.UnitOperations.Tank)
        If Collections.CLCS_AdjustCollection Is Nothing Then Collections.CLCS_AdjustCollection = New Dictionary(Of String, DWSIM.SimulationObjects.SpecialOps.Adjust)
        If Collections.CLCS_SpecCollection Is Nothing Then Collections.CLCS_SpecCollection = New Dictionary(Of String, DWSIM.SimulationObjects.SpecialOps.Spec)
        If Collections.CLCS_RecycleCollection Is Nothing Then Collections.CLCS_RecycleCollection = New Dictionary(Of String, DWSIM.SimulationObjects.SpecialOps.Recycle)
        If Collections.CLCS_ReactorConversionCollection Is Nothing Then Collections.CLCS_ReactorConversionCollection = New Dictionary(Of String, DWSIM.SimulationObjects.Reactors.Reactor_Conversion)
        If Collections.CLCS_ReactorEquilibriumCollection Is Nothing Then Collections.CLCS_ReactorEquilibriumCollection = New Dictionary(Of String, DWSIM.SimulationObjects.Reactors.Reactor_Equilibrium)
        If Collections.CLCS_ReactorGibbsCollection Is Nothing Then Collections.CLCS_ReactorGibbsCollection = New Dictionary(Of String, DWSIM.SimulationObjects.Reactors.Reactor_Gibbs)
        If Collections.CLCS_ReactorCSTRCollection Is Nothing Then Collections.CLCS_ReactorCSTRCollection = New Dictionary(Of String, DWSIM.SimulationObjects.Reactors.Reactor_CSTR)
        If Collections.CLCS_ReactorPFRCollection Is Nothing Then Collections.CLCS_ReactorPFRCollection = New Dictionary(Of String, DWSIM.SimulationObjects.Reactors.Reactor_PFR)
        If Collections.CLCS_HeatExchangerCollection Is Nothing Then Collections.CLCS_HeatExchangerCollection = New Dictionary(Of String, DWSIM.SimulationObjects.UnitOperations.HeatExchanger)
        If Collections.CLCS_ShortcutColumnCollection Is Nothing Then Collections.CLCS_ShortcutColumnCollection = New Dictionary(Of String, DWSIM.SimulationObjects.UnitOperations.ShortcutColumn)
        If Collections.CLCS_DistillationColumnCollection Is Nothing Then Collections.CLCS_DistillationColumnCollection = New Dictionary(Of String, DWSIM.SimulationObjects.UnitOperations.DistillationColumn)
        If Collections.CLCS_AbsorptionColumnCollection Is Nothing Then Collections.CLCS_AbsorptionColumnCollection = New Dictionary(Of String, DWSIM.SimulationObjects.UnitOperations.AbsorptionColumn)
        If Collections.CLCS_ReboiledAbsorberCollection Is Nothing Then Collections.CLCS_ReboiledAbsorberCollection = New Dictionary(Of String, DWSIM.SimulationObjects.UnitOperations.ReboiledAbsorber)
        If Collections.CLCS_RefluxedAbsorberCollection Is Nothing Then Collections.CLCS_RefluxedAbsorberCollection = New Dictionary(Of String, DWSIM.SimulationObjects.UnitOperations.RefluxedAbsorber)
        If Collections.CLCS_EnergyRecycleCollection Is Nothing Then Collections.CLCS_EnergyRecycleCollection = New Dictionary(Of String, DWSIM.SimulationObjects.SpecialOps.EnergyRecycle)
        If Collections.CLCS_ComponentSeparatorCollection Is Nothing Then Collections.CLCS_ComponentSeparatorCollection = New Dictionary(Of String, DWSIM.SimulationObjects.UnitOperations.ComponentSeparator)
        If Collections.CLCS_OrificePlateCollection Is Nothing Then Collections.CLCS_OrificePlateCollection = New Dictionary(Of String, DWSIM.SimulationObjects.UnitOperations.OrificePlate)
        If Collections.CLCS_CustomUOCollection Is Nothing Then Collections.CLCS_CustomUOCollection = New Dictionary(Of String, DWSIM.SimulationObjects.UnitOperations.CustomUO)
        If Collections.CLCS_ExcelUOCollection Is Nothing Then Collections.CLCS_ExcelUOCollection = New Dictionary(Of String, DWSIM.SimulationObjects.UnitOperations.ExcelUO)
        If Collections.CLCS_SolidsSeparatorCollection Is Nothing Then Collections.CLCS_SolidsSeparatorCollection = New Dictionary(Of String, DWSIM.SimulationObjects.UnitOperations.SolidsSeparator)
        If Collections.CLCS_FilterCollection Is Nothing Then Collections.CLCS_FilterCollection = New Dictionary(Of String, DWSIM.SimulationObjects.UnitOperations.Filter)
        If Collections.CLCS_FlowsheetUOCollection Is Nothing Then Collections.CLCS_FlowsheetUOCollection = New Dictionary(Of String, DWSIM.SimulationObjects.UnitOperations.Flowsheet)

        If Collections.OPT_SensAnalysisCollection Is Nothing Then Collections.OPT_SensAnalysisCollection = New List(Of DWSIM.Optimization.SensitivityAnalysisCase)
        If Collections.OPT_OptimizationCollection Is Nothing Then Collections.OPT_OptimizationCollection = New List(Of DWSIM.Optimization.OptimizationCase)

    End Sub

#End Region

#Region "    Click Event Handlers "

    Private Sub FormFlowsheet_HelpRequested(sender As System.Object, hlpevent As System.Windows.Forms.HelpEventArgs) Handles MyBase.HelpRequested

        Dim obj As GraphicObject = Me.FormSurface.FlowsheetDesignSurface.SelectedObject

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
        FormProps.DockState = DockState.DockLeft
        FormLog.DockState = DockState.DockBottom

    End Sub

    Sub UpdateToolstripItemVisibility()

        Dim isenabled As Boolean = Me.FormSurface.FlowsheetDesignSurface.SelectedObjects.Count > 0

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
        Dim n As Integer = Me.FormSurface.FlowsheetDesignSurface.SelectedObjects.Count
        If n > 1 Then
            If MessageBox.Show("Delete " & n & " objects?", "Mass delete", MessageBoxButtons.YesNo) = Windows.Forms.DialogResult.Yes Then
                Dim indexes As New ArrayList
                For Each gobj As GraphicObject In Me.FormSurface.FlowsheetDesignSurface.SelectedObjects.Values
                    indexes.Add(gobj.Tag)
                Next
                For Each s As String In indexes
                    Dim gobj As GraphicObject
                    gobj = GetFlowsheetGraphicObject(s)
                    If Not gobj Is Nothing Then
                        DeleteSelectedObject(sender, e, gobj, False)
                        Me.FormSurface.FlowsheetDesignSurface.SelectedObjects.Remove(gobj.Name)
                    End If
                Next
            End If
        ElseIf n = 1 Then
            DeleteSelectedObject(sender, e, Me.FormSurface.FlowsheetDesignSurface.SelectedObject)
        End If
    End Sub

    Public Sub tsmiCloneSelected_Click(sender As Object, e As EventArgs) Handles tsmiCloneSelected.Click
        For Each obj In Me.FormSurface.FlowsheetDesignSurface.SelectedObjects.Values
            FormSurface.CloneObject(obj)
        Next
    End Sub

    Public Sub tsmiRecalc_Click(sender As Object, e As EventArgs) Handles tsmiRecalc.Click

        If Not Me.FormSurface.FlowsheetDesignSurface.SelectedObject Is Nothing Then

            Dim obj As DWSIM.SimulationObjects.UnitOperations.BaseClass = Collections.FlowsheetObjectCollection(Me.FormSurface.FlowsheetDesignSurface.SelectedObject.Name)

            'Call function to calculate flowsheet
            Dim objargs As New DWSIM.Extras.StatusChangeEventArgs
            With objargs
                .Calculated = False
                .Tag = obj.GraphicObject.Tag
                .Name = obj.Name
                .ObjectType = obj.GraphicObject.ObjectType
                .Sender = "PropertyGrid"
            End With

            CalculationQueue.Enqueue(objargs)

            CalculateAll2(Me, My.Settings.SolverMode, , True)

        End If

    End Sub

    Public Sub tsmiExportData_Click(sender As Object, e As EventArgs) Handles tsmiExportData.Click
        'copy all simulation properties from the selected object to clipboard
        Try
            Select Case Me.FormSurface.FlowsheetDesignSurface.SelectedObject.ObjectType
                Case ObjectType.GO_MasterTable
                    DirectCast(Me.FormSurface.FlowsheetDesignSurface.SelectedObject, DWSIM.GraphicObjects.MasterTableGraphic).CopyToClipboard()
                Case ObjectType.GO_SpreadsheetTable
                    DirectCast(Me.FormSurface.FlowsheetDesignSurface.SelectedObject, DWSIM.GraphicObjects.SpreadsheetTableGraphic).CopyToClipboard()
                Case ObjectType.GO_Table
                    DirectCast(Me.FormSurface.FlowsheetDesignSurface.SelectedObject, DWSIM.GraphicObjects.TableGraphic).CopyToClipboard()
                Case Else
                    Collections.FlowsheetObjectCollection(Me.FormSurface.FlowsheetDesignSurface.SelectedObject.Name).CopyDataToClipboard(Options.SelectedUnitSystem, Options.NumberFormat)
            End Select
        Catch ex As Exception
            WriteToLog("Error copying data to clipboard: " & ex.ToString, Color.Red, DWSIM.Flowsheet.MessageType.GeneralError)
        End Try
    End Sub
    Private Sub tsbCutObj_Click(sender As Object, e As EventArgs) Handles tsbCutObj.Click
        CutObjects()
    End Sub

    Private Sub tsbCopyObj_Click(sender As Object, e As EventArgs) Handles tsbCopyObj.Click
        CopyObjects()
    End Sub

    Private Sub tsbPasteObj_Click(sender As Object, e As EventArgs) Handles tsbPasteObj.Click
        PasteObjects()
    End Sub

    Private Sub showflowsheettoolstripmenuitem_Click(sender As Object, e As EventArgs) Handles showflowsheettoolstripmenuitem.Click
        ToolStripFlowsheet.Visible = showflowsheettoolstripmenuitem.Checked
        My.Settings.ShowFlowsheetToolStrip = showflowsheettoolstripmenuitem.Checked
    End Sub

    Private Sub showsimulationtoolstripmenuitem_Click(sender As Object, e As EventArgs) Handles showsimulationtoolstripmenuitem.Click
        ToolStripSimulation.Visible = showsimulationtoolstripmenuitem.Checked
        ToolStripCalculator.Visible = showsimulationtoolstripmenuitem.Checked
        ToolStripStates.Visible = showsimulationtoolstripmenuitem.Checked
        My.Settings.ShowSimulationToolStrip = showsimulationtoolstripmenuitem.Checked
    End Sub

    Private Sub showunitstoolstripmenuitem_Click(sender As Object, e As EventArgs) Handles showunitstoolstripmenuitem.Click
        ToolStripUnits.Visible = showunitstoolstripmenuitem.Checked
        My.Settings.ShowUnitsToolStrip = showunitstoolstripmenuitem.Checked
    End Sub

    Private Sub tsbAlign_Click(sender As Object, e As EventArgs) Handles tsbAlignLefts.Click, tsbAlignCenters.Click, tsbAlignRights.Click,
                                                                        tsbAlignTops.Click, tsbAlignMiddles.Click, tsbAlignBottoms.Click,
                                                                        tsbAlignVertical.Click, tsbAlignHorizontal.Click

        Dim tsb As ToolStripButton = DirectCast(sender, ToolStripButton)

        Dim direction As Microsoft.Msdn.Samples.DesignSurface.GraphicsSurface.AlignDirection

        If tsb.Name.Contains("Lefts") Then
            direction = Microsoft.Msdn.Samples.DesignSurface.GraphicsSurface.AlignDirection.Lefts
        ElseIf tsb.Name.Contains("Centers") Then
            direction = Microsoft.Msdn.Samples.DesignSurface.GraphicsSurface.AlignDirection.Centers
        ElseIf tsb.Name.Contains("Rights") Then
            direction = Microsoft.Msdn.Samples.DesignSurface.GraphicsSurface.AlignDirection.Rights
        ElseIf tsb.Name.Contains("Tops") Then
            direction = Microsoft.Msdn.Samples.DesignSurface.GraphicsSurface.AlignDirection.Tops
        ElseIf tsb.Name.Contains("Middles") Then
            direction = Microsoft.Msdn.Samples.DesignSurface.GraphicsSurface.AlignDirection.Middles
        ElseIf tsb.Name.Contains("Bottoms") Then
            direction = Microsoft.Msdn.Samples.DesignSurface.GraphicsSurface.AlignDirection.Bottoms
        ElseIf tsb.Name.Contains("Vertical") Then
            direction = Microsoft.Msdn.Samples.DesignSurface.GraphicsSurface.AlignDirection.EqualizeVertical
        ElseIf tsb.Name.Contains("Horizontal") Then
            direction = Microsoft.Msdn.Samples.DesignSurface.GraphicsSurface.AlignDirection.EqualizeHorizontal
        End If

        Me.FormSurface.FlowsheetDesignSurface.AlignSelectedObjects(direction)

    End Sub

    Private Sub InserObjectTSMIClick(ByVal sender As System.Object, ByVal e As EventArgs) Handles _
    TSMIAdjust.Click, TSMIColAbs.Click, TSMIColAbsCond.Click, TSMIColAbsReb.Click, TSMIColDist.Click, _
     TSMIColShortcut.Click, TSMIComponentSeparator.Click, TSMICompressor.Click, TSMICooler.Click, _
     TSMIEnergyRecycle.Click, TSMIEnergyStream.Click, TSMIExpander.Click, TSMIHeater.Click, _
     TSMIHeatExchanger.Click, TSMIMaterialStream.Click, TSMIMixer.Click, TSMIOrificePlate.Click, _
     TSMIPipe.Click, TSMIPump.Click, TSMIReactorConv.Click, TSMIReactorCSTR.Click, TSMIReactorEquilibrium.Click, _
     TSMIReactorGibbs.Click, TSMIReactorPFR.Click, TSMIRecycle.Click, TSMISeparator.Click, _
     TSMISpecification.Click, TSMISplitter.Click, TSMITank.Click, TSMIValve.Click, TSMICUO.Click, TSMICOUO.Click, _
     TSMISolidsSeparator.Click, TSMIFilter.Click, TSMIExcelUO.Click, TSMIFlowsheet.Click

        Me.InsertingObjectToPFD = True
        Me.FormSurface.FlowsheetDesignSurface.Cursor = Cursors.Hand

        Me.ClickedToolStripMenuItem = sender

    End Sub

    Private Sub InsertObjectButtonClick(ByVal sender As System.Object, ByVal e As System.EventArgs)

        If Not Me.CheckedToolstripButton Is Nothing Then
            Try
                Me.CheckedToolstripButton.Checked = False
            Catch ex As Exception

            End Try
        End If

        Me.CheckedToolstripButton = sender

        If Me.CheckedToolstripButton.Name = "TSBSelect" Then
            Me.InsertingObjectToPFD = False
            Me.FormSurface.FlowsheetDesignSurface.Cursor = Cursors.Default
        ElseIf Me.CheckedToolstripButton.Checked = True Then
            Me.InsertingObjectToPFD = True
            Me.FormSurface.FlowsheetDesignSurface.Cursor = Cursors.Hand
        Else
            Me.InsertingObjectToPFD = False
            Me.FormSurface.FlowsheetDesignSurface.Cursor = Cursors.Default
        End If

    End Sub

    Private Sub ToolStripButton6_Click_1(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles tsbConfigPage.Click
        Me.FormSurface.pageSetup.ShowDialog()
    End Sub

    Private Sub ToolStripButton10_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles tsbPrint.Click
        Me.FormSurface.PreviewDialog.ShowDialog()
    End Sub

    Private Sub ToolStripButton11_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles tsbConfigPrinter.Click
        Me.FormSurface.setupPrint.ShowDialog()
    End Sub

    Private Sub TSBTexto_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles TSBTexto.Click
        Dim myTextObject As New TextGraphic(-Me.FormSurface.FlowsheetDesignSurface.AutoScrollPosition.X / Me.FormSurface.FlowsheetDesignSurface.Zoom + 30, _
            -Me.FormSurface.FlowsheetDesignSurface.AutoScrollPosition.Y / Me.FormSurface.FlowsheetDesignSurface.Zoom + 30, _
            DWSIM.App.GetLocalString("caixa_de_texto"), _
            System.Drawing.SystemFonts.DefaultFont, _
            Color.Black)
        Dim gObj As GraphicObject = Nothing
        gObj = myTextObject
        gObj.Name = "TEXT-" & Guid.NewGuid.ToString
        gObj.Tag = "TEXT" & ((From t As GraphicObject In Me.FormSurface.FlowsheetDesignSurface.drawingObjects Select t Where t.ObjectType = ObjectType.GO_Text).Count + 1).ToString
        gObj.AutoSize = True
        gObj.ObjectType = ObjectType.GO_Text
        Me.FormSurface.FlowsheetDesignSurface.drawingObjects.Add(gObj)
        Me.FormSurface.FlowsheetDesignSurface.Invalidate()

    End Sub

    Private Sub ToolStripButton19_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ToolStripButton19.Click
        Dim myMasterTable As New DWSIM.GraphicObjects.MasterTableGraphic(-Me.FormSurface.FlowsheetDesignSurface.AutoScrollPosition.X / Me.FormSurface.FlowsheetDesignSurface.Zoom + 30, _
           -Me.FormSurface.FlowsheetDesignSurface.AutoScrollPosition.Y / Me.FormSurface.FlowsheetDesignSurface.Zoom + 30)
        Dim gObj As GraphicObject = Nothing
        gObj = myMasterTable
        gObj.Tag = "MASTERTABLE-" & Guid.NewGuid.ToString
        gObj.AutoSize = True
        gObj.ObjectType = ObjectType.GO_MasterTable
        Me.FormSurface.FlowsheetDesignSurface.drawingObjects.Add(gObj)
        Me.FormSurface.FlowsheetDesignSurface.Invalidate()
    End Sub

    Private Sub ToolStripButton4_Click(sender As Object, e As EventArgs) Handles ToolStripButton4.Click
        Dim mySpreadsheetTable As New DWSIM.GraphicObjects.SpreadsheetTableGraphic(
            Me.FormSpreadsheet,
            -Me.FormSurface.FlowsheetDesignSurface.AutoScrollPosition.X / Me.FormSurface.FlowsheetDesignSurface.Zoom + 30,
            -Me.FormSurface.FlowsheetDesignSurface.AutoScrollPosition.Y / Me.FormSurface.FlowsheetDesignSurface.Zoom + 30)
        Dim gObj As GraphicObject = Nothing
        gObj = mySpreadsheetTable
        gObj.Tag = "SHEETTABLE-" & Guid.NewGuid.ToString
        gObj.AutoSize = True
        gObj.ObjectType = ObjectType.GO_SpreadsheetTable
        Me.FormSurface.FlowsheetDesignSurface.drawingObjects.Add(gObj)
        Me.FormSurface.FlowsheetDesignSurface.Invalidate()
    End Sub

    Private Sub TSBtabela_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles TSBtabela.Click
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
                Dim img = System.Drawing.Image.FromFile(.FileName)
                Dim gObj As GraphicObject = Nothing
                If Not img Is Nothing Then
                    Dim myEmbeddedImage As New EmbeddedImageGraphic(-Me.FormSurface.FlowsheetDesignSurface.AutoScrollPosition.X / Me.FormSurface.FlowsheetDesignSurface.Zoom, _
                                    -Me.FormSurface.FlowsheetDesignSurface.AutoScrollPosition.Y / Me.FormSurface.FlowsheetDesignSurface.Zoom, img)
                    gObj = myEmbeddedImage
                    gObj.Tag = DWSIM.App.GetLocalString("FIGURA") & Guid.NewGuid.ToString
                    gObj.AutoSize = True
                End If
                Me.FormSurface.FlowsheetDesignSurface.drawingObjects.Add(gObj)
                Me.FormSurface.FlowsheetDesignSurface.Invalidate()
            End If
        End With
        Me.TSBtabela.Checked = False
    End Sub

    Private Sub PropriedadesDosComponentesToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles PropriedadesDosComponentesToolStripMenuItem.Click
        Dim frmpc As New FormPureComp
        frmpc.ShowDialog(Me)
    End Sub

    Private Sub PontoCríticoRealToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles PontoCríticoRealToolStripMenuItem.Click
        Me.FormCritPt.ShowDialog(Me)
    End Sub

    Private Sub DiagramaDePhasesToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles DiagramaDePhasesToolStripMenuItem.Click
        Me.FormStabAn.ShowDialog(Me)
    End Sub

    Private Sub tsbAtivar_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles tsbAtivar.Click
        Me.tsbDesat.Checked = False
        Me.tsbAtivar.Checked = True
        Me.Options.CalculatorActivated = True
        Me.FormSurface.LabelCalculator.Text = DWSIM.App.GetLocalString("CalculadorOcioso")
        Me.WriteToLog(DWSIM.App.GetLocalString("Calculadorativado"), Color.DimGray, DWSIM.Flowsheet.MessageType.Information)
        If Not Me.CalculationQueue Is Nothing Then Me.CalculationQueue.Clear()
    End Sub

    Private Sub tsbDesat_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles tsbDesat.Click
        Me.tsbAtivar.Checked = False
        Me.tsbDesat.Checked = True
        Me.Options.CalculatorActivated = False
        Me.FormSurface.LabelCalculator.Text = DWSIM.App.GetLocalString("CalculadorDesativado1")
    End Sub

    Private Sub HYDVerificaçãoDasCondiçõesDeFormaçãoDeHidratosToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles HYDVerificaçãoDasCondiçõesDeFormaçãoDeHidratosToolStripMenuItem.Click
        Me.FormHid = New FormHYD
        Me.FormHid.Show(Me.dckPanel)
    End Sub

    Private Sub DiagramaDePhasesToolStripMenuItem1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles DiagramaDePhasesToolStripMenuItem1.Click
        Me.FormPE = New FormPhEnv
        Me.FormPE.Show(Me.dckPanel)
    End Sub
    Private Sub LLEDiagramToolStripMenuItem_Click(sender As System.Object, e As System.EventArgs) Handles LLEDiagramToolStripMenuItem.Click
        Me.FormLLEDiag = New FormLLEDiagram
        Me.FormLLEDiag.Show(Me.dckPanel)
    End Sub
    Private Sub DiagramaBinárioToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles DiagramaBinárioToolStripMenuItem.Click
        Me.FormBE = New FormBinEnv
        Me.FormBE.Show(Me.dckPanel)
    End Sub
    Private Sub FecharSimulaçãoAtualToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles CloseToolStripMenuItem.Click
        Me.Close()
    End Sub

    Private Sub PSVSizingToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles PSVSizingToolStripMenuItem.Click
        Me.FormPSVS.ShowDialog(Me)
    End Sub

    Private Sub FlashVesselSizingToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles FlashVesselSizingToolStripMenuItem.Click
        Me.FormVS.ShowDialog(Me)
    End Sub

    Private Sub PropriedadesDePetróleosToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles PropriedadesDePetróleosToolStripMenuItem.Click
        Me.FormColdP = New FrmColdProperties
        Me.FormColdP.Show(Me.dckPanel)
    End Sub

    Private Sub ToolStripButton14_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ToolStripButton14.Click
        My.Application.CalculatorStopRequested = True
        If My.Application.TaskCancellationTokenSource IsNot Nothing Then
            My.Application.TaskCancellationTokenSource.Cancel()
        End If
    End Sub

    Private Sub ToolStripButton13_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ToolStripButton13.Click
        If My.Computer.Keyboard.ShiftKeyDown Then
            CalculateAll(Me)
        Else
            CalculateAll2(Me, My.Settings.SolverMode)
        End If
    End Sub

    Private Sub ToolStripButton15_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ToolStripButton15.Click
        Me.CalculationQueue.Clear()
    End Sub

    Private Sub AnáliseDeSensibilidadeToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles AnáliseDeSensibilidadeToolStripMenuItem.Click
        Me.FormSensAnalysis0 = New FormSensAnalysis
        Me.FormSensAnalysis0.Show(Me.dckPanel)
    End Sub

    Private Sub MultivariateOptimizerToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MultivariateOptimizerToolStripMenuItem.Click
        Me.FormOptimization0 = New FormOptimization
        Me.FormOptimization0.Show(Me.dckPanel)
    End Sub

    Private Sub GerenciadorDeReaçõesToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles GerenciadorDeReaçõesToolStripMenuItem.Click
        Dim rm As New FormReacManager
        rm.Show(Me.dckPanel)
    End Sub

    Private Sub CaracterizaçãoDePetróleosFraçõesC7ToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles CaracterizaçãoDePetróleosFraçõesC7ToolStripMenuItem.Click
        Me.FrmPCBulk.ShowDialog(Me)
    End Sub

    Private Sub CaracterizaçãoDePetróleosCurvasDeDestilaçãoToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles CaracterizaçãoDePetróleosCurvasDeDestilaçãoToolStripMenuItem.Click
        Dim frmdc As New DCCharacterizationWizard
        frmdc.ShowDialog(Me)
    End Sub

    Private Sub ToolStripComboBoxNumberFormatting_SelectedIndexChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles ToolStripComboBoxNumberFormatting.SelectedIndexChanged
        Me.Options.NumberFormat = Me.ToolStripComboBoxNumberFormatting.SelectedItem
        Try
            Me.FormSurface.UpdateSelectedObject()
        Catch ex As Exception

        End Try
    End Sub

    Private Sub ToolStripComboBoxNumberFractionFormatting_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ToolStripComboBoxNumberFractionFormatting.SelectedIndexChanged
        Me.Options.FractionNumberFormat = Me.ToolStripComboBoxNumberFractionFormatting.SelectedItem
        Try
            Me.FormSurface.UpdateSelectedObject()
        Catch ex As Exception

        End Try
    End Sub

    Private Sub ToolStripComboBoxUnitSystem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ToolStripComboBoxUnitSystem.SelectedIndexChanged

        Try

            If FormMain.AvailableUnitSystems.ContainsKey(Me.ToolStripComboBoxUnitSystem.SelectedItem.ToString) Then
                Me.Options.SelectedUnitSystem = FormMain.AvailableUnitSystems.Item(Me.ToolStripComboBoxUnitSystem.SelectedItem.ToString)
            End If

            Me.FormSurface.UpdateSelectedObject()

            For Each o In Collections.FlowsheetObjectCollection.Values
                o.UpdatePropertyNodes(Me.Options.SelectedUnitSystem, Me.Options.NumberFormat)
            Next

        Catch ex As Exception

        End Try

    End Sub

    Private Sub ToolStripButton7_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ToolStripButton7.Click
        Dim frmUnit As New FormUnitGen
        frmUnit.ShowDialog(Me)
    End Sub

    Private Sub IronRubyToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles IronRubyToolStripMenuItem.Click
        Dim fs As New FormScript
        fs.fc = Me
        fs.Show(Me.dckPanel)
    End Sub

    Private Sub ExibirSaídaDoConsoleToolStripMenuItem_CheckedChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles consoletsmi.CheckedChanged
        If consoletsmi.Checked Then
            FormOutput.Show(dckPanel)
        Else
            FormOutput.Hide()
        End If
        Me.Options.FlowsheetShowConsoleWindow = consoletsmi.Checked
    End Sub

    Private Sub ExibirListaDeItensACalcularToolStripMenuItem_CheckedChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles ExibirListaDeItensACalcularToolStripMenuItem.CheckedChanged
        If ExibirListaDeItensACalcularToolStripMenuItem.Checked Then
            FormQueue.Show(dckPanel)
        Else
            FormQueue.Hide()
        End If
        Me.Options.FlowsheetShowCalculationQueue = ExibirListaDeItensACalcularToolStripMenuItem.Checked
    End Sub

    Private Sub ExibirRelatóriosDosObjetosCAPEOPENToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles COObjTSMI.CheckedChanged
        If COObjTSMI.Checked Then
            FormCOReports.Show(dckPanel)
        Else
            FormCOReports.Hide()
        End If
        Me.Options.FlowsheetShowCOReportsWindow = COObjTSMI.Checked
    End Sub

    Private Sub PainelDeVariáveisToolStripMenuItem_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles varpaneltsmi.CheckedChanged
        If varpaneltsmi.Checked Then
            FormWatch.Show(dckPanel)
        Else
            FormWatch.Hide()
        End If
        Me.Options.FlowsheetShowWatchWindow = varpaneltsmi.Checked
    End Sub

    Private Sub ToolStripButton16_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ToolStripButton16.CheckStateChanged
        Me.FormSurface.FlowsheetDesignSurface.SnapToGrid = ToolStripButton16.Checked
        Me.Options.FlowsheetSnapToGrid = ToolStripButton16.Checked
    End Sub

    Private Sub ToolStripButton17_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ToolStripButton17.CheckStateChanged
        Me.FormSurface.FlowsheetDesignSurface.QuickConnect = ToolStripButton17.Checked
        Me.Options.FlowsheetQuickConnect = ToolStripButton17.Checked
    End Sub
    Private Sub ToolStripButton2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ToolStripButton2.Click
        Me.FormSurface.FlowsheetDesignSurface.Zoom += 0.05
        Me.TSTBZoom.Text = Format(Me.FormSurface.FlowsheetDesignSurface.Zoom, "#%")
        Me.FormSurface.FlowsheetDesignSurface.Invalidate()
    End Sub

    Private Sub ToolStripButton1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ToolStripButton1.Click
        Me.FormSurface.FlowsheetDesignSurface.Zoom -= 0.05
        Me.TSTBZoom.Text = Format(Me.FormSurface.FlowsheetDesignSurface.Zoom, "#%")
        Me.FormSurface.FlowsheetDesignSurface.Invalidate()
    End Sub

    Private Sub SimulationConfig_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles tsmiConfigSimulation.Click
        If DWSIM.App.IsRunningOnMono Then
            Me.FrmStSim1 = New FormSimulSettings()
            Me.FrmStSim1.Show(Me.dckPanel)
        Else
            Me.FrmStSim1.Show(Me.dckPanel)
        End If
    End Sub

    Private Sub ToolStripButton5_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ToolStripButton5.Click
        Call Me.SimulationConfig_Click(sender, e)
    End Sub

    Private Sub ToolStripButton8_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ToolStripButton8.Click
        Call Me.GerarRelatórioToolStripMenuItem_Click(sender, e)
    End Sub

    Private Sub GerarRelatórioToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles GerarRelatórioToolStripMenuItem.Click
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

    Private Sub ToolStripButton18_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ToolStripButton18.Click

        If Me.SaveFileDialog1.ShowDialog = Windows.Forms.DialogResult.OK Then
            Dim rect As Rectangle = New Rectangle(0, 0, Me.FormSurface.FlowsheetDesignSurface.Width - 14, Me.FormSurface.FlowsheetDesignSurface.Height - 14)
            Dim img As Image = New Bitmap(rect.Width, rect.Height)
            Me.FormSurface.FlowsheetDesignSurface.DrawToBitmap(img, Me.FormSurface.FlowsheetDesignSurface.Bounds)
            img.Save(Me.SaveFileDialog1.FileName, Imaging.ImageFormat.Png)
            img.Dispose()
        End If

    End Sub

    Private Sub ToolStripButton20_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles ToolStripButton20.Click
        Me.FormSurface.FlowsheetDesignSurface.ZoomAll()
        Application.DoEvents()
        Me.FormSurface.FlowsheetDesignSurface.ZoomAll()
        Application.DoEvents()
        Me.TSTBZoom.Text = Format(Me.FormSurface.FlowsheetDesignSurface.Zoom, "#%")
    End Sub

    Private Sub ToolStripButton3_Click(sender As System.Object, e As System.EventArgs) Handles ToolStripButton3.Click
        Me.FormSurface.FlowsheetDesignSurface.Zoom = 1
        Me.TSTBZoom.Text = Format(Me.FormSurface.FlowsheetDesignSurface.Zoom, "#%")
        Me.FormSurface.FlowsheetDesignSurface.Invalidate()
    End Sub

    Private Sub TSTBZoom_KeyPress(ByVal sender As System.Object, ByVal e As System.Windows.Forms.KeyEventArgs) Handles TSTBZoom.KeyDown
        If e.KeyCode = Keys.Enter Then
            Me.FormSurface.FlowsheetDesignSurface.Zoom = Convert.ToInt32(Me.TSTBZoom.Text.Replace("%", "")) / 100
            Me.TSTBZoom.Text = Format(Me.FormSurface.FlowsheetDesignSurface.Zoom, "#%")
            Me.FormSurface.FlowsheetDesignSurface.Invalidate()
        End If
    End Sub


    Private Sub GerenciadorDeAmostrasDePetróleoToolStripMenuItem_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles GerenciadorDeAmostrasDePetróleoToolStripMenuItem.Click
        Dim frmam As New FormAssayManager
        frmam.ShowDialog(Me)
        Try
            frmam.Close()
        Catch ex As Exception

        End Try
    End Sub

    Private Sub ToolStripButton21_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles tsbSaveState.Click

        FormMain.SaveState(Me)

    End Sub

    Private Sub RestoreState_ItemClick(ByVal sender As System.Object, ByVal e As System.EventArgs)

        Dim tsmi As ToolStripMenuItem = sender

        FormMain.RestoreState(Me, FlowsheetStates(tsmi.Tag))

    End Sub

    Private Sub RemoveState_ItemClick(ByVal sender As System.Object, ByVal e As System.EventArgs)

        Dim tsmi As ToolStripMenuItem = sender

        FlowsheetStates.Remove(tsmi.Tag)

        UpdateStateList()

    End Sub

    Private Sub RestoreSolution_ItemClick(ByVal sender As System.Object, ByVal e As System.EventArgs)

        Dim tsmi As ToolStripMenuItem = sender

        Dim solutionkey As String = tsmi.Tag

        Using ms As New MemoryStream(Me.PreviousSolutions(solutionkey).Solution)
            Using decompressedstream As New IO.MemoryStream
                Using gzs As New IO.BufferedStream(New Compression.GZipStream(ms, Compression.CompressionMode.Decompress, True), 64 * 1024)
                    gzs.CopyTo(decompressedstream)
                    gzs.Close()
                    Me.WriteToLog(DWSIM.App.GetLocalString("ClientUpdatingData") & " " & Math.Round(decompressedstream.Length / 1024).ToString & " KB", Color.Brown, MessageType.Information)
                    decompressedstream.Position = 0
                    Dim xdoc As XDocument = XDocument.Load(decompressedstream)
                    DWSIM.SimulationObjects.UnitOperations.Flowsheet.UpdateProcessData(Me, xdoc)
                    DWSIM.Flowsheet.FlowsheetSolver.UpdateDisplayStatus(Me)
                    Me.WriteToLog(DWSIM.App.GetLocalString("ClientUpdatedDataOK"), Color.Brown, MessageType.Information)
                End Using
            End Using
        End Using

    End Sub

    Sub UpdateSolutionsList()

        With Me.tsbRestoreSolutions.DropDownItems

            .Clear()

            While Me.PreviousSolutions.Count > 15
                Dim idtoremove As String = ""
                For Each s In Me.PreviousSolutions.Values
                    idtoremove = s.ID
                    Exit For
                Next
                If Me.PreviousSolutions.ContainsKey(idtoremove) Then Me.PreviousSolutions.Remove(idtoremove)
            End While

            For Each k As Long In Me.PreviousSolutions.Keys

                Dim tsmi As ToolStripMenuItem = .Add(Me.PreviousSolutions(k).SaveDate.ToString)
                tsmi.Tag = k
                AddHandler tsmi.Click, AddressOf RestoreSolution_ItemClick

            Next

        End With

    End Sub

    Sub UpdateStateList()

        With Me.tsbRestoreStates.DropDownItems

            .Clear()

            For Each k As Date In Me.FlowsheetStates.Keys

                Dim tsmi As ToolStripMenuItem = .Add(Me.FlowsheetStates(k).Description & " (" & k.ToString & ")")
                tsmi.Tag = k
                tsmi.Image = Me.FlowsheetStates(k).Snapshot
                AddHandler tsmi.Click, AddressOf RestoreState_ItemClick

                Dim tsmiR As ToolStripMenuItem = tsmi.DropDownItems.Add(DWSIM.App.GetLocalString("RestoreState"))
                tsmiR.Tag = k
                tsmiR.Image = My.Resources.arrow_in
                AddHandler tsmiR.Click, AddressOf RestoreState_ItemClick

                Dim tsmiE As ToolStripMenuItem = tsmi.DropDownItems.Add(DWSIM.App.GetLocalString("Excluir"))
                tsmiE.Tag = k
                tsmiE.Image = My.Resources.cross
                AddHandler tsmiE.Click, AddressOf RemoveState_ItemClick

            Next

        End With

    End Sub

    Private Sub ToolStripButton21_Click_1(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles tsbClearStates.Click

        If Not Me.FlowsheetStates Is Nothing Then
            Me.FlowsheetStates.Clear()
            UpdateStateList()
        End If

    End Sub

    Private Sub tsbSimultAdjustSolver_CheckedChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles tsbSimultAdjustSolver.CheckedChanged
        m_simultadjustsolverenabled = tsbSimultAdjustSolver.Checked
    End Sub

    Sub ChangeEditMenuStatus(status As Boolean)

        tsmiCut.Enabled = status
        tsmiCopy.Enabled = status
        tsmiPaste.Enabled = status
        tsmiRecalc.Enabled = status
        tsmiCloneSelected.Enabled = status
        tsmiRemoveSelected.Enabled = status
        tsmiExportData.Enabled = status
        tsbCutObj.Enabled = status
        tsbCopyObj.Enabled = status
        tsbPasteObj.Enabled = status

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
                    ElseIf SelectedObj.ObjectType = ObjectType.GO_Table Then
                        MessageBox.Show(DWSIM.App.GetLocalString("Atabelapodeseroculta") & vbCrLf & DWSIM.App.GetLocalString("doobjetoqualelaperte"), DWSIM.App.GetLocalString("Nopossvelexcluirtabe"), MessageBoxButtons.OK, MessageBoxIcon.Information)
                    ElseIf SelectedObj.ObjectType = ObjectType.GO_Text Then
                        msgresult = MessageBox.Show(DWSIM.App.GetLocalString("Excluiracaixadetexto"), DWSIM.App.GetLocalString("Excluirobjeto"), MessageBoxButtons.YesNo, MessageBoxIcon.Question)
                    Else
                        msgresult = MessageBox.Show(DWSIM.App.GetLocalString("Excluir") & gobj.Tag & "?", DWSIM.App.GetLocalString("Excluirobjeto"), MessageBoxButtons.YesNo, MessageBoxIcon.Question)
                    End If
                Else
                    msgresult = MsgBoxResult.Yes
                End If
                If msgresult = MsgBoxResult.Yes Then

                    'remove object property table, if it exists
                    Dim tables As List(Of GraphicObject) = (From t As GraphicObject In Me.FormSurface.FlowsheetDesignSurface.drawingObjects
                                                                      Select t Where t.ObjectType = ObjectType.GO_Table).ToList
                    Dim tablelist As List(Of TableGraphic) = (From t As TableGraphic In tables
                                                                      Select t Where t.BaseOwner.Name = gobj.Name).ToList
                    If Not tablelist Is Nothing Then
                        For Each table As TableGraphic In tablelist
                            Try
                                Me.FormSurface.FlowsheetDesignSurface.DeleteSelectedObject(table)
                            Catch ex As Exception

                            End Try
                        Next
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

                        Me.Collections.EnergyStreamCollection.Remove(namesel)
                        'DWSIM
                        Me.Collections.CLCS_EnergyStreamCollection(namesel).Dispose()
                        Me.Collections.CLCS_EnergyStreamCollection.Remove(namesel)
                        Me.Collections.FlowsheetObjectCollection.Remove(namesel)
                        Me.Collections.FlowsheetObjectCollection.Remove(namesel)
                        Me.FormSurface.FlowsheetDesignSurface.DeleteSelectedObject(gobj)

                    Else

                        If SelectedObj.ObjectType = ObjectType.GO_Image Then
                            Me.FormSurface.FlowsheetDesignSurface.DeleteSelectedObject(gobj)
                        ElseIf SelectedObj.ObjectType = ObjectType.GO_Table Then
                            'Me.FormSurface.FlowsheetDesignSurface.DeleteSelectedObject(gobj)
                        ElseIf SelectedObj.ObjectType = ObjectType.GO_MasterTable Then
                            Me.FormSurface.FlowsheetDesignSurface.DeleteSelectedObject(gobj)
                        ElseIf SelectedObj.ObjectType = ObjectType.GO_Text Then
                            Me.FormSurface.FlowsheetDesignSurface.DeleteSelectedObject(gobj)
                        ElseIf SelectedObj.ObjectType = ObjectType.GO_FloatingTable Then
                            Me.FormSurface.FlowsheetDesignSurface.DeleteSelectedObject(gobj)
                        ElseIf SelectedObj.ObjectType = ObjectType.GO_SpreadsheetTable Then
                            Me.FormSurface.FlowsheetDesignSurface.DeleteSelectedObject(gobj)
                        Else

                            Dim obj As DWSIM.SimulationObjects.UnitOperations.BaseClass = Me.Collections.FlowsheetObjectCollection(SelectedObj.Name)
                            DeCalculateDisconnectedObject(Me, SelectedObj, "Out")

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

                            'dispose object
                            Me.Collections.FlowsheetObjectCollection(namesel).Dispose()

                            Select Case SelectedObj.ObjectType
                                Case ObjectType.NodeIn
                                    Me.Collections.MixerCollection.Remove(namesel)
                                    'DWSIM
                                    Me.Collections.CLCS_MixerCollection.Remove(namesel)
                                    Me.Collections.FlowsheetObjectCollection.Remove(namesel)
                                Case ObjectType.NodeOut
                                    Me.Collections.SplitterCollection.Remove(namesel)
                                    'DWSIM
                                    Me.Collections.CLCS_SplitterCollection.Remove(namesel)
                                    Me.Collections.FlowsheetObjectCollection.Remove(namesel)
                                Case ObjectType.NodeEn
                                    Me.Collections.MixerENCollection.Remove(namesel)
                                    'DWSIM
                                    Me.Collections.CLCS_EnergyMixerCollection.Remove(namesel)
                                    Me.Collections.FlowsheetObjectCollection.Remove(namesel)
                                Case ObjectType.Pump
                                    Me.Collections.PumpCollection.Remove(namesel)
                                    'DWSIM
                                    Me.Collections.CLCS_PumpCollection.Remove(namesel)
                                    Me.Collections.FlowsheetObjectCollection.Remove(namesel)
                                Case ObjectType.Tank
                                    Me.Collections.TankCollection.Remove(namesel)
                                    'DWSIM
                                    Me.Collections.CLCS_TankCollection.Remove(namesel)
                                    Me.Collections.FlowsheetObjectCollection.Remove(namesel)
                                Case ObjectType.Vessel
                                    Me.Collections.SeparatorCollection.Remove(namesel)
                                    'DWSIM
                                    Me.Collections.CLCS_VesselCollection.Remove(namesel)
                                    Me.Collections.FlowsheetObjectCollection.Remove(namesel)
                                Case ObjectType.MaterialStream
                                    Me.Collections.MaterialStreamCollection.Remove(namesel)
                                    'DWSIM
                                    Me.Collections.CLCS_MaterialStreamCollection.Remove(namesel)
                                    Me.Collections.FlowsheetObjectCollection.Remove(namesel)
                                Case ObjectType.Compressor
                                    Me.Collections.CompressorCollection.Remove(namesel)
                                    'DWSIM
                                    Me.Collections.CLCS_CompressorCollection.Remove(namesel)
                                    Me.Collections.FlowsheetObjectCollection.Remove(namesel)
                                Case ObjectType.Expander
                                    Me.Collections.TurbineCollection.Remove(namesel)
                                    'DWSIM
                                    Me.Collections.CLCS_TurbineCollection.Remove(namesel)
                                    Me.Collections.FlowsheetObjectCollection.Remove(namesel)
                                Case ObjectType.TPVessel
                                    Me.Collections.TPSeparatorCollection.Remove(namesel)
                                Case ObjectType.Cooler
                                    Me.Collections.CoolerCollection.Remove(namesel)
                                    'DWSIM
                                    Me.Collections.CLCS_CoolerCollection.Remove(namesel)
                                    Me.Collections.FlowsheetObjectCollection.Remove(namesel)
                                Case ObjectType.Heater
                                    Me.Collections.HeaterCollection.Remove(namesel)
                                    'DWSIM
                                    Me.Collections.CLCS_HeaterCollection.Remove(namesel)
                                    Me.Collections.FlowsheetObjectCollection.Remove(namesel)
                                Case ObjectType.Pipe
                                    Me.Collections.CLCS_PipeCollection.Remove(namesel)
                                    'DWSIM
                                    Me.Collections.PipeCollection.Remove(namesel)
                                    Me.Collections.FlowsheetObjectCollection.Remove(namesel)
                                Case ObjectType.Valve
                                    Me.Collections.ValveCollection.Remove(namesel)
                                    'DWSIM
                                    Me.Collections.CLCS_ValveCollection.Remove(namesel)
                                    Me.Collections.FlowsheetObjectCollection.Remove(namesel)
                                Case ObjectType.OT_Adjust
                                    Me.Collections.AdjustCollection.Remove(namesel)
                                    'DWSIM
                                    Me.Collections.CLCS_AdjustCollection.Remove(namesel)
                                    Me.Collections.FlowsheetObjectCollection.Remove(namesel)
                                Case ObjectType.OT_Spec
                                    If Me.Collections.FlowsheetObjectCollection.ContainsKey(Me.Collections.CLCS_SpecCollection(namesel).TargetObjectData.m_ID) Then
                                        Me.Collections.FlowsheetObjectCollection(Me.Collections.CLCS_SpecCollection(namesel).TargetObjectData.m_ID).IsSpecAttached = False
                                        Me.Collections.FlowsheetObjectCollection(Me.Collections.CLCS_SpecCollection(namesel).TargetObjectData.m_ID).AttachedSpecId = ""
                                    End If
                                    If Me.Collections.FlowsheetObjectCollection.ContainsKey(Me.Collections.CLCS_SpecCollection(namesel).SourceObjectData.m_ID) Then
                                        Me.Collections.FlowsheetObjectCollection(Me.Collections.CLCS_SpecCollection(namesel).SourceObjectData.m_ID).IsSpecAttached = False
                                        Me.Collections.FlowsheetObjectCollection(Me.Collections.CLCS_SpecCollection(namesel).SourceObjectData.m_ID).AttachedSpecId = ""
                                    End If
                                    Me.Collections.SpecCollection.Remove(namesel)
                                    'DWSIM
                                    Me.Collections.CLCS_SpecCollection.Remove(namesel)
                                    Me.Collections.FlowsheetObjectCollection.Remove(namesel)
                                Case ObjectType.OT_Recycle
                                    Me.Collections.RecycleCollection.Remove(namesel)
                                    'DWSIM
                                    Me.Collections.CLCS_RecycleCollection.Remove(namesel)
                                    Me.Collections.FlowsheetObjectCollection.Remove(namesel)
                                Case ObjectType.OT_EnergyRecycle
                                    Me.Collections.EnergyRecycleCollection.Remove(namesel)
                                    'DWSIM
                                    Me.Collections.CLCS_EnergyRecycleCollection.Remove(namesel)
                                    Me.Collections.FlowsheetObjectCollection.Remove(namesel)
                                Case ObjectType.HeatExchanger
                                    Me.Collections.HeatExchangerCollection.Remove(namesel)
                                    'DWSIM
                                    Me.Collections.CLCS_HeatExchangerCollection.Remove(namesel)
                                    Me.Collections.FlowsheetObjectCollection.Remove(namesel)
                                Case ObjectType.ShortcutColumn
                                    Me.Collections.HeatExchangerCollection.Remove(namesel)
                                    'DWSIM
                                    Me.Collections.CLCS_HeatExchangerCollection.Remove(namesel)
                                    Me.Collections.FlowsheetObjectCollection.Remove(namesel)
                                Case ObjectType.OrificePlate
                                    Me.Collections.OrificePlateCollection.Remove(namesel)
                                    'DWSIM
                                    Me.Collections.CLCS_OrificePlateCollection.Remove(namesel)
                                    Me.Collections.FlowsheetObjectCollection.Remove(namesel)
                                Case ObjectType.ComponentSeparator
                                    Me.Collections.ComponentSeparatorCollection.Remove(namesel)
                                    'DWSIM
                                    Me.Collections.CLCS_ComponentSeparatorCollection.Remove(namesel)
                                    Me.Collections.FlowsheetObjectCollection.Remove(namesel)
                                Case ObjectType.CustomUO
                                    Me.Collections.CustomUOCollection.Remove(namesel)
                                    'DWSIM
                                    Me.Collections.CLCS_CustomUOCollection.Remove(namesel)
                                    Me.Collections.FlowsheetObjectCollection.Remove(namesel)
                                Case ObjectType.ExcelUO
                                    Me.Collections.ExcelUOCollection.Remove(namesel)
                                    'DWSIM
                                    Me.Collections.CLCS_ExcelUOCollection.Remove(namesel)
                                    Me.Collections.FlowsheetObjectCollection.Remove(namesel)
                                    Me.Collections.FlowsheetObjectCollection.Remove(namesel)
                                Case ObjectType.FlowsheetUO
                                    Me.Collections.FlowsheetUOCollection.Remove(namesel)
                                    'DWSIM
                                    Me.Collections.CLCS_FlowsheetUOCollection.Remove(namesel)
                                    Me.Collections.FlowsheetObjectCollection.Remove(namesel)
                                Case ObjectType.CapeOpenUO
                                    Me.Collections.CapeOpenUOCollection.Remove(namesel)
                                    'DWSIM
                                    Me.Collections.CLCS_CapeOpenUOCollection.Remove(namesel)
                                    Me.Collections.FlowsheetObjectCollection.Remove(namesel)
                                Case ObjectType.SolidSeparator
                                    Me.Collections.SolidsSeparatorCollection.Remove(namesel)
                                    'DWSIM
                                    Me.Collections.CLCS_SolidsSeparatorCollection.Remove(namesel)
                                    Me.Collections.FlowsheetObjectCollection.Remove(namesel)
                                Case ObjectType.Filter
                                    Me.Collections.FilterCollection.Remove(namesel)
                                    'DWSIM
                                    Me.Collections.CLCS_FilterCollection.Remove(namesel)
                                    Me.Collections.FlowsheetObjectCollection.Remove(namesel)
                                Case ObjectType.RCT_Conversion
                                    Me.Collections.ReactorConversionCollection.Remove(namesel)
                                    'DWSIM
                                    Me.Collections.CLCS_ReactorConversionCollection.Remove(namesel)
                                    Me.Collections.FlowsheetObjectCollection.Remove(namesel)
                                Case ObjectType.RCT_CSTR
                                    Me.Collections.ReactorCSTRCollection.Remove(namesel)
                                    'DWSIM
                                    Me.Collections.CLCS_ReactorCSTRCollection.Remove(namesel)
                                    Me.Collections.FlowsheetObjectCollection.Remove(namesel)
                                Case ObjectType.RCT_Equilibrium
                                    Me.Collections.ReactorEquilibriumCollection.Remove(namesel)
                                    'DWSIM
                                    Me.Collections.CLCS_ReactorEquilibriumCollection.Remove(namesel)
                                    Me.Collections.FlowsheetObjectCollection.Remove(namesel)
                                Case ObjectType.RCT_Gibbs
                                    Me.Collections.ReactorGibbsCollection.Remove(namesel)
                                    'DWSIM
                                    Me.Collections.CLCS_ReactorGibbsCollection.Remove(namesel)
                                    Me.Collections.FlowsheetObjectCollection.Remove(namesel)
                                Case ObjectType.RCT_PFR
                                    Me.Collections.ReactorPFRCollection.Remove(namesel)
                                    'DWSIM
                                    Me.Collections.CLCS_ReactorPFRCollection.Remove(namesel)
                                    Me.Collections.FlowsheetObjectCollection.Remove(namesel)
                                Case ObjectType.DistillationColumn
                                    Me.Collections.DistillationColumnCollection.Remove(namesel)
                                    'DWSIM
                                    Me.Collections.CLCS_DistillationColumnCollection.Remove(namesel)
                                    Me.Collections.FlowsheetObjectCollection.Remove(namesel)
                                Case ObjectType.AbsorptionColumn
                                    Me.Collections.AbsorptionColumnCollection.Remove(namesel)
                                    'DWSIM
                                    Me.Collections.CLCS_AbsorptionColumnCollection.Remove(namesel)
                                    Me.Collections.FlowsheetObjectCollection.Remove(namesel)
                                Case ObjectType.RefluxedAbsorber
                                    Me.Collections.RefluxedAbsorberCollection.Remove(namesel)
                                    'DWSIM
                                    Me.Collections.CLCS_RefluxedAbsorberCollection.Remove(namesel)
                                    Me.Collections.FlowsheetObjectCollection.Remove(namesel)
                                Case ObjectType.ReboiledAbsorber
                                    Me.Collections.ReboiledAbsorberCollection.Remove(namesel)
                                    'DWSIM
                                    Me.Collections.CLCS_ReboiledAbsorberCollection.Remove(namesel)
                                    Me.Collections.FlowsheetObjectCollection.Remove(namesel)
                            End Select

                            Me.FormSurface.FlowsheetDesignSurface.DeleteSelectedObject(gobj)

                        End If

                    End If
                End If
            End If

            If triggercalc Then ProcessCalculationQueue(Me, Nothing, False, False) Else Me.CalculationQueue.Clear()

        End If
    End Sub

    Public Sub DeleteObject(ByVal tag As String, Optional ByVal confirmation As Boolean = True)

        Dim gobj As GraphicObject = Me.GetFlowsheetGraphicObject(tag)

        If Not gobj Is Nothing Then
            Me.FormSurface.FlowsheetDesignSurface.SelectedObject = gobj
            Me.DeleteSelectedObject(Me, New EventArgs(), gobj, confirmation)
        End If

    End Sub

    Public Sub DisconnectObject(ByRef gObjFrom As GraphicObject, ByRef gObjTo As GraphicObject, Optional ByVal triggercalc As Boolean = False)

        Me.WriteToLog(DWSIM.App.GetLocalTipString("FLSH007"), Color.Black, DWSIM.Flowsheet.MessageType.Tip)

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
                            DeCalculateDisconnectedObject(Me, SelObj, "In")
                            conptObj.AttachedConnector.AttachedFrom.OutputConnectors(conptObj.AttachedConnector.AttachedFromConnectorIndex).IsAttached = False
                            conptObj.AttachedConnector.AttachedFrom.OutputConnectors(conptObj.AttachedConnector.AttachedFromConnectorIndex).AttachedConnector = Nothing
                            Me.FormSurface.FlowsheetDesignSurface.SelectedObjects.Clear()
                            conptObj.IsAttached = False
                            Me.FormSurface.FlowsheetDesignSurface.DeleteSelectedObject(conptObj.AttachedConnector)
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
                            DeCalculateDisconnectedObject(Me, SelObj, "Out")
                            conptObj.AttachedConnector.AttachedTo.InputConnectors(conptObj.AttachedConnector.AttachedToConnectorIndex).IsAttached = False
                            conptObj.AttachedConnector.AttachedTo.InputConnectors(conptObj.AttachedConnector.AttachedToConnectorIndex).AttachedConnector = Nothing
                            conptObj.IsAttached = False
                            Me.FormSurface.FlowsheetDesignSurface.DeleteSelectedObject(conptObj.AttachedConnector)
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
                    DeCalculateDisconnectedObject(Me, SelObj, "Out")
                    SelObj.EnergyConnector.AttachedConnector.AttachedTo.InputConnectors(SelObj.EnergyConnector.AttachedConnector.AttachedToConnectorIndex).IsAttached = False
                    SelObj.EnergyConnector.AttachedConnector.AttachedTo.InputConnectors(SelObj.EnergyConnector.AttachedConnector.AttachedToConnectorIndex).AttachedConnector = Nothing
                    SelObj.EnergyConnector.IsAttached = False
                    Me.FormSurface.FlowsheetDesignSurface.DeleteSelectedObject(SelObj.EnergyConnector.AttachedConnector)
                End If
            End If
        End If

        If My.Application.PushUndoRedoAction Then AddUndoRedoAction(New UndoRedoAction() With {.AType = UndoRedoActionType.FlowsheetObjectDisconnected,
                                     .ObjID = gobj1.Name,
                                     .ObjID2 = gobj2.Name,
                                     .OldValue = i1,
                                     .NewValue = i2,
                                     .Name = String.Format(DWSIM.App.GetLocalString("UndoRedo_ObjectDisconnected"), gobj1.Tag, gobj2.Tag)})

        If triggercalc Then ProcessCalculationQueue(Me, Nothing, False, False) Else Me.CalculationQueue.Clear()

    End Sub

    Public Sub ConnectObject(ByRef gObjFrom As GraphicObject, ByRef gObjTo As GraphicObject, Optional ByVal fidx As Integer = -1, Optional ByVal tidx As Integer = -1)

        Me.WriteToLog(DWSIM.App.GetLocalTipString("FLSH007"), Color.Black, DWSIM.Flowsheet.MessageType.Tip)

        If gObjFrom.ObjectType <> ObjectType.GO_Image And gObjFrom.ObjectType <> ObjectType.GO_Table And _
        gObjFrom.ObjectType <> ObjectType.GO_Table And gObjFrom.ObjectType <> ObjectType.GO_FloatingTable And _
        gObjFrom.ObjectType <> ObjectType.Nenhum And _
        gObjTo.ObjectType <> ObjectType.GO_Image And gObjTo.ObjectType <> ObjectType.GO_Table And _
        gObjTo.ObjectType <> ObjectType.GO_Table And gObjTo.ObjectType <> ObjectType.GO_FloatingTable And _
        gObjTo.ObjectType <> ObjectType.Nenhum And gObjTo.ObjectType <> ObjectType.GO_MasterTable Then

            Dim con1OK As Boolean = False
            Dim con2OK As Boolean = False

            'posicionar pontos nos primeiros slots livres
            Dim StartPos, EndPos As New Point
            Dim InConSlot, OutConSlot As New ConnectionPoint
            If Not gObjFrom Is Nothing Then
                If Not gObjTo Is Nothing Then
                    If gObjFrom.ObjectType = ObjectType.MaterialStream And gObjTo.ObjectType = ObjectType.MaterialStream Then
                        Throw New Exception(DWSIM.App.GetLocalString("Nopossvelrealizaress"))
                    ElseIf gObjFrom.ObjectType = ObjectType.EnergyStream And gObjTo.ObjectType = ObjectType.EnergyStream Then
                        Throw New Exception(DWSIM.App.GetLocalString("Nopossvelrealizaress"))
                    ElseIf Not gObjFrom.ObjectType = ObjectType.MaterialStream And Not gObjFrom.ObjectType = ObjectType.EnergyStream Then
                        If Not gObjTo.ObjectType = ObjectType.EnergyStream And Not gObjTo.ObjectType = ObjectType.MaterialStream Then
                            Throw New Exception(DWSIM.App.GetLocalString("Nopossvelrealizaress"))
                        End If
                    ElseIf gObjFrom.ObjectType = ObjectType.MaterialStream And gObjTo.ObjectType = ObjectType.EnergyStream Then
                        Throw New Exception(DWSIM.App.GetLocalString("Nopossvelrealizaress"))
                    ElseIf gObjFrom.ObjectType = ObjectType.EnergyStream And gObjTo.ObjectType = ObjectType.MaterialStream Then
                        Throw New Exception(DWSIM.App.GetLocalString("Nopossvelrealizaress"))
                    End If
                    If gObjTo.IsEnergyStream = False Then
                        If Not gObjFrom.IsEnergyStream Then
                            If tidx = -1 Then
                                For Each InConSlot In gObjTo.InputConnectors
                                    If Not InConSlot.IsAttached And InConSlot.Type = ConType.ConIn Then
                                        EndPos = InConSlot.Position
                                        InConSlot.IsAttached = True
                                        con2OK = True
                                        Exit For
                                    End If
                                Next
                            Else
                                If Not gObjTo.InputConnectors(tidx).IsAttached And gObjTo.InputConnectors(tidx).Type = ConType.ConIn Then
                                    InConSlot = gObjTo.InputConnectors(tidx)
                                    EndPos = InConSlot.Position
                                    InConSlot.IsAttached = True
                                    con2OK = True
                                End If
                            End If
                        Else
                            If tidx = -1 Then
                                For Each InConSlot In gObjTo.InputConnectors
                                    If Not InConSlot.IsAttached And InConSlot.Type = ConType.ConEn Then
                                        EndPos = InConSlot.Position
                                        InConSlot.IsAttached = True
                                        con2OK = True
                                        Exit For
                                    End If
                                Next
                            Else
                                If Not gObjTo.InputConnectors(tidx).IsAttached And gObjTo.InputConnectors(tidx).Type = ConType.ConEn Then
                                    InConSlot = gObjTo.InputConnectors(tidx)
                                    EndPos = InConSlot.Position
                                    InConSlot.IsAttached = True
                                    con2OK = True
                                End If
                            End If
                            If Not con2OK Then
                                Throw New Exception(DWSIM.App.GetLocalString("CorrentesdeEnergyFlowsp"))
                                Exit Sub
                            End If
                        End If
                        If fidx = -1 Then
                            For Each OutConSlot In gObjFrom.OutputConnectors
                                If Not OutConSlot.IsAttached Then
                                    StartPos = OutConSlot.Position
                                    OutConSlot.IsAttached = True
                                    If con2OK Then con1OK = True
                                    Exit For
                                End If
                            Next
                        Else
                            If Not gObjFrom.OutputConnectors(fidx).IsAttached Then
                                OutConSlot = gObjFrom.OutputConnectors(fidx)
                                StartPos = OutConSlot.Position
                                OutConSlot.IsAttached = True
                                If con2OK Then con1OK = True
                            End If
                        End If
                    Else
                        Select Case gObjFrom.ObjectType
                            Case ObjectType.Cooler, ObjectType.Pipe, ObjectType.Expander, ObjectType.ShortcutColumn, ObjectType.DistillationColumn, ObjectType.AbsorptionColumn,
                                ObjectType.ReboiledAbsorber, ObjectType.RefluxedAbsorber, ObjectType.OT_EnergyRecycle, ObjectType.ComponentSeparator, ObjectType.SolidSeparator,
                                ObjectType.Filter, ObjectType.CustomUO, ObjectType.CapeOpenUO, ObjectType.FlowsheetUO
                                GoTo 100
                            Case Else
                                Throw New Exception(DWSIM.App.GetLocalString("CorrentesdeEnergyFlowsp2") & DWSIM.App.GetLocalString("TubulaesTurbinaseRes"))
                        End Select
100:                    If gObjFrom.ObjectType <> ObjectType.CapeOpenUO And gObjFrom.ObjectType <> ObjectType.CustomUO And gObjFrom.ObjectType <> ObjectType.DistillationColumn _
                            And gObjFrom.ObjectType <> ObjectType.AbsorptionColumn And gObjFrom.ObjectType <> ObjectType.OT_EnergyRecycle _
                            And gObjFrom.ObjectType <> ObjectType.RefluxedAbsorber And gObjFrom.ObjectType <> ObjectType.ReboiledAbsorber Then
                            If Not gObjFrom.EnergyConnector.IsAttached Then
                                StartPos = gObjFrom.EnergyConnector.Position
                                gObjFrom.EnergyConnector.IsAttached = True
                                con1OK = True
                                OutConSlot = gObjFrom.EnergyConnector
                                EndPos = gObjTo.InputConnectors(0).Position
                                gObjTo.InputConnectors(0).IsAttached = True
                                con2OK = True
                                InConSlot = gObjTo.InputConnectors(0)
                            End If
                        Else
                            If tidx = -1 Then
                                For Each InConSlot In gObjTo.InputConnectors
                                    If Not InConSlot.IsAttached And InConSlot.Type = ConType.ConIn Then
                                        EndPos = InConSlot.Position
                                        InConSlot.IsAttached = True
                                        con2OK = True
                                        Exit For
                                    End If
                                Next
                            Else
                                If Not gObjTo.InputConnectors(tidx).IsAttached And gObjTo.InputConnectors(tidx).Type = ConType.ConIn Then
                                    InConSlot = gObjTo.InputConnectors(tidx)
                                    EndPos = InConSlot.Position
                                    InConSlot.IsAttached = True
                                    con2OK = True
                                End If
                            End If
                            If fidx = -1 Then
                                For Each OutConSlot In gObjFrom.OutputConnectors
                                    If Not OutConSlot.IsAttached And OutConSlot.Type = ConType.ConEn Then
                                        StartPos = OutConSlot.Position
                                        OutConSlot.IsAttached = True
                                        If con2OK Then con1OK = True
                                        Exit For
                                    End If
                                Next
                            Else
                                If Not gObjFrom.OutputConnectors(fidx).IsAttached Then
                                    OutConSlot = gObjFrom.OutputConnectors(fidx)
                                    StartPos = OutConSlot.Position
                                    OutConSlot.IsAttached = True
                                    If con2OK Then con1OK = True
                                End If
                            End If
                        End If
                    End If
                Else
                    Me.WriteToLog(DWSIM.App.GetLocalString("Nohobjetosaseremcone"), Color.Blue, MessageType.Information)
                    Exit Sub
                End If
            Else
                Me.WriteToLog(DWSIM.App.GetLocalString("Nohobjetosaseremcone"), Color.Blue, MessageType.Information)
                Exit Sub
            End If
            If con1OK = True And con2OK = True Then
                'desenhar conector
                Dim myCon As New ConnectorGraphic(StartPos, EndPos, 1, Color.DarkRed)
                OutConSlot.AttachedConnector = myCon
                InConSlot.AttachedConnector = myCon
                With myCon
                    .IsConnector = True
                    .AttachedFrom = gObjFrom
                    If gObjFrom.IsEnergyStream Then
                        .AttachedFromEnergy = True
                    End If
                    .AttachedFromConnectorIndex = gObjFrom.OutputConnectors.IndexOf(OutConSlot)
                    .AttachedTo = gObjTo
                    If gObjTo.IsEnergyStream Then
                        .AttachedToEnergy = True
                    End If
                    .AttachedToConnectorIndex = gObjTo.InputConnectors.IndexOf(InConSlot)
                    If Not myCon Is Nothing Then
                        Me.FormSurface.FlowsheetDesignSurface.drawingObjects.Add(myCon)
                        Me.FormSurface.FlowsheetDesignSurface.Invalidate()
                    End If
                End With
                If My.Application.PushUndoRedoAction Then AddUndoRedoAction(New UndoRedoAction() With {.AType = UndoRedoActionType.FlowsheetObjectConnected,
                                                     .ObjID = gObjFrom.Name,
                                                     .ObjID2 = gObjTo.Name,
                                                     .OldValue = fidx,
                                                     .NewValue = tidx,
                                                     .Name = String.Format(DWSIM.App.GetLocalString("UndoRedo_ObjectConnected"), gObjFrom.Tag, gObjTo.Tag)})
            Else
                Throw New Exception(DWSIM.App.GetLocalString("Todasasconexespossve"))
            End If

        Else


        End If

    End Sub

#End Region

#Region "    Property Grid 2 Populate Functions "

    Public Function PopulatePGEx2(ByRef gobj As GraphicObject)

        If gobj.ObjectType = ObjectType.GO_Table Then

            Dim gobj2 As DWSIM.GraphicObjects.TableGraphic = CType(gobj, DWSIM.GraphicObjects.TableGraphic)

            With Me.FormProps.PGEx2

                .Item.Clear()

                .Item.Add(DWSIM.App.GetLocalString("Cor"), gobj2, "LineColor", False, DWSIM.App.GetLocalString("Formataodotexto1"), DWSIM.App.GetLocalString("Cordotextodatabela"), True)
                .Item(.Item.Count - 1).Tag2 = "LineColor"
                .Item.Add(DWSIM.App.GetLocalString("Cabealho"), gobj2, "HeaderFont", False, DWSIM.App.GetLocalString("Formataodotexto1"), DWSIM.App.GetLocalString("Fontedotextodocabeal"), True)
                .Item(.Item.Count - 1).Tag2 = "HeaderFont"
                .Item.Add(DWSIM.App.GetLocalString("Coluna1Fonte"), gobj2, "FontCol1", False, DWSIM.App.GetLocalString("Formataodotexto1"), DWSIM.App.GetLocalString("Fontedotextodacoluna"), True)
                .Item(.Item.Count - 1).Tag2 = "FontCol1"
                .Item.Add(DWSIM.App.GetLocalString("Coluna2Fonte"), gobj2, "FontCol2", False, DWSIM.App.GetLocalString("Formataodotexto1"), DWSIM.App.GetLocalString("Fontedotextodacoluna2"), True)
                .Item(.Item.Count - 1).Tag2 = "FontCol2"
                .Item.Add(DWSIM.App.GetLocalString("Coluna3Fonte"), gobj2, "FontCol3", False, DWSIM.App.GetLocalString("Formataodotexto1"), DWSIM.App.GetLocalString("Fontedotextodacoluna3"), True)
                .Item(.Item.Count - 1).Tag2 = "FontCol3"
                .Item.Add(DWSIM.App.GetLocalString("Tratamentodotexto"), gobj2, "TextRenderStyle", False, DWSIM.App.GetLocalString("Aparncia2"), DWSIM.App.GetLocalString("Tipodesuavizaoaplica"), True)
                .Item(.Item.Count - 1).Tag2 = "TextRenderStyle"
                .Item.Add(DWSIM.App.GetLocalString("Estilodaborda"), gobj2, "BorderStyle", False, DWSIM.App.GetLocalString("Aparncia2"), DWSIM.App.GetLocalString("Estilodabordatraceja"), True)
                .Item(.Item.Count - 1).Tag2 = "BorderStyle"
                .Item.Add(DWSIM.App.GetLocalString("Cordaborda"), gobj2, "BorderColor", False, DWSIM.App.GetLocalString("Aparncia2"), "", True)
                .Item(.Item.Count - 1).Tag2 = "BorderColor"
                .Item.Add(DWSIM.App.GetLocalString("Espaamento"), gobj2, "Padding", False, DWSIM.App.GetLocalString("Aparncia2"), DWSIM.App.GetLocalString("Espaamentoentreotext"), True)
                .Item(.Item.Count - 1).Tag2 = "Padding"
                .Item.Add(DWSIM.App.GetLocalString("Rotao"), gobj2, "Rotation", False, DWSIM.App.GetLocalString("Aparncia2"), DWSIM.App.GetLocalString("Inclinaodatabelaemre"), True)
                .Item(.Item.Count - 1).Tag2 = "Rotation"
                .Item.Add(DWSIM.App.GetLocalString("Gradiente2"), gobj2, "IsGradientBackground", False, DWSIM.App.GetLocalString("Fundo"), "Selecione se deve ser utilizado um gradiente no fundo da tabela", True)
                .Item(.Item.Count - 1).Tag2 = "IsGradientBackground"
                .Item.Add(DWSIM.App.GetLocalString("Corsemgradiente"), gobj2, "FillColor", False, DWSIM.App.GetLocalString("Fundo"), DWSIM.App.GetLocalString("Corsemgradiente"), True)
                .Item(.Item.Count - 1).Tag2 = "FillColor"
                .Item.Add(DWSIM.App.GetLocalString("Cor1gradiente"), gobj2, "BackgroundGradientColor1", False, DWSIM.App.GetLocalString("Fundo"), DWSIM.App.GetLocalString("Cor1dogradientecasoa"), True)
                .Item(.Item.Count - 1).Tag2 = "BackgroundGradientColor1"
                .Item.Add(DWSIM.App.GetLocalString("Cor2gradiente"), gobj2, "BackgroundGradientColor2", False, DWSIM.App.GetLocalString("Fundo"), DWSIM.App.GetLocalString("Cor2dogradientecasoa"), True)
                .Item(.Item.Count - 1).Tag2 = "BackgroundGradientColor2"
                .Item.Add(DWSIM.App.GetLocalString("Opacidade0255"), gobj2, "Opacity", False, DWSIM.App.GetLocalString("Fundo"), DWSIM.App.GetLocalString("Nveldetransparnciada"), True)
                .Item(.Item.Count - 1).Tag2 = "Opacity"

                .PropertySort = PropertySort.Categorized
                .ShowCustomProperties = True

            End With

            gobj2 = Nothing
            FormProps.FTSProps.SelectedItem = FormProps.TSProps

        ElseIf gobj.ObjectType = ObjectType.GO_MasterTable Then

            Dim gobj2 As DWSIM.GraphicObjects.MasterTableGraphic = CType(gobj, DWSIM.GraphicObjects.MasterTableGraphic)

            With Me.FormProps.PGEx2

                .Item.Clear()

                .Item.Add(DWSIM.App.GetLocalString("Cor"), gobj2, "LineColor", False, DWSIM.App.GetLocalString("Formataodotexto1"), DWSIM.App.GetLocalString("Cordotextodatabela"), True)
                .Item(.Item.Count - 1).Tag2 = "LineColor"
                .Item.Add(DWSIM.App.GetLocalString("Cabealho"), gobj2, "HeaderFont", False, DWSIM.App.GetLocalString("Formataodotexto1"), DWSIM.App.GetLocalString("Fontedotextodocabeal"), True)
                .Item(.Item.Count - 1).Tag2 = "HeaderFont"
                .Item.Add(DWSIM.App.GetLocalString("Coluna1Fonte"), gobj2, "FontCol1", False, DWSIM.App.GetLocalString("Formataodotexto1"), DWSIM.App.GetLocalString("Fontedotextodacoluna"), True)
                .Item(.Item.Count - 1).Tag2 = "FontCol1"
                .Item.Add(DWSIM.App.GetLocalString("Coluna2Fonte"), gobj2, "FontCol2", False, DWSIM.App.GetLocalString("Formataodotexto1"), DWSIM.App.GetLocalString("Fontedotextodacoluna2"), True)
                .Item(.Item.Count - 1).Tag2 = "FontCol2"
                .Item.Add(DWSIM.App.GetLocalString("Coluna3Fonte"), gobj2, "FontCol3", False, DWSIM.App.GetLocalString("Formataodotexto1"), DWSIM.App.GetLocalString("Fontedotextodacoluna3"), True)
                .Item(.Item.Count - 1).Tag2 = "FontCol3"
                .Item.Add(DWSIM.App.GetLocalString("HeaderText"), gobj2, "HeaderText", False, DWSIM.App.GetLocalString("Aparncia2"), DWSIM.App.GetLocalString(""), True)
                .Item(.Item.Count - 1).Tag2 = "HeaderText"
                .Item.Add(DWSIM.App.GetLocalString("Tratamentodotexto"), gobj2, "TextRenderStyle", False, DWSIM.App.GetLocalString("Aparncia2"), DWSIM.App.GetLocalString("Tipodesuavizaoaplica"), True)
                .Item(.Item.Count - 1).Tag2 = "TextRenderStyle"
                .Item.Add(DWSIM.App.GetLocalString("Estilodaborda"), gobj2, "BorderStyle", False, DWSIM.App.GetLocalString("Aparncia2"), DWSIM.App.GetLocalString("Estilodabordatraceja"), True)
                .Item(.Item.Count - 1).Tag2 = "BorderStyle"
                .Item.Add(DWSIM.App.GetLocalString("Cordaborda"), gobj2, "BorderColor", False, DWSIM.App.GetLocalString("Aparncia2"), "", True)
                .Item(.Item.Count - 1).Tag2 = "BorderColor"
                .Item.Add(DWSIM.App.GetLocalString("Espaamento"), gobj2, "Padding", False, DWSIM.App.GetLocalString("Aparncia2"), DWSIM.App.GetLocalString("Espaamentoentreotext"), True)
                .Item(.Item.Count - 1).Tag2 = "Padding"
                .Item.Add(DWSIM.App.GetLocalString("Rotao"), gobj2, "Rotation", False, DWSIM.App.GetLocalString("Aparncia2"), DWSIM.App.GetLocalString("Inclinaodatabelaemre"), True)
                .Item(.Item.Count - 1).Tag2 = "Rotation"
                .Item.Add(DWSIM.App.GetLocalString("Gradiente2"), gobj2, "IsGradientBackground", False, DWSIM.App.GetLocalString("Fundo"), "Selecione se deve ser utilizado um gradiente no fundo da tabela", True)
                .Item(.Item.Count - 1).Tag2 = "IsGradientBackground"
                .Item.Add(DWSIM.App.GetLocalString("Corsemgradiente"), gobj2, "FillColor", False, DWSIM.App.GetLocalString("Fundo"), DWSIM.App.GetLocalString("Corsemgradiente"), True)
                .Item(.Item.Count - 1).Tag2 = "FillColor"
                .Item.Add(DWSIM.App.GetLocalString("Cor1gradiente"), gobj2, "BackgroundGradientColor1", False, DWSIM.App.GetLocalString("Fundo"), DWSIM.App.GetLocalString("Cor1dogradientecasoa"), True)
                .Item(.Item.Count - 1).Tag2 = "BackgroundGradientColor1"
                .Item.Add(DWSIM.App.GetLocalString("Cor2gradiente"), gobj2, "BackgroundGradientColor2", False, DWSIM.App.GetLocalString("Fundo"), DWSIM.App.GetLocalString("Cor2dogradientecasoa"), True)
                .Item(.Item.Count - 1).Tag2 = "BackgroundGradientColor2"
                .Item.Add(DWSIM.App.GetLocalString("Opacidade0255"), gobj2, "Opacity", False, DWSIM.App.GetLocalString("Fundo"), DWSIM.App.GetLocalString("Nveldetransparnciada"), True)
                .Item(.Item.Count - 1).Tag2 = "Opacity"

                .PropertySort = PropertySort.Categorized
                .ShowCustomProperties = True

            End With

            gobj2 = Nothing
            FormProps.FTSProps.SelectedItem = FormProps.TSProps

        ElseIf gobj.ObjectType = ObjectType.GO_SpreadsheetTable Then

            Dim gobj2 As DWSIM.GraphicObjects.SpreadsheetTableGraphic = CType(gobj, DWSIM.GraphicObjects.SpreadsheetTableGraphic)

            With Me.FormProps.PGEx2

                .Item.Clear()

                .Item.Add(DWSIM.App.GetLocalString("Cor"), gobj2, "LineColor", False, DWSIM.App.GetLocalString("Formataodotexto1"), DWSIM.App.GetLocalString("Cordotextodatabela"), True)
                .Item(.Item.Count - 1).Tag2 = "LineColor"
                .Item.Add(DWSIM.App.GetLocalString("Coluna1Fonte"), gobj2, "FontCol1", False, DWSIM.App.GetLocalString("Formataodotexto1"), DWSIM.App.GetLocalString("Fontedotextodacoluna"), True)
                .Item(.Item.Count - 1).Tag2 = "FontCol1"
                .Item.Add(DWSIM.App.GetLocalString("Tratamentodotexto"), gobj2, "TextRenderStyle", False, DWSIM.App.GetLocalString("Aparncia2"), DWSIM.App.GetLocalString("Tipodesuavizaoaplica"), True)
                .Item(.Item.Count - 1).Tag2 = "TextRenderStyle"
                .Item.Add(DWSIM.App.GetLocalString("Estilodaborda"), gobj2, "BorderStyle", False, DWSIM.App.GetLocalString("Aparncia2"), DWSIM.App.GetLocalString("Estilodabordatraceja"), True)
                .Item(.Item.Count - 1).Tag2 = "BorderStyle"
                .Item.Add(DWSIM.App.GetLocalString("Cordaborda"), gobj2, "BorderColor", False, DWSIM.App.GetLocalString("Aparncia2"), "", True)
                .Item(.Item.Count - 1).Tag2 = "BorderColor"
                .Item.Add(DWSIM.App.GetLocalString("Espaamento"), gobj2, "Padding", False, DWSIM.App.GetLocalString("Aparncia2"), DWSIM.App.GetLocalString("Espaamentoentreotext"), True)
                .Item(.Item.Count - 1).Tag2 = "Padding"
                .Item.Add(DWSIM.App.GetLocalString("Rotao"), gobj2, "Rotation", False, DWSIM.App.GetLocalString("Aparncia2"), DWSIM.App.GetLocalString("Inclinaodatabelaemre"), True)
                .Item(.Item.Count - 1).Tag2 = "Rotation"
                .Item.Add(DWSIM.App.GetLocalString("Gradiente2"), gobj2, "IsGradientBackground", False, DWSIM.App.GetLocalString("Fundo"), "Selecione se deve ser utilizado um gradiente no fundo da tabela", True)
                .Item(.Item.Count - 1).Tag2 = "IsGradientBackground"
                .Item.Add(DWSIM.App.GetLocalString("Corsemgradiente"), gobj2, "FillColor", False, DWSIM.App.GetLocalString("Fundo"), DWSIM.App.GetLocalString("Corsemgradiente"), True)
                .Item(.Item.Count - 1).Tag2 = "FillColor"
                .Item.Add(DWSIM.App.GetLocalString("Cor1gradiente"), gobj2, "BackgroundGradientColor1", False, DWSIM.App.GetLocalString("Fundo"), DWSIM.App.GetLocalString("Cor1dogradientecasoa"), True)
                .Item(.Item.Count - 1).Tag2 = "BackgroundGradientColor1"
                .Item.Add(DWSIM.App.GetLocalString("Cor2gradiente"), gobj2, "BackgroundGradientColor2", False, DWSIM.App.GetLocalString("Fundo"), DWSIM.App.GetLocalString("Cor2dogradientecasoa"), True)
                .Item(.Item.Count - 1).Tag2 = "BackgroundGradientColor2"
                .Item.Add(DWSIM.App.GetLocalString("Opacidade0255"), gobj2, "Opacity", False, DWSIM.App.GetLocalString("Fundo"), DWSIM.App.GetLocalString("Nveldetransparnciada"), True)
                .Item(.Item.Count - 1).Tag2 = "Opacity"

                .PropertySort = PropertySort.Categorized
                .ShowCustomProperties = True

            End With

            gobj2 = Nothing
            FormProps.FTSProps.SelectedItem = FormProps.TSProps

        ElseIf gobj.ObjectType = ObjectType.GO_Text Then

            Dim gobj2 As TextGraphic = CType(gobj, TextGraphic)

            With Me.FormProps.PGEx2

                .Item.Clear()

                .Item.Add(DWSIM.App.GetLocalString("Nome"), gobj.Tag, False, DWSIM.App.GetLocalString("Descrio1"), DWSIM.App.GetLocalString("Nomedoobjeto"), True)
                .Item(.Item.Count - 1).Tag2 = "Tag"
                .Item.Add(DWSIM.App.GetLocalString("Texto"), gobj2, "Text", False, "", DWSIM.App.GetLocalString("Textoaserexibidonaca"), True)
                .Item(.Item.Count - 1).Tag2 = "Text"
                .Item(.Item.Count - 1).CustomEditor = New System.ComponentModel.Design.MultilineStringEditor
                .Item.Add(DWSIM.App.GetLocalString("Tratamentodotexto"), gobj2, "TextRenderStyle", False, DWSIM.App.GetLocalString("Aparncia2"), DWSIM.App.GetLocalString("Tipodesuavizaoaplica"), True)
                .Item(.Item.Count - 1).Tag2 = "TextRenderStyle"
                .Item.Add(DWSIM.App.GetLocalString("Cor"), gobj2, "Color", False, "", DWSIM.App.GetLocalString("Cordotexto"), True)
                .Item(.Item.Count - 1).Tag2 = "Color"
                .Item.Add(DWSIM.App.GetLocalString("Fonte"), gobj2, "Font", False, "", DWSIM.App.GetLocalString("Fontedotexto"), True)
                .Item(.Item.Count - 1).Tag2 = "Font"

                .PropertySort = PropertySort.Categorized
                .ShowCustomProperties = True

            End With

            gobj2 = Nothing
            FormProps.FTSProps.SelectedItem = FormProps.TSObj

        ElseIf gobj.ObjectType = ObjectType.GO_Image Then

            Dim gobj2 As EmbeddedImageGraphic = CType(gobj, EmbeddedImageGraphic)

            With Me.FormProps.PGEx2

                .Item.Clear()

                .Item.Add(DWSIM.App.GetLocalString("Autodimensionar"), gobj2, "AutoSize", False, "", DWSIM.App.GetLocalString("SelecioLiquidrueparaque"), True)
                .Item(.Item.Count - 1).Tag2 = "AutoSize"
                .Item.Add(DWSIM.App.GetLocalString("Altura"), gobj2, "Height", False, "", DWSIM.App.GetLocalString("Alturadafiguraempixe"), True)
                .Item(.Item.Count - 1).Tag2 = "Height"
                .Item.Add(DWSIM.App.GetLocalString("Largura"), gobj2, "Width", False, "", DWSIM.App.GetLocalString("Larguradafiguraempix"), True)
                .Item(.Item.Count - 1).Tag2 = "Width"
                .Item.Add(DWSIM.App.GetLocalString("Rotao"), gobj2, "Rotation", False, "", DWSIM.App.GetLocalString("Rotaodafigurade0a360"), True)
                .Item(.Item.Count - 1).Tag2 = "Rotation"

                .PropertySort = PropertySort.Categorized
                .ShowCustomProperties = True

            End With

            gobj2 = Nothing
            FormProps.FTSProps.SelectedItem = FormProps.TSObj

        ElseIf gobj.ObjectType = ObjectType.GO_Animation Then

            Dim gobj2 As EmbeddedAnimationGraphic = CType(gobj, EmbeddedAnimationGraphic)

            With Me.FormProps.PGEx2

                .Item.Clear()

                .Item.Add(DWSIM.App.GetLocalString("Autodimensionar"), gobj2, "AutoSize", False, "", DWSIM.App.GetLocalString("SelecioLiquidrueparaque"), True)
                .Item(.Item.Count - 1).Tag2 = "AutoSize"
                .Item.Add(DWSIM.App.GetLocalString("Altura"), gobj2, "Height", False, "", DWSIM.App.GetLocalString("Alturadafiguraempixe"), True)
                .Item(.Item.Count - 1).Tag2 = "Height"
                .Item.Add(DWSIM.App.GetLocalString("Largura"), gobj2, "Width", False, "", DWSIM.App.GetLocalString("Larguradafiguraempix"), True)
                .Item(.Item.Count - 1).Tag2 = "Width"
                .Item.Add(DWSIM.App.GetLocalString("Rotao"), gobj2, "Rotation", False, "", DWSIM.App.GetLocalString("Rotaodafigurade0a360"), True)
                .Item(.Item.Count - 1).Tag2 = "Rotation"

                .PropertySort = PropertySort.Categorized
                .ShowCustomProperties = True

            End With

            gobj2 = Nothing
            FormProps.FTSProps.SelectedItem = FormProps.TSObj

        Else

            With Me.FormProps.PGEx2

                .Item.Clear()

                .Item.Add(DWSIM.App.GetLocalString("Nome"), gobj, "Tag", False, DWSIM.App.GetLocalString("Descrio1"), DWSIM.App.GetLocalString("Nomedoobjeto"), True)
                .Item(.Item.Count - 1).Tag2 = "Tag"
                .Item.Add(DWSIM.App.GetLocalString("Gradiente2"), gobj, "GradientMode", False, DWSIM.App.GetLocalString("Aparncia2"), DWSIM.App.GetLocalString("SelecioLiquidrueparaapl"), True)
                .Item(.Item.Count - 1).Tag2 = "GradientMode"
                .Item.Add("Gradiente_Cor1", gobj, "GradientColor1", False, DWSIM.App.GetLocalString("Aparncia2"), DWSIM.App.GetLocalString("Cor1dogradienteseapl"), True)
                .Item(.Item.Count - 1).Tag2 = "GradientColor1"
                .Item.Add("Gradiente_Cor2", gobj, "GradientColor2", False, DWSIM.App.GetLocalString("Aparncia2"), DWSIM.App.GetLocalString("Cor2dogradienteseapl"), True)
                .Item(.Item.Count - 1).Tag2 = "GradientColor2"
                .Item.Add(DWSIM.App.GetLocalString("Cor"), gobj, "FillColor", False, DWSIM.App.GetLocalString("Aparncia2"), "Cor de fundo, caso o modo de gradiente não esteja ativado", True)
                .Item(.Item.Count - 1).Tag2 = "FillColor"
                .Item.Add(DWSIM.App.GetLocalString("EspessuradaBorda"), gobj, "LineWidth", False, DWSIM.App.GetLocalString("Aparncia2"), DWSIM.App.GetLocalString("Espessuradabordadoob"), True)
                .Item(.Item.Count - 1).Tag2 = "LineWidth"
                .Item.Add(DWSIM.App.GetLocalString("Comprimento"), gobj, "Width", False, DWSIM.App.GetLocalString("Tamanho3"), DWSIM.App.GetLocalString("Comprimentodoobjetoe"), True)
                .Item(.Item.Count - 1).Tag2 = "Width"
                .Item.Add(DWSIM.App.GetLocalString("Altura"), gobj, "Height", False, DWSIM.App.GetLocalString("Tamanho3"), DWSIM.App.GetLocalString("Alturadoobjetoempixe"), True)
                .Item(.Item.Count - 1).Tag2 = "Height"
                .Item.Add(DWSIM.App.GetLocalString("Rotao"), gobj, "Rotation", False, DWSIM.App.GetLocalString("Tamanho3"), DWSIM.App.GetLocalString("Rotaodoobjetode0a360"), True)
                .Item(.Item.Count - 1).Tag2 = "Rotation"
                .Item.Add("X", gobj, "X", False, DWSIM.App.GetLocalString("Coordenadas4"), DWSIM.App.GetLocalString("Coordenadahorizontal"), True)
                .Item(.Item.Count - 1).Tag2 = "X"
                .Item.Add("Y", gobj, "Y", False, DWSIM.App.GetLocalString("Coordenadas4"), DWSIM.App.GetLocalString("Coordenadaverticaldo"), True)
                .Item(.Item.Count - 1).Tag2 = "Y"

                .PropertySort = PropertySort.Categorized
                .ShowCustomProperties = True

            End With

            FormProps.FTSProps.SelectedItem = FormProps.TSProps

        End If

        Return 1

    End Function

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

        For Each icomo As DWSIM.SimulationObjects.UnitOperations.Auxiliary.CapeOpen.CapeOpenUnitOpInfo In FormMain.COMonitoringObjects.Values

            Dim tsmi As New ToolStripMenuItem
            With tsmi
                .Text = icomo.Name
                .Tag = icomo.TypeName
                .Image = My.Resources.colan2
                .DisplayStyle = ToolStripItemDisplayStyle.ImageAndText
                .AutoToolTip = False
            End With
            With icomo
                tsmi.ToolTipText = "TypeName: " & vbTab & .TypeName & vbCrLf & _
                                    "Version: " & vbTab & vbTab & .Version & vbCrLf & _
                                    "Vendor URL: " & vbTab & .VendorURL & vbCrLf & _
                                    "About: " & vbTab & vbTab & .AboutInfo
            End With
            Me.CAPEOPENFlowsheetMonitoringObjectsMOsToolStripMenuItem.DropDownItems.Add(tsmi)
            AddHandler tsmi.Click, AddressOf Me.COMOClick
        Next

    End Sub

    Private Sub COMOClick(ByVal sender As System.Object, ByVal e As System.EventArgs)

        Dim tsmi As ToolStripMenuItem = CType(sender, ToolStripMenuItem)

        Dim myCOMO As DWSIM.SimulationObjects.UnitOperations.Auxiliary.CapeOpen.CapeOpenUnitOpInfo = FormMain.COMonitoringObjects.Item(tsmi.Tag)

        Dim _como As Object = Nothing
        Try
            Dim t As Type = Type.GetTypeFromProgID(myCOMO.TypeName)
            _como = Activator.CreateInstance(t)
            If TryCast(_como, CapeOpen.ICapeUtilities) IsNot Nothing Then
                If TryCast(_como, Interfaces.IPersistStreamInit) IsNot Nothing Then
                    CType(_como, Interfaces.IPersistStreamInit).InitNew()
                End If
                With CType(_como, CapeOpen.ICapeUtilities)
                    .Initialize()
                    .simulationContext = Me
                    .Edit()
                End With
            End If
        Catch ex As Exception
            Me.WriteToLog("Error creating CAPE-OPEN Flowsheet Monitoring Object: " & ex.ToString, Color.Red, DWSIM.Flowsheet.MessageType.GeneralError)
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
        For Each pp As PropertyPackage In Me.Options.PropertyPackages.Values
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
            For Each p As PropertyPackage In Me.Options.PropertyPackages.Values
                pps.Add(p.ComponentName)
            Next
            Dim arr2(pps.Count - 1) As String
            Array.Copy(pps.ToArray, arr2, pps.Count)
            Return arr2
        End Get
    End Property

    Public Function GetStreamCollection() As Object Implements CapeOpen.ICapeFlowsheetMonitoring.GetStreamCollection
        Dim _col As New DWSIM.SimulationObjects.UnitOperations.Auxiliary.CapeOpen.CCapeCollection
        For Each o As DWSIM.SimulationObjects.UnitOperations.BaseClass In Me.Collections.FlowsheetObjectCollection.Values
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
        Dim _col As New DWSIM.SimulationObjects.UnitOperations.Auxiliary.CapeOpen.CCapeCollection
        For Each o As DWSIM.SimulationObjects.UnitOperations.BaseClass In Me.Collections.FlowsheetObjectCollection.Values
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
            Return Me.Options.SimComentario
        End Get
        Set(ByVal value As String)
            Me.Options.SimComentario = value
        End Set
    End Property

    Public Property ComponentName() As String Implements CapeOpen.ICapeIdentification.ComponentName
        Get
            Return Me.Options.SimNome
        End Get
        Set(ByVal value As String)
            Me.Options.SimNome = value
        End Set
    End Property

#End Region

#Region "    Script Timers"

    Private Sub TimerScripts1_Tick(sender As Object, e As EventArgs) Handles TimerScripts1.Tick
        Me.ProcessScripts(Script.EventType.SimulationTimer1, Script.ObjectType.Simulation)
    End Sub

    Private Sub TimerScripts5_Tick(sender As Object, e As EventArgs) Handles TimerScripts5.Tick
        Me.ProcessScripts(Script.EventType.SimulationTimer5, Script.ObjectType.Simulation)
    End Sub

    Private Sub TimerScripts15_Tick(sender As Object, e As EventArgs) Handles TimerScripts15.Tick
        Me.ProcessScripts(Script.EventType.SimulationTimer15, Script.ObjectType.Simulation)
    End Sub

    Private Sub TimerScripts30_Tick(sender As Object, e As EventArgs) Handles TimerScripts30.Tick
        Me.ProcessScripts(Script.EventType.SimulationTimer30, Script.ObjectType.Simulation)
    End Sub

    Private Sub TimerScripts60_Tick(sender As Object, e As EventArgs) Handles TimerScripts60.Tick
        Me.ProcessScripts(Script.EventType.SimulationTimer60, Script.ObjectType.Simulation)
    End Sub

#End Region

#Region "    Question Box"

    Private Sub QuestionBox_Button1_Click(sender As Object, e As EventArgs) Handles QuestionBox_Button1.Click
        Me.QuestionBox_Panel.Visible = False
        Select Case QuestionID
            Case 0 'question about adding or not a new user-defined unit from the simulation file
                AddUnitSystem(Me.Options.SelectedUnitSystem)
                Me.ToolStripComboBoxUnitSystem.SelectedItem = Me.Options.SelectedUnitSystem.Name
        End Select
    End Sub

    Private Sub QuestionBox_Button2_Click(sender As Object, e As EventArgs) Handles QuestionBox_Button2.Click
        Me.QuestionBox_Panel.Visible = False
        Select Case QuestionID
            Case 0 'question about adding or not a new user-defined unit from the simulation file
                Me.ToolStripComboBoxUnitSystem.SelectedIndex = 0
        End Select
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
                                     .OldValue = Me.FormSurface.FlowsheetDesignSurface.SelectedObjects.Values.ToList,
                                     .Name = DWSIM.App.GetLocalString("UndoRedo_Cut")})

        Dim indexes As New ArrayList
        For Each gobj As GraphicObject In Me.FormSurface.FlowsheetDesignSurface.SelectedObjects.Values
            indexes.Add(gobj.Tag)
        Next
        For Each s As String In indexes
            Dim gobj As GraphicObject
            gobj = Me.GetFlowsheetGraphicObject(s)
            If Not gobj Is Nothing Then
                Me.DeleteSelectedObject(Me, New EventArgs(), gobj, False)
                Me.FormSurface.FlowsheetDesignSurface.SelectedObjects.Remove(gobj.Name)
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

        For Each so As DWSIM.SimulationObjects.UnitOperations.BaseClass In Collections.FlowsheetObjectCollection.Values
            If so.GraphicObject.Selected Then
                xel.Add(New XElement("SimulationObject", {so.SaveData().ToArray()}))
                If TypeOf so Is Streams.MaterialStream Then
                    If Not ppackages.Contains(DirectCast(so, Streams.MaterialStream).PropertyPackage.Name) Then
                        ppackages.Add(DirectCast(so, Streams.MaterialStream).PropertyPackage.Name)
                    End If
                ElseIf TypeOf so Is DWSIM.SimulationObjects.UnitOperations.UnitOpBaseClass Then
                    If Not ppackages.Contains(DirectCast(so, DWSIM.SimulationObjects.UnitOperations.UnitOpBaseClass).PropertyPackage.Name) Then
                        ppackages.Add(DirectCast(so, DWSIM.SimulationObjects.UnitOperations.UnitOpBaseClass).PropertyPackage.Name)
                    End If
                End If
            End If
        Next

        xdoc.Element("DWSIM_Simulation_Data").Add(New XElement("GraphicObjects"))
        xel = xdoc.Element("DWSIM_Simulation_Data").Element("GraphicObjects")

        For Each go As Microsoft.Msdn.Samples.GraphicObjects.GraphicObject In FormSurface.FlowsheetDesignSurface.drawingObjects
            If Not go.IsConnector And go.Selected Then xel.Add(New XElement("GraphicObject", go.SaveData().ToArray()))
        Next

        xdoc.Element("DWSIM_Simulation_Data").Add(New XElement("PropertyPackages"))
        xel = xdoc.Element("DWSIM_Simulation_Data").Element("PropertyPackages")

        For Each pp As KeyValuePair(Of String, PropertyPackage) In Options.PropertyPackages
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
                    Me.FrmStSim1.AddCompToSimulation(idx)
                End If
            Next

        End If

        If My.Settings.ClipboardCopyMode_PropertyPackages = 1 Then

            data = xdoc.Element("DWSIM_Simulation_Data").Element("PropertyPackages").Elements.ToList

            For Each xel As XElement In data
                Try
                    Dim t As Type = Type.GetType(xel.Element("Type").Value, False)
                    Dim obj As PropertyPackage = Activator.CreateInstance(t)
                    obj.LoadData(xel.Elements.ToList)
                    obj.UniqueID = pkey & obj.UniqueID
                    obj.Tag = obj.Tag & " (C)"
                    Me.Options.PropertyPackages.Add(obj.UniqueID, obj)
                Catch ex As Exception
                    excs.Add(New Exception("Error Loading Property Package Information", ex))
                End Try
            Next

        End If

        data = xdoc.Element("DWSIM_Simulation_Data").Element("SimulationObjects").Elements.ToList

        FormSurface.FlowsheetDesignSurface.SelectedObject = Nothing
        FormSurface.FlowsheetDesignSurface.SelectedObjects.Clear()

        Dim objlist As New Concurrent.ConcurrentBag(Of DWSIM.SimulationObjects.UnitOperations.BaseClass)

        Dim compoundstoremove As New List(Of String)

        For Each xel As XElement In data
            Dim id As String = pkey & xel.<Nome>.Value
            Dim t As Type = Type.GetType(xel.Element("Type").Value, False)
            Dim obj As DWSIM.SimulationObjects.UnitOperations.BaseClass = Activator.CreateInstance(t)
            Dim gobj As GraphicObjects.GraphicObject = (From go As GraphicObjects.GraphicObject In
                                FormSurface.FlowsheetDesignSurface.drawingObjects Where go.Name = id).SingleOrDefault
            obj.GraphicObject = gobj
            obj.SetFlowsheet(Me)
            If Not obj.GraphicObject.ObjectType = ObjectType.FlowsheetUO Then
                obj.FillNodeItems(True)
                obj.QTFillNodeItems()
            End If
            If Not gobj Is Nothing Then
                obj.LoadData(xel.Elements.ToList)
                If TypeOf obj Is Streams.MaterialStream Then
                    If My.Settings.ClipboardCopyMode_Compounds = 0 Then
                        For Each subst As Compound In DirectCast(obj, Streams.MaterialStream).Phases(0).Compounds.Values
                            If Not Options.SelectedComponents.ContainsKey(subst.Name) And Not compoundstoremove.Contains(subst.Name) Then compoundstoremove.Add(subst.Name)
                        Next
                    End If
                    For Each phase As DWSIM.Thermodynamics.BaseClasses.Phase In DirectCast(obj, Streams.MaterialStream).Phases.Values
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
                ElseIf TypeOf obj Is DWSIM.SimulationObjects.UnitOperations.UnitOpBaseClass Then
                    If DirectCast(obj, DWSIM.SimulationObjects.UnitOperations.UnitOpBaseClass)._ppid <> "" Then
                        DirectCast(obj, DWSIM.SimulationObjects.UnitOperations.UnitOpBaseClass).PropertyPackage = Me.Options.PropertyPackages(pkey & DirectCast(obj, DWSIM.SimulationObjects.UnitOperations.UnitOpBaseClass)._ppid)
                    End If
                End If
            End If
            objlist.Add(obj)
        Next

        If My.Settings.ClipboardCopyMode_Compounds = 0 Then

            For Each obj As DWSIM.SimulationObjects.UnitOperations.BaseClass In objlist
                If TypeOf obj Is Streams.MaterialStream Then
                    For Each phase As DWSIM.Thermodynamics.BaseClasses.Phase In DirectCast(obj, Streams.MaterialStream).Phases.Values
                        For Each comp In compoundstoremove
                            phase.Compounds.Remove(comp)
                        Next
                    Next
                End If
            Next

        End If

        FormMain.AddSimulationObjects(Me, objlist, excs, pkey)

        For Each obj In objlist
            If FormSurface.FlowsheetDesignSurface.SelectedObject Is Nothing Then FormSurface.FlowsheetDesignSurface.SelectedObject = obj.GraphicObject
            FormSurface.FlowsheetDesignSurface.SelectedObjects.Add(obj.Name, obj.GraphicObject)
        Next

        If addundo Then AddUndoRedoAction(New UndoRedoAction() With {.AType = UndoRedoActionType.PasteObjects,
                                     .OldValue = Clipboard.GetText,
                                     .NewValue = Me.FormSurface.FlowsheetDesignSurface.SelectedObjects.Values.ToList,
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

                    If fobj.GetProperties(DWSIM.SimulationObjects.UnitOperations.BaseClass.PropertyType.ALL).Contains(act.PropertyName) Then
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

                    Dim gobj = Me.FormSurface.FlowsheetDesignSurface.drawingObjects.FindObjectWithName(act.ObjID)

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
                        FormSurface.FlowsheetDesignSurface.drawingObjects.FindObjectWithName(gobj1.Name).LoadData(gobj1.SaveData)
                        If gobj1.ObjectType = ObjectType.MaterialStream Then
                            For Each phase As DWSIM.Thermodynamics.BaseClasses.Phase In DirectCast(Collections.FlowsheetObjectCollection(gobj1.Name), Streams.MaterialStream).Phases.Values
                                For Each c As ConstantProperties In Options.SelectedComponents.Values
                                    phase.Compounds(c.Name).ConstantProperties = c
                                Next
                            Next
                        End If
                    Else
                        DeleteObject(gobj1.Tag, False)
                    End If

                Case UndoRedoActionType.FlowsheetObjectConnected

                    Dim gobj1 = Me.FormSurface.FlowsheetDesignSurface.drawingObjects.FindObjectWithName(act.ObjID)
                    Dim gobj2 = Me.FormSurface.FlowsheetDesignSurface.drawingObjects.FindObjectWithName(act.ObjID2)

                    If undo Then
                        DisconnectObject(gobj1, gobj2)
                    Else
                        ConnectObject(gobj1, gobj2, act.OldValue, act.NewValue)
                    End If

                Case UndoRedoActionType.FlowsheetObjectDisconnected

                    Dim gobj1 = Me.FormSurface.FlowsheetDesignSurface.drawingObjects.FindObjectWithName(act.ObjID)
                    Dim gobj2 = Me.FormSurface.FlowsheetDesignSurface.drawingObjects.FindObjectWithName(act.ObjID2)

                    If undo Then
                        ConnectObject(gobj1, gobj2, act.OldValue, act.NewValue)
                    Else
                        DisconnectObject(gobj1, gobj2)
                    End If

                Case UndoRedoActionType.PropertyPackageAdded

                    If undo Then
                        Me.Options.PropertyPackages.Remove(act.ObjID)
                    Else
                        Dim pp As PropertyPackage = DirectCast(act.NewValue, PropertyPackage)
                        Me.Options.PropertyPackages.Add(pp.UniqueID, pp)
                    End If

                Case UndoRedoActionType.PropertyPackageRemoved

                    If undo Then
                        Dim pp As PropertyPackage = DirectCast(act.NewValue, PropertyPackage)
                        Me.Options.PropertyPackages.Add(pp.UniqueID, pp)
                    Else
                        Me.Options.PropertyPackages.Remove(act.ObjID)
                    End If

                Case UndoRedoActionType.PropertyPackagePropertyChanged

                    Dim pp As PropertyPackage = DirectCast(act.Tag, PropertyPackage)

                    If act.PropertyName = "PARAM" Then
                        pp.Parameters(act.ObjID) = pval
                    ElseIf act.PropertyName = "PR_IP" Then
                        Dim prip As Auxiliary.PengRobinson = pp.GetType.GetField("m_pr").GetValue(pp)
                        prip.InteractionParameters(act.ObjID)(act.ObjID2).kij = pval
                    ElseIf act.PropertyName = "PRSV2_KIJ" Then
                        Dim prip As Auxiliary.PRSV2 = pp.GetType.GetField("m_pr").GetValue(pp)
                        prip.InteractionParameters(act.ObjID)(act.ObjID2).kij = pval
                    ElseIf act.PropertyName = "PRSV2_KJI Then" Then
                        Dim prip As Auxiliary.PRSV2 = pp.GetType.GetField("m_pr").GetValue(pp)
                        prip.InteractionParameters(act.ObjID)(act.ObjID2).kji = pval
                    ElseIf act.PropertyName = "PRSV2VL_KIJ" Then
                        Dim prip As Auxiliary.PRSV2VL = pp.GetType.GetField("m_pr").GetValue(pp)
                        prip.InteractionParameters(act.ObjID)(act.ObjID2).kij = pval
                    ElseIf act.PropertyName = "PRSV2VL_KJI Then" Then
                        Dim prip As Auxiliary.PRSV2VL = pp.GetType.GetField("m_pr").GetValue(pp)
                        prip.InteractionParameters(act.ObjID)(act.ObjID2).kji = pval
                    ElseIf act.PropertyName = "LK_IP" Then
                        Dim prip As Auxiliary.LeeKeslerPlocker = pp.GetType.GetField("m_lk").GetValue(pp)
                        prip.InteractionParameters(act.ObjID)(act.ObjID2).kij = pval
                    ElseIf act.PropertyName.Contains("NRTL") Then
                        Dim nrtlip As Auxiliary.NRTL = pp.GetType.GetProperty("m_uni").GetValue(pp)
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
                        Dim uniquacip As Auxiliary.UNIQUAC = pp.GetType.GetProperty("m_uni").GetValue(pp)
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

                    Dim su = DirectCast(act.NewValue, DWSIM.SystemsOfUnits.Units)

                    If undo Then
                        Me.FrmStSim1.ComboBox2.SelectedIndex = 0
                        Me.Options.AvailableUnitSystems.Remove(su.Name)
                    Else
                        Me.Options.AvailableUnitSystems.Add(su.Name, su)
                    End If

                Case UndoRedoActionType.SystemOfUnitsRemoved

                    Dim su = DirectCast(act.NewValue, DWSIM.SystemsOfUnits.Units)

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

            End Select

            Me.FormSurface.UpdateSelectedObject()

        Catch ex As Exception

            WriteToLog(ex.ToString(), Color.Red, MessageType.GeneralError)

        End Try

    End Sub

    Sub AddUndoRedoAction(act As UndoRedoAction)

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

    Public Sub tsbUndo_Click(sender As Object, e As EventArgs) Handles tsbUndo.ButtonClick

        UndoActions(tsbUndo.DropDownItems(0), e)

    End Sub

    Public Sub tsbRedo_Click(sender As Object, e As EventArgs) Handles tsbRedo.ButtonClick

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

        If My.Settings.UndoRedo_RecalculateFlowsheet Then CalculateAll2(Me, My.Settings.SolverMode)

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

        If My.Settings.UndoRedo_RecalculateFlowsheet Then CalculateAll2(Me, My.Settings.SolverMode)

    End Sub

    Private Sub tsbUndo_MouseEnter(sender As Object, e As EventArgs)

        Dim hovereditem As ToolStripMenuItem = DirectCast(sender, ToolStripMenuItem)

        For Each tsmi As ToolStripMenuItem In tsbUndo.DropDownItems
            tsmi.Checked = False
        Next

        For Each tsmi As ToolStripMenuItem In tsbUndo.DropDownItems
            tsmi.Checked = True
            If tsmi Is hovereditem Then Exit For
        Next

    End Sub

    Private Sub tsbRedo_MouseEnter(sender As Object, e As EventArgs)

        Dim hovereditem As ToolStripMenuItem = DirectCast(sender, ToolStripMenuItem)

        For Each tsmi As ToolStripMenuItem In tsbRedo.DropDownItems
            tsmi.Checked = False
        Next

        For Each tsmi As ToolStripMenuItem In tsbRedo.DropDownItems
            tsmi.Checked = True
            If tsmi Is hovereditem Then Exit For
        Next

    End Sub

#End Region

End Class
