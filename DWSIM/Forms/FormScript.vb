Imports System.IO
Imports System.Text
Imports Microsoft.Scripting.Hosting

Imports System.Drawing.Text
Imports System.Linq
Imports System.Reflection
Imports FarsiLibrary.Win
Imports System.Threading
Imports Python.Runtime
Imports System.Threading.Tasks
Imports DWSIM.SharedClasses.DWSIM.Flowsheet
Imports IronPython.Hosting
Imports DWSIM.Interfaces
Imports DWSIM.SharedClassesCSharp.FilePicker
Imports Microsoft.Scripting.Utils
Imports PythonConsoleControl

<System.Serializable()> Public Class FormScript

    Inherits WeifenLuo.WinFormsUI.Docking.DockContent

    Public fc As FormFlowsheet

    Private CurrentlyDebugging As Boolean = False
    Private DebuggingPaused As Boolean = False
    Private CancelDebugToken As CancellationTokenSource
    Private ShouldUpdateSnippets As Boolean = True

    Private Sub FormVBScript_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Me.Load

        ExtensionMethods.ChangeDefaultFont(Me)

        Using g1 = Me.CreateGraphics()

            Settings.DpiScale = g1.DpiX / 96.0

            Me.ToolStrip1.AutoSize = False
            Me.ToolStrip1.Size = New Size(ToolStrip1.Width, 28 * Settings.DpiScale)
            Me.ToolStrip1.ImageScalingSize = New Size(20 * Settings.DpiScale, 20 * Settings.DpiScale)
            For Each item In Me.ToolStrip1.Items
                If TryCast(item, ToolStripButton) IsNot Nothing Then
                    DirectCast(item, ToolStripButton).Size = New Size(ToolStrip1.ImageScalingSize.Width, ToolStrip1.ImageScalingSize.Height)
                End If
            Next
            Me.ToolStrip1.Invalidate()

        End Using

        AddHandler fc.FormSurface.FlowsheetSurface.StatusUpdate, Sub(sender2, e2)
                                                                     ShouldUpdateSnippets = True
                                                                 End Sub

        ' Get the installed fonts collection.
        Dim installed_fonts As New InstalledFontCollection
        ' Get an array of the system's font familiies.
        Dim font_families() As FontFamily = installed_fonts.Families()
        ' Display the font families.
        For Each font_family As FontFamily In font_families
            tscb1.Items.Add(font_family.Name)
        Next

        tscb2.Items.AddRange(New Object() {6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16})

        tscb2.SelectedItem = 10
        If tscb1.Items.Contains("Consolas") Then
            tscb1.SelectedItem = "Consolas"
        ElseIf tscb1.Items.Contains("Courier New") Then
            tscb1.SelectedItem = "Courier New"
        ElseIf tscb1.Items.Contains("Ubuntu Mono") Then
            tscb1.SelectedItem = "Ubuntu Mono"
        ElseIf tscb1.Items.Contains("FreeMono") Then
            tscb1.SelectedItem = "FreeMono"
        Else
            tscb1.SelectedIndex = 0
        End If

        'load existing scripts
        For Each s As Script In fc.ScriptCollection.Values
            InsertScriptTab(s)
        Next

        Dim snippets = SharedClasses.Scripts.IronPythonSnippets.GetSnippets()

        For Each group1 In snippets.GroupBy(Function(x) x.Category1)

            Dim tsmi = New ToolStripMenuItem() With {.Text = group1.Key}
            tsbInsertSnippet.DropDownItems.Add(tsmi)

            For Each group2 In group1.GroupBy(Function(x2) x2.Category2)

                Dim tsmi2 = New ToolStripMenuItem() With {.Text = group2.Key}
                tsmi.DropDownItems.Add(tsmi2)

                For Each snippet In group2

                    Dim tsmi3 = New ToolStripMenuItem() With {.Text = snippet.Name & " (" & snippet.Scope & ")", .Tag = snippet.Snippet}

                    AddHandler tsmi3.Click, Sub()

                                                If Not DWSIM.App.IsRunningOnMono Then
                                                    Dim scontrol As ScriptEditorControl = DirectCast(TabStripScripts.SelectedItem.Controls(0).Controls(0), ScriptEditorControl)
                                                    scontrol.txtScript.InsertText(scontrol.txtScript.CurrentPosition, tsmi3.Tag.ToString)
                                                Else
                                                    Dim scontrol As ScriptEditorControlMono = DirectCast(TabStripScripts.SelectedItem.Controls(0).Controls(0), ScriptEditorControlMono)
                                                    scontrol.txtScript.Text = scontrol.txtScript.Text.Insert(scontrol.txtScript.SelectionStart, tsmi3.Tag.ToString)
                                                End If

                                            End Sub

                    tsmi2.DropDownItems.Add(tsmi3)

                Next

            Next

        Next


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

    Private Sub Button1_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles btnRun.Click

        If Not Me.TabStripScripts.SelectedItem Is Nothing Then
            If DWSIM.App.IsRunningOnMono Then
                Dim script = DirectCast(Me.TabStripScripts.SelectedItem.Controls(0).Controls(0), ScriptEditorControlMono).txtScript.Text
                Dim interp = DirectCast(Me.TabStripScripts.SelectedItem.Controls(0).Controls(0), ScriptEditorControlMono).cbPythonEngine.SelectedIndex
                If interp = 0 Then
                    RunScript_IronPython("", script, fc, Nothing)
                Else
                    RunScript_PythonNET("", script, fc)
                End If
            Else
                Dim script = DirectCast(Me.TabStripScripts.SelectedItem.Controls(0).Controls(0), ScriptEditorControl).txtScript.Text
                Dim interp = DirectCast(Me.TabStripScripts.SelectedItem.Controls(0).Controls(0), ScriptEditorControl).cbPythonEngine.SelectedIndex
                If interp = 0 Then
                    RunScript_IronPython("", script, fc, Nothing)
                Else
                    RunScript_PythonNET("", script, fc)
                End If
            End If
        End If

    End Sub

    Public Shared Sub RunScript_IronPython(scripttitle As String, scripttext As String, fsheet As FormFlowsheet, debuggingstep As Action(Of IronPython.Runtime.Exceptions.TraceBackFrame))

        fsheet.PythonPreprocessor?.Invoke(scripttext)

        Dim scope As Microsoft.Scripting.Hosting.ScriptScope
        Dim engine As Microsoft.Scripting.Hosting.ScriptEngine

        Dim opts As New Dictionary(Of String, Object)()

        opts("Frames") = Microsoft.Scripting.Runtime.ScriptingRuntimeHelpers.True

        If debuggingstep IsNot Nothing Then
            opts("Debug") = Microsoft.Scripting.Runtime.ScriptingRuntimeHelpers.True
        End If

        engine = IronPython.Hosting.Python.CreateEngine(opts)

        Dim paths(My.Settings.ScriptPaths.Count - 1) As String

        Dim paths0 = engine.GetSearchPaths().ToList()

        My.Settings.ScriptPaths.CopyTo(paths, 0)

        paths0.AddRange(paths)
        paths0.Add(Path.Combine(My.Application.Info.DirectoryPath, "Lib"))

        Try
            engine.SetSearchPaths(paths0)
        Catch ex As Exception
        End Try

        engine.Runtime.LoadAssembly(GetType(System.String).Assembly)
        engine.Runtime.LoadAssembly(GetType(Thermodynamics.BaseClasses.ConstantProperties).Assembly)
        engine.Runtime.LoadAssembly(GetType(Drawing.SkiaSharp.GraphicObjects.GraphicObject).Assembly)
        engine.Runtime.LoadAssembly(GetType(Drawing.SkiaSharp.GraphicsSurface).Assembly)

        If My.Application.CommandLineMode Then
            engine.Runtime.IO.SetOutput(Console.OpenStandardOutput, Console.OutputEncoding)
        Else
            engine.Runtime.IO.SetOutput(New DataGridViewTextStream(fsheet), UTF8Encoding.UTF8)
        End If
        scope = engine.CreateScope()
        scope.SetVariable("Plugins", My.Application.UtilityPlugins)
        scope.SetVariable("Flowsheet", fsheet)
        scope.SetVariable("Spreadsheet", fsheet.FormSpreadsheet.Spreadsheet)

        Dim Solver As New FlowsheetSolver.FlowsheetSolver

        scope.SetVariable("Solver", Solver)
        For Each obj As SharedClasses.UnitOperations.BaseClass In fsheet.Collections.FlowsheetObjectCollection.Values
            scope.SetVariable(obj.GraphicObject.Tag.Replace("-", "_"), obj)
        Next

        Dim txtcode As String = scripttext

        Dim source As Microsoft.Scripting.Hosting.ScriptSource = engine.CreateScriptSourceFromString(txtcode, Microsoft.Scripting.SourceCodeKind.Statements)

        If debuggingstep IsNot Nothing Then

            'enable debugging

            Dim vars As New List(Of Object)
            Dim names As New List(Of String)

            For Each variable In scope.GetVariableNames
                vars.Add(scope.GetVariable(variable))
                names.Add(variable)
            Next

            Dim var As IronPython.Runtime.Exceptions.TracebackDelegate = Nothing

            var = Function(frame As IronPython.Runtime.Exceptions.TraceBackFrame, result As String, payload As Object)

                      debuggingstep.Invoke(frame)

                      Return var

                  End Function

            engine.SetTrace(var)

        End If

        Try
            source.Execute(scope)
        Catch ex As Exception
            Dim ops As ExceptionOperations = engine.GetService(Of ExceptionOperations)()
            If My.Application.CommandLineMode Then
                Console.WriteLine()
                Console.WriteLine("Error running script: " & ops.FormatException(ex).ToString)
                Console.WriteLine()
            Else
                fsheet.ShowMessage(String.Format("Error running script '{0}': {1}", scripttitle, ops.FormatException(ex).ToString), Interfaces.IFlowsheet.MessageType.GeneralError)
            End If
        Finally
            engine.Runtime.Shutdown()
            engine = Nothing
            scope = Nothing
            source = Nothing
        End Try

    End Sub

    Public Shared Sub RunScript_PythonNET(scripttitle As String, scripttext As String, fsheet As FormFlowsheet)

        fsheet.PythonPreprocessor?.Invoke(scripttext)

        GlobalSettings.Settings.InitializePythonEnvironment()

        Using Py.GIL

            Try

                Dim sys As Object = Py.Import("sys")

                If Not GlobalSettings.Settings.IsRunningOnMono() Then
                    Dim codeToRedirectOutput As String = "import sys" & vbCrLf + "from io import BytesIO as StringIO" & vbCrLf + "sys.stdout = mystdout = StringIO()" & vbCrLf + "sys.stdout.flush()" & vbCrLf + "sys.stderr = mystderr = StringIO()" & vbCrLf + "sys.stderr.flush()"
                    PythonEngine.RunSimpleString(codeToRedirectOutput)
                End If

                Dim locals As New PyDict()

                locals.SetItem("Plugins", My.Application.UtilityPlugins.ToPython)
                locals.SetItem("Flowsheet", fsheet.ToPython)
                locals.SetItem("Spreadsheet", fsheet.FormSpreadsheet.Spreadsheet.ToPython)
                Dim Solver As New FlowsheetSolver.FlowsheetSolver
                locals.SetItem("Solver", Solver.ToPython)

                PythonEngine.Exec(scripttext, Nothing, locals)

                If Not GlobalSettings.Settings.IsRunningOnMono() Then
                    fsheet.WriteToLog(sys.stdout.getvalue().ToString, Color.Blue, MessageType.Information)
                End If

            Catch ex As Exception

                If My.Application.CommandLineMode Then
                    Console.WriteLine()
                    Console.WriteLine("Error running script: " & ex.ToString)
                    Console.WriteLine()
                Else
                    fsheet.ShowMessage(String.Format("Error running script '{0}': {1}", scripttitle, ex.ToString), Interfaces.IFlowsheet.MessageType.GeneralError)
                End If

            Finally

            End Try

        End Using

    End Sub


    Private Sub CutToolStripButton_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles CutToolStripButton.Click
        If Not DWSIM.App.IsRunningOnMono Then
            Dim scontrol As ScriptEditorControl = DirectCast(TabStripScripts.SelectedItem.Controls(0).Controls(0), ScriptEditorControl)
            scontrol.txtScript.Cut()
        Else
            Dim scontrol As ScriptEditorControlMono = DirectCast(TabStripScripts.SelectedItem.Controls(0).Controls(0), ScriptEditorControlMono)
            If scontrol.txtScript.SelectedText <> "" Then
                scontrol.txtScript.Cut()
            End If
        End If
    End Sub

    Private Sub CopyToolStripButton_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles CopyToolStripButton.Click
        If Not DWSIM.App.IsRunningOnMono Then
            Dim scontrol As ScriptEditorControl = DirectCast(TabStripScripts.SelectedItem.Controls(0).Controls(0), ScriptEditorControl)
            scontrol.txtScript.Copy()
        Else
            Dim scontrol As ScriptEditorControlMono = DirectCast(TabStripScripts.SelectedItem.Controls(0).Controls(0), ScriptEditorControlMono)
            If scontrol.txtScript.SelectedText <> "" Then Clipboard.SetText(scontrol.txtScript.SelectedText)
        End If
    End Sub

    Private Sub PasteToolStripButton_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles PasteToolStripButton.Click
        If Not DWSIM.App.IsRunningOnMono Then
            Dim scontrol As ScriptEditorControl = DirectCast(TabStripScripts.SelectedItem.Controls(0).Controls(0), ScriptEditorControl)
            scontrol.txtScript.Paste()
        Else
            Dim scontrol As ScriptEditorControlMono = DirectCast(TabStripScripts.SelectedItem.Controls(0).Controls(0), ScriptEditorControlMono)
            If scontrol.txtScript.SelectionLength <> 0 Then
                scontrol.txtScript.SelectedText = Clipboard.GetText()
            Else
                scontrol.txtScript.Paste()
            End If
        End If
    End Sub

    Public Sub UpdateScripts()

        fc.UIThread(Sub()

                        fc.ScriptCollection.Clear()

                        For Each tab As FATabStripItem In TabStripScripts.Items
                            If Not DWSIM.App.IsRunningOnMono Then
                                Dim seditor As ScriptEditorControl = DirectCast(tab.Controls(0).Controls(0), ScriptEditorControl)
                                Dim scr As New Script() With
                                                {.ID = Guid.NewGuid().ToString,
                                                 .Title = tab.Title,
                                                 .Linked = seditor.chkLink.Checked,
                                                 .ScriptText = seditor.txtScript.Text,
                                                 .PythonInterpreter = seditor.cbPythonEngine.SelectedIndex}
                                Select Case seditor.cbLinkedObject.SelectedIndex
                                    Case 0
                                        scr.LinkedObjectType = Scripts.ObjectType.Simulation
                                        scr.LinkedObjectName = ""
                                        If seditor.cbLinkedEvent.SelectedIndex = 0 Then
                                            scr.LinkedEventType = Scripts.EventType.SimulationOpened
                                        ElseIf seditor.cbLinkedEvent.SelectedIndex = 1 Then
                                            scr.LinkedEventType = Scripts.EventType.SimulationSaved
                                        ElseIf seditor.cbLinkedEvent.SelectedIndex = 2 Then
                                            scr.LinkedEventType = Scripts.EventType.SimulationClosed
                                        ElseIf seditor.cbLinkedEvent.SelectedIndex = 3 Then
                                            scr.LinkedEventType = Scripts.EventType.SimulationTimer1
                                        ElseIf seditor.cbLinkedEvent.SelectedIndex = 4 Then
                                            scr.LinkedEventType = Scripts.EventType.SimulationTimer5
                                        ElseIf seditor.cbLinkedEvent.SelectedIndex = 5 Then
                                            scr.LinkedEventType = Scripts.EventType.SimulationTimer15
                                        ElseIf seditor.cbLinkedEvent.SelectedIndex = 6 Then
                                            scr.LinkedEventType = Scripts.EventType.SimulationTimer30
                                        ElseIf seditor.cbLinkedEvent.SelectedIndex = 7 Then
                                            scr.LinkedEventType = Scripts.EventType.SimulationTimer60
                                        End If
                                    Case 1
                                        scr.LinkedObjectType = Scripts.ObjectType.Solver
                                        scr.LinkedObjectName = ""
                                        If seditor.cbLinkedEvent.SelectedIndex = 0 Then
                                            scr.LinkedEventType = Scripts.EventType.SolverStarted
                                        ElseIf seditor.cbLinkedEvent.SelectedIndex = 1 Then
                                            scr.LinkedEventType = Scripts.EventType.SolverFinished
                                        Else
                                            scr.LinkedEventType = Scripts.EventType.SolverRecycleLoop
                                        End If
                                    Case 2
                                        scr.LinkedObjectType = Scripts.ObjectType.Integrator
                                        scr.LinkedObjectName = ""
                                        If seditor.cbLinkedEvent.SelectedIndex = 0 Then
                                            scr.LinkedEventType = Scripts.EventType.IntegratorStarted
                                        ElseIf seditor.cbLinkedEvent.SelectedIndex = 1 Then
                                            scr.LinkedEventType = Scripts.EventType.IntegratorFinished
                                        ElseIf seditor.cbLinkedEvent.SelectedIndex = 2 Then
                                            scr.LinkedEventType = Scripts.EventType.IntegratorError
                                        ElseIf seditor.cbLinkedEvent.SelectedIndex = 3 Then
                                            scr.LinkedEventType = Scripts.EventType.IntegratorStep
                                        Else
                                            scr.LinkedEventType = Scripts.EventType.IntegratorPreStep
                                        End If
                                    Case Else
                                        If seditor.chkLink.Checked Then
                                            scr.LinkedObjectType = Scripts.ObjectType.FlowsheetObject
                                            Try
                                                scr.LinkedObjectName = fc.GetFlowsheetGraphicObject(seditor.cbLinkedObject.SelectedItem.ToString).Name
                                            Catch ex As Exception
                                            End Try
                                        End If
                                        If seditor.cbLinkedEvent.SelectedIndex = 0 Then
                                            scr.LinkedEventType = Scripts.EventType.ObjectCalculationStarted
                                        ElseIf seditor.cbLinkedEvent.SelectedIndex = 1 Then
                                            scr.LinkedEventType = Scripts.EventType.ObjectCalculationFinished
                                        Else
                                            scr.LinkedEventType = Scripts.EventType.ObjectCalculationError
                                        End If
                                End Select
                                fc.ScriptCollection.Add(scr.ID, scr)
                            Else
                                Dim seditor As ScriptEditorControlMono = DirectCast(tab.Controls(0).Controls(0), ScriptEditorControlMono)
                                Dim scr As New Script() With
                                                {.ID = Guid.NewGuid().ToString,
                                                 .Title = tab.Title,
                                                 .Linked = seditor.chkLink.Checked,
                                                 .ScriptText = seditor.txtScript.Text,
                                                 .PythonInterpreter = seditor.cbPythonEngine.SelectedIndex}
                                Select Case seditor.cbLinkedObject.SelectedIndex
                                    Case 0
                                        scr.LinkedObjectType = Scripts.ObjectType.Simulation
                                        scr.LinkedObjectName = ""
                                        If seditor.cbLinkedEvent.SelectedIndex = 0 Then
                                            scr.LinkedEventType = Scripts.EventType.SimulationOpened
                                        ElseIf seditor.cbLinkedEvent.SelectedIndex = 1 Then
                                            scr.LinkedEventType = Scripts.EventType.SimulationSaved
                                        ElseIf seditor.cbLinkedEvent.SelectedIndex = 2 Then
                                            scr.LinkedEventType = Scripts.EventType.SimulationClosed
                                        ElseIf seditor.cbLinkedEvent.SelectedIndex = 3 Then
                                            scr.LinkedEventType = Scripts.EventType.SimulationTimer1
                                        ElseIf seditor.cbLinkedEvent.SelectedIndex = 4 Then
                                            scr.LinkedEventType = Scripts.EventType.SimulationTimer5
                                        ElseIf seditor.cbLinkedEvent.SelectedIndex = 5 Then
                                            scr.LinkedEventType = Scripts.EventType.SimulationTimer15
                                        ElseIf seditor.cbLinkedEvent.SelectedIndex = 6 Then
                                            scr.LinkedEventType = Scripts.EventType.SimulationTimer30
                                        ElseIf seditor.cbLinkedEvent.SelectedIndex = 7 Then
                                            scr.LinkedEventType = Scripts.EventType.SimulationTimer60
                                        End If
                                    Case 1
                                        scr.LinkedObjectType = Scripts.ObjectType.Solver
                                        scr.LinkedObjectName = ""
                                        If seditor.cbLinkedEvent.SelectedIndex = 0 Then
                                            scr.LinkedEventType = Scripts.EventType.SolverStarted
                                        ElseIf seditor.cbLinkedEvent.SelectedIndex = 1 Then
                                            scr.LinkedEventType = Scripts.EventType.SolverFinished
                                        Else
                                            scr.LinkedEventType = Scripts.EventType.SolverRecycleLoop
                                        End If
                                    Case 2
                                        scr.LinkedObjectType = Scripts.ObjectType.Integrator
                                        scr.LinkedObjectName = ""
                                        If seditor.cbLinkedEvent.SelectedIndex = 0 Then
                                            scr.LinkedEventType = Scripts.EventType.IntegratorStarted
                                        ElseIf seditor.cbLinkedEvent.SelectedIndex = 1 Then
                                            scr.LinkedEventType = Scripts.EventType.IntegratorFinished
                                        ElseIf seditor.cbLinkedEvent.SelectedIndex = 2 Then
                                            scr.LinkedEventType = Scripts.EventType.IntegratorError
                                        ElseIf seditor.cbLinkedEvent.SelectedIndex = 3 Then
                                            scr.LinkedEventType = Scripts.EventType.IntegratorStep
                                        Else
                                            scr.LinkedEventType = Scripts.EventType.IntegratorPreStep
                                        End If
                                    Case Else
                                        If seditor.chkLink.Checked Then
                                            scr.LinkedObjectType = Scripts.ObjectType.FlowsheetObject
                                            scr.LinkedObjectName = fc.GetFlowsheetGraphicObject(seditor.cbLinkedObject.SelectedItem.ToString).Name
                                        End If
                                        If seditor.cbLinkedEvent.SelectedIndex = 0 Then
                                            scr.LinkedEventType = Scripts.EventType.ObjectCalculationStarted
                                        ElseIf seditor.cbLinkedEvent.SelectedIndex = 1 Then
                                            scr.LinkedEventType = Scripts.EventType.ObjectCalculationFinished
                                        Else
                                            scr.LinkedEventType = Scripts.EventType.ObjectCalculationError
                                        End If
                                End Select
                                fc.ScriptCollection.Add(scr.ID, scr)
                            End If
                        Next

                        fc.WriteToLog("Script Data updated sucessfully.", Color.Blue, MessageType.Information)

                    End Sub)

    End Sub

    Private Sub tscb1_SelectedIndexChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles tscb1.SelectedIndexChanged
        For Each ft As FATabStripItem In TabStripScripts.Items
            If Not DWSIM.App.IsRunningOnMono Then
                DirectCast(ft.Controls(0).Controls(0), ScriptEditorControl).txtScript.SetEditorStyle(tscb1.SelectedItem.ToString, tscb2.SelectedItem.ToString, False)
            Else
                DirectCast(ft.Controls(0).Controls(0), ScriptEditorControlMono).txtScript.Font = New Font(tscb1.SelectedItem.ToString, 10)
            End If
        Next
    End Sub

    Private Sub tscb2_SelectedIndexChanged(ByVal sender As Object, ByVal e As System.EventArgs) Handles tscb2.SelectedIndexChanged
        For Each ft As FATabStripItem In TabStripScripts.Items
            If Not DWSIM.App.IsRunningOnMono Then
                DirectCast(ft.Controls(0).Controls(0), ScriptEditorControl).txtScript.SetEditorStyle(tscb1.SelectedItem.ToString, tscb2.SelectedItem.ToString, False)
            Else
                DirectCast(ft.Controls(0).Controls(0), ScriptEditorControlMono).txtScript.Font = New Font(DirectCast(ft.Controls(0).Controls(0), ScriptEditorControlMono).txtScript.Font.Name, tscb2.SelectedItem)
            End If
        Next
    End Sub

    Private Sub NewToolStripButton_Click(sender As Object, e As EventArgs) Handles NewToolStripButton.Click

        Dim newscript As New Script With {.ID = Guid.NewGuid().ToString()}
        fc.ScriptCollection.Add(newscript.ID, newscript)
        InsertScriptTab(newscript)

    End Sub

    Private Sub InsertScriptTab(scriptdata As Script)

        If Not DWSIM.App.IsRunningOnMono Then

            Dim p As New Panel With {.Dock = DockStyle.Fill}
            Dim scontrol As New ScriptEditorControl With {.Dock = DockStyle.Fill}

            With scontrol

                AddHandler scontrol.txtScript.KeyDown, AddressOf scriptcontrol_KeyDown

                .txtScript.Text = scriptdata.ScriptText
                .txtScript.Tag = 1
                .txtScript.SetEditorStyle(tscb1.SelectedItem.ToString, tscb2.SelectedItem.ToString, False)

                .form = fc

                .chkLink.Checked = scriptdata.Linked

                p.Controls.Add(scontrol)

                Dim stab As New FATabStripItem()
                stab.Controls.Add(p)
                stab.Tag = scriptdata.ID
                If scriptdata.Title = "" Then
                    stab.Title = "Script" & TabStripScripts.Items.Count + 1
                    scriptdata.Title = stab.Title
                Else
                    stab.Title = scriptdata.Title
                End If

                AddHandler scontrol.txtScript.TextChanged,
                    Sub()
                        scriptdata.ScriptText = scontrol.txtScript.Text
                    End Sub

                AddHandler scontrol.tbName.TextChanged,
                    Sub()
                        scriptdata.Title = scontrol.tbName.Text
                        stab.Title = scriptdata.Title
                    End Sub

                AddHandler scontrol.btnDelete.Click,
                    Sub()
                        If MessageBox.Show(DWSIM.App.GetLocalString("RemoveScriptQuestion"), "DWSIM", MessageBoxButtons.YesNo, MessageBoxIcon.Question) = DialogResult.Yes Then
                            TabStripScripts.RemoveTab(stab)
                            fc.ScriptCollection.Remove(scriptdata.ID)
                        End If
                    End Sub

                TabStripScripts.AddTab(stab, True)

                TabStripScripts.SelectedItem = stab

                .tbName.Text = scriptdata.Title

                Me.Invalidate()

                If scriptdata.LinkedObjectName <> "" And fc.SimulationObjects.ContainsKey(scriptdata.LinkedObjectName) Then
                    .cbLinkedObject.SelectedItem = fc.Collections.FlowsheetObjectCollection(scriptdata.LinkedObjectName).GraphicObject.Tag
                Else
                    Select Case scriptdata.LinkedObjectType
                        Case Scripts.ObjectType.Simulation
                            .cbLinkedObject.SelectedIndex = 0
                        Case Scripts.ObjectType.Solver
                            .cbLinkedObject.SelectedIndex = 1
                        Case Scripts.ObjectType.Integrator
                            .cbLinkedObject.SelectedIndex = 2
                    End Select
                End If

                Select Case scriptdata.LinkedEventType
                    Case Scripts.EventType.ObjectCalculationStarted
                        .cbLinkedEvent.SelectedIndex = 0
                    Case Scripts.EventType.ObjectCalculationFinished
                        .cbLinkedEvent.SelectedIndex = 1
                    Case Scripts.EventType.ObjectCalculationError
                        .cbLinkedEvent.SelectedIndex = 2
                    Case Scripts.EventType.SimulationOpened
                        .cbLinkedEvent.SelectedIndex = 0
                    Case Scripts.EventType.SimulationSaved
                        .cbLinkedEvent.SelectedIndex = 1
                    Case Scripts.EventType.SimulationClosed
                        .cbLinkedEvent.SelectedIndex = 2
                    Case Scripts.EventType.SolverStarted
                        .cbLinkedEvent.SelectedIndex = 0
                    Case Scripts.EventType.SolverFinished
                        .cbLinkedEvent.SelectedIndex = 1
                    Case Scripts.EventType.SolverRecycleLoop
                        .cbLinkedEvent.SelectedIndex = 2
                    Case Scripts.EventType.SimulationTimer1
                        .cbLinkedEvent.SelectedIndex = 3
                    Case Scripts.EventType.SimulationTimer5
                        .cbLinkedEvent.SelectedIndex = 4
                    Case Scripts.EventType.SimulationTimer15
                        .cbLinkedEvent.SelectedIndex = 5
                    Case Scripts.EventType.SimulationTimer30
                        .cbLinkedEvent.SelectedIndex = 6
                    Case Scripts.EventType.SimulationTimer60
                        .cbLinkedEvent.SelectedIndex = 7
                    Case Scripts.EventType.IntegratorStarted
                        .cbLinkedEvent.SelectedIndex = 0
                    Case Scripts.EventType.IntegratorFinished
                        .cbLinkedEvent.SelectedIndex = 1
                    Case Scripts.EventType.IntegratorError
                        .cbLinkedEvent.SelectedIndex = 2
                    Case Scripts.EventType.IntegratorStep
                        .cbLinkedEvent.SelectedIndex = 3
                    Case Scripts.EventType.IntegratorPreStep
                        .cbLinkedEvent.SelectedIndex = 4
                End Select

                .cbPythonEngine.SelectedIndex = scriptdata.PythonInterpreter

                AddHandler scontrol.cbPythonEngine.SelectedIndexChanged,
                    Sub()
                        scriptdata.PythonInterpreter = scontrol.cbPythonEngine.SelectedIndex
                    End Sub

                Dim cbhandler = Sub()
                                    Dim scr = scriptdata
                                    Select Case scontrol.cbLinkedObject.SelectedIndex
                                        Case 0
                                            scr.LinkedObjectType = Scripts.ObjectType.Simulation
                                            scr.LinkedObjectName = ""
                                            If scontrol.cbLinkedEvent.SelectedIndex = 0 Then
                                                scr.LinkedEventType = Scripts.EventType.SimulationOpened
                                            ElseIf scontrol.cbLinkedEvent.SelectedIndex = 1 Then
                                                scr.LinkedEventType = Scripts.EventType.SimulationSaved
                                            ElseIf scontrol.cbLinkedEvent.SelectedIndex = 2 Then
                                                scr.LinkedEventType = Scripts.EventType.SimulationClosed
                                            ElseIf scontrol.cbLinkedEvent.SelectedIndex = 3 Then
                                                scr.LinkedEventType = Scripts.EventType.SimulationTimer1
                                            ElseIf scontrol.cbLinkedEvent.SelectedIndex = 4 Then
                                                scr.LinkedEventType = Scripts.EventType.SimulationTimer5
                                            ElseIf scontrol.cbLinkedEvent.SelectedIndex = 5 Then
                                                scr.LinkedEventType = Scripts.EventType.SimulationTimer15
                                            ElseIf scontrol.cbLinkedEvent.SelectedIndex = 6 Then
                                                scr.LinkedEventType = Scripts.EventType.SimulationTimer30
                                            ElseIf scontrol.cbLinkedEvent.SelectedIndex = 7 Then
                                                scr.LinkedEventType = Scripts.EventType.SimulationTimer60
                                            End If
                                        Case 1
                                            scr.LinkedObjectType = Scripts.ObjectType.Solver
                                            scr.LinkedObjectName = ""
                                            If scontrol.cbLinkedEvent.SelectedIndex = 0 Then
                                                scr.LinkedEventType = Scripts.EventType.SolverStarted
                                            ElseIf scontrol.cbLinkedEvent.SelectedIndex = 1 Then
                                                scr.LinkedEventType = Scripts.EventType.SolverFinished
                                            Else
                                                scr.LinkedEventType = Scripts.EventType.SolverRecycleLoop
                                            End If
                                        Case 2
                                            scr.LinkedObjectType = Scripts.ObjectType.Integrator
                                            scr.LinkedObjectName = ""
                                            If scontrol.cbLinkedEvent.SelectedIndex = 0 Then
                                                scr.LinkedEventType = Scripts.EventType.IntegratorStarted
                                            ElseIf scontrol.cbLinkedEvent.SelectedIndex = 1 Then
                                                scr.LinkedEventType = Scripts.EventType.IntegratorFinished
                                            ElseIf scontrol.cbLinkedEvent.SelectedIndex = 2 Then
                                                scr.LinkedEventType = Scripts.EventType.IntegratorError
                                            ElseIf scontrol.cbLinkedEvent.SelectedIndex = 3 Then
                                                scr.LinkedEventType = Scripts.EventType.IntegratorStep
                                            ElseIf scontrol.cbLinkedEvent.SelectedIndex = 4 Then
                                                scr.LinkedEventType = Scripts.EventType.IntegratorPreStep
                                            End If
                                        Case Else
                                            If scontrol.chkLink.Checked Then
                                                scr.LinkedObjectType = Scripts.ObjectType.FlowsheetObject
                                                Try
                                                    scr.LinkedObjectName = fc.GetFlowsheetGraphicObject(scontrol.cbLinkedObject.SelectedItem.ToString).Name
                                                Catch ex As Exception
                                                End Try
                                            End If
                                            If scontrol.cbLinkedEvent.SelectedIndex = 0 Then
                                                scr.LinkedEventType = Scripts.EventType.ObjectCalculationStarted
                                            ElseIf scontrol.cbLinkedEvent.SelectedIndex = 1 Then
                                                scr.LinkedEventType = Scripts.EventType.ObjectCalculationFinished
                                            Else
                                                scr.LinkedEventType = Scripts.EventType.ObjectCalculationError
                                            End If
                                    End Select

                                End Sub

                AddHandler scontrol.cbLinkedObject.SelectedIndexChanged, cbhandler

                AddHandler scontrol.cbLinkedEvent.SelectedIndexChanged, cbhandler

                AddHandler scontrol.chkLink.CheckStateChanged,
                    Sub()
                        scriptdata.Linked = scontrol.chkLink.Checked
                    End Sub

            End With

        Else

            Dim p As New Panel With {.Dock = DockStyle.Fill}
            Dim scontrol As New ScriptEditorControlMono With {.Dock = DockStyle.Fill}

            With scontrol

                AddHandler scontrol.txtScript.KeyDown, AddressOf scriptcontrol_KeyDown

                .txtScript.Font = New Font(tscb1.SelectedItem.ToString, tscb2.SelectedItem)
                .txtScript.Text = scriptdata.ScriptText

                AddHandler scontrol.txtScript.TextChanged, Sub()
                                                               scriptdata.ScriptText = scontrol.txtScript.Text
                                                           End Sub
                .form = fc

                .chkLink.Checked = scriptdata.Linked

                p.Controls.Add(scontrol)

                Dim stab As New FATabStripItem()
                stab.Controls.Add(p)
                stab.Tag = scriptdata.ID
                If scriptdata.Title = "" Then stab.Title = "Script" & TabStripScripts.Items.Count + 1 Else stab.Title = scriptdata.Title

                TabStripScripts.AddTab(stab, True)

                TabStripScripts.SelectedItem = stab

                Me.Invalidate()

                If scriptdata.LinkedObjectName <> "" Then
                    .cbLinkedObject.SelectedItem = fc.Collections.FlowsheetObjectCollection(scriptdata.LinkedObjectName).GraphicObject.Tag
                Else
                    Select Case scriptdata.LinkedObjectType
                        Case Scripts.ObjectType.Simulation
                            .cbLinkedObject.SelectedIndex = 0
                        Case Scripts.ObjectType.Solver
                            .cbLinkedObject.SelectedIndex = 1
                        Case Scripts.ObjectType.Integrator
                            .cbLinkedObject.SelectedIndex = 2
                    End Select
                End If

                Select Case scriptdata.LinkedEventType
                    Case Scripts.EventType.ObjectCalculationStarted
                        .cbLinkedEvent.SelectedIndex = 0
                    Case Scripts.EventType.ObjectCalculationFinished
                        .cbLinkedEvent.SelectedIndex = 1
                    Case Scripts.EventType.ObjectCalculationError
                        .cbLinkedEvent.SelectedIndex = 2
                    Case Scripts.EventType.SimulationOpened
                        .cbLinkedEvent.SelectedIndex = 0
                    Case Scripts.EventType.SimulationSaved
                        .cbLinkedEvent.SelectedIndex = 1
                    Case Scripts.EventType.SimulationClosed
                        .cbLinkedEvent.SelectedIndex = 2
                    Case Scripts.EventType.SolverStarted
                        .cbLinkedEvent.SelectedIndex = 0
                    Case Scripts.EventType.SolverFinished
                        .cbLinkedEvent.SelectedIndex = 1
                    Case Scripts.EventType.SolverRecycleLoop
                        .cbLinkedEvent.SelectedIndex = 2
                    Case Scripts.EventType.SimulationTimer1
                        .cbLinkedEvent.SelectedIndex = 3
                    Case Scripts.EventType.SimulationTimer5
                        .cbLinkedEvent.SelectedIndex = 4
                    Case Scripts.EventType.SimulationTimer15
                        .cbLinkedEvent.SelectedIndex = 5
                    Case Scripts.EventType.SimulationTimer30
                        .cbLinkedEvent.SelectedIndex = 6
                    Case Scripts.EventType.SimulationTimer60
                        .cbLinkedEvent.SelectedIndex = 7
                    Case Scripts.EventType.IntegratorStarted
                        .cbLinkedEvent.SelectedIndex = 0
                    Case Scripts.EventType.IntegratorFinished
                        .cbLinkedEvent.SelectedIndex = 1
                    Case Scripts.EventType.IntegratorError
                        .cbLinkedEvent.SelectedIndex = 2
                    Case Scripts.EventType.IntegratorStep
                        .cbLinkedEvent.SelectedIndex = 3
                    Case Scripts.EventType.IntegratorPreStep
                        .cbLinkedEvent.SelectedIndex = 4
                End Select

                .cbPythonEngine.SelectedIndex = scriptdata.PythonInterpreter

            End With

        End If

    End Sub

    Private Sub TabStripScripts_TabStripItemClosing(e As TabStripItemClosingEventArgs) Handles TabStripScripts.TabStripItemClosing
        If MessageBox.Show(DWSIM.App.GetLocalString("RemoveScriptQuestion"), "DWSIM", MessageBoxButtons.YesNo, MessageBoxIcon.Question) = DialogResult.No Then
            e.Cancel = True
        Else
            fc.ScriptCollection.Remove(e.Item.Tag)
        End If
    End Sub

    Private Sub scriptcontrol_KeyDown(sender As Object, e As KeyEventArgs)

        If e.KeyCode = Keys.F5 Then btnRunAsync.PerformClick()
        If e.KeyCode = Keys.F5 And e.Modifiers = Keys.Shift Then btnRun.PerformClick()

    End Sub

    Private Sub APIHelptsbutton_Click(sender As Object, e As EventArgs) Handles APIHelptsbutton.Click
        If FormMain.IsPro Then
            fc.DisplayBrowserWindow("https://dwsim.org/api_help/html/G_DWSIM.htm")
        Else
            Process.Start("https://dwsim.org/api_help/html/G_DWSIM.htm")
        End If
    End Sub

    Private Sub btnUndo_Click(sender As Object, e As EventArgs) Handles btnUndo.Click
        If Not DWSIM.App.IsRunningOnMono Then
            Dim scontrol As ScriptEditorControl = DirectCast(TabStripScripts.SelectedItem.Controls(0).Controls(0), ScriptEditorControl)
            scontrol.txtScript.Undo()
        Else
            Dim scontrol As ScriptEditorControlMono = DirectCast(TabStripScripts.SelectedItem.Controls(0).Controls(0), ScriptEditorControlMono)
            scontrol.txtScript.Undo()
        End If
    End Sub

    Private Sub btnRedo_Click(sender As Object, e As EventArgs) Handles btnRedo.Click
        If Not DWSIM.App.IsRunningOnMono Then
            Dim scontrol As ScriptEditorControl = DirectCast(TabStripScripts.SelectedItem.Controls(0).Controls(0), ScriptEditorControl)
            scontrol.txtScript.Redo()
        End If
    End Sub

    Private Sub ToolStripButton2_Click_1(sender As Object, e As EventArgs) Handles btnComment.Click
        If Not DWSIM.App.IsRunningOnMono Then
            Dim scontrol As ScriptEditorControl = DirectCast(TabStripScripts.SelectedItem.Controls(0).Controls(0), ScriptEditorControl)
            Dim lines = scontrol.txtScript.SelectedText.Split(Environment.NewLine, vbCr, vbLf, vbCrLf)
            Dim newlines As String = ""
            For Each l In lines
                If l <> "" Then newlines += l.Insert(0, "#") & Environment.NewLine
            Next
            scontrol.txtScript.ReplaceSelection(newlines.TrimEnd(Environment.NewLine, vbCr, vbLf, vbCrLf))
        Else
            Dim scontrol As ScriptEditorControlMono = DirectCast(TabStripScripts.SelectedItem.Controls(0).Controls(0), ScriptEditorControlMono)
            Dim lines = scontrol.txtScript.SelectedText.Split(Environment.NewLine, vbCr, vbLf, vbCrLf)
            Dim newlines As String = ""
            For Each l In lines
                If l <> "" Then newlines += l.Insert(0, "#") & Environment.NewLine
            Next
            scontrol.txtScript.SelectedText = newlines.TrimEnd(Environment.NewLine, vbCr, vbLf, vbCrLf)
        End If
    End Sub

    Private Sub btnUncomment_Click(sender As Object, e As EventArgs) Handles btnUncomment.Click
        If Not DWSIM.App.IsRunningOnMono Then
            Dim scontrol As ScriptEditorControl = DirectCast(TabStripScripts.SelectedItem.Controls(0).Controls(0), ScriptEditorControl)
            Dim lines = scontrol.txtScript.SelectedText.Split(Environment.NewLine, vbCr, vbLf, vbCrLf)
            Dim newlines As String = ""
            For Each l In lines
                If l <> "" Then newlines += l.TrimStart("#") & Environment.NewLine
            Next
            scontrol.txtScript.ReplaceSelection(newlines.TrimEnd(Environment.NewLine, vbCr, vbLf, vbCrLf))
        Else
            Dim scontrol As ScriptEditorControlMono = DirectCast(TabStripScripts.SelectedItem.Controls(0).Controls(0), ScriptEditorControlMono)
            Dim lines = scontrol.txtScript.SelectedText.Split(Environment.NewLine, vbCr, vbLf, vbCrLf)
            Dim newlines As String = ""
            For Each l In lines
                If l <> "" Then newlines += l.TrimStart("#") & Environment.NewLine
            Next
            scontrol.txtScript.SelectedText = newlines.TrimEnd(Environment.NewLine, vbCr, vbLf, vbCrLf)
        End If
    End Sub

    Private Sub btnIdent_Click(sender As Object, e As EventArgs) Handles btnIdent.Click
        If Not DWSIM.App.IsRunningOnMono Then
            Dim scontrol As ScriptEditorControl = DirectCast(TabStripScripts.SelectedItem.Controls(0).Controls(0), ScriptEditorControl)
            Dim lines = scontrol.txtScript.SelectedText.Split(Environment.NewLine, vbCr, vbLf, vbCrLf)
            Dim newlines As String = ""
            For Each l In lines
                If l <> "" Then newlines += l.Insert(0, vbTab) & Environment.NewLine
            Next
            scontrol.txtScript.ReplaceSelection(newlines.TrimEnd(Environment.NewLine, vbCr, vbLf, vbCrLf))
        Else
            Dim scontrol As ScriptEditorControlMono = DirectCast(TabStripScripts.SelectedItem.Controls(0).Controls(0), ScriptEditorControlMono)
            Dim lines = scontrol.txtScript.SelectedText.Split(Environment.NewLine, vbCr, vbLf, vbCrLf)
            Dim newlines As String = ""
            For Each l In lines
                If l <> "" Then newlines += l.Insert(0, vbTab) & Environment.NewLine
            Next
            scontrol.txtScript.SelectedText = newlines.TrimEnd(Environment.NewLine, vbCr, vbLf, vbCrLf)
        End If
    End Sub

    Private Sub btnIdentRemove_Click(sender As Object, e As EventArgs) Handles btnIdentRemove.Click
        If Not DWSIM.App.IsRunningOnMono Then
            Dim scontrol As ScriptEditorControl = DirectCast(TabStripScripts.SelectedItem.Controls(0).Controls(0), ScriptEditorControl)
            Dim lines = scontrol.txtScript.SelectedText.Split(Environment.NewLine, vbCr, vbLf, vbCrLf)
            Dim newlines As String = ""
            For Each l In lines
                If l <> "" Then newlines += l.TrimStart(vbTab) & Environment.NewLine
            Next
            scontrol.txtScript.ReplaceSelection(newlines.TrimEnd(Environment.NewLine, vbCr, vbLf, vbCrLf))
        Else
            Dim scontrol As ScriptEditorControlMono = DirectCast(TabStripScripts.SelectedItem.Controls(0).Controls(0), ScriptEditorControlMono)
            Dim lines = scontrol.txtScript.SelectedText.Split(Environment.NewLine, vbCr, vbLf, vbCrLf)
            Dim newlines As String = ""
            For Each l In lines
                If l <> "" Then newlines += l.TrimStart(vbTab) & Environment.NewLine
            Next
            scontrol.txtScript.SelectedText = newlines.TrimEnd(Environment.NewLine, vbCr, vbLf, vbCrLf)
        End If
    End Sub

    Private Sub btnRunAsync_Click(sender As Object, e As EventArgs) Handles btnRunAsync.Click

        If Not Me.TabStripScripts.SelectedItem Is Nothing Then
            If DWSIM.App.IsRunningOnMono Then
                Dim script = DirectCast(Me.TabStripScripts.SelectedItem.Controls(0).Controls(0), ScriptEditorControlMono).txtScript.Text
                Dim interp = DirectCast(Me.TabStripScripts.SelectedItem.Controls(0).Controls(0), ScriptEditorControlMono).cbPythonEngine.SelectedIndex
                If interp = 0 Then
                    TaskHelper.Run(Sub() RunScript_IronPython("", script, fc, Nothing))
                Else
                    TaskHelper.Run(Sub() RunScript_PythonNET("", script, fc))
                End If
            Else
                Dim script = DirectCast(Me.TabStripScripts.SelectedItem.Controls(0).Controls(0), ScriptEditorControl).txtScript.Text
                Dim interp = DirectCast(Me.TabStripScripts.SelectedItem.Controls(0).Controls(0), ScriptEditorControl).cbPythonEngine.SelectedIndex
                If interp = 0 Then
                    TaskHelper.Run(Sub() RunScript_IronPython("", script, fc, Nothing))
                Else
                    TaskHelper.Run(Sub() RunScript_PythonNET("", script, fc))
                End If
            End If
        End If

    End Sub

    Private Sub btnRunDebug_Click(sender As Object, e As EventArgs) Handles btnRunDebug.Click

        If CurrentlyDebugging Then

            DebuggingPaused = False

        Else

            Dim scripteditor = DirectCast(Me.TabStripScripts.SelectedItem.Controls(0).Controls(0), ScriptEditorControl)

            Dim script = scripteditor.txtScript.Text

            Dim interp = scripteditor.cbPythonEngine.SelectedIndex

            If interp = 0 Then

                Dim tv = scripteditor.tvVariables

                scripteditor.SplitContainer1.Panel2Collapsed = False

                CurrentlyDebugging = True
                btnRunDebug.ToolTipText = "Continue Debugging"
                btnStopDebug.Enabled = True

                CancelDebugToken = New CancellationTokenSource()

                Dim t = TaskHelper.Run(Sub() RunScript_IronPython("", script, fc, Sub(frame)

                                                                                      Dim breakpoints As New List(Of Integer)

                                                                                      Me.UIThreadInvoke(Sub() breakpoints = scripteditor.txtScript.GetBookmarks)

                                                                                      If breakpoints.Contains(frame.f_lineno) Then

                                                                                          DebuggingPaused = True

                                                                                          Me.UIThreadInvoke(Sub()

                                                                                                                scripteditor.txtScript.Lines(frame.f_lineno - 1).MarkerAdd(4)
                                                                                                                scripteditor.txtScript.Lines(frame.f_lineno - 1).MarkerAdd(5)
                                                                                                                scripteditor.txtScript.Lines(frame.f_lineno - 1).Goto()

                                                                                                                Dim vars As New List(Of Object)
                                                                                                                Dim names As New List(Of String)
                                                                                                                For Each item In frame.f_globals
                                                                                                                    names.Add(item.Key)
                                                                                                                    vars.Add(item.Value)
                                                                                                                Next

                                                                                                                tv.Model = New TypeBrowserModel(vars, names)

                                                                                                            End Sub)

                                                                                          While DebuggingPaused

                                                                                              If CancelDebugToken.IsCancellationRequested Then

                                                                                                  Me.UIThreadInvoke(Sub()

                                                                                                                        scripteditor.txtScript.Lines(frame.f_lineno - 1).MarkerDelete(4)
                                                                                                                        scripteditor.txtScript.Lines(frame.f_lineno - 1).MarkerDelete(5)

                                                                                                                        tv.Model = Nothing

                                                                                                                    End Sub)

                                                                                                  Throw New TaskCanceledException()

                                                                                              End If

                                                                                              Thread.Sleep(100)

                                                                                          End While

                                                                                          Me.UIThreadInvoke(Sub()

                                                                                                                scripteditor.txtScript.Lines(frame.f_lineno - 1).MarkerDelete(4)
                                                                                                                scripteditor.txtScript.Lines(frame.f_lineno - 1).MarkerDelete(5)

                                                                                                                tv.Model = Nothing

                                                                                                            End Sub)

                                                                                      End If

                                                                                  End Sub), CancelDebugToken.Token)

                t.ContinueWith(Sub()
                                   UIThread(Sub()
                                                CurrentlyDebugging = False
                                                btnRunDebug.ToolTipText = "Debug Script (Async)"
                                                scripteditor.SplitContainer1.Panel2Collapsed = True
                                                btnStopDebug.Enabled = False
                                            End Sub)
                               End Sub)

            Else

                MessageBox.Show("Python.NET script debugging is not supported.")

            End If

        End If


    End Sub

    Private Sub ToolStripButton1_Click(sender As Object, e As EventArgs) Handles btnStopDebug.Click

        CancelDebugToken.Cancel()

    End Sub

    Private Sub TsbInsertSnippet_DropDownOpening(sender As Object, e As EventArgs) Handles tsbInsertSnippet.DropDownOpening

        Dim gettsmi = GetPropertyTSMI
        Dim settsmi = SetPropertyTSMI

        If ShouldUpdateSnippets Then

            ShouldUpdateSnippets = False

            gettsmi.DropDownItems.Clear()
            settsmi.DropDownItems.Clear()

            For Each item In fc.SimulationObjects.Values.OrderBy(Function(x) x.GraphicObject.Tag)

                Dim itemtsmig As New ToolStripMenuItem
                itemtsmig.Text = item.GraphicObject.Tag

                gettsmi.DropDownItems.Add(itemtsmig)

                Dim itemtsmis As New ToolStripMenuItem
                itemtsmis.Text = item.GraphicObject.Tag

                settsmi.DropDownItems.Add(itemtsmis)

                If TypeOf item Is Streams.MaterialStream Then

                    ' set overall properties

                    itemtsmis.DropDownItems.Add(
                        CreateToolStripMenuItem("Stream Temperature", Sub()
                                                                          InsertText("# Define Stream Temperature")
                                                                          InsertText(System.Environment.NewLine)
                                                                          InsertText(System.Environment.NewLine)
                                                                          InsertText(String.Format("stream = Flowsheet.GetFlowsheetSimulationObject('{0}')", item.GraphicObject.Tag))
                                                                          InsertText(System.Environment.NewLine)
                                                                          InsertText(String.Format("stream.SetTemperature(value) # value must be in K"))
                                                                          InsertText(System.Environment.NewLine)
                                                                      End Sub))

                    itemtsmis.DropDownItems.Add(
                        CreateToolStripMenuItem("Stream Pressure", Sub()
                                                                       InsertText("# Define Stream Pressure")
                                                                       InsertText(System.Environment.NewLine)
                                                                       InsertText(System.Environment.NewLine)
                                                                       InsertText(String.Format("stream = Flowsheet.GetFlowsheetSimulationObject('{0}')", item.GraphicObject.Tag))
                                                                       InsertText(System.Environment.NewLine)
                                                                       InsertText(String.Format("stream.SetPressure(value) # value must be in Pa"))
                                                                       InsertText(System.Environment.NewLine)
                                                                   End Sub))

                    itemtsmis.DropDownItems.Add(
                        CreateToolStripMenuItem("Stream Enthalpy", Sub()
                                                                       InsertText("# Define Stream Enthalpy")
                                                                       InsertText(System.Environment.NewLine)
                                                                       InsertText(System.Environment.NewLine)
                                                                       InsertText(String.Format("stream = Flowsheet.GetFlowsheetSimulationObject('{0}')", item.GraphicObject.Tag))
                                                                       InsertText(System.Environment.NewLine)
                                                                       InsertText(String.Format("stream.SetMassEnthalpy(value) # value must be in kJ/kg"))
                                                                       InsertText(System.Environment.NewLine)
                                                                   End Sub))

                    itemtsmis.DropDownItems.Add(
                        CreateToolStripMenuItem("Stream Entropy", Sub()
                                                                      InsertText("# Define Stream Entropy")
                                                                      InsertText(System.Environment.NewLine)
                                                                      InsertText(System.Environment.NewLine)
                                                                      InsertText(String.Format("stream = Flowsheet.GetFlowsheetSimulationObject('{0}')", item.GraphicObject.Tag))
                                                                      InsertText(System.Environment.NewLine)
                                                                      InsertText(String.Format("stream.SetMassEntropy(value) # value must be in kJ/[kg.K]"))
                                                                      InsertText(System.Environment.NewLine)
                                                                  End Sub))

                    itemtsmis.DropDownItems.Add(
                        CreateToolStripMenuItem("Stream Mass Flow", Sub()
                                                                        InsertText("Define Stream Mass Flow")
                                                                        InsertText(System.Environment.NewLine)
                                                                        InsertText(System.Environment.NewLine)
                                                                        InsertText(String.Format("stream = Flowsheet.GetFlowsheetSimulationObject('{0}')", item.GraphicObject.Tag))
                                                                        InsertText(System.Environment.NewLine)
                                                                        InsertText(String.Format("stream.SetMassFlow(value) # value must be in kg/s"))
                                                                        InsertText(System.Environment.NewLine)
                                                                    End Sub))

                    itemtsmis.DropDownItems.Add(
                        CreateToolStripMenuItem("Stream Molar Flow", Sub()
                                                                         InsertText("# Define Stream Molar Flow")
                                                                         InsertText(System.Environment.NewLine)
                                                                         InsertText(System.Environment.NewLine)
                                                                         InsertText(String.Format("stream = Flowsheet.GetFlowsheetSimulationObject('{0}')", item.GraphicObject.Tag))
                                                                         InsertText(System.Environment.NewLine)
                                                                         InsertText(String.Format("stream.SetMolarFlow(value) # value must be in mol/s"))
                                                                         InsertText(System.Environment.NewLine)
                                                                     End Sub))

                    itemtsmis.DropDownItems.Add(
                        CreateToolStripMenuItem("Stream Volumetric Flow", Sub()
                                                                              InsertText("# Define Stream Volumetric Flow")
                                                                              InsertText(System.Environment.NewLine)
                                                                              InsertText(System.Environment.NewLine)
                                                                              InsertText(String.Format("stream = Flowsheet.GetFlowsheetSimulationObject('{0}')", item.GraphicObject.Tag))
                                                                              InsertText(System.Environment.NewLine)
                                                                              InsertText(String.Format("stream.SetVolumetricFlow(value) # value must be in m3/s"))
                                                                              InsertText(System.Environment.NewLine)
                                                                          End Sub))

                    itemtsmis.DropDownItems.Add(
                        CreateToolStripMenuItem("Stream Vapor Molar Fraction", Sub()
                                                                                   InsertText("# Set Stream Vapor Molar Fraction")
                                                                                   InsertText(System.Environment.NewLine)
                                                                                   InsertText(System.Environment.NewLine)
                                                                                   InsertText(String.Format("obj = Flowsheet.GetFlowsheetSimulationObject('{0}')", item.GraphicObject.Tag))
                                                                                   InsertText(System.Environment.NewLine)
                                                                                   InsertText(String.Format("obj.GetPhase('Vapor').Properties.molarfraction = value # number ranging from 0.0 to 1.0"))
                                                                                   InsertText(System.Environment.NewLine)
                                                                               End Sub))

                    itemtsmis.DropDownItems.Add(
                        CreateToolStripMenuItem("Stream Molar Composition", Sub()
                                                                                InsertText("# Define Stream Molar Composition")
                                                                                InsertText(System.Environment.NewLine)
                                                                                InsertText(System.Environment.NewLine)
                                                                                InsertText(String.Format("obj = Flowsheet.GetFlowsheetSimulationObject('{0}')", item.GraphicObject.Tag))
                                                                                InsertText(System.Environment.NewLine)
                                                                                InsertText(String.Format("value = System.Array[double]([0.1, ..., x])"))
                                                                                InsertText(System.Environment.NewLine)
                                                                                InsertText(String.Format("obj.SetOverallComposition(value) # value must be an array of mole fractions"))
                                                                                InsertText(System.Environment.NewLine)
                                                                            End Sub))

                    itemtsmis.DropDownItems.Add(
                        CreateToolStripMenuItem("Stream Flash Specification", Sub()
                                                                                  InsertText("# Define Stream Flash Specification")
                                                                                  InsertText(System.Environment.NewLine)
                                                                                  InsertText(System.Environment.NewLine)
                                                                                  InsertText(String.Format("obj = Flowsheet.GetFlowsheetSimulationObject('{0}')", item.GraphicObject.Tag))
                                                                                  InsertText(System.Environment.NewLine)
                                                                                  InsertText(String.Format("obj.SpecType = value # number from 0 to 5"))
                                                                                  InsertText(System.Environment.NewLine)
                                                                                  InsertText("# Accepted values:")
                                                                                  InsertText(System.Environment.NewLine)
                                                                                  InsertText("# 0: Temperature_and_Pressure")
                                                                                  InsertText(System.Environment.NewLine)
                                                                                  InsertText("# 1: Pressure_and_Enthalpy")
                                                                                  InsertText(System.Environment.NewLine)
                                                                                  InsertText("# 2: Pressure_and_Entropy")
                                                                                  InsertText(System.Environment.NewLine)
                                                                                  InsertText("# 3: Pressure_and_VaporFraction")
                                                                                  InsertText(System.Environment.NewLine)
                                                                                  InsertText("# 4: Temperature_and_VaporFraction")
                                                                                  InsertText(System.Environment.NewLine)
                                                                                  InsertText("# 5: Pressure_and_SolidFraction")
                                                                                  InsertText(System.Environment.NewLine)
                                                                              End Sub))

                    'get overall properties

                    itemtsmig.DropDownItems.Add(
                       CreateToolStripMenuItem("Stream Temperature", Sub()
                                                                         InsertText("# Get Stream Temperature")
                                                                         InsertText(System.Environment.NewLine)
                                                                         InsertText(System.Environment.NewLine)
                                                                         InsertText(String.Format("stream = Flowsheet.GetFlowsheetSimulationObject('{0}')", item.GraphicObject.Tag))
                                                                         InsertText(System.Environment.NewLine)
                                                                         InsertText(String.Format("value = stream.GetTemperature() # in K"))
                                                                         InsertText(System.Environment.NewLine)
                                                                     End Sub))

                    itemtsmig.DropDownItems.Add(
                        CreateToolStripMenuItem("Stream Pressure", Sub()
                                                                       InsertText("# Get Stream Pressure")
                                                                       InsertText(System.Environment.NewLine)
                                                                       InsertText(System.Environment.NewLine)
                                                                       InsertText(String.Format("stream = Flowsheet.GetFlowsheetSimulationObject('{0}')", item.GraphicObject.Tag))
                                                                       InsertText(System.Environment.NewLine)
                                                                       InsertText(String.Format("value = stream.GetPressure() # in Pa"))
                                                                       InsertText(System.Environment.NewLine)
                                                                   End Sub))

                    itemtsmig.DropDownItems.Add(
                        CreateToolStripMenuItem("Stream Enthalpy", Sub()
                                                                       InsertText("# Get Stream Enthalpy")
                                                                       InsertText(System.Environment.NewLine)
                                                                       InsertText(String.Format("stream = Flowsheet.GetFlowsheetSimulationObject('{0}')", item.GraphicObject.Tag))
                                                                       InsertText(System.Environment.NewLine)
                                                                       InsertText(String.Format("value = stream.GetMassEnthalpy() # in kJ/kg"))
                                                                       InsertText(System.Environment.NewLine)
                                                                   End Sub))

                    itemtsmig.DropDownItems.Add(
                        CreateToolStripMenuItem("Stream Entropy", Sub()
                                                                      InsertText("# Get Stream Entropy")
                                                                      InsertText(System.Environment.NewLine)
                                                                      InsertText(System.Environment.NewLine)
                                                                      InsertText(String.Format("stream = Flowsheet.GetFlowsheetSimulationObject('{0}')", item.GraphicObject.Tag))
                                                                      InsertText(System.Environment.NewLine)
                                                                      InsertText(String.Format("value = stream.GetMassEntropy() # in kJ/[kg.K]"))
                                                                      InsertText(System.Environment.NewLine)
                                                                  End Sub))

                    itemtsmig.DropDownItems.Add(
                        CreateToolStripMenuItem("Stream Mass Flow", Sub()
                                                                        InsertText("Get Stream Mass Flow")
                                                                        InsertText(System.Environment.NewLine)
                                                                        InsertText(System.Environment.NewLine)
                                                                        InsertText(String.Format("stream = Flowsheet.GetFlowsheetSimulationObject('{0}')", item.GraphicObject.Tag))
                                                                        InsertText(System.Environment.NewLine)
                                                                        InsertText(String.Format("value = stream.GetMassFlow() # in kg/s"))
                                                                        InsertText(System.Environment.NewLine)
                                                                    End Sub))

                    itemtsmig.DropDownItems.Add(
                        CreateToolStripMenuItem("Stream Molar Flow", Sub()
                                                                         InsertText("# Get Stream Molar Flow")
                                                                         InsertText(System.Environment.NewLine)
                                                                         InsertText(System.Environment.NewLine)
                                                                         InsertText(String.Format("stream = Flowsheet.GetFlowsheetSimulationObject('{0}')", item.GraphicObject.Tag))
                                                                         InsertText(System.Environment.NewLine)
                                                                         InsertText(String.Format("value = stream.GetMolarFlow() # in mol/s"))
                                                                         InsertText(System.Environment.NewLine)
                                                                     End Sub))

                    itemtsmig.DropDownItems.Add(
                        CreateToolStripMenuItem("Stream Volumetric Flow", Sub()
                                                                              InsertText("# Get Stream Volumetric Flow")
                                                                              InsertText(System.Environment.NewLine)
                                                                              InsertText(System.Environment.NewLine)
                                                                              InsertText(String.Format("stream = Flowsheet.GetFlowsheetSimulationObject('{0}')", item.GraphicObject.Tag))
                                                                              InsertText(System.Environment.NewLine)
                                                                              InsertText(String.Format("value = stream.GetVolumetricFlow() # in m3/s"))
                                                                              InsertText(System.Environment.NewLine)
                                                                          End Sub))

                    itemtsmig.DropDownItems.Add(
                        CreateToolStripMenuItem("Stream Vapor Molar Fraction", Sub()
                                                                                   InsertText("# Get Stream Vapor Molar Fraction")
                                                                                   InsertText(System.Environment.NewLine)
                                                                                   InsertText(System.Environment.NewLine)
                                                                                   InsertText(String.Format("obj = Flowsheet.GetFlowsheetSimulationObject('{0}')", item.GraphicObject.Tag))
                                                                                   InsertText(System.Environment.NewLine)
                                                                                   InsertText(String.Format("value = obj.GetPhase('Vapor').Properties.molarfraction # number ranging from 0.0 to 1.0"))
                                                                                   InsertText(System.Environment.NewLine)
                                                                               End Sub))

                    itemtsmig.DropDownItems.Add(
                        CreateToolStripMenuItem("Stream Molar Composition", Sub()
                                                                                InsertText("# Get Stream Molar Composition")
                                                                                InsertText(System.Environment.NewLine)
                                                                                InsertText(System.Environment.NewLine)
                                                                                InsertText(String.Format("obj = Flowsheet.GetFlowsheetSimulationObject('{0}')", item.GraphicObject.Tag))
                                                                                InsertText(System.Environment.NewLine)
                                                                                InsertText(String.Format("value = obj.GetOverallComposition() # array of mole fractions"))
                                                                                InsertText(System.Environment.NewLine)
                                                                            End Sub))

                    itemtsmig.DropDownItems.Add(
                        CreateToolStripMenuItem("Stream Flash Specification", Sub()
                                                                                  InsertText("# Get Stream Flash Specification")
                                                                                  InsertText(System.Environment.NewLine)
                                                                                  InsertText(System.Environment.NewLine)
                                                                                  InsertText(String.Format("obj = Flowsheet.GetFlowsheetSimulationObject('{0}')", item.GraphicObject.Tag))
                                                                                  InsertText(System.Environment.NewLine)
                                                                                  InsertText(String.Format("value = obj.SpecType # number from 0 to 5"))
                                                                                  InsertText(System.Environment.NewLine)
                                                                                  InsertText("# Current values:")
                                                                                  InsertText(System.Environment.NewLine)
                                                                                  InsertText("# 0: Temperature_and_Pressure")
                                                                                  InsertText(System.Environment.NewLine)
                                                                                  InsertText("# 1: Pressure_and_Enthalpy")
                                                                                  InsertText(System.Environment.NewLine)
                                                                                  InsertText("# 2: Pressure_and_Entropy")
                                                                                  InsertText(System.Environment.NewLine)
                                                                                  InsertText("# 3: Pressure_and_VaporFraction")
                                                                                  InsertText(System.Environment.NewLine)
                                                                                  InsertText("# 4: Temperature_and_VaporFraction")
                                                                                  InsertText(System.Environment.NewLine)
                                                                                  InsertText("# 5: Pressure_and_SolidFraction")
                                                                                  InsertText(System.Environment.NewLine)
                                                                              End Sub))

                    Dim p1 = CreateToolStripMenuItem(DWSIM.App.GetLocalString("PhaseProperties") & " - " & DWSIM.App.GetLocalString("Overall"), Nothing)
                    Dim p2 = CreateToolStripMenuItem(DWSIM.App.GetLocalString("PhaseProperties") & " - " & DWSIM.App.GetLocalString("Vapor"), Nothing)
                    Dim p3 = CreateToolStripMenuItem(DWSIM.App.GetLocalString("PhaseProperties") & " - " & DWSIM.App.GetLocalString("OverallLiquid"), Nothing)
                    Dim p4 = CreateToolStripMenuItem(DWSIM.App.GetLocalString("PhaseProperties") & " - " & DWSIM.App.GetLocalString("Liquid1"), Nothing)
                    Dim p5 = CreateToolStripMenuItem(DWSIM.App.GetLocalString("PhaseProperties") & " - " & DWSIM.App.GetLocalString("Liquid2"), Nothing)
                    Dim p6 = CreateToolStripMenuItem(DWSIM.App.GetLocalString("PhaseProperties") & " - " & DWSIM.App.GetLocalString("Solid"), Nothing)

                    Dim pprops = GetType(Thermodynamics.BaseClasses.PhaseProperties).GetRuntimeProperties()

                    For Each pitem In pprops
                        p1.DropDownItems.Add(CreateToolStripMenuItem(pitem.Name, Sub()
                                                                                     InsertText("# Get Stream Phase Property: " & pitem.Name)
                                                                                     InsertText(System.Environment.NewLine)
                                                                                     InsertText(System.Environment.NewLine)
                                                                                     InsertText(String.Format("obj = Flowsheet.GetFlowsheetSimulationObject('{0}')", item.GraphicObject.Tag))
                                                                                     InsertText(System.Environment.NewLine)
                                                                                     InsertText(String.Format("value = obj.GetPhase('Overall').Properties.{0}", pitem.Name))
                                                                                     InsertText(System.Environment.NewLine)
                                                                                 End Sub))
                        p2.DropDownItems.Add(CreateToolStripMenuItem(pitem.Name, Sub()
                                                                                     InsertText("# Get Stream Phase Property: " & pitem.Name)
                                                                                     InsertText(System.Environment.NewLine)
                                                                                     InsertText(System.Environment.NewLine)
                                                                                     InsertText(String.Format("obj = Flowsheet.GetFlowsheetSimulationObject('{0}')", item.GraphicObject.Tag))
                                                                                     InsertText(System.Environment.NewLine)
                                                                                     InsertText(String.Format("value = obj.GetPhase('Vapor').Properties.{0}", pitem.Name))
                                                                                     InsertText(System.Environment.NewLine)
                                                                                 End Sub))
                        p3.DropDownItems.Add(CreateToolStripMenuItem(pitem.Name, Sub()
                                                                                     InsertText("# Get Stream Phase Property: " & pitem.Name)
                                                                                     InsertText(System.Environment.NewLine)
                                                                                     InsertText(System.Environment.NewLine)
                                                                                     InsertText(String.Format("obj = Flowsheet.GetFlowsheetSimulationObject('{0}')", item.GraphicObject.Tag))
                                                                                     InsertText(System.Environment.NewLine)
                                                                                     InsertText(String.Format("value = obj.GetPhase('OverallLiquid').Properties.{0}", pitem.Name))
                                                                                     InsertText(System.Environment.NewLine)
                                                                                 End Sub))
                        p4.DropDownItems.Add(CreateToolStripMenuItem(pitem.Name, Sub()
                                                                                     InsertText("# Get Stream Phase Property: " & pitem.Name)
                                                                                     InsertText(System.Environment.NewLine)
                                                                                     InsertText(System.Environment.NewLine)
                                                                                     InsertText(String.Format("obj = Flowsheet.GetFlowsheetSimulationObject('{0}')", item.GraphicObject.Tag))
                                                                                     InsertText(System.Environment.NewLine)
                                                                                     InsertText(String.Format("value = obj.GetPhase('Liquid1').Properties.{0}", pitem.Name))
                                                                                     InsertText(System.Environment.NewLine)
                                                                                 End Sub))
                        p5.DropDownItems.Add(CreateToolStripMenuItem(pitem.Name, Sub()
                                                                                     InsertText("# Get Stream Phase Property: " & pitem.Name)
                                                                                     InsertText(System.Environment.NewLine)
                                                                                     InsertText(System.Environment.NewLine)
                                                                                     InsertText(String.Format("obj = Flowsheet.GetFlowsheetSimulationObject('{0}')", item.GraphicObject.Tag))
                                                                                     InsertText(System.Environment.NewLine)
                                                                                     InsertText(String.Format("value = obj.GetPhase('Liquid2').Properties.{0}", pitem.Name))
                                                                                     InsertText(System.Environment.NewLine)
                                                                                 End Sub))
                        p6.DropDownItems.Add(CreateToolStripMenuItem(pitem.Name, Sub()
                                                                                     InsertText("# Get Stream Phase Property: " & pitem.Name)
                                                                                     InsertText(System.Environment.NewLine)
                                                                                     InsertText(System.Environment.NewLine)
                                                                                     InsertText(String.Format("obj = Flowsheet.GetFlowsheetSimulationObject('{0}')", item.GraphicObject.Tag))
                                                                                     InsertText(System.Environment.NewLine)
                                                                                     InsertText(String.Format("value = obj.GetPhase('Solid').Properties.{0}", pitem.Name))
                                                                                     InsertText(System.Environment.NewLine)
                                                                                 End Sub))
                    Next

                    Dim pc = CreateToolStripMenuItem(DWSIM.App.GetLocalString("Compounds"), Nothing)

                    itemtsmig.DropDownItems.AddRange({pc, p1, p2, p3, p4, p5, p6})

                    Dim ccprops = GetType(Thermodynamics.BaseClasses.ConstantProperties).GetRuntimeProperties()
                    Dim cpprops = GetType(Thermodynamics.BaseClasses.Compound).GetRuntimeProperties()

                    For Each c In fc.SelectedCompounds.Values
                        Dim cx = CreateToolStripMenuItem(c.Name, Nothing)
                        Dim c1 = CreateToolStripMenuItem(DWSIM.App.GetLocalString("ConstantProperties"), Nothing)
                        Dim c2 = CreateToolStripMenuItem(DWSIM.App.GetLocalString("PhaseProperties"), Nothing)
                        pc.DropDownItems.Add(cx)
                        cx.DropDownItems.Add(c1)
                        cx.DropDownItems.Add(c2)
                        For Each cc1 In ccprops
                            c1.DropDownItems.Add(CreateToolStripMenuItem(cc1.Name, Sub()
                                                                                       InsertText("# Get Compound Constant Property: " & cc1.Name)
                                                                                       InsertText(System.Environment.NewLine)
                                                                                       InsertText(System.Environment.NewLine)
                                                                                       InsertText(String.Format("compound = Flowsheet.SelectedCompounds['{0}']", c.Name))
                                                                                       InsertText(System.Environment.NewLine)
                                                                                       InsertText(String.Format("propval = compound.{0}", cc1.Name))
                                                                                       InsertText(System.Environment.NewLine)
                                                                                   End Sub))
                        Next
                        For Each cp1 In cpprops
                            c2.DropDownItems.Add(CreateToolStripMenuItem(cp1.Name & " (Mixture Phase)", Sub()
                                                                                                            InsertText("# Get Compound Property in Mixture (Overall) Phase: " & cp1.Name)
                                                                                                            InsertText(System.Environment.NewLine)
                                                                                                            InsertText(System.Environment.NewLine)
                                                                                                            InsertText(String.Format("obj = Flowsheet.GetFlowsheetSimulationObject('{0}')", item.GraphicObject.Tag))
                                                                                                            InsertText(System.Environment.NewLine)
                                                                                                            InsertText(String.Format("propval = obj.GetPhase('Overall').Compounds['{0}'].{1}", c.Name, cp1.Name))
                                                                                                            InsertText(System.Environment.NewLine)
                                                                                                        End Sub))
                            c2.DropDownItems.Add(CreateToolStripMenuItem(cp1.Name & " (Vapor Phase)", Sub()
                                                                                                          InsertText("# Get Compound Property in Vapor Phase: " & cp1.Name)
                                                                                                          InsertText(System.Environment.NewLine)
                                                                                                          InsertText(System.Environment.NewLine)
                                                                                                          InsertText(String.Format("obj = Flowsheet.GetFlowsheetSimulationObject('{0}')", item.GraphicObject.Tag))
                                                                                                          InsertText(System.Environment.NewLine)
                                                                                                          InsertText(String.Format("propval = obj.GetPhase('Vapor').Compounds['{0}'].{1}", c.Name, cp1.Name))
                                                                                                          InsertText(System.Environment.NewLine)
                                                                                                      End Sub))
                            c2.DropDownItems.Add(CreateToolStripMenuItem(cp1.Name & " (Overall Liquid Phase)", Sub()
                                                                                                                   InsertText("# Get Compound Property in Overall Liquid Phase: " & cp1.Name)
                                                                                                                   InsertText(System.Environment.NewLine)
                                                                                                                   InsertText(System.Environment.NewLine)
                                                                                                                   InsertText(String.Format("obj = Flowsheet.GetFlowsheetSimulationObject('{0}')", item.GraphicObject.Tag))
                                                                                                                   InsertText(System.Environment.NewLine)
                                                                                                                   InsertText(String.Format("propval = obj.GetPhase('OverallLiquid').Compounds['{0}'].{1}", c.Name, cp1.Name))
                                                                                                                   InsertText(System.Environment.NewLine)
                                                                                                               End Sub))
                            c2.DropDownItems.Add(CreateToolStripMenuItem(cp1.Name & " (Liquid Phase 1)", Sub()
                                                                                                             InsertText("# Get Compound Property in Liquid Phase 1: " & cp1.Name)
                                                                                                             InsertText(System.Environment.NewLine)
                                                                                                             InsertText(System.Environment.NewLine)
                                                                                                             InsertText(String.Format("obj = Flowsheet.GetFlowsheetSimulationObject('{0}')", item.GraphicObject.Tag))
                                                                                                             InsertText(System.Environment.NewLine)
                                                                                                             InsertText(String.Format("propval = obj.GetPhase('Liquid1').Compounds['{0}'].{1}", c.Name, cp1.Name))
                                                                                                             InsertText(System.Environment.NewLine)
                                                                                                         End Sub))
                            c2.DropDownItems.Add(CreateToolStripMenuItem(cp1.Name & " (Liquid Phase 2)", Sub()
                                                                                                             InsertText("# Get Compound Property in Liquid Phase 2: " & cp1.Name)
                                                                                                             InsertText(System.Environment.NewLine)
                                                                                                             InsertText(System.Environment.NewLine)
                                                                                                             InsertText(String.Format("obj = Flowsheet.GetFlowsheetSimulationObject('{0}')", item.GraphicObject.Tag))
                                                                                                             InsertText(System.Environment.NewLine)
                                                                                                             InsertText(String.Format("propval = obj.GetPhase('Liquid2').Compounds['{0}'].{1}", c.Name, cp1.Name))
                                                                                                             InsertText(System.Environment.NewLine)
                                                                                                         End Sub))
                            c2.DropDownItems.Add(CreateToolStripMenuItem(cp1.Name & " (Solid Phase)", Sub()
                                                                                                          InsertText("# Get Compound Property in Solid Phase: " & cp1.Name)
                                                                                                          InsertText(System.Environment.NewLine)
                                                                                                          InsertText(System.Environment.NewLine)
                                                                                                          InsertText(String.Format("obj = Flowsheet.GetFlowsheetSimulationObject('{0}')", item.GraphicObject.Tag))
                                                                                                          InsertText(System.Environment.NewLine)
                                                                                                          InsertText(String.Format("propval = obj.GetPhase('Solid').Compounds['{0}'].{1}", c.Name, cp1.Name))
                                                                                                          InsertText(System.Environment.NewLine)
                                                                                                      End Sub))
                        Next
                    Next

                Else

                    Dim itemprops = item.GetType.GetRuntimeProperties()
                    Dim itemfields = item.GetType.GetRuntimeFields()

                    For Each pitem In itemprops

                        itemtsmis.DropDownItems.Add(
                        CreateToolStripMenuItem("Object Property: " & pitem.Name, Sub()
                                                                                      InsertText("# Define Object Property: " & pitem.Name)
                                                                                      InsertText(System.Environment.NewLine)
                                                                                      InsertText(System.Environment.NewLine)
                                                                                      InsertText(String.Format("obj = Flowsheet.GetFlowsheetSimulationObject('{0}')", item.GraphicObject.Tag))
                                                                                      InsertText(System.Environment.NewLine)
                                                                                      InsertText(String.Format("obj.{0} = value", pitem.Name))
                                                                                      InsertText(System.Environment.NewLine)
                                                                                      If pitem.PropertyType.BaseType Is GetType([Enum]) Then
                                                                                          InsertText(System.Environment.NewLine)
                                                                                          InsertText("# This property is an Enumeration (Enum) type.")
                                                                                          InsertText(System.Environment.NewLine)
                                                                                          InsertText(String.Format("# Full type name: {0}", pitem.PropertyType.ToString.Replace("+", ".")))
                                                                                          InsertText(System.Environment.NewLine)
                                                                                          InsertText(System.Environment.NewLine)
                                                                                          InsertText("# Accepted enumeration values:")
                                                                                          InsertText(System.Environment.NewLine)
                                                                                          For Each etype In [Enum].GetNames(pitem.PropertyType)
                                                                                              InsertText(String.Format("# {0}.{1}", pitem.PropertyType.ToString.Replace("+", "."), etype))
                                                                                              InsertText(System.Environment.NewLine)
                                                                                          Next
                                                                                          InsertText(System.Environment.NewLine)
                                                                                          InsertText(System.Environment.NewLine)
                                                                                          InsertText("# example usage:")
                                                                                          InsertText(System.Environment.NewLine)
                                                                                          InsertText(String.Format("# obj.{0} = {1}.{2}", pitem.Name, pitem.PropertyType.ToString.Replace("+", "."), [Enum].GetNames(pitem.PropertyType)(0)))
                                                                                      End If
                                                                                  End Sub))

                        itemtsmig.DropDownItems.Add(
                            CreateToolStripMenuItem("Object Property: " & pitem.Name, Sub()
                                                                                          InsertText("# Get Object Property: " & pitem.Name)
                                                                                          InsertText(System.Environment.NewLine)
                                                                                          InsertText(System.Environment.NewLine)
                                                                                          InsertText(String.Format("obj = Flowsheet.GetFlowsheetSimulationObject('{0}')", item.GraphicObject.Tag))
                                                                                          InsertText(System.Environment.NewLine)
                                                                                          InsertText(String.Format("value = obj.{0}", pitem.Name))
                                                                                          InsertText(System.Environment.NewLine)
                                                                                          If pitem.PropertyType.BaseType Is GetType([Enum]) Then
                                                                                              InsertText(System.Environment.NewLine)
                                                                                              InsertText("# This property is an Enumeration (Enum) type.")
                                                                                              InsertText(System.Environment.NewLine)
                                                                                              InsertText(String.Format("# Full type name: {0}", pitem.PropertyType.ToString.Replace("+", ".")))
                                                                                              InsertText(System.Environment.NewLine)
                                                                                              InsertText(System.Environment.NewLine)
                                                                                              InsertText("# Possible enumeration values:")
                                                                                              InsertText(System.Environment.NewLine)
                                                                                              For Each etype In [Enum].GetNames(pitem.PropertyType)
                                                                                                  InsertText(String.Format("# {0}.{1}", pitem.PropertyType.ToString.Replace("+", "."), etype))
                                                                                                  InsertText(System.Environment.NewLine)
                                                                                              Next
                                                                                          End If
                                                                                      End Sub))

                    Next

                End If

            Next

        End If

    End Sub

    Public Function CreateToolStripMenuItem(text As String, clickaction As Action) As ToolStripMenuItem

        Dim tsmi As New ToolStripMenuItem With {.Text = DWSIM.App.GetLocalString(text)}
        AddHandler tsmi.Click, Sub()
                                   If clickaction IsNot Nothing Then Me.UIThread(clickaction)
                               End Sub

        Return tsmi

    End Function

    Public Sub InsertText(text As String)

        If Not DWSIM.App.IsRunningOnMono Then
            Dim scontrol As ScriptEditorControl = DirectCast(TabStripScripts.SelectedItem.Controls(0).Controls(0), ScriptEditorControl)
            scontrol.txtScript.InsertText(scontrol.txtScript.CurrentPosition, text)
            scontrol.txtScript.CurrentPosition += text.Length + 1
            scontrol.txtScript.SelectionStart = scontrol.txtScript.CurrentPosition
            scontrol.txtScript.SelectionEnd = scontrol.txtScript.CurrentPosition
        Else
            Dim scontrol As ScriptEditorControlMono = DirectCast(TabStripScripts.SelectedItem.Controls(0).Controls(0), ScriptEditorControlMono)
            scontrol.txtScript.Text = scontrol.txtScript.Text.Insert(scontrol.txtScript.SelectionStart, text)
            scontrol.txtScript.SelectionStart += text.Length + 1
            scontrol.txtScript.SelectionLength = 0
        End If

    End Sub

    Private Sub ToolStripButton1_Click_1(sender As Object, e As EventArgs) Handles ToolStripButton1.Click

        Dim filename = TabStripScripts.SelectedItem.Title

        Dim filePickerForm As IFilePicker = FilePickerService.GetInstance().GetFilePicker()

        filePickerForm.SuggestedFilename = filename

        Dim handler As IVirtualFile = filePickerForm.ShowSaveDialog(
                New List(Of FilePickerAllowedType) From {New FilePickerAllowedType("Python Script File", "*.py")})

        If handler IsNot Nothing Then
            Dim scontrol As ScriptEditorControl = DirectCast(TabStripScripts.SelectedItem.Controls(0).Controls(0), ScriptEditorControl)
            Dim text = scontrol.txtScript.Text
            Using stream As New IO.MemoryStream()
                Using writer As New StreamWriter(stream) With {.AutoFlush = True}
                    writer.Write(text)
                    handler.Write(stream)
                End Using
            End Using
        End If

    End Sub

    Private Sub ToolStripButton2_Click(sender As Object, e As EventArgs) Handles ToolStripButton2.Click

        Dim filePickerForm As IFilePicker = FilePickerService.GetInstance().GetFilePicker()

        Dim handler As IVirtualFile = filePickerForm.ShowOpenDialog(
            New List(Of FilePickerAllowedType) From {New FilePickerAllowedType("Python Script File", "*.py")})

        If handler IsNot Nothing Then
            Dim scripttext = handler.ReadAllText()
            Dim scr As New Script() With
                {.ID = Guid.NewGuid().ToString,
                .Title = Path.GetFileNameWithoutExtension(handler.Filename),
                .Linked = False,
                .ScriptText = scripttext,
                .PythonInterpreter = Scripts.Interpreter.IronPython}
            InsertScriptTab(scr)
        End If

    End Sub

    Private Sub FormScript_Shown(sender As Object, e As EventArgs) Handles Me.Shown
        FormMain.TranslateFormFunction?.Invoke(Me)
    End Sub
End Class

Public Class DataGridViewTextStream

    Inherits MemoryStream
    Private target As FormFlowsheet

    Public Sub New(ByVal target As FormFlowsheet)
        Me.target = target
    End Sub

    Public Overrides Sub Write(ByVal buffer As Byte(), ByVal offset As Integer, ByVal count As Integer)
        Dim output As String = Encoding.UTF8.GetString(buffer, offset, count)
        target.WriteToLog(output, Color.DarkGray, MessageType.Information)
    End Sub

End Class



