Imports System.IO
Imports System.Text
Imports Microsoft.Scripting.Hosting

Imports System.Drawing.Text
Imports System.Reflection
Imports System.ComponentModel
Imports FarsiLibrary.Win
Imports System.Threading
Imports Python.Runtime
Imports System.Threading.Tasks
Imports DWSIM.SharedClasses.DWSIM.Flowsheet
Imports IronPython.Hosting

<System.Serializable()> Public Class FormScript

    Inherits WeifenLuo.WinFormsUI.Docking.DockContent

    Public fc As FormFlowsheet
    Private reader As Jolt.XmlDocCommentReader

    Private CurrentlyDebugging As Boolean = False
    Private DebuggingPaused As Boolean = False
    Private CancelDebugToken As CancellationTokenSource

    Private Sub FormVBScript_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Me.Load

        If Not DWSIM.App.IsRunningOnMono Then reader = New Jolt.XmlDocCommentReader(My.Application.Info.DirectoryPath & Path.DirectorySeparatorChar & "DWSIM.xml")

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
        Else
            tscb1.SelectedIndex = 0
        End If

        'load existing scripts
        For Each s As Script In fc.ScriptCollection.Values
            InsertScriptTab(s)
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
                    RunScript_IronPython(script, fc, Nothing)
                Else
                    RunScript_PythonNET(script, fc)
                End If
            Else
                Dim script = DirectCast(Me.TabStripScripts.SelectedItem.Controls(0).Controls(0), ScriptEditorControl).txtScript.Text
                Dim interp = DirectCast(Me.TabStripScripts.SelectedItem.Controls(0).Controls(0), ScriptEditorControl).cbPythonEngine.SelectedIndex
                If interp = 0 Then
                    RunScript_IronPython(script, fc, Nothing)
                Else
                    RunScript_PythonNET(script, fc)
                End If
            End If
        End If

    End Sub

    Public Shared Sub RunScript_IronPython(scripttext As String, fsheet As FormFlowsheet, debuggingstep As Action(Of IronPython.Runtime.Exceptions.TraceBackFrame))

        Dim scope As Microsoft.Scripting.Hosting.ScriptScope
        Dim engine As Microsoft.Scripting.Hosting.ScriptEngine

        Dim opts As New Dictionary(Of String, Object)()

        opts("Frames") = Microsoft.Scripting.Runtime.ScriptingRuntimeHelpers.True

        If debuggingstep IsNot Nothing Then
            opts("Debug") = Microsoft.Scripting.Runtime.ScriptingRuntimeHelpers.True
        End If

        engine = IronPython.Hosting.Python.CreateEngine(opts)

        Dim paths(My.Settings.ScriptPaths.Count - 1) As String

        My.Settings.ScriptPaths.CopyTo(paths, 0)
        Try
            engine.SetSearchPaths(paths)
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
        scope.SetVariable("Spreadsheet", fsheet.FormSpreadsheet)

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
                fsheet.WriteToLog("Error running script: " & ops.FormatException(ex).ToString, Color.Red, MessageType.GeneralError)
            End If
        Finally
            engine.Runtime.Shutdown()
            engine = Nothing
            scope = Nothing
            source = Nothing
        End Try

    End Sub

    Public Shared Sub RunScript_PythonNET(scripttext As String, fsheet As FormFlowsheet)

        If Not Settings.PythonInitialized Then

            Dim t As Task = Task.Factory.StartNew(Sub()
                                                      fsheet.RunCodeOnUIThread(Sub()
                                                                                   If Not GlobalSettings.Settings.IsRunningOnMono() Then
                                                                                       PythonEngine.PythonHome = GlobalSettings.Settings.PythonPath
                                                                                   End If
                                                                                   PythonEngine.Initialize()
                                                                                   Settings.PythonInitialized = True
                                                                               End Sub)
                                                  End Sub)
            t.Wait()

            Dim t2 As Task = Task.Factory.StartNew(Sub()
                                                       fsheet.RunCodeOnUIThread(Sub()
                                                                                    PythonEngine.BeginAllowThreads()
                                                                                End Sub)
                                                   End Sub)
            t2.Wait()

        End If

        Dim t3 As Task = Task.Factory.StartNew(Sub()
                                                   fsheet.RunCodeOnUIThread(Sub()
                                                                                Using Py.GIL

                                                                                    Try

                                                                                        Dim sys As Object = PythonEngine.ImportModule("sys")

                                                                                        If Not GlobalSettings.Settings.IsRunningOnMono() Then
                                                                                            Dim codeToRedirectOutput As String = "import sys" & vbCrLf + "from io import BytesIO as StringIO" & vbCrLf + "sys.stdout = mystdout = StringIO()" & vbCrLf + "sys.stdout.flush()" & vbCrLf + "sys.stderr = mystderr = StringIO()" & vbCrLf + "sys.stderr.flush()"
                                                                                            PythonEngine.RunSimpleString(codeToRedirectOutput)
                                                                                        End If

                                                                                        Dim locals As New PyDict()

                                                                                        locals.SetItem("Plugins", My.Application.UtilityPlugins.ToPython)
                                                                                        locals.SetItem("Flowsheet", fsheet.ToPython)
                                                                                        locals.SetItem("Spreadsheet", fsheet.FormSpreadsheet.ToPython)
                                                                                        Dim Solver As New FlowsheetSolver.FlowsheetSolver
                                                                                        locals.SetItem("Solver", Solver.ToPython)

                                                                                        PythonEngine.Exec(scripttext, Nothing, locals.Handle)

                                                                                        If Not GlobalSettings.Settings.IsRunningOnMono() Then
                                                                                            fsheet.WriteToLog(sys.stdout.getvalue().ToString, Color.Blue, MessageType.Information)
                                                                                        End If

                                                                                    Catch ex As Exception

                                                                                        If My.Application.CommandLineMode Then
                                                                                            Console.WriteLine()
                                                                                            Console.WriteLine("Error running script: " & ex.ToString)
                                                                                            Console.WriteLine()
                                                                                        Else
                                                                                            fsheet.WriteToLog("Error running script: " & ex.Message.ToString, Color.Red, MessageType.GeneralError)
                                                                                        End If

                                                                                    Finally

                                                                                    End Try

                                                                                End Using
                                                                            End Sub)
                                               End Sub)
        t3.Wait()

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

    Private Sub ToolStripButton2_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)
        If Me.ofd1.ShowDialog = Windows.Forms.DialogResult.OK Then
            For Each fname As String In Me.ofd1.FileNames
                Me.ListBox1.Items.Add(fname)
            Next
        End If
    End Sub

    Private Sub ToolStripButton3_Click(ByVal sender As System.Object, ByVal e As System.EventArgs)
        If Me.ListBox1.SelectedItems.Count > 0 Then
            Dim names As New ArrayList
            For Each fname As Object In Me.ListBox1.SelectedItems
                names.Add(fname)
            Next
            For Each fname As String In names
                Me.ListBox1.Items.Remove(fname)
            Next
            names = Nothing
        End If
    End Sub

    Private Sub SaveToolStripButton_Click(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles SaveToolStripButton.Click

        UpdateScripts()

    End Sub

    Public Sub UpdateScripts()

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

        InsertScriptTab(New Script())

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
                .reader = reader

                .chkLink.Checked = scriptdata.Linked

                p.Controls.Add(scontrol)

                Dim stab As New FATabStripItem()
                stab.Controls.Add(p)
                stab.Tag = scriptdata.ID
                If scriptdata.Title = "" Then stab.Title = "Script" & TabStripScripts.Items.Count + 1 Else stab.Title = scriptdata.Title

                TabStripScripts.AddTab(stab, True)

                TabStripScripts.SelectedItem = stab

                Me.tsTextBoxRename.Text = stab.Title

                Me.Invalidate()

                If scriptdata.LinkedObjectName <> "" Then
                    .cbLinkedObject.SelectedItem = fc.Collections.FlowsheetObjectCollection(scriptdata.LinkedObjectName).GraphicObject.Tag
                Else
                    Select Case scriptdata.LinkedObjectType
                        Case Scripts.ObjectType.Simulation
                            .cbLinkedObject.SelectedIndex = 0
                        Case Scripts.ObjectType.Solver
                            .cbLinkedObject.SelectedIndex = 1
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
                End Select

                .cbPythonEngine.SelectedIndex = scriptdata.PythonInterpreter

            End With

        Else

            Dim p As New Panel With {.Dock = DockStyle.Fill}
            Dim scontrol As New ScriptEditorControlMono With {.Dock = DockStyle.Fill}

            With scontrol

                AddHandler scontrol.txtScript.KeyDown, AddressOf scriptcontrol_KeyDown

                .txtScript.Font = New Font(tscb1.SelectedItem.ToString, tscb2.SelectedItem)
                .txtScript.Text = scriptdata.ScriptText

                .form = fc

                .chkLink.Checked = scriptdata.Linked

                p.Controls.Add(scontrol)

                Dim stab As New FATabStripItem()
                stab.Controls.Add(p)
                stab.Tag = scriptdata.ID
                If scriptdata.Title = "" Then stab.Title = "Script" & TabStripScripts.Items.Count + 1 Else stab.Title = scriptdata.Title

                TabStripScripts.AddTab(stab, True)

                TabStripScripts.SelectedItem = stab

                Me.tsTextBoxRename.Text = stab.Title

                Me.Invalidate()

                If scriptdata.LinkedObjectName <> "" Then
                    .cbLinkedObject.SelectedItem = fc.Collections.FlowsheetObjectCollection(scriptdata.LinkedObjectName).GraphicObject.Tag
                Else
                    Select Case scriptdata.LinkedObjectType
                        Case Scripts.ObjectType.Simulation
                            .cbLinkedObject.SelectedIndex = 0
                        Case Scripts.ObjectType.Solver
                            .cbLinkedObject.SelectedIndex = 1
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
                End Select

                .cbPythonEngine.SelectedIndex = scriptdata.PythonInterpreter

            End With

        End If

    End Sub

    Private Sub tsTextBoxRename_KeyDown(sender As Object, e As KeyEventArgs) Handles tsTextBoxRename.KeyDown
        If e.KeyCode = Keys.Enter Then
            TabStripScripts.SelectedItem.Title = tsTextBoxRename.Text
        End If
    End Sub

    Private Sub TabStripScripts_TabStripItemSelectionChanged(e As TabStripItemChangedEventArgs) Handles TabStripScripts.TabStripItemSelectionChanged
        tsTextBoxRename.Text = TabStripScripts.SelectedItem.Title
    End Sub

    Private Sub TabStripScripts_TabStripItemClosing(e As TabStripItemClosingEventArgs) Handles TabStripScripts.TabStripItemClosing
        If MessageBox.Show(DWSIM.App.GetLocalString("RemoveScriptQuestion"), "DWSIM", MessageBoxButtons.YesNo, MessageBoxIcon.Question) = Windows.Forms.DialogResult.No Then
            e.Cancel = True
        End If
    End Sub

    Private Sub HelpToolStripButton_Click(sender As Object, e As EventArgs) Handles HelpToolStripButton.Click
        Process.Start("http://dwsim.inforside.com.br/wiki/index.php?title=Using_the_IronPython_Script_Manager")
    End Sub

    Private Sub scriptcontrol_KeyDown(sender As Object, e As KeyEventArgs)

        If e.KeyCode = Keys.F5 Then btnRunAsync.PerformClick()
        If e.KeyCode = Keys.F5 And e.Modifiers = Keys.Shift Then btnRun.PerformClick()

    End Sub

    Private Sub APIHelptsbutton_Click(sender As Object, e As EventArgs) Handles APIHelptsbutton.Click
        Process.Start("http://dwsim.inforside.com.br/api_help/index.html")
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
                    Task.Factory.StartNew(Sub() RunScript_IronPython(script, fc, Nothing))
                Else
                    Task.Factory.StartNew(Sub() RunScript_PythonNET(script, fc))
                End If
            Else
                Dim script = DirectCast(Me.TabStripScripts.SelectedItem.Controls(0).Controls(0), ScriptEditorControl).txtScript.Text
                Dim interp = DirectCast(Me.TabStripScripts.SelectedItem.Controls(0).Controls(0), ScriptEditorControl).cbPythonEngine.SelectedIndex
                If interp = 0 Then
                    Task.Factory.StartNew(Sub() RunScript_IronPython(script, fc, Nothing))
                Else
                    Task.Factory.StartNew(Sub() RunScript_PythonNET(script, fc))
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

                Dim t = Task.Factory.StartNew(Sub() RunScript_IronPython(script, fc, Sub(frame)

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

                                                                                     End Sub), CancelDebugToken)

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



