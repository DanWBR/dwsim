Imports System.Windows.Forms.Integration

Public Class FormInteractiveIronPythonConsole

    Inherits WeifenLuo.WinFormsUI.Docking.DockContent

    Public Property Flowsheet As Interfaces.IFlowsheet

    Private LoadedAssemblies As Boolean = False

    Private wpfCtrl As IronPythonConsole.PythonConsoleWindow

    Private Sub FormInteractiveIronPythonConsole_Load(sender As Object, e As EventArgs) Handles MyBase.Load

        Width = 600

        Me.AutoHidePortion = 600

        ExtensionMethods.ChangeDefaultFont(Me)

        Dim ctrlHost = New ElementHost()
        ctrlHost.Dock = DockStyle.Fill
        Me.Panel1.Controls.Add(ctrlHost)
        wpfCtrl = New IronPythonConsole.PythonConsoleWindow()
        wpfCtrl.InitializeComponent()
        ctrlHost.Child = wpfCtrl

        AddHandler wpfCtrl.ConsoleInitialized, Sub(s2, e2)

                                                   wpfCtrl.PythonConsole.UpdateVariables =
                                                                     Sub(scope)

                                                                         Dim engine = scope.Engine

                                                                         If Not LoadedAssemblies Then
                                                                             engine.Runtime.LoadAssembly(GetType(System.String).Assembly)
                                                                             engine.Runtime.LoadAssembly(GetType(Thermodynamics.BaseClasses.ConstantProperties).Assembly)
                                                                             engine.Runtime.LoadAssembly(GetType(Drawing.SkiaSharp.GraphicObjects.GraphicObject).Assembly)
                                                                             engine.Runtime.LoadAssembly(GetType(UnitOperations.UnitOperations.Compressor).Assembly)
                                                                             LoadedAssemblies = True
                                                                         End If

                                                                         Dim scriptSource = engine.CreateScriptSourceFromString(My.Resources.pythonfunctions, Microsoft.Scripting.SourceCodeKind.Statements)
                                                                         scriptSource.Execute(scope)

                                                                         scope.SetVariable("Flowsheet", Flowsheet)
                                                                         scope.SetVariable("Spreadsheet", Flowsheet.FormSpreadsheet.Spreadsheet)

                                                                         For Each obj As SharedClasses.UnitOperations.BaseClass In Flowsheet.Collections.FlowsheetObjectCollection.Values
                                                                             scope.SetVariable(obj.GraphicObject.Tag.Replace("-", "").Replace(" ", "").Replace("_", ""), obj)
                                                                         Next

                                                                     End Sub
                                               End Sub

    End Sub

    Private Sub FormInteractiveIronPythonConsole_Shown(sender As Object, e As EventArgs) Handles Me.Shown


    End Sub

End Class