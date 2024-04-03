'Natural Gas Properties Plugin for DWSIM
'Copyright 2010-2014 Daniel Wagner

Imports FileHelpers
Imports DWSIM
Imports System.Windows.Forms
Imports System.Linq
Imports DWSIM.Thermodynamics.BaseClasses
Imports System.Threading.Tasks
Imports DWSIM.Thermodynamics.PropertyPackages
Imports DWSIM.SharedClasses.SystemsOfUnits
Imports DWSIM.Thermodynamics
Imports DWSIM.SharedClasses
Imports DWSIM.Interfaces.Enums.GraphicObjects

Public Class Form1

    Inherits WeifenLuo.WinFormsUI.Docking.DockContent

    'flowsheet reference
    Public fsheet As DWSIM.FormFlowsheet


    Private Sub Form1_FormClosing(sender As Object, e As FormClosingEventArgs) Handles Me.FormClosing

        'remove SelectedObjectChanged event handler

        Dim eventhandler As DWSIM.Drawing.SkiaSharp.GraphicsSurface.SelectionChangedEventHandler = AddressOf SelectedObjectChanged

        RemoveHandler fsheet.FormSurface.FlowsheetSurface.SelectionChanged, eventhandler

        'For Each f In fsheet.Collections.FlowsheetObjectCollection.Values
        '    If f.GraphicObject.ObjectType = ObjectType.FlowsheetUO Then
        '        RemoveHandler DirectCast(f, DWSIM.DWSIM.SimulationObjects.UnitOperations.Flowsheet).Fsheet.FormSurface.ObjectSelected, eventhandler
        '    End If
        'Next

        My.Settings.Save()

    End Sub

    Private Sub Form1_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load

        'add SelectedObjectChanged event handler

        Dim eventhandler As DWSIM.Drawing.SkiaSharp.GraphicsSurface.SelectionChangedEventHandler = AddressOf SelectedObjectChanged

        AddHandler fsheet.FormSurface.FlowsheetSurface.SelectionChanged, eventhandler

        'For Each f In fsheet.Collections.FlowsheetObjectCollection.Values
        '    If f.GraphicObject.ObjectType = ObjectType.FlowsheetUO Then
        '        AddHandler DirectCast(f, DWSIM.DWSIM.SimulationObjects.UnitOperations.Flowsheet).Fsheet.FormSurface.ObjectSelected, eventhandler
        '    End If
        'Next

    End Sub

    Sub SelectedObjectChanged(ByVal sender As Object, ByVal e As DWSIM.Drawing.SkiaSharp.SelectionChangedEventArgs)

        Me.lblStream.Text = ""
        Me.lblCalcd.Text = ""
        Me.lblVapOnly.Text = ""

        Me.Invalidate()

        'check if we have a selected object.
        If Not fsheet.FormSurface.FlowsheetSurface.SelectedObject Is Nothing Then

            Dim p As New Populate()
            p.Populate(fsheet, Me)

        End If

    End Sub

End Class
