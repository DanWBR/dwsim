'    Custom (Scripting) Unit Operation Calculation Routines 
'    Copyright 2010-2011 Daniel Wagner O. de Medeiros
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

Imports DWSIM.DrawingTools.GraphicObjects
Imports DWSIM.Thermodynamics
Imports DWSIM.Thermodynamics.Streams
Imports DWSIM.SharedClasses
Imports System.Windows.Forms
Imports DWSIM.UnitOperations.UnitOperations.Auxiliary
Imports DWSIM.Thermodynamics.BaseClasses
Imports DWSIM.Interfaces.Enums
Imports System.IO
Imports System.Runtime.InteropServices
Imports CapeOpen
Imports System.Runtime.Serialization.Formatters
Imports System.Linq
Imports System.ComponentModel
Imports System.Drawing.Design
Imports Microsoft.Scripting.Hosting
Imports System.Drawing.Text
Imports System.Drawing

Namespace UnitOperations.CAPEOPENWrappers

    <Guid(CO_CustomUO.ClassId)> <System.Serializable()> <ComVisible(True)> Public Class CO_CustomUO

        Inherits CapeOpen.CapeUnitBase

        Implements CapeOpen.ICapeUtilities

        Private _scripttext As String = ""
        Private _fontname As String = "Courier New"
        Private _fontsize As Integer = 10
        Private _lastrun As String = ""

        Public Property HighlightSpaces As Boolean = False
        Public Property HighlightTabs As Boolean = False

        Public Shadows Const ClassId As String = "1FD2DC53-DC7B-4c4d-BBEE-F37F4E5ADDFB"

        Private _sctxt As Object

        Private Property engine As ScriptEngine
        Private Property scope As Object

        Public Shadows WriteOnly Property simulationContext As Object Implements ICapeUtilities.simulationContext
            Set(value As Object)
                _sctxt = value
            End Set
        End Property

        Public Shadows Sub Initialize() Implements ICapeUtilities.Initialize

            My.Application.ChangeUICulture("en")

            'handler for unhandled exceptions

            Application.SetUnhandledExceptionMode(UnhandledExceptionMode.CatchException)
            AddHandler Application.ThreadException, AddressOf UnhandledException
            AddHandler AppDomain.CurrentDomain.UnhandledException, AddressOf UnhandledException2

            'create port collection

            Me.ComponentName = "Scripting Unit Operation"
            Me.ComponentDescription = "IronPython Scripting Unit Operation"

            ' create ports

            With DirectCast(Me.Ports, CapeOpen.PortCollection)
                .Clear()
                .Add(New UnitPort("Inlet_Port_1", "Material Object Inlet Port 1", CapePortDirection.CAPE_INLET, CapePortType.CAPE_MATERIAL))
                .Add(New UnitPort("Inlet_Port_2", "Material Object Inlet Port 2", CapePortDirection.CAPE_INLET, CapePortType.CAPE_MATERIAL))
                .Add(New UnitPort("Inlet_Port_3", "Material Object Inlet Port 3", CapePortDirection.CAPE_INLET, CapePortType.CAPE_MATERIAL))
                .Add(New UnitPort("Inlet_Port_4", "Material Object Inlet Port 4", CapePortDirection.CAPE_INLET, CapePortType.CAPE_MATERIAL))
                .Add(New UnitPort("Inlet_Port_5", "Material Object Inlet Port 5", CapePortDirection.CAPE_INLET, CapePortType.CAPE_MATERIAL))
                .Add(New UnitPort("Inlet_Port_6", "Material Object Inlet Port 6", CapePortDirection.CAPE_INLET, CapePortType.CAPE_MATERIAL))
                .Add(New UnitPort("Inlet_Port_7", "Material Object Inlet Port 7", CapePortDirection.CAPE_INLET, CapePortType.CAPE_MATERIAL))
                .Add(New UnitPort("Inlet_Port_8", "Material Object Inlet Port 8", CapePortDirection.CAPE_INLET, CapePortType.CAPE_MATERIAL))
                .Add(New UnitPort("Inlet_Port_9", "Material Object Inlet Port 9", CapePortDirection.CAPE_INLET, CapePortType.CAPE_MATERIAL))
                .Add(New UnitPort("Inlet_Port_10", "Material Object Inlet Port 10", CapePortDirection.CAPE_INLET, CapePortType.CAPE_MATERIAL))
                .Add(New UnitPort("Outlet_Port_1", "Material Object Outlet Port 1", CapePortDirection.CAPE_OUTLET, CapePortType.CAPE_MATERIAL))
                .Add(New UnitPort("Outlet_Port_2", "Material Object Outlet Port 2", CapePortDirection.CAPE_OUTLET, CapePortType.CAPE_MATERIAL))
                .Add(New UnitPort("Outlet_Port_3", "Material Object Outlet Port 3", CapePortDirection.CAPE_OUTLET, CapePortType.CAPE_MATERIAL))
                .Add(New UnitPort("Outlet_Port_4", "Material Object Outlet Port 4", CapePortDirection.CAPE_OUTLET, CapePortType.CAPE_MATERIAL))
                .Add(New UnitPort("Outlet_Port_5", "Material Object Outlet Port 5", CapePortDirection.CAPE_OUTLET, CapePortType.CAPE_MATERIAL))
                .Add(New UnitPort("Outlet_Port_6", "Material Object Outlet Port 6", CapePortDirection.CAPE_OUTLET, CapePortType.CAPE_MATERIAL))
                .Add(New UnitPort("Outlet_Port_7", "Material Object Outlet Port 7", CapePortDirection.CAPE_OUTLET, CapePortType.CAPE_MATERIAL))
                .Add(New UnitPort("Outlet_Port_8", "Material Object Outlet Port 8", CapePortDirection.CAPE_OUTLET, CapePortType.CAPE_MATERIAL))
                .Add(New UnitPort("Outlet_Port_9", "Material Object Outlet Port 9", CapePortDirection.CAPE_OUTLET, CapePortType.CAPE_MATERIAL))
                .Add(New UnitPort("Outlet_Port_10", "Material Object Outlet Port 10", CapePortDirection.CAPE_OUTLET, CapePortType.CAPE_MATERIAL))
                .Add(New UnitPort("Energy_Inlet_Port_1", "Energy Stream Inlet Port", CapePortDirection.CAPE_INLET, CapePortType.CAPE_ENERGY))
                .Add(New UnitPort("Energy_Outlet_Port_1", "Energy Stream Outlet Port", CapePortDirection.CAPE_OUTLET, CapePortType.CAPE_ENERGY))
            End With

            Dim fontnames As New List(Of String)

            ' Get the installed fonts collection.
            Dim installed_fonts As New InstalledFontCollection
            ' Get an array of the system's font familiies.
            Dim font_families() As FontFamily = installed_fonts.Families()
            ' Display the font families.
            For Each font_family As FontFamily In font_families
                fontnames.Add(font_family.Name)
            Next font_family

            With DirectCast(Me.Parameters, CapeOpen.ParameterCollection)
                .Clear()
                .Add(New CapeOpen.OptionParameter("Script", _scripttext))
                .Add(New CapeOpen.OptionParameter("Font Name", "Script editor font name", "Courier New", "Courier New", fontnames.ToArray, False, CapeParamMode.CAPE_INPUT))
                .Add(New CapeOpen.OptionParameter("Font Size", "Script editor font name", 10, 10, New String() {"6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16"}, True, CapeParamMode.CAPE_INPUT))
            End With

        End Sub

        Public Shadows Sub Terminate() Implements ICapeUtilities.Terminate

            If Not _sctxt Is Nothing Then
                If System.Runtime.InteropServices.Marshal.IsComObject(_sctxt) Then
                    System.Runtime.InteropServices.Marshal.ReleaseComObject(_sctxt)
                End If
            End If

            Me.simulationContext = Nothing

        End Sub

        Public Shadows Sub Calculate()

            Dim ims1, ims2, ims3, ims4, ims5, ims6, ims7, ims8, ims9, ims10,
                oms1, oms2, oms3, oms4, oms5, oms6, oms7, oms8, oms9, oms10 As ICapeThermoMaterialObject

            ims1 = TryCast(Me.Ports(0).connectedObject, ICapeThermoMaterialObject)
            ims2 = TryCast(Me.Ports(1).connectedObject, ICapeThermoMaterialObject)
            ims3 = TryCast(Me.Ports(2).connectedObject, ICapeThermoMaterialObject)
            ims4 = TryCast(Me.Ports(3).connectedObject, ICapeThermoMaterialObject)
            ims5 = TryCast(Me.Ports(4).connectedObject, ICapeThermoMaterialObject)
            ims6 = TryCast(Me.Ports(5).connectedObject, ICapeThermoMaterialObject)
            ims7 = TryCast(Me.Ports(6).connectedObject, ICapeThermoMaterialObject)
            ims8 = TryCast(Me.Ports(7).connectedObject, ICapeThermoMaterialObject)
            ims9 = TryCast(Me.Ports(8).connectedObject, ICapeThermoMaterialObject)
            ims10 = TryCast(Me.Ports(9).connectedObject, ICapeThermoMaterialObject)
            oms1 = TryCast(Me.Ports(10).connectedObject, ICapeThermoMaterialObject)
            oms2 = TryCast(Me.Ports(11).connectedObject, ICapeThermoMaterialObject)
            oms3 = TryCast(Me.Ports(12).connectedObject, ICapeThermoMaterialObject)
            oms4 = TryCast(Me.Ports(13).connectedObject, ICapeThermoMaterialObject)
            oms5 = TryCast(Me.Ports(14).connectedObject, ICapeThermoMaterialObject)
            oms6 = TryCast(Me.Ports(15).connectedObject, ICapeThermoMaterialObject)
            oms7 = TryCast(Me.Ports(16).connectedObject, ICapeThermoMaterialObject)
            oms8 = TryCast(Me.Ports(17).connectedObject, ICapeThermoMaterialObject)
            oms9 = TryCast(Me.Ports(18).connectedObject, ICapeThermoMaterialObject)
            oms10 = TryCast(Me.Ports(19).connectedObject, ICapeThermoMaterialObject)

            Dim ies1, oes1 As ICapeCollection

            ies1 = TryCast(Me.Ports(20).connectedObject, ICapeCollection)
            oes1 = TryCast(Me.Ports(21).connectedObject, ICapeCollection)

            Dim source As Microsoft.Scripting.Hosting.ScriptSource
            Try
                engine = IronPython.Hosting.Python.CreateEngine()
                engine.Runtime.LoadAssembly(GetType(System.String).Assembly)
                engine.Runtime.LoadAssembly(GetType(ICapeIdentification).Assembly)
                engine.Runtime.LoadAssembly(GetType(CapeOpen.ICapeIdentification).Assembly)
                engine.Runtime.LoadAssembly(GetType(BaseClasses.ConstantProperties).Assembly)
                scope = engine.CreateScope()
                scope.SetVariable("pme", Me._sctxt)
                scope.SetVariable("this", Me)
                scope.SetVariable("ims1", ims1)
                scope.SetVariable("ims2", ims2)
                scope.SetVariable("ims3", ims3)
                scope.SetVariable("ims4", ims4)
                scope.SetVariable("ims5", ims5)
                scope.SetVariable("ims6", ims6)
                scope.SetVariable("ims7", ims7)
                scope.SetVariable("ims8", ims8)
                scope.SetVariable("ims9", ims9)
                scope.SetVariable("ims10", ims10)
                scope.SetVariable("oms1", oms1)
                scope.SetVariable("oms2", oms2)
                scope.SetVariable("oms3", oms3)
                scope.SetVariable("oms4", oms4)
                scope.SetVariable("oms5", oms5)
                scope.SetVariable("oms6", oms6)
                scope.SetVariable("oms7", oms7)
                scope.SetVariable("oms8", oms8)
                scope.SetVariable("oms9", oms9)
                scope.SetVariable("oms10", oms10)
                scope.SetVariable("ies1", ies1)
                scope.SetVariable("oes1", oes1)
                Dim txtcode As String = ""
                txtcode += Me._scripttext
                source = Me.engine.CreateScriptSourceFromString(txtcode, Microsoft.Scripting.SourceCodeKind.Statements)
                source.Execute(Me.scope)
                _lastrun = "script executed succesfully."
            Catch ex As Exception
                Dim ops As ExceptionOperations = engine.GetService(Of ExceptionOperations)()
                engine = Nothing
                scope = Nothing
                source = Nothing
                _lastrun = "error executing script: " & ops.FormatException(ex).ToString
                Throw ex
            Finally
                engine = Nothing
                scope = Nothing
                source = Nothing
            End Try

        End Sub

        Public Overrides Sub Edit()

            Dim edform As New EditingForm_CustomUO_ScriptEditor
            With edform
                .CAPEOPEN = True
                .fontname = Me._fontname
                .fontsize = Me._fontsize
                .txtScript.Text = Me._scripttext
                .ShowDialog()
                Me._fontname = .fontname
                Me._fontsize = .fontsize
                Me._scripttext = .scripttext

            End With
            edform.Dispose()
            edform = Nothing

        End Sub

        Public Overrides Sub ProduceReport(ByRef report As String)
            report = _lastrun
        End Sub

        Private Sub UnhandledException(ByVal sender As Object, ByVal e As System.Threading.ThreadExceptionEventArgs)

            Try
                Dim frmEx As New FormUnhandledException
                frmEx.TextBox1.Text = e.Exception.ToString
                frmEx.ex = e.Exception
                frmEx.ShowDialog()
            Finally
            End Try

            If Settings.CAPEOPENMode Then
                Dim hcode As Integer = 0
                Dim comEx As COMException = New COMException(e.Exception.Message.ToString, e.Exception)
                If Not IsNothing(comEx) Then hcode = comEx.ErrorCode
                ThrowCAPEException(e.Exception, "Error", e.Exception.Message, "UnhandledException", e.Exception.Source, e.Exception.StackTrace, "UnhandledException", hcode)
            End If

        End Sub

        Private Sub UnhandledException2(ByVal sender As Object, ByVal e As System.UnhandledExceptionEventArgs)

            Try
                Dim frmEx As New FormUnhandledException
                frmEx.TextBox1.Text = e.ExceptionObject.ToString
                frmEx.ex = e.ExceptionObject
                frmEx.ShowDialog()
            Catch ex As Exception
            End Try

            If Settings.CAPEOPENMode Then
                Dim hcode As Integer = 0
                Dim comEx As COMException = e.ExceptionObject
                If Not IsNothing(comEx) Then hcode = comEx.ErrorCode
                ThrowCAPEException(e.ExceptionObject, "Error", e.ExceptionObject.ToString, "UnhandledException", e.ExceptionObject.ToString, "", "UnhandledException", hcode)
            End If

        End Sub

        Sub ThrowCAPEException(ByVal ex As Exception, ByVal name As String, ByVal description As String, ByVal interf As String, ByVal moreinfo As String, ByVal operation As String, ByVal scope As String, ByVal code As Integer)

            Throw New CapeOpen.CapeUnknownException(ex.Message.ToArray, ex)

        End Sub

        Public Overrides Sub OnCalculate()

        End Sub

    End Class

End Namespace

