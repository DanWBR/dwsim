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
Imports DWSIM.Interfaces.Interfaces2

Namespace UnitOperations.CAPEOPENWrappers

    <Guid(CO_CustomUO.ClassId)> <System.Serializable()> <ComVisible(True)> Public Class CO_CustomUO

        Inherits CapeOpen.CapeUnitBase

        Implements CapeOpen.ICapeUtilities, IPersistStreamInit, ICapeUnit

        Private _scripttext As String = ""
        Private _fontname As String = "Consolas"
        Private _fontsize As Integer = 10
        Private _lastrun As String = ""

        Private _inletmaterialports As Integer = 1
        Private _outletmaterialports As Integer = 1
        Private _inletenergyports As Integer = 0
        Private _outletenergyports As Integer = 0

        Public Property HighlightSpaces As Boolean = False

        Public Shadows Const ClassId As String = "1FD2DC53-DC7B-4c4d-BBEE-F37F4E5ADDFB"

        Private _sctxt As Object

        <System.NonSerialized()> Public scope As Microsoft.Scripting.Hosting.ScriptScope
        <System.NonSerialized()> Public engine As Microsoft.Scripting.Hosting.ScriptEngine

        Private _initialized As Boolean = False

        Public Shadows WriteOnly Property simulationContext As Object Implements ICapeUtilities.simulationContext
            Set(value As Object)
                _sctxt = value
            End Set
        End Property

        Public Shadows Sub Initialize() Implements ICapeUtilities.Initialize

            My.Application.ChangeUICulture("en")

            'handler for unhandled exceptions

            Try
                Application.SetUnhandledExceptionMode(UnhandledExceptionMode.CatchException)
                AddHandler Application.ThreadException, AddressOf UnhandledException
                AddHandler AppDomain.CurrentDomain.UnhandledException, AddressOf UnhandledException2
            Catch ex As Exception

            End Try

            'create port collection

            Me.ComponentName = "Scripting Unit Operation"
            Me.ComponentDescription = "IronPython Scripting Unit Operation"

            CreatePorts()

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
                .Add(New CapeOpen.OptionParameter("FontName", "Script editor font name", _fontname, "Consolas", fontnames.ToArray, False, CapeParamMode.CAPE_INPUT))
                .Add(New CapeOpen.OptionParameter("FontSize", "Script editor font size", _fontsize, 10, New String() {"6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16"}, True, CapeParamMode.CAPE_INPUT))
                .Add(New CapeOpen.BooleanParameter("HighlightSpaces", "Highlight spaces in the script editor", HighlightSpaces, True, CapeParamMode.CAPE_INPUT))
                .Add(New CapeOpen.IntegerParameter("InletMaterialPorts", "Number of Inlet Material Object Ports", _inletmaterialports, 1, 0, 10, CapeParamMode.CAPE_INPUT))
                .Add(New CapeOpen.IntegerParameter("OutletMaterialPorts", "Number of Outlet Material Object Ports", _outletmaterialports, 1, 0, 10, CapeParamMode.CAPE_INPUT))
                .Add(New CapeOpen.IntegerParameter("InletEnergyPorts", "Number of Inlet Energy Ports", _inletenergyports, 1, 0, 1, CapeParamMode.CAPE_INPUT))
                .Add(New CapeOpen.IntegerParameter("OutletEnergyPorts", "Number of Outlet Energy Ports", _outletenergyports, 1, 0, 1, CapeParamMode.CAPE_INPUT))
            End With

        End Sub

        Sub CreatePorts()

            ' create ports

            If Me.Ports.Count > 0 Then
                For Each p In Me.Ports
                    If p.connectedObject IsNot Nothing Then p.Disconnect()
                Next
            End If

            With Me.Ports
                .Clear()
                For i As Integer = 1 To _inletmaterialports
                    .Add(New UnitPort("Inlet_Material_Port_" & i, "Material Object Inlet Port " & i, CapePortDirection.CAPE_INLET, CapePortType.CAPE_MATERIAL))
                Next
                For i As Integer = 1 To _inletenergyports
                    .Add(New UnitPort("Inlet_Energy_Port_" & i, "Energy Stream Inlet Port " & i, CapePortDirection.CAPE_INLET, CapePortType.CAPE_ENERGY))
                Next
                For i As Integer = 1 To _outletmaterialports
                    .Add(New UnitPort("Outlet_Material_Port_" & i, "Material Object Outlet Port " & i, CapePortDirection.CAPE_OUTLET, CapePortType.CAPE_MATERIAL))
                Next
                For i As Integer = 1 To _outletenergyports
                    .Add(New UnitPort("Outlet_Energy_Port_" & i, "Energy Stream Outlet Port " & i, CapePortDirection.CAPE_OUTLET, CapePortType.CAPE_ENERGY))
                Next
            End With

        End Sub

        Private Sub CO_CustomUO_PropertyChanged(sender As Object, e As PropertyChangedEventArgs) Handles Me.PropertyChanged

            Select Case e.PropertyName
                Case "Script"
                    _scripttext = DirectCast(Me.Parameters(0), OptionParameter).Value
                Case "Font Name"
                    _scripttext = DirectCast(Me.Parameters(1), OptionParameter).Value
                Case "Font Size"
                    _scripttext = DirectCast(Me.Parameters(2), OptionParameter).Value
                Case "HighlightSpaces"
                    HighlightSpaces = DirectCast(Me.Parameters(3), BooleanParameter).Value
                Case "InletMaterialPorts"
                    _inletmaterialports = DirectCast(Me.Parameters(4), IntegerParameter).Value
                    CreatePorts()
                Case "OutletMaterialPorts"
                    _outletmaterialports = DirectCast(Me.Parameters(5), IntegerParameter).Value
                    CreatePorts()
                Case "InletEnergyPorts"
                    _inletenergyports = DirectCast(Me.Parameters(6), IntegerParameter).Value
                    CreatePorts()
                Case "OutletEnergyPorts"
                    _outletenergyports = DirectCast(Me.Parameters(7), IntegerParameter).Value
                    CreatePorts()
            End Select

        End Sub

        Public Shadows Sub Terminate() Implements ICapeUtilities.Terminate

            If Not _sctxt Is Nothing Then
                If System.Runtime.InteropServices.Marshal.IsComObject(_sctxt) Then
                    System.Runtime.InteropServices.Marshal.ReleaseComObject(_sctxt)
                End If
            End If

            Me.simulationContext = Nothing

        End Sub

        Public Shadows Sub Calculate() Implements CapeOpen.ICapeUnit.Calculate

            Dim source As Microsoft.Scripting.Hosting.ScriptSource
            Try
                engine = IronPython.Hosting.Python.CreateEngine()
                engine.Runtime.LoadAssembly(GetType(System.String).Assembly)
                engine.Runtime.LoadAssembly(GetType(CAPEOPEN110.ICapeIdentification).Assembly)
                engine.Runtime.LoadAssembly(GetType(CapeOpen.ICapeIdentification).Assembly)
                engine.Runtime.LoadAssembly(Reflection.Assembly.GetExecutingAssembly)
                scope = engine.CreateScope()
                scope.SetVariable("pme", Me._sctxt)
                scope.SetVariable("this", Me)
                Dim ocount As Integer = 0
                For i As Integer = 1 To _inletmaterialports
                    scope.SetVariable("ims" & i, TryCast(Me.Ports(ocount).connectedObject, ICapeThermoMaterialObject))
                    ocount += 1
                Next
                For i As Integer = 1 To _inletenergyports
                    scope.SetVariable("ies" & i, TryCast(Me.Ports(ocount).connectedObject, ICapeCollection))
                    ocount += 1
                Next
                For i As Integer = 1 To _outletmaterialports
                    scope.SetVariable("oms" & i, TryCast(Me.Ports(ocount).connectedObject, ICapeThermoMaterialObject))
                    ocount += 1
                Next
                For i As Integer = 1 To _outletenergyports
                    scope.SetVariable("oes" & i, TryCast(Me.Ports(ocount).connectedObject, ICapeCollection))
                    ocount += 1
                Next
                Dim txtcode As String = ""
                txtcode += DirectCast(Me.Parameters(0), OptionParameter).Value
                source = Me.engine.CreateScriptSourceFromString(txtcode, Microsoft.Scripting.SourceCodeKind.Statements)
                source.Execute(Me.scope)
                _lastrun = "Script executed succesfully."
            Catch ex As Exception
                Dim ops As ExceptionOperations = engine.GetService(Of ExceptionOperations)()
                engine = Nothing
                scope = Nothing
                source = Nothing
                _lastrun = "Error executing script: " & ops.FormatException(ex).ToString
                MessageBox.Show(_lastrun, Me.ComponentName)
                Throw New CapeOpen.CapeSolvingErrorException(_lastrun, ex)
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
                .fontname = DirectCast(Me.Parameters(1), OptionParameter).Value
                .fontsize = DirectCast(Me.Parameters(2), OptionParameter).Value
                .txtScript.Text = DirectCast(Me.Parameters(0), OptionParameter).Value
                .highlightspaces = DirectCast(Me.Parameters(3), BooleanParameter).Value
                .ShowDialog()
                DirectCast(Me.Parameters(1), OptionParameter).Value = .fontname
                DirectCast(Me.Parameters(2), OptionParameter).Value = .fontsize
                DirectCast(Me.Parameters(0), OptionParameter).Value = .scripttext
                DirectCast(Me.Parameters(3), BooleanParameter).Value = .highlightspaces
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

        End Sub

        Private Sub UnhandledException2(ByVal sender As Object, ByVal e As System.UnhandledExceptionEventArgs)

            Try
                Dim frmEx As New FormUnhandledException
                frmEx.TextBox1.Text = e.ExceptionObject.ToString
                frmEx.ex = e.ExceptionObject
                frmEx.ShowDialog()
            Catch ex As Exception
            End Try

        End Sub

        Public Overrides Sub OnCalculate()

        End Sub

#Region "   CAPE-OPEN Persistence Implementation"

        Protected m_dirty As Boolean = True

        Public Sub GetClassID(ByRef pClassID As System.Guid) Implements IPersistStreamInit.GetClassID
            pClassID = New Guid(CO_CustomUO.ClassId)
        End Sub

        Public Sub GetSizeMax(ByRef pcbSize As Long) Implements IPersistStreamInit.GetSizeMax
            pcbSize = 1024 * 1024
        End Sub

        Public Overridable Sub InitNew() Implements IPersistStreamInit.InitNew


        End Sub

        Public Function IsDirty() As Integer Implements IPersistStreamInit.IsDirty
            Return m_dirty
        End Function

        Public Sub Load(ByVal pStm As System.Runtime.InteropServices.ComTypes.IStream) Implements IPersistStreamInit.Load

            ' Read the length of the string  
            Dim arrLen As Byte() = New [Byte](3) {}
            pStm.Read(arrLen, arrLen.Length, IntPtr.Zero)

            ' Calculate the length  
            Dim cb As Integer = BitConverter.ToInt32(arrLen, 0)

            ' Read the stream to get the string    
            Dim bytes As Byte() = New Byte(cb - 1) {}
            Dim pcb As New IntPtr()
            pStm.Read(bytes, bytes.Length, pcb)
            If System.Runtime.InteropServices.Marshal.IsComObject(pStm) Then System.Runtime.InteropServices.Marshal.ReleaseComObject(pStm)

            ' Deserialize byte array    

            Dim memoryStream As New System.IO.MemoryStream(bytes)

            Try

                Dim domain As AppDomain = AppDomain.CurrentDomain
                AddHandler domain.AssemblyResolve, New ResolveEventHandler(AddressOf MyResolveEventHandler)

                Dim myarr As ArrayList

                Dim mySerializer As Binary.BinaryFormatter = New Binary.BinaryFormatter(Nothing, New System.Runtime.Serialization.StreamingContext())
                myarr = mySerializer.Deserialize(memoryStream)

                _scripttext = myarr(0)
                _fontname = myarr(1)
                _fontsize = myarr(2)
                If myarr.Count > 3 Then
                    HighlightSpaces = myarr(3)
                    _inletmaterialports = myarr(4)
                    _outletmaterialports = myarr(5)
                    _inletenergyports = myarr(6)
                    _outletenergyports = myarr(7)
                End If

                myarr = Nothing
                mySerializer = Nothing

                RemoveHandler domain.AssemblyResolve, New ResolveEventHandler(AddressOf MyResolveEventHandler)

            Catch p_Ex As System.Exception

                System.Windows.Forms.MessageBox.Show(p_Ex.ToString())

            End Try

            memoryStream.Close()

        End Sub

        Public Sub Save(ByVal pStm As System.Runtime.InteropServices.ComTypes.IStream, ByVal fClearDirty As Boolean) Implements IPersistStreamInit.Save

            Dim props As New ArrayList

            With props

                .Add(DirectCast(Me.Parameters(0), OptionParameter).Value)
                .Add(DirectCast(Me.Parameters(1), OptionParameter).Value)
                .Add(DirectCast(Me.Parameters(2), OptionParameter).Value)
                .Add(DirectCast(Me.Parameters(3), BooleanParameter).Value)
                .Add(DirectCast(Me.Parameters(4), IntegerParameter).Value)
                .Add(DirectCast(Me.Parameters(5), IntegerParameter).Value)
                .Add(DirectCast(Me.Parameters(6), IntegerParameter).Value)
                .Add(DirectCast(Me.Parameters(7), IntegerParameter).Value)

            End With

            Dim mySerializer As Binary.BinaryFormatter = New Binary.BinaryFormatter(Nothing, New System.Runtime.Serialization.StreamingContext())
            Dim mstr As New MemoryStream
            mySerializer.Serialize(mstr, props)
            Dim bytes As Byte() = mstr.ToArray()
            mstr.Close()

            ' construct length (separate into two separate bytes)    

            Dim arrLen As Byte() = BitConverter.GetBytes(bytes.Length)
            Try

                ' Save the array in the stream    
                pStm.Write(arrLen, arrLen.Length, IntPtr.Zero)
                pStm.Write(bytes, bytes.Length, IntPtr.Zero)
                If System.Runtime.InteropServices.Marshal.IsComObject(pStm) Then System.Runtime.InteropServices.Marshal.ReleaseComObject(pStm)

            Catch p_Ex As System.Exception

                System.Windows.Forms.MessageBox.Show(p_Ex.ToString())

            End Try

            If fClearDirty Then
                m_dirty = False
            End If

        End Sub

        Protected Function MyResolveEventHandler(ByVal sender As Object, ByVal args As ResolveEventArgs) As System.Reflection.Assembly
            Return Me.[GetType]().Assembly
        End Function

#End Region

    End Class

End Namespace

