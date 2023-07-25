'    Stream Classes
'    Copyright 2008-2011 Daniel Wagner O. de Medeiros
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


Imports CapeOpen
Imports System.Linq
Imports DWSIM.Thermodynamics.PropertyPackages
Imports DWSIM.Thermodynamics
Imports System.Runtime.InteropServices
Imports System.Threading.Tasks
Imports System.Runtime.Serialization
Imports System.Reflection
Imports DWSIM.Interfaces
Imports DWSIM.Interfaces.Enums
Imports DWSIM.SharedClasses.UnitOperations
Imports DWSIM.SharedClasses

Namespace Streams

    <System.Serializable()> <ComVisible(True)> Public Class EnergyStream

        Inherits BaseClass

        Implements ICapeIdentification, ICapeCollection, IEnergyStream

        'CAPE-OPEN Error Interfaces
        Implements ECapeUser, ECapeUnknown, ECapeRoot

        <NonSerialized> <Xml.Serialization.XmlIgnore> Public f As EditingForm_EnergyStream

        Private WithEvents m_work As CapeOpen.RealParameter
        Private WithEvents m_tLow As CapeOpen.RealParameter
        Private WithEvents m_tUp As CapeOpen.RealParameter

        Private initialized As Boolean = False

        Public Overrides Property ObjectClass As SimulationObjectClass = SimulationObjectClass.Streams

        Public Overrides ReadOnly Property SupportsDynamicMode As Boolean = True

        Public Overrides ReadOnly Property HasPropertiesForDynamicMode As Boolean = False

#Region "   CAPE-OPEN ICapeIdentification"

        Public Overrides Property ComponentDescription() As String = "" Implements CapeOpen.ICapeIdentification.ComponentDescription

        Public Overrides Property ComponentName() As String = "" Implements CapeOpen.ICapeIdentification.ComponentName

#End Region

#Region "   DWSIM Specific"

        Public Sub New()
            MyBase.New()
        End Sub

        Public Sub New(ByVal name As String, ByVal description As String)

            MyBase.CreateNew()
            Me.ComponentName = name
            Me.ComponentDescription = description
            Init()

        End Sub

        Sub Init()

            If Type.GetType("Mono.Runtime") Is Nothing Then CreateParamCol()
            initialized = True

        End Sub

        Sub CreateParamCol()

            m_work = New CapeOpen.RealParameter("work", Me.EnergyFlow.GetValueOrDefault, 0.0#, "J/s")
            m_tLow = New CapeOpen.RealParameter("temperatureLow", 0.0, 0.0#, "K")
            m_tUp = New CapeOpen.RealParameter("temperatureHigh", 2000.0, 2000.0#, "K")

        End Sub

        Private _eflow As Double?

        ''' <summary>
        ''' Power (energy) associated with this stream.
        ''' </summary>
        ''' <value></value>
        ''' <returns></returns>
        ''' <remarks></remarks>
        Public Overrides Property EnergyFlow() As Double?
            Get
                Return _eflow
            End Get
            Set(value As Double?)
                _eflow = value
                SetDirtyStatus(True)
            End Set
        End Property

        Public Sub SetValue(ByVal energyflow_kW As Double)
            EnergyFlow = energyflow_kW
        End Sub

        Public Sub Assign(ByVal ASource As EnergyStream)

            'Copy properties from the ASource stream.

            Me.EnergyFlow = ASource.EnergyFlow

        End Sub

        Public Overrides Sub Calculate(Optional args As Object = Nothing)

            SetDirtyStatus(False)

        End Sub

        Public Overrides Function GetPropertyValue(ByVal prop As String, Optional ByVal su As Interfaces.IUnitsOfMeasure = Nothing) As Object

            If su Is Nothing Then su = New SystemsOfUnits.SI

            Dim val0 As Object = MyBase.GetPropertyValue(prop, su)

            If val0 Is Nothing Then

                Dim epcol = DirectCast(ExtraProperties, IDictionary(Of String, Object))
                Dim epucol = DirectCast(ExtraPropertiesUnitTypes, IDictionary(Of String, Object))

                If epcol.ContainsKey(prop) Then

                    If epucol.ContainsKey(prop) Then
                        Dim utype = epucol(prop)
                        If su Is Nothing Then
                            Return Convert.ToDouble(epcol(prop)).ConvertFromSI(SharedClasses.SystemsOfUnits.Converter.SharedSI.GetCurrentUnits(utype))
                        Else
                            Return Convert.ToDouble(epcol(prop)).ConvertFromSI(su.GetCurrentUnits(utype))
                        End If
                    Else
                        Return epcol(prop)
                    End If

                Else

                    Dim cv As New SystemsOfUnits.Converter
                    Dim value As Double = 0
                    Dim propidx As Integer = -1

                    Try
                        propidx = Convert.ToInt32(prop.Split("_")(2))
                    Catch ex As Exception

                    End Try

                    Select Case propidx

                        Case 0
                            'PROP_ES_0	Power
                            value = SystemsOfUnits.Converter.ConvertFromSI(su.heatflow, Me.EnergyFlow.GetValueOrDefault)

                    End Select

                    Return value

                End If

            Else

                Return val0

            End If

        End Function

        Public Overloads Overrides Function GetProperties(ByVal proptype As Interfaces.Enums.PropertyType) As String()
            Dim i As Integer = 0
            Dim proplist As New ArrayList
            Select Case proptype
                Case PropertyType.RO
                    For i = 0 To 0
                        proplist.Add("PROP_ES_" + CStr(i))
                    Next
                Case PropertyType.RW
                    For i = 0 To 0
                        proplist.Add("PROP_ES_" + CStr(i))
                    Next
                Case PropertyType.WR
                    For i = 0 To 0
                        proplist.Add("PROP_ES_" + CStr(i))
                    Next
                Case PropertyType.ALL
                    For i = 0 To 0
                        proplist.Add("PROP_ES_" + CStr(i))
                    Next
            End Select
            Return proplist.ToArray(GetType(System.String))
            proplist = Nothing
        End Function

        Public Overrides Function SetPropertyValue(ByVal prop As String, ByVal propval As Object, Optional ByVal su As Interfaces.IUnitsOfMeasure = Nothing) As Boolean
            If su Is Nothing Then su = New SystemsOfUnits.SI
            Dim cv As New SystemsOfUnits.Converter
            Dim propidx As Integer = Convert.ToInt32(prop.Split("_")(2))

            Select Case propidx
                Case 0
                    'PROP_ES_0	Power
                    Me.EnergyFlow = SystemsOfUnits.Converter.ConvertToSI(su.heatflow, propval)
            End Select
            Return 1
        End Function

        Public Overrides Function GetPropertyUnit(ByVal prop As String, Optional ByVal su As Interfaces.IUnitsOfMeasure = Nothing) As String
            If su Is Nothing Then su = New SystemsOfUnits.SI
            Dim value As String = ""
            Dim propidx As Integer = Convert.ToInt32(prop.Split("_")(2))

            Select Case propidx

                Case 0
                    'PROP_ES_0	Power
                    value = su.heatflow

            End Select

            Return value

        End Function

#End Region

#Region "   CAPE-OPEN"

        Private Sub m_work_OnParameterValueChanged(ByVal sender As Object, ByVal args As System.EventArgs) Handles m_work.ParameterValueChanged
            Me.EnergyFlow = m_work.SIValue / 1000
        End Sub

        Public Function Count() As Integer Implements CapeOpen.ICapeCollection.Count
            Return 1
        End Function

        Public Function Item(ByVal index As Object) As Object Implements CapeOpen.ICapeCollection.Item
            If Not initialized Then Init()
            Select Case index.ToString()
                Case "1", "work"
                    Return m_work
                Case "2", "temperatureLow"
                    Return m_tLow
                Case "3", "temperatureHigh"
                    Return m_tUp
                Case Else
                    Return m_work
            End Select
        End Function

#End Region

        Public Overrides Sub RunDynamicModel()

        End Sub

        Public Overrides Function CloneXML() As Object
            Dim obj As ICustomXMLSerialization = New EnergyStream()
            obj.LoadData(Me.SaveData)
            Return obj
        End Function

        Public Overrides Function CloneJSON() As Object
            Return Newtonsoft.Json.JsonConvert.DeserializeObject(Of EnergyStream)(Newtonsoft.Json.JsonConvert.SerializeObject(Me))
        End Function

        Public Overrides Sub DisplayEditForm()

            If f Is Nothing Then
                f = New EditingForm_EnergyStream With {.SimObject = Me}
                f.ShowHint = GlobalSettings.Settings.DefaultEditFormLocation
                f.Tag = "ObjectEditor"
                Me.FlowSheet.DisplayForm(f)
            Else
                If f.IsDisposed Then
                    f = New EditingForm_EnergyStream With {.SimObject = Me}
                    f.ShowHint = GlobalSettings.Settings.DefaultEditFormLocation
                    f.Tag = "ObjectEditor"
                    Me.FlowSheet.DisplayForm(f)
                Else
                    f.UpdateInfo()
                    f.Activate()
                End If
            End If

        End Sub

        Public Overrides Sub UpdateEditForm()
            If f IsNot Nothing Then
                If Not f.IsDisposed Then
                    f.UIThread(Sub() f.UpdateInfo())
                End If
            End If
        End Sub

        Public Overrides Function GetIconBitmap() As Object
            Return My.Resources.energy_stream
        End Function

        Public Overrides Function GetDisplayDescription() As String
            Return ResMan.GetLocalString("ESTR_Desc")
        End Function

        Public Overrides Function GetDisplayName() As String
            Return ResMan.GetLocalString("ESTR_Name")
        End Function

        Public Overrides Sub CloseEditForm()
            If f IsNot Nothing Then
                If Not f.IsDisposed Then
                    f.Close()
                    f = Nothing
                End If
            End If
        End Sub

        Public Overrides ReadOnly Property MobileCompatible As Boolean
            Get
                Return True
            End Get
        End Property

        Public Overrides Function GetReport(su As IUnitsOfMeasure, ci As Globalization.CultureInfo, numberformat As String) As String

            Dim str As New Text.StringBuilder

            str.AppendLine("Energy Stream : " & Me.GraphicObject.Tag)
            str.AppendLine()
            str.AppendLine("Heat Flow: " & SystemsOfUnits.Converter.ConvertFromSI(su.heatflow, Me.EnergyFlow.GetValueOrDefault).ToString(numberformat, ci) & " " & su.heatflow)

            Return str.ToString

        End Function

        Public Overrides Function GetStructuredReport() As List(Of Tuple(Of ReportItemType, String()))

            Dim su As IUnitsOfMeasure = GetFlowsheet().FlowsheetOptions.SelectedUnitSystem
            Dim nf = GetFlowsheet().FlowsheetOptions.NumberFormat

            Dim list As New List(Of Tuple(Of ReportItemType, String()))

            list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.Label, New String() {"Results Report for Energy Stream '" & Me.GraphicObject.Tag + "'"}))
            list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.SingleColumn, New String() {"Calculated successfully on " & LastUpdated.ToString}))

            list.Add(New Tuple(Of ReportItemType, String())(ReportItemType.TripleColumn,
                            New String() {"Energy Flow",
                            EnergyFlow.GetValueOrDefault.ConvertFromSI(su.heatflow).ToString(nf),
                            su.heatflow}))

            Return list

        End Function

        Public Overrides Function GetPropertyDescription(p As String) As String
            Return "Amount of heat flow carried by this stream."
        End Function

#Region "    CAPE-OPEN Error Interfaces"

        Sub ThrowCAPEException(ByRef ex As Exception, ByVal name As String, ByVal description As String, ByVal interf As String, ByVal moreinfo As String, ByVal operation As String, ByVal scope As String, ByVal code As Integer)

            _code = code
            _description = description
            _interfacename = interf
            _moreinfo = moreinfo
            _operation = operation
            _scope = scope

            Throw New CapeComputationException(ex.Message.ToString, ex)

        End Sub

        Public Function GetEnergyFlow() As Double Implements IEnergyStream.GetEnergyFlow
            Return EnergyFlow.GetValueOrDefault()
        End Function

        Public Sub SetEnergyFlow(value As Double) Implements IEnergyStream.SetEnergyFlow
            EnergyFlow = value
        End Sub

        Private _description, _interfacename, _moreinfo, _operation, _scope As String, _code As Integer

        Public ReadOnly Property Name2() As String Implements CapeOpen.ECapeRoot.Name
            Get
                Return Me.Name
            End Get
        End Property

        Public ReadOnly Property code() As Integer Implements CapeOpen.ECapeUser.code
            Get
                Return _code
            End Get
        End Property

        Public ReadOnly Property description() As String Implements CapeOpen.ECapeUser.description
            Get
                Return _description
            End Get
        End Property

        Public ReadOnly Property interfaceName() As String Implements CapeOpen.ECapeUser.interfaceName
            Get
                Return _interfacename
            End Get
        End Property

        Public ReadOnly Property moreInfo() As String Implements CapeOpen.ECapeUser.moreInfo
            Get
                Return _moreinfo
            End Get
        End Property

        Public ReadOnly Property operation() As String Implements CapeOpen.ECapeUser.operation
            Get
                Return _operation
            End Get
        End Property

        Public ReadOnly Property scope() As String Implements CapeOpen.ECapeUser.scope
            Get
                Return _scope
            End Get
        End Property

#End Region


    End Class

End Namespace
