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

Imports DWSIM.DrawingTools.GraphicObjects
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

Namespace Streams

    <System.Serializable()> Public Class EnergyStream

        Inherits BaseClass

        Implements ICapeIdentification, ICapeCollection

        Protected WithEvents m_work As CapeOpen.RealParameter

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


            If Not DWSIM.App.IsRunningOnMono Then CreateParamCol()
        End Sub

        Sub CreateParamCol()

            m_work = New CapeOpen.RealParameter("work", Me.EnergyFlow.GetValueOrDefault, 0.0#, "J/s")

        End Sub

        ''' <summary>
        ''' Power (energy) associated with this stream.
        ''' </summary>
        ''' <value></value>
        ''' <returns></returns>
        ''' <remarks></remarks>
        Public Overrides Property EnergyFlow() As Nullable(Of Double)

        Public Sub Assign(ByVal ASource As EnergyStream)

            'Copy properties from the ASource stream.

            Me.EnergyFlow = ASource.EnergyFlow

        End Sub

        Public Overrides Sub PopulatePropertyGrid(pgrid As PropertyGridEx.PropertyGridEx, su As SystemsOfUnits.Units)

            Dim Conversor As New SystemsOfUnits.Converter

            With pgrid

                .PropertySort = PropertySort.Categorized
                .ShowCustomProperties = True
                .Item.Clear()

                Dim ent, saida As String
                If Me.GraphicObject.InputConnectors(0).IsAttached = True Then
                    ent = Me.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.Tag
                Else
                    ent = ""
                End If
                If Me.GraphicObject.OutputConnectors(0).IsAttached = True Then
                    saida = Me.GraphicObject.OutputConnectors(0).AttachedConnector.AttachedTo.Tag
                Else
                    saida = ""
                End If

                .Item.Add(DWSIM.App.GetLocalString("Conectadoaentrada"), ent, True, DWSIM.App.GetLocalString("Conexes1"), "", True)
                With .Item(.Item.Count - 1)
                    .DefaultValue = Nothing
                End With

                .Item.Add(DWSIM.App.GetLocalString("Conectadoasada"), saida, True, DWSIM.App.GetLocalString("Conexes1"), "", True)
                With .Item(.Item.Count - 1)
                    .DefaultValue = Nothing
                End With

                Dim valor = Format(SystemsOfUnits.Converter.ConvertFromSI(su.heatflow, Me.EnergyFlow.GetValueOrDefault), FlowSheet.Options.NumberFormat)
                .Item.Add(FT(DWSIM.App.GetPropertyName("PROP_ES_0"), su.heatflow), valor, False, DWSIM.App.GetLocalString("Propriedades2"), DWSIM.App.GetLocalString("QuantidadedeEnergyFlowp"), True)
                With .Item(.Item.Count - 1)
                    .CustomTypeConverter = New System.ComponentModel.StringConverter
                    .Tag2 = "PROP_ES_0"
                    .DefaultValue = Nothing
                    .DefaultType = GetType(Nullable(Of Double))
                End With

                If Me.GraphicObject.InputConnectors(0).IsAttached Then
                    If Not Me.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.ObjectType = ObjectType.AbsorptionColumn And
                        Not Me.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.ObjectType = ObjectType.DistillationColumn And
                        Not Me.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.ObjectType = ObjectType.ReboiledAbsorber And
                        Not Me.GraphicObject.InputConnectors(0).AttachedConnector.AttachedFrom.ObjectType = ObjectType.RefluxedAbsorber Then
                        .Item(2).IsReadOnly = True
                    End If
                Else
                    .Item(2).IsReadOnly = False
                End If
                If Me.IsSpecAttached = True Then
                    .Item.Add(DWSIM.App.GetLocalString("ObjetoUtilizadopor"), FlowSheet.Collections.FlowsheetObjectCollection(Me.AttachedSpecId).GraphicObject.Tag, True, DWSIM.App.GetLocalString("Miscelnea4"), "", True)
                    Select Case Me.SpecVarType
                        Case Enums.SpecVarType.Target
                            .Item.Add(DWSIM.App.GetLocalString("Utilizadocomo"), DWSIM.App.GetLocalString(Me.SpecVarType.ToString), True, DWSIM.App.GetLocalString("Miscelnea4"), "", True)
                        Case Enums.SpecVarType.Source
                            .Item.Add(DWSIM.App.GetLocalString("Utilizadocomo"), DWSIM.App.GetLocalString("SpecSource"), True, DWSIM.App.GetLocalString("Miscelnea4"), "", True)
                    End Select
                End If
                .Item.Add("ID", Me.Name, True, DWSIM.App.GetLocalString("Outros"), "", True)
                .Item.Add(DWSIM.App.GetLocalString("LastUpdatedOn"), Me.LastUpdated.ToString("O"), True, DWSIM.App.GetLocalString("Outros"), "", True)
            End With

        End Sub

        Public Overrides Function GetPropertyValue(ByVal prop As String, Optional ByVal su As Interfaces.IUnitsOfMeasure = Nothing) As Object

            If su Is Nothing Then su = New SystemsOfUnits.SI
            Dim cv As New SystemsOfUnits.Converter
            Dim value As Double = 0
            Dim propidx As Integer = Convert.ToInt32(prop.Split("_")(2))

            Select Case propidx

                Case 0
                    'PROP_ES_0	Power
                    value = SystemsOfUnits.Converter.ConvertFromSI(su.heatflow, Me.EnergyFlow.GetValueOrDefault)

            End Select

            Return value

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

            Return m_work
        End Function

#End Region

        Public Overrides Sub HandlePropertyChange(s As Object, e As Windows.Forms.PropertyValueChangedEventArgs)

        End Sub

        Public Overrides Sub PropertyValueChanged(s As Object, e As Windows.Forms.PropertyValueChangedEventArgs)

        End Sub

    End Class

End Namespace
