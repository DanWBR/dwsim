'    Rigorous Columns (Distillation and Absorption) Unit Operations
'    Copyright 2008-2010 Daniel Wagner O. de Medeiros
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

Imports DWSIM.MathOps.MathEx
Imports System.Math
Imports Mapack

Imports DWSIM.Thermodynamics
Imports DWSIM.Thermodynamics.Streams
Imports DWSIM.SharedClasses
Imports System.Windows.Forms
Imports DWSIM.UnitOperations.UnitOperations.Auxiliary
Imports DWSIM.Thermodynamics.BaseClasses
Imports DWSIM.Interfaces.Enums
Imports DWSIM.UnitOperations.UnitOperations.Auxiliary.SepOps
Imports DWSIM.MathOps
Imports DWSIM.DrawingTools
Imports OxyPlot
Imports OxyPlot.Axes

Namespace UnitOperations.Auxiliary.SepOps

    <System.Serializable()> Public Class Parameter

        Implements Interfaces.ICustomXMLSerialization

        Enum ParameterType
            Fixed
            Variable
        End Enum

        Private m_value As Double
        Private m_type As ParameterType = ParameterType.Fixed
        Private _minval, _maxval As Double

        Public Property MaxVal() As Double
            Get
                Return _maxval
            End Get
            Set(ByVal value As Double)
                _maxval = value
            End Set
        End Property

        Public Property MinVal() As Double
            Get
                Return _minval
            End Get
            Set(ByVal value As Double)
                _minval = value
            End Set
        End Property

        Public Property Value() As Double
            Get
                Return m_value
            End Get
            Set(ByVal value As Double)
                m_value = value
            End Set
        End Property

        Public Property ParamType() As ParameterType
            Get
                Return m_type
            End Get
            Set(ByVal value As ParameterType)
                m_type = value
            End Set
        End Property

        Public Overrides Function ToString() As String
            Return Me.Value.ToString
        End Function

        Public Function LoadData(data As System.Collections.Generic.List(Of System.Xml.Linq.XElement)) As Boolean Implements Interfaces.ICustomXMLSerialization.LoadData

            XMLSerializer.XMLSerializer.Deserialize(Me, data)
            Return True

        End Function

        Public Function SaveData() As System.Collections.Generic.List(Of System.Xml.Linq.XElement) Implements Interfaces.ICustomXMLSerialization.SaveData

            Return XMLSerializer.XMLSerializer.Serialize(Me)

        End Function

    End Class

    <System.Serializable()> Public Class Stage

        Implements Interfaces.ICustomXMLSerialization

        Private _f, _lin, _vin, _lout, _vout, _lss, _vss, _q As New Parameter
        Private _eff As Double = 1.0#
        Private _p, _t As Double
        Private _k As New Dictionary(Of String, Parameter)
        Private _l As New Dictionary(Of String, Parameter)
        Private _v As New Dictionary(Of String, Parameter)
        Private _name As String = ""
        Private _id As String = ""

        Public Property Name() As String
            Get
                Return _name
            End Get
            Set(ByVal value As String)
                _name = value
            End Set
        End Property
        Public Property ID() As String
            Get
                Return _id
            End Get
            Set(ByVal value As String)
                _id = value
            End Set
        End Property

        Public ReadOnly Property Kvalues() As Dictionary(Of String, Parameter)
            Get
                Return _k
            End Get
        End Property

        Public ReadOnly Property v() As Dictionary(Of String, Parameter)
            Get
                Return _v
            End Get
        End Property

        Public ReadOnly Property l() As Dictionary(Of String, Parameter)
            Get
                Return _l
            End Get
        End Property

        Public Property P() As Double
            Get
                Return _p
            End Get
            Set(ByVal value As Double)
                _p = value
            End Set
        End Property

        Public Property T() As Double
            Get
                Return _t
            End Get
            Set(ByVal value As Double)
                _t = value
            End Set
        End Property

        Public Property Efficiency() As Double
            Get
                Return _eff
            End Get
            Set(ByVal value As Double)
                _eff = value
            End Set
        End Property

        Public Property Q() As Parameter
            Get
                Return _q
            End Get
            Set(ByVal value As Parameter)
                _q = value
            End Set
        End Property

        Public Property Vss() As Parameter
            Get
                Return _vss
            End Get
            Set(ByVal value As Parameter)
                _vss = value
            End Set
        End Property

        Public Property Lss() As Parameter
            Get
                Return _lss
            End Get
            Set(ByVal value As Parameter)
                _lss = value
            End Set
        End Property

        Public Property Vout() As Parameter
            Get
                Return _vout
            End Get
            Set(ByVal value As Parameter)
                _vout = value
            End Set
        End Property

        Public Property Vin() As Parameter
            Get
                Return _vin
            End Get
            Set(ByVal value As Parameter)
                _vin = value
            End Set
        End Property

        Public Property Lout() As Parameter
            Get
                Return _lout
            End Get
            Set(ByVal value As Parameter)
                _lout = value
            End Set
        End Property

        Public Property Lin() As Parameter
            Get
                Return _lin
            End Get
            Set(ByVal value As Parameter)
                _lin = value
            End Set
        End Property

        Public Property F() As Parameter
            Get
                Return _f
            End Get
            Set(ByVal value As Parameter)
                _f = value
            End Set
        End Property

        Sub New(id As String)

            _id = id

            _k = New Dictionary(Of String, Parameter)
            _l = New Dictionary(Of String, Parameter)
            _v = New Dictionary(Of String, Parameter)

        End Sub

        Public Function LoadData(data As System.Collections.Generic.List(Of System.Xml.Linq.XElement)) As Boolean Implements Interfaces.ICustomXMLSerialization.LoadData

            XMLSerializer.XMLSerializer.Deserialize(Me, data)
            Dim fields As Reflection.PropertyInfo() = Me.GetType.GetProperties()
            For Each fi As Reflection.PropertyInfo In fields
                Dim propname As String = fi.Name
                If TypeOf Me.GetType.GetProperty(fi.Name).PropertyType Is IDictionary(Of String, Parameter) Then
                    Dim xel As XElement = (From xmlprop In data Select xmlprop Where xmlprop.Name = propname).SingleOrDefault
                    If Not xel Is Nothing Then
                        Dim val As List(Of XElement) = xel.Elements.ToList()
                        For Each xel2 As XElement In val
                            Dim p As New Parameter()
                            p.LoadData(xel2.Elements.ToList)
                            DirectCast(Me.GetType.GetProperty(fi.Name).PropertyType, IDictionary(Of String, Parameter)).Add(xel.@Key, p)
                        Next
                    End If
                End If
            Next
            Return True
        End Function

        Public Function SaveData() As System.Collections.Generic.List(Of System.Xml.Linq.XElement) Implements Interfaces.ICustomXMLSerialization.SaveData

            Dim elements As List(Of System.Xml.Linq.XElement) = XMLSerializer.XMLSerializer.Serialize(Me)
            Dim ci As Globalization.CultureInfo = Globalization.CultureInfo.InvariantCulture
            With elements
                Dim fields As Reflection.PropertyInfo() = Me.GetType.GetProperties()
                For Each fi As Reflection.PropertyInfo In fields
                    If TypeOf Me.GetType.GetProperty(fi.Name).PropertyType Is IDictionary(Of String, Parameter) Then
                        Dim collection As IDictionary(Of String, Parameter) = DirectCast(Me.GetType.GetProperty(fi.Name).GetValue(Me, Nothing), IDictionary(Of String, Parameter))
                        .Add(New XElement(fi.Name))
                        For Each kvp As KeyValuePair(Of String, Parameter) In collection
                            .Item(.Count - 1).Add(New XElement("Item", New XAttribute("Key", kvp.Key), kvp.Value.SaveData.ToArray))
                        Next
                    End If
                Next
            End With

            Return elements

        End Function

    End Class

    <System.Serializable()> Public Class InitialEstimates

        Implements Interfaces.ICustomXMLSerialization

        Private _liqcompositions As New List(Of Dictionary(Of String, Parameter))
        Private _vapcompositions As New List(Of Dictionary(Of String, Parameter))
        Private _stagetemps As New List(Of Parameter)
        Private _liqmolflows As New List(Of Parameter)
        Private _vapmolflows As New List(Of Parameter)

        Public Function LoadData(data As System.Collections.Generic.List(Of System.Xml.Linq.XElement)) As Boolean Implements Interfaces.ICustomXMLSerialization.LoadData

            For Each xel As XElement In (From xel2 As XElement In data Select xel2 Where xel2.Name = "LiquidCompositions").SingleOrDefault.Elements.ToList
                Dim var As New Dictionary(Of String, Parameter)
                For Each xel2 As XElement In xel.Elements
                    Dim p As New Parameter
                    p.LoadData(xel2.Elements.ToList)
                    var.Add(xel2.@ID, p)
                Next
                _liqcompositions.Add(var)
            Next

            For Each xel As XElement In (From xel2 As XElement In data Select xel2 Where xel2.Name = "VaporCompositions").SingleOrDefault.Elements.ToList
                Dim var As New Dictionary(Of String, Parameter)
                For Each xel2 As XElement In xel.Elements
                    Dim p As New Parameter
                    p.LoadData(xel2.Elements.ToList)
                    var.Add(xel2.@ID, p)
                Next
                _vapcompositions.Add(var)
            Next

            For Each xel As XElement In (From xel2 As XElement In data Select xel2 Where xel2.Name = "StageTemps").SingleOrDefault.Elements.ToList
                Dim var As New Parameter
                var.LoadData(xel.Elements.ToList)
                _stagetemps.Add(var)
            Next

            For Each xel As XElement In (From xel2 As XElement In data Select xel2 Where xel2.Name = "LiqMoleFlows").SingleOrDefault.Elements.ToList
                Dim var As New Parameter
                var.LoadData(xel.Elements.ToList)
                _liqmolflows.Add(var)
            Next

            For Each xel As XElement In (From xel2 As XElement In data Select xel2 Where xel2.Name = "VapMoleFlows").SingleOrDefault.Elements.ToList
                Dim var As New Parameter
                var.LoadData(xel.Elements.ToList)
                _vapmolflows.Add(var)
            Next
            Return True
        End Function

        Public Function SaveData() As System.Collections.Generic.List(Of System.Xml.Linq.XElement) Implements Interfaces.ICustomXMLSerialization.SaveData

            Dim elements As New List(Of System.Xml.Linq.XElement)
            Dim ci As Globalization.CultureInfo = Globalization.CultureInfo.InvariantCulture

            With elements

                .Add(New XElement("LiquidCompositions"))
                For Each dict As Dictionary(Of String, Parameter) In _liqcompositions
                    .Item(.Count - 1).Add(New XElement("LiquidComposition"))
                    For Each kvp As KeyValuePair(Of String, Parameter) In dict
                        .Item(.Count - 1).Elements.Last.Add(New XElement("Compound", New XAttribute("ID", kvp.Key), kvp.Value.SaveData.ToArray))
                    Next
                Next
                .Add(New XElement("VaporCompositions"))
                For Each dict As Dictionary(Of String, Parameter) In _vapcompositions
                    .Item(.Count - 1).Add(New XElement("VaporComposition"))
                    For Each kvp As KeyValuePair(Of String, Parameter) In dict
                        .Item(.Count - 1).Elements.Last.Add(New XElement("Compound", New XAttribute("ID", kvp.Key), kvp.Value.SaveData.ToArray))
                    Next
                Next
                .Add(New XElement("StageTemps"))
                For Each p As Parameter In _stagetemps
                    .Item(.Count - 1).Add(New XElement("StageTemp", p.SaveData.ToArray))
                Next
                .Add(New XElement("LiqMoleFlows"))
                For Each p As Parameter In _liqmolflows
                    .Item(.Count - 1).Add(New XElement("LiqMoleFlow", p.SaveData.ToArray))
                Next
                .Add(New XElement("VapMoleFlows"))
                For Each p As Parameter In _vapmolflows
                    .Item(.Count - 1).Add(New XElement("VapMoleFlow", p.SaveData.ToArray))
                Next

            End With

            Return elements

        End Function

        Public ReadOnly Property LiqCompositions() As List(Of Dictionary(Of String, Parameter))
            Get
                Return _liqcompositions
            End Get
        End Property

        Public ReadOnly Property VapCompositions() As List(Of Dictionary(Of String, Parameter))
            Get
                Return _vapcompositions
            End Get
        End Property

        Public ReadOnly Property StageTemps() As List(Of Parameter)
            Get
                Return _stagetemps
            End Get
        End Property

        Public ReadOnly Property LiqMolarFlows() As List(Of Parameter)
            Get
                Return _liqmolflows
            End Get
        End Property

        Public ReadOnly Property VapMolarFlows() As List(Of Parameter)
            Get
                Return _vapmolflows
            End Get
        End Property

        Sub New()
            _liqcompositions = New List(Of Dictionary(Of String, Parameter))
            _vapcompositions = New List(Of Dictionary(Of String, Parameter))
            _stagetemps = New List(Of Parameter)
            _liqmolflows = New List(Of Parameter)
            _vapmolflows = New List(Of Parameter)
        End Sub

    End Class

    <System.Serializable()> Public Class StreamInformation

        Implements Interfaces.ICustomXMLSerialization

        Public Enum Type
            Material = 0
            Energy = 1
        End Enum

        Public Enum Behavior
            Distillate = 0
            BottomsLiquid = 1
            Feed = 2
            Sidedraw = 3
            OverheadVapor = 4
            SideOpLiquidProduct = 5
            SideOpVaporProduct = 6
            Steam = 7
            InterExchanger = 8
        End Enum

        Public Enum Phase
            L = 0
            V = 1
            B = 2
            None = 3
        End Enum

        Dim _as As String = ""
        Dim _id As String = ""
        Dim _sideopid As String = ""
        Dim _t As Type = Type.Material
        Dim _bhv As Behavior = Behavior.Feed
        Dim _ph As Phase = Phase.B
        Dim _flow As Parameter

        Public Property StreamID As String = ""

        Public Function LoadData(data As System.Collections.Generic.List(Of System.Xml.Linq.XElement)) As Boolean Implements Interfaces.ICustomXMLSerialization.LoadData

            Dim xel = (From xe In data Select xe Where xe.Name = "Name").SingleOrDefault

            XMLSerializer.XMLSerializer.Deserialize(Me, data)

            If Not xel Is Nothing Then Me.StreamID = xel.Value
            If Me.StreamID = "" Then Me.StreamID = Me.ID

            Return True

        End Function

        Public Function SaveData() As System.Collections.Generic.List(Of System.Xml.Linq.XElement) Implements Interfaces.ICustomXMLSerialization.SaveData

            Return XMLSerializer.XMLSerializer.Serialize(Me)

        End Function

        Public Property FlowRate() As Parameter
            Get
                Return _flow
            End Get
            Set(ByVal value As Parameter)
                _flow = value
            End Set
        End Property

        Public Property ID() As String
            Get
                Return _id
            End Get
            Set(ByVal value As String)
                _id = value
            End Set
        End Property

        Public Property SideOpID() As String
            Get
                Return _sideopid
            End Get
            Set(ByVal value As String)
                _sideopid = value
            End Set
        End Property

        Public Property StreamPhase() As Phase
            Get
                Return _ph
            End Get
            Set(ByVal value As Phase)
                _ph = value
            End Set
        End Property

        Public Property StreamBehavior() As Behavior
            Get
                Return _bhv
            End Get
            Set(ByVal value As Behavior)
                _bhv = value
            End Set
        End Property

        Public Property StreamType() As Type
            Get
                Return _t
            End Get
            Set(ByVal value As Type)
                _t = value
            End Set
        End Property

        Public Property AssociatedStage() As String
            Get
                Return _as
            End Get
            Set(ByVal value As String)
                _as = value
            End Set
        End Property

        Sub New()
            _flow = New Parameter
        End Sub

        Sub New(ByVal id As String, ByVal streamID As String, ByVal associatedstage As String, ByVal t As Type, ByVal bhv As Behavior, ByVal ph As Phase)
            Me.New()
            _id = id
            _as = associatedstage
            _t = t
            _bhv = bhv
            _ph = ph
        End Sub

    End Class

    <System.Serializable()> Public Class ColumnSpec

        Implements Interfaces.ICustomXMLSerialization

        Public Enum SpecType
            Heat_Duty = 0
            Product_Molar_Flow_Rate = 1
            Component_Molar_Flow_Rate = 2
            Product_Mass_Flow_Rate = 3
            Component_Mass_Flow_Rate = 4
            Component_Fraction = 5
            Component_Recovery = 6
            Stream_Ratio = 7
            Temperature = 8
        End Enum

        Private m_stagenumber As Integer = 0
        Private m_type As SpecType = SpecType.Heat_Duty
        Private m_compID As String = ""
        Private m_compindex As Integer = 0
        Private m_value As Double = 0.0#
        Private m_unit As String = ""

        Public Function LoadData(data As System.Collections.Generic.List(Of System.Xml.Linq.XElement)) As Boolean Implements Interfaces.ICustomXMLSerialization.LoadData

            Return XMLSerializer.XMLSerializer.Deserialize(Me, data)

        End Function

        Public Function SaveData() As System.Collections.Generic.List(Of System.Xml.Linq.XElement) Implements Interfaces.ICustomXMLSerialization.SaveData

            Return XMLSerializer.XMLSerializer.Serialize(Me)

        End Function

        Sub New()

        End Sub

        Public Property SpecUnit() As String
            Get
                Return m_unit
            End Get
            Set(ByVal value As String)
                m_unit = value
            End Set
        End Property

        Public Property SpecValue() As Double
            Get
                Return m_value
            End Get
            Set(ByVal value As Double)
                m_value = value
            End Set
        End Property

        Public Property ComponentID() As String
            Get
                Return m_compID
            End Get
            Set(ByVal value As String)
                m_compID = value
            End Set
        End Property

        Public Property ComponentIndex() As Integer
            Get
                Return m_compindex
            End Get
            Set(ByVal value As Integer)
                m_compindex = value
            End Set
        End Property

        Public Property StageNumber() As Integer
            Get
                Return m_stagenumber
            End Get
            Set(ByVal value As Integer)
                m_stagenumber = value
            End Set
        End Property

        Public Property SType() As SpecType
            Get
                Return m_type
            End Get
            Set(ByVal value As SpecType)
                m_type = value
            End Set
        End Property

    End Class

End Namespace

Namespace UnitOperations

    <Serializable()> Public Class DistillationColumn

        Inherits Column

        'solving method (default = IO)

        Public _sm As SolvingMethods.ColSolvingMethod = SolvingMethods.ColSolvingMethod.WangHenke_BubblePoint

        Public Sub New()
            MyBase.New()
        End Sub

        Public Overrides Property SolvingMethod() As Integer
            Get
                Return _sm
            End Get
            Set(ByVal value As Integer)
                _sm = value
            End Set
        End Property

        Public Sub New(ByVal name As String, ByVal description As String, fs As IFlowsheet)
            MyBase.New(name, description, fs)
            Me.ColumnType = ColType.DistillationColumn
            MyBase.AddStages()
            For k2 = 0 To Me.Stages.Count - 1
                Me.Stages(k2).P = 101325
            Next
        End Sub

        Public Overrides Function CloneXML() As Object
            Dim obj As ICustomXMLSerialization = New DistillationColumn()
            obj.LoadData(Me.SaveData)
            Return obj
        End Function

        Public Overrides Function CloneJSON() As Object
            Return Newtonsoft.Json.JsonConvert.DeserializeObject(Of DistillationColumn)(Newtonsoft.Json.JsonConvert.SerializeObject(Me))
        End Function

        Public Overloads Overrides Function GetProperties(ByVal proptype As Interfaces.Enums.PropertyType) As String()
            Dim i As Integer = 0
            Dim proplist As New ArrayList
            Dim basecol = MyBase.GetProperties(proptype)
            If basecol.Length > 0 Then proplist.AddRange(basecol)
            Select Case proptype
                Case PropertyType.RO
                    For i = 5 To 7
                        proplist.Add("PROP_DC_" + CStr(i))
                    Next
                    For i = 1 To Me.Stages.Count
                        proplist.Add("Stage_Temperature_" + CStr(i))
                    Next
                Case PropertyType.RW, PropertyType.ALL
                    For i = 0 To 7
                        proplist.Add("PROP_DC_" + CStr(i))
                    Next
                    For i = 1 To Me.Stages.Count
                        proplist.Add("Stage_Pressure_" + CStr(i))
                    Next
                    For i = 1 To Me.Stages.Count
                        proplist.Add("Stage_Efficiency_" + CStr(i))
                    Next
                    For i = 1 To Me.Stages.Count
                        proplist.Add("Stage_Temperature_" + CStr(i))
                    Next
                    proplist.Add("Condenser_Specification_Value")
                    proplist.Add("Reboiler_Specification_Value")
                    proplist.Add("Global_Stage_Efficiency")
                Case PropertyType.WR
                    For i = 0 To 4
                        proplist.Add("PROP_DC_" + CStr(i))
                    Next
                    For i = 1 To Me.Stages.Count
                        proplist.Add("Stage_Pressure_" + CStr(i))
                    Next
                    For i = 1 To Me.Stages.Count
                        proplist.Add("Stage_Efficiency_" + CStr(i))
                    Next
                    proplist.Add("Condenser_Specification_Value")
                    proplist.Add("Reboiler_Specification_Value")
                    proplist.Add("Global_Stage_Efficiency")
            End Select
            Return proplist.ToArray(GetType(System.String))
            proplist = Nothing
        End Function

        Public Overrides Function GetPropertyValue(ByVal prop As String, Optional ByVal su As Interfaces.IUnitsOfMeasure = Nothing) As Object
            Dim val0 As Object = MyBase.GetPropertyValue(prop, su)

            If Not val0 Is Nothing Then
                Return val0
            Else
                If su Is Nothing Then su = New SystemsOfUnits.SI
                Dim cv As New SystemsOfUnits.Converter
                Dim value As Object = Nothing
                Dim propidx As Integer = -1
                Integer.TryParse(prop.Split("_")(2), propidx)

                Select Case propidx

                    Case 0
                        'PROP_DC_0	Condenser Pressure
                        value = SystemsOfUnits.Converter.ConvertFromSI(su.pressure, Me.CondenserPressure)
                    Case 1
                        'PROP_DC_1	Reboiler Pressure
                        value = SystemsOfUnits.Converter.ConvertFromSI(su.pressure, Me.ReboilerPressure)
                    Case 2
                        'PROP_DC_2	Condenser Pressure Drop
                        value = SystemsOfUnits.Converter.ConvertFromSI(su.deltaP, Me.CondenserDeltaP)
                    Case 3
                        'reflux ratio
                        value = Me.RefluxRatio
                    Case 4
                        'distillate molar flow
                        If LSSf IsNot Nothing AndAlso LSSf.Length > 0 Then
                            value = SystemsOfUnits.Converter.ConvertFromSI(su.molarflow, LSSf(0))
                        Else
                            value = 0.0
                        End If
                    Case 5
                        'PROP_DC_5	Condenser Duty
                        value = SystemsOfUnits.Converter.ConvertFromSI(su.heatflow, Me.CondenserDuty)
                    Case 6
                        'PROP_DC_6	Reboiler Duty
                        value = SystemsOfUnits.Converter.ConvertFromSI(su.heatflow, Me.ReboilerDuty)
                    Case 7
                        'PROP_DC_7	Number of Stages
                        value = Me.NumberOfStages
                End Select

                Select Case prop
                    Case "Condenser_Specification_Value"
                        value = Me.Specs("C").SpecValue
                    Case "Reboiler_Specification_Value"
                        value = Me.Specs("R").SpecValue
                End Select

                If prop.Contains("Stage_Pressure_") Then
                    Dim stageindex As Integer = prop.Split("_")(2)
                    If Me.Stages.Count >= stageindex Then value = SystemsOfUnits.Converter.ConvertFromSI(su.pressure, Me.Stages(stageindex - 1).P)
                End If

                If prop.Contains("Stage_Temperature_") Then
                    Dim stageindex As Integer = prop.Split("_")(2)
                    If Me.Stages.Count >= stageindex Then value = SystemsOfUnits.Converter.ConvertFromSI(su.temperature, Me.Stages(stageindex - 1).T)
                End If

                If prop.Contains("Stage_Efficiency_") Then
                    Dim stageindex As Integer = prop.Split("_")(2)
                    If Me.Stages.Count >= stageindex Then value = Me.Stages(stageindex - 1).Efficiency
                End If

                If prop.Contains("Global_Stage_Efficiency") Then value = "N/D"

                Return value
            End If

        End Function

        Public Overrides Function GetPropertyUnit(ByVal prop As String, Optional ByVal su As Interfaces.IUnitsOfMeasure = Nothing) As String
            Dim u0 As String = MyBase.GetPropertyUnit(prop, su)

            If u0 <> "NF" Then
                Return u0
            Else
                If su Is Nothing Then su = New SystemsOfUnits.SI
                Dim cv As New SystemsOfUnits.Converter
                Dim value As String = ""
                Dim propidx As Integer = -1
                Integer.TryParse(prop.Split("_")(2), propidx)

                Select Case propidx

                    Case 0
                        'PROP_DC_0	Condenser Pressure
                        value = su.pressure
                    Case 1
                        'PROP_DC_1	Reboiler Pressure
                        value = su.pressure
                    Case 2
                        'PROP_DC_2	Condenser Pressure Drop
                        value = su.deltaP
                    Case 4
                        value = su.molarflow
                    Case 5
                        'PROP_DC_5	Condenser Duty
                        value = su.heatflow
                    Case 6
                        'PROP_DC_6	Reboiler Duty
                        value = su.heatflow
                    Case 7
                        'PROP_DC_7	Number of Stages
                        value = ""
                End Select

                Select Case prop
                    Case "Condenser_Specification_Value"
                        value = Me.Specs("C").SpecUnit
                    Case "Reboiler_Specification_Value"
                        value = Me.Specs("R").SpecUnit
                End Select

                If prop.Contains("Stage_Pressure") Then value = su.pressure
                If prop.Contains("Stage_Temperature") Then value = su.temperature
                If prop.Contains("Stage_Efficiency") Then value = ""

                Return value
            End If
        End Function

        Public Overrides Function SetPropertyValue(ByVal prop As String, ByVal propval As Object, Optional ByVal su As Interfaces.IUnitsOfMeasure = Nothing) As Boolean

            If MyBase.SetPropertyValue(prop, propval, su) Then Return True

            If su Is Nothing Then su = New SystemsOfUnits.SI
            Dim cv As New SystemsOfUnits.Converter

            If prop.Contains("PROP_DC") Then
                Dim propidx As Integer = -1
                Integer.TryParse(prop.Split("_")(2), propidx)
                Select Case propidx
                    Case 0
                        'PROP_DC_0	Condenser Pressure
                        Me.CondenserPressure = SystemsOfUnits.Converter.ConvertToSI(su.pressure, propval)
                    Case 1
                        'PROP_DC_1	Reboiler Pressure
                        Me.ReboilerPressure = SystemsOfUnits.Converter.ConvertToSI(su.pressure, propval)
                    Case 2
                        'PROP_DC_2	Condenser Pressure Drop
                        Me.CondenserDeltaP = SystemsOfUnits.Converter.ConvertToSI(su.deltaP, propval)
                End Select
            End If

            Select Case prop
                Case "Condenser_Specification_Value"
                    Me.Specs("C").SpecValue = propval
                Case "Reboiler_Specification_Value"
                    Me.Specs("R").SpecValue = propval
            End Select

            If prop.Contains("Stage_Pressure_") Then
                Dim stageindex As Integer = prop.Split("_")(2)
                If Me.Stages.Count >= stageindex Then Me.Stages(stageindex - 1).P = SystemsOfUnits.Converter.ConvertToSI(su.pressure, propval)
            End If

            If prop.Contains("Stage_Temperature_") Then
                Dim stageindex As Integer = prop.Split("_")(2)
                If Me.Stages.Count >= stageindex Then Me.Stages(stageindex - 1).T = SystemsOfUnits.Converter.ConvertToSI(su.temperature, propval)
            End If

            If prop.Contains("Stage_Efficiency_") Then
                Dim stageindex As Integer = prop.Split("_")(2)
                If Me.Stages.Count >= stageindex Then Me.Stages(stageindex - 1).Efficiency = propval
            End If

            If prop = "Global_Stage_Efficiency" Then
                For Each st As Stage In Me.Stages
                    st.Efficiency = propval
                Next
            End If

            Return 1

        End Function

        Public Overrides Function GetIconBitmap() As Object
            Return My.Resources.col_dc_32
        End Function

        Public Overrides Function GetDisplayDescription() As String
            Return ResMan.GetLocalString("CDEST_Desc")
        End Function

        Public Overrides Function GetDisplayName() As String
            Return ResMan.GetLocalString("CDEST_Name")
        End Function

        Public Overrides ReadOnly Property MobileCompatible As Boolean
            Get
                Return True
            End Get
        End Property

        Public Overrides Function GetReport(su As IUnitsOfMeasure, ci As Globalization.CultureInfo, numberformat As String) As String

            Dim str As New Text.StringBuilder

            str.AppendLine("Distillation Column: " & Me.GraphicObject.Tag)
            str.AppendLine("Property Package: " & Me.PropertyPackage.ComponentName)
            str.AppendLine()
            str.AppendLine("Calculation parameters")
            str.AppendLine()
            str.AppendLine("    Condenser type: " & Me.CondenserType.ToString)
            str.AppendLine("    Condenser Pressure: " & SystemsOfUnits.Converter.ConvertFromSI(su.pressure, Me.CondenserPressure).ToString(numberformat, ci) & " " & su.pressure)
            str.AppendLine("    Reboiler Pressure: " & SystemsOfUnits.Converter.ConvertFromSI(su.pressure, Me.ReboilerPressure).ToString(numberformat, ci) & " " & su.pressure)
            str.AppendLine("    Number of Stages: " & Me.Stages.Count)
            str.AppendLine()
            str.AppendLine("Results")
            str.AppendLine()
            str.AppendLine("    Condenser heat duty: " & SystemsOfUnits.Converter.ConvertFromSI(su.heatflow, Me.CondenserDuty).ToString(numberformat, ci) & " " & su.heatflow)
            str.AppendLine("    Reboiler heat duty: " & SystemsOfUnits.Converter.ConvertFromSI(su.heatflow, Me.ReboilerDuty).ToString(numberformat, ci) & " " & su.heatflow)
            str.AppendLine()
            str.AppendLine("Column Profiles")
            str.AppendLine()
            str.AppendLine(("Stage").PadRight(20) & ("Temperature (" & su.temperature & ")").PadRight(20))
            For i As Integer = 0 To Tf.Count - 1
                str.AppendLine(i.ToString.PadRight(20) & SystemsOfUnits.Converter.ConvertFromSI(su.temperature, Tf(i)).ToString(numberformat, ci).PadRight(20))
            Next
            str.AppendLine()
            str.AppendLine(("Stage").PadRight(20) & ("Pressure (" & su.pressure & ")").PadRight(20))
            For i As Integer = 0 To P0.Count - 1
                str.AppendLine(i.ToString.PadRight(20) & SystemsOfUnits.Converter.ConvertFromSI(su.pressure, P0(i)).ToString(numberformat, ci).PadRight(20))
            Next
            str.AppendLine()
            str.AppendLine(("Stage").PadRight(20) & ("Vapor Flow (" & su.molarflow & ")").PadRight(20))
            For i As Integer = 0 To Vf.Count - 1
                str.AppendLine(i.ToString.PadRight(20) & SystemsOfUnits.Converter.ConvertFromSI(su.molarflow, Vf(i)).ToString(numberformat, ci).PadRight(20))
            Next
            str.AppendLine()
            str.AppendLine(("Stage").PadRight(20) & ("Liquid Flow (" & su.molarflow & ")").PadRight(20))
            For i As Integer = 0 To Lf.Count - 1
                str.AppendLine(i.ToString.PadRight(20) & SystemsOfUnits.Converter.ConvertFromSI(su.molarflow, Lf(i)).ToString(numberformat, ci).PadRight(20))
            Next

            Return str.ToString

        End Function

    End Class

    <Serializable()> Public Class AbsorptionColumn

        Inherits Column

        Public _sm As SolvingMethods.ColSolvingMethod

        Public _opmode As OpMode = OpMode.Absorber

        Public Sub New()
            MyBase.New()
        End Sub

        Public Enum OpMode
            Absorber
            Extractor
        End Enum

        Public Property OperationMode() As OpMode
            Get
                Return _opmode
            End Get
            Set(ByVal value As OpMode)
                _opmode = value
            End Set
        End Property

        Public Overrides Property SolvingMethod() As Integer
            Get
                Return _sm
            End Get
            Set(ByVal value As Integer)
                _sm = value
            End Set
        End Property

        Public Sub New(ByVal name As String, ByVal description As String, fs As IFlowsheet)
            MyBase.New(name, description, fs)
            Me.ColumnType = ColType.AbsorptionColumn
            MyBase.AddStages()
            For k2 = 0 To Me.Stages.Count - 1
                Me.Stages(k2).P = 101325
            Next
        End Sub

        Public Overrides Function CloneXML() As Object
            Dim obj As ICustomXMLSerialization = New AbsorptionColumn()
            obj.LoadData(Me.SaveData)
            Return obj
        End Function

        Public Overrides Function CloneJSON() As Object
            Return Newtonsoft.Json.JsonConvert.DeserializeObject(Of AbsorptionColumn)(Newtonsoft.Json.JsonConvert.SerializeObject(Me))
        End Function

        Public Overloads Overrides Function GetProperties(ByVal proptype As Interfaces.Enums.PropertyType) As String()
            Dim i As Integer = 0
            Dim proplist As New ArrayList
            Dim basecol = MyBase.GetProperties(proptype)
            If basecol.Length > 0 Then proplist.AddRange(basecol)
            Select Case proptype
                Case PropertyType.RO
                    For i = 2 To 2
                        proplist.Add("PROP_AC_" + CStr(i))
                    Next
                Case PropertyType.RW
                    For i = 0 To 2
                        proplist.Add("PROP_AC_" + CStr(i))
                    Next
                Case PropertyType.WR
                    For i = 0 To 1
                        proplist.Add("PROP_AC_" + CStr(i))
                    Next
                Case PropertyType.ALL
                    For i = 0 To 2
                        proplist.Add("PROP_AC_" + CStr(i))
                    Next
            End Select
            Return proplist.ToArray(GetType(System.String))
            proplist = Nothing
        End Function

        Public Overrides Function GetPropertyValue(ByVal prop As String, Optional ByVal su As Interfaces.IUnitsOfMeasure = Nothing) As Object
            Dim val0 As Object = MyBase.GetPropertyValue(prop, su)

            If Not val0 Is Nothing Then
                Return val0
            Else
                If su Is Nothing Then su = New SystemsOfUnits.SI
                Dim cv As New SystemsOfUnits.Converter
                Dim value As Double = 0
                Dim propidx As Integer = Convert.ToInt32(prop.Split("_")(2))

                Select Case propidx

                    Case 0
                        'PROP_DC_0	Condenser Pressure
                        value = SystemsOfUnits.Converter.ConvertFromSI(su.pressure, Me.CondenserPressure)
                    Case 1
                        'PROP_DC_1	Reboiler Pressure
                        value = SystemsOfUnits.Converter.ConvertFromSI(su.pressure, Me.ReboilerPressure)
                    Case 2
                        'PROP_DC_7	Number of Stages
                        value = Me.NumberOfStages
                End Select

                Return value
            End If

        End Function

        Public Overrides Function GetPropertyUnit(ByVal prop As String, Optional ByVal su As Interfaces.IUnitsOfMeasure = Nothing) As String
            If su Is Nothing Then su = New SystemsOfUnits.SI
            Dim cv As New SystemsOfUnits.Converter
            Dim value As String = ""
            Dim propidx As Integer = Convert.ToInt32(prop.Split("_")(2))

            Select Case propidx

                Case 0
                    'PROP_DC_0	Condenser Pressure
                    value = su.pressure
                Case 1
                    'PROP_DC_1	Reboiler Pressure
                    value = su.pressure
                Case 2
                    'PROP_DC_7	Number of Stages
                    value = ""
            End Select

            Return value
        End Function

        Public Overrides Function SetPropertyValue(ByVal prop As String, ByVal propval As Object, Optional ByVal su As Interfaces.IUnitsOfMeasure = Nothing) As Boolean

            If MyBase.SetPropertyValue(prop, propval, su) Then Return True

            If su Is Nothing Then su = New SystemsOfUnits.SI
            Dim cv As New SystemsOfUnits.Converter
            Dim propidx As Integer = Convert.ToInt32(prop.Split("_")(2))

            Select Case propidx

                Case 0
                    'PROP_DC_0	Condenser Pressure
                    Me.CondenserPressure = SystemsOfUnits.Converter.ConvertToSI(su.pressure, propval)
                Case 1
                    'PROP_DC_1	Reboiler Pressure
                    Me.ReboilerPressure = SystemsOfUnits.Converter.ConvertToSI(su.pressure, propval)

            End Select
            Return 1
        End Function

        Public Overrides Function GetIconBitmap() As Object
            Return My.Resources.col_abs_32
        End Function

        Public Overrides Function GetDisplayDescription() As String
            Return ResMan.GetLocalString("CABS_Desc")
        End Function

        Public Overrides Function GetDisplayName() As String
            Return ResMan.GetLocalString("CABS_Name")
        End Function

        Public Overrides ReadOnly Property MobileCompatible As Boolean
            Get
                Return True
            End Get
        End Property

        Public Overrides Function GetReport(su As IUnitsOfMeasure, ci As Globalization.CultureInfo, numberformat As String) As String

            Dim str As New Text.StringBuilder

            str.AppendLine("Absorption Column: " & Me.GraphicObject.Tag)
            str.AppendLine("Property Package: " & Me.PropertyPackage.ComponentName)
            str.AppendLine()
            str.AppendLine("Calculation parameters")
            str.AppendLine()
            str.AppendLine("    Number of Stages: " & Me.Stages.Count)
            str.AppendLine()
            str.AppendLine("Column Profiles")
            str.AppendLine()
            str.AppendLine(("Stage").PadRight(20) & ("Temperature (" & su.temperature & ")").PadRight(20))
            For i As Integer = 0 To Tf.Count - 1
                str.AppendLine(i.ToString.PadRight(20) & SystemsOfUnits.Converter.ConvertFromSI(su.temperature, Tf(i)).ToString(numberformat, ci).PadRight(20))
            Next
            str.AppendLine()
            str.AppendLine(("Stage").PadRight(20) & ("Pressure (" & su.pressure & ")").PadRight(20))
            For i As Integer = 0 To P0.Count - 1
                str.AppendLine(i.ToString.PadRight(20) & SystemsOfUnits.Converter.ConvertFromSI(su.pressure, P0(i)).ToString(numberformat, ci).PadRight(20))
            Next
            str.AppendLine()
            str.AppendLine(("Stage").PadRight(20) & ("Vapor Flow (" & su.molarflow & ")").PadRight(20))
            For i As Integer = 0 To Vf.Count - 1
                str.AppendLine(i.ToString.PadRight(20) & SystemsOfUnits.Converter.ConvertFromSI(su.molarflow, Vf(i)).ToString(numberformat, ci).PadRight(20))
            Next
            str.AppendLine()
            str.AppendLine(("Stage").PadRight(20) & ("Liquid Flow (" & su.molarflow & ")").PadRight(20))
            For i As Integer = 0 To Lf.Count - 1
                str.AppendLine(i.ToString.PadRight(20) & SystemsOfUnits.Converter.ConvertFromSI(su.molarflow, Lf(i)).ToString(numberformat, ci).PadRight(20))
            Next

            Return str.ToString

        End Function



    End Class

    <Serializable()> Public Class ReboiledAbsorber

        Inherits Column

        'solving method (default = IO)

        Public _sm As SolvingMethods.ColSolvingMethod

        Public Overrides Property Visible As Boolean = False

        Public Sub New()
            MyBase.New()
        End Sub

        Public Overrides Property SolvingMethod() As Integer
            Get
                Return _sm
            End Get
            Set(ByVal value As Integer)
                _sm = value
            End Set
        End Property

        Public Sub New(ByVal name As String, ByVal description As String, fs As IFlowsheet)
            MyBase.New(name, description, fs)
            Me.ColumnType = ColType.ReboiledAbsorber
            MyBase.AddStages()
            For k2 = 0 To Me.Stages.Count - 1
                Me.Stages(k2).P = 101325
            Next
        End Sub

        Public Overrides Function CloneXML() As Object
            Dim obj As ICustomXMLSerialization = New ReboiledAbsorber()
            obj.LoadData(Me.SaveData)
            Return obj
        End Function

        Public Overrides Function CloneJSON() As Object
            Return Newtonsoft.Json.JsonConvert.DeserializeObject(Of ReboiledAbsorber)(Newtonsoft.Json.JsonConvert.SerializeObject(Me))
        End Function

        Public Overloads Overrides Function GetProperties(ByVal proptype As Interfaces.Enums.PropertyType) As String()
            Dim i As Integer = 0
            Dim proplist As New ArrayList
            Select Case proptype
                Case PropertyType.RO
                    For i = 3 To 4
                        proplist.Add("PROP_RA_" + CStr(i))
                    Next
                Case PropertyType.RW
                    For i = 0 To 4
                        proplist.Add("PROP_RA_" + CStr(i))
                    Next
                Case PropertyType.WR
                    For i = 0 To 2
                        proplist.Add("PROP_RA_" + CStr(i))
                    Next
                Case PropertyType.ALL
                    For i = 0 To 4
                        proplist.Add("PROP_RA_" + CStr(i))
                    Next
            End Select
            Return proplist.ToArray(GetType(System.String))
            proplist = Nothing
        End Function

        Public Overrides Function GetPropertyValue(ByVal prop As String, Optional ByVal su As Interfaces.IUnitsOfMeasure = Nothing) As Object
            If su Is Nothing Then su = New SystemsOfUnits.SI
            Dim cv As New SystemsOfUnits.Converter
            Dim value As Double = 0
            Dim propidx As Integer = Convert.ToInt32(prop.Split("_")(2))

            Select Case propidx

                Case 0
                    'PROP_DC_0	Condenser Pressure
                    value = SystemsOfUnits.Converter.ConvertFromSI(su.pressure, Me.CondenserPressure)
                Case 1
                    'PROP_DC_1	Reboiler Pressure
                    value = SystemsOfUnits.Converter.ConvertFromSI(su.pressure, Me.ReboilerPressure)
                Case 3
                    'PROP_DC_6	Reboiler Duty
                    value = SystemsOfUnits.Converter.ConvertFromSI(su.heatflow, Me.ReboilerDuty)
                Case 4
                    'PROP_DC_7	Number of Stages
                    value = Me.NumberOfStages
            End Select

            Return value
        End Function

        Public Overrides Function GetPropertyUnit(ByVal prop As String, Optional ByVal su As Interfaces.IUnitsOfMeasure = Nothing) As String
            If su Is Nothing Then su = New SystemsOfUnits.SI
            Dim cv As New SystemsOfUnits.Converter
            Dim value As String = ""
            Dim propidx As Integer = Convert.ToInt32(prop.Split("_")(2))

            Select Case propidx

                Case 0
                    'PROP_DC_0	Condenser Pressure
                    value = su.pressure
                Case 1
                    'PROP_DC_1	Reboiler Pressure
                    value = su.pressure
                Case 2
                    'PROP_DC_3	Reflux Ratio
                    value = ""
                Case 3
                    'PROP_DC_6	Reboiler Duty
                    value = su.heatflow
                Case 4
                    'PROP_DC_7	Number of Stages
                    value = ""
            End Select

            Return value
        End Function

        Public Overrides Function SetPropertyValue(ByVal prop As String, ByVal propval As Object, Optional ByVal su As Interfaces.IUnitsOfMeasure = Nothing) As Boolean
            If su Is Nothing Then su = New SystemsOfUnits.SI
            Dim cv As New SystemsOfUnits.Converter
            Dim propidx As Integer = Convert.ToInt32(prop.Split("_")(2))

            Select Case propidx

                Case 0
                    'PROP_DC_0	Condenser Pressure
                    Me.CondenserPressure = SystemsOfUnits.Converter.ConvertToSI(su.pressure, propval)
                Case 1
                    'PROP_DC_1	Reboiler Pressure
                    Me.ReboilerPressure = SystemsOfUnits.Converter.ConvertToSI(su.pressure, propval)

            End Select
            Return 1
        End Function

        Public Overrides Function GetIconBitmap() As Object
            Return My.Resources.col_rebabs_32
        End Function

        Public Overrides Function GetDisplayDescription() As String
            Return ResMan.GetLocalString("CRABS_Desc")
        End Function

        Public Overrides Function GetDisplayName() As String
            Return ResMan.GetLocalString("CRABS_Name")
        End Function

        Public Overrides ReadOnly Property MobileCompatible As Boolean
            Get
                Return False
            End Get
        End Property
    End Class

    <Serializable()> Public Class RefluxedAbsorber

        Inherits Column

        'solving method (default = IO)

        Public _sm As SolvingMethods.ColSolvingMethod

        Public Overrides Property Visible As Boolean = False

        Public Sub New()
            MyBase.New()
        End Sub

        Public Overrides Property SolvingMethod() As Integer
            Get
                Return _sm
            End Get
            Set(ByVal value As Integer)
                _sm = value
            End Set
        End Property

        Public Sub New(ByVal name As String, ByVal description As String, fs As IFlowsheet)
            MyBase.New(name, description, fs)
            Me.ColumnType = ColType.RefluxedAbsorber
            MyBase.AddStages()
            For k2 = 0 To Me.Stages.Count - 1
                Me.Stages(k2).P = 101325
            Next
        End Sub

        Public Overrides Function CloneXML() As Object
            Dim obj As ICustomXMLSerialization = New RefluxedAbsorber()
            obj.LoadData(Me.SaveData)
            Return obj
        End Function

        Public Overrides Function CloneJSON() As Object
            Return Newtonsoft.Json.JsonConvert.DeserializeObject(Of RefluxedAbsorber)(Newtonsoft.Json.JsonConvert.SerializeObject(Me))
        End Function

        Public Overloads Overrides Function GetProperties(ByVal proptype As Interfaces.Enums.PropertyType) As String()
            Dim i As Integer = 0
            Dim proplist As New ArrayList
            Select Case proptype
                Case PropertyType.RO
                    For i = 5 To 6
                        proplist.Add("PROP_RF_" + CStr(i))
                    Next
                Case PropertyType.RW
                    For i = 0 To 6
                        proplist.Add("PROP_RF_" + CStr(i))
                    Next
                Case PropertyType.WR
                    For i = 0 To 4
                        proplist.Add("PROP_RF_" + CStr(i))
                    Next
                Case PropertyType.ALL
                    For i = 0 To 6
                        proplist.Add("PROP_RF_" + CStr(i))
                    Next
            End Select
            Return proplist.ToArray(GetType(System.String))
            proplist = Nothing
        End Function

        Public Overrides Function GetPropertyValue(ByVal prop As String, Optional ByVal su As Interfaces.IUnitsOfMeasure = Nothing) As Object

            If su Is Nothing Then su = New SystemsOfUnits.SI

            Dim cv As New SystemsOfUnits.Converter
            Dim value As Double = 0
            Dim propidx As Integer = Convert.ToInt32(prop.Split("_")(2))

            Select Case propidx

                Case 0
                    'PROP_DC_0	Condenser Pressure
                    value = SystemsOfUnits.Converter.ConvertFromSI(su.pressure, Me.CondenserPressure)
                Case 1
                    'PROP_DC_1	Reboiler Pressure
                    value = SystemsOfUnits.Converter.ConvertFromSI(su.pressure, Me.ReboilerPressure)
                Case 2
                    'PROP_DC_2	Condenser Pressure Drop
                    value = SystemsOfUnits.Converter.ConvertFromSI(su.deltaP, Me.CondenserDeltaP)
                Case 3
                    'reflux ratio
                    value = Me.RefluxRatio
                Case 4
                    'distillate molar flow
                    If LSSf IsNot Nothing AndAlso LSSf.Length > 0 Then
                        value = SystemsOfUnits.Converter.ConvertFromSI(su.molarflow, LSSf(0))
                    Else
                        value = 0.0
                    End If
                Case 5
                    'PROP_DC_5	Condenser Duty
                    value = SystemsOfUnits.Converter.ConvertFromSI(su.heatflow, Me.CondenserDuty)
                Case 6
                    'PROP_DC_7	Number of Stages
                    value = Me.NumberOfStages
            End Select

            Return value

        End Function

        Public Overrides Function GetPropertyUnit(ByVal prop As String, Optional ByVal su As Interfaces.IUnitsOfMeasure = Nothing) As String
            If su Is Nothing Then su = New SystemsOfUnits.SI
            Dim cv As New SystemsOfUnits.Converter
            Dim value As String = ""
            Dim propidx As Integer = Convert.ToInt32(prop.Split("_")(2))

            Select Case propidx

                Case 0
                    'PROP_DC_0	Condenser Pressure
                    value = su.pressure
                Case 1
                    'PROP_DC_1	Reboiler Pressure
                    value = su.pressure
                Case 2
                    'PROP_DC_2	Condenser Pressure Drop
                    value = su.deltaP
                Case 4
                    value = su.molarflow
                Case 5
                    'PROP_DC_5	Condenser Duty
                    value = su.heatflow
                Case 6
                    'PROP_DC_7	Number of Stages
                    value = ""
            End Select

            Return value
        End Function

        Public Overrides Function SetPropertyValue(ByVal prop As String, ByVal propval As Object, Optional ByVal su As Interfaces.IUnitsOfMeasure = Nothing) As Boolean
            If su Is Nothing Then su = New SystemsOfUnits.SI
            Dim cv As New SystemsOfUnits.Converter
            Dim propidx As Integer = Convert.ToInt32(prop.Split("_")(2))

            Select Case propidx

                Case 0
                    'PROP_DC_0	Condenser Pressure
                    Me.CondenserPressure = SystemsOfUnits.Converter.ConvertToSI(su.pressure, propval)
                Case 1
                    'PROP_DC_1	Reboiler Pressure
                    Me.ReboilerPressure = SystemsOfUnits.Converter.ConvertToSI(su.pressure, propval)
                Case 2
                    'PROP_DC_2	Condenser Pressure Drop
                    Me.CondenserDeltaP = SystemsOfUnits.Converter.ConvertToSI(su.deltaP, propval)

            End Select
            Return 1
        End Function

        Public Overrides Function GetIconBitmap() As Object
            Return My.Resources.col_rflabs_32
        End Function

        Public Overrides Function GetDisplayDescription() As String
            Return ResMan.GetLocalString("CRFABS_Desc")
        End Function

        Public Overrides Function GetDisplayName() As String
            Return ResMan.GetLocalString("CRFABS_Name")
        End Function

        Public Overrides ReadOnly Property MobileCompatible As Boolean
            Get
                Return False
            End Get
        End Property
    End Class

    <System.Serializable()> Public MustInherit Class Column

        Inherits UnitOperations.UnitOpBaseClass
        Public Overrides Property ObjectClass As SimulationObjectClass = SimulationObjectClass.Columns

        <NonSerialized> <Xml.Serialization.XmlIgnore> Public f As EditingForm_Column

        Public Enum ColType
            DistillationColumn = 0
            AbsorptionColumn = 1
            ReboiledAbsorber = 2
            RefluxedAbsorber = 3
        End Enum

        Public Enum SolvingScheme
            Ideal_K_Init = 0
            Ideal_Enthalpy_Init = 1
            Ideal_K_and_Enthalpy_Init = 2
            Direct = 3
        End Enum

        'column type
        Private _type As ColType = Column.ColType.DistillationColumn

        'stage numbering is up to bottom. 
        'condenser (when applicable) is the 0th stage.
        'reboiler (when applicable) is the nth stage. 

        Private _cond As New Stage(Guid.NewGuid().ToString)
        Private _reb As New Stage(Guid.NewGuid().ToString)


        'stream collections (for the *entire* column, including side operations)

        Private _conn_ms As New System.Collections.Generic.Dictionary(Of String, StreamInformation)
        Private _conn_es As New System.Collections.Generic.Dictionary(Of String, StreamInformation)

        'iteration variables

        Private _maxiterations As Integer = 100
        Private _ilooptolerance As Double = 0.000001
        Private _elooptolerance As Double = 0.00001
        Private _adjSb As Boolean = False
        Private _kbjWA As Boolean = False
        Private _uijac As Boolean = False
        Private _udf As Boolean = True
        Private _unu As Boolean = False
        Private _stopatiter As Integer = -1
        Private _scdf As Double = 0.5
        Private _scmaxtc As Double = 10.0#
        Private _scnderivstep As Double = 0.0000000001
        Private _ionderivstep As Double = 0.0000000001
        Private _iomaxvarchgfac As Integer = 10
        Private _scmaxvarchgfac As Integer = 10
        Private _iodfmin As Double = 0.1#
        Private _iodfmax As Double = 2.0#
        Private _iodeltat_el As Double = 0.01#

        'new solver parameters

        Public Property NS_Solver As OptimizationMethod = OptimizationMethod.Truncated_Newton
        Public Property NS_SimplexPreconditioning As Boolean = False
        Public Property NS_LowerBound As Double = 0.0#
        Public Property NS_UpperBound As Double = 2.0#

        Public Property IO_Solver As OptimizationMethod = OptimizationMethod.Simplex
        Public Property IO_LowerBound As Double = Log(1.0E-40)
        Public Property IO_UpperBound As Double = Log(5.0#)

        Public Property MaximumTemperatureStep As Double = 10

        Public Property SolverScheme As SolvingScheme = SolvingScheme.Direct

        'general variables

        Private _nst As Integer = 12
        Private _rr As Double = 5.0#
        Private _condp As Double = 101325
        Private _rebp As Double = 101325
        Private _conddp, _drate, _vrate, _condd, _rebd As Double
        Private _st As New List(Of Auxiliary.SepOps.Stage)
        Public Property CondenserType As condtype = condtype.Total_Condenser
        Private m_specs As New Collections.Generic.Dictionary(Of String, Auxiliary.SepOps.ColumnSpec)
        Private m_jac As Object
        Private _vrateunit As String = "mol/s"

        'initial estimates

        Private _use_ie As Boolean = False
        Private _use_ie1 As Boolean = False
        Private _use_ie2 As Boolean = False
        Private _use_ie3 As Boolean = False
        Private _ie As New InitialEstimates
        Private _autoupdie As Boolean = False
        Private _storejac As Boolean = True

        Public Overrides Function LoadData(data As System.Collections.Generic.List(Of System.Xml.Linq.XElement)) As Boolean

            MyBase.LoadData(data)

            If Not Stages Is Nothing Then Stages.Clear()
            For Each xel As XElement In (From xel2 As XElement In data Select xel2 Where xel2.Name = "Stages").SingleOrDefault.Elements.ToList
                Dim id As String = xel.@ID
                If id = "" Then id = Guid.NewGuid().ToString
                Dim var As New Stage(id)
                var.LoadData(xel.Elements.ToList)
                Stages.Add(var)
            Next

            If _conn_ms.Count = 0 Then
                For Each xel As XElement In (From xel2 As XElement In data Select xel2 Where xel2.Name = "MaterialStreams").SingleOrDefault.Elements.ToList
                    Dim var As New StreamInformation
                    var.LoadData(xel.Elements.ToList)
                    _conn_ms.Add(xel.@ID, var)
                Next
            End If

            If _conn_es.Count = 0 Then
                For Each xel As XElement In (From xel2 As XElement In data Select xel2 Where xel2.Name = "EnergyStreams").SingleOrDefault.Elements.ToList
                    Dim var As New StreamInformation
                    var.LoadData(xel.Elements.ToList)
                    _conn_es.Add(xel.@ID, var)
                Next
            End If

            If Not m_specs Is Nothing Then m_specs.Clear()
            For Each xel As XElement In (From xel2 As XElement In data Select xel2 Where xel2.Name = "Specs").SingleOrDefault.Elements.ToList
                Dim var As New ColumnSpec
                var.LoadData(xel.Elements.ToList)
                m_specs.Add(xel.@ID, var)
            Next

            Dim ci As Globalization.CultureInfo = Globalization.CultureInfo.InvariantCulture

            Dim elm As XElement = (From xel2 As XElement In data Select xel2 Where xel2.Name = "Results").SingleOrDefault

            If Not elm Is Nothing Then

                compids = XMLSerializer.XMLSerializer.StringToArray(elm.Element("compids").Value, ci)

                T0 = elm.Element("T0").Value.ToDoubleArray(ci)
                Tf = elm.Element("Tf").Value.ToDoubleArray(ci)
                V0 = elm.Element("V0").Value.ToDoubleArray(ci)
                Vf = elm.Element("Vf").Value.ToDoubleArray(ci)
                L0 = elm.Element("L0").Value.ToDoubleArray(ci)
                Lf = elm.Element("Lf").Value.ToDoubleArray(ci)
                VSS0 = elm.Element("VSS0").Value.ToDoubleArray(ci)
                VSSf = elm.Element("VSSf").Value.ToDoubleArray(ci)
                LSS0 = elm.Element("LSS0").Value.ToDoubleArray(ci)
                LSSf = elm.Element("LSSf").Value.ToDoubleArray(ci)
                P0 = elm.Element("P0").Value.ToDoubleArray(ci)

                x0 = New ArrayList()
                For Each xel In elm.Element("x0").Elements
                    x0.Add(xel.Value.ToDoubleArray(ci))
                Next
                xf = New ArrayList()
                For Each xel In elm.Element("xf").Elements
                    xf.Add(xel.Value.ToDoubleArray(ci))
                Next
                y0 = New ArrayList()
                For Each xel In elm.Element("y0").Elements
                    y0.Add(xel.Value.ToDoubleArray(ci))
                Next
                yf = New ArrayList()
                For Each xel In elm.Element("yf").Elements
                    yf.Add(xel.Value.ToDoubleArray(ci))
                Next
                K0 = New ArrayList()
                For Each xel In elm.Element("K0").Elements
                    K0.Add(xel.Value.ToDoubleArray(ci))
                Next
                Kf = New ArrayList()
                For Each xel In elm.Element("Kf").Elements
                    Kf.Add(xel.Value.ToDoubleArray(ci))
                Next

            End If
            Return True
        End Function

        Public Overrides Function SaveData() As System.Collections.Generic.List(Of System.Xml.Linq.XElement)

            Dim elements As List(Of System.Xml.Linq.XElement) = MyBase.SaveData
            Dim ci As Globalization.CultureInfo = Globalization.CultureInfo.InvariantCulture

            With elements
                .Add(New XElement("Stages"))
                For Each st As Stage In Stages
                    .Item(.Count - 1).Add(New XElement("Stage", st.SaveData.ToArray))
                Next
                .Add(New XElement("MaterialStreams"))
                For Each kvp As KeyValuePair(Of String, StreamInformation) In _conn_ms
                    .Item(.Count - 1).Add(New XElement("MaterialStream", New XAttribute("ID", kvp.Key), kvp.Value.SaveData.ToArray))
                Next
                .Add(New XElement("EnergyStreams"))
                For Each kvp As KeyValuePair(Of String, StreamInformation) In _conn_es
                    .Item(.Count - 1).Add(New XElement("EnergyStream", New XAttribute("ID", kvp.Key), kvp.Value.SaveData.ToArray))
                Next
                .Add(New XElement("Specs"))
                For Each kvp As KeyValuePair(Of String, Auxiliary.SepOps.ColumnSpec) In m_specs
                    .Item(.Count - 1).Add(New XElement("Spec", New XAttribute("ID", kvp.Key), kvp.Value.SaveData.ToArray))
                Next

                .Add(New XElement("Results"))

                .Item(.Count - 1).Add(New XElement("compids", XMLSerializer.XMLSerializer.ArrayToString(compids, ci)))

                .Item(.Count - 1).Add(New XElement("T0", T0.ToArrayString(ci)))
                .Item(.Count - 1).Add(New XElement("Tf", Tf.ToArrayString(ci)))
                .Item(.Count - 1).Add(New XElement("V0", V0.ToArrayString(ci)))
                .Item(.Count - 1).Add(New XElement("Vf", Vf.ToArrayString(ci)))
                .Item(.Count - 1).Add(New XElement("L0", L0.ToArrayString(ci)))
                .Item(.Count - 1).Add(New XElement("Lf", Lf.ToArrayString(ci)))
                .Item(.Count - 1).Add(New XElement("VSS0", VSS0.ToArrayString(ci)))
                .Item(.Count - 1).Add(New XElement("VSSf", VSSf.ToArrayString(ci)))
                .Item(.Count - 1).Add(New XElement("LSS0", LSS0.ToArrayString(ci)))
                .Item(.Count - 1).Add(New XElement("LSSf", LSSf.ToArrayString(ci)))
                .Item(.Count - 1).Add(New XElement("P0", P0.ToArrayString(ci)))

                .Item(.Count - 1).Add(New XElement("x0"))
                For Each d As Double() In x0
                    .Item(.Count - 1).Element("x0").Add(New XElement("data", d.ToArrayString(ci)))
                Next
                .Item(.Count - 1).Add(New XElement("xf"))
                For Each d As Double() In xf
                    .Item(.Count - 1).Element("xf").Add(New XElement("data", d.ToArrayString(ci)))
                Next
                .Item(.Count - 1).Add(New XElement("y0"))
                For Each d As Double() In y0
                    .Item(.Count - 1).Element("y0").Add(New XElement("data", d.ToArrayString(ci)))
                Next
                .Item(.Count - 1).Add(New XElement("yf"))
                For Each d As Double() In yf
                    .Item(.Count - 1).Element("yf").Add(New XElement("data", d.ToArrayString(ci)))
                Next
                .Item(.Count - 1).Add(New XElement("K0"))
                For Each d As Double() In K0
                    .Item(.Count - 1).Element("K0").Add(New XElement("data", d.ToArrayString(ci)))
                Next
                .Item(.Count - 1).Add(New XElement("Kf"))
                For Each d As Double() In Kf
                    .Item(.Count - 1).Element("Kf").Add(New XElement("data", d.ToArrayString(ci)))
                Next

            End With

            Return elements

        End Function

        'constructor

        Public Sub New()
            MyBase.New()
        End Sub

        Public Sub New(ByVal name As String, ByVal description As String, fs As IFlowsheet)

            MyBase.CreateNew()

            SetFlowsheet(fs)

            ComponentName = name
            ComponentDescription = description

            _st = New System.Collections.Generic.List(Of Stage)

            _conn_ms = New System.Collections.Generic.Dictionary(Of String, StreamInformation)
            _conn_es = New System.Collections.Generic.Dictionary(Of String, StreamInformation)

            _ie = New InitialEstimates

            _cond = New Stage(Guid.NewGuid().ToString)
            _reb = New Stage(Guid.NewGuid().ToString)

        End Sub

        Public Function StreamExists(ByVal st As StreamInformation.Behavior)

            For Each si As StreamInformation In Me.MaterialStreams.Values
                If si.StreamBehavior = st Then
                    Return True
                End If
            Next

            Return False

        End Function

        Sub AddStages()

            Dim i As Integer
            For i = 0 To Me.NumberOfStages - 1
                _st.Add(New Stage(Guid.NewGuid().ToString))
                Select Case Me.ColumnType
                    Case ColType.DistillationColumn
                        If i = 0 Then
                            _st(_st.Count - 1).Name = FlowSheet.GetTranslatedString("DCCondenser")
                        ElseIf i = Me.NumberOfStages - 1 Then
                            _st(_st.Count - 1).Name = FlowSheet.GetTranslatedString("DCReboiler")
                        Else
                            _st(_st.Count - 1).Name = FlowSheet.GetTranslatedString("DCStage") & _st.Count - 1
                        End If
                    Case ColType.AbsorptionColumn
                        _st(_st.Count - 1).Name = FlowSheet.GetTranslatedString("DCStage") & _st.Count - 1
                    Case ColType.ReboiledAbsorber
                        If i = Me.NumberOfStages - 1 Then
                            _st(_st.Count - 1).Name = FlowSheet.GetTranslatedString("DCReboiler")
                        Else
                            _st(_st.Count - 1).Name = FlowSheet.GetTranslatedString("DCStage") & _st.Count - 1
                        End If
                    Case ColType.RefluxedAbsorber
                        If i = 0 Then
                            _st(_st.Count - 1).Name = FlowSheet.GetTranslatedString("DCCondenser")
                        Else
                            _st(_st.Count - 1).Name = FlowSheet.GetTranslatedString("DCStage") & _st.Count - 1
                        End If
                End Select
            Next

            RebuildEstimates()

        End Sub

        Public Sub RebuildEstimates()

            _ie = New InitialEstimates

            Dim i As Integer
            For i = 0 To Me.NumberOfStages - 1
                _ie.LiqMolarFlows.Add(New Parameter())
                _ie.VapMolarFlows.Add(New Parameter())
                _ie.StageTemps.Add(New Parameter())
                Dim d As New Dictionary(Of String, Parameter)
                For Each cp As BaseClasses.ConstantProperties In Me.FlowSheet.SelectedCompounds.Values
                    d.Add(cp.Name, New Parameter)
                Next
                _ie.LiqCompositions.Add(d)
                Dim d2 As New Dictionary(Of String, Parameter)
                For Each cp As BaseClasses.ConstantProperties In Me.FlowSheet.SelectedCompounds.Values
                    d2.Add(cp.Name, New Parameter)
                Next
                _ie.VapCompositions.Add(d2)
            Next

        End Sub

        Public Property ColumnType() As ColType
            Get
                Return _type
            End Get
            Set(ByVal value As ColType)
                _type = value
            End Set
        End Property

        Public Enum condtype
            Total_Condenser = 0
            Partial_Condenser = 1
            Full_Reflux = 2
        End Enum

        Public MustOverride Property SolvingMethod() As Integer

        Public ReadOnly Property MaterialStreams() As System.Collections.Generic.Dictionary(Of String, StreamInformation)
            Get
                Return _conn_ms
            End Get
        End Property

        Public ReadOnly Property EnergyStreams() As System.Collections.Generic.Dictionary(Of String, StreamInformation)
            Get
                Return _conn_es
            End Get
        End Property

        Public Property IO_ExtLoop_DeltaT() As Double
            Get
                Return _iodeltat_el
            End Get
            Set(ByVal value As Double)
                _iodeltat_el = value
            End Set
        End Property

        Public Property IO_DampingFactorMax() As Double
            Get
                Return _iodfmax
            End Get
            Set(ByVal value As Double)
                _iodfmax = value
            End Set
        End Property

        Public Property IO_DampingFactorMin() As Double
            Get
                Return _iodfmin
            End Get
            Set(ByVal value As Double)
                _iodfmin = value
            End Set
        End Property

        Public Property SC_MaxVarChgFac() As Integer
            Get
                Return _scmaxvarchgfac
            End Get
            Set(ByVal value As Integer)
                _scmaxvarchgfac = value
            End Set
        End Property

        Public Property IO_MaxVarChgFac() As Integer
            Get
                Return _iomaxvarchgfac
            End Get
            Set(ByVal value As Integer)
                _iomaxvarchgfac = value
            End Set
        End Property

        Public Property IO_NumericalDerivativeStep() As Double
            Get
                Return _ionderivstep
            End Get
            Set(ByVal value As Double)
                _ionderivstep = value
            End Set
        End Property

        Public Property SC_DampingFactor() As Double
            Get
                Return _scdf
            End Get
            Set(ByVal value As Double)
                _scdf = value
            End Set
        End Property

        Public Property SC_MaximumTemperatureChange() As Double
            Get
                Return _scmaxtc
            End Get
            Set(ByVal value As Double)
                _scmaxtc = value
            End Set
        End Property

        Public Property SC_NumericalDerivativeStep() As Double
            Get
                Return _scnderivstep
            End Get
            Set(ByVal value As Double)
                _scnderivstep = value
            End Set
        End Property

        Public Property UseNewtonUpdate() As Boolean
            Get
                Return _unu
            End Get
            Set(ByVal value As Boolean)
                _unu = value
            End Set
        End Property

        Public Property UseDampingFactor() As Boolean
            Get
                Return _udf
            End Get
            Set(ByVal value As Boolean)
                _udf = value
            End Set
        End Property

        Public Property UseIdentityAsJacobianInverse() As Boolean
            Get
                Return _uijac
            End Get
            Set(ByVal value As Boolean)
                _uijac = value
            End Set
        End Property

        Public Property StopAtIterationNumber() As Integer
            Get
                Return _stopatiter
            End Get
            Set(ByVal value As Integer)
                _stopatiter = value
            End Set
        End Property

        Public Property KbjWeightedAverage() As Boolean
            Get
                Return _kbjWA
            End Get
            Set(ByVal value As Boolean)
                _kbjWA = value
            End Set
        End Property

        Public Property AdjustSb() As Boolean
            Get
                Return _adjSb
            End Get
            Set(ByVal value As Boolean)
                _adjSb = value
            End Set
        End Property

        Public Property MaxIterations() As Integer
            Get
                Return _maxiterations
            End Get
            Set(ByVal value As Integer)
                _maxiterations = value
            End Set
        End Property

        Public Property InternalLoopTolerance() As Double
            Get
                Return _ilooptolerance
            End Get
            Set(ByVal value As Double)
                _ilooptolerance = value
            End Set
        End Property

        Public Property ExternalLoopTolerance() As Double
            Get
                Return _elooptolerance
            End Get
            Set(ByVal value As Double)
                _elooptolerance = value
            End Set
        End Property

        Public Property VaporFlowRateUnit() As String
            Get
                If _vrateunit Is Nothing And Not FlowSheet Is Nothing Then
                    _vrateunit = FlowSheet.FlowsheetOptions.SelectedUnitSystem.molarflow
                End If
                Return _vrateunit
            End Get
            Set(ByVal value As String)
                _vrateunit = value
            End Set
        End Property

        <Xml.Serialization.XmlIgnore()> Public Property JacobianMatrix() As Object
            Get
                Return m_jac
            End Get
            Set(ByVal value As Object)
                m_jac = value
            End Set
        End Property

        Public ReadOnly Property Specs() As Collections.Generic.Dictionary(Of String, Auxiliary.SepOps.ColumnSpec)
            Get
                If m_specs Is Nothing Then
                    m_specs = New Collections.Generic.Dictionary(Of String, Auxiliary.SepOps.ColumnSpec)
                End If
                If Not m_specs.ContainsKey("C") Then
                    m_specs.Add("C", New ColumnSpec)
                    With m_specs("C")
                        .SType = ColumnSpec.SpecType.Stream_Ratio
                        .SpecUnit = ""
                        .SpecValue = Me.RefluxRatio
                    End With
                End If
                If Not m_specs.ContainsKey("R") Then
                    m_specs.Add("R", New ColumnSpec)
                    With m_specs("R")
                        .SType = ColumnSpec.SpecType.Product_Molar_Flow_Rate
                        .SpecUnit = "mol/s"
                        .SpecValue = Me.DistillateFlowRate
                        .StageNumber = -1
                    End With
                End If
                Return m_specs
            End Get
        End Property

        Public Property VaporFlowRate() As Double
            Get
                Return _vrate
            End Get
            Set(ByVal value As Double)
                _vrate = value
            End Set
        End Property

        Public Property DistillateFlowRate() As Double
            Get
                Return _drate
            End Get
            Set(ByVal value As Double)
                _drate = value
            End Set
        End Property

        Public Property ReboilerPressure() As Double
            Get
                Return _rebp
            End Get
            Set(ByVal value As Double)
                _rebp = value
            End Set
        End Property

        Public Property CondenserPressure() As Double
            Get
                Return _condp
            End Get
            Set(ByVal value As Double)
                _condp = value
            End Set
        End Property

        Public Property CondenserDeltaP() As Double
            Get
                Return _conddp
            End Get
            Set(ByVal value As Double)
                _conddp = value
            End Set
        End Property

        Public Property ReboilerDuty() As Double
            Get
                Return _rebd
            End Get
            Set(ByVal value As Double)
                _rebd = value
            End Set
        End Property

        Public Property CondenserDuty() As Double
            Get
                Return _condd
            End Get
            Set(ByVal value As Double)
                _condd = value
            End Set
        End Property

        Public Property RefluxRatio() As Double
            Get
                Return _rr
            End Get
            Set(ByVal value As Double)
                _rr = value
            End Set
        End Property

        Public Property NumberOfStages() As Integer
            Get
                Return _nst
            End Get
            Set(ByVal value As Integer)
                _nst = value
            End Set
        End Property

        Public ReadOnly Property Stages() As System.Collections.Generic.List(Of Auxiliary.SepOps.Stage)
            Get
                Return _st
            End Get
        End Property

        Public Function StageIndex(ByVal name As String) As Integer
            Dim i As Integer = 0
            For Each st As Stage In Me.Stages
                If st.ID = name Or st.Name = name Then Return i
                i = i + 1
            Next
            Return i
        End Function

        Public Property StoreAndReuseJacobian() As Boolean
            Get
                Return _storejac
            End Get
            Set(ByVal value As Boolean)
                _storejac = value
            End Set
        End Property

        Public Property AutoUpdateInitialEstimates() As Boolean
            Get
                Return _autoupdie
            End Get
            Set(ByVal value As Boolean)
                _autoupdie = value
            End Set
        End Property

        Public Property UseTemperatureEstimates() As Boolean
            Get
                Return _use_ie
            End Get
            Set(ByVal value As Boolean)
                _use_ie = value
            End Set
        End Property

        Public Property UseVaporFlowEstimates() As Boolean
            Get
                Return _use_ie1
            End Get
            Set(ByVal value As Boolean)
                _use_ie1 = value
            End Set
        End Property

        Public Property UseLiquidFlowEstimates() As Boolean
            Get
                Return _use_ie3
            End Get
            Set(ByVal value As Boolean)
                _use_ie3 = value
            End Set
        End Property

        Public Property UseCompositionEstimates() As Boolean
            Get
                Return _use_ie2
            End Get
            Set(ByVal value As Boolean)
                _use_ie2 = value
            End Set
        End Property

        Public Property InitialEstimates() As InitialEstimates
            Get
                Return _ie
            End Get
            Set(ByVal value As InitialEstimates)
                _ie = value
            End Set
        End Property

        Public Sub CheckConnPos()
            Dim idx As Integer
            For Each strinfo As StreamInformation In Me.MaterialStreams.Values
                Try
                    Select Case strinfo.StreamBehavior
                        Case StreamInformation.Behavior.Feed
                            idx = FlowSheet.GraphicObjects(strinfo.StreamID).OutputConnectors(0).AttachedConnector.AttachedToConnectorIndex
                            If Me.GraphicObject.FlippedH Then
                                Me.GraphicObject.InputConnectors(idx).Position = New Point.Point(Me.GraphicObject.X + Me.GraphicObject.Width, Me.GraphicObject.Y + Me.StageIndex(strinfo.AssociatedStage) / Me.NumberOfStages * Me.GraphicObject.Height)
                            Else
                                Me.GraphicObject.InputConnectors(idx).Position = New Point.Point(Me.GraphicObject.X, Me.GraphicObject.Y + Me.StageIndex(strinfo.AssociatedStage) / Me.NumberOfStages * Me.GraphicObject.Height)
                            End If
                        Case StreamInformation.Behavior.Distillate
                            idx = FlowSheet.GraphicObjects(strinfo.StreamID).InputConnectors(0).AttachedConnector.AttachedFromConnectorIndex
                            If Not Me.GraphicObject.FlippedH Then
                                Me.GraphicObject.OutputConnectors(idx).Position = New Point.Point(Me.GraphicObject.X + Me.GraphicObject.Width, Me.GraphicObject.Y + 0.3 * Me.GraphicObject.Height)
                            Else
                                Me.GraphicObject.OutputConnectors(idx).Position = New Point.Point(Me.GraphicObject.X, Me.GraphicObject.Y + 0.3 * Me.GraphicObject.Height)
                            End If
                        Case StreamInformation.Behavior.BottomsLiquid
                            idx = FlowSheet.GraphicObjects(strinfo.StreamID).InputConnectors(0).AttachedConnector.AttachedFromConnectorIndex
                            If Not Me.GraphicObject.FlippedH Then
                                Me.GraphicObject.OutputConnectors(idx).Position = New Point.Point(Me.GraphicObject.X + Me.GraphicObject.Width, Me.GraphicObject.Y + 0.98 * Me.GraphicObject.Height)
                            Else
                                Me.GraphicObject.OutputConnectors(idx).Position = New Point.Point(Me.GraphicObject.X, Me.GraphicObject.Y + 0.98 * Me.GraphicObject.Height)
                            End If
                        Case StreamInformation.Behavior.OverheadVapor
                            idx = FlowSheet.GraphicObjects(strinfo.StreamID).InputConnectors(0).AttachedConnector.AttachedFromConnectorIndex
                            If Not Me.GraphicObject.FlippedH Then
                                Me.GraphicObject.OutputConnectors(idx).Position = New Point.Point(Me.GraphicObject.X + Me.GraphicObject.Width, Me.GraphicObject.Y + 0.02 * Me.GraphicObject.Height)
                            Else
                                Me.GraphicObject.OutputConnectors(idx).Position = New Point.Point(Me.GraphicObject.X, Me.GraphicObject.Y + 0.02 * Me.GraphicObject.Height)
                            End If
                        Case StreamInformation.Behavior.Sidedraw
                            idx = FlowSheet.GraphicObjects(strinfo.StreamID).InputConnectors(0).AttachedConnector.AttachedFromConnectorIndex
                            If Me.GraphicObject.FlippedH Then
                                Me.GraphicObject.OutputConnectors(idx).Position = New Point.Point(Me.GraphicObject.X, Me.GraphicObject.Y + Me.StageIndex(strinfo.AssociatedStage) / Me.NumberOfStages * Me.GraphicObject.Height)
                            Else
                                Me.GraphicObject.OutputConnectors(idx).Position = New Point.Point(Me.GraphicObject.X + Me.GraphicObject.Width, Me.GraphicObject.Y + Me.StageIndex(strinfo.AssociatedStage) / Me.NumberOfStages * Me.GraphicObject.Height)
                            End If
                    End Select
                Catch ex As Exception
                End Try
            Next

            For Each strinfo As StreamInformation In Me.EnergyStreams.Values
                Try
                    idx = FlowSheet.GraphicObjects(strinfo.StreamID).InputConnectors(0).AttachedConnector.AttachedFromConnectorIndex
                    Select Case strinfo.StreamBehavior
                        Case StreamInformation.Behavior.Distillate
                            If Me.GraphicObject.FlippedH Then
                                Me.GraphicObject.OutputConnectors(idx).Position = New Point.Point(Me.GraphicObject.X, Me.GraphicObject.Y + 0.08 * Me.GraphicObject.Height)
                            Else
                                Me.GraphicObject.OutputConnectors(idx).Position = New Point.Point(Me.GraphicObject.X + Me.GraphicObject.Width, Me.GraphicObject.Y + 0.08 * Me.GraphicObject.Height)
                            End If
                        Case StreamInformation.Behavior.BottomsLiquid
                            If Me.GraphicObject.FlippedH Then
                                Me.GraphicObject.OutputConnectors(idx).Position = New Point.Point(Me.GraphicObject.X, Me.GraphicObject.Y + 0.825 * Me.GraphicObject.Height)
                            Else
                                Me.GraphicObject.OutputConnectors(idx).Position = New Point.Point(Me.GraphicObject.X + Me.GraphicObject.Width, Me.GraphicObject.Y + 0.825 * Me.GraphicObject.Height)
                            End If
                        Case StreamInformation.Behavior.InterExchanger
                            If Me.GraphicObject.FlippedH Then
                                Me.GraphicObject.OutputConnectors(idx).Position = New Point.Point(Me.GraphicObject.X, Me.GraphicObject.Y + Me.StageIndex(strinfo.AssociatedStage) / Me.NumberOfStages * Me.GraphicObject.Height)
                            Else
                                Me.GraphicObject.OutputConnectors(idx).Position = New Point.Point(Me.GraphicObject.X + Me.GraphicObject.Width, Me.GraphicObject.Y + Me.StageIndex(strinfo.AssociatedStage) / Me.NumberOfStages * Me.GraphicObject.Height)
                            End If
                    End Select
                Catch ex As Exception
                End Try
            Next

            Dim i As Integer = 0
            Dim obj1(Me.GraphicObject.InputConnectors.Count), obj2(Me.GraphicObject.InputConnectors.Count) As Double
            Dim obj3(Me.GraphicObject.OutputConnectors.Count), obj4(Me.GraphicObject.OutputConnectors.Count) As Double
            For Each ic As IConnectionPoint In Me.GraphicObject.InputConnectors
                obj1(i) = -Me.GraphicObject.X + ic.Position.X
                obj2(i) = -Me.GraphicObject.Y + ic.Position.Y
                i = i + 1
            Next
            i = 0
            For Each oc As IConnectionPoint In Me.GraphicObject.OutputConnectors
                obj3(i) = -Me.GraphicObject.X + oc.Position.X
                obj4(i) = -Me.GraphicObject.Y + oc.Position.Y
                i = i + 1
            Next
            Me.GraphicObject.AdditionalInfo = New Object() {obj1, obj2, obj3, obj4}

        End Sub

        Public T0 As Double() = New Double() {}
        Public Tf As Double() = New Double() {}
        Public V0 As Double() = New Double() {}
        Public Vf As Double() = New Double() {}
        Public L0 As Double() = New Double() {}
        Public Lf As Double() = New Double() {}
        Public LSS0 As Double() = New Double() {}
        Public LSSf As Double() = New Double() {}
        Public VSS0 As Double() = New Double() {}
        Public VSSf As Double() = New Double() {}
        Public P0 As Double() = New Double() {}
        Public x0, xf, y0, yf, K0, Kf As New ArrayList
        Public ic, ec As Integer
        Public compids As New ArrayList

        Public Overrides Sub Calculate(Optional ByVal args As Object = Nothing)

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

            Inspector.Host.CheckAndAdd(IObj, "", "Calculate", If(GraphicObject IsNot Nothing, GraphicObject.Tag, "Temporary Object") & " (" & GetDisplayName() & ")", GetDisplayName() & " Calculation Routine", True)

            IObj?.SetCurrent()

            IObj?.Paragraphs.Add("For any stage in a countercurrent cascade, assume (1) phase equilibrium is achieved at each stage, (2) no chemical reactions occur, and (3) entrainment of liquid drops in vapor and occlusion of vapor bubbles in liquid are negligible. Figure 1 represents such a stage for the vapor–liquid case, where the stages are numbered down from the top. The same representation applies to liquid–liquid extraction if the higher-density liquid phases are represented by liquid streams and the lower-density liquid phases are represented by vapor streams.")

            IObj?.Paragraphs.Add(InspectorItem.GetImageHTML("image1.jpg"))

            IObj?.Paragraphs.Add("Entering stage j is a single- or two-phase feed of molar flow rate Fj, with overall composition in mole fractions zi,j of component i, temperature TFj , pressure PFj , and corresponding overall molar enthalpy hFj.")

            IObj?.Paragraphs.Add("Also entering stage j is interstage liquid from stage j-1 above, if any, of molar flow rate Lj-1, with composition in mole fractions xij-1, enthalpy hLj-1, temperature Tj-1, and pressure Pj-1, which is equal to or less than the pressure of stage j. Pressure of liquid from stage j-1 is increased adiabatically by hydrostatic head change across head L.")

            IObj?.Paragraphs.Add("Similarly, from stage j+1 below, interstage vapor of molar flow rate V+1, with composition in mole fractions yij+1, enthalpy hV+1, temperature Tj+1, and pressure Pj+1 enters stage j.")

            IObj?.Paragraphs.Add("Leaving stage j is vapor of intensive properties yij, hVj, Tj, and Pj. This stream can be divided into a vapor sidestream of molar flow rate Wj and an interstage stream of molar flow rate Vj to be sent to stage j-1 or, if j=1, to leave as a product. Also leaving stage j is liquid of intensive properties xij, hLj, Tj, and Pj, in equilibrium with vapor (Vj+Wj). This liquid is divided into a sidestream of molar flow rate Uj and an interstage stream of molar flow rate Lj to be sent to stage j+1 or, if j=N, to leave as a product.")

            IObj?.Paragraphs.Add("Associated with each general stage are the following indexed equations expressed in terms of the variable set in Figure 1. However, variables other than those shown in Figure 1 can be used, e.g. component flow rates can replace mole fractions, and sidestream flow rates can be expressed as fractions of interstage flow rates. The equations are referred to as MESH equations, after Wang and Henke.")

            IObj?.Paragraphs.Add("M equations—Material balance for each component (C equations for each stage).")

            IObj?.Paragraphs.Add("<m>M_{i,j}=L_{j-1}x_{i,j-1}+V_{j+1}y_{i,j+1}+F_jz_{i,j}-(L_j+U_j)x_{i,j}-(V_j+W_j)y_{i,j}</m>")

            IObj?.Paragraphs.Add("E equations—phase-Equilibrium relation for each component (C equations for each stage),")

            IObj?.Paragraphs.Add("<m>E_{i,j}=y_{i,j}-K_{i,j}x_{i,j}=0</m>")

            IObj?.Paragraphs.Add("where <mi>K_{i,j}</mi> is the phase-equilibrium ratio or K-value.")

            IObj?.Paragraphs.Add("S equations—mole-fraction Summations (one for each stage),")

            IObj?.Paragraphs.Add("<m>(S_y)_j=\sum\limits_{i=1}^{C}{y_{i,j}}-1=0</m>")

            IObj?.Paragraphs.Add("<m>(S_x)_j=\sum\limits_{i=1}^{C}{x_{i,j}} -1=0</m>")

            IObj?.Paragraphs.Add("H equation—energy balance (one for each stage).")

            IObj?.Paragraphs.Add("<m>H_j=L_{j-1}h_{L_{j-1}}+V_{j+1}h_{V_{j+1}}+F_jh_{F_j}-(L_j+U_j)h_{L_j}-(V_j+W_j)h_{V_j}-Q_j=0</m>")

            IObj?.Paragraphs.Add("A countercurrent cascade of N such stages is represented by N(2C+3) such equations in [N(3C+10)+1] variables. If N and all Fj, zij, TFj, PFj, Pj, Uj, Wj, and Qj are specified, the model is represented by N(2C+3) simultaneous algebraic equations in N(2C+3) unknown (output) variables comprising all xij, yij, Lj, Vj, and Tj, where the M, E, and H equations are nonlinear. If other variables are specified, corresponding substitutions are made to the list of output variables. Regardless, the result is a set containing nonlinear equations that must be solved by an iterative technique.")

            IObj?.Paragraphs.Add("<h2>Initial Estimates</h2>")

            IObj?.Paragraphs.Add("DWSIM will calculate new or use existing initial estimates and forward the values to the selected solver.")

            'Validate unitop status.
            Me.Validate()

            'Check connectors' positions
            Me.CheckConnPos()

            'prepare variables

            Dim llextractor As Boolean = False
            Dim myabs As AbsorptionColumn = TryCast(Me, AbsorptionColumn)
            If myabs IsNot Nothing Then
                If CType(Me, AbsorptionColumn).OperationMode = AbsorptionColumn.OpMode.Absorber Then
                    llextractor = False
                Else
                    llextractor = True
                End If
            End If

            Dim pp As PropertyPackages.PropertyPackage = Me.PropertyPackage

            Dim nc, ns, maxits, i, j As Integer
            Dim firstF As Integer = -1
            Dim lastF As Integer = -1
            nc = Me.FlowSheet.SelectedCompounds.Count
            ns = Me.Stages.Count - 1
            maxits = Me.MaxIterations

            Dim tol(4) As Double
            tol(0) = Me.InternalLoopTolerance
            tol(1) = Me.ExternalLoopTolerance

            Dim F(ns), Q(ns), V(ns), L(ns), VSS(ns), LSS(ns), HF(ns), T(ns), FT(ns), P(ns), fracv(ns), eff(ns),
                distrate, rr, vaprate As Double

            Dim x(ns)() As Double, y(ns)() As Double, z(ns)() As Double, fc(ns)() As Double
            Dim idealK(ns)(), Kval(ns)(), Pvap(ns)() As Double

            For i = 0 To ns
                Array.Resize(x(i), nc)
                Array.Resize(y(i), nc)
                Array.Resize(fc(i), nc)
                Array.Resize(z(i), nc)
                Array.Resize(idealK(i), nc)
                Array.Resize(Kval(i), nc)
                Array.Resize(Pvap(i), nc)
            Next

            Dim sumcf(nc - 1), sumF, zm(nc - 1) As Double

            IObj?.Paragraphs.Add("Collecting data from connected streams...")

            i = 0

            Dim stream As MaterialStream = Nothing

            For Each ms As StreamInformation In Me.MaterialStreams.Values
                Select Case ms.StreamBehavior
                    Case StreamInformation.Behavior.Feed
                        stream = FlowSheet.SimulationObjects(ms.StreamID)
                        pp.CurrentMaterialStream = stream
                        F(StageIndex(ms.AssociatedStage)) = stream.Phases(0).Properties.molarflow.GetValueOrDefault
                        HF(StageIndex(ms.AssociatedStage)) = stream.Phases(0).Properties.enthalpy.GetValueOrDefault *
                                                                stream.Phases(0).Properties.molecularWeight.GetValueOrDefault
                        FT(StageIndex(ms.AssociatedStage)) = stream.Phases(0).Properties.temperature.GetValueOrDefault
                        sumF += F(StageIndex(ms.AssociatedStage))
                        j = 0
                        For Each comp As Thermodynamics.BaseClasses.Compound In stream.Phases(0).Compounds.Values
                            fc(StageIndex(ms.AssociatedStage))(j) = comp.MoleFraction.GetValueOrDefault
                            z(StageIndex(ms.AssociatedStage))(j) = comp.MoleFraction.GetValueOrDefault
                            sumcf(j) += comp.MoleFraction.GetValueOrDefault * F(StageIndex(ms.AssociatedStage))
                            j = j + 1
                        Next
                        If firstF = -1 Then firstF = StageIndex(ms.AssociatedStage)
                    Case StreamInformation.Behavior.Sidedraw
                        If ms.StreamPhase = StreamInformation.Phase.L Then
                            LSS(StageIndex(ms.AssociatedStage)) = ms.FlowRate.Value
                        Else
                            VSS(StageIndex(ms.AssociatedStage)) = ms.FlowRate.Value
                        End If
                    Case StreamInformation.Behavior.InterExchanger
                        Q(StageIndex(ms.AssociatedStage)) = -DirectCast(FlowSheet.SimulationObjects(ms.StreamID), Streams.EnergyStream).EnergyFlow.GetValueOrDefault
                End Select
                i += 1
            Next

            For Each ms As StreamInformation In Me.EnergyStreams.Values
                Select Case ms.StreamBehavior
                    Case StreamInformation.Behavior.InterExchanger
                        Q(StageIndex(ms.AssociatedStage)) = -DirectCast(FlowSheet.SimulationObjects(ms.StreamID), Streams.EnergyStream).EnergyFlow.GetValueOrDefault
                End Select
                i += 1
            Next

            Dim cv As New SystemsOfUnits.Converter

            vaprate = SystemsOfUnits.Converter.ConvertToSI(Me.VaporFlowRateUnit, Me.VaporFlowRate)

            Dim sum1(ns), sum0_ As Double
            sum0_ = 0
            For i = 0 To ns
                sum1(i) = 0
                For j = 0 To i
                    sum1(i) += F(j) - LSS(j) - VSS(j)
                Next
                sum0_ += LSS(i) + VSS(i)
            Next

            If Me.Specs("C").SType = ColumnSpec.SpecType.Stream_Ratio Then
                rr = Me.Specs("C").SpecValue
            Else
                rr = 2.5
            End If
            If Me.Specs("R").SType = ColumnSpec.SpecType.Product_Molar_Flow_Rate Then
                If Me.CondenserType = condtype.Full_Reflux Then
                    vaprate = sumF - SystemsOfUnits.Converter.ConvertToSI(Me.Specs("R").SpecUnit, Me.Specs("R").SpecValue) - sum0_
                    distrate = 0.0
                ElseIf Me.CondenserType = condtype.Partial_Condenser Then
                    If Me.Specs("C").SType = ColumnSpec.SpecType.Product_Molar_Flow_Rate Then
                        distrate = SystemsOfUnits.Converter.ConvertToSI(Me.Specs("C").SpecUnit, Me.Specs("C").SpecValue)
                    Else
                        distrate = sumF - SystemsOfUnits.Converter.ConvertToSI(Me.Specs("R").SpecUnit, Me.Specs("R").SpecValue) - sum0_ - vaprate
                    End If

                Else
                    distrate = sumF - SystemsOfUnits.Converter.ConvertToSI(Me.Specs("R").SpecUnit, Me.Specs("R").SpecValue) - sum0_
                    vaprate = 0.0
                End If
            Else
                If Me.CondenserType = condtype.Full_Reflux Then
                    vaprate = sumF / 2 - sum0_
                Else
                    distrate = sumF / 2 - sum0_ - vaprate
                End If
            End If

            IObj?.Paragraphs.Add(String.Format("Estimated/Specified Distillate Rate: {0} mol/s", distrate))
            IObj?.Paragraphs.Add(String.Format("Estimated/Specified Vapor Overflow Rate: {0} mol/s", vaprate))
            IObj?.Paragraphs.Add(String.Format("Estimated/Specified Reflux Ratio: {0}", rr))

            compids = New ArrayList
            compids.Clear()
            For Each comp As Thermodynamics.BaseClasses.Compound In stream.Phases(0).Compounds.Values
                compids.Add(comp.Name)
            Next

            For i = ns To 0 Step -1
                If F(i) <> 0 Then
                    lastF = i
                    Exit For
                End If
            Next

            For i = 0 To nc - 1
                zm(i) = sumcf(i) / sumF
            Next

            Dim T1, T2 As Double

            Select Case Me.ColumnType
                Case ColType.DistillationColumn
                    LSS(0) = distrate
                Case ColType.RefluxedAbsorber
                    LSS(0) = distrate
            End Select

            P(ns) = Me.ReboilerPressure
            P(0) = Me.CondenserPressure

            Select Case Me.ColumnType
                Case ColType.AbsorptionColumn
                    T1 = FT.First
                    T2 = FT.Last
                Case ColType.ReboiledAbsorber
                    T1 = MathEx.Common.WgtAvg(F, FT)
                    T2 = T1
                Case ColType.RefluxedAbsorber
                    T1 = MathEx.Common.WgtAvg(F, FT)
                    T2 = T1
                Case ColType.DistillationColumn
                    Try
                        IObj?.SetCurrent()
                        T1 = pp.DW_CalcBubT(zm, P(0), MathEx.Common.Min(FT))(4) '* 1.01
                    Catch ex As Exception
                        T1 = MathEx.Common.Min(FT)
                    End Try
                    Try
                        IObj?.SetCurrent()
                        T2 = pp.DW_CalcDewT(zm, P(ns), MathEx.Common.Max(FT))(4) '* 0.99
                    Catch ex As Exception
                        T2 = MathEx.Common.Max(FT)
                    End Try
            End Select

            For i = 0 To ns
                sum1(i) = 0
                For j = 0 To i
                    sum1(i) += F(j) - LSS(j) - VSS(j)
                Next
            Next

            pp.CurrentMaterialStream = pp.CurrentMaterialStream.Clone()
            pp.CurrentMaterialStream.SetPropertyPackageObject(pp)
            DirectCast(pp.CurrentMaterialStream, MaterialStream).SetFlowsheet(FlowSheet)
            DirectCast(pp.CurrentMaterialStream, MaterialStream).PreferredFlashAlgorithmTag = Me.PreferredFlashAlgorithmTag

            T(0) = T1
            T(ns) = T2

            i = 0
            For Each st As Stage In Me.Stages
                P(i) = st.P
                eff(i) = st.Efficiency
                If Me.UseTemperatureEstimates Then
                    T(i) = Me.InitialEstimates.StageTemps(i).Value
                Else
                    T(i) = (T2 - T1) * (i) / ns + T1
                End If
                If Me.UseVaporFlowEstimates Then
                    V(i) = Me.InitialEstimates.VapMolarFlows(i).Value
                Else
                    If i = 0 Then
                        Select Case Me.ColumnType
                            Case ColType.DistillationColumn
                                If Me.CondenserType = condtype.Total_Condenser Then
                                    V(0) = 0.0005
                                Else
                                    V(0) = vaprate
                                End If
                            Case ColType.RefluxedAbsorber
                                If Me.CondenserType = condtype.Total_Condenser Then
                                    V(0) = 0.0005
                                Else
                                    V(0) = vaprate
                                End If
                            Case Else
                                V(0) = F(lastF)
                        End Select
                    Else
                        Select Case Me.ColumnType
                            Case ColType.DistillationColumn
                                If Me.CondenserType = condtype.Partial_Condenser Then
                                    V(i) = (rr + 1) * (distrate + vaprate) - F(0)
                                ElseIf Me.CondenserType = condtype.Full_Reflux Then
                                    V(i) = (rr + 1) * V(0) - F(0)
                                Else
                                    V(i) = (rr + 1) * distrate - F(0)
                                End If
                            Case ColType.RefluxedAbsorber
                                V(i) = (rr + 1) * distrate - F(0) + V(0)
                            Case ColType.AbsorptionColumn
                                V(i) = F(lastF)
                            Case ColType.ReboiledAbsorber
                                V(i) = F(lastF)
                        End Select
                    End If
                End If
                If Me.UseLiquidFlowEstimates Then
                    L(i) = Me.InitialEstimates.LiqMolarFlows(i).Value
                Else
                    If i = 0 Then
                        Select Case Me.ColumnType
                            Case ColType.DistillationColumn
                                If Me.CondenserType = condtype.Partial_Condenser Then
                                    L(0) = (distrate + vaprate) * rr
                                ElseIf Me.CondenserType = condtype.Full_Reflux Then
                                    L(0) = vaprate * rr
                                Else
                                    L(0) = distrate * rr
                                End If
                            Case ColType.RefluxedAbsorber
                                If Me.CondenserType = condtype.Partial_Condenser Then
                                    L(0) = distrate * rr
                                ElseIf Me.CondenserType = condtype.Full_Reflux Then
                                Else
                                    L(0) = distrate * rr
                                End If
                            Case Else
                                L(0) = F(firstF)
                                If L(0) = 0 Then L(i) = 0.00001
                        End Select
                    Else
                        Select Case Me.ColumnType
                            Case ColType.DistillationColumn
                                If i < ns Then L(i) = V(i) + sum1(i) - V(0) Else L(i) = sum1(i) - V(0)
                            Case ColType.RefluxedAbsorber
                                If i < ns Then L(i) = V(i) + sum1(i) - V(0) Else L(i) = sum1(i) - V(0)
                            Case ColType.AbsorptionColumn
                                L(i) = F(firstF)
                            Case ColType.ReboiledAbsorber
                                L(i) = F(firstF)
                        End Select
                        If L(i) = 0 Then L(i) = 0.00001
                    End If
                End If
                If Me.UseCompositionEstimates Then
                    j = 0
                    For Each par As Parameter In Me.InitialEstimates.LiqCompositions(i).Values
                        x(i)(j) = par.Value
                        j = j + 1
                    Next
                    j = 0
                    For Each par As Parameter In Me.InitialEstimates.VapCompositions(i).Values
                        y(i)(j) = par.Value
                        j = j + 1
                    Next
                    For j = 0 To nc - 1
                        Kval(i)(j) = y(i)(j) / x(i)(j)
                        z(i)(j) = zm(j)
                    Next
                Else
                    IObj?.SetCurrent()
                    Dim flashresult As Object = Nothing
                    If llextractor Then
                        Dim L1, L2, Vx1(), Vx2(), rho1, rho2 As Double
                        Dim trialcomp As Double() = zm.Clone
                        For counter As Integer = 0 To 20
                            flashresult = pp.FlashBase.Flash_PT(trialcomp, P(i), T(i), pp)
                            L1 = flashresult(0)
                            L2 = flashresult(5)
                            If L2 > 0.0 Then
                                Exit For
                            Else
                                Dim rnd As New Random(counter)
                                trialcomp = Enumerable.Repeat(0, nc).Select(Function(d) rnd.NextDouble()).ToArray
                                trialcomp = trialcomp.NormalizeY
                            End If
                        Next
                        If L2 = 0.0 Then
                            'try simple lle
                            trialcomp  = zm.Clone
                            Dim slle As New PropertyPackages.Auxiliary.FlashAlgorithms.SimpleLLE()
                            For counter As Integer = 0 To 20
                                flashresult = slle.Flash_PT(trialcomp, P(i), T(i), pp)
                                L1 = flashresult(0)
                                L2 = flashresult(5)
                                If L2 > 0.0 Then
                                    Exit For
                                Else
                                    Dim rnd As New Random(counter)
                                    trialcomp = Enumerable.Repeat(0, nc).Select(Function(d) rnd.NextDouble()).ToArray
                                    trialcomp = trialcomp.NormalizeY
                                End If
                            Next
                            If L2 = 0.0 Then
                                Throw New Exception("Your column is configured as a Liquid-Liquid Extractor, but the Property Package / Flash Algorithm set associated with the column is unable to generate an initial estimate for two liquid phases. Please select a different set or change the Flash Algorithm's Stability Analysis parameters and try again.")
                            End If
                        End If
                        Vx1 = flashresult(2)
                        Vx2 = flashresult(6)
                        x(i) = Vx1
                        y(i) = Vx2
                        If y(i).SumY = 0.0# Then
                            y(i) = x(i).Clone
                        End If
                        If Not Me.UseVaporFlowEstimates Then
                            V(i) = F(lastF) + F(firstF) * L1
                        End If
                        If Not Me.UseLiquidFlowEstimates Then
                            L(i) = F(firstF) * (1 - L1)
                        End If
                    Else
                        flashresult = pp.FlashBase.Flash_PT(zm, P(i), T(i), pp)
                        x(i) = flashresult(2)
                        y(i) = flashresult(3)
                    End If
                    z(i) = zm
                    For j = 0 To nc - 1
                        Kval(i)(j) = y(i)(j) / x(i)(j)
                    Next
                    If llextractor And pp.AUX_CheckTrivial(Kval(i)) Then
                        Throw New Exception("Your column is configured as a Liquid-Liquid Extractor, but the Property Package / Flash Algorithm set associated with the column is unable to generate an initial estimate for two liquid phases. Please select a different set or change the Flash Algorithm's Stability Analysis parameters and try again.")
                    End If
                End If
                i = i + 1
            Next
            Select Case Me.ColumnType
                Case ColType.DistillationColumn
                    Q(0) = 0
                    Q(ns) = 0
                Case ColType.ReboiledAbsorber
                    Q(ns) = 0
                Case ColType.RefluxedAbsorber
                    Q(0) = 0
            End Select

            IObj?.Paragraphs.Add(String.Format("Estimated/Specified Temperature Profile: {0}", T.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Estimated/Specified Interstage Liquid Flow Rate: {0} mol/s", L.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Estimated/Specified Interstage Vapor/Liquid2 Flow Rate: {0} mol/s", V.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Estimated/Specified Liquid Side Draw Rate: {0} mol/s", LSS.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Estimated/Specified Vapor/Liquid2 Side Draw Rate: {0} mol/s", VSS.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Estimated/Specified Heat Added/Removed Profile: {0} kW", Q.ToMathArrayString))

            'store initial values

            x0.Clear()
            y0.Clear()
            K0.Clear()
            For i = 0 To ns
                x0.Add(x(i))
                y0.Add(y(i))
                K0.Add(Kval(i))
            Next
            T0 = T
            P0 = P
            V0 = V
            L0 = L
            VSS0 = VSS
            LSS0 = LSS

            IObj?.Paragraphs.Add("<h2>Column Specifications</h2>")

            IObj?.Paragraphs.Add("Processing Specs...")

            'process specifications
            For Each sp As Auxiliary.SepOps.ColumnSpec In Me.Specs.Values
                If sp.SType = ColumnSpec.SpecType.Component_Fraction Or
                sp.SType = ColumnSpec.SpecType.Component_Mass_Flow_Rate Or
                sp.SType = ColumnSpec.SpecType.Component_Molar_Flow_Rate Or
                sp.SType = ColumnSpec.SpecType.Component_Recovery Then
                    i = 0
                    For Each comp As BaseClasses.Compound In stream.Phases(0).Compounds.Values
                        If sp.ComponentID = comp.Name Then sp.ComponentIndex = i
                        i = i + 1
                    Next
                End If
                If sp.StageNumber = -1 And sp.SpecValue = Me.DistillateFlowRate Then
                    sumF = 0
                    Dim sumLSS As Double = 0
                    Dim sumVSS As Double = 0
                    For i = 0 To ns
                        sumF += F(i)
                        sumLSS += LSS(i)
                        sumVSS += VSS(i)
                    Next
                    sp.SpecValue = sumF - sumLSS - sumVSS - V(0)
                    sp.StageNumber = 0
                End If
                IObj?.Paragraphs.Add(String.Format("Spec Type: {0}", [Enum].GetName(sp.SType.GetType, sp.SType)))
                IObj?.Paragraphs.Add(String.Format("Spec Value: {0}", sp.SpecValue))
                IObj?.Paragraphs.Add(String.Format("Spec Stage: {0}", sp.StageNumber))
                IObj?.Paragraphs.Add(String.Format("Spec Units: {0}", sp.SpecUnit))
                IObj?.Paragraphs.Add(String.Format("Compound (if applicable): {0}", sp.ComponentID))
            Next

            Dim idealk0, idealh0 As Boolean

            Select Case SolverScheme
                Case SolvingScheme.Direct
                    idealk0 = False
                    idealh0 = False
                Case SolvingScheme.Ideal_K_Init
                    idealk0 = True
                    idealh0 = False
                Case SolvingScheme.Ideal_Enthalpy_Init
                    idealk0 = False
                    idealh0 = True
                Case SolvingScheme.Ideal_K_and_Enthalpy_Init
                    idealk0 = True
                    idealh0 = True
            End Select


            IObj?.Paragraphs.Add("<h2>Rigorous Solver</h2>")

            Select Case Me.SolvingMethod
                Case 0 'BP
                    IObj?.Paragraphs.Add("Selected Solver: Wang-Henke Bubble-Point (BP)")
                Case 1 'SC
                    IObj?.Paragraphs.Add("Selected Solver: Naphtali-Sandholm Simultaneous Correction (SC)")
                Case 2 'IO 
                    IObj?.Paragraphs.Add("Selected Solver: Russell Inside-Out (IO)")
                Case 3 'SR
                    IObj?.Paragraphs.Add("Selected Solver: Burningham-Otto Sum-Rates (SR)")
            End Select

            IObj?.Paragraphs.Add(String.Format("Solving Scheme: {0}", [Enum].GetName(SolverScheme.GetType, SolverScheme)))

            If idealk0 Or idealh0 Then
                IObj?.Paragraphs.Add("Calling the rigorous solver to calculate solution using Ideal K-values and/or Enthalpy values.")
            End If

            Dim result As Object

            IObj?.SetCurrent()

            Select Case Me.SolvingMethod
                Case 0 'BP
                    If Not TypeOf Me Is DistillationColumn Then Throw New Exception(FlowSheet.GetTranslatedString("UnsupportedSolver"))
                    result = SolvingMethods.WangHenkeMethod.Solve(nc, ns, maxits, tol, F, V, Q, L, VSS, LSS, Kval, x, y, z, fc, HF, T, P, Me.CondenserType, Me.StopAtIterationNumber, eff, Me.ColumnType, pp, Me.Specs, idealk0, idealh0, MaximumTemperatureStep)
                    ic = result(9)
                Case 1 'SC
                    Dim scm As New SolvingMethods.NaphtaliSandholmMethod
                    result = scm.Solve(nc, ns, maxits, tol, F, V, Q, L, VSS, LSS, Kval, x, y, z, fc, HF, T, P, Me.CondenserType, eff, Me.ColumnType, pp, Me.Specs, Me.SC_NumericalDerivativeStep, NS_Solver, NS_LowerBound, NS_UpperBound, NS_SimplexPreconditioning, idealk0, idealh0, MaximumTemperatureStep, llextractor)
                    ec = result(11)
                Case 2 'IO 
                    Dim rm As New SolvingMethods.RussellMethod
                    result = rm.Solve(nc, ns, maxits, tol, F, V, Q, L, VSS, LSS, Kval, x, y, z, fc, HF, T, P, Me.CondenserType, eff, Me.AdjustSb, Me.ColumnType, Me.KbjWeightedAverage, pp, Me.Specs, IO_NumericalDerivativeStep, IO_Solver, IO_LowerBound, IO_UpperBound, idealk0, idealh0, MaximumTemperatureStep, llextractor)
                    ic = result(9)
                    ec = result(11)
                Case 3 'SR
                    If Not TypeOf Me Is AbsorptionColumn Then Throw New Exception(FlowSheet.GetTranslatedString("UnsupportedSolver"))
                    result = SolvingMethods.BurninghamOttoMethod.Solve(nc, ns, maxits, tol, F, V, Q, L, VSS, LSS, Kval, x, y, z, fc, HF, T, P, Me.StopAtIterationNumber, eff, pp, Me.Specs, idealk0, idealh0, MaximumTemperatureStep, llextractor)
                    ic = result(9)
                Case Else
                    result = Nothing
            End Select

            '{Tj, Vj, Lj, VSSj, LSSj, yc, xc, K, Q, ic, t_error}

            Me.CondenserDuty = result(8)(0)
            Me.ReboilerDuty = result(8)(ns)

            'store final values
            xf.Clear()
            yf.Clear()
            Kf.Clear()
            For i = 0 To ns
                yf.Add(result(5)(i))
                xf.Add(result(6)(i))
                x(i) = result(5)(i)
                y(i) = result(6)(i)
                If Me.SolvingMethod = SolvingMethods.ColSolvingMethod.Russell_InsideOut Or
                   Me.SolvingMethod = SolvingMethods.ColSolvingMethod.NaphtaliSandholm_SimultaneousCorrection Then
                    Dim obj(nc - 1) As Double
                    For j = 0 To nc - 1
                        obj(j) = result(7)(i, j)
                    Next
                    Kval(i) = obj
                    Kf.Add(obj)
                    Me.JacobianMatrix = result(13)
                Else
                    Kf.Add(result(7)(i))
                    Kval(i) = result(7)(i)
                End If
            Next
            Tf = result(0)
            Vf = result(1)
            Lf = result(2)
            VSSf = result(3)
            LSSf = result(4)
            Q = result(8)

            IObj?.SetCurrent()

            If SolverScheme <> SolvingScheme.Direct Then
                're-run solver using refined initial estimates from last run with ideal properties
                IObj?.Paragraphs.Add("Re-running solver using refined initial estimates from last run with ideal properties.")
                Select Case Me.SolvingMethod
                    Case 2 'IO 
                        Dim rm As New SolvingMethods.RussellMethod
                        result = rm.Solve(nc, ns, maxits, tol, F, Vf, Q, Lf, VSSf, LSSf, Kval, x, y, z, fc, HF, Tf, P, Me.CondenserType, eff, Me.AdjustSb, Me.ColumnType, Me.KbjWeightedAverage, pp, Me.Specs, IO_NumericalDerivativeStep, IO_Solver, IO_LowerBound, IO_UpperBound, False, False, MaximumTemperatureStep, llextractor)
                        ic = result(9)
                        ec = result(11)
                    Case 0 'BP
                        result = SolvingMethods.WangHenkeMethod.Solve(nc, ns, maxits, tol, F, Vf, Q, Lf, VSSf, LSSf, Kval, x, y, z, fc, HF, T, P, Me.CondenserType, Me.StopAtIterationNumber, eff, Me.ColumnType, pp, Me.Specs, False, False, MaximumTemperatureStep)
                        ic = result(9)
                    Case 3 'SR
                        result = SolvingMethods.BurninghamOttoMethod.Solve(nc, ns, maxits, tol, F, Vf, Q, Lf, VSSf, LSSf, Kval, x, y, z, fc, HF, T, P, Me.StopAtIterationNumber, eff, pp, Me.Specs, False, False, MaximumTemperatureStep, llextractor)
                        ic = result(9)
                    Case 1 'SC
                        Dim scm As New SolvingMethods.NaphtaliSandholmMethod
                        result = scm.Solve(nc, ns, maxits, tol, F, Vf, Q, Lf, VSSf, LSSf, Kval, x, y, z, fc, HF, T, P, Me.CondenserType, eff, Me.ColumnType, pp, Me.Specs, Me.SC_NumericalDerivativeStep, NS_Solver, NS_LowerBound, NS_UpperBound, NS_SimplexPreconditioning, False, False, MaximumTemperatureStep, llextractor)
                        ec = result(11)
                    Case Else
                        result = Nothing
                End Select
            End If

            IObj?.Paragraphs.Add("Column is solved.")

            'if enabled, auto update initial estimates

            If Me.AutoUpdateInitialEstimates Then
                IObj?.Paragraphs.Add("Auto-updating initial estimates...")
                For i = 0 To Me.Stages.Count - 1
                    Me.InitialEstimates.StageTemps(i).Value = Tf(i)
                    Me.InitialEstimates.VapMolarFlows(i).Value = Vf(i)
                    Me.InitialEstimates.LiqMolarFlows(i).Value = Lf(i)
                    j = 0
                    For Each par As Parameter In Me.InitialEstimates.LiqCompositions(i).Values
                        par.Value = xf(i)(j)
                        j = j + 1
                    Next
                    j = 0
                    For Each par As Parameter In Me.InitialEstimates.VapCompositions(i).Values
                        par.Value = yf(i)(j)
                        j = j + 1
                    Next
                Next
            End If

            'update stage temperatures

            For i = 0 To Me.Stages.Count - 1
                Me.Stages(i).T = Tf(i)
            Next

            'update reflux ratio

            RefluxRatio = Lf(0) / (LSSf(0) + Vf(0))

            'copy results to output streams

            IObj?.Paragraphs.Add("Copying results to outlet streams...")

            'product flows

            Dim msm As MaterialStream = Nothing
            Dim sinf As StreamInformation

            For Each sinf In Me.MaterialStreams.Values
                Select Case sinf.StreamBehavior
                    Case StreamInformation.Behavior.Distillate
                        msm = FlowSheet.SimulationObjects(sinf.StreamID)
                        With msm
                            .SpecType = StreamSpec.Pressure_and_Enthalpy
                            .Phases(0).Properties.massflow = LSSf(0) * pp.AUX_MMM(xf(0)) / 1000
                            .Phases(0).Properties.molarflow = LSSf(0)
                            .Phases(0).Properties.temperature = Tf(0)
                            .Phases(0).Properties.pressure = P(0) - Me.CondenserDeltaP
                            IObj?.SetCurrent()
                            .Phases(0).Properties.enthalpy = pp.DW_CalcEnthalpy(xf(0), Tf(0), P(0), PropertyPackages.State.Liquid)
                            i = 0
                            For Each subst As BaseClasses.Compound In .Phases(0).Compounds.Values
                                subst.MoleFraction = xf(0)(i)
                                i += 1
                            Next
                            i = 0
                            For Each subst As BaseClasses.Compound In .Phases(0).Compounds.Values
                                subst.MassFraction = pp.AUX_CONVERT_MOL_TO_MASS(xf(0))(i)
                                i += 1
                            Next
                        End With
                    Case StreamInformation.Behavior.OverheadVapor
                        msm = FlowSheet.SimulationObjects(sinf.StreamID)
                        With msm
                            .SpecType = StreamSpec.Pressure_and_Enthalpy
                            .Phases(0).Properties.massflow = Vf(0) * pp.AUX_MMM(yf(0)) / 1000
                            .Phases(0).Properties.temperature = Tf(0)
                            .Phases(0).Properties.pressure = P(0)
                            IObj?.SetCurrent()
                            If llextractor Then
                                .Phases(0).Properties.enthalpy = pp.DW_CalcEnthalpy(yf(0), Tf(0), P(0), PropertyPackages.State.Liquid)
                            Else
                                .Phases(0).Properties.enthalpy = pp.DW_CalcEnthalpy(yf(0), Tf(0), P(0), PropertyPackages.State.Vapor)
                            End If
                            i = 0
                            For Each subst As BaseClasses.Compound In .Phases(0).Compounds.Values
                                subst.MoleFraction = yf(0)(i)
                                i += 1
                            Next
                            i = 0
                            For Each subst As BaseClasses.Compound In .Phases(0).Compounds.Values
                                subst.MassFraction = pp.AUX_CONVERT_MOL_TO_MASS(yf(0))(i)
                                i += 1
                            Next
                        End With
                    Case StreamInformation.Behavior.BottomsLiquid
                        msm = FlowSheet.SimulationObjects(sinf.StreamID)
                        With msm
                            .SpecType = StreamSpec.Pressure_and_Enthalpy
                            .Phases(0).Properties.massflow = Lf(ns) * pp.AUX_MMM(xf(ns)) / 1000
                            .Phases(0).Properties.temperature = Tf(ns)
                            .Phases(0).Properties.pressure = P(ns)
                            IObj?.SetCurrent()
                            .Phases(0).Properties.enthalpy = pp.DW_CalcEnthalpy(xf(ns), Tf(ns), P(ns), PropertyPackages.State.Liquid)
                            i = 0
                            For Each subst As BaseClasses.Compound In .Phases(0).Compounds.Values
                                subst.MoleFraction = xf(ns)(i)
                                i += 1
                            Next
                            i = 0
                            For Each subst As BaseClasses.Compound In .Phases(0).Compounds.Values
                                subst.MassFraction = pp.AUX_CONVERT_MOL_TO_MASS(xf(ns))(i)
                                i += 1
                            Next
                        End With
                    Case StreamInformation.Behavior.Sidedraw
                        Dim sidx As Integer = StageIndex(sinf.AssociatedStage)
                        msm = FlowSheet.SimulationObjects(sinf.StreamID)
                        If sinf.StreamPhase = StreamInformation.Phase.L Then
                            With msm
                                .SpecType = StreamSpec.Pressure_and_Enthalpy
                                .Phases(0).Properties.massflow = LSSf(sidx) * pp.AUX_MMM(xf(sidx)) / 1000
                                .Phases(0).Properties.temperature = Tf(sidx)
                                .Phases(0).Properties.pressure = P(sidx)
                                IObj?.SetCurrent()
                                .Phases(0).Properties.enthalpy = pp.DW_CalcEnthalpy(xf(sidx), Tf(sidx), P(sidx), PropertyPackages.State.Liquid)
                                i = 0
                                For Each subst As BaseClasses.Compound In .Phases(0).Compounds.Values
                                    subst.MoleFraction = xf(sidx)(i)
                                    i += 1
                                Next
                                i = 0
                                For Each subst As BaseClasses.Compound In .Phases(0).Compounds.Values
                                    subst.MassFraction = pp.AUX_CONVERT_MOL_TO_MASS(xf(sidx))(i)
                                    i += 1
                                Next
                            End With
                        ElseIf sinf.StreamPhase = StreamInformation.Phase.V Then
                            With msm
                                .SpecType = StreamSpec.Pressure_and_Enthalpy
                                .Phases(0).Properties.massflow = VSSf(sidx) * pp.AUX_MMM(yf(sidx)) / 1000
                                .Phases(0).Properties.temperature = Tf(sidx)
                                .Phases(0).Properties.pressure = P(sidx)
                                IObj?.SetCurrent()
                                .Phases(0).Properties.enthalpy = pp.DW_CalcEnthalpy(yf(sidx), Tf(sidx), P(sidx), PropertyPackages.State.Vapor)
                                i = 0
                                For Each subst As BaseClasses.Compound In .Phases(0).Compounds.Values
                                    subst.MoleFraction = yf(sidx)(i)
                                    i += 1
                                Next
                                i = 0
                                For Each subst As BaseClasses.Compound In .Phases(0).Compounds.Values
                                    subst.MassFraction = pp.AUX_CONVERT_MOL_TO_MASS(yf(sidx))(i)
                                    i += 1
                                Next
                            End With
                        End If
                End Select
            Next

            'condenser/reboiler duties

            IObj?.Paragraphs.Add("Updating Reboiler/Condenser duties...")

            Dim esm As Streams.EnergyStream

            For Each sinf In Me.EnergyStreams.Values
                If sinf.StreamBehavior = StreamInformation.Behavior.Distillate Then
                    'condenser
                    esm = FlowSheet.SimulationObjects(sinf.StreamID)
                    esm.EnergyFlow = Q(0)
                    esm.GraphicObject.Calculated = True
                ElseIf sinf.StreamBehavior = StreamInformation.Behavior.BottomsLiquid Then
                    'reboiler
                    esm = FlowSheet.SimulationObjects(sinf.StreamID)
                    If esm.GraphicObject.InputConnectors(0).IsAttached Then
                        esm.EnergyFlow = Q(Me.NumberOfStages - 1)
                    Else
                        esm.EnergyFlow = -Q(Me.NumberOfStages - 1)
                    End If
                    esm.GraphicObject.Calculated = True
                End If
            Next

            IObj?.Close()

        End Sub

        Function CalcIdealVapFrac(ByVal Vz As Object, ByVal PVAP As Object, ByVal P As Double) As Double

            Dim Pmin, Pmax, Px, vfrac As Double
            Dim n As Integer = UBound(Vz)
            Dim i As Integer

            i = 0
            Px = 0
            Do
                Px = Px + (Vz(i) / PVAP(i))
                i = i + 1
            Loop Until i = n + 1
            Px = 1 / Px

            Pmin = Px

            i = 0
            Px = 0
            Do
                Px = Px + Vz(i) * PVAP(i)
                i = i + 1
            Loop Until i = n + 1

            Pmax = Px

            vfrac = (P - Pmin) / (Pmax - Pmin)

            Return 1 - vfrac

        End Function

        Public Overrides Sub DeCalculate()

            Dim i As Integer

            'update output streams

            'product flows

            Dim sinf As StreamInformation
            Dim msm As MaterialStream = Nothing

            For Each sinf In Me.MaterialStreams.Values
                If FlowSheet.SimulationObjects.ContainsKey(sinf.StreamID) Then
                    Select Case sinf.StreamBehavior
                        Case StreamInformation.Behavior.Distillate
                            msm = FlowSheet.SimulationObjects(sinf.StreamID)
                            With msm
                                .Phases(0).Properties.massflow = 0
                                .Phases(0).Properties.temperature = 0
                                .Phases(0).Properties.pressure = 0
                                i = 0
                                For Each subst As BaseClasses.Compound In .Phases(0).Compounds.Values
                                    subst.MoleFraction = 0
                                    i += 1
                                Next
                            End With
                        Case StreamInformation.Behavior.OverheadVapor
                            msm = FlowSheet.SimulationObjects(sinf.StreamID)
                            With msm
                                .Phases(0).Properties.massflow = 0
                                .Phases(0).Properties.temperature = 0
                                .Phases(0).Properties.pressure = 0
                                i = 0
                                For Each subst As BaseClasses.Compound In .Phases(0).Compounds.Values
                                    subst.MoleFraction = 0
                                    i += 1
                                Next
                            End With
                        Case StreamInformation.Behavior.BottomsLiquid
                            msm = FlowSheet.SimulationObjects(sinf.StreamID)
                            With msm
                                .Phases(0).Properties.massflow = 0
                                .Phases(0).Properties.temperature = 0
                                .Phases(0).Properties.pressure = 0
                                i = 0
                                For Each subst As BaseClasses.Compound In .Phases(0).Compounds.Values
                                    subst.MoleFraction = 0
                                    i += 1
                                Next
                            End With
                        Case StreamInformation.Behavior.Sidedraw
                            Dim sidx As Integer = StageIndex(sinf.AssociatedStage)
                            msm = FlowSheet.SimulationObjects(sinf.StreamID)
                            If sinf.StreamPhase = StreamInformation.Phase.L Then
                                With msm
                                    .Phases(0).Properties.massflow = 0
                                    .Phases(0).Properties.temperature = 0
                                    .Phases(0).Properties.pressure = 0
                                    i = 0
                                    For Each subst As BaseClasses.Compound In .Phases(0).Compounds.Values
                                        subst.MoleFraction = 0
                                        i += 1
                                    Next
                                End With
                            ElseIf sinf.StreamPhase = StreamInformation.Phase.V Then
                                With msm
                                    .Phases(0).Properties.massflow = 0
                                    .Phases(0).Properties.temperature = 0
                                    .Phases(0).Properties.pressure = 0
                                    i = 0
                                    For Each subst As BaseClasses.Compound In .Phases(0).Compounds.Values
                                        subst.MoleFraction = 0
                                        i += 1
                                    Next
                                End With
                            End If
                    End Select
                End If
            Next

            'condenser/reboiler duties

            Dim esm As New Streams.EnergyStream("", "")

            For Each sinf In Me.EnergyStreams.Values
                If FlowSheet.SimulationObjects.ContainsKey(sinf.StreamID) Then
                    If sinf.StreamBehavior = StreamInformation.Behavior.Distillate Then
                        'condenser
                        esm = FlowSheet.SimulationObjects(sinf.StreamID)
                        esm.EnergyFlow = 0
                        esm.GraphicObject.Calculated = False
                    ElseIf sinf.StreamBehavior = StreamInformation.Behavior.BottomsLiquid Then
                        'reboiler
                        esm = FlowSheet.SimulationObjects(sinf.StreamID)
                        esm.EnergyFlow = 0
                        esm.GraphicObject.Calculated = False
                    End If
                End If
            Next

        End Sub

        Public Overrides Sub Validate()

            Dim sinf As StreamInformation
            Dim feedok As Boolean = False
            Dim rmok As Boolean = False
            Dim cmok As Boolean = False
            Dim cmvok As Boolean = False
            Dim reok As Boolean = False
            Dim ceok As Boolean = False

            'check existence/status of all specified material streams

            For Each sinf In Me.MaterialStreams.Values
                If Not FlowSheet.SimulationObjects.ContainsKey(sinf.StreamID) Then
                    Throw New Exception(FlowSheet.GetTranslatedString("DCStreamMissingException"))
                Else
                    Select Case sinf.StreamBehavior
                        Case StreamInformation.Behavior.Feed
                            'If Not FlowSheet.SimulationObjects(sinf.StreamID).GraphicObject.Calculated Then
                            '    Throw New Exception(FlowSheet.GetTranslatedString("DCStreamNotCalculatedException"))
                            'Else
                            feedok = True
                            'End If
                        Case StreamInformation.Behavior.Distillate
                            cmok = True
                        Case StreamInformation.Behavior.OverheadVapor
                            cmvok = True
                        Case StreamInformation.Behavior.BottomsLiquid
                            rmok = True
                    End Select
                End If
            Next

            For Each sinf In Me.EnergyStreams.Values
                If Not FlowSheet.SimulationObjects.ContainsKey(sinf.StreamID) Then
                    Throw New Exception(FlowSheet.GetTranslatedString("DCStreamMissingException"))
                Else
                    Select Case sinf.StreamBehavior
                        Case StreamInformation.Behavior.InterExchanger

                        Case StreamInformation.Behavior.Distillate
                            ceok = True
                        Case StreamInformation.Behavior.BottomsLiquid
                            reok = True
                    End Select
                End If
            Next

            'check if all connections were done correctly

            Select Case Me.ColumnType
                Case ColType.DistillationColumn
                    Select Case Me.CondenserType
                        Case condtype.Total_Condenser
                            If Not feedok Or Not cmok Or Not rmok Or Not ceok Or Not reok Then
                                Throw New Exception(FlowSheet.GetTranslatedString("DCConnectionMissingException"))
                            ElseIf Not cmvok And Me.CondenserType = condtype.Partial_Condenser Then
                                Throw New Exception(FlowSheet.GetTranslatedString("DCConnectionMissingException"))
                            End If
                        Case condtype.Partial_Condenser
                            If Not feedok Or Not cmok Or Not cmvok Or Not rmok Or Not ceok Or Not reok Then
                                Throw New Exception(FlowSheet.GetTranslatedString("DCConnectionMissingException"))
                            ElseIf Not cmvok And Me.CondenserType = condtype.Partial_Condenser Then
                                Throw New Exception(FlowSheet.GetTranslatedString("DCConnectionMissingException"))
                            End If
                        Case condtype.Full_Reflux
                            If Not feedok Or Not cmvok Or Not rmok Or Not ceok Or Not reok Then
                                Throw New Exception(FlowSheet.GetTranslatedString("DCConnectionMissingException"))
                            End If
                    End Select
                Case ColType.AbsorptionColumn
                    If Not feedok Or Not rmok Or Not (cmvok Or cmok) Then
                        Throw New Exception(FlowSheet.GetTranslatedString("DCConnectionMissingException"))
                    End If
                Case ColType.ReboiledAbsorber
                    If Not feedok Or Not cmvok Or Not rmok Or Not reok Then
                        Throw New Exception(FlowSheet.GetTranslatedString("DCConnectionMissingException"))
                    ElseIf Not cmvok And Me.CondenserType = condtype.Partial_Condenser Then
                        Throw New Exception(FlowSheet.GetTranslatedString("DCConnectionMissingException"))
                    End If
                Case ColType.RefluxedAbsorber
                    Select Case Me.CondenserType
                        Case condtype.Total_Condenser
                            If Not feedok Or Not cmok Or Not rmok Or Not ceok Then
                                Throw New Exception(FlowSheet.GetTranslatedString("DCConnectionMissingException"))
                            ElseIf Not cmvok And Me.CondenserType = condtype.Partial_Condenser Then
                                Throw New Exception(FlowSheet.GetTranslatedString("DCConnectionMissingException"))
                            End If
                        Case condtype.Partial_Condenser
                            If Not feedok Or Not cmok Or Not cmvok Or Not rmok Or Not ceok Then
                                Throw New Exception(FlowSheet.GetTranslatedString("DCConnectionMissingException"))
                            ElseIf Not cmvok And Me.CondenserType = condtype.Partial_Condenser Then
                                Throw New Exception(FlowSheet.GetTranslatedString("DCConnectionMissingException"))
                            End If
                        Case condtype.Full_Reflux
                            If Not feedok Or Not cmvok Or Not rmok Or Not ceok Then
                                Throw New Exception(FlowSheet.GetTranslatedString("DCConnectionMissingException"))
                            ElseIf Not cmvok And Me.CondenserType = condtype.Partial_Condenser Then
                                Throw New Exception(FlowSheet.GetTranslatedString("DCConnectionMissingException"))
                            End If
                    End Select
            End Select

            'all ok, proceed to calculations...

        End Sub

        Public Overrides Sub DisplayEditForm()

            If f Is Nothing Then
                f = New EditingForm_Column With {.SimObject = Me}
                f.ShowHint = GlobalSettings.Settings.DefaultEditFormLocation
                f.Tag = "ObjectEditor"
                Me.FlowSheet.DisplayForm(f)
            Else
                If f.IsDisposed Then
                    f = New EditingForm_Column With {.SimObject = Me}
                    f.ShowHint = GlobalSettings.Settings.DefaultEditFormLocation
                    f.Tag = "ObjectEditor"
                    Me.FlowSheet.DisplayForm(f)
                Else
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

        Public Overrides Sub CloseEditForm()
            If f IsNot Nothing Then
                If Not f.IsDisposed Then
                    f.Close()
                    f = Nothing
                End If
            End If
        End Sub

        Public Overrides Function GetChartModelNames() As List(Of String)

            Return New List(Of String)({"Temperature Profile", "Pressure Profile", "Vapor Flow Profile", "Liquid Flow Profile"})

        End Function

        Public Overrides Function GetChartModel(name As String) As Object
            Dim su = FlowSheet.FlowsheetOptions.SelectedUnitSystem

            Dim model = New PlotModel() With {.Subtitle = name, .Title = GraphicObject.Tag}

            model.TitleFontSize = 11
            model.SubtitleFontSize = 10

            model.Axes.Add(New LinearAxis() With {
                .MajorGridlineStyle = LineStyle.Dash,
                .MinorGridlineStyle = LineStyle.Dot,
                .Position = AxisPosition.Bottom,
                .FontSize = 10
            })

            model.Axes.Add(New LinearAxis() With {
                .MajorGridlineStyle = LineStyle.Dash,
                .MinorGridlineStyle = LineStyle.Dot,
                .Position = AxisPosition.Left,
                .FontSize = 10,
                .Title = "Stage",
                .StartPosition = 1,
                .EndPosition = 0,
                .MajorStep = 1.0,
                .MinorStep = 0.5
            })

            model.LegendFontSize = 11
            model.LegendPlacement = LegendPlacement.Outside
            model.LegendOrientation = LegendOrientation.Horizontal
            model.LegendPosition = LegendPosition.BottomCenter
            model.TitleHorizontalAlignment = TitleHorizontalAlignment.CenteredWithinView

            Dim py = PopulateColumnData(0)

            Select Case name
                Case "Temperature Profile"
                    model.AddLineSeries(PopulateColumnData(2), py)
                    model.Axes(0).Title = "Temperature (" + su.temperature + ")"
                Case "Pressure Profile"
                    model.AddLineSeries(PopulateColumnData(1), py)
                    model.Axes(0).Title = "Pressure (" + su.pressure + ")"
                Case "Vapor Flow Profile"
                    model.AddLineSeries(PopulateColumnData(3), py)
                    model.Axes(0).Title = "Molar Flow (" + su.molarflow + ")"
                Case "Liquid Flow Profile"
                    model.AddLineSeries(PopulateColumnData(4), py)
                    model.Axes(0).Title = "Molar Flow (" + su.molarflow + ")"
            End Select

            Return model

        End Function

        Private Function PopulateColumnData(position As Integer) As List(Of Double)
            Dim su = FlowSheet.FlowsheetOptions.SelectedUnitSystem
            Dim vec As New List(Of Double)()
            Select Case position
                Case 0
                    'stage
                    Dim comp_ant As Double = 1.0F
                    For Each st In Stages
                        vec.Add(comp_ant)
                        comp_ant += 1.0F
                    Next
                    Exit Select
                Case 1
                    'pressure
                    vec = SystemsOfUnits.Converter.ConvertArrayFromSI(su.pressure, P0).ToList()
                    Exit Select
                Case 2
                    'temperature
                    vec = SystemsOfUnits.Converter.ConvertArrayFromSI(su.temperature, Tf).ToList()
                    Exit Select
                Case 3
                    'vapor flow
                    vec = SystemsOfUnits.Converter.ConvertArrayFromSI(su.molarflow, Vf).ToList()
                    Exit Select
                Case 4
                    'liquid flow
                    vec = SystemsOfUnits.Converter.ConvertArrayFromSI(su.molarflow, Lf).ToList()
                    Exit Select
            End Select
            Return vec
        End Function

    End Class

End Namespace
