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
Imports DotNumerics.Optimization
Imports DWSIM.MathOps.MathEx.Optimization
Imports DWSIM.MathOps.MathEx.BrentOpt
Imports DWSIM.Thermodynamics.PropertyPackages.Auxiliary.FlashAlgorithms

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

        Public Function ValidateTemperatures() As Boolean

            If _stagetemps.Count = 0 Then Return False

            If _stagetemps.Select(Function(x) x.Value).ToArray().Sum = 0.0 Then Return False

            Return True

        End Function

        Public Function ValidateVaporFlows() As Boolean

            If _vapmolflows.Count = 0 Then Return False

            If _vapmolflows.Select(Function(x) x.Value).ToArray().Sum = 0.0 Then Return False

            Return True

        End Function

        Public Function ValidateLiquidFlows() As Boolean

            If _liqmolflows.Count = 0 Then Return False

            If _liqmolflows.Select(Function(x) x.Value).ToArray().Sum = 0.0 Then Return False

            Return True

        End Function

        Public Function ValidateCompositions() As Boolean

            If _liqcompositions.Select(Function(x) x.Values.Select(Function(x2) x2.Value).Sum).Sum = 0.0 Then
                Return False
            End If
            If _vapcompositions.Select(Function(x) x.Values.Select(Function(x2) x2.Value).Sum).Sum = 0.0 Then
                Return False
            End If

            If _liqcompositions.Count = 0 Then Return False
            If _vapcompositions.Count = 0 Then Return False

            Return True

        End Function

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
        Dim _ph As Phase = Phase.L
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

        Public Property CalculatedValue As Double

        Public Property InitialEstimate As Double?

    End Class

End Namespace

Namespace UnitOperations

    <Serializable()> Public Class DistillationColumn

        Inherits Column

        Public Property ReboiledAbsorber As Boolean = False

        Public Property RefluxedAbsorber As Boolean = False

        Public Sub New()
            MyBase.New()
        End Sub

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
                    proplist.Add("Condenser_Calculated_Value")
                    proplist.Add("Reboiler_Calculated_Value")
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
                    proplist.Add("Condenser_Calculated_Value")
                    proplist.Add("Reboiler_Calculated_Value")
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
                    Case "Condenser_Calculated_Value"
                        value = Me.Specs("C").CalculatedValue
                    Case "Reboiler_Calculated_Value"
                        value = Me.Specs("R").CalculatedValue
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
                    Case "Condenser_Specification_Value", "Condenser_Calculated_Value"
                        value = Me.Specs("C").SpecUnit
                    Case "Reboiler_Specification_Value", "Reboiler_Calculated_Value"
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

        Public _opmode As OpMode = OpMode.Absorber

        Public Sub New()
            MyBase.New()
        End Sub

        Public Enum OpMode
            Absorber = 0
            Extractor = 1
        End Enum

        Public Property OperationMode() As OpMode
            Get
                Return _opmode
            End Get
            Set(ByVal value As OpMode)
                _opmode = value
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
                        Try
                            value = SystemsOfUnits.Converter.ConvertFromSI(su.pressure, Me.Stages.First.P)
                        Catch ex As Exception
                            value = 0.0
                        End Try
                    Case 1
                        'PROP_DC_1	Reboiler Pressure
                        Try
                            value = SystemsOfUnits.Converter.ConvertFromSI(su.pressure, Me.Stages.Last.P)
                        Catch ex As Exception
                            value = 0.0
                        End Try
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

        Public Overrides Property Visible As Boolean = False

        Public Sub New()
            MyBase.New()
        End Sub

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

        Public Overrides Property Visible As Boolean = False

        Public Sub New()
            MyBase.New()
        End Sub

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
        Private _ilooptolerance As Double = 0.00001
        Private _elooptolerance As Double = 0.00001

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

        ''' <summary>
        ''' Set the number of stages (n > 3)
        ''' </summary>
        ''' <param name="n"></param>
        Public Sub SetNumberOfStages(n As Integer)

            If n <= 3 Then Throw New Exception("Invalid number of stages")

            NumberOfStages = n

            Dim ne As Integer = NumberOfStages

            Dim nep As Integer = Stages.Count

            Dim dif As Integer = ne - nep

            If dif < 0 Then
                Stages.RemoveRange(nep + dif - 1, -dif)
                With InitialEstimates
                    .LiqCompositions.RemoveRange(nep + dif - 1, -dif)
                    .VapCompositions.RemoveRange(nep + dif - 1, -dif)
                    .LiqMolarFlows.RemoveRange(nep + dif - 1, -dif)
                    .VapMolarFlows.RemoveRange(nep + dif - 1, -dif)
                    .StageTemps.RemoveRange(nep + dif - 1, -dif)
                End With
            ElseIf dif > 0 Then
                Dim i As Integer
                For i = 1 To dif
                    Stages.Insert(Stages.Count - 1, New Stage(Guid.NewGuid().ToString))
                    Stages(Stages.Count - 2).Name = "Stage_" & Stages.Count - 2
                    With InitialEstimates
                        Dim d As New Dictionary(Of String, Parameter)
                        For Each cp In FlowSheet.SelectedCompounds.Values
                            d.Add(cp.Name, New Parameter)
                        Next
                        .LiqCompositions.Insert(.LiqCompositions.Count - 1, d)
                        .VapCompositions.Insert(.VapCompositions.Count - 1, d)
                        .LiqMolarFlows.Insert(.LiqMolarFlows.Count - 1, New Parameter)
                        .VapMolarFlows.Insert(.VapMolarFlows.Count - 1, New Parameter)
                        .StageTemps.Insert(.StageTemps.Count - 1, New Parameter)
                    End With
                Next
            End If

        End Sub

        ''' <summary>
        ''' Sets the Stream feed stage.
        ''' </summary>
        ''' <param name="streamName">Material Stream ID ('Name') property.</param>
        ''' <param name="stageIndex">Stage Index (0 = condenser)</param>
        Public Sub SetStreamFeedStage(streamName As String, stageIndex As Integer)

            Dim si = MaterialStreams.Where(Function(s) s.Value.StreamID = streamName).FirstOrDefault()
            si.Value.AssociatedStage = Stages(stageIndex).ID

        End Sub

        ''' <summary>
        ''' Sets the Stream feed stage.
        ''' </summary>
        ''' <param name="streamName">Material Stream ID ('Name') property.</param>
        ''' <param name="stageID">Stage ID (unique ID)</param>
        Public Sub SetStreamFeedStage(streamName As String, stageID As String)

            Dim si = MaterialStreams.Where(Function(s) s.Value.StreamID = streamName).FirstOrDefault()
            si.Value.AssociatedStage = stageID

        End Sub

        ''' <summary>
        ''' Sets the Stream feed stage.
        ''' </summary>
        ''' <param name="stream"></param>
        ''' <param name="stageIndex">Stage Index (0 = condenser)</param>
        Public Sub SetStreamFeedStage(stream As MaterialStream, stageIndex As Integer)

            Dim si = MaterialStreams.Where(Function(s) s.Value.StreamID = stream.Name).FirstOrDefault()
            si.Value.AssociatedStage = Stages(stageIndex).ID

        End Sub

        ''' <summary>
        ''' Sets the Stream feed stage.
        ''' </summary>
        ''' <param name="stream"></param>
        ''' <param name="stageID">Stage ID (unique ID)</param>
        Public Sub SetStreamFeedStage(stream As MaterialStream, stageID As String)

            Dim si = MaterialStreams.Where(Function(s) s.Value.StreamID = stream.Name).FirstOrDefault()
            si.Value.AssociatedStage = stageID

        End Sub

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

        Public Property UseBroydenAcceleration As Boolean = True

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
                    Select Case strinfo.StreamBehavior
                        Case StreamInformation.Behavior.Distillate
                            idx = FlowSheet.GraphicObjects(strinfo.StreamID).InputConnectors(0).AttachedConnector.AttachedFromConnectorIndex
                            If Me.GraphicObject.FlippedH Then
                                Me.GraphicObject.OutputConnectors(idx).Position = New Point.Point(Me.GraphicObject.X, Me.GraphicObject.Y + 0.08 * Me.GraphicObject.Height)
                            Else
                                Me.GraphicObject.OutputConnectors(idx).Position = New Point.Point(Me.GraphicObject.X + Me.GraphicObject.Width, Me.GraphicObject.Y + 0.08 * Me.GraphicObject.Height)
                            End If
                        Case StreamInformation.Behavior.BottomsLiquid
                            idx = FlowSheet.GraphicObjects(strinfo.StreamID).OutputConnectors(0).AttachedConnector.AttachedToConnectorIndex
                            If Me.GraphicObject.FlippedH Then
                                Me.GraphicObject.OutputConnectors(idx).Position = New Point.Point(Me.GraphicObject.X, Me.GraphicObject.Y + 0.825 * Me.GraphicObject.Height)
                            Else
                                Me.GraphicObject.OutputConnectors(idx).Position = New Point.Point(Me.GraphicObject.X + Me.GraphicObject.Width, Me.GraphicObject.Y + 0.825 * Me.GraphicObject.Height)
                            End If
                        Case StreamInformation.Behavior.InterExchanger
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

            'handle special cases when no initial estimates are used

            Dim special As Boolean = False

            Dim Vn = FlowSheet.SelectedCompounds.Keys.ToList()

            If Vn.Contains("Ethanol") And Vn.Contains("Water") Then
                'probably an azeotrope situation.
                special = True
            End If

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

            Dim sumcf(nc - 1), sumF, zm(nc - 1), alpha(nc - 1), distVx(nc - 1), rebVx(nc - 1), distVy(nc - 1), rebVy(nc - 1) As Double

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
                        If ms.StreamPhase = StreamInformation.Phase.V Then
                            VSS(StageIndex(ms.AssociatedStage)) = ms.FlowRate.Value
                        Else
                            LSS(StageIndex(ms.AssociatedStage)) = ms.FlowRate.Value
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

            For i = ns To 0 Step -1
                If F(i) <> 0 Then
                    lastF = i
                    Exit For
                End If
            Next

            For i = 0 To nc - 1
                zm(i) = sumcf(i) / sumF
            Next

            Dim mwf = pp.AUX_MMM(zm)

            If Me.Specs("C").SType = ColumnSpec.SpecType.Stream_Ratio Then
                rr = Me.Specs("C").SpecValue
            ElseIf Me.Specs("C").SType = ColumnSpec.SpecType.Component_Fraction Or
                Me.Specs("C").SType = ColumnSpec.SpecType.Component_Recovery Then
                rr = 15.0
            Else
                rr = 2.5
            End If

            Dim Tref = FT.Where(Function(ti) ti > 0).Average
            Dim Pref = Stages.Select(Function(s) s.P).Average

            Dim Kref = pp.DW_CalcKvalue(zm, Tref, Pref)

            Dim Vprops = pp.DW_GetConstantProperties()

            Dim feedflash = pp.CalculateEquilibrium(FlashCalculationType.PressureTemperature, Pref, Tref, zm, Nothing, Nothing)

            Dim hamount As Double = 0.0

            Select Case Specs("R").SType
                Case ColumnSpec.SpecType.Component_Fraction
                    Dim cname = Specs("R").ComponentID
                    Dim cvalue = Specs("R").SpecValue
                    Dim cunits = Specs("R").SpecUnit
                    Dim cindex = Vn.IndexOf(cname)
                    rebVx(cindex) = cvalue * zm(cindex) * sumF
                    hamount = cvalue * zm(cindex) * sumF
                    For i = 0 To nc - 1
                        If Kref(i) < Kref(cindex) Then
                            hamount += sumF * zm(i)
                            rebVx(i) = sumF * zm(i)
                        ElseIf i <> cindex Then
                            rebVx(i) = 0.0
                        End If
                    Next
                    rebVx = rebVx.NormalizeY()
                Case ColumnSpec.SpecType.Component_Mass_Flow_Rate
                    Dim cname = Specs("R").ComponentID
                    Dim cvalue = Specs("R").SpecValue
                    Dim cunits = Specs("R").SpecUnit
                    Dim cindex = Vn.IndexOf(cname)
                    Dim camount = cvalue.ConvertToSI(cunits) / Vprops(cindex).Molar_Weight * 1000
                    hamount = camount
                    rebVx(cindex) = camount
                    For i = 0 To nc - 1
                        If Kref(i) < Kref(cindex) Then
                            hamount += sumF * zm(i)
                            rebVx(i) = sumF * zm(i)
                        ElseIf i <> cindex Then
                            rebVx(i) = 0.0
                        End If
                    Next
                    rebVx = rebVx.NormalizeY()
                Case ColumnSpec.SpecType.Component_Molar_Flow_Rate
                    Dim cname = Specs("R").ComponentID
                    Dim cvalue = Specs("R").SpecValue
                    Dim cunits = Specs("R").SpecUnit
                    Dim cindex = Vn.IndexOf(cname)
                    Dim camount = cvalue.ConvertToSI(cunits) / Vprops(cindex).Molar_Weight * 1000
                    hamount = camount
                    rebVx(cindex) = camount
                    For i = 0 To nc - 1
                        If Kref(i) < Kref(cindex) Then
                            hamount += sumF * zm(i)
                            rebVx(i) = sumF * zm(i)
                        ElseIf i <> cindex Then
                            rebVx(i) = 0.0
                        End If
                    Next
                    rebVx = rebVx.NormalizeY()
                Case ColumnSpec.SpecType.Component_Recovery
                    Dim cname = Specs("R").ComponentID
                    Dim cvalue = Specs("R").SpecValue
                    Dim cindex = Vn.IndexOf(cname)
                    Dim camount = sumF * zm(cindex) * cvalue / 100
                    hamount = camount
                    rebVx(cindex) = camount
                    For i = 0 To nc - 1
                        If Kref(i) < Kref(cindex) Then
                            hamount += sumF * zm(i)
                            rebVx(i) = sumF * zm(i)
                        ElseIf i <> cindex Then
                            rebVx(i) = 0.0
                        End If
                    Next
                    rebVx = rebVx.NormalizeY()
                Case ColumnSpec.SpecType.Product_Mass_Flow_Rate
                    If Me.CondenserType = condtype.Full_Reflux Then
                        vaprate = sumF - SystemsOfUnits.Converter.ConvertToSI(Me.Specs("R").SpecUnit, Me.Specs("R").SpecValue) / mwf * 1000 - sum0_
                        distrate = 0.0
                    ElseIf Me.CondenserType = condtype.Partial_Condenser Then
                        If Me.Specs("C").SType = ColumnSpec.SpecType.Product_Molar_Flow_Rate Then
                            distrate = SystemsOfUnits.Converter.ConvertToSI(Me.Specs("C").SpecUnit, Me.Specs("C").SpecValue) / mwf * 1000
                        Else
                            distrate = sumF - SystemsOfUnits.Converter.ConvertToSI(Me.Specs("R").SpecUnit, Me.Specs("R").SpecValue) / mwf * 1000 - sum0_ - vaprate
                        End If
                    Else
                        distrate = sumF - SystemsOfUnits.Converter.ConvertToSI(Me.Specs("R").SpecUnit, Me.Specs("R").SpecValue) / mwf * 1000 - sum0_
                        vaprate = 0.0
                    End If
                Case ColumnSpec.SpecType.Product_Molar_Flow_Rate
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
                Case Else
                    If Me.CondenserType = condtype.Full_Reflux Then
                        vaprate = sumF / 2 - sum0_
                    Else
                        distrate = sumF / 2 - sum0_ - vaprate
                    End If
            End Select

            Select Case Specs("R").SType
                Case ColumnSpec.SpecType.Component_Mass_Flow_Rate,
                      ColumnSpec.SpecType.Component_Molar_Flow_Rate,
                      ColumnSpec.SpecType.Component_Recovery,
                      ColumnSpec.SpecType.Component_Fraction
                    If Me.CondenserType = condtype.Full_Reflux Then
                        vaprate = sumF - hamount - sum0_
                        distrate = 0.0
                    ElseIf Me.CondenserType = condtype.Partial_Condenser Then
                        If Me.Specs("C").SType = ColumnSpec.SpecType.Product_Molar_Flow_Rate Then
                            distrate = SystemsOfUnits.Converter.ConvertToSI(Me.Specs("C").SpecUnit, Me.Specs("C").SpecValue) / mwf * 1000
                        Else
                            distrate = sumF - hamount - sum0_ - vaprate
                        End If
                    Else
                        distrate = sumF - hamount - sum0_
                        vaprate = 0.0
                    End If
            End Select

            Dim lamount As Double = 0.0

            Select Case Specs("C").SType
                Case ColumnSpec.SpecType.Component_Fraction
                    Dim cname = Specs("C").ComponentID
                    Dim cvalue = Specs("C").SpecValue
                    Dim cunits = Specs("C").SpecUnit
                    Dim cindex = Vn.IndexOf(cname)
                    lamount = cvalue * zm(cindex) * sumF
                    distVx(cindex) = cvalue * zm(cindex) * sumF
                    For i = 0 To nc - 1
                        If Kref(i) > Kref(cindex) Then
                            lamount += sumF * zm(i)
                            distVx(i) = sumF * zm(i)
                        ElseIf i <> cindex Then
                            distVx(i) = 0.0
                        End If
                    Next
                    distVx = distVx.NormalizeY()
                Case ColumnSpec.SpecType.Component_Mass_Flow_Rate
                    Dim cname = Specs("C").ComponentID
                    Dim cvalue = Specs("C").SpecValue
                    Dim cunits = Specs("C").SpecUnit
                    Dim cindex = Vn.IndexOf(cname)
                    Dim camount = cvalue.ConvertToSI(cunits) / Vprops(cindex).Molar_Weight * 1000
                    lamount = camount
                    distVx(cindex) = camount
                    For i = 0 To nc - 1
                        If Kref(i) > Kref(cindex) Then
                            lamount += sumF * zm(i)
                            distVx(i) = sumF * zm(i)
                        ElseIf i <> cindex Then
                            distVx(i) = 0.0
                        End If
                    Next
                    distVx = distVx.NormalizeY()
                Case ColumnSpec.SpecType.Component_Molar_Flow_Rate
                    Dim cname = Specs("C").ComponentID
                    Dim cvalue = Specs("C").SpecValue
                    Dim cunits = Specs("C").SpecUnit
                    Dim cindex = Vn.IndexOf(cname)
                    Dim camount = cvalue.ConvertToSI(cunits) / Vprops(cindex).Molar_Weight * 1000
                    lamount = camount
                    distVx(cindex) = camount
                    For i = 0 To nc - 1
                        If Kref(i) > Kref(cindex) Then
                            lamount += sumF * zm(i)
                            distVx(i) = sumF * zm(i)
                        ElseIf i <> cindex Then
                            distVx(i) = 0.0
                        End If
                    Next
                    distVx = distVx.NormalizeY()
                Case ColumnSpec.SpecType.Component_Recovery
                    Dim cname = Specs("C").ComponentID
                    Dim cvalue = Specs("C").SpecValue
                    Dim cindex = Vn.IndexOf(cname)
                    Dim camount = sumF * zm(cindex) * cvalue / 100
                    lamount = camount
                    distVx(cindex) = camount
                    For i = 0 To nc - 1
                        If Kref(i) > Kref(cindex) Then
                            lamount += sumF * zm(i)
                            distVx(i) = sumF * zm(i)
                        ElseIf i <> cindex Then
                            distVx(i) = 0.0
                        End If
                    Next
                    distVx = distVx.NormalizeY()
            End Select

            IObj?.Paragraphs.Add(String.Format("Estimated/Specified Distillate Rate: {0} mol/s", distrate))
            IObj?.Paragraphs.Add(String.Format("Estimated/Specified Vapor Overflow Rate: {0} mol/s", vaprate))
            IObj?.Paragraphs.Add(String.Format("Estimated/Specified Reflux Ratio: {0}", rr))

            compids = New ArrayList
            compids.Clear()
            For Each comp As Thermodynamics.BaseClasses.Compound In stream.Phases(0).Compounds.Values
                compids.Add(comp.Name)
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
                    If (T1 = 0.0) Then Throw New Exception("The absorber needs a feed stream connected to the first stage.")
                    If (T2 = 0.0) Then Throw New Exception("The absorber needs a feed stream connected to the last stage.")
                Case ColType.ReboiledAbsorber
                    T1 = MathEx.Common.WgtAvg(F, FT)
                    T2 = T1
                Case ColType.RefluxedAbsorber
                    P(0) -= CondenserDeltaP
                    T1 = MathEx.Common.WgtAvg(F, FT)
                    T2 = T1
                Case ColType.DistillationColumn
                    If Not DirectCast(Me, DistillationColumn).ReboiledAbsorber Then
                        P(0) -= CondenserDeltaP
                    End If
                    If special Then
                        T1 = Tref
                    Else
                        Try
                            IObj?.SetCurrent()
                            If distVx.Sum > 0 Then
                                Dim fcalc = pp.CalculateEquilibrium(FlashCalculationType.PressureVaporFraction, P(0), 0, distVx, Nothing, FT.Max)
                                T1 = fcalc.CalculatedTemperature
                                distVy = distVx.MultiplyY(fcalc.Kvalues.Select(Function(k) Convert.ToDouble(IIf(Double.IsNaN(k), 0.0, k))).ToArray()).NormalizeY()
                            Else
                                T1 = pp.DW_CalcBubT(zm, P(0), MathEx.Common.Min(FT))(4) '* 1.01
                            End If
                        Catch ex As Exception
                            T1 = FT.Where(Function(t_) t_ > 0.0).Min
                        End Try
                    End If
                    If special Then
                        T2 = Tref
                    Else
                        Try
                            IObj?.SetCurrent()
                            If rebVx.Sum > 0 Then
                                Dim fcalc = pp.CalculateEquilibrium(FlashCalculationType.PressureVaporFraction, P(ns), 0, rebVx, Nothing, FT.Max)
                                T2 = fcalc.CalculatedTemperature
                                rebVy = rebVx.MultiplyY(fcalc.Kvalues.Select(Function(k) Convert.ToDouble(IIf(Double.IsNaN(k), 0.0, k))).ToArray()).NormalizeY()
                            Else
                                T2 = pp.DW_CalcDewT(zm, P(ns), MathEx.Common.Max(FT))(4) '* 0.99
                            End If
                        Catch ex As Exception
                            T2 = FT.Where(Function(t_) t_ > 0.0).Max
                        End Try
                    End If
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
                If Me.UseTemperatureEstimates And InitialEstimates.ValidateTemperatures() Then
                    T(i) = Me.InitialEstimates.StageTemps(i).Value
                Else
                    T(i) = (T2 - T1) * (i) / ns + T1
                End If
                If Me.UseVaporFlowEstimates And InitialEstimates.ValidateVaporFlows() Then
                    V(i) = Me.InitialEstimates.VapMolarFlows(i).Value
                Else
                    If i = 0 Then
                        Select Case Me.ColumnType
                            Case ColType.DistillationColumn
                                If Me.CondenserType = condtype.Total_Condenser Then
                                    If DirectCast(Me, DistillationColumn).ReboiledAbsorber Then
                                        V(0) = F(lastF)
                                    Else
                                        V(0) = 0.0000000001
                                    End If
                                Else
                                    V(0) = vaprate
                                End If
                            Case ColType.RefluxedAbsorber
                                If Me.CondenserType = condtype.Total_Condenser Then
                                    V(0) = 0.0000000001
                                Else
                                    V(0) = vaprate
                                End If
                            Case Else
                                V(0) = F(lastF)
                        End Select
                    Else
                        Select Case Me.ColumnType
                            Case ColType.DistillationColumn
                                If DirectCast(Me, DistillationColumn).ReboiledAbsorber Then
                                    V(i) = (rr + 1) * V(0) - F(0)
                                Else
                                    If Me.CondenserType = condtype.Partial_Condenser Then
                                        V(i) = (rr + 1) * (distrate + vaprate) - F(0)
                                    ElseIf Me.CondenserType = condtype.Full_Reflux Then
                                        V(i) = (rr + 1) * V(0) - F(0)
                                    Else
                                        V(i) = (rr + 1) * distrate - F(0)
                                    End If
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
                If Me.UseLiquidFlowEstimates And InitialEstimates.ValidateLiquidFlows() Then
                    L(i) = Me.InitialEstimates.LiqMolarFlows(i).Value
                Else
                    If i = 0 Then
                        Select Case Me.ColumnType
                            Case ColType.DistillationColumn
                                If DirectCast(Me, DistillationColumn).ReboiledAbsorber Then
                                    L(0) = vaprate * rr
                                Else
                                    If Me.CondenserType = condtype.Partial_Condenser Then
                                        L(0) = (distrate + vaprate) * rr
                                    ElseIf Me.CondenserType = condtype.Full_Reflux Then
                                        L(0) = vaprate * rr
                                    Else
                                        L(0) = distrate * rr
                                    End If
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
                                If DirectCast(Me, DistillationColumn).ReboiledAbsorber Then
                                    L(i) = F(firstF)
                                Else
                                    If i < ns Then L(i) = V(i) + sum1(i) - V(0) Else L(i) = sum1(i) - V(0)
                                End If
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
                If Me.UseCompositionEstimates And InitialEstimates.ValidateCompositions() Then
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
                    z(i) = zm
                    Kval(i) = pp.DW_CalcKvalue(x(i), y(i), T(i), P(i))
                Else
                    IObj?.SetCurrent()

                    Dim flashresult As Object = Nothing

                    z(i) = zm

                    If rebVx.Sum > 0 And distVx.Sum > 0 Then
                        For j = 0 To nc - 1
                            x(i)(j) = distVx(j) + Convert.ToDouble(i) / Convert.ToDouble(ns) * (rebVx(j) - distVx(j))
                            y(i)(j) = distVy(j) + Convert.ToDouble(i) / Convert.ToDouble(ns) * (rebVy(j) - distVy(j))
                        Next
                        x(i) = x(i).NormalizeY
                        y(i) = y(i).NormalizeY
                        'ElseIf distVx.Sum > 0 Then
                        '    For j = 0 To nc - 1
                        '        x(i)(j) = distVx(j) + Convert.ToDouble(i) / Convert.ToDouble(ns) * (zm(j) - distVx(j))
                        '        If x(i)(j) < 0.0 Then x(i)(j) = 0.0
                        '        y(i)(j) = distVy(j) + Convert.ToDouble(i) / Convert.ToDouble(ns) * (zm(j) - distVy(j))
                        '        If y(i)(j) < 0.0 Then y(i)(j) = 0.0
                        '    Next
                        '    x(i) = x(i).NormalizeY
                        '    y(i) = y(i).NormalizeY
                        'ElseIf rebVx.Sum > 0 Then
                        '    For j = 0 To nc - 1
                        '        x(i)(j) = zm(j) + Convert.ToDouble(i) / Convert.ToDouble(ns) * (rebVx(j) - zm(j))
                        '        y(i)(j) = zm(j) + Convert.ToDouble(i) / Convert.ToDouble(ns) * (rebVy(j) - zm(j))
                        '    Next
                        '    x(i) = x(i).NormalizeY
                        '    y(i) = y(i).NormalizeY
                        Kval(i) = pp.DW_CalcKvalue(x(i), y(i), T(i), P(i))
                    Else
                        Kval(i) = pp.DW_CalcKvalue_Ideal_VP(T(i), P(i))
                        For j = 0 To nc - 1
                            x(i)(j) = (L(i) + V(i)) * z(i)(j) / (L(i) + V(i) * Kval(i)(j))
                            y(i)(j) = Kval(i)(j) * x(i)(j)
                        Next
                        x(i) = x(i).NormalizeY()
                        y(i) = y(i).NormalizeY()
                        'flashresult = pp.FlashBase.Flash_PT(zm, P(i), T(i), pp)
                        'For j = 0 To nc - 1
                        '    If flashresult(0) = 0.0 Then
                        '        x(i)(j) = flashresult(2)(j)
                        '    Else
                        '        x(i)(j) = (flashresult(0) * flashresult(2)(j) + flashresult(5) * flashresult(6)(j)) / (flashresult(0) + flashresult(5))
                        '    End If
                        'Next
                        'y(i) = flashresult(3)
                    End If
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

            Dim L1trials, L2trials As New List(Of Double())
            Dim x1trials, x2trials As New List(Of Double()())

            If llextractor Then

                If Not UseCompositionEstimates Or Not UseLiquidFlowEstimates Or Not UseVaporFlowEstimates Then

                    'll extractor
                    Dim L1, L2, Vx1(), Vx2() As Double
                    Dim trialcomp As Double() = zm.Clone
                    For counter As Integer = 0 To 30
                        Dim flashresult = pp.FlashBase.Flash_PT(trialcomp, P.Average, T.Average, pp)
                        L1 = flashresult(0)
                        L2 = flashresult(5)
                        Vx1 = flashresult(2)
                        Vx2 = flashresult(6)
                        If L2 > 0.0 Then
                            Dim L1t, L2t As New List(Of Double)
                            Dim xt1, xt2 As New List(Of Double())
                            For i = 0 To Stages.Count - 1
                                If UseLiquidFlowEstimates Then
                                    L1t.Add(L.Clone)
                                Else
                                    L1t.Add(F.Sum * L1)
                                End If
                                If UseVaporFlowEstimates Then
                                    L2t.Add(V.Clone)
                                Else
                                    L2t.Add(F.Sum * L2)
                                End If
                                If UseCompositionEstimates Then
                                    xt1.Add(x(i).Clone)
                                    xt2.Add(y(i).Clone)
                                Else
                                    xt1.Add(Vx1)
                                    xt2.Add(Vx2)
                                End If
                            Next
                            L1trials.Add(L1t.ToArray())
                            L2trials.Add(L2t.ToArray())
                            x1trials.Add(xt1.ToArray())
                            x2trials.Add(xt2.ToArray())
                        End If
                        Dim rnd As New Random(counter)
                        trialcomp = Enumerable.Repeat(0, nc).Select(Function(d) rnd.NextDouble()).ToArray
                        trialcomp = trialcomp.NormalizeY
                    Next

                    trialcomp = zm.Clone
                    Dim lle As New PropertyPackages.Auxiliary.FlashAlgorithms.SimpleLLE()
                    For counter As Integer = 0 To 20
                        Dim flashresult = lle.Flash_PT(trialcomp, P.Average, T.Average, pp)
                        L1 = flashresult(0)
                        L2 = flashresult(5)
                        Vx1 = flashresult(2)
                        Vx2 = flashresult(6)
                        If L2 > 0.0 Then
                            Dim L1t, L2t As New List(Of Double)
                            Dim xt1, xt2 As New List(Of Double())
                            For i = 0 To Stages.Count - 1
                                If UseLiquidFlowEstimates Then
                                    L1t.Add(L.Clone)
                                Else
                                    L1t.Add(F.Sum * L1)
                                End If
                                If UseVaporFlowEstimates Then
                                    L2t.Add(V.Clone)
                                Else
                                    L2t.Add(F.Sum * L2)
                                End If
                                If UseCompositionEstimates Then
                                    xt1.Add(x(i).Clone)
                                    xt2.Add(y(i).Clone)
                                Else
                                    xt1.Add(Vx1)
                                    xt2.Add(Vx2)
                                End If
                            Next
                            L1trials.Add(L1t.ToArray())
                            L2trials.Add(L2t.ToArray())
                            x1trials.Add(xt1.ToArray())
                            x2trials.Add(xt2.ToArray())
                        End If
                        Dim rnd As New Random(counter)
                        trialcomp = Enumerable.Repeat(0, nc).Select(Function(d) rnd.NextDouble()).ToArray
                        trialcomp = trialcomp.NormalizeY
                    Next

                Else

                    Dim L1t, L2t As New List(Of Double)
                    Dim xt1, xt2 As New List(Of Double())
                    For i = 0 To Stages.Count - 1
                        L1t.AddRange(L)
                        L2t.AddRange(V)
                        xt1.Add(x(i).Clone)
                        xt2.Add(y(i).Clone)
                    Next
                    L1trials.Add(L1t.ToArray())
                    L2trials.Add(L2t.ToArray())
                    x1trials.Add(xt1.ToArray())
                    x2trials.Add(xt2.ToArray())

                End If

            End If

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

            Dim result As Object = Nothing

            IObj?.SetCurrent()

            If TypeOf Me Is DistillationColumn Then
                result = SolvingMethods.WangHenkeMethod.Solve(Me, nc, ns, maxits, tol, F, V, Q, L, VSS, LSS, Kval, x, y, z, fc, HF, T, P, Me.CondenserType, -1, eff, Me.ColumnType, pp, Me.Specs, False, False)
                ic = result(9)
            ElseIf TypeOf Me Is AbsorptionColumn Then
                If llextractor Then
                    'run all trial compositions until it solves
                    Dim ntrials = L1trials.Count
                    Dim ex0 As Exception = Nothing
                    For i = 0 To ntrials - 1
                        Try
                            For j = 0 To Stages.Count - 1
                                For k = 0 To nc - 1
                                    If x1trials(i)(j)(k) = 0.0 Then
                                        Kval(j)(k) = 1.0E+20
                                    Else
                                        Kval(j)(k) = (x2trials(i)(j)(k) / x1trials(i)(j)(k))
                                    End If
                                Next
                            Next
                            result = SolvingMethods.BurninghamOttoMethod.Solve(nc, ns, maxits, tol, F, L2trials(i), Q, L1trials(i), VSS, LSS, Kval, x1trials(i), x2trials(i), z, fc, HF, T, P, -1, eff, pp, Me.Specs, False, False, llextractor)
                            ic = result(9)
                            ex0 = Nothing
                            Exit For
                        Catch ex As Exception
                            'do nothing, try next set
                            ex0 = ex
                        End Try
                    Next
                    If ex0 IsNot Nothing Then Throw ex0
                Else
                    result = SolvingMethods.BurninghamOttoMethod.Solve(nc, ns, maxits, tol, F, V, Q, L, VSS, LSS, Kval, x, y, z, fc, HF, T, P, -1, eff, pp, Me.Specs, False, False, llextractor)
                    ic = result(9)
                End If
            End If

            IObj?.Paragraphs.Add("Column is solved.")

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
                Kf.Add(result(7)(i))
                Kval(i) = result(7)(i)
            Next
            Tf = result(0)
            Vf = result(1)
            Lf = result(2)
            VSSf = result(3)
            LSSf = result(4)
            Q = result(8)

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
                            .Clear()
                            .SpecType = StreamSpec.Pressure_and_Enthalpy
                            .Phases(0).Properties.massflow = LSSf(0) * pp.AUX_MMM(xf(0)) / 1000
                            .Phases(0).Properties.molarflow = LSSf(0)
                            .Phases(0).Properties.temperature = Tf(0)
                            .Phases(0).Properties.pressure = P(0)
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
                            .Phases(3).Properties.molarfraction = 1.0
                            .CopyCompositions(PhaseLabel.Mixture, PhaseLabel.Liquid1)
                            .AtEquilibrium = True
                        End With
                    Case StreamInformation.Behavior.OverheadVapor
                        msm = FlowSheet.SimulationObjects(sinf.StreamID)
                        With msm
                            .Clear()
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
                            .CopyCompositions(PhaseLabel.Mixture, PhaseLabel.Vapor)
                            .Phases(2).Properties.molarfraction = 1.0
                            .AtEquilibrium = True
                        End With
                    Case StreamInformation.Behavior.BottomsLiquid
                        msm = FlowSheet.SimulationObjects(sinf.StreamID)
                        With msm
                            .Clear()
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
                            .Phases(3).Properties.molarfraction = 1.0
                            .CopyCompositions(PhaseLabel.Mixture, PhaseLabel.Liquid1)
                            .AtEquilibrium = True
                        End With
                    Case StreamInformation.Behavior.Sidedraw
                        Dim sidx As Integer = StageIndex(sinf.AssociatedStage)
                        msm = FlowSheet.SimulationObjects(sinf.StreamID)
                        If sinf.StreamPhase = StreamInformation.Phase.L Or sinf.StreamPhase = StreamInformation.Phase.B Then
                            With msm
                                .Clear()
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
                                .Phases(3).Properties.molarfraction = 1.0
                                .CopyCompositions(PhaseLabel.Mixture, PhaseLabel.Liquid1)
                                .AtEquilibrium = True
                            End With
                        ElseIf sinf.StreamPhase = StreamInformation.Phase.V Then
                            With msm
                                .Clear()
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
                                .CopyCompositions(PhaseLabel.Mixture, PhaseLabel.Vapor)
                                .Phases(2).Properties.molarfraction = 1.0
                                .AtEquilibrium = True
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

Namespace UnitOperations.Auxiliary.SepOps.SolvingMethods

    <System.Serializable()> Public Class WangHenkeMethod

        Public Shared Function Solve(ByVal rc As Column, ByVal nc As Integer, ByVal ns As Integer, ByVal maxits As Integer,
                                ByVal tol As Double(), ByVal F As Double(), ByVal V As Double(),
                                ByVal Q As Double(), ByVal L As Double(),
                                ByVal VSS As Double(), ByVal LSS As Double(), ByVal Kval()() As Double,
                                ByVal x()() As Double, ByVal y()() As Double, ByVal z()() As Double,
                                ByVal fc()() As Double,
                                ByVal HF As Double(), ByVal T As Double(), ByVal P As Double(),
                                ByVal condt As DistillationColumn.condtype,
                                ByVal stopatitnumber As Integer,
                                ByVal eff() As Double,
                                ByVal coltype As Column.ColType,
                                ByVal pp As PropertyPackages.PropertyPackage,
                                ByVal specs As Dictionary(Of String, SepOps.ColumnSpec),
                                ByVal IdealK As Boolean, ByVal IdealH As Boolean) As Object

            Dim flashalgs As New List(Of FlashAlgorithm)

            For ia As Integer = 0 To ns
                flashalgs.Add(pp.FlashBase)
            Next

            Dim spval1, spval2 As Double
            Dim spci1, spci2 As Integer

            Select Case specs("C").SType
                Case ColumnSpec.SpecType.Component_Fraction,
                      ColumnSpec.SpecType.Stream_Ratio,
                      ColumnSpec.SpecType.Component_Recovery
                    spval1 = specs("C").SpecValue
                Case Else
                    spval1 = SystemsOfUnits.Converter.ConvertToSI(specs("C").SpecUnit, specs("C").SpecValue)
            End Select
            spci1 = specs("C").ComponentIndex
            Select Case specs("C").SType
                Case ColumnSpec.SpecType.Component_Fraction,
                      ColumnSpec.SpecType.Stream_Ratio,
                      ColumnSpec.SpecType.Component_Recovery
                    spval2 = specs("R").SpecValue
                Case Else
                    spval2 = SystemsOfUnits.Converter.ConvertToSI(specs("R").SpecUnit, specs("R").SpecValue)
            End Select
            spci2 = specs("R").ComponentIndex

            Dim specC_OK, specR_OK As Boolean

            Select Case specs("C").SType
                Case ColumnSpec.SpecType.Component_Fraction,
                ColumnSpec.SpecType.Component_Mass_Flow_Rate,
                ColumnSpec.SpecType.Component_Molar_Flow_Rate,
                ColumnSpec.SpecType.Component_Recovery,
                ColumnSpec.SpecType.Temperature
                    specC_OK = False
                Case Else
                    specC_OK = True
            End Select

            Select Case specs("R").SType
                Case ColumnSpec.SpecType.Component_Fraction,
                ColumnSpec.SpecType.Component_Mass_Flow_Rate,
                ColumnSpec.SpecType.Component_Molar_Flow_Rate,
                ColumnSpec.SpecType.Component_Recovery,
                ColumnSpec.SpecType.Temperature
                    specR_OK = False
                Case Else
                    specR_OK = True
            End Select

            If DirectCast(rc, DistillationColumn).ReboiledAbsorber Then specC_OK = True
            If DirectCast(rc, DistillationColumn).RefluxedAbsorber Then specR_OK = True

            Dim ObjFunctionValues As New List(Of Double)
            Dim ResultsVector As New List(Of Object)

            Dim altmode As Boolean = False

            If Not specC_OK And Not specR_OK Then

                Dim refluxratio As Double = 0.0
                If specs("C").InitialEstimate.HasValue Then
                    refluxratio = specs("C").InitialEstimate.GetValueOrDefault()
                Else
                    If condt <> Column.condtype.Full_Reflux Then
                        refluxratio = (L(0) + LSS(0)) / LSS(0)
                    Else
                        refluxratio = (V(1) + F(0)) / V(0) - 1
                    End If
                End If

                Dim bottomsrate As Double = L.Last

                Dim newspecs As New Dictionary(Of String, ColumnSpec)
                Dim cspec As New ColumnSpec()
                cspec.SpecValue = refluxratio
                cspec.SType = ColumnSpec.SpecType.Stream_Ratio
                Dim rspec As New ColumnSpec()
                rspec.SpecValue = bottomsrate
                rspec.SType = ColumnSpec.SpecType.Product_Molar_Flow_Rate
                newspecs.Add("C", cspec)
                newspecs.Add("R", rspec)

                Dim result As Object = Nothing

                Try
                    result = Solve_Internal(rc, nc, ns, maxits, tol, F, V, Q, L, VSS, LSS, Kval,
                                           x, y, z, fc, HF, T, P, condt, stopatitnumber, eff,
                                           coltype, pp, newspecs, IdealK, IdealH, False, flashalgs)
                    altmode = False
                Catch ex As Exception
                    altmode = True
                End Try

                If altmode Then
                    result = Solve_Internal(rc, nc, ns, maxits, tol, F, V, Q, L, VSS, LSS, Kval,
                                           x, y, z, fc, HF, T, P, condt, stopatitnumber, eff,
                                           coltype, pp, newspecs, IdealK, IdealH, True, flashalgs)
                End If

                T = result(0)
                V = result(1)
                L = result(2)
                VSS = result(3)
                LSS = result(4)
                y = result(5)
                x = result(6)
                Kval = result(7)
                Q = result(8)

                bottomsrate = L.Last

                Dim counter As Integer = 0

                Dim errfunc As Double = 1.0E+20

                Dim ival, ubo, lbo As New List(Of Double)

                ival.Add(refluxratio)
                ival.Add(bottomsrate)

                lbo.Add(0.01)
                lbo.Add(0.01 * F.Sum)

                ubo.Add(100.0)
                ubo.Add(F.Sum)

                Dim fbody = Function(xvars)

                                cspec.SpecValue = xvars(0)
                                rspec.SpecValue = xvars(1)

                                If cspec.SpecValue < 0 Then Return 1.0E+20

                                Try
                                    result = Solve_Internal(rc, nc, ns, maxits, tol, F, V, Q, L, VSS, LSS, Kval,
                                    x, y, z, fc, HF, T, P, condt, stopatitnumber, eff,
                                    coltype, pp, newspecs, IdealK, IdealH, False, flashalgs)
                                    altmode = False
                                Catch ex As Exception
                                    altmode = True
                                End Try

                                If altmode Then
                                    result = Solve_Internal(rc, nc, ns, maxits, tol, F, V, Q, L, VSS, LSS, Kval,
                                    x, y, z, fc, HF, T, P, condt, stopatitnumber, eff,
                                    coltype, pp, newspecs, IdealK, IdealH, True, flashalgs)
                                End If

                                'Return New Object() {Tj, Vj, Lj, VSSj, LSSj, yc, xc, K, Q, ic, t_error}

                                errfunc = 0.0

                                Dim T2 = result(0)
                                Dim V2 = result(1)
                                Dim L2 = result(2)
                                Dim VSS2 = result(3)
                                Dim LSS2 = result(4)
                                Dim y2 = result(5)
                                Dim x2 = result(6)
                                Dim Kval2 = result(7)
                                Dim Q2 = result(8)

                                Select Case specs("C").SType
                                    Case ColumnSpec.SpecType.Component_Fraction
                                        If condt <> Column.condtype.Full_Reflux Then
                                            If specs("C").SpecUnit = "M" Or specs("C").SpecUnit = "Molar" Then
                                                errfunc += Log(x2(0)(spci1) / spval1) ^ 2
                                                specs("C").CalculatedValue = x2(0)(spci1)
                                            Else 'W
                                                errfunc += Log((pp.AUX_CONVERT_MOL_TO_MASS(x2(0))(spci1)) / spval1) ^ 2
                                                specs("C").CalculatedValue = pp.AUX_CONVERT_MOL_TO_MASS(x2(0))(spci1)
                                            End If
                                        Else
                                            If specs("C").SpecUnit = "M" Or specs("C").SpecUnit = "Molar" Then
                                                errfunc += Log((y2(0)(spci1)) / spval1) ^ 2
                                                specs("C").CalculatedValue = y2(0)(spci1)
                                            Else 'W
                                                errfunc += Log((pp.AUX_CONVERT_MOL_TO_MASS(y2(0))(spci1)) / spval1) ^ 2
                                                specs("C").CalculatedValue = pp.AUX_CONVERT_MOL_TO_MASS(y2(0))(spci1)
                                            End If
                                        End If
                                    Case ColumnSpec.SpecType.Component_Mass_Flow_Rate
                                        If condt <> Column.condtype.Full_Reflux Then
                                            errfunc += Log((LSS2(0) * x2(0)(spci1) * pp.RET_VMM()(spci1) / 1000) / spval1) ^ 2
                                            specs("C").CalculatedValue = LSS2(0) * x2(0)(spci1) * pp.RET_VMM()(spci1) / 1000
                                        Else
                                            errfunc += Log((V2(0) * y2(0)(spci1) * pp.RET_VMM()(spci1) / 1000) / spval1) ^ 2
                                            specs("C").CalculatedValue = V2(0) * y2(0)(spci1) * pp.RET_VMM()(spci1) / 1000
                                        End If
                                    Case ColumnSpec.SpecType.Component_Molar_Flow_Rate
                                        If condt <> Column.condtype.Full_Reflux Then
                                            errfunc += Log((LSS2(0) * x2(0)(spci1)) / spval1) ^ 2
                                            specs("C").CalculatedValue = LSS2(0) * x2(0)(spci1)
                                        Else
                                            errfunc += Log((V2(0) * y2(0)(spci1)) / spval1) ^ 2
                                            specs("C").CalculatedValue = V2(0) * y2(0)(spci1)
                                        End If
                                    Case ColumnSpec.SpecType.Component_Recovery
                                        Dim rec As Double = spval1 / 100
                                        Dim sumc As Double = 0
                                        For j = 0 To ns
                                            sumc += z(j)(spci1) * F(j)
                                        Next
                                        If condt <> Column.condtype.Full_Reflux Then
                                            errfunc += Log(LSS2(0) * x2(0)(spci1) / sumc / rec) ^ 2
                                            specs("C").CalculatedValue = LSS2(0) * x2(0)(spci1) / sumc * 100
                                        Else
                                            errfunc += Log(V2(0) * y2(0)(spci1) / sumc / rec) ^ 2
                                            specs("C").CalculatedValue = V2(0) * y2(0)(spci1) / sumc * 100
                                        End If
                                    Case ColumnSpec.SpecType.Temperature
                                        errfunc += Log((T2(0)) / spval1) ^ 2
                                        specs("C").CalculatedValue = T2(0)
                                End Select

                                Select Case specs("R").SType
                                    Case ColumnSpec.SpecType.Component_Fraction
                                        If specs("R").SpecUnit = "M" Or specs("R").SpecUnit = "Molar" Then
                                            errfunc += Log((x2(ns)(spci2)) / spval2) ^ 2
                                            specs("R").CalculatedValue = x2(ns)(spci1)
                                        Else 'W
                                            errfunc += Log((pp.AUX_CONVERT_MOL_TO_MASS(x2(ns))(spci2)) / spval2) ^ 2
                                            specs("R").CalculatedValue = pp.AUX_CONVERT_MOL_TO_MASS(x2(ns))(spci2)
                                        End If
                                    Case ColumnSpec.SpecType.Component_Mass_Flow_Rate
                                        errfunc += Log((L2(ns) * x2(ns)(spci2) * pp.RET_VMM()(spci2) / 1000) / spval2) ^ 2
                                        specs("R").CalculatedValue = L2(ns) * x2(ns)(spci2) * pp.RET_VMM()(spci2) / 1000
                                    Case ColumnSpec.SpecType.Component_Molar_Flow_Rate
                                        errfunc += Log((L2(ns) * x2(ns)(spci2)) / spval2) ^ 2
                                        specs("R").CalculatedValue = L2(ns) * x2(ns)(spci2)
                                    Case ColumnSpec.SpecType.Component_Recovery
                                        Dim rec As Double = spval2 / 100
                                        Dim sumc As Double = 0
                                        For j = 0 To ns
                                            sumc += z(j)(spci2) * F(j)
                                        Next
                                        errfunc += Log(L2(ns) * x2(ns)(spci2) / sumc / rec) ^ 2
                                        specs("R").CalculatedValue = L2(ns) * x2(ns)(spci2) / sumc * 100
                                    Case ColumnSpec.SpecType.Temperature
                                        errfunc += Log(T2(ns) / spval2) ^ 2
                                        specs("R").CalculatedValue = T2(ns)
                                End Select

                                counter += 1

                                If Math.IEEERemainder(counter, 10) = 0.0 Then
                                    pp.Flowsheet?.ShowMessage(String.Format("BP solver: external iteration #{0}, current objective function (error) value = {1}", counter, errfunc), IFlowsheet.MessageType.Information)
                                End If

                                ResultsVector.Add(result)
                                ObjFunctionValues.Add(errfunc)

                                Return errfunc

                            End Function

                Dim simplex As New Simplex()

                Dim vars As New List(Of OptSimplexBoundVariable)
                vars.Add(New OptSimplexBoundVariable(refluxratio, 0.01, 100.0))
                vars.Add(New OptSimplexBoundVariable(bottomsrate, 0.01 * F.Sum, F.Sum))

                simplex.MaxFunEvaluations = maxits
                simplex.Tolerance = tol(1) / 100
                simplex.ComputeMin(Function(xv0)
                                       If Settings.CalculatorStopRequested Then Return errfunc
                                       If errfunc < simplex.Tolerance Then Return errfunc
                                       Return fbody.Invoke(xv0)
                                   End Function, vars.ToArray())

                If Double.IsNaN(errfunc) Or errfunc > tol(1) Then Throw New Exception(pp.Flowsheet?.GetTranslatedString("DCGeneralError"))

                result = ResultsVector(ObjFunctionValues.IndexOf(ObjFunctionValues.Min))

                pp.Flowsheet?.ShowMessage(String.Format("BP solver: converged at external iteration #{0}, final objective function (error) value = {1}", counter, errfunc), IFlowsheet.MessageType.Information)

                Return result

            ElseIf Not specC_OK Then

                Dim refluxratio As Double = 0.0
                If specs("C").InitialEstimate.HasValue Then
                    refluxratio = specs("C").InitialEstimate.GetValueOrDefault()
                Else
                    If condt <> Column.condtype.Full_Reflux Then
                        refluxratio = (L(0) + LSS(0)) / LSS(0)
                    Else
                        refluxratio = (V(1) + F(0)) / V(0) - 1
                    End If
                End If

                Dim newspecs As New Dictionary(Of String, ColumnSpec)
                Dim cspec As New ColumnSpec()
                cspec.SpecValue = refluxratio
                cspec.SType = ColumnSpec.SpecType.Stream_Ratio

                newspecs.Add("C", cspec)
                newspecs.Add("R", specs("R"))

                Dim result As Object = Nothing

                Dim counter As Integer = 0

                Dim errfunc As Double = 1.0E+20

                Dim simplex As New Simplex()

                Dim vars As New List(Of OptSimplexBoundVariable)
                vars.Add(New OptSimplexBoundVariable(refluxratio, 0.01, 100.0))

                simplex.MaxFunEvaluations = maxits
                simplex.Tolerance = tol(1) / 100
                simplex.ComputeMin(Function(xvars)

                                       If errfunc < simplex.Tolerance Then Return errfunc

                                       If Settings.CalculatorStopRequested Then Return errfunc

                                       cspec.SpecValue = xvars(0)

                                       If cspec.SpecValue < 0 Then Return 1.0E+20

                                       Try
                                           result = Solve_Internal(rc, nc, ns, maxits, tol, F, V, Q, L, VSS, LSS, Kval,
                                                x, y, z, fc, HF, T, P, condt, stopatitnumber, eff,
                                                coltype, pp, newspecs, IdealK, IdealH, False, flashalgs)
                                           altmode = False
                                       Catch ex As Exception
                                           altmode = True
                                       End Try

                                       If altmode Then
                                           result = Solve_Internal(rc, nc, ns, maxits, tol, F, V, Q, L, VSS, LSS, Kval,
                                           x, y, z, fc, HF, T, P, condt, stopatitnumber, eff,
                                           coltype, pp, newspecs, IdealK, IdealH, True, flashalgs)
                                       End If

                                       'Return New Object() {Tj, Vj, Lj, VSSj, LSSj, yc, xc, K, Q, ic, t_error}

                                       errfunc = 0.0

                                       Dim T2 = result(0)
                                       Dim V2 = result(1)
                                       Dim L2 = result(2)
                                       Dim VSS2 = result(3)
                                       Dim LSS2 = result(4)
                                       Dim y2 = result(5)
                                       Dim x2 = result(6)
                                       Dim Kval2 = result(7)
                                       Dim Q2 = result(8)

                                       Select Case specs("C").SType
                                           Case ColumnSpec.SpecType.Component_Fraction
                                               If condt <> Column.condtype.Full_Reflux Then
                                                   If specs("C").SpecUnit = "M" Or specs("C").SpecUnit = "Molar" Then
                                                       errfunc += Log(x2(0)(spci1) / spval1) ^ 2
                                                       specs("C").CalculatedValue = x2(0)(spci1)
                                                   Else 'W
                                                       errfunc += Log((pp.AUX_CONVERT_MOL_TO_MASS(x2(0))(spci1)) / spval1) ^ 2
                                                       specs("C").CalculatedValue = pp.AUX_CONVERT_MOL_TO_MASS(x2(0))(spci1)
                                                   End If
                                               Else
                                                   If specs("C").SpecUnit = "M" Or specs("C").SpecUnit = "Molar" Then
                                                       errfunc += Log((y2(0)(spci1)) / spval1) ^ 2
                                                       specs("C").CalculatedValue = y2(0)(spci1)
                                                   Else 'W
                                                       errfunc += Log((pp.AUX_CONVERT_MOL_TO_MASS(y2(0))(spci1)) / spval1) ^ 2
                                                       specs("C").CalculatedValue = pp.AUX_CONVERT_MOL_TO_MASS(y2(0))(spci1)
                                                   End If
                                               End If
                                           Case ColumnSpec.SpecType.Component_Mass_Flow_Rate
                                               If condt <> Column.condtype.Full_Reflux Then
                                                   errfunc += Log((LSS2(0) * x2(0)(spci1) * pp.RET_VMM()(spci1) / 1000) / spval1) ^ 2
                                                   specs("C").CalculatedValue = LSS2(0) * x2(0)(spci1) * pp.RET_VMM()(spci1) / 1000
                                               Else
                                                   errfunc += Log((V2(0) * y2(0)(spci1) * pp.RET_VMM()(spci1) / 1000) / spval1) ^ 2
                                                   specs("C").CalculatedValue = V2(0) * y2(0)(spci1) * pp.RET_VMM()(spci1) / 1000
                                               End If
                                           Case ColumnSpec.SpecType.Component_Molar_Flow_Rate
                                               If condt <> Column.condtype.Full_Reflux Then
                                                   errfunc += Log((LSS2(0) * x2(0)(spci1)) / spval1) ^ 2
                                                   specs("C").CalculatedValue = LSS2(0) * x2(0)(spci1)
                                               Else
                                                   errfunc += Log((V2(0) * y2(0)(spci1)) / spval1) ^ 2
                                                   specs("C").CalculatedValue = V2(0) * y2(0)(spci1)
                                               End If
                                           Case ColumnSpec.SpecType.Component_Recovery
                                               Dim rec As Double = spval1 / 100
                                               Dim sumc As Double = 0
                                               For j = 0 To ns
                                                   sumc += z(j)(spci1) * F(j)
                                               Next
                                               If condt <> Column.condtype.Full_Reflux Then
                                                   errfunc += Log(LSS2(0) * x2(0)(spci1) / sumc / rec) ^ 2
                                                   specs("C").CalculatedValue = LSS2(0) * x2(0)(spci1) / sumc * 100
                                               Else
                                                   errfunc += Log(V2(0) * y2(0)(spci1) / sumc / rec) ^ 2
                                                   specs("C").CalculatedValue = V2(0) * y2(0)(spci1) / sumc * 100
                                               End If
                                           Case ColumnSpec.SpecType.Temperature
                                               errfunc += Log((T2(0)) / spval1) ^ 2
                                               specs("C").CalculatedValue = T2(0)
                                       End Select

                                       counter += 1

                                       If Math.IEEERemainder(counter, 10) = 0.0 Then
                                           pp.Flowsheet?.ShowMessage(String.Format("BP solver: external iteration #{0}, current objective function (error) value = {1}", counter, errfunc), IFlowsheet.MessageType.Information)
                                       End If

                                       ResultsVector.Add(result)
                                       ObjFunctionValues.Add(errfunc)

                                       Return errfunc

                                   End Function, vars.ToArray())

                If Double.IsNaN(errfunc) Or errfunc > tol(1) Then Throw New Exception(pp.Flowsheet?.GetTranslatedString("DCGeneralError"))

                result = ResultsVector(ObjFunctionValues.IndexOf(ObjFunctionValues.Min))

                pp.Flowsheet?.ShowMessage(String.Format("BP solver: converged at external iteration #{0}, final objective function (error) value = {1}", counter, errfunc), IFlowsheet.MessageType.Information)

                Return result

            ElseIf Not specR_OK Then

                Dim bottomsrate As Double = L.Last

                Dim newspecs As New Dictionary(Of String, ColumnSpec)
                Dim rspec As New ColumnSpec()
                rspec.SpecValue = bottomsrate
                rspec.SType = ColumnSpec.SpecType.Product_Molar_Flow_Rate
                newspecs.Add("C", specs("C"))
                newspecs.Add("R", rspec)

                Dim result As Object = Nothing

                Dim counter As Integer = 0

                Dim errfunc As Double = 1.0E+20

                Dim bmin As New BrentMinimize()
                bmin.brentoptimize2(F.Sum * 0.05, F.Sum, tol(1) / 100,
                                    Function(xvar)

                                        rspec.SpecValue = xvar

                                        Try
                                            result = Solve_Internal(rc, nc, ns, maxits, tol, F, V, Q, L, VSS, LSS, Kval,
                                                x, y, z, fc, HF, T, P, condt, stopatitnumber, eff,
                                                coltype, pp, newspecs, IdealK, IdealH, False, flashalgs)
                                            altmode = False
                                        Catch ex As Exception
                                            altmode = True
                                        End Try

                                        If altmode Then
                                            result = Solve_Internal(rc, nc, ns, maxits, tol, F, V, Q, L, VSS, LSS, Kval,
                                           x, y, z, fc, HF, T, P, condt, stopatitnumber, eff,
                                           coltype, pp, newspecs, IdealK, IdealH, True, flashalgs)
                                        End If

                                        'Return New Object() {Tj, Vj, Lj, VSSj, LSSj, yc, xc, K, Q, ic, t_error}

                                        errfunc = 0.0

                                        Dim T2 = result(0)
                                        Dim V2 = result(1)
                                        Dim L2 = result(2)
                                        Dim VSS2 = result(3)
                                        Dim LSS2 = result(4)
                                        Dim y2 = result(5)
                                        Dim x2 = result(6)
                                        Dim Kval2 = result(7)
                                        Dim Q2 = result(8)

                                        Select Case specs("R").SType
                                            Case ColumnSpec.SpecType.Component_Fraction
                                                If specs("R").SpecUnit = "M" Or specs("R").SpecUnit = "Molar" Then
                                                    errfunc += Log((x2(ns)(spci2)) / spval2) ^ 2
                                                    specs("R").CalculatedValue = x2(ns)(spci1)
                                                Else 'W
                                                    errfunc += Log((pp.AUX_CONVERT_MOL_TO_MASS(x2(ns))(spci2)) / spval2) ^ 2
                                                    specs("R").CalculatedValue = pp.AUX_CONVERT_MOL_TO_MASS(x2(ns))(spci2)
                                                End If
                                            Case ColumnSpec.SpecType.Component_Mass_Flow_Rate
                                                errfunc += Log((L2(ns) * x2(ns)(spci2) * pp.RET_VMM()(spci2) / 1000) / spval2) ^ 2
                                                specs("R").CalculatedValue = L2(ns) * x2(ns)(spci2) * pp.RET_VMM()(spci2) / 1000
                                            Case ColumnSpec.SpecType.Component_Molar_Flow_Rate
                                                errfunc += Log((L2(ns) * x2(ns)(spci2)) / spval2) ^ 2
                                                specs("R").CalculatedValue = L2(ns) * x2(ns)(spci2)
                                            Case ColumnSpec.SpecType.Component_Recovery
                                                Dim rec As Double = spval2 / 100
                                                Dim sumc As Double = 0
                                                For j = 0 To ns
                                                    sumc += z(j)(spci2) * F(j)
                                                Next
                                                errfunc += Log(L2(ns) * x2(ns)(spci2) / sumc / rec) ^ 2
                                                specs("R").CalculatedValue = L2(ns) * x2(ns)(spci2) / sumc * 100
                                            Case ColumnSpec.SpecType.Temperature
                                                errfunc += Log(T2(ns) / spval2) ^ 2
                                                specs("R").CalculatedValue = T2(ns)
                                        End Select

                                        counter += 1

                                        If Math.IEEERemainder(counter, 10) = 0.0 Then
                                            pp.Flowsheet?.ShowMessage(String.Format("BP solver: external iteration #{0}, current objective function (error) value = {1}", counter, errfunc), IFlowsheet.MessageType.Information)
                                        End If

                                        ResultsVector.Add(result)
                                        ObjFunctionValues.Add(errfunc)

                                        Return errfunc

                                    End Function)

                If Double.IsNaN(errfunc) Or errfunc > tol(1) Then Throw New Exception(pp.Flowsheet?.GetTranslatedString("DCGeneralError"))

                result = ResultsVector(ObjFunctionValues.IndexOf(ObjFunctionValues.Min))

                pp.Flowsheet?.ShowMessage(String.Format("BP solver: converged at external iteration #{0}, final objective function (error) value = {1}", counter, errfunc), IFlowsheet.MessageType.Information)

                Return result

            Else

                Try
                    Return Solve_Internal(rc, nc, ns, maxits, tol, F, V, Q, L, VSS, LSS, Kval, x, y, z, fc, HF, T, P, condt,
                                          stopatitnumber, eff, coltype, pp, specs, IdealK, IdealH, False, flashalgs)
                    altmode = False
                Catch ex As Exception
                    altmode = True
                End Try

                If altmode Then
                    Return Solve_Internal(rc, nc, ns, maxits, tol, F, V, Q, L, VSS, LSS, Kval, x, y, z, fc, HF, T, P, condt,
                                          stopatitnumber, eff, coltype, pp, specs, IdealK, IdealH, True, flashalgs)
                End If

            End If

        End Function

        Public Shared Function Solve_Internal(ByVal rc As Column, ByVal nc As Integer, ByVal ns As Integer, ByVal maxits As Integer,
                                ByVal tol As Double(), ByVal F As Double(), ByVal V As Double(),
                                ByVal Q As Double(), ByVal L As Double(),
                                ByVal VSS As Double(), ByVal LSS As Double(), ByVal Kval()() As Double,
                                ByVal x()() As Double, ByVal y()() As Double, ByVal z()() As Double,
                                ByVal fc()() As Double,
                                ByVal HF As Double(), ByVal T As Double(), ByVal P As Double(),
                                ByVal condt As DistillationColumn.condtype,
                                ByVal stopatitnumber As Integer,
                                ByVal eff() As Double,
                                ByVal coltype As Column.ColType,
                                ByVal pp As PropertyPackages.PropertyPackage,
                                ByVal specs As Dictionary(Of String, SepOps.ColumnSpec),
                                ByVal IdealK As Boolean, ByVal IdealH As Boolean, ByVal AlternateMode As Boolean,
                                ByVal flashalgs As List(Of FlashAlgorithm)) As Object

            pp.CurrentMaterialStream.Flowsheet.CheckStatus()

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

            Inspector.Host.CheckAndAdd(IObj, "", "Solve", "Bubble-Point (BP) Method", "Wang-Henke Bubble-Point (BP) Method for Distillation Columns", True)

            IObj?.SetCurrent()

            IObj?.Paragraphs.Add("Frequently, distillation involves species that cover a relatively narrow range of K-values. A particularly effective procedure for this case was suggested by Friday and Smith and developed in detail by Wang and Henke. It is referred to as the bubble-point (BP) method because a new set of stage temperatures is computed during each iteration from the bubble-point equations. All equations are partitioned and solved sequentially except for the M equations, which are solved separately for each component by the tridiagonal-matrix technique.")

            IObj?.Paragraphs.Add("Specifications are conditions and stage location of feeds, stage pressures, flow rates of sidestreams (note that liquid distillate flow rate, if any, is designated as U1), heat-transfer rates for all stages except stage 1 (condenser) and stage N (reboiler), total stages, bubble-point reflux flow rate, and vapor distillate flow rate.")

            IObj?.Paragraphs.Add("To initiate the calculations, values of tear variables, Vj and Tj, are assumed. Generally, it is sufficient to establish an initial set of Vj values based on constant-molar interstage flows using the specified reflux, distillate, feed, and sidestream flows. Initial Tj values can be provided by computing the bubble-point temperature of an estimated bottoms product and the dew-point temperature of an assumed distillate product (or computing a bubble-point temperature if distillate is liquid, or a temperature in between the dew point and bubble point if distillate is both vapor and liquid), and then using linear interpolation for the other stage temperatures.")

            IObj?.Paragraphs.Add("To solve the Tridiagonal Matrix for <mi>x_{i}</mi> by the Thomas method, <mi>K_{i,j}</mi> values are required. When they are composition-dependent, initial assumptions for all <mi>x_{i,j}</mi> and <mi>y_{i,j}</mi> values are also needed, unless ideal K-values are employed initially. For each iteration, the computed set of <mi>x_{i,j}</mi> values for each stage are not likely to satisfy the summation constraint. Although not mentioned by Wang and Henke, it is advisable to normalize the set of computed <mi>x_{i,j}</mi> values by the relation")

            IObj?.Paragraphs.Add("<m>(x_{i,j})_{normalized}=\frac{x_{i,j}}{\sum\limits_{i=1}^{C}{x_{i,j}} }</m>")

            IObj?.Paragraphs.Add("New temperatures for the stages are obtained by bubble point calculations using normalized <mi>x_{i,j}</mi> values.")

            IObj?.Paragraphs.Add("Values of <mi>y_{i,j}</mi> are determined along with the calculation of stage temperatures using the E equations. With a consistent set of values for <mi>x_{i,j}</mi>, Tj, and <mi>y_{i,j}</mi>, molar enthalpies are computed for each liquid and vapor stream leaving a stage. Since F1, V1, U1, W1, and L1 are specified, V2 and the condenser duty are readily obtained. Reboiler duty is determined by")

            IObj?.Paragraphs.Add("<m>Q_N=\sum\limits_{j=1}^{N}{(F_jh_{F_j}-U_jh_{L_j}-W_jh_{V_j})}-\sum\limits_{j=1}^{N-1}{(Q_j-V_1h_{V_1}-L_Nh_{L_N})}</m>")

            IObj?.Paragraphs.Add("A new set of Vj tear variables is computed by applying a modified energy balance obtained by")

            IObj?.Paragraphs.Add("<m>\alpha _jV_j+\beta _jV_{j+1}=\gamma _j</m>")

            IObj?.Paragraphs.Add("where")

            IObj?.Paragraphs.Add("<m>\alpha _j = h_{L_{j-1}}-h_{V_j}</m>")

            IObj?.Paragraphs.Add("<m>\beta _j=h_{V_{j+1}}-h_{L_j}</m>")

            IObj?.Paragraphs.Add("<m>\gamma _j =\left[\sum\limits_{m=1}^{j-1}{(F_m-W_m-U_m)-V_1}\right](h_{L_j}-h_{L_{j=1}})+F_j(h_{L_j}-h_{F_j})+W_j(h_{V_j}-h_{L_j})+Q_j</m>")

            IObj?.Paragraphs.Add("and enthalpies are evaluated at the stage temperatures last computed rather than at those used to initiate the iteration.")

            IObj?.Paragraphs.Add("<m>V_j=\frac{\gamma _{j-1}-\alpha _{j-1}V_{j-1}}{\beta _{j-1}} </m>")

            IObj?.Paragraphs.Add("Corresponding liquid flow rates are obtained from")

            IObj?.Paragraphs.Add("<m>L_j=V_{j+1}+\sum\limits_{m=1}^{j}{(F_m-U_m-W_m)-V_1} </m>")

            IObj?.Paragraphs.Add("One convergence criterion is")

            IObj?.Paragraphs.Add("<m>\sum\limits_{j=1}^{N}{\left[\frac{T_j^{(k)}-T_j^{(k-1)}}{T_j^{(k)}}\right]^2+\sum\limits_{j=1}^{N}{\left[\frac{V_j^{(k)}-V_j^{(k-1)}}{V_j^{(k)}} \right]^2 }}\leq \in</m>")

            IObj?.Paragraphs.Add("where T is an absolute temperature and <mi>\in</mi> is some prescribed tolerance. However, Wang and Henke suggest that the following simpler criterion, which is based on successive sets of Tj values only, is adequate.")

            IObj?.Paragraphs.Add("Successive substitution is often employed for iterating the tear variables; that is, values of Tj and Vj are used directly to initiate the next iteration. It is desirable to inspect, and, if necessary, adjust the generated tear variables prior to beginning the next iteration.")

            IObj?.Paragraphs.Add("The BP convergence rate is unpredictable, and can depend on the assumed initial set of Tj values. Cases with high reflux ratios can be more difficult to converge than those with low ratios. Orbach and Crowe describe an extrapolation method for accelerating convergence based on periodic adjustment of the tear variables when their values form geometric progressions during at least four successive iterations.")

            IObj?.Paragraphs.Add("<h2>Input Parameters / Initial Estimates</h2>")

            IObj?.Paragraphs.Add(String.Format("Stage Temperatures: {0}", T.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Stage Pressures: {0}", P.ToMathArrayString))

            IObj?.Paragraphs.Add(String.Format("Feeds: {0}", F.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Vapor Flows: {0}", V.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Liquid Flows: {0}", L.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Vapor Side Draws: {0}", VSS.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Liquid Side Draws: {0}", LSS.ToMathArrayString))

            IObj?.Paragraphs.Add(String.Format("Mixture Compositions: {0}", z.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Liquid Phase Compositions: {0}", x.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Vapor/Liquid2 Phase Compositions: {0}", y.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("K-values: {0}", Kval.ToMathArrayString))

            IObj?.Paragraphs.Add("<h2>Calculated Parameters</h2>")

            Dim ppr As PropertyPackages.RaoultPropertyPackage = Nothing

            If IdealK Or IdealH Then
                ppr = New PropertyPackages.RaoultPropertyPackage
                ppr.Flowsheet = pp.Flowsheet
                ppr.CurrentMaterialStream = pp.CurrentMaterialStream
            End If

            Dim doparallel As Boolean = Settings.EnableParallelProcessing
            Dim poptions As New ParallelOptions() With {.TaskScheduler = Settings.AppTaskScheduler}

            Dim ic As Integer
            Dim t_error, t_error_ant, xcerror(ns) As Double
            Dim Tj(ns), Tj_ant(ns), dTj(ns) As Double
            Dim Fj(ns), Lj(ns), Vj(ns), Vj_ant(ns), dVj(ns), xc(ns)(), xc0(ns)(), fcj(ns)(), yc(ns)(), lc(ns)(), vc(ns)(), zc(ns)(), K(ns)(), Kant(ns)() As Double
            Dim Hfj(ns), Hv(ns), Hl(ns) As Double
            Dim VSSj(ns), LSSj(ns) As Double

            Dim rebabs As Boolean = False, refabs As Boolean = False

            If DirectCast(rc, DistillationColumn).ReboiledAbsorber Then rebabs = True
            If DirectCast(rc, DistillationColumn).RefluxedAbsorber Then refabs = True

            'step0

            Dim cv As New SystemsOfUnits.Converter
            Dim spval1, spval2 As Double

            spval1 = specs("C").SpecValue
            spval2 = specs("R").SpecValue

            Select Case specs("C").SType
                Case ColumnSpec.SpecType.Temperature,
                      ColumnSpec.SpecType.Product_Molar_Flow_Rate,
                      ColumnSpec.SpecType.Product_Mass_Flow_Rate,
                      ColumnSpec.SpecType.Heat_Duty,
                      ColumnSpec.SpecType.Component_Molar_Flow_Rate,
                      ColumnSpec.SpecType.Component_Mass_Flow_Rate
                    spval1 = spval1.ConvertToSI(specs("C").SpecUnit)
            End Select

            Select Case specs("R").SType
                Case ColumnSpec.SpecType.Temperature,
                      ColumnSpec.SpecType.Product_Molar_Flow_Rate,
                      ColumnSpec.SpecType.Product_Mass_Flow_Rate,
                      ColumnSpec.SpecType.Heat_Duty,
                      ColumnSpec.SpecType.Component_Molar_Flow_Rate,
                      ColumnSpec.SpecType.Component_Mass_Flow_Rate
                    spval2 = spval2.ConvertToSI(specs("R").SpecUnit)
            End Select

            'step1

            Dim rr, B, D2 As Double

            'step2

            Dim i, j As Integer

            For i = 0 To ns
                Array.Resize(fcj(i), nc)
                Array.Resize(xc(i), nc)
                Array.Resize(xc0(i), nc)
                Array.Resize(yc(i), nc)
                Array.Resize(lc(i), nc)
                Array.Resize(vc(i), nc)
                Array.Resize(zc(i), nc)
                Array.Resize(zc(i), nc)
                Array.Resize(K(i), nc)
                Array.Resize(Kant(i), nc)
            Next

            For i = 0 To ns
                VSSj(i) = VSS(i)
                LSSj(i) = LSS(i)
                Lj(i) = L(i)
                Vj(i) = V(i)
                Tj(i) = T(i)
                K(i) = Kval(i)
                Fj(i) = F(i)
                Hfj(i) = HF(i) / 1000
                fcj(i) = fc(i)
            Next

            Dim sumFHF As Double = 0
            Dim sumF As Double = 0
            Dim sumLSSHl As Double = 0
            Dim sumLSS As Double = 0
            Dim sumVSSHv As Double = 0
            Dim sumVSS As Double = 0
            For i = 0 To ns
                sumF += F(i)
                If i > 0 Then sumLSS += LSS(i)
                sumVSS += VSS(i)
                sumFHF += Fj(i) * Hfj(i)
            Next

            If doparallel Then

                Dim task1 As Task = Task.Factory.StartNew(Sub() Parallel.For(0, ns + 1, poptions,
                                                         Sub(ipar)
                                                             If IdealH Then
                                                                 Hl(ipar) = ppr.DW_CalcEnthalpy(x(ipar), Tj(ipar), P(ipar), PropertyPackages.State.Liquid) * ppr.AUX_MMM(x(ipar)) / 1000
                                                                 Hv(ipar) = ppr.DW_CalcEnthalpy(y(ipar), Tj(ipar), P(ipar), PropertyPackages.State.Vapor) * ppr.AUX_MMM(y(ipar)) / 1000
                                                             Else
                                                                 Hl(ipar) = pp.DW_CalcEnthalpy(x(ipar), Tj(ipar), P(ipar), PropertyPackages.State.Liquid) * pp.AUX_MMM(x(ipar)) / 1000
                                                                 Hv(ipar) = pp.DW_CalcEnthalpy(y(ipar), Tj(ipar), P(ipar), PropertyPackages.State.Vapor) * pp.AUX_MMM(y(ipar)) / 1000
                                                             End If
                                                         End Sub),
                                                      Settings.TaskCancellationTokenSource.Token,
                                                      TaskCreationOptions.None,
                                                      Settings.AppTaskScheduler)
                task1.Wait()

            Else

                For i = 0 To ns
                    IObj?.SetCurrent
                    If IdealH Then
                        ppr.CurrentMaterialStream.Flowsheet.CheckStatus()
                        Hl(i) = ppr.DW_CalcEnthalpy(x(i), Tj(i), P(i), PropertyPackages.State.Liquid) * ppr.AUX_MMM(x(i)) / 1000
                        IObj?.SetCurrent
                        Hv(i) = ppr.DW_CalcEnthalpy(y(i), Tj(i), P(i), PropertyPackages.State.Vapor) * ppr.AUX_MMM(y(i)) / 1000
                    Else
                        pp.CurrentMaterialStream.Flowsheet.CheckStatus()
                        Hl(i) = pp.DW_CalcEnthalpy(x(i), Tj(i), P(i), PropertyPackages.State.Liquid) * pp.AUX_MMM(x(i)) / 1000
                        IObj?.SetCurrent
                        Hv(i) = pp.DW_CalcEnthalpy(y(i), Tj(i), P(i), PropertyPackages.State.Vapor) * pp.AUX_MMM(y(i)) / 1000
                    End If
                Next

            End If

            IObj?.Paragraphs.Add(String.Format("Vapor Enthalpies: {0}", Hv.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Liquid Enthalpies: {0}", Hl.ToMathArrayString))

            If Not rebabs Then
                Select Case specs("C").SType
                    Case ColumnSpec.SpecType.Product_Mass_Flow_Rate
                        LSSj(0) = spval1 / pp.AUX_MMM(x(0)) * 1000
                        rr = (Lj(0) + LSSj(0)) / LSSj(0)
                    Case ColumnSpec.SpecType.Product_Molar_Flow_Rate
                        LSSj(0) = spval1
                        rr = (Lj(0) + LSSj(0)) / LSSj(0)
                    Case ColumnSpec.SpecType.Stream_Ratio
                        rr = spval1
                    Case ColumnSpec.SpecType.Heat_Duty
                        Q(0) = spval1
                        LSSj(0) = -Lj(0) - (Q(0) - Vj(1) * Hv(1) - F(0) * Hfj(0) + (Vj(0) + VSSj(0)) * Hv(0)) / Hl(0)
                        rr = (Lj(0) + LSSj(0)) / LSSj(0)
                End Select
            Else
                LSSj(0) = 0.0
                rr = (Vj(1) + Fj(0)) / Vj(0) - 1
            End If

            If Not refabs Then
                Select Case specs("R").SType
                    Case ColumnSpec.SpecType.Product_Mass_Flow_Rate
                        B = spval2 / pp.AUX_MMM(x(ns)) * 1000
                    Case ColumnSpec.SpecType.Product_Molar_Flow_Rate
                        B = spval2
                    Case ColumnSpec.SpecType.Stream_Ratio
                        B = sumF - LSSj(0) - sumLSS - sumVSS - Vj(0)
                        Vj(ns) = B * spval2
                    Case ColumnSpec.SpecType.Heat_Duty
                        Q(ns) = spval2
                        Dim sum3, sum4, val1 As Double
                        sum3 = 0
                        sum4 = 0
                        For i = 0 To ns
                            sum3 += F(i) * Hfj(i) - LSSj(i) * Hl(i) - VSSj(i) * Hv(i)
                        Next
                        val1 = sum3 - Q(ns)
                        sum4 = 0
                        For i = 0 To ns - 1
                            sum4 += Q(i) '- Lj(ns) * Hl(ns)
                        Next
                        B = -(val1 - (sum4 - Vj(0) * Hv(0))) / Hl(ns)
                End Select
            Else
                B = Lj(ns)
            End If

            If Not rebabs Then
                If condt = Column.condtype.Full_Reflux Then
                    Vj(0) = sumF - B - sumLSS - sumVSS
                    LSSj(0) = 0.0#
                Else
                    D2 = sumF - B - sumLSS - sumVSS - Vj(0)
                    LSSj(0) = D2
                End If
            Else
                Vj(0) = sumF - B - sumLSS - sumVSS
                LSSj(0) = 0.0#
            End If

            'step3

            Dim fx(ns), xtj(ns), dfdx(ns, ns), fxb(ns), xtjb(ns), dxtj(ns) As Double

            'internal loop

            ic = 0
            Do

                IObj?.SetCurrent()

                Dim IObj2 As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

                Inspector.Host.CheckAndAdd(IObj2, "", "Solve", "Bubble-Point (BP) Internal Loop #" & ic, "Wang-Henke Bubble-Point (BP) Method for Distillation", True)

                Dim il_err As Double = 0.0#
                Dim il_err_ant As Double = 0.0#
                Dim num, denom, x0, fx0 As New ArrayList

                'step4

                'find component liquid flows by the tridiagonal matrix method

                IObj2?.Paragraphs.Add(String.Format("Find component liquid flows by the tridiagonal matrix method"))
                IObj2?.Paragraphs.Add(String.Format("Calculating TDM A, B, C, D"))

                Dim at(nc - 1)(), bt(nc - 1)(), ct(nc - 1)(), dt(nc - 1)(), xt(nc - 1)() As Double

                For i = 0 To nc - 1
                    Array.Resize(at(i), ns + 1)
                    Array.Resize(bt(i), ns + 1)
                    Array.Resize(ct(i), ns + 1)
                    Array.Resize(dt(i), ns + 1)
                    Array.Resize(xt(i), ns + 1)
                Next

                For i = 0 To ns
                    For j = 1 To nc
                        If Double.IsNaN(K(i)(j - 1)) Or Double.IsInfinity(K(i)(j - 1)) Then K(i)(j - 1) = pp.AUX_PVAPi(j - 1, Tj(i)) / P(i)
                    Next
                Next

                Dim sum1(ns), sum2(ns) As Double

                For i = 0 To ns
                    sum1(i) = 0
                    sum2(i) = 0
                    For j = 0 To i
                        sum1(i) += Fj(j) - LSSj(j) - VSSj(j)
                    Next
                    If i > 0 Then
                        For j = 0 To i - 1
                            sum2(i) += Fj(j) - LSSj(j) - VSSj(j)
                        Next
                    End If
                Next

                For i = 0 To nc - 1
                    For j = 0 To ns
                        dt(i)(j) = -Fj(j) * fcj(j)(i)
                        If j < ns Then
                            bt(i)(j) = -(Vj(j + 1) + sum1(j) - Vj(0) + LSSj(j) + (Vj(j) + VSSj(j)) * K(j)(i))
                        Else
                            bt(i)(j) = -(sum1(j) - Vj(0) + LSSj(j) + (Vj(j) + VSSj(j)) * K(j)(i))
                        End If
                        'tdma solve
                        If j < ns Then ct(i)(j) = Vj(j + 1) * K(j + 1)(i)
                        If j > 0 Then at(i)(j) = Vj(j) + sum2(j) - Vj(0)
                    Next
                Next

                'solve matrices

                IObj2?.Paragraphs.Add(String.Format("Calling TDM Solver to calculate liquid phase compositions"))

                IObj2?.SetCurrent()

                IObj2?.Paragraphs.Add(String.Format("A: {0}", at.ToMathArrayString))

                IObj2?.Paragraphs.Add(String.Format("B: {0}", bt.ToMathArrayString))

                IObj2?.Paragraphs.Add(String.Format("C: {0}", ct.ToMathArrayString))

                IObj2?.Paragraphs.Add(String.Format("D: {0}", dt.ToMathArrayString))

                'tomich

                If doparallel Then

                    Dim t1 As Task = Task.Factory.StartNew(Sub() Parallel.For(0, nc, poptions,
                                                                 Sub(ipar)
                                                                     xt(ipar) = Tomich.TDMASolve(at(ipar), bt(ipar), ct(ipar), dt(ipar))
                                                                 End Sub),
                                                      Settings.TaskCancellationTokenSource.Token,
                                                      TaskCreationOptions.None,
                                                      Settings.AppTaskScheduler)
                    t1.Wait()

                Else
                    For i = 0 To nc - 1
                        IObj2?.SetCurrent()
                        IObj2?.Paragraphs.Add(String.Format("Calling TDM Solver for Stage #{0}...", i + 1))
                        xt(i) = Tomich.TDMASolve(at(i), bt(i), ct(i), dt(i))
                    Next
                End If

                IObj2?.SetCurrent()

                IObj2?.Paragraphs.Add(String.Format("TDM solved successfully."))

                Dim sumx(ns), sumy(ns) As Double

                For i = 0 To ns
                    sumx(i) = 0
                    For j = 0 To nc - 1
                        lc(i)(j) = xt(j)(i)
                        If lc(i)(j) < 0.0# Then lc(i)(j) = 0.0000001
                        sumx(i) += lc(i)(j)
                    Next
                Next


                For i = 0 To ns
                    xcerror(i) = 0.0
                    For j = 0 To nc - 1
                        xc0(i)(j) = xc(i)(j)
                        If sumx(i) > 0.0# Then xc(i)(j) = lc(i)(j) / sumx(i) Else xc(i)(j) = yc(i)(j) / K(i)(j)
                    Next
                    xcerror(i) = xc(i).Sum - xc0(i).Sum
                Next

                For i = 0 To ns
                    Lj(i) = 0
                    For j = 0 To nc - 1
                        lc(i)(j) = xt(j)(i)
                        Lj(i) += lc(i)(j)
                    Next
                    If Lj(i) < 0.0# Then Lj(i) = 0.001
                Next

                IObj2?.Paragraphs.Add(String.Format("l: {0}", lc.ToMathArrayString))

                IObj2?.Paragraphs.Add(String.Format("L: {0}", Lj.ToMathArrayString))

                IObj2?.Paragraphs.Add(String.Format("x: {0}", xc.ToMathArrayString))

                Dim tmp As Object

                'calculate new temperatures

                For i = 0 To ns
                    Tj_ant(i) = Tj(i)
                Next

                IObj2?.Paragraphs.Add("Calculating new temperatures...")

                If doparallel Then
                    Dim t1 As Task = Task.Factory.StartNew(Sub()
                                                               Parallel.For(0, ns + 1, poptions,
                                                                 Sub(ipar)
                                                                     If IdealK Then
                                                                         Dim tmpvar As Object = flashalgs(ipar).Flash_PV(xc(ipar), P(ipar), 0.0, Tj(ipar), ppr, True, K(ipar))
                                                                         Tj(ipar) = tmpvar(4)
                                                                         Kant(ipar) = K(ipar)
                                                                         K(ipar) = tmpvar(6)
                                                                     Else
                                                                         Dim tmpvar As Object = flashalgs(ipar).Flash_PV(xc(ipar), P(ipar), 0.0, Tj(ipar), pp, True, K(ipar))
                                                                         Tj(ipar) = tmpvar(4)
                                                                         Kant(ipar) = K(ipar)
                                                                         K(ipar) = tmpvar(6)
                                                                     End If
                                                                     If Tj(ipar) < 0.0 Or Double.IsNaN(Tj(ipar)) Then
                                                                         Tj(ipar) = Tj_ant(ipar)
                                                                         K(ipar) = Kant(ipar)
                                                                     End If
                                                                 End Sub)
                                                           End Sub,
                                                      Settings.TaskCancellationTokenSource.Token,
                                                      TaskCreationOptions.None,
                                                      Settings.AppTaskScheduler)
                    t1.Wait()
                Else
                    For i = 0 To ns
                        IObj2?.SetCurrent
                        If IdealK Then
                            ppr.CurrentMaterialStream.Flowsheet.CheckStatus()
                            tmp = flashalgs(i).Flash_PV(xc(i), P(i), 0.0, Tj(i), ppr, True, K(i))
                        Else
                            pp.CurrentMaterialStream.Flowsheet.CheckStatus()
                            tmp = flashalgs(i).Flash_PV(xc(i), P(i), 0.0, Tj(i), pp, True, K(i))
                        End If
                        Tj(i) = tmp(4)
                        Kant(i) = K(i)
                        K(i) = tmp(6)
                        If Tj(i) < 0.0 Or Double.IsNaN(Tj(i)) Then
                            Tj(i) = Tj_ant(i)
                            K(i) = Kant(i)
                        End If
                    Next
                End If

                dTj = Tj.SubtractY(Tj_ant)

                fx = dTj
                xtj = Tj

                If AlternateMode Then

                    If ic = 0 Then

                        For i = 0 To ns
                            For j = 0 To ns
                                If i = j Then dfdx(i, j) = 1 Else dfdx(i, j) = 0
                            Next
                        Next

                        Broyden.broydn(ns, xtj, fx, dxtj, xtjb, fxb, dfdx, 0)

                    Else

                        Broyden.broydn(ns, xtj, fx, dxtj, xtjb, fxb, dfdx, 1)

                        For i = 0 To ns
                            Tj(i) = Tj(i) + dxtj(i)
                        Next

                    End If

                Else

                    If ic < 5 Then

                        For i = 0 To ns
                            Tj(i) = 0.5 * Tj(i) + 0.5 * Tj_ant(i)
                        Next

                    End If

                End If

                For i = 0 To ns
                    If Double.IsNaN(Tj(i)) Or Double.IsInfinity(Tj(i)) Then
                        Tj(i) = Tj_ant(i)
                    End If
                    For j = 0 To nc - 1
                        If Double.IsNaN(K(i)(j)) Then K(i)(j) = pp.AUX_PVAPi(j, Tj(i)) / P(i)
                    Next
                Next

                IObj2?.Paragraphs.Add(String.Format("Updated Temperatures: {0}", Tj.ToMathArrayString))

                t_error_ant = t_error
                t_error = Tj.SubtractY(Tj_ant).AbsSqrSumY

                IObj2?.Paragraphs.Add(String.Format("Temperature error: {0}", t_error))

                For i = ns To 0 Step -1
                    sumy(i) = 0
                    For j = 0 To nc - 1
                        If i = ns Then
                            yc(i)(j) = K(i)(j) * xc(i)(j)
                        Else
                            yc(i)(j) = eff(i) * K(i)(j) * xc(i)(j) + (1 - eff(i)) * yc(i + 1)(j)
                        End If
                        sumy(i) += yc(i)(j)
                    Next
                Next

                For i = 0 To ns
                    For j = 0 To nc - 1
                        yc(i)(j) = yc(i)(j) / sumy(i)
                    Next
                Next

                IObj2?.Paragraphs.Add(String.Format("y: {0}", yc.ToMathArrayString))

                If doparallel Then

                    Dim t1 As Task = Task.Factory.StartNew(Sub() Parallel.For(0, ns + 1, poptions,
                                                                                     Sub(ipar)
                                                                                         If IdealH Then
                                                                                             Hl(ipar) = ppr.DW_CalcEnthalpy(xc(ipar), Tj(ipar), P(ipar), PropertyPackages.State.Liquid) * ppr.AUX_MMM(xc(ipar)) / 1000
                                                                                         Else
                                                                                             Hl(ipar) = pp.DW_CalcEnthalpy(xc(ipar), Tj(ipar), P(ipar), PropertyPackages.State.Liquid) * pp.AUX_MMM(xc(ipar)) / 1000
                                                                                         End If
                                                                                     End Sub),
                                                      Settings.TaskCancellationTokenSource.Token,
                                                      TaskCreationOptions.None,
                                                      Settings.AppTaskScheduler)

                    Dim t2 As Task = Task.Factory.StartNew(Sub() Parallel.For(0, ns + 1, poptions,
                                                                                     Sub(ipar)
                                                                                         If IdealH Then
                                                                                             Hv(ipar) = ppr.DW_CalcEnthalpy(yc(ipar), Tj(ipar), P(ipar), PropertyPackages.State.Vapor) * ppr.AUX_MMM(yc(ipar)) / 1000
                                                                                         Else
                                                                                             Hv(ipar) = pp.DW_CalcEnthalpy(yc(ipar), Tj(ipar), P(ipar), PropertyPackages.State.Vapor) * pp.AUX_MMM(yc(ipar)) / 1000
                                                                                         End If
                                                                                     End Sub),
                                                      Settings.TaskCancellationTokenSource.Token,
                                                      TaskCreationOptions.None,
                                                      Settings.AppTaskScheduler)
                    Task.WaitAll(t1, t2)

                Else
                    For i = 0 To ns
                        IObj2?.SetCurrent
                        If IdealH Then
                            ppr.CurrentMaterialStream.Flowsheet.CheckStatus()
                            Hl(i) = ppr.DW_CalcEnthalpy(xc(i), Tj(i), P(i), PropertyPackages.State.Liquid) * ppr.AUX_MMM(xc(i)) / 1000
                            IObj2?.SetCurrent
                            Hv(i) = ppr.DW_CalcEnthalpy(yc(i), Tj(i), P(i), PropertyPackages.State.Vapor) * ppr.AUX_MMM(yc(i)) / 1000
                        Else
                            pp.CurrentMaterialStream.Flowsheet.CheckStatus()
                            Hl(i) = pp.DW_CalcEnthalpy(xc(i), Tj(i), P(i), PropertyPackages.State.Liquid) * pp.AUX_MMM(xc(i)) / 1000
                            IObj2?.SetCurrent
                            Hv(i) = pp.DW_CalcEnthalpy(yc(i), Tj(i), P(i), PropertyPackages.State.Vapor) * pp.AUX_MMM(yc(i)) / 1000
                        End If
                    Next
                End If

                'handle specs

                If Not rebabs Then
                    Select Case specs("C").SType
                        Case ColumnSpec.SpecType.Product_Mass_Flow_Rate
                            LSSj(0) = spval1 / pp.AUX_MMM(xc(0)) * 1000
                            rr = (Lj(0) + LSSj(0)) / LSSj(0)
                        Case ColumnSpec.SpecType.Product_Molar_Flow_Rate
                            LSSj(0) = spval1
                            rr = (Lj(0) + LSSj(0)) / LSSj(0)
                        Case ColumnSpec.SpecType.Stream_Ratio
                            rr = spval1
                        Case ColumnSpec.SpecType.Heat_Duty
                            Q(0) = spval1
                            LSSj(0) = -Lj(0) - (Q(0) - Vj(1) * Hv(1) - F(0) * Hfj(0) + (Vj(0) + VSSj(0)) * Hv(0)) / Hl(0)
                            rr = (Lj(0) + LSSj(0)) / LSSj(0)
                    End Select
                Else
                    LSSj(0) = 0.0
                    rr = (Vj(1) + Fj(0)) / Vj(0) - 1
                End If

                If Not refabs Then
                    Select Case specs("R").SType
                        Case ColumnSpec.SpecType.Product_Mass_Flow_Rate
                            B = spval2 / pp.AUX_MMM(xc(ns)) * 1000
                        Case ColumnSpec.SpecType.Product_Molar_Flow_Rate
                            B = spval2
                        Case ColumnSpec.SpecType.Stream_Ratio
                            B = Vj(ns) / spval2
                        Case ColumnSpec.SpecType.Heat_Duty
                            Q(ns) = spval2
                            Dim sum3, sum4, val1 As Double
                            sum3 = 0
                            sum4 = 0
                            For i = 0 To ns
                                sum3 += F(i) * Hfj(i) - LSSj(i) * Hl(i) - VSSj(i) * Hv(i)
                            Next
                            val1 = sum3 - Q(ns)
                            sum4 = 0
                            For i = 0 To ns - 1
                                sum4 += Q(i)
                            Next
                            B = -(val1 - (sum4 - Vj(0) * Hv(0))) / Hl(ns)
                    End Select
                Else
                    B = Lj(ns)
                End If

                sumF = 0
                sumLSS = 0
                sumVSS = 0
                For i = 0 To ns
                    sumF += F(i)
                    If i > 0 Then sumLSS += LSS(i)
                    sumVSS += VSS(i)
                Next

                If condt = Column.condtype.Full_Reflux Or rebabs Then
                    Vj(0) = sumF - B - sumLSS - sumVSS
                    LSSj(0) = 0.0
                Else
                    LSSj(0) = sumF - B - sumLSS - sumVSS - Vj(0)
                End If

                'reboiler and condenser heat duties

                Dim alpha(ns), beta(ns), gamma(ns) As Double

                For i = 0 To ns
                    sum1(i) = 0
                    sum2(i) = 0
                    For j = 0 To i
                        sum1(i) += Fj(j) - LSSj(j) - VSSj(j)
                    Next
                    If i > 0 Then
                        For j = 0 To i - 1
                            sum2(i) += Fj(j) - LSSj(j) - VSSj(j)
                        Next
                    End If
                Next

                For j = 1 To ns
                    gamma(j) = (sum2(j) - Vj(0)) * (Hl(j) - Hl(j - 1)) + Fj(j) * (Hl(j) - Hfj(j)) + VSSj(j) * (Hv(j) - Hl(j)) + Q(j)
                    alpha(j) = Hl(j - 1) - Hv(j)
                    If j < ns Then beta(j) = Hv(j + 1) - Hl(j)
                Next

                'solve matrices

                For i = 0 To ns
                    Vj_ant(i) = Vj(i)
                Next

                If Not rebabs Then
                    If Not condt = Column.condtype.Full_Reflux Then
                        Vj(0) = V(0)
                        Vj(1) = (rr + 1) * LSSj(0) - Fj(0) + Vj(0)
                    Else
                        Vj(1) = (rr + 1) * Vj(0) - Fj(0)
                    End If
                Else
                    Vj(0) = V(0)
                    Vj(1) = (rr + 1) * Vj(0) - Fj(0)
                End If

                For i = 2 To ns
                    Vj(i) = (gamma(i - 1) - alpha(i - 1) * Vj(i - 1)) / beta(i - 1)
                    If Vj(i) < 0 Then Vj(i) = 0.000001
                Next

                For i = 0 To ns
                    dVj(i) = Vj(i) - Vj_ant(i)
                    If Abs(dVj(i)) > 0.1 * Vj_ant(i) Then Vj(i) = Vj_ant(i) * (1 + Math.Sign(dVj(i)) * 0.1)
                Next

                'Ljs
                For i = 0 To ns
                    If i < ns Then Lj(i) = Vj(i + 1) + sum1(i) - Vj(0) Else Lj(i) = sum1(i) - Vj(0)
                    If Lj(i) < 0.0# Then Lj(i) = 0.0001 * Fj.Sum
                Next

                IObj2?.Paragraphs.Add(String.Format("alpha: {0}", alpha.ToMathArrayString))

                IObj2?.Paragraphs.Add(String.Format("beta: {0}", beta.ToMathArrayString))

                IObj2?.Paragraphs.Add(String.Format("gamma: {0}", gamma.ToMathArrayString))

                IObj2?.Paragraphs.Add(String.Format("Updated L: {0}", Lj.ToMathArrayString))

                IObj2?.Paragraphs.Add(String.Format("Updated V: {0}", Vj.ToMathArrayString))

                IObj2?.Paragraphs.Add(String.Format("Updated LSS: {0}", LSSj.ToMathArrayString))

                IObj2?.Paragraphs.Add(String.Format("Updated VSS: {0}", VSSj.ToMathArrayString))

                'reboiler and condenser heat duties
                Select Case coltype
                    Case Column.ColType.DistillationColumn
                        If Not specs("C").SType = ColumnSpec.SpecType.Heat_Duty Then
                            Q(0) = (Vj(1) * Hv(1) + F(0) * Hfj(0) - (Lj(0) + LSSj(0)) * Hl(0) - (Vj(0) + VSSj(0)) * Hv(0))
                        End If
                        If Not specs("R").SType = ColumnSpec.SpecType.Heat_Duty Then
                            Dim sum3, sum4 As Double
                            sum3 = 0
                            sum4 = 0
                            For i = 0 To ns
                                sum3 += F(i) * Hfj(i) - LSSj(i) * Hl(i) - VSSj(i) * Hv(i)
                            Next
                            For i = 0 To ns - 1
                                sum4 += Q(i)
                            Next
                            Q(ns) = sum3 - sum4 - Vj(0) * Hv(0) - Lj(ns) * Hl(ns)
                        End If
                    Case Column.ColType.AbsorptionColumn
                        'use provided values
                    Case Column.ColType.RefluxedAbsorber
                        If Not specs("C").SType = ColumnSpec.SpecType.Heat_Duty Then
                            Q(0) = (Vj(1) * Hv(1) + F(0) * Hfj(0) - (Lj(0) + LSSj(0)) * Hl(0) - (Vj(0) + VSSj(0)) * Hv(0))
                        End If
                    Case Column.ColType.ReboiledAbsorber
                        If Not specs("R").SType = ColumnSpec.SpecType.Heat_Duty Then
                            Q(ns) = (Lj(ns - 1) * Hl(ns - 1) + F(ns) * Hfj(ns) - (Lj(ns) + LSSj(ns)) * Hl(ns) - (Vj(ns) + VSSj(ns)) * Hv(ns))
                        End If
                End Select

                IObj2?.Paragraphs.Add(String.Format("Updated Q: {0}", Q.ToMathArrayString))

                ic = ic + 1

                If Not IdealH And Not IdealK Then
                    If ic >= maxits Then Throw New Exception(pp.CurrentMaterialStream.Flowsheet.GetTranslatedString("DCMaxIterationsReached"))
                End If
                If Double.IsNaN(t_error) Then Throw New Exception(pp.CurrentMaterialStream.Flowsheet.GetTranslatedString("DCGeneralError"))
                If ic = stopatitnumber - 1 Then Exit Do

                pp.CurrentMaterialStream.Flowsheet.CheckStatus()

                Calculator.WriteToConsole("Bubble Point solver T error = " & t_error, 1)

                IObj2?.Close()

            Loop Until t_error < tol(1) * ns

            IObj?.Paragraphs.Add("The algorithm converged in " & ic & " iterations.")

            IObj?.Paragraphs.Add("<h2>Results</h2>")

            IObj?.Paragraphs.Add(String.Format("Final converged values for T: {0}", Tj.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Final converged values for V: {0}", Vj.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Final converged values for L: {0}", Lj.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Final converged values for VSS: {0}", VSSj.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Final converged values for LSS: {0}", LSSj.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Final converged values for y: {0}", yc.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Final converged values for x: {0}", xc.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Final converged values for K: {0}", K.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Final converged values for Q: {0}", Q.ToMathArrayString))

            IObj?.Close()

            'finished, return arrays

            Return New Object() {Tj, Vj, Lj, VSSj, LSSj, yc, xc, K, Q, ic, t_error}

        End Function

    End Class

    <System.Serializable()> Public Class BurninghamOttoMethod

        Public Shared Function Solve(ByVal nc As Integer, ByVal ns As Integer, ByVal maxits As Integer,
                                ByVal tol As Double(), ByVal F As Double(), ByVal V As Double(),
                                ByVal Q As Double(), ByVal L As Double(),
                                ByVal VSS As Double(), ByVal LSS As Double(), ByVal Kval()() As Double,
                                ByVal x()() As Double, ByVal y()() As Double, ByVal z()() As Double,
                                ByVal fc()() As Double,
                                ByVal HF As Double(), ByVal T As Double(), ByVal P As Double(),
                                ByVal stopatitnumber As Integer,
                                ByVal eff() As Double,
                                ByVal pp As PropertyPackages.PropertyPackage,
                                ByVal specs As Dictionary(Of String, SepOps.ColumnSpec),
                                ByVal IdealK As Boolean, ByVal IdealH As Boolean,
                                Optional ByVal llextr As Boolean = False) As Object

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

            Inspector.Host.CheckAndAdd(IObj, "", "Solve", "Sum-Rates (SR) Method", "Burningham–Otto Sum-Rates (SR) Method for Absorption and Stripping", True)

            IObj?.SetCurrent()

            IObj?.Paragraphs.Add("The species in most absorbers and strippers cover a wide range of volatility. Hence, the BP method of 
                                    solving the MESH equations fails because bubble-point temperature calculations are too sensitive to 
                                    liquid-phase composition, and the stage energy balance is much more sensitive to stage temperatures 
                                    than to interstage flow rates. In this case, Friday and Smith showed that an alternative procedure 
                                    devised by Sujata could be used. This sum-rates (SR) method was further developed in conjunction with 
                                    the tridiagonal-matrix formulation for the modified M equations by Burningham and Otto.")

            IObj?.Paragraphs.Add("Problem specifications consist of conditions and stage locations for feeds, stage pressure, sidestream 
                                    flow rates, stage heat-transfer rates, and number of stages. Tear variables Tj and Vj are assumed to 
                                    initiate the calculations. It is sufficient to assume a set of Vj values based on the assumption of 
                                    constant-molar flows, working up from the absorber bottom using specified vapor feeds and vapor sidestream 
                                    flows, if any. An initial set of Tj values can be obtained from assumed top-stage and bottom-stage values 
                                    and a linear variation with stages in between. Values of xi,j are established by solving the tridiagonal 
                                    matrix by the Thomas algorithm. These values are not normalized but utilized directly to produce new values 
                                    of Lj through the sum-rates equation:")

            IObj?.Paragraphs.Add("<math>L^{(k+1)}_{j}=L^{(k)}_j\sum\limits_{i=1}^{C}{x_{i,j}}</math>")

            IObj?.Paragraphs.Add("where <math_inline>L^{(k)}_j</math_inline> values are obtained from <math_inline>V^{(k)}_j</math_inline> 
                                    values by")

            IObj?.Paragraphs.Add("<math>L_{j}=V_{j+1}+\sum\limits_{m=1}^{j}{(F_{m}-U_{m}-W_{m})-V_{1}}</math>")

            IObj?.Paragraphs.Add("Corresponding values of <math_inline>V^{(k+1)}_j</math_inline> are obtained from a total material balance:")

            IObj?.Paragraphs.Add("<math>V_{j}=L_{j-1}-L_{N}+\sum\limits_{m=j}^{N}{(F_{m}-U_{m}-W_{m})}</math>")

            IObj?.Paragraphs.Add("Normalized <math_inline>x_{i,j}</math_inline> values are calculated and Corresponding values of 
                                    <math_inline>y_{i,j}</math_inline> are computed from")

            IObj?.Paragraphs.Add("<math>y_{i,j}=K_{i,j}x_{i,j}</math>")

            IObj?.Paragraphs.Add("A new set of Tj is obtained by solving the simultaneous energy-balance relations for the N stages. The 
                                    temperatures are embedded in the specific enthalpies for the unspecified vapor and liquid flow rates. 
                                    Typically, these enthalpies are nonlinear in temperature. Therefore, an iterative procedure such as the 
                                    Newton–Raphson method is required.")

            IObj?.Paragraphs.Add("To obtain a new set of Tj from the energy equation, the Newton–Raphson recursion equation is")

            IObj?.Paragraphs.Add("<math>\left(\frac{\partial H_j}{\partial T_{j-1}} \right)^{(r)}\Delta T^{(r)}_{j-1} +\left(\frac{\partial H_j}{\partial T_{j}} \right)^{(r)}\Delta T^{(r)}_{j}+ \left(\frac{\partial H_j}{\partial T_{j+1}} \right)^{(r)}\Delta T^{(r)}_{j+1}=-H^{(r)}_j</math>")

            IObj?.Paragraphs.Add("where")

            IObj?.Paragraphs.Add("<math>\Delta T^{(r)}_{j}=T^{(r+1)}_j-T^{(r)}_j</math>")

            IObj?.Paragraphs.Add("<math>\frac{\partial H_j}{\partial T_{j-1}}=L_{j-1}\frac{\partial h_{L_{j-1}}}{\partial T_{j-1}}</math>")

            IObj?.Paragraphs.Add("<math>\frac{\partial H_j}{\partial T_{j}}=-(L_{j}+U_{j})\frac{\partial h_{L_{j}}}{\partial T_{j}}-(V_j+W_j)\frac{\partial h_{V_{j}}}{\partial T_{j}}</math>")

            IObj?.Paragraphs.Add("<math>\frac{\partial H_j}{\partial T_{j+1}}=V_{j+1}\frac{\partial h_{V_{j+1}}}{\partial T_{j+1}}</math>")

            IObj?.Paragraphs.Add("The partial derivatives are calculated numerically using the values provided by the Property Package.")

            IObj?.Paragraphs.Add("The N relations form a tridiagonal matrix equation that is linear in <math_inline>\Delta T^{(r)}_{j}</math_inline>.")

            IObj?.Paragraphs.Add("The matrix of partial derivatives is called the Jacobian correction matrix. The Thomas algorithm can be employed 
                                    to solve for the set of corrections <math_inline>\Delta T^{(r)}_{j}</math_inline>. New guesses of Tj are then determined from")

            IObj?.Paragraphs.Add("<math>T_j^{(r+1)}=T_j^{(r)}+\Delta T_j^{(r)}</math>")

            IObj?.Paragraphs.Add("When corrections <math_inline>\Delta T^{(r)}_{j}</math_inline> approach zero, the resulting values of Tj are 
                                    used with criteria to determine if convergence has been achieved. If not, before beginning a new k iteration, 
                                    values of Vj and Tj are adjusted. Convergence is rapid for the sum-rates method.")

            Dim ppr As PropertyPackages.RaoultPropertyPackage = Nothing

            If IdealK Or IdealH Then
                ppr = New PropertyPackages.RaoultPropertyPackage
                ppr.Flowsheet = pp.Flowsheet
                ppr.CurrentMaterialStream = pp.CurrentMaterialStream
            End If

            Dim doparallel As Boolean = Settings.EnableParallelProcessing
            Dim poptions As New ParallelOptions() With {.MaxDegreeOfParallelism = Settings.MaxDegreeOfParallelism}

            Dim ic As Integer
            Dim t_error, comperror As Double
            Dim Tj(ns), Tj_ant(ns) As Double
            Dim Fj(ns), Lj(ns), Vj(ns), Vj_ant(ns), xc(ns)(), fcj(ns)(), yc(ns)(), yc_ant(ns)(), lc(ns)(), vc(ns)(), zc(ns)(), K(ns)() As Double
            Dim Hfj(ns), Hv(ns), Hl(ns) As Double
            Dim VSSj(ns), LSSj(ns) As Double
            Dim sum1(ns), sum2(ns), sum3(ns) As Double

            'step1

            'step2

            Dim i, j As Integer

            For i = 0 To ns
                Array.Resize(fcj(i), nc)
                Array.Resize(xc(i), nc)
                Array.Resize(yc(i), nc)
                Array.Resize(yc_ant(i), nc)
                Array.Resize(lc(i), nc)
                Array.Resize(vc(i), nc)
                Array.Resize(zc(i), nc)
                Array.Resize(zc(i), nc)
                Array.Resize(K(i), nc)
            Next

            For i = 0 To ns
                VSSj(i) = VSS(i)
                LSSj(i) = LSS(i)
                Lj(i) = L(i)
                Vj(i) = V(i)
                Tj(i) = T(i)
                Fj(i) = F(i)
                Hfj(i) = HF(i) / 1000
                fcj(i) = fc(i)
            Next

            IObj?.Paragraphs.Add("<h2>Input Parameters / Initial Estimates</h2>")

            IObj?.Paragraphs.Add(String.Format("Stage Temperatures: {0}", T.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Stage Pressures: {0}", P.ToMathArrayString))

            IObj?.Paragraphs.Add(String.Format("Feeds: {0}", F.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Vapor Flows: {0}", V.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Liquid Flows: {0}", L.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Vapor Side Draws: {0}", VSS.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Liquid Side Draws: {0}", LSS.ToMathArrayString))

            IObj?.Paragraphs.Add(String.Format("Mixture Compositions: {0}", z.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Liquid Phase Compositions: {0}", x.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Vapor/Liquid2 Phase Compositions: {0}", y.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("K-values: {0}", K.ToMathArrayString))

            IObj?.Paragraphs.Add("<h2>Calculated Parameters</h2>")

            For i = 0 To ns
                For j = 0 To nc - 1
                    K(i)(j) = Kval(i)(j)
                    IObj?.SetCurrent()
                    If Double.IsNaN(K(i)(j)) Or Double.IsInfinity(K(i)(j)) Or K(i)(j) = 0# Then
                        If llextr Then
                            If i > 0 Then
                                If K(i - 1).Sum > 0.0 Then
                                    K(i) = K(i - 1).Clone
                                End If
                            End If
                        Else
                            K(i)(j) = pp.AUX_PVAPi(j, T(i)) / P(i)
                        End If
                    End If
                Next
            Next

            'step3

            'internal loop
            ic = 0
            Do

                IObj?.SetCurrent()

                Dim IObj2 As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

                Inspector.Host.CheckAndAdd(IObj2, "", "Solve", "Sum-Rates (SR) Internal Loop #" & ic, "Burningham–Otto Sum-Rates (SR) Method for Absorption and Stripping", True)

                Dim il_err As Double = 0.0#
                Dim il_err_ant As Double = 0.0#
                Dim num, denom, x0, fx0 As New ArrayList

                'step4

                'find component liquid flows by the tridiagonal matrix method

                IObj2?.Paragraphs.Add(String.Format("Find component liquid flows by the tridiagonal matrix method"))
                IObj2?.Paragraphs.Add(String.Format("Calculating TDM A, B, C, D"))

                Dim at(nc - 1)(), bt(nc - 1)(), ct(nc - 1)(), dt(nc - 1)(), xt(nc - 1)() As Double

                For i = 0 To nc - 1
                    Array.Resize(at(i), ns + 1)
                    Array.Resize(bt(i), ns + 1)
                    Array.Resize(ct(i), ns + 1)
                    Array.Resize(dt(i), ns + 1)
                    Array.Resize(xt(i), ns + 1)
                Next

                For i = 0 To ns
                    sum1(i) = 0
                    sum2(i) = 0
                    For j = 0 To i
                        sum1(i) += Fj(j) - LSSj(j) - VSSj(j)
                    Next
                    If i > 0 Then
                        For j = 0 To i - 1
                            sum2(i) += Fj(j) - LSSj(j) - VSSj(j)
                        Next
                    End If
                Next

                For i = 0 To nc - 1
                    For j = 0 To ns
                        dt(i)(j) = -Fj(j) * fcj(j)(i)
                        If j < ns Then
                            bt(i)(j) = -(Vj(j + 1) + sum1(j) - Vj(0) + LSSj(j) + (Vj(j) + VSSj(j)) * K(j)(i))
                        Else
                            bt(i)(j) = -(sum1(j) - Vj(0) + LSSj(j) + (Vj(j) + VSSj(j)) * K(j)(i))
                        End If
                        'tdma solve
                        If j < ns Then ct(i)(j) = Vj(j + 1) * K(j + 1)(i)
                        If j > 0 Then at(i)(j) = Vj(j) + sum2(j) - Vj(0)
                    Next
                Next

                'solve matrices

                IObj2?.Paragraphs.Add(String.Format("Calling TDM Solver to calculate liquid phase compositions"))

                IObj2?.SetCurrent()

                IObj2?.Paragraphs.Add(String.Format("A: {0}", at.ToMathArrayString))

                IObj2?.Paragraphs.Add(String.Format("B: {0}", bt.ToMathArrayString))

                IObj2?.Paragraphs.Add(String.Format("C: {0}", ct.ToMathArrayString))

                IObj2?.Paragraphs.Add(String.Format("D: {0}", dt.ToMathArrayString))

                'tomich

                For i = 0 To nc - 1
                    IObj2?.SetCurrent()
                    IObj2?.Paragraphs.Add(String.Format("Calling TDM Solver for Stage #{0}...", i + 1))
                    xt(i) = Tomich.TDMASolve(at(i), bt(i), ct(i), dt(i))
                Next

                IObj2?.SetCurrent()

                IObj2?.Paragraphs.Add(String.Format("TDM solved successfully."))

                Dim sumx(ns), sumy(ns), sumz(ns) As Double

                For i = 0 To ns
                    sumx(i) = 0
                    For j = 0 To nc - 1
                        lc(i)(j) = xt(j)(i)
                        If lc(i)(j) < 0 Then lc(i)(j) = 0.0000001
                        sumx(i) += lc(i)(j)
                    Next
                Next

                'Ljs
                For i = 0 To ns
                    Lj(i) = Lj(i) * sumx(i)
                Next

                IObj2?.Paragraphs.Add(String.Format("l: {0}", lc.ToMathArrayString))

                IObj2?.Paragraphs.Add(String.Format("L: {0}", Lj.ToMathArrayString))

                For i = 0 To ns
                    For j = 0 To nc - 1
                        xc(i)(j) = lc(i)(j) / sumx(i)
                        yc(i)(j) = xc(i)(j) * K(i)(j)
                        sumy(i) += yc(i)(j)
                    Next
                Next

                IObj2?.Paragraphs.Add(String.Format("x: {0}", xc.ToMathArrayString))

                For i = 0 To ns
                    For j = 0 To nc - 1
                        yc(i)(j) = yc(i)(j) / sumy(i)
                    Next
                Next

                IObj2?.Paragraphs.Add(String.Format("y: {0}", yc.ToMathArrayString))

                For i = 0 To ns
                    sum3(i) = 0
                    For j = i To ns
                        sum3(i) += Fj(j) - LSSj(j) - VSSj(j)
                    Next
                Next

                'solve matrices

                For i = 0 To ns
                    Vj_ant(i) = Vj(i)
                Next

                For i = 0 To ns
                    If i > 0 Then
                        Vj(i) = Lj(i - 1) - Lj(ns) + sum3(i)
                    Else
                        Vj(i) = -Lj(ns) + sum3(i)
                    End If
                    If Vj(i) < 0 Then Vj(i) = -Vj(i)
                Next

                For i = 0 To ns
                    sumz(i) = 0
                    For j = 0 To nc - 1
                        vc(i)(j) = xc(i)(j) * Vj(i) * K(i)(j)
                        zc(i)(j) = (lc(i)(j) + vc(i)(j)) / (Lj(i) + Vj(i))
                        sumz(i) += zc(i)(j)
                    Next
                Next

                IObj2?.Paragraphs.Add(String.Format("v: {0}", vc.ToMathArrayString))

                IObj2?.Paragraphs.Add(String.Format("V: {0}", Vj.ToMathArrayString))

                For i = 0 To ns
                    For j = 0 To nc - 1
                        zc(i)(j) = zc(i)(j) / sumz(i)
                    Next
                Next

                'Dim tmp As Object

                'calculate new temperatures

                IObj2?.Paragraphs.Add("Calculating new temperatures...")

                ''''''''''''''''''''
                Dim H(ns), dHldT(ns), dHvdT(ns), dHdTa(ns), dHdTb(ns), dHdTc(ns), dHl(ns), dHv(ns) As Double

                Dim epsilon As Double = 0.1

                If doparallel Then

                    Dim task1 As Task = Task.Factory.StartNew(Sub() Parallel.For(0, ns + 1, poptions,
                                                             Sub(ipar)
                                                                 If IdealH Then
                                                                     Hl(ipar) = ppr.DW_CalcEnthalpy(xc(ipar), Tj(ipar), P(ipar), PropertyPackages.State.Liquid) * ppr.AUX_MMM(xc(ipar)) / 1000
                                                                     dHl(ipar) = ppr.DW_CalcEnthalpy(xc(ipar), Tj(ipar) - epsilon, P(ipar), PropertyPackages.State.Liquid) * ppr.AUX_MMM(xc(ipar)) / 1000
                                                                     If llextr Then
                                                                         Hv(ipar) = ppr.DW_CalcEnthalpy(yc(ipar), Tj(ipar), P(ipar), PropertyPackages.State.Liquid) * ppr.AUX_MMM(yc(ipar)) / 1000
                                                                         dHv(ipar) = ppr.DW_CalcEnthalpy(yc(ipar), Tj(ipar) - epsilon, P(ipar), PropertyPackages.State.Liquid) * ppr.AUX_MMM(yc(ipar)) / 1000
                                                                     Else
                                                                         Hv(ipar) = ppr.DW_CalcEnthalpy(yc(ipar), Tj(ipar), P(ipar), PropertyPackages.State.Vapor) * ppr.AUX_MMM(yc(ipar)) / 1000
                                                                         dHv(ipar) = ppr.DW_CalcEnthalpy(yc(ipar), Tj(ipar) - epsilon, P(ipar), PropertyPackages.State.Vapor) * ppr.AUX_MMM(yc(ipar)) / 1000
                                                                     End If
                                                                 Else
                                                                     Hl(ipar) = pp.DW_CalcEnthalpy(xc(ipar), Tj(ipar), P(ipar), PropertyPackages.State.Liquid) * pp.AUX_MMM(xc(ipar)) / 1000
                                                                     dHl(ipar) = pp.DW_CalcEnthalpy(xc(ipar), Tj(ipar) - epsilon, P(ipar), PropertyPackages.State.Liquid) * pp.AUX_MMM(xc(ipar)) / 1000
                                                                     If llextr Then
                                                                         Hv(ipar) = pp.DW_CalcEnthalpy(yc(ipar), Tj(ipar), P(ipar), PropertyPackages.State.Liquid) * pp.AUX_MMM(yc(ipar)) / 1000
                                                                         dHv(ipar) = pp.DW_CalcEnthalpy(yc(ipar), Tj(ipar) - epsilon, P(ipar), PropertyPackages.State.Liquid) * pp.AUX_MMM(yc(ipar)) / 1000
                                                                     Else
                                                                         Hv(ipar) = pp.DW_CalcEnthalpy(yc(ipar), Tj(ipar), P(ipar), PropertyPackages.State.Vapor) * pp.AUX_MMM(yc(ipar)) / 1000
                                                                         dHv(ipar) = pp.DW_CalcEnthalpy(yc(ipar), Tj(ipar) - epsilon, P(ipar), PropertyPackages.State.Vapor) * pp.AUX_MMM(yc(ipar)) / 1000
                                                                     End If
                                                                 End If
                                                             End Sub),
                                                      Settings.TaskCancellationTokenSource.Token,
                                                      TaskCreationOptions.None,
                                                      Settings.AppTaskScheduler)

                    task1.Wait(30000)
                Else
                    If IdealH Then
                        For i = 0 To ns
                            IObj2?.SetCurrent()
                            Hl(i) = ppr.DW_CalcEnthalpy(xc(i), Tj(i), P(i), PropertyPackages.State.Liquid) * ppr.AUX_MMM(xc(i)) / 1000
                            IObj2?.SetCurrent()
                            dHl(i) = ppr.DW_CalcEnthalpy(xc(i), Tj(i) - epsilon, P(i), PropertyPackages.State.Liquid) * ppr.AUX_MMM(xc(i)) / 1000
                            IObj2?.SetCurrent()
                            If llextr Then
                                Hv(i) = ppr.DW_CalcEnthalpy(yc(i), Tj(i), P(i), PropertyPackages.State.Liquid) * ppr.AUX_MMM(yc(i)) / 1000
                                IObj2?.SetCurrent()
                                dHv(i) = ppr.DW_CalcEnthalpy(yc(i), Tj(i) - epsilon, P(i), PropertyPackages.State.Liquid) * ppr.AUX_MMM(yc(i)) / 1000
                            Else
                                Hv(i) = ppr.DW_CalcEnthalpy(yc(i), Tj(i), P(i), PropertyPackages.State.Vapor) * ppr.AUX_MMM(yc(i)) / 1000
                                IObj2?.SetCurrent()
                                dHv(i) = ppr.DW_CalcEnthalpy(yc(i), Tj(i) - epsilon, P(i), PropertyPackages.State.Vapor) * ppr.AUX_MMM(yc(i)) / 1000
                            End If
                            ppr.CurrentMaterialStream.Flowsheet.CheckStatus()
                        Next
                    Else
                        For i = 0 To ns
                            IObj2?.SetCurrent()
                            Hl(i) = pp.DW_CalcEnthalpy(xc(i), Tj(i), P(i), PropertyPackages.State.Liquid) * pp.AUX_MMM(xc(i)) / 1000
                            IObj2?.SetCurrent()
                            dHl(i) = pp.DW_CalcEnthalpy(xc(i), Tj(i) - epsilon, P(i), PropertyPackages.State.Liquid) * pp.AUX_MMM(xc(i)) / 1000
                            IObj2?.SetCurrent()
                            If llextr Then
                                Hv(i) = pp.DW_CalcEnthalpy(yc(i), Tj(i), P(i), PropertyPackages.State.Liquid) * pp.AUX_MMM(yc(i)) / 1000
                                IObj2?.SetCurrent()
                                dHv(i) = pp.DW_CalcEnthalpy(yc(i), Tj(i) - epsilon, P(i), PropertyPackages.State.Liquid) * pp.AUX_MMM(yc(i)) / 1000
                            Else
                                Hv(i) = pp.DW_CalcEnthalpy(yc(i), Tj(i), P(i), PropertyPackages.State.Vapor) * pp.AUX_MMM(yc(i)) / 1000
                                IObj2?.SetCurrent()
                                dHv(i) = pp.DW_CalcEnthalpy(yc(i), Tj(i) - epsilon, P(i), PropertyPackages.State.Vapor) * pp.AUX_MMM(yc(i)) / 1000
                            End If
                            pp.CurrentMaterialStream.Flowsheet.CheckStatus()
                        Next
                    End If
                End If

                IObj2?.Paragraphs.Add(String.Format("HL: {0}", Hl.ToMathArrayString))
                IObj2?.Paragraphs.Add(String.Format("HV: {0}", Hl.ToMathArrayString))

                For i = 0 To ns
                    If i = 0 Then
                        H(i) = Vj(i + 1) * Hv(i + 1) + Fj(i) * Hfj(i) - (Lj(i) + LSSj(i)) * Hl(i) - (Vj(i) + VSSj(i)) * Hv(i) - Q(i)
                    ElseIf i = ns Then
                        H(i) = Lj(i - 1) * Hl(i - 1) + Fj(i) * Hfj(i) - (Lj(i) + LSSj(i)) * Hl(i) - (Vj(i) + VSSj(i)) * Hv(i) - Q(i)
                    Else
                        H(i) = Lj(i - 1) * Hl(i - 1) + Vj(i + 1) * Hv(i + 1) + Fj(i) * Hfj(i) - (Lj(i) + LSSj(i)) * Hl(i) - (Vj(i) + VSSj(i)) * Hv(i) - Q(i)
                    End If
                    dHldT(i) = (Hl(i) - dHl(i)) / epsilon
                    dHvdT(i) = (Hv(i) - dHv(i)) / epsilon
                Next

                IObj2?.Paragraphs.Add(("<mi>\frac{\partial h_L}{\partial T}</mi>: " & dHldT.ToMathArrayString))
                IObj2?.Paragraphs.Add(("<mi>\frac{\partial h_V}{\partial T}</mi>: " & dHvdT.ToMathArrayString))

                For i = 0 To ns
                    If i > 0 Then dHdTa(i) = Lj(i - 1) * dHldT(i - 1)
                    dHdTb(i) = -(Lj(i) + LSSj(i)) * dHldT(i) - (Vj(i) + VSSj(i)) * dHvdT(i)
                    If i < ns Then dHdTc(i) = Vj(i + 1) * dHvdT(i + 1)
                Next

                IObj2?.Paragraphs.Add(String.Format("H: {0}", H.ToMathArrayString))

                Dim ath(ns), bth(ns), cth(ns), dth(ns), xth(ns) As Double

                For i = 0 To ns
                    dth(i) = -H(i)
                    bth(i) = dHdTb(i)
                    If i < ns Then cth(i) = dHdTc(i)
                    If i > 0 Then ath(i) = dHdTa(i)
                Next

                'solve matrices
                'tomich

                IObj2?.Paragraphs.Add("Calling TDM Solver to solve for enthalpies/temperatures")

                IObj2?.SetCurrent()

                xth = Tomich.TDMASolve(ath, bth, cth, dth)

                Dim tmp As Object

                IObj2?.Paragraphs.Add(String.Format("Calculated Temperature perturbations: {0}", xth.ToMathArrayString))

                Dim deltat As Double()
                Dim maxdt As Double = xth.Select(Function(tp) Abs(tp)).Max

                deltat = xth

                t_error = 0.0#
                comperror = 0.0#
                For i = 0 To ns
                    Tj_ant(i) = Tj(i)
                    If Math.Abs(deltat(i)) > 20.0 Then
                        Tj(i) = Tj(i) + Math.Sign(deltat(i)) * 20.0
                    Else
                        Tj(i) = Tj(i) + deltat(i)
                    End If
                    If Double.IsNaN(Tj(i)) Or Double.IsInfinity(Tj(i)) Then Throw New Exception(pp.CurrentMaterialStream.Flowsheet.GetTranslatedString("DCGeneralError"))
                    If IdealK Then
                        IObj2?.SetCurrent()
                        If llextr Then
                            tmp = ppr.DW_CalcKvalue(xc(i), yc(i), Tj(i), P(i), "LL")
                        Else
                            tmp = ppr.DW_CalcKvalue(xc(i), yc(i), Tj(i), P(i))
                        End If
                    Else
                        IObj2?.SetCurrent()
                        If llextr Then
                            tmp = pp.DW_CalcKvalue(xc(i), yc(i), Tj(i), P(i), "LL")
                        Else
                            tmp = pp.DW_CalcKvalue(xc(i), yc(i), Tj(i), P(i))
                        End If
                    End If
                    sumy(i) = 0
                    For j = 0 To nc - 1
                        K(i)(j) = tmp(j)
                        If Double.IsNaN(K(i)(j)) Or Double.IsInfinity(K(i)(j)) Then
                            If llextr Then
                                IObj2?.SetCurrent()
                                K(i)(j) = pp.AUX_PVAPi(j, Tj(i)) / P(i)
                            Else
                                IObj2?.SetCurrent()
                                K(i)(j) = pp.AUX_PVAPi(j, Tj(i)) / P(i)
                            End If
                        End If
                        yc_ant(i)(j) = yc(i)(j)
                        yc(i)(j) = K(i)(j) * xc(i)(j)
                        sumy(i) += yc(i)(j)
                        comperror += Abs(yc(i)(j) - yc_ant(i)(j)) ^ 2
                    Next
                    t_error += Abs(Tj(i) - Tj_ant(i)) ^ 2
                    pp.CurrentMaterialStream.Flowsheet.CheckStatus()
                Next

                IObj2?.Paragraphs.Add(String.Format("Updated Temperatures: {0}", Tj.ToMathArrayString))

                IObj2?.Paragraphs.Add(String.Format("Updated K-values: {0}", K.ToMathArrayString))

                For i = 0 To ns
                    For j = 0 To nc - 1
                        yc(i)(j) = yc(i)(j) / sumy(i)
                    Next
                Next

                IObj2?.Paragraphs.Add(String.Format("Updated y: {0}", yc.ToMathArrayString))

                IObj2?.Paragraphs.Add(String.Format("Temperature error: {0}", t_error))

                IObj2?.Paragraphs.Add(String.Format("Composition error: {0}", comperror))

                IObj2?.Paragraphs.Add(String.Format("Combined Temperature/Composition error: {0}", t_error + comperror))

                ic = ic + 1

                If Not IdealH And Not IdealK Then
                    If ic >= maxits Then Throw New Exception(pp.CurrentMaterialStream.Flowsheet.GetTranslatedString("DCMaxIterationsReached"))
                End If

                If Double.IsNaN(t_error) Then Throw New Exception(pp.CurrentMaterialStream.Flowsheet.GetTranslatedString("DCGeneralError"))
                If Double.IsNaN(comperror) Then Throw New Exception(pp.CurrentMaterialStream.Flowsheet.GetTranslatedString("DCGeneralError"))

                pp.CurrentMaterialStream.Flowsheet.CheckStatus()

                IObj2?.Close()

                'pp.CurrentMaterialStream.Flowsheet.ShowMessage("Sum-Rates solver: iteration #" & ic.ToString & ", Temperature error = " & t_error.ToString, IFlowsheet.MessageType.Information)
                'pp.CurrentMaterialStream.Flowsheet.ShowMessage("Sum-Rates solver: iteration #" & ic.ToString & ", Composition error = " & comperror.ToString, IFlowsheet.MessageType.Information)
                'pp.CurrentMaterialStream.Flowsheet.ShowMessage("Sum-Rates solver: iteration #" & ic.ToString & ", combined Temperature/Composition error = " & (t_error + comperror).ToString, IFlowsheet.MessageType.Information)

            Loop Until t_error <= tol(1) And comperror <= tol(1)

            IObj?.Paragraphs.Add("The algorithm converged in " & ic & " iterations.")

            IObj?.Paragraphs.Add("<h2>Results</h2>")

            IObj?.Paragraphs.Add(String.Format("Final converged values for T: {0}", Tj.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Final converged values for V: {0}", Vj.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Final converged values for L: {0}", Lj.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Final converged values for VSS: {0}", VSSj.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Final converged values for LSS: {0}", LSSj.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Final converged values for y: {0}", yc.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Final converged values for x: {0}", xc.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Final converged values for K: {0}", K.ToMathArrayString))
            IObj?.Paragraphs.Add(String.Format("Final converged values for Q: {0}", Q.ToMathArrayString))

            IObj?.Close()

            For Each Ki In K
                If pp.AUX_CheckTrivial(Ki) Then
                    IObj?.Paragraphs.Add("Invalid result - converged to the trivial solution.")
                    Throw New Exception("Invalid result - converged to the trivial solution.")
                End If
            Next

            ' finished, return arrays

            Return New Object() {Tj, Vj, Lj, VSSj, LSSj, yc, xc, K, Q, ic, t_error}

        End Function

    End Class

    <System.Serializable()> Public Class Tomich

        Public Shared Function TDMASolve(ByVal a As Double(), ByVal b As Double(), ByVal c As Double(), ByVal d As Double()) As Double()

            Dim IObj As Inspector.InspectorItem = Inspector.Host.GetNewInspectorItem()

            Inspector.Host.CheckAndAdd(IObj, "", "TDMASolve", "Tridiagonal Matrix Algorithm", "Tomich TDM Solver", True)

            IObj?.Paragraphs.Add("The key to the BP and SR tearing procedures is the tridiagonal matrix, which results from a modified form of the M equations, when they are torn from the other equations by selecting Tj and Vj as the tear variables, leaving the modified M equations linear in the unknown liquid mole fractions.")

            IObj?.Paragraphs.Add("This set of equations, one for each component, is solved by a modified Gaussian–elimination algorithm due to Thomas as applied by Wang and Henke. Equations for calculating y and L are partitioned from the other equations. The result for each component, i, and each stage, j, is as follows, where the i subscripts have been dropped from the B, C, and D terms.")

            IObj?.Paragraphs.Add("<math>A_jx_{i,j-1}+B_jx_{i,j}+C_jx_{i,j+1}=D_j</math>")

            IObj?.Paragraphs.Add("where")

            IObj?.Paragraphs.Add("<math>A_j = V_j+\sum\limits_{m=1}^{j-1} (F_m-W_m-U_m)-V_1</math>")

            IObj?.Paragraphs.Add("<math>B_j=-[V_{j+1}+\sum\limits_{m+1}^j(F_m-W_m-U_m)-V_1+U_j+(V_j+W_j)K_{i,j}], 1\leq j\leq N</math>")

            IObj?.Paragraphs.Add("<math>C_j=V_{j+1}K_{i,j+1}, 1 \leq j \leq N-1</math>")

            IObj?.Paragraphs.Add("<math>D_j=-F_jz_{i,j}, 1 \leq j \leq N</math>")

            IObj?.Paragraphs.Add("with <mi>x_{i,0}=0</mi>; <mi>V_{N+1}=0</mi>; <mi>W_1 = 0</mi>, and <mi>U_N=0</mi>, as indicated in Figure 10.3. If the modified M equations are grouped by component, they can be partitioned by writing them as a series of separate tridiagonal-matrix equations, one for each component, where the output variable for each matrix equation is xi over the entire N-stage cascade.")

            IObj?.Paragraphs.Add("Constants Bj and Cj for each component depend only on tear variables T and V if K-values are composition-independent. If not, previous iteration compositions may be used to estimate K-values.")

            IObj?.Paragraphs.Add("The Thomas algorithm for solving the linearized equation set is a Gaussian–elimination procedure involving forward elimination starting from stage 1 and working toward stage N to finally isolate <mi>x_{i,N}</mi>. Other values of <mi>x_{i,j}</mi> are then obtained, starting with <mi>x_{i,N-1}</mi> by backward substitution.")

            IObj?.Paragraphs.Add("The Thomas algorithm avoids buildup of computer truncation errors because none of the steps involves subtraction of nearly equal quantities. Furthermore, computed values of xi,j are almost always positive. The algorithmis superior to alternative matrix-inversion routines. A modified Thomas algorithm for difficult cases is given by Boston and Sullivan [9]. Such cases can occur for columns with large numbers of equilibrium stages and components whose absorption factors, <mi>A=L/KV</mi>, are less than unity in one section and greater than unity in another.")

            IObj?.Paragraphs.Add("<h2>Input Parameters</h2>")

            IObj?.Paragraphs.Add(String.Format("A: {0}", a.ToMathArrayString))

            IObj?.Paragraphs.Add(String.Format("B: {0}", b.ToMathArrayString))

            IObj?.Paragraphs.Add(String.Format("C: {0}", c.ToMathArrayString))

            IObj?.Paragraphs.Add(String.Format("D: {0}", d.ToMathArrayString))

            ' Warning: will modify c and d!

            Dim n As Integer = d.Length
            ' All arrays should be the same length
            Dim x As Double() = New Double(n - 1) {}
            Dim id As Double

            ' Modify the coefficients.

            c(0) /= b(0)
            ' Division by zero risk.
            d(0) /= b(0)
            ' Division by zero would imply a singular matrix.
            For i As Integer = 1 To n - 1
                id = b(i) - c(i - 1) * a(i)
                c(i) /= id
                ' This calculation during the last iteration is redundant.
                d(i) = (d(i) - d(i - 1) * a(i)) / id
            Next

            ' Now back substitute.

            x(n - 1) = d(n - 1)
            For i As Integer = n - 2 To 0 Step -1
                x(i) = d(i) - c(i) * x(i + 1)
            Next

            IObj?.Paragraphs.Add("<h2>Results</h2>")

            IObj?.Paragraphs.Add(String.Format("x: {0}", x.ToMathArrayString))

            IObj?.Close()

            Return x

        End Function

    End Class

End Namespace
