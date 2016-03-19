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

Imports System.Collections.Generic
Imports DWSIM.DrawingTools.GraphicObjects
Imports DWSIM.DWSIM.SimulationObjects
Imports DWSIM.DWSIM.MathEx
Imports System.Math
Imports DWSIM.DWSIM.SimulationObjects.UnitOperations.Auxiliary.SepOps
Imports Mapack
Imports System.Linq
Imports DWSIM.DWSIM.Flowsheet.FlowSheetSolver

Namespace DWSIM.SimulationObjects.UnitOperations.Auxiliary.SepOps

    <System.Serializable()> Public Class Parameter

        Implements XMLSerializer.Interfaces.ICustomXMLSerialization

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

        Public Function LoadData(data As System.Collections.Generic.List(Of System.Xml.Linq.XElement)) As Boolean Implements XMLSerializer.Interfaces.ICustomXMLSerialization.LoadData

            XMLSerializer.XMLSerializer.Deserialize(Me, data)

        End Function

        Public Function SaveData() As System.Collections.Generic.List(Of System.Xml.Linq.XElement) Implements XMLSerializer.Interfaces.ICustomXMLSerialization.SaveData

            Return XMLSerializer.XMLSerializer.Serialize(Me)

        End Function

    End Class

    <System.Serializable()> Public Class Stage

        Implements XMLSerializer.Interfaces.ICustomXMLSerialization

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

        Public Function LoadData(data As System.Collections.Generic.List(Of System.Xml.Linq.XElement)) As Boolean Implements XMLSerializer.Interfaces.ICustomXMLSerialization.LoadData

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

        End Function

        Public Function SaveData() As System.Collections.Generic.List(Of System.Xml.Linq.XElement) Implements XMLSerializer.Interfaces.ICustomXMLSerialization.SaveData

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

    <System.Serializable()> Public Class SideOp

        Implements XMLSerializer.Interfaces.ICustomXMLSerialization

        Private _st As New System.Collections.Generic.List(Of Stage)
        Private _fromstage As String = ""
        Private _tostage As String = ""
        Private _id As String = ""
        Private _name As String = ""
        Private _ns As Integer

        Public Property NumberOfStages() As Integer
            Get
                Return _ns
            End Get
            Set(ByVal value As Integer)
                _ns = value
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


        Public Property Name() As String
            Get
                Return _name
            End Get
            Set(ByVal value As String)
                _name = value
            End Set
        End Property

        Public Property FromStage() As String
            Get
                Return _fromstage
            End Get
            Set(ByVal value As String)
                _fromstage = value
            End Set
        End Property

        Public Property ToStage() As String
            Get
                Return _tostage
            End Get
            Set(ByVal value As String)
                _tostage = value
            End Set
        End Property

        Public Sub New(ByVal id As String, ByVal stages As Integer)
            Me.New(stages)
            Me.ID = id
        End Sub

        Public Sub New(ByVal id As String)
            Me.New()
            Me.ID = id
        End Sub

        Public Sub New(ByVal stages As Integer)

            Me.New()

            Dim i As Integer
            For i = 0 To stages - 1
                _st.Add(New Stage(Guid.NewGuid().ToString))
            Next

        End Sub

        Public Sub New()
            _st = New System.Collections.Generic.List(Of Stage)
        End Sub

        Public ReadOnly Property Stages() As System.Collections.Generic.List(Of Stage)
            Get
                Return _st
            End Get
        End Property

        Public Overridable Function LoadData(data As System.Collections.Generic.List(Of System.Xml.Linq.XElement)) As Boolean Implements XMLSerializer.Interfaces.ICustomXMLSerialization.LoadData

            XMLSerializer.XMLSerializer.Deserialize(Me, data)

            For Each xel As XElement In (From xel2 As XElement In data Select xel2 Where xel2.Name = "Stages").Elements.ToList
                Dim id As String = xel.@ID
                If id = "" Then
                    id = Guid.NewGuid().ToString
                End If
                Dim st As New Stage(id)
                st.LoadData(xel.Elements.ToList)
                _st.Add(st)
            Next

        End Function

        Public Overridable Function SaveData() As System.Collections.Generic.List(Of System.Xml.Linq.XElement) Implements XMLSerializer.Interfaces.ICustomXMLSerialization.SaveData

            Dim elements As List(Of System.Xml.Linq.XElement) = XMLSerializer.XMLSerializer.Serialize(Me)
            Dim ci As Globalization.CultureInfo = Globalization.CultureInfo.InvariantCulture

            With elements

                .Add(New XElement("Stages"))
                For Each s As Stage In _st
                    .Item(.Count - 1).Add(New XElement("Stage", s.SaveData.ToArray))
                Next

            End With

            Return elements

        End Function

    End Class

    <System.Serializable()> Public Class SideRectifier

        Inherits SideOp

        Private _ld As Double = 10.0#
        Private _liqrate, _vaprate As Double
        Private _liqprodstream As String = ""
        Private _vapprodstream As String = ""

        Sub New()
            ' TODO: Complete member initialization 
        End Sub

        Public Property VaporProductStreamID() As String
            Get
                Return _vapprodstream
            End Get
            Set(ByVal value As String)
                _vapprodstream = value
            End Set
        End Property

        Public Property LiquidProductStreamID() As String
            Get
                Return _liqprodstream
            End Get
            Set(ByVal value As String)
                _liqprodstream = value
            End Set
        End Property

        Public Property LiquidRate() As Double
            Get
                Return _liqrate
            End Get
            Set(ByVal value As Double)
                _liqrate = value
            End Set
        End Property

        Public Property VaporRate() As Double
            Get
                Return _vaprate
            End Get
            Set(ByVal value As Double)
                _vaprate = value
            End Set
        End Property

        Public Property RefluxRatio() As Double
            Get
                Return _ld
            End Get
            Set(ByVal value As Double)
                _ld = value
            End Set
        End Property

        Sub New(ByVal id As String, ByVal stages As Integer)
            Me.New(stages)
            Me.ID = id
        End Sub

        Sub New(ByVal id As String)
            MyBase.New()
            Me.ID = id
        End Sub

        Sub New(ByVal stages As Integer)

            MyBase.New()

            Dim i As Integer
            For i = 0 To stages - 1
                Me.Stages.Add(New Stage(Guid.NewGuid().ToString))
            Next

        End Sub

    End Class

    <System.Serializable()> Public Class SideRectifierCollection

        Private _src As New Dictionary(Of String, SideRectifier)

        Public ReadOnly Property Collection() As Dictionary(Of String, SideRectifier)
            Get
                Return _src
            End Get
        End Property

        Public Overrides Function ToString() As String
            Return DWSIM.App.GetLocalString("Cliqueparaeditar")
        End Function

        Sub New()
            _src = New Dictionary(Of String, SideRectifier)
        End Sub

    End Class

    <System.Serializable()> Public Class ReboiledSideStripper

        Inherits SideOp

        Private _br As Double = 0.75#
        Private _prodrate As Double = 0
        Private _productstream As String = ""

        Sub New()
            ' TODO: Complete member initialization 
        End Sub

        Public Property ProductStreamID() As String
            Get
                Return _productstream
            End Get
            Set(ByVal value As String)
                _productstream = value
            End Set
        End Property

        Public Property ProductRate() As Double
            Get
                Return _prodrate
            End Get
            Set(ByVal value As Double)
                _prodrate = value
            End Set
        End Property

        Public Property BoilUpRatio() As Double
            Get
                Return _br
            End Get
            Set(ByVal value As Double)
                _br = value
            End Set
        End Property

        Public Sub New(ByVal id As String, ByVal stages As Integer)
            Me.New(stages)
            Me.ID = id
        End Sub

        Public Sub New(ByVal id As String)
            MyBase.New()
            Me.ID = id
        End Sub

        Public Sub New(ByVal stages As Integer)

            MyBase.New()

            Dim i As Integer
            For i = 0 To stages - 1
                Me.Stages.Add(New Stage(Guid.NewGuid().ToString))
            Next

        End Sub

    End Class

    <System.Serializable()> Public Class ReboiledSideStripperCollection

        Private _rss As New Dictionary(Of String, ReboiledSideStripper)

        Public ReadOnly Property Collection() As Dictionary(Of String, ReboiledSideStripper)
            Get
                Return _rss
            End Get
        End Property

        Public Overrides Function ToString() As String
            Return DWSIM.App.GetLocalString("Cliqueparaeditar")
        End Function

        Sub New()
            _rss = New Dictionary(Of String, ReboiledSideStripper)
        End Sub

    End Class

    <System.Serializable()> Public Class SteamedSideStripper

        Inherits SideOp

        Private _prodrate As Double = 0
        Private _productstream As String = ""
        Private _steamstream As String = ""

        Sub New()
            ' TODO: Complete member initialization 
        End Sub

        Public Property SteamStreamID() As String
            Get
                Return _steamstream
            End Get
            Set(ByVal value As String)
                _steamstream = value
            End Set
        End Property

        Public Property ProductStreamID() As String
            Get
                Return _productstream
            End Get
            Set(ByVal value As String)
                _productstream = value
            End Set
        End Property

        Public Property ProductRate() As Double
            Get
                Return _prodrate
            End Get
            Set(ByVal value As Double)
                _prodrate = value
            End Set
        End Property

        Public Sub New(ByVal id As String, ByVal stages As Integer)
            Me.New(stages)
            Me.ID = id
        End Sub

        Public Sub New(ByVal id As String)
            MyBase.New()
            Me.ID = id
        End Sub

        Public Sub New(ByVal stages As Integer)

            MyBase.New()

            Dim i As Integer
            For i = 0 To stages - 1
                Me.Stages.Add(New Stage(Guid.NewGuid().ToString))
            Next

        End Sub

    End Class

    <System.Serializable()> Public Class SteamedSideStripperCollection

        Private _sss As Dictionary(Of String, SteamedSideStripper)

        Public ReadOnly Property Collection() As Dictionary(Of String, SteamedSideStripper)
            Get
                Return _sss
            End Get
        End Property

        Public Overrides Function ToString() As String
            Return DWSIM.App.GetLocalString("Cliqueparaeditar")
        End Function

        Sub New()
            _sss = New Dictionary(Of String, SteamedSideStripper)
        End Sub

    End Class

    <System.Serializable()> Public Class PumpAround

        Inherits SideOp

        Private _duty As Double = 0.0#
        Private _dp As Double = 0.0#
        Private _name As String = ""

        Sub New()
            MyBase.New()
        End Sub

        Sub New(ByVal id As String)
            MyBase.New(id)
        End Sub

        Public Property DeltaP() As Double
            Get
                Return _dp
            End Get
            Set(ByVal value As Double)
                _dp = value
            End Set
        End Property

        Public Property Duty() As Double
            Get
                Return _duty
            End Get
            Set(ByVal value As Double)
                _duty = value
            End Set
        End Property

    End Class

    <System.Serializable()> Public Class PumpAroundCollection

        Private _pa As Dictionary(Of String, PumpAround)

        Public ReadOnly Property Collection() As Dictionary(Of String, PumpAround)
            Get
                Return _pa
            End Get
        End Property

        Public Overrides Function ToString() As String
            Return DWSIM.App.GetLocalString("Cliqueparaeditar")
        End Function

        Sub New()
            _pa = New Dictionary(Of String, PumpAround)
        End Sub

    End Class

    <System.Serializable()> Public Class InitialEstimates

        Implements XMLSerializer.Interfaces.ICustomXMLSerialization

        Private _liqcompositions As New List(Of Dictionary(Of String, Parameter))
        Private _vapcompositions As New List(Of Dictionary(Of String, Parameter))
        Private _stagetemps As New List(Of Parameter)
        Private _liqmolflows As New List(Of Parameter)
        Private _vapmolflows As New List(Of Parameter)

        Public Function LoadData(data As System.Collections.Generic.List(Of System.Xml.Linq.XElement)) As Boolean Implements XMLSerializer.Interfaces.ICustomXMLSerialization.LoadData

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

        End Function

        Public Function SaveData() As System.Collections.Generic.List(Of System.Xml.Linq.XElement) Implements XMLSerializer.Interfaces.ICustomXMLSerialization.SaveData

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

        Implements XMLSerializer.Interfaces.ICustomXMLSerialization

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

        Public Function LoadData(data As System.Collections.Generic.List(Of System.Xml.Linq.XElement)) As Boolean Implements XMLSerializer.Interfaces.ICustomXMLSerialization.LoadData

            Dim xel = (From xe In data Select xe Where xe.Name = "Name").SingleOrDefault

            XMLSerializer.XMLSerializer.Deserialize(Me, data)

            If Not xel Is Nothing Then Me.StreamID = xel.Value
            If Me.StreamID = "" Then Me.StreamID = Me.ID

            Return True

        End Function

        Public Function SaveData() As System.Collections.Generic.List(Of System.Xml.Linq.XElement) Implements XMLSerializer.Interfaces.ICustomXMLSerialization.SaveData

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

        Implements XMLSerializer.Interfaces.ICustomXMLSerialization

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

        Public Function LoadData(data As System.Collections.Generic.List(Of System.Xml.Linq.XElement)) As Boolean Implements XMLSerializer.Interfaces.ICustomXMLSerialization.LoadData

            Return XMLSerializer.XMLSerializer.Deserialize(Me, data)

        End Function

        Public Function SaveData() As System.Collections.Generic.List(Of System.Xml.Linq.XElement) Implements XMLSerializer.Interfaces.ICustomXMLSerialization.SaveData

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

Namespace DWSIM.SimulationObjects.UnitOperations

    <Serializable()> Public Class DistillationColumn

        Inherits Column

        'solving method (default = IO)

        Public _sm As SolvingMethods.DistColSolvingMethod = SolvingMethods.DistColSolvingMethod.WangHenke_BubblePoint

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

        Public Sub New(ByVal name As String, ByVal description As String)
            MyBase.New(name, description)
            Me.ColumnType = ColType.DistillationColumn
            MyBase.AddStages()
            For k2 = 0 To Me.Stages.Count - 1
                Me.Stages(k2).P = 101325
            Next
        End Sub

        Public Overloads Overrides Function GetProperties(ByVal proptype As DWSIM.SimulationObjects.UnitOperations.BaseClass.PropertyType) As String()
            Dim i As Integer = 0
            Dim proplist As New ArrayList
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

        Public Overrides Function GetPropertyValue(ByVal prop As String, Optional ByVal su As SystemsOfUnits.Units = Nothing) As Object

            If su Is Nothing Then su = New DWSIM.SystemsOfUnits.SI
            Dim cv As New DWSIM.SystemsOfUnits.Converter
            Dim value As Double = 0
            Dim propidx As Integer = -1
            Integer.TryParse(prop.Split("_")(2), propidx)

            Select Case propidx

                Case 0
                    'PROP_DC_0	Condenser Pressure
                    value = Converter.ConvertFromSI(su.pressure, Me.CondenserPressure)
                Case 1
                    'PROP_DC_1	Reboiler Pressure
                    value = Converter.ConvertFromSI(su.pressure, Me.ReboilerPressure)
                Case 2
                    'PROP_DC_2	Condenser Pressure Drop
                    value = Converter.ConvertFromSI(su.deltaP, Me.CondenserDeltaP)
                Case 5
                    'PROP_DC_5	Condenser Duty
                    value = Converter.ConvertFromSI(su.heatflow, Me.CondenserDuty)
                Case 6
                    'PROP_DC_6	Reboiler Duty
                    value = Converter.ConvertFromSI(su.heatflow, Me.ReboilerDuty)
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
                If Me.Stages.Count >= stageindex Then value = Converter.ConvertFromSI(su.pressure, Me.Stages(stageindex - 1).P)
            End If

            If prop.Contains("Stage_Temperature_") Then
                Dim stageindex As Integer = prop.Split("_")(2)
                If Me.Stages.Count >= stageindex Then value = Converter.ConvertFromSI(su.temperature, Me.Stages(stageindex - 1).T)
            End If

            If prop.Contains("Stage_Efficiency_") Then
                Dim stageindex As Integer = prop.Split("_")(2)
                If Me.Stages.Count >= stageindex Then value = Me.Stages(stageindex - 1).Efficiency
            End If

            If prop.Contains("Global_Stage_Efficiency") Then value = -1

            Return value

        End Function

        Public Overrides Function GetPropertyUnit(ByVal prop As String, Optional ByVal su As SystemsOfUnits.Units = Nothing) As Object
            If su Is Nothing Then su = New DWSIM.SystemsOfUnits.SI
            Dim cv As New DWSIM.SystemsOfUnits.Converter
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

            If prop.Contains("Stage_Pressure_") Then value = su.pressure
            If prop.Contains("Stage_Temperature_") Then value = su.temperature
            If prop.Contains("Stage_Efficiency_") Then value = ""

            Return value

        End Function

        Public Overrides Function SetPropertyValue(ByVal prop As String, ByVal propval As Object, Optional ByVal su As SystemsOfUnits.Units = Nothing) As Object
            If su Is Nothing Then su = New DWSIM.SystemsOfUnits.SI
            Dim cv As New DWSIM.SystemsOfUnits.Converter
            Dim propidx As Integer = -1
            Integer.TryParse(prop.Split("_")(2), propidx)

            Select Case propidx

                Case 0
                    'PROP_DC_0	Condenser Pressure
                    Me.CondenserPressure = Converter.ConvertToSI(su.pressure, propval)
                Case 1
                    'PROP_DC_1	Reboiler Pressure
                    Me.ReboilerPressure = Converter.ConvertToSI(su.pressure, propval)
                Case 2
                    'PROP_DC_2	Condenser Pressure Drop
                    Me.CondenserDeltaP = Converter.ConvertToSI(su.deltaP, propval)

            End Select

            Select Case prop
                Case "Condenser_Specification_Value"
                    Me.Specs("C").SpecValue = propval
                Case "Reboiler_Specification_Value"
                    Me.Specs("R").SpecValue = propval
            End Select

            If prop.Contains("Stage_Pressure_") Then
                Dim stageindex As Integer = prop.Split("_")(2)
                If Me.Stages.Count >= stageindex Then Me.Stages(stageindex - 1).P = Converter.ConvertToSI(su.pressure, propval)
            End If

            If prop.Contains("Stage_Temperature_") Then
                Dim stageindex As Integer = prop.Split("_")(2)
                If Me.Stages.Count >= stageindex Then Me.Stages(stageindex - 1).T = Converter.ConvertToSI(su.temperature, propval)
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

    End Class

    <Serializable()> Public Class AbsorptionColumn

        Inherits Column

        Public _sm As SolvingMethods.AbsColSolvingMethod

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

        Public Sub New(ByVal name As String, ByVal description As String)
            MyBase.New(name, description)
            Me.ColumnType = ColType.AbsorptionColumn
            MyBase.AddStages()
            For k2 = 0 To Me.Stages.Count - 1
                Me.Stages(k2).P = 101325
            Next
        End Sub

        Public Overloads Overrides Function GetProperties(ByVal proptype As DWSIM.SimulationObjects.UnitOperations.BaseClass.PropertyType) As String()
            Dim i As Integer = 0
            Dim proplist As New ArrayList
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

        Public Overrides Function GetPropertyValue(ByVal prop As String, Optional ByVal su As SystemsOfUnits.Units = Nothing) As Object
            If su Is Nothing Then su = New DWSIM.SystemsOfUnits.SI
            Dim cv As New DWSIM.SystemsOfUnits.Converter
            Dim value As Double = 0
            Dim propidx As Integer = Convert.ToInt32(prop.Split("_")(2))

            Select Case propidx

                Case 0
                    'PROP_DC_0	Condenser Pressure
                    value = Converter.ConvertFromSI(su.pressure, Me.CondenserPressure)
                Case 1
                    'PROP_DC_1	Reboiler Pressure
                    value = Converter.ConvertFromSI(su.pressure, Me.ReboilerPressure)
                Case 2
                    'PROP_DC_7	Number of Stages
                    value = Me.NumberOfStages
            End Select

            Return value
        End Function

        Public Overrides Function GetPropertyUnit(ByVal prop As String, Optional ByVal su As SystemsOfUnits.Units = Nothing) As Object
            If su Is Nothing Then su = New DWSIM.SystemsOfUnits.SI
            Dim cv As New DWSIM.SystemsOfUnits.Converter
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

        Public Overrides Function SetPropertyValue(ByVal prop As String, ByVal propval As Object, Optional ByVal su As SystemsOfUnits.Units = Nothing) As Object
            If su Is Nothing Then su = New DWSIM.SystemsOfUnits.SI
            Dim cv As New DWSIM.SystemsOfUnits.Converter
            Dim propidx As Integer = Convert.ToInt32(prop.Split("_")(2))

            Select Case propidx

                Case 0
                    'PROP_DC_0	Condenser Pressure
                    Me.CondenserPressure = Converter.ConvertToSI(su.pressure, propval)
                Case 1
                    'PROP_DC_1	Reboiler Pressure
                    Me.ReboilerPressure = Converter.ConvertToSI(su.pressure, propval)

            End Select
            Return 1
        End Function
    End Class

    <Serializable()> Public Class ReboiledAbsorber

        Inherits Column

        'solving method (default = IO)

        Public _sm As SolvingMethods.RebAbsColSolvingMethod

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

        Public Sub New(ByVal name As String, ByVal description As String)
            MyBase.New(name, description)
            Me.ColumnType = ColType.ReboiledAbsorber
            MyBase.AddStages()
            For k2 = 0 To Me.Stages.Count - 1
                Me.Stages(k2).P = 101325
            Next
        End Sub

        Public Overloads Overrides Function GetProperties(ByVal proptype As DWSIM.SimulationObjects.UnitOperations.BaseClass.PropertyType) As String()
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

        Public Overrides Function GetPropertyValue(ByVal prop As String, Optional ByVal su As SystemsOfUnits.Units = Nothing) As Object
            If su Is Nothing Then su = New DWSIM.SystemsOfUnits.SI
            Dim cv As New DWSIM.SystemsOfUnits.Converter
            Dim value As Double = 0
            Dim propidx As Integer = Convert.ToInt32(prop.Split("_")(2))

            Select Case propidx

                Case 0
                    'PROP_DC_0	Condenser Pressure
                    value = Converter.ConvertFromSI(su.pressure, Me.CondenserPressure)
                Case 1
                    'PROP_DC_1	Reboiler Pressure
                    value = Converter.ConvertFromSI(su.pressure, Me.ReboilerPressure)
                Case 3
                    'PROP_DC_6	Reboiler Duty
                    value = Converter.ConvertFromSI(su.heatflow, Me.ReboilerDuty)
                Case 4
                    'PROP_DC_7	Number of Stages
                    value = Me.NumberOfStages
            End Select

            Return value
        End Function

        Public Overrides Function GetPropertyUnit(ByVal prop As String, Optional ByVal su As SystemsOfUnits.Units = Nothing) As Object
            If su Is Nothing Then su = New DWSIM.SystemsOfUnits.SI
            Dim cv As New DWSIM.SystemsOfUnits.Converter
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

        Public Overrides Function SetPropertyValue(ByVal prop As String, ByVal propval As Object, Optional ByVal su As SystemsOfUnits.Units = Nothing) As Object
            If su Is Nothing Then su = New DWSIM.SystemsOfUnits.SI
            Dim cv As New DWSIM.SystemsOfUnits.Converter
            Dim propidx As Integer = Convert.ToInt32(prop.Split("_")(2))

            Select Case propidx

                Case 0
                    'PROP_DC_0	Condenser Pressure
                    Me.CondenserPressure = Converter.ConvertToSI(su.pressure, propval)
                Case 1
                    'PROP_DC_1	Reboiler Pressure
                    Me.ReboilerPressure = Converter.ConvertToSI(su.pressure, propval)

            End Select
            Return 1
        End Function
    End Class

    <Serializable()> Public Class RefluxedAbsorber

        Inherits Column

        'solving method (default = IO)

        Public _sm As SolvingMethods.RefAbsColSolvingMethod

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

        Public Sub New(ByVal name As String, ByVal description As String)
            MyBase.New(name, description)
            Me.ColumnType = ColType.RefluxedAbsorber
            MyBase.AddStages()
            For k2 = 0 To Me.Stages.Count - 1
                Me.Stages(k2).P = 101325
            Next
        End Sub

        Public Overloads Overrides Function GetProperties(ByVal proptype As DWSIM.SimulationObjects.UnitOperations.BaseClass.PropertyType) As String()
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

        Public Overrides Function GetPropertyValue(ByVal prop As String, Optional ByVal su As SystemsOfUnits.Units = Nothing) As Object
            If su Is Nothing Then su = New DWSIM.SystemsOfUnits.SI
            Dim cv As New DWSIM.SystemsOfUnits.Converter
            Dim value As Double = 0
            Dim propidx As Integer = Convert.ToInt32(prop.Split("_")(2))

            Select Case propidx

                Case 0
                    'PROP_DC_0	Condenser Pressure
                    value = Converter.ConvertFromSI(su.pressure, Me.CondenserPressure)
                Case 1
                    'PROP_DC_1	Reboiler Pressure
                    value = Converter.ConvertFromSI(su.pressure, Me.ReboilerPressure)
                Case 2
                    'PROP_DC_2	Condenser Pressure Drop
                    value = Converter.ConvertFromSI(su.deltaP, Me.CondenserDeltaP)
                Case 5
                    'PROP_DC_5	Condenser Duty
                    value = Converter.ConvertFromSI(su.heatflow, Me.CondenserDuty)
                Case 6
                    'PROP_DC_7	Number of Stages
                    value = Me.NumberOfStages
            End Select

            Return value
        End Function

        Public Overrides Function GetPropertyUnit(ByVal prop As String, Optional ByVal su As SystemsOfUnits.Units = Nothing) As Object
            If su Is Nothing Then su = New DWSIM.SystemsOfUnits.SI
            Dim cv As New DWSIM.SystemsOfUnits.Converter
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
                Case 5
                    'PROP_DC_5	Condenser Duty
                    value = su.heatflow
                Case 6
                    'PROP_DC_7	Number of Stages
                    value = ""
            End Select

            Return value
        End Function

        Public Overrides Function SetPropertyValue(ByVal prop As String, ByVal propval As Object, Optional ByVal su As SystemsOfUnits.Units = Nothing) As Object
            If su Is Nothing Then su = New DWSIM.SystemsOfUnits.SI
            Dim cv As New DWSIM.SystemsOfUnits.Converter
            Dim propidx As Integer = Convert.ToInt32(prop.Split("_")(2))

            Select Case propidx

                Case 0
                    'PROP_DC_0	Condenser Pressure
                    Me.CondenserPressure = Converter.ConvertToSI(su.pressure, propval)
                Case 1
                    'PROP_DC_1	Reboiler Pressure
                    Me.ReboilerPressure = Converter.ConvertToSI(su.pressure, propval)
                Case 2
                    'PROP_DC_2	Condenser Pressure Drop
                    Me.CondenserDeltaP = Converter.ConvertToSI(su.deltaP, propval)

            End Select
            Return 1
        End Function
    End Class

    <System.Serializable()> Public MustInherit Class Column

        Inherits DWSIM.SimulationObjects.UnitOperations.UnitOpBaseClass

        Public Enum ColType
            DistillationColumn = 0
            AbsorptionColumn = 1
            ReboiledAbsorber = 2
            RefluxedAbsorber = 3
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
        Private _scnderivstep As Double = 0.1
        Private _ionderivstep As Double = 0.01
        Private _iomaxvarchgfac As Integer = 10
        Private _scmaxvarchgfac As Integer = 10
        Private _iodfmin As Double = 0.1#
        Private _iodfmax As Double = 2.0#
        Private _iodeltat_el As Double = 0.01#

        'general variables

        Private _nst As Integer = 12
        Private _rr As Double = 5.0#
        Private _condp As Double = 101325
        Private _rebp As Double = 101325
        Private _conddp, _drate, _vrate, _condd, _rebd As Double
        Private _st As New System.Collections.Generic.List(Of Auxiliary.SepOps.Stage)
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

        'side operations collections

        Private _src As New SideRectifierCollection
        Private _rss As New ReboiledSideStripperCollection
        Private _sss As New SteamedSideStripperCollection
        Private _par As New PumpAroundCollection

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

            If Not _src.Collection Is Nothing Then _src.Collection.Clear()
            For Each xel As XElement In (From xel2 As XElement In data Select xel2 Where xel2.Name = "SideRectifiers").SingleOrDefault.Elements.ToList
                Dim var As New SideRectifier
                var.LoadData(xel.Elements.ToList)
                _src.Collection.Add(xel.@ID, var)
            Next

            If Not _sss.Collection Is Nothing Then _sss.Collection.Clear()
            For Each xel As XElement In (From xel2 As XElement In data Select xel2 Where xel2.Name = "SteamedSideStrippers").SingleOrDefault.Elements.ToList
                Dim var As New SteamedSideStripper
                var.LoadData(xel.Elements.ToList)
                _sss.Collection.Add(xel.@ID, var)
            Next

            If Not _par.Collection Is Nothing Then _par.Collection.Clear()
            For Each xel As XElement In (From xel2 As XElement In data Select xel2 Where xel2.Name = "PumpArounds").SingleOrDefault.Elements.ToList
                Dim var As New PumpAround
                var.LoadData(xel.Elements.ToList)
                _par.Collection.Add(xel.@ID, var)
            Next

            If Not _rss.Collection Is Nothing Then _rss.Collection.Clear()
            For Each xel As XElement In (From xel2 As XElement In data Select xel2 Where xel2.Name = "ReboiledSideStrippers").SingleOrDefault.Elements.ToList
                Dim var As New ReboiledSideStripper
                var.LoadData(xel.Elements.ToList)
                _rss.Collection.Add(xel.@ID, var)
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
                .Add(New XElement("SideRectifiers"))
                For Each kvp As KeyValuePair(Of String, Auxiliary.SepOps.SideRectifier) In _src.Collection
                    .Item(.Count - 1).Add(New XElement("SideRectifier", New XAttribute("ID", kvp.Key), kvp.Value.SaveData.ToArray))
                Next
                .Add(New XElement("SteamedSideStrippers"))
                For Each kvp As KeyValuePair(Of String, Auxiliary.SepOps.SteamedSideStripper) In _sss.Collection
                    .Item(.Count - 1).Add(New XElement("SteamedSideStripper", New XAttribute("ID", kvp.Key), kvp.Value.SaveData.ToArray))
                Next
                .Add(New XElement("PumpArounds"))
                For Each kvp As KeyValuePair(Of String, Auxiliary.SepOps.PumpAround) In _par.Collection
                    .Item(.Count - 1).Add(New XElement("PumpArounds", New XAttribute("ID", kvp.Key), kvp.Value.SaveData.ToArray))
                Next
                .Add(New XElement("ReboiledSideStrippers"))
                For Each kvp As KeyValuePair(Of String, Auxiliary.SepOps.ReboiledSideStripper) In _rss.Collection
                    .Item(.Count - 1).Add(New XElement("ReboiledSideStripper", New XAttribute("ID", kvp.Key), kvp.Value.SaveData.ToArray))
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

        Public Sub New(ByVal name As String, ByVal description As String)

            MyBase.CreateNew()

            ComponentName = name
            ComponentDescription = description

            _st = New System.Collections.Generic.List(Of Stage)

            _conn_ms = New System.Collections.Generic.Dictionary(Of String, StreamInformation)
            _conn_es = New System.Collections.Generic.Dictionary(Of String, StreamInformation)

            _src = New SideRectifierCollection
            _rss = New ReboiledSideStripperCollection
            _sss = New SteamedSideStripperCollection
            _par = New PumpAroundCollection

            __dc = New DummyClass

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
                            _st(_st.Count - 1).Name = DWSIM.App.GetLocalString("DCCondenser")
                        ElseIf i = Me.NumberOfStages - 1 Then
                            _st(_st.Count - 1).Name = DWSIM.App.GetLocalString("DCReboiler")
                        Else
                            _st(_st.Count - 1).Name = DWSIM.App.GetLocalString("DCStage") & "_" & _st.Count - 1
                        End If
                    Case ColType.AbsorptionColumn
                        _st(_st.Count - 1).Name = DWSIM.App.GetLocalString("DCStage") & "_" & _st.Count - 1
                    Case ColType.ReboiledAbsorber
                        If i = Me.NumberOfStages - 1 Then
                            _st(_st.Count - 1).Name = DWSIM.App.GetLocalString("DCReboiler")
                        Else
                            _st(_st.Count - 1).Name = DWSIM.App.GetLocalString("DCStage") & "_" & _st.Count - 1
                        End If
                    Case ColType.RefluxedAbsorber
                        If i = 0 Then
                            _st(_st.Count - 1).Name = DWSIM.App.GetLocalString("DCCondenser")
                        Else
                            _st(_st.Count - 1).Name = DWSIM.App.GetLocalString("DCStage") & "_" & _st.Count - 1
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
                For Each cp As DWSIM.Thermodynamics.BaseClasses.ConstantProperties In Me.FlowSheet.Options.SelectedComponents.Values
                    d.Add(cp.Name, New Parameter)
                Next
                _ie.LiqCompositions.Add(d)
                Dim d2 As New Dictionary(Of String, Parameter)
                For Each cp As DWSIM.Thermodynamics.BaseClasses.ConstantProperties In Me.FlowSheet.Options.SelectedComponents.Values
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
                    _vrateunit = FlowSheet.Options.SelectedUnitSystem.molarflow
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

        Public ReadOnly Property SideRectCol() As SideRectifierCollection
            Get
                Return _src
            End Get
        End Property

        Public ReadOnly Property RebSStrCol() As ReboiledSideStripperCollection
            Get
                Return _rss
            End Get
        End Property

        Public ReadOnly Property StmSStrCol() As SteamedSideStripperCollection
            Get
                Return _sss
            End Get
        End Property

        Public ReadOnly Property PArCol() As PumpAroundCollection
            Get
                Return _par
            End Get
        End Property

        'dummy class

        Public __dc As New DummyClass

        Public Overrides Sub PopulatePropertyGrid(ByVal pgrid As PropertyGridEx.PropertyGridEx, ByVal su As SystemsOfUnits.Units)

            Dim cv As New DWSIM.SystemsOfUnits.Converter
            Dim nf As String = FlowSheet.Options.NumberFormat

            Dim obj = Me.Specs

            With pgrid

                .PropertySort = PropertySort.Categorized
                .ShowCustomProperties = True
                .Item.Clear()

                MyBase.PopulatePropertyGrid(pgrid, su)

                .Item.Add(DWSIM.App.GetLocalString("DCNumbStages"), Me, "NumberOfStages", False, DWSIM.App.GetLocalString("DCStages"), "", True)
                With .Item(.Item.Count - 1)
                    .DefaultValue = Nothing
                End With

                .Item.Add(DWSIM.App.GetLocalString("DCStagesE"), Me.__dc, False, DWSIM.App.GetLocalString("DCStages"), "", True)
                With .Item(.Item.Count - 1)
                    .CustomEditor = New DWSIM.Editors.Distillation.UIStagesEditor
                End With

                Dim val As Double

                Select Case Me.ColumnType
                    Case ColType.DistillationColumn
                        val = Format(Converter.ConvertFromSI(su.pressure, Me.CondenserPressure), nf)
                        .Item.Add(FT(DWSIM.App.GetLocalString("DCCondenserPressure"), su.pressure), val, False, DWSIM.App.GetLocalString("DCColumnProperties"), "", True)
                        .Item(.Item.Count - 1).Tag2 = "PROP_DC_0"
                        val = Format(Converter.ConvertFromSI(su.pressure, Me.ReboilerPressure), nf)
                        .Item.Add(FT(DWSIM.App.GetLocalString("DCReboilerPressure"), su.pressure), val, False, DWSIM.App.GetLocalString("DCColumnProperties"), "", True)
                        .Item(.Item.Count - 1).Tag2 = "PROP_DC_1"
                        .Item.Add(DWSIM.App.GetLocalString("DCCondenserType"), Me, "CondenserType", False, DWSIM.App.GetLocalString("DCColumnProperties"), "", True)
                        val = Format(Converter.ConvertFromSI(su.deltaP, Me.CondenserDeltaP), nf)
                        .Item.Add(FT(DWSIM.App.GetLocalString("DCCondenserDeltaP"), su.deltaP), val, False, DWSIM.App.GetLocalString("DCColumnProperties"), "", True)
                        '.Item.Add(DWSIM.App.GetLocalString("DCRefluxRatio"), Me, "RefluxRatio", False, DWSIM.App.GetLocalString("DCColumnProperties"), "", True)
                        Dim units As String() = New String() {}

                        Dim cspec As New PropertyGridEx.CustomPropertyCollection()
                        With cspec
                            'condenser spec
                            If Me.CondenserType = condtype.Partial_Condenser Then
                                Dim cspecv As New PropertyGridEx.CustomPropertyCollection()
                                With cspecv
                                    .Add(DWSIM.App.GetLocalString("DCVaporFlowRate"), Me, "VaporFlowRate", False, DWSIM.App.GetLocalString("DCCondenserSpec"), "", True)
                                    .Add(DWSIM.App.GetLocalString("DCCondenserSpecUnit"), Me, "VaporFlowRateUnit", False, DWSIM.App.GetLocalString("DCCondenserSpec"))
                                    units = New String() {"mol/s", "lbmol/h", "mol/h", "mol/d", "kmol/s", "kmol/h", "kmol/d", "m3/d @ BR", "m3/d @ NC", "m3/d @ CNTP", "m3/d @ SC", "m3/d @ 0 C, 1 atm", "m3/d @ 15.56 C, 1 atm", "m3/d @ 20 C, 1 atm", "ft3/d @ 60 F, 14.7 psia", "ft3/d @ 0 C, 1 atm"}
                                    With .Item(.Count - 1)
                                        .Choices = New PropertyGridEx.CustomChoices(units, False)
                                    End With
                                End With
                                .Add(DWSIM.App.GetLocalString("DCVaporFlowRate"), cspecv, False, DWSIM.App.GetLocalString("DCCondenserSpec"), "", True)
                                With .Item(.Count - 1)
                                    .IsBrowsable = True
                                    .BrowsableLabelStyle = PropertyGridEx.BrowsableTypeConverter.LabelStyle.lsEllipsis
                                    .CustomEditor = New System.Drawing.Design.UITypeEditor
                                End With
                            End If
                            .Add(DWSIM.App.GetLocalString("DCCondenserSpecType"), Me.Specs("C"), "SType", False, DWSIM.App.GetLocalString("DCCondenserSpec"))
                            If Me.Specs("C").SType = ColumnSpec.SpecType.Component_Fraction Or _
                                Me.Specs("C").SType = ColumnSpec.SpecType.Component_Mass_Flow_Rate Or _
                                Me.Specs("C").SType = ColumnSpec.SpecType.Component_Recovery Or _
                                Me.Specs("C").SType = ColumnSpec.SpecType.Component_Molar_Flow_Rate Then
                                .Add(DWSIM.App.GetLocalString("DCCondenserSpecComp"), DWSIM.App.GetComponentName(Me.Specs("C").ComponentID), False, DWSIM.App.GetLocalString("DCCondenserSpec"))
                                With .Item(.Count - 1)
                                    .CustomEditor = New DWSIM.Editors.Components.UIComponentSelector
                                End With
                            End If
                            .Add(DWSIM.App.GetLocalString("DCCondenserSpecValue"), Me.Specs("C"), "SpecValue", False, DWSIM.App.GetLocalString("DCCondenserSpec"))
                            .Add(DWSIM.App.GetLocalString("DCCondenserSpecUnit"), Me.Specs("C"), "SpecUnit", False, DWSIM.App.GetLocalString("DCCondenserSpec"))
                            Select Case Me.Specs("C").SType
                                Case ColumnSpec.SpecType.Component_Fraction
                                    units = New String() {"M", "We"}
                                Case ColumnSpec.SpecType.Component_Mass_Flow_Rate
                                    units = New String() {"g/s", "lbm/h", "kg/s", "kg/h", "kg/d", "kg/min", "lb/min", "lb/s"}
                                Case ColumnSpec.SpecType.Component_Molar_Flow_Rate
                                    units = New String() {"mol/s", "lbmol/h", "mol/h", "mol/d", "kmol/s", "kmol/h", "kmol/d", "m3/d @ BR", "m3/d @ NC", "m3/d @ CNTP", "m3/d @ SC", "m3/d @ 0 C, 1 atm", "m3/d @ 15.56 C, 1 atm", "m3/d @ 20 C, 1 atm", "ft3/d @ 60 F, 14.7 psia", "ft3/d @ 0 C, 1 atm"}
                                Case ColumnSpec.SpecType.Component_Recovery
                                    units = New String() {"% M/M", "% W/W"}
                                Case ColumnSpec.SpecType.Heat_Duty
                                    units = New String() {"kW", "kcal/h", "BTU/h", "BTU/s", "cal/s", "HP", "kJ/h", "kJ/d", "MW", "W"}
                                Case ColumnSpec.SpecType.Product_Mass_Flow_Rate
                                    units = New String() {"g/s", "lbm/h", "kg/s", "kg/h", "kg/d", "kg/min", "lb/min", "lb/s"}
                                Case ColumnSpec.SpecType.Product_Molar_Flow_Rate
                                    units = New String() {"mol/s", "lbmol/h", "mol/h", "mol/d", "kmol/s", "kmol/h", "kmol/d", "m3/d @ BR", "m3/d @ NC", "m3/d @ CNTP", "m3/d @ SC", "m3/d @ 0 C, 1 atm", "m3/d @ 15.56 C, 1 atm", "m3/d @ 20 C, 1 atm", "ft3/d @ 60 F, 14.7 psia", "ft3/d @ 0 C, 1 atm"}
                                Case ColumnSpec.SpecType.Stream_Ratio
                                    units = New String() {""}
                                Case ColumnSpec.SpecType.Temperature
                                    units = New String() {"K", "R", "C", "F"}
                            End Select
                            With .Item(.Count - 1)
                                .Choices = New PropertyGridEx.CustomChoices(units, False)
                            End With
                        End With
                        .Item.Add(DWSIM.App.GetLocalString("DCCondenserSpecs"), cspec, False, DWSIM.App.GetLocalString("DCColumnProperties"), "", True)
                        With .Item(.Item.Count - 1)
                            .IsBrowsable = True
                            .BrowsableLabelStyle = PropertyGridEx.BrowsableTypeConverter.LabelStyle.lsEllipsis
                            .CustomEditor = New System.Drawing.Design.UITypeEditor
                        End With
                        Dim rspec As New PropertyGridEx.CustomPropertyCollection()
                        With rspec
                            'reboiler spec
                            .Add(DWSIM.App.GetLocalString("DCReboilerSpecType"), Me.Specs("R"), "SType", False, DWSIM.App.GetLocalString("DCReboilerSpec"))
                            If Me.Specs("R").SType = ColumnSpec.SpecType.Component_Fraction Or _
                               Me.Specs("R").SType = ColumnSpec.SpecType.Component_Mass_Flow_Rate Or _
                               Me.Specs("R").SType = ColumnSpec.SpecType.Component_Recovery Or _
                               Me.Specs("R").SType = ColumnSpec.SpecType.Component_Molar_Flow_Rate Then
                                .Add(DWSIM.App.GetLocalString("DCReboilerSpecComp"), DWSIM.App.GetComponentName(Me.Specs("R").ComponentID), False, DWSIM.App.GetLocalString("DCCondenserSpec"))
                                With .Item(.Count - 1)
                                    .CustomEditor = New DWSIM.Editors.Components.UIComponentSelector
                                End With
                            End If
                            .Add(DWSIM.App.GetLocalString("DCReboilerSpecValue"), Me.Specs("R"), "SpecValue", False, DWSIM.App.GetLocalString("DCReboilerSpec"))
                            .Add(DWSIM.App.GetLocalString("DCReboilerSpecUnit"), Me.Specs("R"), "SpecUnit", False, DWSIM.App.GetLocalString("DCReboilerSpec"))
                            Select Case Me.Specs("R").SType
                                Case ColumnSpec.SpecType.Component_Fraction
                                    units = New String() {"M", "We"}
                                Case ColumnSpec.SpecType.Component_Mass_Flow_Rate
                                    units = New String() {"g/s", "lbm/h", "kg/s", "kg/h", "kg/d", "kg/min", "lb/min", "lb/s"}
                                Case ColumnSpec.SpecType.Component_Molar_Flow_Rate
                                    units = New String() {"mol/s", "lbmol/h", "mol/h", "mol/d", "kmol/s", "kmol/h", "kmol/d", "m3/d @ BR", "m3/d @ NC", "m3/d @ CNTP", "m3/d @ SC", "m3/d @ 0 C, 1 atm", "m3/d @ 15.56 C, 1 atm", "m3/d @ 20 C, 1 atm", "ft3/d @ 60 F, 14.7 psia", "ft3/d @ 0 C, 1 atm"}
                                Case ColumnSpec.SpecType.Component_Recovery
                                    units = New String() {"% M/M", "% W/W"}
                                Case ColumnSpec.SpecType.Heat_Duty
                                    units = New String() {"kW", "kcal/h", "BTU/h", "BTU/s", "cal/s", "HP", "kJ/h", "kJ/d", "MW", "W"}
                                Case ColumnSpec.SpecType.Product_Mass_Flow_Rate
                                    units = New String() {"g/s", "lbm/h", "kg/s", "kg/h", "kg/d", "kg/min", "lb/min", "lb/s"}
                                Case ColumnSpec.SpecType.Product_Molar_Flow_Rate
                                    units = New String() {"mol/s", "lbmol/h", "mol/h", "mol/d", "kmol/s", "kmol/h", "kmol/d", "m3/d @ BR", "m3/d @ NC", "m3/d @ CNTP", "m3/d @ SC", "m3/d @ 0 C, 1 atm", "m3/d @ 15.56 C, 1 atm", "m3/d @ 20 C, 1 atm", "ft3/d @ 60 F, 14.7 psia", "ft3/d @ 0 C, 1 atm"}
                                Case ColumnSpec.SpecType.Stream_Ratio
                                    units = New String() {""}
                                Case ColumnSpec.SpecType.Temperature
                                    units = New String() {"K", "R", "C", "F"}
                            End Select
                            With .Item(.Count - 1)
                                .Choices = New PropertyGridEx.CustomChoices(units, False)
                            End With
                        End With
                        .Item.Add(DWSIM.App.GetLocalString("DCReboilerSpecs"), rspec, False, DWSIM.App.GetLocalString("DCColumnProperties"), "", True)
                        With .Item(.Item.Count - 1)
                            .IsBrowsable = True
                            .BrowsableLabelStyle = PropertyGridEx.BrowsableTypeConverter.LabelStyle.lsEllipsis
                            .CustomEditor = New System.Drawing.Design.UITypeEditor
                        End With
                    Case ColType.AbsorptionColumn
                        val = Format(Converter.ConvertFromSI(su.pressure, Me.CondenserPressure), nf)
                        .Item.Add(FT(DWSIM.App.GetLocalString("DCFirstStgPressure"), su.pressure), val, False, DWSIM.App.GetLocalString("DCColumnProperties"), "", True)
                        val = Format(Converter.ConvertFromSI(su.pressure, Me.ReboilerPressure), nf)
                        .Item.Add(FT(DWSIM.App.GetLocalString("DCLastStgPressure"), su.pressure), val, False, DWSIM.App.GetLocalString("DCColumnProperties"), "", True)
                        .Item.Add(DWSIM.App.GetLocalString("DCOperationMode"), Me, "OperationMode", False, DWSIM.App.GetLocalString("DCColumnProperties"), "", True)
                    Case ColType.ReboiledAbsorber
                        val = Format(Converter.ConvertFromSI(su.pressure, Me.CondenserPressure), nf)
                        .Item.Add(FT(DWSIM.App.GetLocalString("DCFirstStgPressure"), su.pressure), val, False, DWSIM.App.GetLocalString("DCColumnProperties"), "", True)
                        val = Format(Converter.ConvertFromSI(su.pressure, Me.ReboilerPressure), nf)
                        .Item.Add(FT(DWSIM.App.GetLocalString("DCReboilerPressure"), su.pressure), val, False, DWSIM.App.GetLocalString("DCColumnProperties"), "", True)
                        Dim units As String() = New String() {}
                        Dim rspec As New PropertyGridEx.CustomPropertyCollection()
                        With rspec
                            'reboiler spec
                            .Add(DWSIM.App.GetLocalString("DCReboilerSpecType"), Me.Specs("R"), "SType", False, DWSIM.App.GetLocalString("DCReboilerSpec"))
                            If Me.Specs("R").SType = ColumnSpec.SpecType.Component_Fraction Or _
                               Me.Specs("R").SType = ColumnSpec.SpecType.Component_Mass_Flow_Rate Or _
                               Me.Specs("R").SType = ColumnSpec.SpecType.Component_Recovery Or _
                               Me.Specs("R").SType = ColumnSpec.SpecType.Component_Molar_Flow_Rate Then
                                .Add(DWSIM.App.GetLocalString("DCReboilerSpecComp"), DWSIM.App.GetComponentName(Me.Specs("R").ComponentID), False, DWSIM.App.GetLocalString("DCCondenserSpec"))
                                With .Item(.Count - 1)
                                    .CustomEditor = New DWSIM.Editors.Components.UIComponentSelector
                                End With
                            End If
                            .Add(DWSIM.App.GetLocalString("DCReboilerSpecValue"), Me.Specs("R"), "SpecValue", False, DWSIM.App.GetLocalString("DCReboilerSpec"))
                            .Add(DWSIM.App.GetLocalString("DCReboilerSpecUnit"), Me.Specs("R"), "SpecUnit", False, DWSIM.App.GetLocalString("DCReboilerSpec"))
                            Select Case Me.Specs("R").SType
                                Case ColumnSpec.SpecType.Component_Fraction
                                    units = New String() {"M", "We"}
                                Case ColumnSpec.SpecType.Component_Mass_Flow_Rate
                                    units = New String() {"g/s", "lbm/h", "kg/s", "kg/h", "kg/d", "kg/min", "lb/min", "lb/s"}
                                Case ColumnSpec.SpecType.Component_Molar_Flow_Rate
                                    units = New String() {"mol/s", "lbmol/h", "mol/h", "mol/d", "kmol/s", "kmol/h", "kmol/d", "m3/d @ BR", "m3/d @ NC", "m3/d @ CNTP", "m3/d @ SC", "m3/d @ 0 C, 1 atm", "m3/d @ 15.56 C, 1 atm", "m3/d @ 20 C, 1 atm", "ft3/d @ 60 F, 14.7 psia", "ft3/d @ 0 C, 1 atm"}
                                Case ColumnSpec.SpecType.Component_Recovery
                                    units = New String() {"% M/M", "% W/W"}
                                Case ColumnSpec.SpecType.Heat_Duty
                                    units = New String() {"kW", "kcal/h", "BTU/h", "BTU/s", "cal/s", "HP", "kJ/h", "kJ/d", "MW", "W"}
                                Case ColumnSpec.SpecType.Product_Mass_Flow_Rate
                                    units = New String() {"g/s", "lbm/h", "kg/s", "kg/h", "kg/d", "kg/min", "lb/min", "lb/s"}
                                Case ColumnSpec.SpecType.Product_Molar_Flow_Rate
                                    units = New String() {"mol/s", "lbmol/h", "mol/h", "mol/d", "kmol/s", "kmol/h", "kmol/d", "m3/d @ BR", "m3/d @ NC", "m3/d @ CNTP", "m3/d @ SC", "m3/d @ 0 C, 1 atm", "m3/d @ 15.56 C, 1 atm", "m3/d @ 20 C, 1 atm", "ft3/d @ 60 F, 14.7 psia", "ft3/d @ 0 C, 1 atm"}
                                Case ColumnSpec.SpecType.Stream_Ratio
                                    units = New String() {""}
                                Case ColumnSpec.SpecType.Temperature
                                    units = New String() {"K", "R", "C", "F"}
                            End Select
                            With .Item(.Count - 1)
                                .Choices = New PropertyGridEx.CustomChoices(units, False)
                            End With
                        End With
                        .Item.Add(DWSIM.App.GetLocalString("DCReboilerSpecs"), rspec, False, DWSIM.App.GetLocalString("DCColumnProperties"), "", True)
                        With .Item(.Item.Count - 1)
                            .IsBrowsable = True
                            .BrowsableLabelStyle = PropertyGridEx.BrowsableTypeConverter.LabelStyle.lsEllipsis
                            .CustomEditor = New System.Drawing.Design.UITypeEditor
                        End With
                    Case ColType.RefluxedAbsorber
                        val = Format(Converter.ConvertFromSI(su.pressure, Me.CondenserPressure), nf)
                        .Item.Add(FT(DWSIM.App.GetLocalString("DCCondenserPressure"), su.pressure), val, False, DWSIM.App.GetLocalString("DCColumnProperties"), "", True)
                        val = Format(Converter.ConvertFromSI(su.pressure, Me.ReboilerPressure), nf)
                        val = Format(Converter.ConvertFromSI(su.pressure, Me.ReboilerPressure), nf)
                        .Item.Add(FT(DWSIM.App.GetLocalString("DCLastStgPressure"), su.pressure), val, False, DWSIM.App.GetLocalString("DCColumnProperties"), "", True)
                        .Item.Add(DWSIM.App.GetLocalString("DCCondenserType"), Me, "CondenserType", False, DWSIM.App.GetLocalString("DCColumnProperties"), "", True)
                        val = Format(Converter.ConvertFromSI(su.deltaP, Me.CondenserDeltaP), nf)
                        .Item.Add(FT(DWSIM.App.GetLocalString("DCCondenserDeltaP"), su.deltaP), val, False, DWSIM.App.GetLocalString("DCColumnProperties"), "", True)
                        Dim units As String() = New String() {}
                        If Not Me.CondenserType = condtype.Full_Reflux Then
                            Dim cspec As New PropertyGridEx.CustomPropertyCollection()
                            With cspec
                                'condenser spec
                                If Me.CondenserType = condtype.Full_Reflux Or Me.CondenserType = condtype.Partial_Condenser Then
                                    Dim cspecv As New PropertyGridEx.CustomPropertyCollection()
                                    With cspecv
                                        .Add(DWSIM.App.GetLocalString("DCVaporFlowRate"), Me, "VaporFlowRate", False, DWSIM.App.GetLocalString("DCCondenserSpec"), "", True)
                                        .Add(DWSIM.App.GetLocalString("DCCondenserSpecUnit"), Me, "VaporFlowRateUnit", False, DWSIM.App.GetLocalString("DCCondenserSpec"))
                                        units = New String() {"mol/s", "lbmol/h", "mol/h", "mol/d", "kmol/s", "kmol/h", "kmol/d", "m3/d @ BR", "m3/d @ NC", "m3/d @ CNTP", "m3/d @ SC", "m3/d @ 0 C, 1 atm", "m3/d @ 15.56 C, 1 atm", "m3/d @ 20 C, 1 atm", "ft3/d @ 60 F, 14.7 psia", "ft3/d @ 0 C, 1 atm"}
                                        With .Item(.Count - 1)
                                            .Choices = New PropertyGridEx.CustomChoices(units, False)
                                        End With
                                    End With
                                    .Add(DWSIM.App.GetLocalString("DCVaporFlowRate"), cspecv, False, DWSIM.App.GetLocalString("DCCondenserSpec"), "", True)
                                    With .Item(.Count - 1)
                                        .IsBrowsable = True
                                        .BrowsableLabelStyle = PropertyGridEx.BrowsableTypeConverter.LabelStyle.lsEllipsis
                                        .CustomEditor = New System.Drawing.Design.UITypeEditor
                                    End With
                                End If

                                .Add(DWSIM.App.GetLocalString("DCCondenserSpecType"), Me.Specs("C"), "SType", False, DWSIM.App.GetLocalString("DCCondenserSpec"))
                                If Me.Specs("C").SType = ColumnSpec.SpecType.Component_Fraction Or _
                                    Me.Specs("C").SType = ColumnSpec.SpecType.Component_Mass_Flow_Rate Or _
                                    Me.Specs("C").SType = ColumnSpec.SpecType.Component_Recovery Or _
                                    Me.Specs("C").SType = ColumnSpec.SpecType.Component_Molar_Flow_Rate Then
                                    .Add(DWSIM.App.GetLocalString("DCCondenserSpecComp"), DWSIM.App.GetComponentName(Me.Specs("C").ComponentID), False, DWSIM.App.GetLocalString("DCCondenserSpec"))
                                    With .Item(.Count - 1)
                                        .CustomEditor = New DWSIM.Editors.Components.UIComponentSelector
                                    End With
                                End If
                                .Add(DWSIM.App.GetLocalString("DCCondenserSpecValue"), Me.Specs("C"), "SpecValue", False, DWSIM.App.GetLocalString("DCCondenserSpec"))
                                .Add(DWSIM.App.GetLocalString("DCCondenserSpecUnit"), Me.Specs("C"), "SpecUnit", False, DWSIM.App.GetLocalString("DCCondenserSpec"))
                                Select Case Me.Specs("C").SType
                                    Case ColumnSpec.SpecType.Component_Fraction
                                        units = New String() {"M", "We"}
                                    Case ColumnSpec.SpecType.Component_Mass_Flow_Rate
                                        units = New String() {"g/s", "lbm/h", "kg/s", "kg/h", "kg/d", "kg/min", "lb/min", "lb/s"}
                                    Case ColumnSpec.SpecType.Component_Molar_Flow_Rate
                                        units = New String() {"mol/s", "lbmol/h", "mol/h", "mol/d", "kmol/s", "kmol/h", "kmol/d", "m3/d @ BR", "m3/d @ NC", "m3/d @ CNTP", "m3/d @ SC", "m3/d @ 0 C, 1 atm", "m3/d @ 15.56 C, 1 atm", "m3/d @ 20 C, 1 atm", "ft3/d @ 60 F, 14.7 psia", "ft3/d @ 0 C, 1 atm"}
                                    Case ColumnSpec.SpecType.Component_Recovery
                                        units = New String() {"% M/M", "% W/W"}
                                    Case ColumnSpec.SpecType.Heat_Duty
                                        units = New String() {"kW", "kcal/h", "BTU/h", "BTU/s", "cal/s", "HP", "kJ/h", "kJ/d", "MW", "W"}
                                    Case ColumnSpec.SpecType.Product_Mass_Flow_Rate
                                        units = New String() {"g/s", "lbm/h", "kg/s", "kg/h", "kg/d", "kg/min", "lb/min", "lb/s"}
                                    Case ColumnSpec.SpecType.Product_Molar_Flow_Rate
                                        units = New String() {"mol/s", "lbmol/h", "mol/h", "mol/d", "kmol/s", "kmol/h", "kmol/d", "m3/d @ BR", "m3/d @ NC", "m3/d @ CNTP", "m3/d @ SC", "m3/d @ 0 C, 1 atm", "m3/d @ 15.56 C, 1 atm", "m3/d @ 20 C, 1 atm", "ft3/d @ 60 F, 14.7 psia", "ft3/d @ 0 C, 1 atm"}
                                    Case ColumnSpec.SpecType.Stream_Ratio
                                        units = New String() {""}
                                    Case ColumnSpec.SpecType.Temperature
                                        units = New String() {"K", "R", "C", "F"}
                                End Select
                                With .Item(.Count - 1)
                                    .Choices = New PropertyGridEx.CustomChoices(units, False)
                                End With
                            End With
                            .Item.Add(DWSIM.App.GetLocalString("DCCondenserSpecs"), cspec, False, DWSIM.App.GetLocalString("DCColumnProperties"), "", True)
                            With .Item(.Item.Count - 1)
                                .IsBrowsable = True
                                .BrowsableLabelStyle = PropertyGridEx.BrowsableTypeConverter.LabelStyle.lsEllipsis
                                .CustomEditor = New System.Drawing.Design.UITypeEditor
                            End With
                        End If
                End Select

                .Item.Add(DWSIM.App.GetLocalString("DCEditConnections"), Me.__dc, False, DWSIM.App.GetLocalString("DCConnections"), "", True)
                With .Item(.Item.Count - 1)
                    .CustomEditor = New DWSIM.Editors.Distillation.UIConnectionsEditor
                End With

                .Item.Add(DWSIM.App.GetLocalString("DCUseIE"), Me, "UseTemperatureEstimates", False, DWSIM.App.GetLocalString("DCInitialEstimates"), "", True)
                .Item.Add(DWSIM.App.GetLocalString("DCUseIE1"), Me, "UseVaporFlowEstimates", False, DWSIM.App.GetLocalString("DCInitialEstimates"), "", True)
                .Item.Add(DWSIM.App.GetLocalString("DCUseIE3"), Me, "UseLiquidFlowEstimates", False, DWSIM.App.GetLocalString("DCInitialEstimates"), "", True)
                .Item.Add(DWSIM.App.GetLocalString("DCUseIE2"), Me, "UseCompositionEstimates", False, DWSIM.App.GetLocalString("DCInitialEstimates"), "", True)
                .Item.Add(DWSIM.App.GetLocalString("DCInitialEstimates2"), Me.__dc, False, DWSIM.App.GetLocalString("DCInitialEstimates"), "", True)
                With .Item(.Item.Count - 1)
                    .CustomEditor = New DWSIM.Editors.Distillation.UIInitialEstimates
                End With
                .Item.Add(DWSIM.App.GetLocalString("DCAutoUpdInitEst"), Me, "AutoUpdateInitialEstimates", False, DWSIM.App.GetLocalString("DCInitialEstimates"), "", True)

                If Me.SolvingMethod = SolvingMethods.DistColSolvingMethod.Russell_InsideOut Or _
                            Me.SolvingMethod = SolvingMethods.AbsColSolvingMethod.Russell_InsideOut Or _
                            Me.SolvingMethod = SolvingMethods.RebAbsColSolvingMethod.Russell_InsideOut Or _
                            Me.SolvingMethod = SolvingMethods.RefAbsColSolvingMethod.Russell_InsideOut Then
                    .Item.Add(DWSIM.App.GetLocalString("DCILTol"), Me, "InternalLoopTolerance", False, DWSIM.App.GetLocalString("DCTolerances"), "", True)
                End If
                .Item.Add(DWSIM.App.GetLocalString("DCELTol"), Me, "ExternalLoopTolerance", False, DWSIM.App.GetLocalString("DCTolerances"), "", True)
                .Item.Add(DWSIM.App.GetLocalString("DCMaxIt"), Me, "MaxIterations", False, DWSIM.App.GetLocalString("DCTolerances"), "", True)

                .Item.Add(DWSIM.App.GetLocalString("DCSolvingMethod"), Me, "_sm", False, DWSIM.App.GetLocalString("DCSolvingMethod1"), "", True)
                If Me.SolvingMethod = SolvingMethods.DistColSolvingMethod.WangHenke_BubblePoint Then
                    .Item.Add(DWSIM.App.GetLocalString("DCStopAtIter"), Me, "StopAtIterationNumber", False, DWSIM.App.GetLocalString("DCSolvingMethod1"), "", True)
                ElseIf Me.SolvingMethod = SolvingMethods.DistColSolvingMethod.Russell_InsideOut Or _
                        Me.SolvingMethod = SolvingMethods.AbsColSolvingMethod.Russell_InsideOut Or _
                        Me.SolvingMethod = SolvingMethods.RebAbsColSolvingMethod.Russell_InsideOut Or _
                        Me.SolvingMethod = SolvingMethods.RefAbsColSolvingMethod.Russell_InsideOut Then
                    '.Item.Add(DWSIM.App.GetLocalString("DC_IO_MaxVarChgFac"), Me, "IO_MaxVarChgFac", False, DWSIM.App.GetLocalString("DCSolvingMethod1"), "", True)
                    .Item.Add(DWSIM.App.GetLocalString("DCAdjustSb"), Me, "AdjustSb", False, DWSIM.App.GetLocalString("DCSolvingMethod1"), "", True)
                    .Item.Add(DWSIM.App.GetLocalString("DC_UseWeightedAverageKbj"), Me, "KbjWeightedAverage", False, DWSIM.App.GetLocalString("DCSolvingMethod1"), "", True)
                    .Item.Add(DWSIM.App.GetLocalString("DCUseIMJac"), Me, "UseIdentityAsJacobianInverse", False, DWSIM.App.GetLocalString("DCSolvingMethod1"), "", True)
                    .Item.Add(DWSIM.App.GetLocalString("DCUseDampingFactor"), Me, "UseDampingFactor", False, DWSIM.App.GetLocalString("DCSolvingMethod1"), "", True)
                    .Item.Add(DWSIM.App.GetLocalString("DCDampingFactorMin"), Me, "IO_DampingFactorMin", False, DWSIM.App.GetLocalString("DCSolvingMethod1"), "[0.0 - 0.2]", True)
                    .Item.Add(DWSIM.App.GetLocalString("DCDampingFactorMax"), Me, "IO_DampingFactorMax", False, DWSIM.App.GetLocalString("DCSolvingMethod1"), "[1.0 - 2.0]", True)
                    .Item.Add(DWSIM.App.GetLocalString("DCExtLoopDeltaT"), Me, "IO_ExtLoop_DeltaT", False, DWSIM.App.GetLocalString("DCSolvingMethod1"), "[0.01 - 1]", True)
                    .Item.Add(DWSIM.App.GetLocalString("DCUseNewtonUpdate"), Me, "UseNewtonUpdate", False, DWSIM.App.GetLocalString("DCSolvingMethod1"), "", True)
                    .Item.Add(DWSIM.App.GetLocalString("DC_SC_NumericalDerivativeStep"), Me, "IO_NumericalDerivativeStep", False, DWSIM.App.GetLocalString("DCSolvingMethod1"), "", True)
                    .Item.Add(DWSIM.App.GetLocalString("DCStoreAndReuseJacobian"), Me, "StoreAndReuseJacobian", False, DWSIM.App.GetLocalString("DCSolvingMethod1"), "", True)
                ElseIf Me.SolvingMethod = SolvingMethods.DistColSolvingMethod.NaphtaliSandholm_SimultaneousCorrection Or _
                       Me.SolvingMethod = SolvingMethods.RebAbsColSolvingMethod.NaphtaliSandholm_SimultaneousCorrection Or _
                       Me.SolvingMethod = SolvingMethods.RefAbsColSolvingMethod.NaphtaliSandholm_SimultaneousCorrection Or _
                        Me.SolvingMethod = SolvingMethods.AbsColSolvingMethod.NaphtaliSandholm_SimultaneousCorrection Then
                    .Item.Add(DWSIM.App.GetLocalString("DC_SC_NumericalDerivativeStep"), Me, "SC_NumericalDerivativeStep", False, DWSIM.App.GetLocalString("DCSolvingMethod1"), "", True)
                    val = Format(Converter.ConvertFromSI(su.deltaT, Me.SC_MaximumTemperatureChange), nf)
                    .Item.Add(FT(DWSIM.App.GetLocalString("DC_SC_MaximumTemperatureChange"), su.deltaT), val, False, DWSIM.App.GetLocalString("DCSolvingMethod1"), "", True)
                    .Item.Add(DWSIM.App.GetLocalString("DCUseDampingFactor"), Me, "UseDampingFactor", False, DWSIM.App.GetLocalString("DCSolvingMethod1"), "", True)
                    .Item.Add(DWSIM.App.GetLocalString("DCUseNewtonUpdate"), Me, "UseNewtonUpdate", False, DWSIM.App.GetLocalString("DCSolvingMethod1"), "", True)
                    .Item.Add(DWSIM.App.GetLocalString("DCStoreAndReuseJacobian"), Me, "StoreAndReuseJacobian", False, DWSIM.App.GetLocalString("DCSolvingMethod1"), "", True)
                End If

                If Me.GraphicObject.Calculated Then
                    If Me.SolvingMethod = SolvingMethods.DistColSolvingMethod.Russell_InsideOut Or _
                        Me.SolvingMethod = SolvingMethods.AbsColSolvingMethod.Russell_InsideOut Or _
                        Me.SolvingMethod = SolvingMethods.RebAbsColSolvingMethod.Russell_InsideOut Or _
                        Me.SolvingMethod = SolvingMethods.RefAbsColSolvingMethod.Russell_InsideOut Then
                        .Item.Add(DWSIM.App.GetLocalString("DCILIts"), Me, "ic", True, DWSIM.App.GetLocalString("DCResults"), "", True)
                        .Item.Add(DWSIM.App.GetLocalString("DCELIts"), Me, "ec", True, DWSIM.App.GetLocalString("DCResults"), "", True)
                    ElseIf Me.SolvingMethod = SolvingMethods.DistColSolvingMethod.NaphtaliSandholm_SimultaneousCorrection Then
                        .Item.Add(DWSIM.App.GetLocalString("DCELIts"), Me, "ec", True, DWSIM.App.GetLocalString("DCResults"), "", True)
                    Else
                        .Item.Add(DWSIM.App.GetLocalString("DCELIts"), Me, "ic", True, DWSIM.App.GetLocalString("DCResults"), "", True)
                    End If
                    Select Case Me.ColumnType
                        Case ColType.DistillationColumn
                            val = Format(Converter.ConvertFromSI(su.heatflow, Me.CondenserDuty), nf)
                            .Item.Add(FT(DWSIM.App.GetLocalString("DCCondenserDuty"), su.heatflow), val, True, DWSIM.App.GetLocalString("DCResults"), "", True)
                            val = Format(Converter.ConvertFromSI(su.heatflow, Me.ReboilerDuty), nf)
                            .Item.Add(FT(DWSIM.App.GetLocalString("DCReboilerDuty"), su.heatflow), val, True, DWSIM.App.GetLocalString("DCResults"), "", True)
                        Case ColType.AbsorptionColumn
                        Case ColType.ReboiledAbsorber
                            val = Format(Converter.ConvertFromSI(su.heatflow, Me.ReboilerDuty), nf)
                            .Item.Add(FT(DWSIM.App.GetLocalString("DCReboilerDuty"), su.heatflow), val, True, DWSIM.App.GetLocalString("DCResults"), "", True)
                        Case ColType.RefluxedAbsorber
                            val = Format(Converter.ConvertFromSI(su.heatflow, Me.CondenserDuty), nf)
                            .Item.Add(FT(DWSIM.App.GetLocalString("DCCondenserDuty"), su.heatflow), val, True, DWSIM.App.GetLocalString("DCResults"), "", True)
                    End Select
                    .Item.Add(DWSIM.App.GetLocalString("DCResults1"), __dc, False, DWSIM.App.GetLocalString("DCResults"), "", True)
                    With .Item(.Item.Count - 1)
                        .CustomEditor = New DWSIM.Editors.Distillation.UIResults
                    End With

                End If

                'Dim trig As Boolean = False

                '.Item.Add("Trigger", trig, False, DWSIM.App.GetLocalString("Outros"), "", True)

                .ExpandAllGridItems()

            End With
        End Sub

        Public Overrides Sub PropertyValueChanged(ByVal s As Object, ByVal e As System.Windows.Forms.PropertyValueChangedEventArgs)

            MyBase.PropertyValueChanged(s, e)

            Dim su As DWSIM.SystemsOfUnits.Units = FlowSheet.Options.SelectedUnitSystem
            Dim cv As New DWSIM.SystemsOfUnits.Converter

            If e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("DCNumbStages")) Then

                Dim ne As Integer = e.ChangedItem.Value
                Dim nep As Integer = _st.Count
                Dim dif As Integer = ne - nep
                If dif < 0 Then
                    Me.Stages.RemoveRange(nep + dif - 1, -dif)
                    With Me.InitialEstimates
                        .LiqCompositions.RemoveRange(nep + dif - 1, -dif)
                        .VapCompositions.RemoveRange(nep + dif - 1, -dif)
                        .LiqMolarFlows.RemoveRange(nep + dif - 1, -dif)
                        .VapMolarFlows.RemoveRange(nep + dif - 1, -dif)
                        .StageTemps.RemoveRange(nep + dif - 1, -dif)
                    End With
                ElseIf dif > 0 Then
                    Dim i As Integer
                    For i = 1 To dif
                        Me.Stages.Insert(Me.Stages.Count - 1, New Stage(Guid.NewGuid().ToString))
                        Me.Stages(Me.Stages.Count - 2).Name = DWSIM.App.GetLocalString("DCStage") & "_" & _st.Count - 2
                        With Me.InitialEstimates
                            Dim d As New Dictionary(Of String, Parameter)
                            For Each cp As DWSIM.Thermodynamics.BaseClasses.ConstantProperties In Me.FlowSheet.Options.SelectedComponents.Values
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

            ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("DCCondenserDeltaP")) Then

                Me.CondenserDeltaP = Converter.ConvertToSI(su.deltaP, e.ChangedItem.Value)

            ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("DCCondenserPressure")) Or _
            e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("DCFirstStgPressure")) Then

                Me.CondenserPressure = Converter.ConvertToSI(su.pressure, e.ChangedItem.Value)
                Me.Stages(0).P = Me.CondenserPressure

            ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("DCReboilerPressure")) Or _
            e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("DCLastStgPressure")) Then

                Me.ReboilerPressure = Converter.ConvertToSI(su.pressure, e.ChangedItem.Value)
                Me.Stages(Me.Stages.Count - 1).P = Me.ReboilerPressure

            ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("DCDistillateFlowRate")) Then

                Me.DistillateFlowRate = Converter.ConvertToSI(su.molarflow, e.ChangedItem.Value)

            ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("DCCondenserType")) Then

                Me.GraphicObject.Shape = Me.CondenserType

                Select Case Me.CondenserType
                    Case condtype.Full_Reflux

                        For Each si As StreamInformation In Me.MaterialStreams.Values
                            If si.StreamBehavior = StreamInformation.Behavior.Distillate Then
                                'disconnect and remove from collection
                                If FlowSheet.Collections.GraphicObjectCollection.ContainsKey(si.StreamID) Then
                                    Dim idx As Integer = FormFlowsheet.SearchSurfaceObjectsByName(si.StreamID, FlowSheet.FormSurface.FlowsheetDesignSurface).InputConnectors(0).AttachedConnector.AttachedFromConnectorIndex
                                    FlowSheet.DisconnectObject(Me.GraphicObject, FormFlowsheet.SearchSurfaceObjectsByName(si.StreamID, FlowSheet.FormSurface.FlowsheetDesignSurface))
                                    'Me.GraphicObject.OutputConnectors.RemoveAt(idx)
                                End If
                                Me.MaterialStreams.Remove(si.ID)
                                Exit For
                            End If
                        Next

                    Case condtype.Partial_Condenser

                    Case condtype.Total_Condenser

                        For Each si As StreamInformation In Me.MaterialStreams.Values
                            If si.StreamBehavior = StreamInformation.Behavior.OverheadVapor Then
                                'disconnect and remove from collection
                                If FlowSheet.Collections.GraphicObjectCollection.ContainsKey(si.StreamID) Then
                                    Dim idx As Integer = FormFlowsheet.SearchSurfaceObjectsByName(si.StreamID, FlowSheet.FormSurface.FlowsheetDesignSurface).InputConnectors(0).AttachedConnector.AttachedFromConnectorIndex
                                    FlowSheet.DisconnectObject(Me.GraphicObject, FormFlowsheet.SearchSurfaceObjectsByName(si.StreamID, FlowSheet.FormSurface.FlowsheetDesignSurface))
                                    'Me.GraphicObject.OutputConnectors.RemoveAt(idx)
                                End If
                                Me.MaterialStreams.Remove(si.ID)
                                Exit For
                            End If
                        Next

                End Select

            ElseIf e.ChangedItem.Label = DWSIM.App.GetLocalString("DCCondenserSpecComp") And e.ChangedItem.Parent.Label = DWSIM.App.GetLocalString("DCCondenserSpecs") Then
                Me.Specs("C").ComponentID = e.ChangedItem.Value
            ElseIf e.ChangedItem.Label = DWSIM.App.GetLocalString("DCReboilerSpecComp") And e.ChangedItem.Parent.Label = DWSIM.App.GetLocalString("DCReboilerSpecs") Then
                Me.Specs("R").ComponentID = e.ChangedItem.Value
            ElseIf e.ChangedItem.Label.Contains(DWSIM.App.GetLocalString("DC_SC_MaximumTemperatureChange")) Then
                Me.SC_MaximumTemperatureChange = Converter.ConvertToSI(su.deltaT, e.ChangedItem.Value)
            End If


        End Sub

        Public Sub CheckConnPos()
            Dim idx As Integer
            For Each strinfo As StreamInformation In Me.MaterialStreams.Values
                Try
                    Select Case strinfo.StreamBehavior
                        Case StreamInformation.Behavior.Feed
                            idx = FlowSheet.Collections.GraphicObjectCollection(strinfo.StreamID).OutputConnectors(0).AttachedConnector.AttachedToConnectorIndex
                            If Me.GraphicObject.FlippedH Then
                                Me.GraphicObject.InputConnectors(idx).Position = New Point(Me.GraphicObject.X + Me.GraphicObject.Width, Me.GraphicObject.Y + Me.StageIndex(strinfo.AssociatedStage) / Me.NumberOfStages * Me.GraphicObject.Height)
                            Else
                                Me.GraphicObject.InputConnectors(idx).Position = New Point(Me.GraphicObject.X, Me.GraphicObject.Y + Me.StageIndex(strinfo.AssociatedStage) / Me.NumberOfStages * Me.GraphicObject.Height)
                            End If
                        Case StreamInformation.Behavior.Distillate
                            idx = FlowSheet.Collections.GraphicObjectCollection(strinfo.StreamID).InputConnectors(0).AttachedConnector.AttachedFromConnectorIndex
                            If Not Me.GraphicObject.FlippedH Then
                                Me.GraphicObject.OutputConnectors(idx).Position = New Point(Me.GraphicObject.X + Me.GraphicObject.Width, Me.GraphicObject.Y + 0.3 * Me.GraphicObject.Height)
                            Else
                                Me.GraphicObject.OutputConnectors(idx).Position = New Point(Me.GraphicObject.X, Me.GraphicObject.Y + 0.3 * Me.GraphicObject.Height)
                            End If
                        Case StreamInformation.Behavior.BottomsLiquid
                            idx = FlowSheet.Collections.GraphicObjectCollection(strinfo.StreamID).InputConnectors(0).AttachedConnector.AttachedFromConnectorIndex
                            If Not Me.GraphicObject.FlippedH Then
                                Me.GraphicObject.OutputConnectors(idx).Position = New Point(Me.GraphicObject.X + Me.GraphicObject.Width, Me.GraphicObject.Y + 0.98 * Me.GraphicObject.Height)
                            Else
                                Me.GraphicObject.OutputConnectors(idx).Position = New Point(Me.GraphicObject.X, Me.GraphicObject.Y + 0.98 * Me.GraphicObject.Height)
                            End If
                        Case StreamInformation.Behavior.OverheadVapor
                            idx = FlowSheet.Collections.GraphicObjectCollection(strinfo.StreamID).InputConnectors(0).AttachedConnector.AttachedFromConnectorIndex
                            If Not Me.GraphicObject.FlippedH Then
                                Me.GraphicObject.OutputConnectors(idx).Position = New Point(Me.GraphicObject.X + Me.GraphicObject.Width, Me.GraphicObject.Y + 0.02 * Me.GraphicObject.Height)
                            Else
                                Me.GraphicObject.OutputConnectors(idx).Position = New Point(Me.GraphicObject.X, Me.GraphicObject.Y + 0.02 * Me.GraphicObject.Height)
                            End If
                        Case StreamInformation.Behavior.Sidedraw
                            idx = FlowSheet.Collections.GraphicObjectCollection(strinfo.StreamID).InputConnectors(0).AttachedConnector.AttachedFromConnectorIndex
                            If Me.GraphicObject.FlippedH Then
                                Me.GraphicObject.OutputConnectors(idx).Position = New Point(Me.GraphicObject.X, Me.GraphicObject.Y + Me.StageIndex(strinfo.AssociatedStage) / Me.NumberOfStages * Me.GraphicObject.Height)
                            Else
                                Me.GraphicObject.OutputConnectors(idx).Position = New Point(Me.GraphicObject.X + Me.GraphicObject.Width, Me.GraphicObject.Y + Me.StageIndex(strinfo.AssociatedStage) / Me.NumberOfStages * Me.GraphicObject.Height)
                            End If
                    End Select
                Catch ex As Exception
                End Try
            Next

            For Each strinfo As StreamInformation In Me.EnergyStreams.Values
                Try
                    idx = FlowSheet.Collections.GraphicObjectCollection(strinfo.StreamID).InputConnectors(0).AttachedConnector.AttachedFromConnectorIndex
                    Select Case strinfo.StreamBehavior
                        Case StreamInformation.Behavior.Distillate
                            If Me.GraphicObject.FlippedH Then
                                Me.GraphicObject.OutputConnectors(idx).Position = New Point(Me.GraphicObject.X, Me.GraphicObject.Y + 0.08 * Me.GraphicObject.Height)
                            Else
                                Me.GraphicObject.OutputConnectors(idx).Position = New Point(Me.GraphicObject.X + Me.GraphicObject.Width, Me.GraphicObject.Y + 0.08 * Me.GraphicObject.Height)
                            End If
                        Case StreamInformation.Behavior.BottomsLiquid
                            If Me.GraphicObject.FlippedH Then
                                Me.GraphicObject.OutputConnectors(idx).Position = New Point(Me.GraphicObject.X, Me.GraphicObject.Y + 0.825 * Me.GraphicObject.Height)
                            Else
                                Me.GraphicObject.OutputConnectors(idx).Position = New Point(Me.GraphicObject.X + Me.GraphicObject.Width, Me.GraphicObject.Y + 0.825 * Me.GraphicObject.Height)
                            End If
                        Case StreamInformation.Behavior.InterExchanger
                            If Me.GraphicObject.FlippedH Then
                                Me.GraphicObject.OutputConnectors(idx).Position = New Point(Me.GraphicObject.X, Me.GraphicObject.Y + Me.StageIndex(strinfo.AssociatedStage) / Me.NumberOfStages * Me.GraphicObject.Height)
                            Else
                                Me.GraphicObject.OutputConnectors(idx).Position = New Point(Me.GraphicObject.X + Me.GraphicObject.Width, Me.GraphicObject.Y + Me.StageIndex(strinfo.AssociatedStage) / Me.NumberOfStages * Me.GraphicObject.Height)
                            End If
                    End Select
                Catch ex As Exception
                End Try
            Next

            Dim i As Integer = 0
            Dim obj1(Me.GraphicObject.InputConnectors.Count), obj2(Me.GraphicObject.InputConnectors.Count) As Double
            Dim obj3(Me.GraphicObject.OutputConnectors.Count), obj4(Me.GraphicObject.OutputConnectors.Count) As Double
            For Each ic As ConnectionPoint In Me.GraphicObject.InputConnectors
                obj1(i) = -Me.GraphicObject.X + ic.Position.X
                obj2(i) = -Me.GraphicObject.Y + ic.Position.Y
                i = i + 1
            Next
            i = 0
            For Each oc As ConnectionPoint In Me.GraphicObject.OutputConnectors
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

        Public Overrides Function Calculate(Optional ByVal args As Object = Nothing) As Integer

            Dim objargs As New DWSIM.Extras.StatusChangeEventArgs

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
            nc = Me.FlowSheet.Options.SelectedComponents.Count
            ns = Me.Stages.Count - 1
            maxits = Me.MaxIterations

            Dim tol(4) As Double
            tol(0) = Me.InternalLoopTolerance
            tol(1) = Me.ExternalLoopTolerance

            Dim F(ns), Q(ns), V(ns), L(ns), VSS(ns), LSS(ns), HF(ns), T(ns), FT(ns), P(ns), fracv(ns), eff(ns), _
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

            i = 0

            Dim stream As SimulationObjects.Streams.MaterialStream = Nothing

            For Each ms As StreamInformation In Me.MaterialStreams.Values
                Select Case ms.StreamBehavior
                    Case StreamInformation.Behavior.Feed
                        stream = FlowSheet.Collections.FlowsheetObjectCollection(ms.StreamID)
                        pp.CurrentMaterialStream = stream
                        F(StageIndex(ms.AssociatedStage)) = stream.Phases(0).Properties.molarflow.GetValueOrDefault
                        HF(StageIndex(ms.AssociatedStage)) = stream.Phases(0).Properties.enthalpy.GetValueOrDefault * _
                                                                stream.Phases(0).Properties.molecularWeight.GetValueOrDefault
                        FT(StageIndex(ms.AssociatedStage)) = stream.Phases(0).Properties.temperature.GetValueOrDefault
                        sumF += F(StageIndex(ms.AssociatedStage))
                        j = 0
                        For Each comp As Thermodynamics.BaseClasses.Compound In stream.Phases(0).Compounds.Values
                            fc(StageIndex(ms.AssociatedStage))(j) = comp.FracaoMolar.GetValueOrDefault
                            z(StageIndex(ms.AssociatedStage))(j) = comp.FracaoMolar.GetValueOrDefault
                            sumcf(j) += comp.FracaoMolar.GetValueOrDefault * F(StageIndex(ms.AssociatedStage))
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
                        Q(StageIndex(ms.AssociatedStage)) = -FlowSheet.Collections.FlowsheetObjectCollection(ms.StreamID).EnergyFlow.GetValueOrDefault
                End Select
                i += 1
            Next

            For Each ms As StreamInformation In Me.EnergyStreams.Values
                Select Case ms.StreamBehavior
                    Case StreamInformation.Behavior.InterExchanger
                        Q(StageIndex(ms.AssociatedStage)) = -FlowSheet.Collections.FlowsheetObjectCollection(ms.StreamID).EnergyFlow.GetValueOrDefault
                End Select
                i += 1
            Next

            Dim cv As New SystemsOfUnits.Converter

            vaprate = Converter.ConvertToSI(Me.VaporFlowRateUnit, Me.VaporFlowRate)

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
                    vaprate = sumF - Converter.ConvertToSI(Me.Specs("R").SpecUnit, Me.Specs("R").SpecValue) - sum0_
                    distrate = 0.0
                ElseIf Me.CondenserType = condtype.Partial_Condenser Then
                    If Me.Specs("C").SType = ColumnSpec.SpecType.Product_Molar_Flow_Rate Then
                        distrate = Converter.ConvertToSI(Me.Specs("C").SpecUnit, Me.Specs("C").SpecValue)
                    Else
                        distrate = sumF - Converter.ConvertToSI(Me.Specs("R").SpecUnit, Me.Specs("R").SpecValue) - sum0_ - vaprate
                    End If

                Else
                    distrate = sumF - Converter.ConvertToSI(Me.Specs("R").SpecUnit, Me.Specs("R").SpecValue) - sum0_
                    vaprate = 0.0
                End If
            Else
                If Me.CondenserType = condtype.Full_Reflux Then
                    vaprate = sumF / 2 - sum0_
                Else
                    distrate = sumF / 2 - sum0_ - vaprate
                End If
            End If

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
                    T1 = MathEx.Common.WgtAvg(F, FT)
                    T2 = T1
                Case ColType.ReboiledAbsorber
                    T1 = MathEx.Common.WgtAvg(F, FT)
                    T2 = T1
                Case ColType.RefluxedAbsorber
                    T1 = MathEx.Common.WgtAvg(F, FT)
                    T2 = T1
                Case ColType.DistillationColumn
                    Try
                        T1 = pp.DW_CalcBubT(zm, P(0), MathEx.Common.Min(FT))(4) '* 1.01
                    Catch ex As Exception
                        T1 = MathEx.Common.Min(FT)
                    End Try
                    Try
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
                    Dim flashresult = pp.FlashBase.Flash_PT(zm, P(i), T(i), pp)
                    If llextractor Then
                        x(i) = flashresult(2)
                        y(i) = flashresult(6)
                    Else
                        x(i) = flashresult(2)
                        y(i) = flashresult(3)
                    End If
                    z(i) = zm
                    For j = 0 To nc - 1
                        Kval(i)(j) = y(i)(j) / x(i)(j)
                    Next
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

            'process specifications
            For Each sp As Auxiliary.SepOps.ColumnSpec In Me.Specs.Values
                If sp.SType = ColumnSpec.SpecType.Component_Fraction Or _
                sp.SType = ColumnSpec.SpecType.Component_Mass_Flow_Rate Or _
                sp.SType = ColumnSpec.SpecType.Component_Molar_Flow_Rate Or _
                sp.SType = ColumnSpec.SpecType.Component_Recovery Then
                    i = 0
                    For Each comp As DWSIM.Thermodynamics.BaseClasses.Compound In stream.Phases(0).Compounds.Values
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
            Next

            Dim result As Object

            Select Case Me.SolvingMethod
                Case 0 'IO
                    Dim rm As New SolvingMethods.RussellMethod
                    result = rm.Solve(nc, ns, maxits, tol, F, V, Q, L, VSS, LSS, Kval, x, y, z, fc, HF, T, P, Me.CondenserType, eff, Me.UseDampingFactor, Me.UseNewtonUpdate, Me.AdjustSb, Me.UseIdentityAsJacobianInverse, Me.ColumnType, Me.KbjWeightedAverage, pp, Me.Specs, Me.StoreAndReuseJacobian, Me.JacobianMatrix, Me.IO_NumericalDerivativeStep, Me.IO_MaxVarChgFac, Me.IO_DampingFactorMin, Me.IO_DampingFactorMax, Me.IO_ExtLoop_DeltaT, llextractor)
                    ic = result(9)
                    ec = result(11)
                Case 1 'BP
                    result = SolvingMethods.WangHenkeMethod.Solve(nc, ns, maxits, tol, F, V, Q, L, VSS, LSS, Kval, x, y, z, fc, HF, T, P, Me.CondenserType, Me.StopAtIterationNumber, eff, Me.ColumnType, pp, Me.Specs)
                    ic = result(9)
                Case 2 'SR
                    result = SolvingMethods.BurninghamOttoMethod.Solve(nc, ns, maxits, tol, F, V, Q, L, VSS, LSS, Kval, x, y, z, fc, HF, T, P, Me.StopAtIterationNumber, eff, pp, Me.Specs, llextractor)
                    ic = result(9)
                Case 3 'SC
                    Dim scm As New SolvingMethods.NaphtaliSandholmMethod
                    result = scm.Solve(nc, ns, maxits, tol, F, V, Q, L, VSS, LSS, Kval, x, y, z, fc, HF, T, P, Me.CondenserType, eff, Me.UseDampingFactor, Me.UseNewtonUpdate, Me.UseIdentityAsJacobianInverse, Me.ColumnType, pp, Me.Specs, Me.StoreAndReuseJacobian, Me.JacobianMatrix, Me.SC_DampingFactor, Me.SC_MaximumTemperatureChange, Me.SC_NumericalDerivativeStep, Me.SC_MaxVarChgFac, llextractor)
                    ec = result(11)
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
                If Me.SolvingMethod = SolvingMethods.DistColSolvingMethod.Russell_InsideOut Or _
                   Me.SolvingMethod = SolvingMethods.DistColSolvingMethod.NaphtaliSandholm_SimultaneousCorrection Then
                    Dim obj(nc - 1) As Double
                    For j = 0 To nc - 1
                        obj(j) = result(7)(i, j)
                    Next
                    Kf.Add(obj)
                    Me.JacobianMatrix = result(13)
                Else
                    Kf.Add(result(7)(i))
                End If
            Next
            Tf = result(0)
            Vf = result(1)
            Lf = result(2)
            VSSf = result(3)
            LSSf = result(4)
            Q = result(8)

            'if enabled, auto update initial estimates

            If Me.AutoUpdateInitialEstimates Then
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

            'copy results to output streams

            'product flows

            Dim msm As Streams.MaterialStream
            Dim sinf As StreamInformation

            For Each sinf In Me.MaterialStreams.Values
                Select Case sinf.StreamBehavior
                    Case StreamInformation.Behavior.Distillate
                        msm = FlowSheet.Collections.FlowsheetObjectCollection(sinf.StreamID)
                        With msm
                            .Phases(0).Properties.massflow = LSSf(0) * pp.AUX_MMM(xf(0)) / 1000
                            .Phases(0).Properties.molarflow = LSSf(0)
                            .Phases(0).Properties.temperature = Tf(0)
                            .Phases(0).Properties.pressure = P(0) - Me.CondenserDeltaP
                            i = 0
                            For Each subst As DWSIM.Thermodynamics.BaseClasses.Compound In .Phases(0).Compounds.Values
                                subst.FracaoMolar = xf(0)(i)
                                i += 1
                            Next
                            i = 0
                            For Each subst As DWSIM.Thermodynamics.BaseClasses.Compound In .Phases(0).Compounds.Values
                                subst.FracaoMassica = pp.AUX_CONVERT_MOL_TO_MASS(xf(0))(i)
                                i += 1
                            Next
                        End With
                    Case StreamInformation.Behavior.OverheadVapor
                        msm = FlowSheet.Collections.FlowsheetObjectCollection(sinf.StreamID)
                        With msm
                            .Phases(0).Properties.massflow = Vf(0) * pp.AUX_MMM(yf(0)) / 1000
                            .Phases(0).Properties.temperature = Tf(0)
                            .Phases(0).Properties.pressure = P(0)
                            i = 0
                            For Each subst As DWSIM.Thermodynamics.BaseClasses.Compound In .Phases(0).Compounds.Values
                                subst.FracaoMolar = yf(0)(i)
                                i += 1
                            Next
                            i = 0
                            For Each subst As DWSIM.Thermodynamics.BaseClasses.Compound In .Phases(0).Compounds.Values
                                subst.FracaoMassica = pp.AUX_CONVERT_MOL_TO_MASS(yf(0))(i)
                                i += 1
                            Next
                        End With
                    Case StreamInformation.Behavior.BottomsLiquid
                        msm = FlowSheet.Collections.FlowsheetObjectCollection(sinf.StreamID)
                        With msm
                            .Phases(0).Properties.massflow = Lf(ns) * pp.AUX_MMM(xf(ns)) / 1000
                            .Phases(0).Properties.temperature = Tf(ns)
                            .Phases(0).Properties.pressure = P(ns)
                            i = 0
                            For Each subst As DWSIM.Thermodynamics.BaseClasses.Compound In .Phases(0).Compounds.Values
                                subst.FracaoMolar = xf(ns)(i)
                                i += 1
                            Next
                            i = 0
                            For Each subst As DWSIM.Thermodynamics.BaseClasses.Compound In .Phases(0).Compounds.Values
                                subst.FracaoMassica = pp.AUX_CONVERT_MOL_TO_MASS(xf(ns))(i)
                                i += 1
                            Next
                        End With
                    Case StreamInformation.Behavior.Sidedraw
                        Dim sidx As Integer = StageIndex(sinf.AssociatedStage)
                        msm = FlowSheet.Collections.FlowsheetObjectCollection(sinf.StreamID)
                        If sinf.StreamPhase = StreamInformation.Phase.L Then
                            With msm
                                .Phases(0).Properties.massflow = LSSf(sidx) * pp.AUX_MMM(xf(sidx)) / 1000
                                .Phases(0).Properties.temperature = Tf(sidx)
                                .Phases(0).Properties.pressure = P(sidx)
                                i = 0
                                For Each subst As DWSIM.Thermodynamics.BaseClasses.Compound In .Phases(0).Compounds.Values
                                    subst.FracaoMolar = xf(sidx)(i)
                                    i += 1
                                Next
                                i = 0
                                For Each subst As DWSIM.Thermodynamics.BaseClasses.Compound In .Phases(0).Compounds.Values
                                    subst.FracaoMassica = pp.AUX_CONVERT_MOL_TO_MASS(xf(sidx))(i)
                                    i += 1
                                Next
                            End With
                        ElseIf sinf.StreamPhase = StreamInformation.Phase.V Then
                            With msm
                                .Phases(0).Properties.massflow = VSSf(sidx) * pp.AUX_MMM(yf(sidx)) / 1000
                                .Phases(0).Properties.temperature = Tf(sidx)
                                .Phases(0).Properties.pressure = P(sidx)
                                i = 0
                                For Each subst As DWSIM.Thermodynamics.BaseClasses.Compound In .Phases(0).Compounds.Values
                                    subst.FracaoMolar = yf(sidx)(i)
                                    i += 1
                                Next
                                i = 0
                                For Each subst As DWSIM.Thermodynamics.BaseClasses.Compound In .Phases(0).Compounds.Values
                                    subst.FracaoMassica = pp.AUX_CONVERT_MOL_TO_MASS(yf(sidx))(i)
                                    i += 1
                                Next
                            End With
                        End If
                End Select
            Next

            'condenser/reboiler duties

            Dim esm As Streams.EnergyStream

            For Each sinf In Me.EnergyStreams.Values
                If sinf.StreamBehavior = StreamInformation.Behavior.Distillate Then
                    'condenser
                    esm = FlowSheet.Collections.FlowsheetObjectCollection(sinf.StreamID)
                    esm.EnergyFlow = Q(0)
                    esm.GraphicObject.Calculated = True
                ElseIf sinf.StreamBehavior = StreamInformation.Behavior.BottomsLiquid Then
                    'reboiler
                    esm = FlowSheet.Collections.FlowsheetObjectCollection(sinf.StreamID)
                    esm.EnergyFlow = Q(Me.NumberOfStages - 1)
                    esm.GraphicObject.Calculated = True
                End If
            Next

            'call the flowsheet calculation routine

            With objargs
                .Calculated = True
                .Name = Me.Name
                .ObjectType = Me.GraphicObject.ObjectType
            End With

final:      FlowSheet.CalculationQueue.Enqueue(objargs)

        End Function

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

        Public Overrides Function DeCalculate() As Integer

            Dim objargs As New DWSIM.Extras.StatusChangeEventArgs

            Dim i As Integer

            'update output streams

            'product flows

            Dim msm As New Streams.MaterialStream("", "")
            Dim sinf As StreamInformation

            For Each sinf In Me.MaterialStreams.Values
                If FlowSheet.Collections.FlowsheetObjectCollection.ContainsKey(sinf.StreamID) Then
                    Select Case sinf.StreamBehavior
                        Case StreamInformation.Behavior.Distillate
                            msm = FlowSheet.Collections.FlowsheetObjectCollection(sinf.StreamID)
                            With msm
                                .Phases(0).Properties.massflow = 0
                                .Phases(0).Properties.temperature = 0
                                .Phases(0).Properties.pressure = 0
                                i = 0
                                For Each subst As DWSIM.Thermodynamics.BaseClasses.Compound In .Phases(0).Compounds.Values
                                    subst.FracaoMolar = 0
                                    i += 1
                                Next
                            End With
                        Case StreamInformation.Behavior.OverheadVapor
                            msm = FlowSheet.Collections.FlowsheetObjectCollection(sinf.StreamID)
                            With msm
                                .Phases(0).Properties.massflow = 0
                                .Phases(0).Properties.temperature = 0
                                .Phases(0).Properties.pressure = 0
                                i = 0
                                For Each subst As DWSIM.Thermodynamics.BaseClasses.Compound In .Phases(0).Compounds.Values
                                    subst.FracaoMolar = 0
                                    i += 1
                                Next
                            End With
                        Case StreamInformation.Behavior.BottomsLiquid
                            msm = FlowSheet.Collections.FlowsheetObjectCollection(sinf.StreamID)
                            With msm
                                .Phases(0).Properties.massflow = 0
                                .Phases(0).Properties.temperature = 0
                                .Phases(0).Properties.pressure = 0
                                i = 0
                                For Each subst As DWSIM.Thermodynamics.BaseClasses.Compound In .Phases(0).Compounds.Values
                                    subst.FracaoMolar = 0
                                    i += 1
                                Next
                            End With
                        Case StreamInformation.Behavior.Sidedraw
                            Dim sidx As Integer = StageIndex(sinf.AssociatedStage)
                            msm = FlowSheet.Collections.FlowsheetObjectCollection(sinf.StreamID)
                            If sinf.StreamPhase = StreamInformation.Phase.L Then
                                With msm
                                    .Phases(0).Properties.massflow = 0
                                    .Phases(0).Properties.temperature = 0
                                    .Phases(0).Properties.pressure = 0
                                    i = 0
                                    For Each subst As DWSIM.Thermodynamics.BaseClasses.Compound In .Phases(0).Compounds.Values
                                        subst.FracaoMolar = 0
                                        i += 1
                                    Next
                                End With
                            ElseIf sinf.StreamPhase = StreamInformation.Phase.V Then
                                With msm
                                    .Phases(0).Properties.massflow = 0
                                    .Phases(0).Properties.temperature = 0
                                    .Phases(0).Properties.pressure = 0
                                    i = 0
                                    For Each subst As DWSIM.Thermodynamics.BaseClasses.Compound In .Phases(0).Compounds.Values
                                        subst.FracaoMolar = 0
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
                If FlowSheet.Collections.FlowsheetObjectCollection.ContainsKey(sinf.StreamID) Then
                    If sinf.StreamBehavior = StreamInformation.Behavior.Distillate Then
                        'condenser
                        esm = FlowSheet.Collections.FlowsheetObjectCollection(sinf.StreamID)
                        esm.EnergyFlow = 0
                        esm.GraphicObject.Calculated = False
                    ElseIf sinf.StreamBehavior = StreamInformation.Behavior.BottomsLiquid Then
                        'reboiler
                        esm = FlowSheet.Collections.FlowsheetObjectCollection(sinf.StreamID)
                        esm.EnergyFlow = 0
                        esm.GraphicObject.Calculated = False
                    End If
                End If
            Next

            'call the flowsheet calculation routine

            With objargs
                .Calculated = False
                .Name = Me.Name
                .ObjectType = Me.GraphicObject.ObjectType
            End With

final:      FlowSheet.CalculationQueue.Enqueue(objargs)


        End Function

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
                If Not FlowSheet.Collections.FlowsheetObjectCollection.ContainsKey(sinf.StreamID) Then
                    Throw New Exception(DWSIM.App.GetLocalString("DCStreamMissingException"))
                Else
                    Select Case sinf.StreamBehavior
                        Case StreamInformation.Behavior.Feed
                            If Not FlowSheet.Collections.FlowsheetObjectCollection(sinf.StreamID).GraphicObject.Calculated Then
                                Throw New Exception(DWSIM.App.GetLocalString("DCStreamNotCalculatedException"))
                            Else
                                feedok = True
                            End If
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
                If Not FlowSheet.Collections.FlowsheetObjectCollection.ContainsKey(sinf.StreamID) Then
                    Throw New Exception(DWSIM.App.GetLocalString("DCStreamMissingException"))
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
                                Throw New Exception(DWSIM.App.GetLocalString("DCConnectionMissingException"))
                            ElseIf Not cmvok And Me.CondenserType = condtype.Partial_Condenser Then
                                Throw New Exception(DWSIM.App.GetLocalString("DCConnectionMissingException"))
                            End If
                        Case condtype.Partial_Condenser
                            If Not feedok Or Not cmok Or Not cmvok Or Not rmok Or Not ceok Or Not reok Then
                                Throw New Exception(DWSIM.App.GetLocalString("DCConnectionMissingException"))
                            ElseIf Not cmvok And Me.CondenserType = condtype.Partial_Condenser Then
                                Throw New Exception(DWSIM.App.GetLocalString("DCConnectionMissingException"))
                            End If
                        Case condtype.Full_Reflux
                            If Not feedok Or Not cmvok Or Not rmok Or Not ceok Or Not reok Then
                                Throw New Exception(DWSIM.App.GetLocalString("DCConnectionMissingException"))
                            End If
                    End Select
                Case ColType.AbsorptionColumn
                    If Not feedok Or Not rmok Or Not cmvok Then
                        Throw New Exception(DWSIM.App.GetLocalString("DCConnectionMissingException"))
                    ElseIf Not cmvok And Me.CondenserType = condtype.Partial_Condenser Then
                        Throw New Exception(DWSIM.App.GetLocalString("DCConnectionMissingException"))
                    End If
                Case ColType.ReboiledAbsorber
                    If Not feedok Or Not cmvok Or Not rmok Or Not reok Then
                        Throw New Exception(DWSIM.App.GetLocalString("DCConnectionMissingException"))
                    ElseIf Not cmvok And Me.CondenserType = condtype.Partial_Condenser Then
                        Throw New Exception(DWSIM.App.GetLocalString("DCConnectionMissingException"))
                    End If
                Case ColType.RefluxedAbsorber
                    Select Case Me.CondenserType
                        Case condtype.Total_Condenser
                            If Not feedok Or Not cmok Or Not rmok Or Not ceok Then
                                Throw New Exception(DWSIM.App.GetLocalString("DCConnectionMissingException"))
                            ElseIf Not cmvok And Me.CondenserType = condtype.Partial_Condenser Then
                                Throw New Exception(DWSIM.App.GetLocalString("DCConnectionMissingException"))
                            End If
                        Case condtype.Partial_Condenser
                            If Not feedok Or Not cmok Or Not cmvok Or Not rmok Or Not ceok Then
                                Throw New Exception(DWSIM.App.GetLocalString("DCConnectionMissingException"))
                            ElseIf Not cmvok And Me.CondenserType = condtype.Partial_Condenser Then
                                Throw New Exception(DWSIM.App.GetLocalString("DCConnectionMissingException"))
                            End If
                        Case condtype.Full_Reflux
                            If Not feedok Or Not cmvok Or Not rmok Or Not ceok Then
                                Throw New Exception(DWSIM.App.GetLocalString("DCConnectionMissingException"))
                            ElseIf Not cmvok And Me.CondenserType = condtype.Partial_Condenser Then
                                Throw New Exception(DWSIM.App.GetLocalString("DCConnectionMissingException"))
                            End If
                    End Select
            End Select

            'all ok, proceed to calculations...

        End Sub

    End Class

End Namespace
