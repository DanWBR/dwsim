'    Pipe Profile Editor
'    Copyright 2008 Daniel Wagner O. de Medeiros
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

Imports System.Linq

Namespace UnitOperations.Auxiliary.Pipe

    <System.Serializable()> Public Class PipeProfile

        Implements Interfaces.ICustomXMLSerialization

        Protected m_collection As New Generic.SortedDictionary(Of Integer, PipeSection)
        Protected m_status As PipeEditorStatus = PipeEditorStatus.Definir

        Public Property Sections() As Generic.SortedDictionary(Of Integer, PipeSection)
            Get
                Return m_collection
            End Get
            Set(ByVal value As Generic.SortedDictionary(Of Integer, PipeSection))
                m_collection = value
            End Set
        End Property

        Public Sub New()
            m_collection = New Generic.SortedDictionary(Of Integer, PipeSection)
        End Sub

        Public Property Status() As PipeEditorStatus
            Get
                Return m_status
            End Get
            Set(ByVal value As PipeEditorStatus)
                m_status = value
            End Set
        End Property

        Public Function CloneXML() As PipeProfile
            Dim obj As ICustomXMLSerialization = New PipeProfile()
            obj.LoadData(Me.SaveData)
            Return obj
        End Function

        Public Function LoadData(data As System.Collections.Generic.List(Of System.Xml.Linq.XElement)) As Boolean Implements Interfaces.ICustomXMLSerialization.LoadData

            XMLSerializer.XMLSerializer.Deserialize(Me, data)
            Dim ci As Globalization.CultureInfo = Globalization.CultureInfo.InvariantCulture

            Try
                Me.Status = [Enum].Parse(Type.GetType("DWSIM.UnitOperations.UnitOperations.Auxiliary.Pipe.PipeEditorStatus"), (From xel2 As XElement In data Select xel2 Where xel2.Name = "Status").SingleOrDefault.Value)
            Catch ex As Exception
                Me.Status = PipeEditorStatus.OK
            End Try

            m_collection.Clear()
            For Each xel As XElement In (From xel2 As XElement In data Select xel2 Where xel2.Name = "Sections").Elements.ToList
                Dim pr As New PipeSection()
                pr.LoadData(xel.Elements.ToList)
                pr.TipoSegmento = pr.TipoSegmento.Replace("°", " dg")
                m_collection.Add(xel.@ID, pr)
            Next
            Return True


        End Function

        Public Function SaveData() As System.Collections.Generic.List(Of System.Xml.Linq.XElement) Implements Interfaces.ICustomXMLSerialization.SaveData

            Dim elements As New System.Collections.Generic.List(Of System.Xml.Linq.XElement)
            Dim ci As Globalization.CultureInfo = Globalization.CultureInfo.InvariantCulture

            With elements
                .Add(New XElement("Status", m_status))
                .Add(New XElement("Sections", m_status))
                For Each kvp As KeyValuePair(Of Integer, PipeSection) In m_collection
                    .Item(.Count - 1).Add(New XElement("Section", New XAttribute("ID", kvp.Key), kvp.Value.SaveData.ToArray()))
                Next
            End With

            Return elements

        End Function

    End Class

    <System.Serializable()> Public Class PipeSection

        Implements Interfaces.ICustomXMLSerialization

        Protected m_index As Integer
        Protected m_tipo As String = ""
        Protected m_qtde As Integer
        Protected m_incrementos As Integer
        Protected m_material As String = ""
        Protected m_comprimento, m_elev As Double
        Protected m_de, m_di As Double

        Public Property PipeWallRugosity As Double = 0.0# 'm

        Public Property PipeWallThermalConductivityExpression As String = ""

        Protected m_results As New System.Collections.Generic.List(Of PipeResults)

        Public Function LoadData(data As System.Collections.Generic.List(Of System.Xml.Linq.XElement)) As Boolean Implements Interfaces.ICustomXMLSerialization.LoadData

            XMLSerializer.XMLSerializer.Deserialize(Me, data)
            Dim ci As Globalization.CultureInfo = Globalization.CultureInfo.InvariantCulture

            For Each xel As XElement In (From xel2 As XElement In data Select xel2 Where xel2.Name = "Results").Elements.ToList
                Dim pr As New PipeResults
                pr.LoadData(xel.Elements.ToList)
                m_results.Add(pr)
            Next
            Return True

        End Function

        Public Function SaveData() As System.Collections.Generic.List(Of System.Xml.Linq.XElement) Implements Interfaces.ICustomXMLSerialization.SaveData

            Dim elements As System.Collections.Generic.List(Of System.Xml.Linq.XElement) = XMLSerializer.XMLSerializer.Serialize(Me)
            Dim ci As Globalization.CultureInfo = Globalization.CultureInfo.InvariantCulture

            With elements
                .Add(New XElement("Results"))
                For Each result As PipeResults In m_results
                    .Item(.Count - 1).Add(New XElement("Result", result.SaveData.ToArray()))
                Next
            End With

            Return elements

        End Function

        Public ReadOnly Property Results() As System.Collections.Generic.List(Of PipeResults)
            Get
                Return m_results
            End Get
        End Property

        Public Property Indice() As Integer
            Get
                Return m_index
            End Get
            Set(ByVal valor As Integer)
                m_index = valor
            End Set
        End Property

        Public Property Quantidade() As Integer
            Get
                Return m_qtde
            End Get
            Set(ByVal valor As Integer)
                m_qtde = valor
            End Set
        End Property

        Public Property Incrementos() As Integer
            Get
                Return m_incrementos
            End Get
            Set(ByVal valor As Integer)
                m_incrementos = valor
            End Set
        End Property

        Public Property TipoSegmento() As String
            Get
                Return m_tipo
            End Get
            Set(ByVal valor As String)
                m_tipo = valor
            End Set
        End Property

        Public Property Material() As String
            Get
                Return m_material
            End Get
            Set(ByVal valor As String)
                m_material = valor
            End Set
        End Property

        Public Property Comprimento() As Double
            Get
                Return m_comprimento
            End Get
            Set(ByVal valor As Double)
                m_comprimento = valor
            End Set
        End Property

        Public Property Elevacao() As Double
            Get
                Return m_elev
            End Get
            Set(ByVal valor As Double)
                m_elev = valor
            End Set
        End Property

        Public Property DE() As Double
            Get
                Return m_de
            End Get
            Set(ByVal valor As Double)
                m_de = valor
            End Set
        End Property

        Public Property DI() As Double
            Get
                Return m_di
            End Get
            Set(ByVal valor As Double)
                m_di = valor
            End Set
        End Property

        Public Sub New()
            Me.m_results = New System.Collections.Generic.List(Of PipeResults)
        End Sub

        Public Sub New(ByVal indice As Integer, ByVal tipo As String, ByVal qtde As Integer, ByVal incrementos As Integer, ByVal material As String, _
                        ByVal comp As Double, ByVal elev As Double, ByVal de As Double, ByVal di As Double)
            Me.m_index = indice
            Me.m_tipo = tipo
            Me.m_qtde = qtde
            Me.m_incrementos = incrementos
            Me.m_material = material
            Me.m_comprimento = comp
            Me.m_elev = elev
            Me.m_de = de
            Me.m_di = di

            Me.m_results = New System.Collections.Generic.List(Of PipeResults)

        End Sub

    End Class

    <System.Serializable()> Public Class PipeResults

        Implements Interfaces.ICustomXMLSerialization

        Protected m_Pi As Nullable(Of Double)
        Protected m_Ti As Nullable(Of Double)

        Protected m_muv As Nullable(Of Double)
        Protected m_mul As Nullable(Of Double)
        Protected m_rhov As Nullable(Of Double)
        Protected m_rhol As Nullable(Of Double)
        Protected m_kv As Nullable(Of Double)
        Protected m_kl As Nullable(Of Double)
        Protected m_cpv As Nullable(Of Double)
        Protected m_cpl As Nullable(Of Double)
        Protected m_qv As Nullable(Of Double)
        Protected m_ql As Nullable(Of Double)
        Protected m_surft As Nullable(Of Double)

        Protected m_dPf As Nullable(Of Double)
        Protected m_dPh As Nullable(Of Double)
        Protected m_LiqHoldup As Nullable(Of Double)

        Protected m_tipo_fluxo As String = ""
        Protected m_fluxo_desc As String = ""

        Protected m_LiqRe As Nullable(Of Double)
        Protected m_VapRe As Nullable(Of Double)

        Protected m_LiqVel As Nullable(Of Double)
        Protected m_VapVel As Nullable(Of Double)

        Protected m_HeatTransf As Nullable(Of Double)
        Protected m_Einicial As Nullable(Of Double)

        Protected m_HTC As Nullable(Of Double)

#Region "Constructors"

        Sub New()

        End Sub

        Sub New(ByVal Pi As Nullable(Of Double), ByVal Ti As Nullable(Of Double), ByVal MUV As Nullable(Of Double), ByVal MUL As Nullable(Of Double), _
        ByVal RHOV As Nullable(Of Double), ByVal RHOL As Nullable(Of Double), ByVal CPV As Nullable(Of Double), ByVal CPL As Nullable(Of Double), _
        ByVal KV As Nullable(Of Double), ByVal KL As Nullable(Of Double), ByVal QV As Nullable(Of Double), ByVal QL As Nullable(Of Double), _
        ByVal SURFT As Nullable(Of Double), ByVal DPF As Nullable(Of Double), ByVal DPH As Nullable(Of Double), ByVal EL As Nullable(Of Double), _
        ByVal TIPOFLUXO As String, ByVal LIQRE As Nullable(Of Double), ByVal VAPRE As Nullable(Of Double), ByVal LIQVEL As Nullable(Of Double), _
        ByVal VAPVEL As Nullable(Of Double), ByVal QTRANSF As Nullable(Of Double), ByVal EINICIAL As Nullable(Of Double), ByVal HTC As Nullable(Of Double))

            With Me
                .Pressure_Initial = Pi
                .Temperature_Initial = Ti
                .MUv = MUV
                .MUl = MUL
                .RHOv = RHOV
                .RHOl = RHOL
                .Cpv = CPV
                .Cpl = CPL
                .Kv = KV
                .Kl = KL
                .Qv = QV
                .Ql = QL
                .Surft = SURFT
                .DpFriction = DPF
                .DpStatic = DPH
                .LiquidHoldup = EL
                .FlowRegime = TIPOFLUXO
                .VapVel = VAPVEL
                .LiqVel = LIQVEL
                .VapRe = VAPRE
                .LiqRe = LIQRE
                .HeatTransferred = QTRANSF
                .EnergyFlow_Initial = EINICIAL
                .HTC = HTC
            End With

        End Sub

#End Region

        Public Property Pressure_Initial() As Nullable(Of Double)
            Get
                Return Me.m_Pi
            End Get
            Set(ByVal value As Nullable(Of Double))
                Me.m_Pi = value
            End Set
        End Property

        Public Property Temperature_Initial() As Nullable(Of Double)
            Get
                Return Me.m_Ti
            End Get
            Set(ByVal value As Nullable(Of Double))
                Me.m_Ti = value
            End Set
        End Property

        Public Property DpFriction() As Nullable(Of Double)
            Get
                Return Me.m_dPf
            End Get
            Set(ByVal value As Nullable(Of Double))
                Me.m_dPf = value
            End Set
        End Property

        Public Property DpStatic() As Nullable(Of Double)
            Get
                Return Me.m_dPh
            End Get
            Set(ByVal value As Nullable(Of Double))
                Me.m_dPh = value
            End Set
        End Property

        Public Property LiquidHoldup() As Nullable(Of Double)
            Get
                Return Me.m_LiqHoldup
            End Get
            Set(ByVal value As Nullable(Of Double))
                Me.m_LiqHoldup = value
            End Set
        End Property

        Public Property LiqRe() As Nullable(Of Double)
            Get
                Return Me.m_LiqRe
            End Get
            Set(ByVal value As Nullable(Of Double))
                Me.m_LiqRe = value
            End Set
        End Property

        Public Property VapRe() As Nullable(Of Double)
            Get
                Return Me.m_VapRe
            End Get
            Set(ByVal value As Nullable(Of Double))
                Me.m_VapRe = value
            End Set
        End Property

        Public Property LiqVel() As Nullable(Of Double)
            Get
                Return Me.m_LiqVel
            End Get
            Set(ByVal value As Nullable(Of Double))
                Me.m_LiqVel = value
            End Set
        End Property

        Public Property VapVel() As Nullable(Of Double)
            Get
                Return Me.m_VapVel
            End Get
            Set(ByVal value As Nullable(Of Double))
                Me.m_VapVel = value
            End Set
        End Property

        Public Property HeatTransferred() As Nullable(Of Double)
            Get
                Return Me.m_HeatTransf
            End Get
            Set(ByVal value As Nullable(Of Double))
                Me.m_HeatTransf = value
            End Set
        End Property

        Public Property FlowRegime() As String
            Get
                Return Me.m_tipo_fluxo
            End Get
            Set(ByVal TipoFluxo As String)
                Me.m_tipo_fluxo = TipoFluxo
            End Set
        End Property

        Public Property FlowRegimeDescription() As String
            Get
                Return Me.m_fluxo_desc
            End Get
            Set(ByVal TipoFluxoDescricao As String)
                Me.m_fluxo_desc = TipoFluxoDescricao
            End Set
        End Property

        Public Property MUv() As Nullable(Of Double)
            Get
                Return Me.m_muv
            End Get
            Set(ByVal value As Nullable(Of Double))
                Me.m_muv = value
            End Set
        End Property

        Public Property MUl() As Nullable(Of Double)
            Get
                Return Me.m_mul
            End Get
            Set(ByVal value As Nullable(Of Double))
                Me.m_mul = value
            End Set
        End Property

        Public Property Qv() As Nullable(Of Double)
            Get
                Return Me.m_qv
            End Get
            Set(ByVal value As Nullable(Of Double))
                Me.m_qv = value
            End Set
        End Property

        Public Property Ql() As Nullable(Of Double)
            Get
                Return Me.m_ql
            End Get
            Set(ByVal value As Nullable(Of Double))
                Me.m_ql = value
            End Set
        End Property

        Public Property RHOv() As Nullable(Of Double)
            Get
                Return Me.m_rhov
            End Get
            Set(ByVal value As Nullable(Of Double))
                Me.m_rhov = value
            End Set
        End Property

        Public Property RHOl() As Nullable(Of Double)
            Get
                Return Me.m_rhol
            End Get
            Set(ByVal value As Nullable(Of Double))
                Me.m_rhol = value
            End Set
        End Property

        Public Property EnergyFlow_Initial() As Nullable(Of Double)
            Get
                Return Me.m_Einicial
            End Get
            Set(ByVal value As Nullable(Of Double))
                Me.m_Einicial = value
            End Set
        End Property

        Public Property Cpv() As Nullable(Of Double)
            Get
                Return Me.m_cpv
            End Get
            Set(ByVal value As Nullable(Of Double))
                Me.m_cpv = value
            End Set
        End Property

        Public Property Cpl() As Nullable(Of Double)
            Get
                Return Me.m_cpl
            End Get
            Set(ByVal value As Nullable(Of Double))
                Me.m_cpl = value
            End Set
        End Property

        Public Property Kl() As Nullable(Of Double)
            Get
                Return Me.m_kl
            End Get
            Set(ByVal value As Nullable(Of Double))
                Me.m_kl = value
            End Set
        End Property

        Public Property Kv() As Nullable(Of Double)
            Get
                Return Me.m_kv
            End Get
            Set(ByVal value As Nullable(Of Double))
                Me.m_kv = value
            End Set
        End Property

        Public Property Surft() As Nullable(Of Double)
            Get
                Return Me.m_surft
            End Get
            Set(ByVal value As Nullable(Of Double))
                Me.m_surft = value
            End Set
        End Property

        Public Property HTC() As Nullable(Of Double)
            Get
                Return m_HTC
            End Get
            Set(ByVal value As Nullable(Of Double))
                m_HTC = value
            End Set
        End Property

        Public Property HTC_internal As Double = 0.0#
        Public Property HTC_pipewall As Double = 0.0#
        Public Property HTC_insulation As Double = 0.0#
        Public Property HTC_external As Double = 0.0#

        Public Property External_Temperature As Double = 0.0

        Public Property MachNumber As Double = 0.0

        Public Function LoadData(data As System.Collections.Generic.List(Of System.Xml.Linq.XElement)) As Boolean Implements Interfaces.ICustomXMLSerialization.LoadData

            XMLSerializer.XMLSerializer.Deserialize(Me, data)
            Return True
        End Function

        Public Function SaveData() As System.Collections.Generic.List(Of System.Xml.Linq.XElement) Implements Interfaces.ICustomXMLSerialization.SaveData

            Return XMLSerializer.XMLSerializer.Serialize(Me)

        End Function

    End Class

    Public Enum PipeEditorStatus
        OK
        Erro
        Definir
    End Enum

End Namespace
