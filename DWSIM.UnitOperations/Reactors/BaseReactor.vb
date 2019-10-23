'    Reactor Base Class
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


Imports System.Collections.Generic
Imports DWSIM.Thermodynamics.BaseClasses
Imports System.Linq
Imports DWSIM.Interfaces.Enums
Imports DWSIM.SharedClasses

Namespace Reactors

    Public Enum OperationMode
        Isothermic = 0
        Adiabatic = 1
        OutletTemperature = 2
    End Enum

    <System.Serializable()> Public MustInherit Class Reactor

        Inherits UnitOperations.UnitOpBaseClass

        Protected m_reactionSequence As List(Of List(Of String))
        Protected m_reactions As List(Of String)
        Protected m_conversions As Dictionary(Of String, Double)
        Protected m_componentconversions As Dictionary(Of String, Double)
        Protected m_reactionSetID As String = "DefaultSet"
        Protected m_reactionSetName As String = ""
        Protected m_opmode As OperationMode = OperationMode.Adiabatic
        Protected m_dp As Nullable(Of Double)
        Protected m_dt As Nullable(Of Double)
        Protected m_DQ As Nullable(Of Double)

        Public Overrides Property ObjectClass As SimulationObjectClass = SimulationObjectClass.Reactors

        Public Overrides Function LoadData(data As System.Collections.Generic.List(Of System.Xml.Linq.XElement)) As Boolean

            MyBase.LoadData(data)
            Dim ci As Globalization.CultureInfo = Globalization.CultureInfo.InvariantCulture

            For Each xel2 As XElement In (From xel As XElement In data Select xel Where xel.Name = "ReactionConversions").LastOrDefault.Elements
                m_conversions.Add(xel2.@ID, Double.Parse(xel2.Value, ci))
            Next

            For Each xel2 As XElement In (From xel As XElement In data Select xel Where xel.Name = "CompoundConversions").LastOrDefault.Elements
                m_componentconversions.Add(xel2.@ID, Double.Parse(xel2.Value, ci))
            Next

            Return True

        End Function

        Public Overrides Function SaveData() As System.Collections.Generic.List(Of System.Xml.Linq.XElement)

            Dim elements As System.Collections.Generic.List(Of System.Xml.Linq.XElement) = MyBase.SaveData()
            Dim ci As Globalization.CultureInfo = Globalization.CultureInfo.InvariantCulture

            With elements
                .Add(New XElement("ReactionConversions"))
                For Each kvp As KeyValuePair(Of String, Double) In m_conversions
                    .Item(.Count - 1).Add(New XElement("Reaction", New XAttribute("ID", kvp.Key), kvp.Value.ToString(ci)))
                Next
                .Add(New XElement("CompoundConversions"))
                For Each kvp As KeyValuePair(Of String, Double) In m_componentconversions
                    .Item(.Count - 1).Add(New XElement("Compound", New XAttribute("ID", kvp.Key), kvp.Value.ToString(ci)))
                Next
            End With

            Return elements

        End Function

        Function GetConvFactors(rxn As Reaction, ims As IMaterialStream) As Dictionary(Of String, Double)

            Dim conv As New SystemsOfUnits.Converter

            Dim P As Double = ims.Phases(0).Properties.pressure.GetValueOrDefault
            Dim T As Double = ims.Phases(0).Properties.temperature.GetValueOrDefault
            Dim amounts As New Dictionary(Of String, Double)
            Dim val1, val2, val3, Z As Double

            For Each sb As ReactionStoichBase In rxn.Components.Values

                If Not amounts.ContainsKey(sb.CompName) Then amounts.Add(sb.CompName, 0.0#)

                Select Case rxn.ReactionBasis
                    Case ReactionBasis.Activity
                        val1 = ims.Phases(3).Compounds(sb.CompName).ActivityCoeff.GetValueOrDefault
                        val2 = ims.Phases(3).Properties.molecularWeight.GetValueOrDefault
                        val3 = ims.Phases(3).Properties.density.GetValueOrDefault
                        amounts(sb.CompName) = val1 * val2 / val3
                    Case ReactionBasis.Fugacity
                        Select Case rxn.ReactionPhase
                            Case PhaseName.Vapor
                                val1 = ims.Phases(2).Compounds(sb.CompName).FugacityCoeff.GetValueOrDefault
                                Z = ims.Phases(2).Properties.compressibilityFactor.GetValueOrDefault
                                amounts(sb.CompName) = val1 * Z * 8.314 * T
                            Case PhaseName.Liquid
                                val1 = ims.Phases(3).Compounds(sb.CompName).FugacityCoeff.GetValueOrDefault
                                val2 = ims.Phases(3).Properties.molecularWeight.GetValueOrDefault
                                val3 = ims.Phases(3).Properties.density.GetValueOrDefault
                                amounts(sb.CompName) = val1 * val2 / val3 * P
                            Case PhaseName.Mixture
                                val1 = ims.Phases(0).Compounds(sb.CompName).FugacityCoeff.GetValueOrDefault
                                val2 = ims.Phases(0).Properties.molecularWeight.GetValueOrDefault
                                val3 = ims.Phases(0).Properties.density.GetValueOrDefault
                                amounts(sb.CompName) = val1 * val2 / val3 * P
                        End Select
                    Case ReactionBasis.MassConc
                        Select Case rxn.ReactionPhase
                            Case PhaseName.Vapor
                                val1 = ims.Phases(2).Properties.molecularWeight.GetValueOrDefault
                                amounts(sb.CompName) = 1000 / val1
                            Case PhaseName.Liquid
                                val1 = ims.Phases(3).Properties.molecularWeight.GetValueOrDefault
                                amounts(sb.CompName) = 1000 / val1
                            Case PhaseName.Mixture
                                val1 = ims.Phases(0).Properties.molecularWeight.GetValueOrDefault
                                amounts(sb.CompName) = 1000 / val1
                        End Select
                    Case ReactionBasis.MassFrac
                        Select Case rxn.ReactionPhase
                            Case PhaseName.Vapor
                                val1 = ims.Phases(2).Properties.molecularWeight.GetValueOrDefault
                                Z = ims.Phases(2).Properties.compressibilityFactor.GetValueOrDefault
                                amounts(sb.CompName) = Z * 8.314 * T / P * 1000 / val1
                            Case PhaseName.Liquid
                                val1 = ims.Phases(3).Properties.molecularWeight.GetValueOrDefault
                                val2 = ims.Phases(3).Properties.density.GetValueOrDefault
                                amounts(sb.CompName) = val2 * 1000 / val1
                            Case PhaseName.Mixture
                                val1 = ims.Phases(0).Properties.molecularWeight.GetValueOrDefault
                                val2 = ims.Phases(0).Properties.density.GetValueOrDefault
                                amounts(sb.CompName) = val2 * 1000 / val1
                        End Select
                    Case ReactionBasis.MolarConc
                        amounts(sb.CompName) = 1.0#
                    Case ReactionBasis.MolarFrac
                        Select Case rxn.ReactionPhase
                            Case PhaseName.Vapor
                                Z = ims.Phases(2).Properties.compressibilityFactor.GetValueOrDefault
                                amounts(sb.CompName) = Z * 8.314 * T / P
                            Case PhaseName.Liquid
                                val1 = ims.Phases(3).Properties.molecularWeight.GetValueOrDefault
                                val2 = ims.Phases(3).Properties.density.GetValueOrDefault
                                amounts(sb.CompName) = val1 / val2
                            Case PhaseName.Mixture
                                val1 = ims.Phases(0).Properties.molecularWeight.GetValueOrDefault
                                val2 = ims.Phases(0).Properties.density.GetValueOrDefault
                                amounts(sb.CompName) = val1 / val2
                        End Select
                    Case ReactionBasis.PartialPress
                        Z = ims.Phases(2).Properties.compressibilityFactor.GetValueOrDefault
                        amounts(sb.CompName) = Z * 8.314 * T
                End Select

                amounts(sb.CompName) = SharedClasses.SystemsOfUnits.Converter.ConvertFromSI(rxn.ConcUnit, amounts(sb.CompName))

            Next

            Return amounts

        End Function

        Sub New()
            MyBase.CreateNew()
            Me.m_reactionSequence = New List(Of List(Of String))
            Me.m_reactions = New List(Of String)
            Me.m_conversions = New Dictionary(Of String, Double)
            Me.m_componentconversions = New Dictionary(Of String, Double)
        End Sub

        Public Property OutletTemperature As Double = 298.15#

        <Xml.Serialization.XmlIgnore()> Public Property ReactionsSequence() As List(Of List(Of String))
            Get
                Return m_reactionSequence
            End Get
            Set(ByVal value As List(Of List(Of String)))
                m_reactionSequence = value
            End Set
        End Property

        <Xml.Serialization.XmlIgnore()> Public ReadOnly Property Conversions() As Dictionary(Of String, Double)
            Get
                Return Me.m_conversions
            End Get
        End Property

        <Xml.Serialization.XmlIgnore()> Public ReadOnly Property ComponentConversions() As Dictionary(Of String, Double)
            Get
                Return Me.m_componentconversions
            End Get
        End Property

        <Xml.Serialization.XmlIgnore()> Public Property Reactions() As List(Of String)
            Get
                Return m_reactions
            End Get
            Set(ByVal value As List(Of String))
                m_reactions = value
            End Set
        End Property

        Public Property ReactorOperationMode() As OperationMode
            Get
                Return Me.m_opmode
            End Get
            Set(ByVal value As OperationMode)
                Me.m_opmode = value
            End Set
        End Property

        Public Property ReactionSetID() As String
            Get
                Return Me.m_reactionSetID
            End Get
            Set(ByVal value As String)
                Me.m_reactionSetID = value
            End Set
        End Property

        Public Property ReactionSetName() As String
            Get
                Return Me.m_reactionSetName
            End Get
            Set(ByVal value As String)
                Me.m_reactionSetName = value
            End Set
        End Property

        Public Property DeltaP() As Nullable(Of Double)
            Get
                Return m_dp
            End Get
            Set(ByVal value As Nullable(Of Double))
                m_dp = value
            End Set
        End Property

        Public Property DeltaT() As Nullable(Of Double)
            Get
                Return m_dt
            End Get
            Set(ByVal value As Nullable(Of Double))
                m_dt = value
            End Set
        End Property

        Public Property DeltaQ() As Nullable(Of Double)
            Get
                Return m_DQ
            End Get
            Set(ByVal value As Nullable(Of Double))
                m_DQ = value
            End Set
        End Property

    End Class

End Namespace
