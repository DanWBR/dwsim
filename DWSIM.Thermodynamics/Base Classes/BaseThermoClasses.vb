'    Basic Thermodynamic Classes for DWSIM
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
Imports System.Xml.Serialization
Imports FileHelpers
Imports Ciloci.Flee
Imports System.Runtime.Serialization.Formatters.Binary
Imports System.Runtime.Serialization
Imports System.IO
Imports System.Linq
Imports System.Reflection
Imports System.Globalization

Namespace BaseClasses

    <System.Serializable()> Public Class Compound

        Implements XMLSerializer.Interfaces.ICustomXMLSerialization

        Protected m_ComponentDescription As String = ""
        Protected m_ComponentName As String = ""
        Protected m_molarfraction As Nullable(Of Double) = 0.0#
        Protected m_massfraction As Nullable(Of Double) = 0.0#
        Protected m_molarflow As Nullable(Of Double) = 0.0#
        Protected m_massflow As Nullable(Of Double) = 0.0#
        Protected m_fugacitycoeff As Nullable(Of Double) = 0.0#
        Protected m_activitycoeff As Nullable(Of Double) = 0.0#
        Protected m_partialvolume As Nullable(Of Double) = 0.0#
        Protected m_partialpressure As Nullable(Of Double) = 0.0#
        Protected m_volumetricflow As Nullable(Of Double) = 0.0#
        Protected m_volumetricfraction As Nullable(Of Double) = 0.0#
        Protected m_isPF As Boolean = False
        Protected m_lnKval As Double = 0
        Protected m_Kval As Double = 0

        Public Property lnKvalue() As Double
            Get
                Return m_lnKval
            End Get
            Set(ByVal value As Double)
                m_lnKval = value
            End Set
        End Property

        Public Property Kvalue() As Double
            Get
                Return m_Kval
            End Get
            Set(ByVal value As Double)
                m_Kval = value
            End Set
        End Property

        Public Property FracaoDePetroleo() As Boolean
            Get
                Return m_isPF
            End Get
            Set(ByVal value As Boolean)
                m_isPF = value
            End Set
        End Property

        Public Property FracaoMolar() As Nullable(Of Double)
            Get
                Return m_molarfraction
            End Get
            Set(ByVal value As Nullable(Of Double))
                m_molarfraction = value
            End Set
        End Property

        Public Property FracaoMassica() As Nullable(Of Double)
            Get
                Return m_massfraction
            End Get
            Set(ByVal value As Nullable(Of Double))
                m_massfraction = value
            End Set
        End Property

        Public Property MolarFlow() As Nullable(Of Double)
            Get
                Return Me.m_molarflow
            End Get
            Set(ByVal value As Nullable(Of Double))
                Me.m_molarflow = value
            End Set
        End Property

        Public Property MassFlow() As Nullable(Of Double)
            Get
                Return Me.m_massflow
            End Get
            Set(ByVal value As Nullable(Of Double))
                Me.m_massflow = value
            End Set
        End Property

        Public Property FugacityCoeff() As Nullable(Of Double)
            Get
                Return Me.m_fugacitycoeff
            End Get
            Set(ByVal value As Nullable(Of Double))
                Me.m_fugacitycoeff = value
            End Set
        End Property

        Public Property ActivityCoeff() As Nullable(Of Double)
            Get
                Return Me.m_activitycoeff
            End Get
            Set(ByVal value As Nullable(Of Double))
                Me.m_activitycoeff = value
            End Set
        End Property

        Public Property PartialVolume() As Nullable(Of Double)
            Get
                Return Me.m_partialvolume
            End Get
            Set(ByVal value As Nullable(Of Double))
                Me.m_partialvolume = value
            End Set
        End Property

        Public Property PartialPressure() As Nullable(Of Double)
            Get
                Return Me.m_partialpressure
            End Get
            Set(ByVal value As Nullable(Of Double))
                Me.m_partialpressure = value
            End Set
        End Property

        Public Property VolumetricFlow() As Nullable(Of Double)
            Get
                Return Me.m_volumetricflow
            End Get
            Set(ByVal value As Nullable(Of Double))
                Me.m_volumetricflow = value
            End Set
        End Property

        Public Property VolumetricFraction() As Nullable(Of Double)
            Get
                Return Me.m_volumetricfraction
            End Get
            Set(ByVal value As Nullable(Of Double))
                Me.m_volumetricfraction = value
            End Set
        End Property

        Public Property ComponentDescription() As String
            Get
                Return m_ComponentDescription
            End Get
            Set(ByVal value As String)
                m_ComponentDescription = value
            End Set
        End Property

        Public Property ComponentName() As String
            Get
                Return m_ComponentName
            End Get
            Set(ByVal value As String)
                m_ComponentName = value
            End Set
        End Property

        Public Property Name() As String
            Get
                Return m_ComponentName
            End Get
            Set(ByVal value As String)
                m_ComponentName = value
            End Set
        End Property

        Public TDProperties As New TemperatureDependentProperties
        Public PDProperties As New PressureDependentProperties
        Public ConstantProperties As New ConstantProperties

        Public Sub New(ByVal name As String, ByVal description As String)

            Me.m_ComponentName = name
            Me.m_ComponentDescription = description

        End Sub

        Public Function LoadData(data As System.Collections.Generic.List(Of System.Xml.Linq.XElement)) As Boolean Implements XMLSerializer.Interfaces.ICustomXMLSerialization.LoadData
            Return XMLSerializer.XMLSerializer.Deserialize(Me, data)
        End Function

        Public Function SaveData() As System.Collections.Generic.List(Of System.Xml.Linq.XElement) Implements XMLSerializer.Interfaces.ICustomXMLSerialization.SaveData

            Return XMLSerializer.XMLSerializer.Serialize(Me)

        End Function

    End Class

    <System.Serializable()> Public Class Phase

        Implements XMLSerializer.Interfaces.ICustomXMLSerialization

        Public Property ComponentDescription As String = ""
        Public Property ComponentName As String = ""

        Public Property Nome() As String
            Get
                Return ComponentName
            End Get
            Set(value As String)
                ComponentName = value
            End Set
        End Property

        Public Property Compounds As Dictionary(Of String, Compound)

        Public Property Properties As New SinglePhaseMixtureProperties
        Public Property Properties2 As New TwoPhaseMixtureProperties

        Public Sub New(ByVal name As String, ByVal description As String)

            Me.ComponentName = name
            Me.ComponentDescription = description
            Me.Compounds = New Dictionary(Of String, Compound)

        End Sub

        Public Sub New(ByVal name As String, ByVal description As String, ByVal Compounds As Dictionary(Of String, Compound))

            Me.ComponentName = name
            Me.ComponentDescription = description
            Me.Compounds = Compounds

        End Sub

        Public Function LoadData(data As System.Collections.Generic.List(Of System.Xml.Linq.XElement)) As Boolean Implements XMLSerializer.Interfaces.ICustomXMLSerialization.LoadData

            XMLSerializer.XMLSerializer.Deserialize(Me, data)

            Dim datac As List(Of XElement) = (From xel As XElement In data Select xel Where xel.Name = "Compounds").Elements.ToList

            For Each xel As XElement In datac
                Dim s As New Compound("", "")
                s.LoadData(xel.Elements.ToList)
                Me.Compounds.Add(s.Name, s)
            Next

            XMLSerializer.XMLSerializer.Deserialize(Me.Properties, (From xel As XElement In data Select xel Where xel.Name = "Properties").Elements.ToList)
            XMLSerializer.XMLSerializer.Deserialize(Me.Properties2, (From xel As XElement In data Select xel Where xel.Name = "Properties2").Elements.ToList)

            Return True

        End Function

        Public Function SaveData() As System.Collections.Generic.List(Of System.Xml.Linq.XElement) Implements XMLSerializer.Interfaces.ICustomXMLSerialization.SaveData

            Dim elements As New List(Of System.Xml.Linq.XElement)
            Dim ci As CultureInfo = CultureInfo.InvariantCulture

            With elements

                .Add(New XElement("Compounds"))

                For Each kvp As KeyValuePair(Of String, Compound) In Me.Compounds
                    elements(elements.Count - 1).Add(New XElement("Compound", kvp.Value.SaveData().ToArray()))
                Next

                Dim props As PropertyInfo() = Me.GetType.GetProperties()
                For Each fi As PropertyInfo In props
                    If TypeOf Me.GetType.GetProperty(fi.Name).GetValue(Me, Nothing) Is Double Then
                        .Add(New XElement(fi.Name, Double.Parse(Me.GetType.GetProperty(fi.Name).GetValue(Me, Nothing)).ToString(ci)))
                    Else
                        .Add(New XElement(fi.Name, Me.GetType.GetProperty(fi.Name).GetValue(Me, Nothing).ToString()))
                    End If
                Next

                .Add(New XElement("Properties"))
                elements(elements.Count - 1).Add(XMLSerializer.XMLSerializer.Serialize(Me.Properties))

                .Add(New XElement("Properties2"))
                elements(elements.Count - 1).Add(XMLSerializer.XMLSerializer.Serialize(Me.Properties2))

            End With

            Return elements

        End Function

    End Class

    Public Enum PhaseName
        Liquid
        Vapor
        Mixture
        Solid
    End Enum

    Public Enum ReactionType
        Equilibrium
        Kinetic
        Heterogeneous_Catalytic
        Conversion
    End Enum

    Public Enum ReactionBasis
        Activity
        Fugacity
        MolarConc
        MassConc
        MolarFrac
        MassFrac
        PartialPress
    End Enum

    <System.Serializable()> <XmlRoot(ElementName:="Reaction")> _
    Public Class Reaction

        Implements ICloneable, XMLSerializer.Interfaces.ICustomXMLSerialization

        Protected m_Components_and_stoichcoeffs As Dictionary(Of String, ReactionStoichBase)

        Protected m_name As String = ""
        Protected m_id As String = ""
        Protected m_description As String = ""
        Protected m_equation As String = ""

        Protected m_base_reactant As String = ""
        Protected m_phase As PhaseName = PhaseName.Vapor
        Protected m_reactiontype As ReactionType = ReactionType.Conversion
        Protected m_reactionbasis As ReactionBasis = ReactionBasis.MolarConc
        Protected m_reactionheat As Double = 0.0#
        Protected m_reactionheatCO As Double = 0.0#
        Protected m_stoichbalance As Double = 0.0#

#Region "    DWSIM Specific"

        Public Property StoichBalance() As Double
            Get
                Return Me.m_stoichbalance
            End Get
            Set(ByVal value As Double)
                Me.m_stoichbalance = value
            End Set
        End Property

        Public Property ReactionHeatCO() As Double
            Get
                Return Me.m_reactionheatCO
            End Get
            Set(ByVal value As Double)
                Me.m_reactionheatCO = value
            End Set
        End Property

        Public Property ReactionHeat() As Double
            Get
                Return Me.m_reactionheat
            End Get
            Set(ByVal value As Double)
                Me.m_reactionheat = value
            End Set
        End Property

        Public Property Equation() As String
            Get
                Return Me.m_equation
            End Get
            Set(ByVal value As String)
                Me.m_equation = value
            End Set
        End Property

        Public ReadOnly Property Components() As Dictionary(Of String, ReactionStoichBase)
            Get
                Return m_Components_and_stoichcoeffs
            End Get
        End Property

        Public Property Name() As String
            Get
                Return m_name
            End Get
            Set(ByVal value As String)
                m_name = value
            End Set
        End Property

        Public Property ID() As String
            Get
                Return m_id
            End Get
            Set(ByVal value As String)
                m_id = value
            End Set
        End Property

        Public Property Description() As String
            Get
                Return m_description
            End Get
            Set(ByVal value As String)
                m_description = value
            End Set
        End Property

        Public Property BaseReactant() As String
            Get
                Return m_base_reactant
            End Get
            Set(ByVal value As String)
                m_base_reactant = value
            End Set
        End Property

        Public Property ReactionPhase() As PhaseName
            Get
                Return m_phase
            End Get
            Set(ByVal value As PhaseName)
                m_phase = value
            End Set
        End Property

        Public Property ReactionType() As ReactionType
            Get
                Return m_reactiontype
            End Get
            Set(ByVal value As ReactionType)
                m_reactiontype = value
            End Set
        End Property

        Public Property ReactionBasis() As ReactionBasis
            Get
                Return m_reactionbasis
            End Get
            Set(ByVal value As ReactionBasis)
                m_reactionbasis = value
            End Set
        End Property

        Public Function EvaluateK(ByVal T As Double, ByVal pp As PropertyPackages.PropertyPackage) As Double

            'equilibrium constant calculation

            Select Case KExprType
                Case Reaction.KOpt.Constant

                    Return ConstantKeqValue

                Case Reaction.KOpt.Expression

                    ExpContext.Variables("T") = T
                    ExpContext.Options.ParseCulture = Globalization.CultureInfo.InvariantCulture
                    Expr = ExpContext.CompileGeneric(Of Double)(Expression)

                    Return Math.Exp(Expr.Evaluate)

                Case Reaction.KOpt.Gibbs

                    Dim id(Components.Count - 1) As String
                    Dim stcoef(Components.Count - 1) As Double
                    Dim bcidx As Integer = 0
                    Dim j As Integer = 0
                    For Each sb As ReactionStoichBase In Components.Values
                        id(j) = sb.CompName
                        stcoef(j) = sb.StoichCoeff
                        If sb.IsBaseReactant Then bcidx = j
                        j += 1
                    Next

                    Dim DelG_RT = pp.AUX_DELGig_RT(298.15, T, id, stcoef, bcidx)

                    Return Math.Exp(-DelG_RT)

            End Select

            Return Nothing

        End Function

        'Equilibrium

        Enum KOpt
            Gibbs
            Expression
            Constant
        End Enum

        <System.NonSerialized()> <XmlIgnore()> Protected m_e As IGenericExpression(Of Double)
        <System.NonSerialized()> <XmlIgnore()> Protected m_eopt As ExpressionContext
        Protected m_Expression As String = ""
        Protected m_OptionForK As KOpt = KOpt.Gibbs
        Protected m_approach As Double
        Protected m_tmin As Double = 0
        Protected m_tmax As Double = 2000
        Protected m_constantkeq As Double
        Protected m_reactiongibbs As Double
        Protected m_kval As Double

        Public Property Kvalue() As Double
            Get
                Return m_kval
            End Get
            Set(ByVal value As Double)
                m_kval = value
            End Set
        End Property

        Public Property ReactionGibbsEnergy() As Double
            Get
                Return m_reactiongibbs
            End Get
            Set(ByVal value As Double)
                m_reactiongibbs = value
            End Set
        End Property

        Public Property ConstantKeqValue() As Double
            Get
                Return Me.m_constantkeq
            End Get
            Set(ByVal value As Double)
                Me.m_constantkeq = value
            End Set
        End Property

        Public Property Tmax() As Double
            Get
                Return m_tmax
            End Get
            Set(ByVal value As Double)
                m_tmax = value
            End Set
        End Property

        Public Property Tmin() As Double
            Get
                Return m_tmin
            End Get
            Set(ByVal value As Double)
                m_tmin = value
            End Set
        End Property

        Public Property Approach() As Double
            Get
                Return m_approach
            End Get
            Set(ByVal value As Double)
                m_approach = value
            End Set
        End Property

        Public Property Expr() As IGenericExpression(Of Double)
            Get
                Return m_e
            End Get
            Set(ByVal value As IGenericExpression(Of Double))
                m_e = value
            End Set
        End Property

        Public Property ExpContext() As ExpressionContext
            Get
                Return m_eopt
            End Get
            Set(ByVal value As ExpressionContext)
                m_eopt = value
            End Set
        End Property

        Public Property Expression() As String
            Get
                Return Me.m_Expression
            End Get
            Set(ByVal value As String)
                Me.m_Expression = value
            End Set
        End Property

        Public Property KExprType() As KOpt
            Get
                Return m_OptionForK
            End Get
            Set(ByVal value As KOpt)
                m_OptionForK = value
            End Set
        End Property

        'Kinetic

        Protected m_A_fwd, m_E_fwd, m_A_rev, m_E_rev As Double
        Protected m_concunit As String = "mol/m3"
        Protected m_velunit As String = "mol/[m3.s]"
        Protected m_rate As Double = 0.0#

        Public Property Rate() As Double
            Get
                Return m_rate
            End Get
            Set(ByVal value As Double)
                m_rate = value
            End Set
        End Property

        Public Property VelUnit() As String
            Get
                Return m_velunit
            End Get
            Set(ByVal value As String)
                m_velunit = value
            End Set
        End Property

        Public Property ConcUnit() As String
            Get
                Return m_concunit
            End Get
            Set(ByVal value As String)
                m_concunit = value
            End Set
        End Property

        Public Property A_Forward() As Double
            Get
                Return m_A_fwd
            End Get
            Set(ByVal value As Double)
                m_A_fwd = value
            End Set
        End Property

        Public Property A_Reverse() As Double
            Get
                Return m_A_rev
            End Get
            Set(ByVal value As Double)
                m_A_rev = value
            End Set
        End Property

        Public Property E_Forward() As Double
            Get
                Return m_E_fwd
            End Get
            Set(ByVal value As Double)
                m_E_fwd = value
            End Set
        End Property

        Public Property E_Reverse() As Double
            Get
                Return m_E_rev
            End Get
            Set(ByVal value As Double)
                m_E_rev = value
            End Set
        End Property

        'Heterogeneous

        Public Property RateEquationNumerator As String = ""

        Public Property RateEquationDenominator As String = ""

        'Initializers

        Public Sub New()
            Me.m_Components_and_stoichcoeffs = New Dictionary(Of String, ReactionStoichBase)
            ExpContext = New Ciloci.Flee.ExpressionContext
            ExpContext.Imports.AddType(GetType(System.Math))
            ExpContext.Variables.Add("T", 0.0#)
        End Sub

        Public Sub New(ByVal Name As String, ByVal Id As String)
            Me.New()
            Me.Name = Name
            Me.ID = Id
        End Sub

        Public Sub New(ByVal Name As String, ByVal Id As String, ByVal Description As String)
            Me.New(Name, Id)
            Me.Description = Description
        End Sub

        Public Function Clone() As Object Implements System.ICloneable.Clone

            Dim rxn As Reaction = ObjectCopy(Me)
            rxn.ID = Guid.NewGuid.ToString

            Return rxn

        End Function

        Function ObjectCopy(ByVal obj As Reaction) As Reaction

            Dim objMemStream As New IO.MemoryStream(100000)
            Dim objBinaryFormatter As New BinaryFormatter(Nothing, New StreamingContext(StreamingContextStates.Clone))

            objBinaryFormatter.Serialize(objMemStream, obj)

            objMemStream.Seek(0, SeekOrigin.Begin)

            ObjectCopy = objBinaryFormatter.Deserialize(objMemStream)

            objMemStream.Close()
        End Function

#End Region

        Public Function LoadData(data As System.Collections.Generic.List(Of System.Xml.Linq.XElement)) As Boolean Implements XMLSerializer.Interfaces.ICustomXMLSerialization.LoadData

            XMLSerializer.XMLSerializer.Deserialize(Me, data)

            Dim ci As CultureInfo = CultureInfo.InvariantCulture
            For Each xel2 As XElement In (From xel As XElement In data Select xel Where xel.Name = "Compounds").Elements
                Me.Components.Add(xel2.@Name, New ReactionStoichBase(xel2.@Name, Double.Parse(xel2.@StoichCoeff, ci), xel2.@IsBaseReactant, Double.Parse(xel2.@DirectOrder, ci), Double.Parse(xel2.@ReverseOrder, ci)))
            Next
            Return True
        End Function

        Public Function SaveData() As System.Collections.Generic.List(Of System.Xml.Linq.XElement) Implements XMLSerializer.Interfaces.ICustomXMLSerialization.SaveData

            Dim elements As List(Of System.Xml.Linq.XElement) = XMLSerializer.XMLSerializer.Serialize(Me)
            Dim ci As CultureInfo = CultureInfo.InvariantCulture

            With elements

                .Add(New XElement("Compounds"))
                For Each rsb As ReactionStoichBase In Me.Components.Values
                    .Item(.Count - 1).Add(New XElement("Compound", New XAttribute("Name", rsb.CompName),
                                                    New XAttribute("StoichCoeff", rsb.StoichCoeff.ToString(ci)),
                                                    New XAttribute("DirectOrder", rsb.DirectOrder.ToString(ci)),
                                                    New XAttribute("ReverseOrder", rsb.ReverseOrder.ToString(ci)),
                                                    New XAttribute("IsBaseReactant", rsb.IsBaseReactant)))
                Next

            End With

            Return elements

        End Function

    End Class

    <System.Serializable()> Public Class ReactionSet

        Implements ICloneable, XMLSerializer.Interfaces.ICustomXMLSerialization

        'CAPE-OPEN Reaction Package Interfaces
        Implements CapeOpen.ICapeIdentification
        Implements CapeOpen.ICapeUtilities, CapeOpen.ICapeCollection, CapeOpen.ICapeReactionsRoutine, CapeOpen.ICapeReactionChemistry
        Implements CapeOpen.ICapeThermoContext, CapeOpen.ICapeKineticReactionContext, CapeOpen.ICapeReactionProperties
        Implements CAPEOPEN110.ICapeThermoMaterialContext

        Protected m_reactionset As Dictionary(Of String, ReactionSetBase)
        Protected m_attachedToGUID As List(Of String)

        Protected m_ID As String = ""
        Protected m_name As String = ""
        Protected m_description As String = ""

#Region "    DWSIM Specific"

        Public Function GetIDbyName(ByVal reactname As String)
            Dim ID As String = ""
            For Each r As Reaction In Me.m_pme.Options.Reactions.Values
                If r.Name = reactname Then
                    ID = r.ID
                    Exit For
                End If
            Next
            Return ID
        End Function

        Public ReadOnly Property Reactions() As Dictionary(Of String, ReactionSetBase)
            Get
                Return Me.m_reactionset
            End Get
        End Property

        Public ReadOnly Property AttachedTo() As List(Of String)
            Get
                Return Me.m_attachedToGUID
            End Get
        End Property

        Public Property ID() As String
            Get
                Return Me.m_ID
            End Get
            Set(ByVal value As String)
                Me.m_ID = value
            End Set
        End Property

        Public Property Name() As String
            Get
                Return Me.m_name
            End Get
            Set(ByVal value As String)
                Me.m_name = value
            End Set
        End Property

        Public Property Description() As String
            Get
                Return Me.m_description
            End Get
            Set(ByVal value As String)
                Me.m_description = value
            End Set
        End Property

        Sub New()
            MyBase.New()
            Me.m_reactionset = New Dictionary(Of String, ReactionSetBase)
        End Sub

        Sub New(ByVal id As String, ByVal name As String, ByVal description As String)
            Me.New()
            Me.ID = id
            Me.Name = name
            Me.Description = description
        End Sub

        Public Function Clone() As Object Implements System.ICloneable.Clone

            Dim rxs As ReactionSet = ObjectCopy(Me)
            rxs.ID = Guid.NewGuid.ToString

            Return rxs

        End Function

        Function ObjectCopy(ByVal obj As ReactionSet) As ReactionSet

            Dim objMemStream As New IO.MemoryStream(500000)
            Dim objBinaryFormatter As New BinaryFormatter(Nothing, New StreamingContext(StreamingContextStates.Clone))

            objBinaryFormatter.Serialize(objMemStream, obj)

            objMemStream.Seek(0, SeekOrigin.Begin)

            ObjectCopy = objBinaryFormatter.Deserialize(objMemStream)

            objMemStream.Close()
        End Function

#End Region

#Region "    CAPE-OPEN Reaction Package Methods and Properties"

        Protected m_params As CapeOpen.ParameterCollection
        Protected m_str As MaterialStream
        <System.NonSerialized()> Protected m_pme As FormFlowsheet
        Protected m_kre As Reaction

        Public Function Count() As Integer Implements CapeOpen.ICapeCollection.Count
            Return m_params.Count
        End Function

        Public Function Item(ByVal index As Object) As Object Implements CapeOpen.ICapeCollection.Item
            Dim mypar As Object = Nothing
            If IsNumeric(index) Then
                mypar = m_params(index - 1)
                Return mypar
            Else
                For Each p As CapeOpen.ICapeIdentification In m_params
                    If p.ComponentName = index Then
                        mypar = p
                        Exit For
                    End If
                Next
                Return mypar
            End If
        End Function

        ''' <summary>
        ''' Returns the name of the base reactant for a particular reaction.
        ''' </summary>
        ''' <param name="reacId">The reaction identifier</param>
        ''' <returns></returns>
        ''' <remarks></remarks>
        Public Function GetBaseReactant(ByVal reacId As String) As String Implements CapeOpen.ICapeReactionChemistry.GetBaseReactant
            Return Me.m_pme.Options.Reactions(GetIDbyName(reacId)).BaseReactant
        End Function

        ''' <summary>
        ''' Gets the number of compounds occurring in a particular reaction within a Reactions Package.
        ''' </summary>
        ''' <param name="reacID">The reaction identifier</param>
        ''' <returns></returns>
        ''' <remarks></remarks>
        Public Function GetNumberOfReactionCompounds(ByVal reacID As String) As Integer Implements CapeOpen.ICapeReactionChemistry.GetNumberOfReactionCompounds
            Return Me.m_pme.Options.Reactions(GetIDbyName(reacID)).Components.Count
        End Function

        ''' <summary>
        ''' Gets the number of reactions contained in the Reactions Package.
        ''' </summary>
        ''' <returns></returns>
        ''' <remarks></remarks>
        Public Function GetNumberOfReactions() As Integer Implements CapeOpen.ICapeReactionChemistry.GetNumberOfReactions
            Return Me.Reactions.Count
        End Function

        ''' <summary>
        ''' Returns the number and ids of the compounds in the specified phase.
        ''' </summary>
        ''' <param name="reacID">Label of the required phase</param>
        ''' <param name="compNo"></param>
        ''' <param name="compIds">The ids of the compounds present in the specified phase.</param>
        ''' <remarks></remarks>
        Public Sub GetPhaseCompounds(ByVal reacID As String, ByRef compNo As Integer, ByRef compIds As Object) Implements CapeOpen.ICapeReactionChemistry.GetPhaseCompounds
            Throw New CapeOpen.CapeNoImplException()
        End Sub

        ''' <summary>
        ''' Get the identifiers of the components participating in the specified reaction within the reaction set defined in the
        ''' Reactions Package.
        ''' </summary>
        ''' <param name="reacId">The reaction identifier</param>
        ''' <param name="compIds">List of compound IDs</param>
        ''' <param name="compCharge">The charge for each compound</param>
        ''' <param name="compCASNumber">The CAS Registry numbers for the compounds</param>
        ''' <remarks></remarks>
        Public Sub GetReactionCompoundIds(ByVal reacId As String, ByRef compIds As Object, ByRef compCharge As Object, ByRef compCASNumber As Object) Implements CapeOpen.ICapeReactionChemistry.GetReactionCompoundIds
            Dim i As Integer = 0
            Dim narr, carr, charr As New ArrayList
            Dim nm As Object = Nothing
            Dim fm As Object = Nothing
            Dim ci As Object = Nothing
            Dim bp As Object = Nothing
            Dim mw As Object = Nothing
            Dim cid As Object = Nothing
            Me.m_str.GetCompoundList(cid, fm, nm, bp, mw, ci)
            Dim n As Integer = CType(nm, String()).Length - 1
            For Each c As ReactionStoichBase In Me.m_pme.Options.Reactions(GetIDbyName(reacId)).Components.Values
                With Me.m_pme.Options.SelectedComponents(c.CompName)
                    For i = 0 To n
                        If ci(i) = .CAS_Number Then
                            narr.Add(cid(i))
                            carr.Add(ci(i))
                            charr.Add(0.0#)
                            Exit For
                        End If
                    Next
                End With
            Next
            Dim names(narr.Count - 1), casids(narr.Count - 1) As String, charges(narr.Count - 1) As Double
            Array.Copy(narr.ToArray, names, narr.Count)
            Array.Copy(carr.ToArray, casids, narr.Count)
            Array.Copy(charr.ToArray, charges, narr.Count)
            compIds = names
            compCharge = charges
            compCASNumber = casids
        End Sub

        ''' <summary>
        ''' Gets the concentration basis required that will be used by a particular reaction in its rate equation.
        ''' Qualifiers defined in the THRM spec can be used here (i.e. “fugacity”, “moleFraction”, etc)
        ''' </summary>
        ''' <param name="reacId">The reaction identifier</param>
        ''' <returns></returns>
        ''' <remarks></remarks>
        Public Function GetReactionConcBasis(ByVal reacId As String) As String Implements CapeOpen.ICapeReactionChemistry.GetReactionConcBasis
            Select Case Me.m_pme.Options.Reactions(GetIDbyName(reacId)).ReactionBasis
                Case ReactionBasis.Activity
                    Return "activity"
                Case ReactionBasis.Fugacity
                    Return "fugacity"
                Case ReactionBasis.MassConc
                    Return "concentration"
                Case ReactionBasis.MassFrac
                    Return "massfraction"
                Case ReactionBasis.MolarConc
                    Return "molarity"
                Case ReactionBasis.MolarFrac
                    Return "molefraction"
                Case Else
                    Throw New CapeOpen.CapeNoImplException
            End Select
        End Function

        ''' <summary>
        ''' Returns a collection containing the rate expression parameters for a particular reaction.
        ''' </summary>
        ''' <param name="reacId">Identifier of a particular reaction</param>
        ''' <returns></returns>
        ''' <remarks>GetReactionParameters returns a collection of CAPE-OPEN parameters [6] that characterize the rate expression
        ''' used by the reaction model in a Reaction Package. For a PowerLaw model this collection would contain
        ''' parameters for activation energy, pre-exponential factor and compound exponents for example. It is up to the
        ''' Reactions Package implementor to decide whether a client can update the values of these parameters. If this
        ''' operation is allowed, then the implementor must also provide support for persistence [5] interfaces, so that the
        ''' updated values can be saved and restored. In this case the COSE is also responsible for calling the persistence
        ''' methods.
        ''' Deliberately, the standard does not define the names of the parameters that may appear in such a collection, even
        ''' for well-known reaction models, such as PowerLaw and Langmuir – Hinshelwood – Hougen – Watson
        ''' (LHHW). This is because the formulation of well-known models is not fixed, and because the standard needs to
        ''' support custom models as well as the well-known models.
        ''' This decision is not expected to be restrictive: in most cases the (software) client of a Reactions Package does
        ''' not need to know what model the package implements and what parameters it has. However, the parameters may
        ''' be of interest to an end-user who wants to adjust or estimate the parameter values. In these cases the COSE can
        ''' invoke the Reaction Package’s own GUI, or, if it doesn’t have one, present the parameters in a generic grid. It is
        ''' the Reaction Package implementor’s responsibility to provide documentation for the parameters so that an enduser
        ''' can understand how they are used.</remarks>
        Public Function GetReactionParameters(ByVal reacId As String) As Object Implements CapeOpen.ICapeReactionChemistry.GetReactionParameters
            Throw New CapeOpen.CapeNoImplException("GetReactionParameters not implemented.")
        End Function

        ''' <summary>
        ''' Gets the phase on which a particular reaction contained in the Reactions Package will take place.
        ''' </summary>
        ''' <param name="reacId">The reaction identifier</param>
        ''' <returns></returns>
        ''' <remarks>The string returned by this method must match one of the phase labels known to the Property Package.</remarks>
        Public Function GetReactionPhase(ByVal reacId As String) As String Implements CapeOpen.ICapeReactionChemistry.GetReactionPhase
            Select Case Me.m_pme.Options.Reactions(GetIDbyName(reacId)).ReactionPhase
                Case PhaseName.Vapor
                    Return Me.m_str.PropertyPackage.PhaseMappings("Vapor").PhaseLabel
                Case PhaseName.Liquid
                    Return Me.m_str.PropertyPackage.PhaseMappings("Liquid1").PhaseLabel
                Case Else
                    Return "Overall"
            End Select
        End Function

        ''' <summary>
        ''' Gets the phase on which the reactions contained in the package will take place. 
        ''' </summary>
        ''' <param name="reacId">The reaction identifier</param>
        ''' <returns></returns>
        ''' <remarks>The reaction rate basis (i.e.
        ''' “Homogeneous” or “Heterogeneous”) Homogeneous reactions will be provided in kgmole/h/m3 and
        ''' heterogeneous will be provided in kgmole/h/kg-cat.
        ''' CapeReactionRateBasis:
        ''' CAPE_HOMOGENEOUS = 0,
        ''' CAPE_HETEROGENEOUS = 1,</remarks>
        Public Function GetReactionRateBasis(ByVal reacId As String) As CapeOpen.CapeReactionRateBasis Implements CapeOpen.ICapeReactionChemistry.GetReactionRateBasis
            Return CapeOpen.CapeReactionRateBasis.CAPE_HOMOGENEOUS
        End Function

        ''' <summary>
        ''' Returns the identifiers of all the reactions contained within the Reactions Package.
        ''' </summary>
        ''' <returns></returns>
        ''' <remarks></remarks>
        Public Function GetReactionsIds() As Object Implements CapeOpen.ICapeReactionChemistry.GetReactionsIds
            Dim narr As New ArrayList
            For Each r As ReactionSetBase In Me.Reactions.Values
                narr.Add(Me.m_pme.Options.Reactions(r.ReactionID).Name)
            Next
            Dim names(narr.Count - 1) As String
            Array.Copy(narr.ToArray, names, narr.Count)
            Return names
        End Function

        ''' <summary>
        ''' Returns the type of a particular reaction contained in the Reactions Package.
        ''' </summary>
        ''' <param name="reacID">The reaction identifier</param>
        ''' <returns>Returns the type of a particular reaction contained in the Reactions Package. CapeReactionType constants for the
        ''' various reaction:
        ''' CAPE_EQUILIBRIUM = 0,
        ''' CAPE_KINETIC = 1,</returns>
        ''' <remarks></remarks>
        Public Function GetReactionType(ByVal reacID As String) As CapeOpen.CapeReactionType Implements CapeOpen.ICapeReactionChemistry.GetReactionType
            Select Case Me.m_pme.Options.Reactions(GetIDbyName(reacID)).ReactionType
                Case ReactionType.Conversion
                    Return CapeOpen.CapeReactionType.CAPE_KINETIC
                Case ReactionType.Equilibrium
                    Return CapeOpen.CapeReactionType.CAPE_EQUILIBRIUM
                Case ReactionType.Heterogeneous_Catalytic
                    Return CapeOpen.CapeReactionType.CAPE_KINETIC
                Case ReactionType.Kinetic
                    Return CapeOpen.CapeReactionType.CAPE_KINETIC
            End Select
        End Function

        ''' <summary>
        ''' Returns the stoichiometric coefficients of the specified reaction (positive numbers indicate products, negative
        ''' numbers indicate reactants)
        ''' </summary>
        ''' <param name="reacId">The reaction identifier</param>
        ''' <returns></returns>
        ''' <remarks>The array of coefficients returned by this method is parallel to the array returned by calling
        ''' GetReactionCompuoundIds meaning that the first coefficient corresponds to the first compound and so on.</remarks>
        Public Function GetStoichiometricCoefficients(ByVal reacId As String) As Object Implements CapeOpen.ICapeReactionChemistry.GetStoichiometricCoefficients
            Dim narr As New ArrayList
            For Each c As ReactionStoichBase In Me.m_pme.Options.Reactions(GetIDbyName(reacId)).Components.Values
                narr.Add(c.StoichCoeff)
            Next
            Dim sc(narr.Count - 1) As Double
            Array.Copy(narr.ToArray, sc, narr.Count)
            Return sc
        End Function

        Public Sub Edit() Implements CapeOpen.ICapeUtilities.Edit
            Dim rm As New FormReacManager
            rm.Show()
        End Sub

        Public Sub Initialize() Implements CapeOpen.ICapeUtilities.Initialize
            If m_params Is Nothing Then
                m_params = New CapeOpen.ParameterCollection
                'm_params.Add(...)
            End If
        End Sub

        Public ReadOnly Property parameters() As Object Implements CapeOpen.ICapeUtilities.parameters
            Get
                Return m_params
            End Get
        End Property

        Public WriteOnly Property simulationContext() As Object Implements CapeOpen.ICapeUtilities.simulationContext
            Set(ByVal value As Object)
                m_pme = value
            End Set
        End Property

        Public Sub Terminate() Implements CapeOpen.ICapeUtilities.Terminate
            'do nothing
        End Sub

        Public Sub SetReactionObject(ByRef reactionsObject As Object) Implements CapeOpen.ICapeKineticReactionContext.SetReactionObject
            Me.m_kre = reactionsObject
        End Sub

        ''' <summary>
        ''' Gets the value of the specified reaction property within a reactions object.\</summary>
        ''' <param name="property">The Reaction Property to be got.</param>
        ''' <param name="phase">The qualified phase for the Reaction Property.</param>
        ''' <param name="reacIds">The qualified reactions for the Reaction Property. NULL to specify all reactions in the set.</param>
        ''' <param name="basis">Qualifies the basis of the Reaction Property (i.e., mass /mole). Default is mole. Use NULL only 
        ''' as a placeholder for property for which basis does not apply. This qualifier could be extended with values such as 
        ''' activity, fugacity, fractions, molality…This way when an equilibrium constant is requested its basis can be specified</param>
        ''' <returns></returns>
        ''' <remarks>The qualifiers passed in determine the reactions, phase and calculation basis for 
        ''' which the property will be got. The order of the array is the same as in the passed in reacIds 
        ''' array (i.e. property value for reaction reacIds[1] will be stored in property[1])</remarks>
        Public Function GetReactionProp(ByVal [property] As String, ByVal phase As String, ByVal reacIds As Object, ByVal basis As String) As Object Implements CapeOpen.ICapeReactionProperties.GetReactionProp
            Dim res As New ArrayList
            For Each rid As String In reacIds
                Dim ro As Reaction = Me.m_pme.Options.Reactions(GetIDbyName(rid))
                With ro
                    Select Case [property].ToLower
                        Case "reactionrate"
                            res.Add(ro.Rate)
                        Case "chemicalequilibriumconstant"
                            res.Add(ro.Kvalue)
                        Case "enthalpyofreaction"
                            Select Case basis.ToLower
                                Case "mole"
                                    res.Add(ro.ReactionHeatCO)
                                Case "mass"
                                    res.Add(ro.ReactionHeatCO / Me.m_str.Phases(0).Properties.molecularWeight.GetValueOrDefault)
                            End Select
                        Case Else
                            Throw New CapeOpen.CapeNoImplException
                    End Select
                End With
            Next
            Dim propvals(res.Count - 1) As Double
            Array.Copy(res.ToArray, propvals, res.Count)
            Return propvals
        End Function

        ''' <summary>
        ''' Sets the values of the specified reaction property within a reactions object. The qualifiers passed in determine the
        ''' reactions, phase and calculation basis for which the property will be got
        ''' </summary>
        ''' <param name="property">The Reaction Property to be got.</param>
        ''' <param name="phase">The qualified phase for the Reaction Property.</param>
        ''' <param name="reacIds">The qualified reactions for the Reaction Property. NULL to specify all reactions in the set.</param>
        ''' <param name="basis">Qualifies the basis of the Reaction Property (i.e., mass /mole).
        ''' Default is mole. Use NULL only as a placeholder for property
        ''' for which basis does not apply.
        ''' This qualifier could be extended with values such as activity,
        ''' fugacity, fractions, molality…This way when an equilibrium
        ''' constant is requested its basis can be specified</param>
        ''' <param name="propVals">The values of the requested reaction property. The order of the
        ''' array is the same as in the passed in reacIds array (i.e. property
        ''' value for reaction reacIds[1] will be stored in property[1])</param>
        ''' <remarks></remarks>
        Public Sub SetReactionProp(ByVal [property] As String, ByVal phase As String, ByVal reacIds As Object, ByVal basis As String, ByVal propVals As Object) Implements CapeOpen.ICapeReactionProperties.SetReactionProp
            Dim i As Integer = 0
            For Each rid As String In reacIds
                Dim ro As Reaction = Me.m_pme.Options.Reactions(GetIDbyName(rid))
                With ro
                    Select Case [property].ToLower
                        Case "reactionrate"
                            ro.Rate = propVals(i)
                        Case "chemicalequilibriumconstant"
                            ro.Kvalue = propVals(i)
                        Case "enthalpyofreaction"
                            ro.ReactionHeatCO = propVals(i)
                        Case Else
                            Throw New CapeOpen.CapeNoImplException
                    End Select
                End With
                i += 1
            Next
        End Sub

        ''' <summary>
        ''' The Reactions Package is passed a list of reaction properties to be calculated.
        ''' </summary>
        ''' <param name="props">The Reaction Properties to be calculated.</param>
        ''' <param name="phase">The qualified phase for the results.</param>
        ''' <param name="reacIds">The qualified reactions for the results. NULL to specify all 
        ''' reactions in the set.</param>
        ''' <param name="basis">Qualifies the basis of the result (i.e., mass /mole). Default is
        ''' mole. Use NULL only as a placeholder for properties for which
        ''' basis does not apply.</param>
        ''' <remarks>The Reactions Package is passed a list of reaction properties to be calculated, the reaction IDS for which the
        ''' properties are required, and the calculation basis for the reaction properties (i.e. mole or mass). A material object
        ''' containing the thermodynamic state variables that need to be used for calculating the reaction properties (e.g. T,
        ''' P and compositions) is passed separately via a call to the setMaterial method of the Reaction Package’s
        ''' ICapeThermoContext interface.
        ''' The results of the calculation will be written to the reaction object passed to the Reactions Package via either the
        ''' ICapeKineticReactionContext interface for a kinetic reaction package, or the ICapeElectrolyteReactionContext
        ''' interface for an Electrolyte Property Package.</remarks>
        Public Sub CalcReactionProp(ByVal props As Object, ByVal phase As String, ByVal reacIds As Object, ByVal basis As String) Implements CapeOpen.ICapeReactionsRoutine.CalcReactionProp

            For Each rid As String In reacIds

                Dim ro As Reaction = Me.m_pme.Options.Reactions(GetIDbyName(rid))

                With ro
                    For Each p As String In props
                        Select Case p.ToLower
                            Case "reactionrate"

                                Dim ims As MaterialStream = Me.m_str
                                Dim co As New Dictionary(Of String, Double)

                                'initial mole flows

                                For Each sb As ReactionStoichBase In .Components.Values

                                    Select Case ro.ReactionPhase
                                        Case PhaseName.Liquid
                                            co.Add(sb.CompName, ims.Phases(1).Compounds(sb.CompName).MolarFlow.GetValueOrDefault / ims.Phases(1).Properties.volumetric_flow.GetValueOrDefault)
                                        Case PhaseName.Vapor
                                            co.Add(sb.CompName, ims.Phases(2).Compounds(sb.CompName).MolarFlow.GetValueOrDefault / ims.Phases(2).Properties.volumetric_flow.GetValueOrDefault)
                                        Case PhaseName.Mixture
                                            co.Add(sb.CompName, ims.Phases(0).Compounds(sb.CompName).MolarFlow.GetValueOrDefault / ims.Phases(0).Properties.volumetric_flow.GetValueOrDefault)
                                    End Select

                                Next

                                Dim T = ims.Phases(0).Properties.temperature.GetValueOrDefault

                                Dim kxf As Double = ro.A_Forward * Math.Exp(-ro.E_Forward / (8.314 * T))
                                Dim kxr As Double = ro.A_Reverse * Math.Exp(-ro.E_Reverse / (8.314 * T))

                                Dim rx As Double = 0
                                Dim rxf As Double = 1
                                Dim rxr As Double = 1

                                'kinetic expression

                                For Each sb As ReactionStoichBase In ro.Components.Values
                                    rxf *= co(sb.CompName) ^ sb.DirectOrder
                                    rxr *= co(sb.CompName) ^ sb.ReverseOrder
                                Next

                                rx = kxf * rxf - kxr * rxr

                                ro.Rate = rx

                            Case "chemicalequilibriumconstant"

                                Dim T = Me.m_str.Phases(0).Properties.temperature.GetValueOrDefault

                                'equilibrium constant calculation

                                Select Case .KExprType
                                    Case Reaction.KOpt.Constant
                                        .Kvalue = .ConstantKeqValue
                                    Case Reaction.KOpt.Expression
                                        If .ExpContext Is Nothing Then
                                            .ExpContext = New Ciloci.Flee.ExpressionContext
                                            With .ExpContext
                                                .Imports.AddType(GetType(System.Math))
                                            End With
                                        End If
                                        .ExpContext.Variables.Add("T", T)
                                        .Expr = .ExpContext.CompileGeneric(Of Double)(.Expression)
                                        .Kvalue = Math.Exp(.Expr.Evaluate)
                                    Case Reaction.KOpt.Gibbs
                                        Dim id(.Components.Count - 1) As String
                                        Dim stcoef(.Components.Count - 1) As Double
                                        Dim bcidx As Integer = 0
                                        Dim j As Integer = 0
                                        For Each sb As ReactionStoichBase In .Components.Values
                                            id(j) = sb.CompName
                                            stcoef(j) = sb.StoichCoeff
                                            If sb.IsBaseReactant Then bcidx = j
                                            j += 1
                                        Next
                                        Dim DelG_RT = Me.m_str.PropertyPackage.AUX_DELGig_RT(298.15, T, id, stcoef, bcidx)
                                        .Kvalue = Math.Exp(-DelG_RT)
                                End Select

                            Case "enthalpyofreaction"

                                'Dim rh As Double = 0.0#

                                'Dim T = Me.m_str.Phases(0).Properties.temperature.GetValueOrDefault

                                'Dim id(.Components.Count - 1) As String
                                'Dim stcoef(.Components.Count - 1) As Double
                                'Dim bcidx As Integer = 0
                                'Dim j As Integer = 0
                                'For Each sb As ReactionStoichBase In .Components.Values
                                '    id(j) = sb.CompName
                                '    stcoef(j) = sb.StoichCoeff
                                '    If sb.IsBaseReactant Then bcidx = j
                                '    j += 1
                                'Next

                                'Me.m_str.PropertyPackage.CurrentMaterialStream = Me.m_str
                                'rh = Me.m_str.PropertyPackage.AUX_DELHig_RT(298.15, T, id, stcoef, bcidx) * 8.314 * T

                                .ReactionHeatCO = .ReactionHeat

                            Case Else

                                Throw New CapeOpen.CapeNoImplException

                        End Select
                    Next
                End With
            Next

        End Sub

        Public Sub SetMaterial(ByVal materialObject As Object) Implements CapeOpen.ICapeThermoContext.SetMaterial
            If Not System.Runtime.InteropServices.Marshal.IsComObject(materialObject) Then
                Me.m_str = materialObject
            Else
                'get ID
                Dim id As String = CType(materialObject, CapeOpen.ICapeIdentification).ComponentDescription
                Dim myms As MaterialStream = Me.m_pme.Collections.FlowsheetObjectCollection(id)
                'proceed with copy
                Me.m_str = myms
            End If
        End Sub

        Public Property ComponentDescription() As String Implements CapeOpen.ICapeIdentification.ComponentDescription
            Get
                Return m_description
            End Get
            Set(ByVal value As String)
                m_description = value
            End Set
        End Property

        Public Property ComponentName() As String Implements CapeOpen.ICapeIdentification.ComponentName
            Get
                Return m_name
            End Get
            Set(ByVal value As String)
                m_name = value
            End Set
        End Property

        Public Sub SetMaterial1(ByVal material As Object) Implements CAPEOPEN110.ICapeThermoMaterialContext.SetMaterial
            If Not System.Runtime.InteropServices.Marshal.IsComObject(material) Then
                Me.m_str = material
            Else
                'get ID
                Dim id As String = CType(material, CapeOpen.ICapeIdentification).ComponentDescription
                Dim myms As MaterialStream = Me.m_pme.Collections.FlowsheetObjectCollection(id)
                'proceed with copy
                Me.m_str = myms
            End If

        End Sub

        Public Sub UnsetMaterial() Implements CAPEOPEN110.ICapeThermoMaterialContext.UnsetMaterial
            Me.m_str = Nothing
        End Sub

#End Region

        Public Function LoadData(data As System.Collections.Generic.List(Of System.Xml.Linq.XElement)) As Boolean Implements XMLSerializer.Interfaces.ICustomXMLSerialization.LoadData

            Me.ID = (From xel As XElement In data Select xel Where xel.Name = "ID").SingleOrDefault.Value
            Me.Name = (From xel As XElement In data Select xel Where xel.Name = "Name").SingleOrDefault.Value
            Me.Description = (From xel As XElement In data Select xel Where xel.Name = "Description").SingleOrDefault.Value

            For Each xel2 As XElement In (From xel As XElement In data Select xel Where xel.Name = "Reactions").Elements
                Me.Reactions.Add(xel2.@Key, New ReactionSetBase(xel2.@ReactionID, xel2.@Rank, xel2.@IsActive))
            Next
            Return True

        End Function

        Public Function SaveData() As System.Collections.Generic.List(Of System.Xml.Linq.XElement) Implements XMLSerializer.Interfaces.ICustomXMLSerialization.SaveData

            Dim elements As New List(Of System.Xml.Linq.XElement)
            Dim ci As CultureInfo = CultureInfo.InvariantCulture

            With elements

                .Add(New XElement("ID", m_ID))
                .Add(New XElement("Name", m_name))
                .Add(New XElement("Description", m_description))

                .Add(New XElement("Reactions"))

                For Each kvp As KeyValuePair(Of String, ReactionSetBase) In Reactions
                    .Item(.Count - 1).Add(New XElement("Reaction", New XAttribute("Key", kvp.Key),
                                                                    New XAttribute("ReactionID", kvp.Value.ReactionID),
                                                                    New XAttribute("Rank", kvp.Value.Rank),
                                                                    New XAttribute("IsActive", kvp.Value.IsActive)))
                Next

            End With

            Return elements

        End Function

    End Class

    <System.Serializable()> Public Class ReactionSetBase

        Protected m_reactionID As String = ""
        Protected m_rank As Integer = 0
        Protected m_isActive As Boolean = True

        Public Property ReactionID() As String
            Get
                Return Me.m_reactionID
            End Get
            Set(ByVal value As String)
                Me.m_reactionID = value
            End Set
        End Property

        Public Property Rank() As Integer
            Get
                Return Me.m_rank
            End Get
            Set(ByVal value As Integer)
                Me.m_rank = value
            End Set
        End Property

        Public Property IsActive() As Boolean
            Get
                Return Me.m_isActive
            End Get
            Set(ByVal value As Boolean)
                Me.m_isActive = value
            End Set
        End Property

        Sub New()

        End Sub

        Sub New(ByVal id As String, ByVal rank As Integer, ByVal isactive As Boolean)
            Me.IsActive = isactive
            Me.Rank = rank
            Me.ReactionID = id
        End Sub

    End Class

    <System.Serializable()> Public Class ReactionStoichBase

        Protected m_compname As String = ""
        Protected m_stoichcoeff As Double = 0.0#
        Protected m_directorder As Double = 0.0#
        Protected m_reverseorder As Double = 0.0#
        Protected m_basecomp As Boolean = False

        Public Property CompName() As String
            Get
                Return m_compname
            End Get
            Set(ByVal value As String)
                m_compname = value
            End Set
        End Property

        Public Property StoichCoeff() As Double
            Get
                Return Me.m_stoichcoeff
            End Get
            Set(ByVal value As Double)
                Me.m_stoichcoeff = value
            End Set
        End Property

        Public Property DirectOrder() As Double
            Get
                Return Me.m_directorder
            End Get
            Set(ByVal value As Double)
                Me.m_directorder = value
            End Set
        End Property

        Public Property ReverseOrder() As Double
            Get
                Return Me.m_reverseorder
            End Get
            Set(ByVal value As Double)
                Me.m_reverseorder = value
            End Set
        End Property

        Public Property IsBaseReactant() As Boolean
            Get
                Return Me.m_basecomp
            End Get
            Set(ByVal value As Boolean)
                Me.m_basecomp = value
            End Set
        End Property

        Public Sub New(ByVal name As String, ByVal stoichcoeff As Double, ByVal isbasereactant As Boolean, ByVal directorder As Double, ByVal reversorder As Double)
            Me.m_compname = name
            Me.m_stoichcoeff = stoichcoeff
            Me.m_basecomp = isbasereactant
            Me.m_directorder = directorder
            Me.m_reverseorder = reversorder
        End Sub

    End Class

    <System.Serializable()> Public Class ReactionsCollection
        Public Collection() As Reaction
        Sub New()

        End Sub
    End Class

#Region "Subclasses"

    <System.Serializable()> Public Class TemperatureDependentProperties

        Protected tdp_idealGasHeatCapacity As Nullable(Of Double) = Nothing
        Protected tdp_surfaceTension As Nullable(Of Double) = Nothing
        Protected tdp_thermalConductivityOfLiquid As Nullable(Of Double) = Nothing
        Protected tdp_thermalConductivityOfVapor As Nullable(Of Double) = Nothing
        Protected tdp_vaporPressure As Nullable(Of Double) = Nothing
        Protected tdp_viscosityOfLiquid As Nullable(Of Double) = Nothing
        Protected tdp_viscosityOfVapor As Nullable(Of Double) = Nothing

        Public Property idealGasHeatCapacity() As Nullable(Of Double)
            Get
                Return tdp_idealGasHeatCapacity
            End Get
            Set(ByVal value As Nullable(Of Double))
                tdp_idealGasHeatCapacity = value
            End Set
        End Property
        Public Property thermalConductivityOfLiquid() As Nullable(Of Double)
            Get
                Return tdp_thermalConductivityOfLiquid
            End Get
            Set(ByVal value As Nullable(Of Double))
                tdp_thermalConductivityOfLiquid = value
            End Set
        End Property
        Public Property thermalConductivityOfVapor() As Nullable(Of Double)
            Get
                Return tdp_thermalConductivityOfVapor
            End Get
            Set(ByVal value As Nullable(Of Double))
                tdp_thermalConductivityOfVapor = value
            End Set
        End Property
        Public Property vaporPressure() As Nullable(Of Double)
            Get
                Return tdp_vaporPressure
            End Get
            Set(ByVal value As Nullable(Of Double))
                tdp_vaporPressure = value
            End Set
        End Property
        Public Property viscosityOfLiquid() As Nullable(Of Double)
            Get
                Return tdp_viscosityOfLiquid
            End Get
            Set(ByVal value As Nullable(Of Double))
                tdp_viscosityOfLiquid = value
            End Set
        End Property
        Public Property viscosityOfVapor() As Nullable(Of Double)
            Get
                Return tdp_viscosityOfVapor
            End Get
            Set(ByVal value As Nullable(Of Double))
                tdp_viscosityOfVapor = value
            End Set
        End Property
        Public Property surfaceTension() As Nullable(Of Double)
            Get
                Return tdp_surfaceTension
            End Get
            Set(ByVal value As Nullable(Of Double))
                tdp_surfaceTension = value
            End Set
        End Property

    End Class

    <System.Serializable()> Public Class PressureDependentProperties

        Protected pdp_boilingPointTemperature As Nullable(Of Double) = Nothing
        Public Property boilingPointTemperature() As Nullable(Of Double)
            Get
                Return pdp_boilingPointTemperature
            End Get
            Set(ByVal value As Nullable(Of Double))
                pdp_boilingPointTemperature = value
            End Set
        End Property
        Protected pdp_meltingTemperature As Nullable(Of Double) = Nothing
        Public Property meltingTemperature() As Nullable(Of Double)
            Get
                Return pdp_meltingTemperature
            End Get
            Set(ByVal value As Nullable(Of Double))
                pdp_meltingTemperature = value
            End Set
        End Property

    End Class

    <System.Serializable()> Public Class SinglePhaseMixtureProperties

        Public Property osmoticCoefficient As Nullable(Of Double)
        Public Property freezingPointDepression As Nullable(Of Double)
        Public Property freezingPoint As Nullable(Of Double)
        Public Property ionicStrength As Nullable(Of Double)
        Public Property pH As Nullable(Of Double)

        Protected _dewtemperature As Nullable(Of Double) = Nothing
        Public Property dewTemperature() As Nullable(Of Double)
            Get
                Return _dewtemperature
            End Get
            Set(ByVal value As Nullable(Of Double))
                _dewtemperature = value
            End Set
        End Property


        Protected _dewpressure As Nullable(Of Double) = Nothing
        Public Property dewPressure() As Nullable(Of Double)
            Get
                Return _dewpressure
            End Get
            Set(ByVal value As Nullable(Of Double))
                _dewpressure = value
            End Set
        End Property

        Protected _bubbletemperature As Nullable(Of Double) = Nothing
        Public Property bubbleTemperature() As Nullable(Of Double)
            Get
                Return _bubbletemperature
            End Get
            Set(ByVal value As Nullable(Of Double))
                _bubbletemperature = value
            End Set
        End Property


        Protected _bubblepressure As Nullable(Of Double) = Nothing
        Public Property bubblePressure() As Nullable(Of Double)
            Get
                Return _bubblepressure
            End Get
            Set(ByVal value As Nullable(Of Double))
                _bubblepressure = value
            End Set
        End Property

        Protected spmp_activity As Nullable(Of Double) = Nothing
        Public Property activity() As Nullable(Of Double)
            Get
                Return spmp_activity
            End Get
            Set(ByVal value As Nullable(Of Double))
                spmp_activity = value
            End Set
        End Property
        Protected spmp_activityCoefficient As Nullable(Of Double) = Nothing
        Public Property activityCoefficient() As Nullable(Of Double)
            Get
                Return spmp_activityCoefficient
            End Get
            Set(ByVal value As Nullable(Of Double))
                spmp_activityCoefficient = value
            End Set
        End Property
        Protected spmp_compressibility As Nullable(Of Double) = Nothing
        Public Property compressibility() As Nullable(Of Double)
            Get
                Return spmp_compressibility
            End Get
            Set(ByVal value As Nullable(Of Double))
                spmp_compressibility = value
            End Set
        End Property
        Protected spmp_compressibilityFactor As Nullable(Of Double) = Nothing
        Public Property compressibilityFactor() As Nullable(Of Double)
            Get
                Return spmp_compressibilityFactor
            End Get
            Set(ByVal value As Nullable(Of Double))
                spmp_compressibilityFactor = value
            End Set
        End Property
        Protected spmp_density As Nullable(Of Double) = Nothing
        Public Property density() As Nullable(Of Double)
            Get
                Return spmp_density
            End Get
            Set(ByVal value As Nullable(Of Double))
                spmp_density = value
            End Set
        End Property
        Protected spmp_enthalpy As Nullable(Of Double) = Nothing
        Public Property enthalpy() As Nullable(Of Double)
            Get
                Return spmp_enthalpy
            End Get
            Set(ByVal value As Nullable(Of Double))
                spmp_enthalpy = value
            End Set
        End Property
        Protected spmp_entropy As Nullable(Of Double) = Nothing
        Public Property entropy() As Nullable(Of Double)
            Get
                Return spmp_entropy
            End Get
            Set(ByVal value As Nullable(Of Double))
                spmp_entropy = value
            End Set
        End Property
        Protected spmp_enthalpyF As Nullable(Of Double) = Nothing
        Public Property enthalpyF() As Nullable(Of Double)
            Get
                Return spmp_enthalpyF
            End Get
            Set(ByVal value As Nullable(Of Double))
                spmp_enthalpyF = value
            End Set
        End Property
        Protected spmp_entropyF As Nullable(Of Double) = Nothing
        Public Property entropyF() As Nullable(Of Double)
            Get
                Return spmp_entropyF
            End Get
            Set(ByVal value As Nullable(Of Double))
                spmp_entropyF = value
            End Set
        End Property
        Protected spmp_excessEnthalpy As Nullable(Of Double) = Nothing
        Public Property excessEnthalpy() As Nullable(Of Double)
            Get
                Return spmp_excessEnthalpy
            End Get
            Set(ByVal value As Nullable(Of Double))
                spmp_excessEnthalpy = value
            End Set
        End Property
        Protected spmp_excessEntropy As Nullable(Of Double) = Nothing
        Public Property excessEntropy() As Nullable(Of Double)
            Get
                Return spmp_excessEntropy
            End Get
            Set(ByVal value As Nullable(Of Double))
                spmp_excessEntropy = value
            End Set
        End Property
        Protected spmp_molarflow As Nullable(Of Double) = Nothing
        Public Property molarflow() As Nullable(Of Double)
            Get
                Return spmp_molarflow
            End Get
            Set(ByVal value As Nullable(Of Double))
                spmp_molarflow = value
            End Set
        End Property
        Protected spmp_massflow As Nullable(Of Double) = Nothing
        Public Property massflow() As Nullable(Of Double)
            Get
                Return spmp_massflow
            End Get
            Set(ByVal value As Nullable(Of Double))
                spmp_massflow = value
            End Set
        End Property
        Protected spmp_molarfraction As Nullable(Of Double) = Nothing
        Public Property molarfraction() As Nullable(Of Double)
            Get
                Return spmp_molarfraction
            End Get
            Set(ByVal value As Nullable(Of Double))
                spmp_molarfraction = value
            End Set
        End Property
        Protected spmp_massfraction As Nullable(Of Double) = Nothing
        Public Property massfraction() As Nullable(Of Double)
            Get
                Return spmp_massfraction
            End Get
            Set(ByVal value As Nullable(Of Double))
                spmp_massfraction = value
            End Set
        End Property
        Protected spmp_fugacity As Nullable(Of Double) = Nothing
        Public Property fugacity() As Nullable(Of Double)
            Get
                Return spmp_fugacity
            End Get
            Set(ByVal value As Nullable(Of Double))
                spmp_fugacity = value
            End Set
        End Property
        Protected spmp_fugacityCoefficient As Nullable(Of Double) = Nothing
        Public Property fugacityCoefficient() As Nullable(Of Double)
            Get
                Return spmp_fugacityCoefficient
            End Get
            Set(ByVal value As Nullable(Of Double))
                spmp_fugacityCoefficient = value
            End Set
        End Property
        Protected spmp_heatCapacityCp As Nullable(Of Double) = Nothing
        Public Property heatCapacityCp() As Nullable(Of Double)
            Get
                Return spmp_heatCapacityCp
            End Get
            Set(ByVal value As Nullable(Of Double))
                spmp_heatCapacityCp = value
            End Set
        End Property
        Protected spmp_heatCapacityCv As Nullable(Of Double) = Nothing
        Public Property heatCapacityCv() As Nullable(Of Double)
            Get
                Return spmp_heatCapacityCv
            End Get
            Set(ByVal value As Nullable(Of Double))
                spmp_heatCapacityCv = value
            End Set
        End Property
        Protected spmp_jouleThomsonCoefficient As Nullable(Of Double) = Nothing
        Public Property jouleThomsonCoefficient() As Nullable(Of Double)
            Get
                Return spmp_jouleThomsonCoefficient
            End Get
            Set(ByVal value As Nullable(Of Double))
                spmp_jouleThomsonCoefficient = value
            End Set
        End Property
        Protected spmp_logFugacityCoefficient As Nullable(Of Double) = Nothing
        Public Property logFugacityCoefficient() As Nullable(Of Double)
            Get
                Return spmp_logFugacityCoefficient
            End Get
            Set(ByVal value As Nullable(Of Double))
                spmp_logFugacityCoefficient = value
            End Set
        End Property
        Protected spmp_molecularWeight As Nullable(Of Double) = Nothing
        Public Property molecularWeight() As Nullable(Of Double)
            Get
                Return spmp_molecularWeight
            End Get
            Set(ByVal value As Nullable(Of Double))
                spmp_molecularWeight = value
            End Set
        End Property
        Protected spmp_pressure As Nullable(Of Double) = Nothing
        Public Property pressure() As Nullable(Of Double)
            Get
                Return spmp_pressure
            End Get
            Set(ByVal value As Nullable(Of Double))
                spmp_pressure = value
            End Set
        End Property
        Protected spmp_temperature As Nullable(Of Double) = Nothing
        Public Property temperature() As Nullable(Of Double)
            Get
                Return spmp_temperature
            End Get
            Set(ByVal value As Nullable(Of Double))
                spmp_temperature = value
            End Set
        End Property
        Protected spmp_speedOfSound As Nullable(Of Double) = Nothing
        Public Property speedOfSound() As Nullable(Of Double)
            Get
                Return spmp_speedOfSound
            End Get
            Set(ByVal value As Nullable(Of Double))
                spmp_speedOfSound = value
            End Set
        End Property
        Protected spmp_thermalConductivity As Nullable(Of Double) = Nothing
        Public Property thermalConductivity() As Nullable(Of Double)
            Get
                Return spmp_thermalConductivity
            End Get
            Set(ByVal value As Nullable(Of Double))
                spmp_thermalConductivity = value
            End Set
        End Property
        Protected spmp_viscosity As Nullable(Of Double) = Nothing
        Public Property viscosity() As Nullable(Of Double)
            Get
                Return spmp_viscosity
            End Get
            Set(ByVal value As Nullable(Of Double))
                spmp_viscosity = value
            End Set
        End Property
        Protected spmp_kinematic_viscosity As Nullable(Of Double) = Nothing
        Public Property kinematic_viscosity() As Nullable(Of Double)
            Get
                Return spmp_kinematic_viscosity
            End Get
            Set(ByVal value As Nullable(Of Double))
                spmp_kinematic_viscosity = value
            End Set
        End Property
        Protected spmp_volumetric_flow As Nullable(Of Double) = Nothing
        Public Property volumetric_flow() As Nullable(Of Double)
            Get
                Return spmp_volumetric_flow
            End Get
            Set(ByVal value As Nullable(Of Double))
                spmp_volumetric_flow = value
            End Set
        End Property
        Protected spmp_molarenthalpy As Nullable(Of Double) = Nothing
        Public Property molar_enthalpy() As Nullable(Of Double)
            Get
                Return spmp_molarenthalpy
            End Get
            Set(ByVal value As Nullable(Of Double))
                spmp_molarenthalpy = value
            End Set
        End Property
        Protected spmp_molarentropy As Nullable(Of Double) = Nothing
        Public Property molar_entropy() As Nullable(Of Double)
            Get
                Return spmp_molarentropy
            End Get
            Set(ByVal value As Nullable(Of Double))
                spmp_molarentropy = value
            End Set
        End Property
        Protected spmp_molarenthalpyF As Nullable(Of Double) = Nothing
        Public Property molar_enthalpyF() As Nullable(Of Double)
            Get
                Return spmp_molarenthalpyF
            End Get
            Set(ByVal value As Nullable(Of Double))
                spmp_molarenthalpyF = value
            End Set
        End Property
        Protected spmp_molarentropyF As Nullable(Of Double) = Nothing
        Public Property molar_entropyF() As Nullable(Of Double)
            Get
                Return spmp_molarentropyF
            End Get
            Set(ByVal value As Nullable(Of Double))
                spmp_molarentropyF = value
            End Set
        End Property

        Public Sub New()

        End Sub
    End Class

    <System.Serializable()> Public Class TwoPhaseMixtureProperties

        Protected tpmp_kvalue As Nullable(Of Double) = Nothing
        Public Property kvalue() As Nullable(Of Double)
            Get
                Return tpmp_kvalue
            End Get
            Set(ByVal value As Nullable(Of Double))
                tpmp_kvalue = value
            End Set
        End Property
        Protected tpmp_logKvalue As Nullable(Of Double) = Nothing
        Public Property logKvalue() As Nullable(Of Double)
            Get
                Return tpmp_logKvalue
            End Get
            Set(ByVal value As Nullable(Of Double))
                tpmp_logKvalue = value
            End Set
        End Property
        Protected tpmp_surfaceTension As Nullable(Of Double) = Nothing
        Public Property surfaceTension() As Nullable(Of Double)
            Get
                Return tpmp_surfaceTension
            End Get
            Set(ByVal value As Nullable(Of Double))
                tpmp_surfaceTension = value
            End Set
        End Property

    End Class

    <System.Serializable()> Public Class InteractionParameter

        Implements ICloneable, XMLSerializer.Interfaces.ICustomXMLSerialization
        Public Comp1 As String = ""
        Public Comp2 As String = ""
        Public Model As String = ""
        Public DataType As String = ""
        Public Description As String = ""
        Public RegressionFile As String = ""
        Public Parameters As Dictionary(Of String, Object)

        Public Sub New()
            Parameters = New Dictionary(Of String, Object)
        End Sub

        Public Function Clone() As Object Implements System.ICloneable.Clone
            Return ObjectCopy(Me)
        End Function

        Function ObjectCopy(ByVal obj As InteractionParameter) As InteractionParameter

            Dim objMemStream As New MemoryStream(50000)
            Dim objBinaryFormatter As New BinaryFormatter(Nothing, New StreamingContext(StreamingContextStates.Clone))

            objBinaryFormatter.Serialize(objMemStream, obj)

            objMemStream.Seek(0, SeekOrigin.Begin)

            ObjectCopy = objBinaryFormatter.Deserialize(objMemStream)

            objMemStream.Close()

        End Function

        Public Function LoadData(data As System.Collections.Generic.List(Of System.Xml.Linq.XElement)) As Boolean Implements XMLSerializer.Interfaces.ICustomXMLSerialization.LoadData

            XMLSerializer.XMLSerializer.Deserialize(Me, data, True)
            Return True

        End Function

        Public Function SaveData() As System.Collections.Generic.List(Of System.Xml.Linq.XElement) Implements XMLSerializer.Interfaces.ICustomXMLSerialization.SaveData

            Dim elements As List(Of System.Xml.Linq.XElement) = XMLSerializer.XMLSerializer.Serialize(Me, True)
            Dim ci As CultureInfo = CultureInfo.InvariantCulture

            Return elements

        End Function

    End Class

    <System.Serializable()> Public Class ConstantProperties

        Implements ICloneable, XMLSerializer.Interfaces.ICustomXMLSerialization

        Public Name As String = ""
        Public CAS_Number As String = ""
        Public Formula As String = ""
        Public SMILES As String = ""
        Public InChI As String = ""
        Public ChemicalStructure As String = ""
        Public Molar_Weight As Double
        Public Critical_Temperature As Double
        Public Critical_Pressure As Double
        Public Critical_Volume As Double
        Public Critical_Compressibility As Double
        Public Acentric_Factor As Double
        Public Z_Rackett As Double
        Public PR_Volume_Translation_Coefficient As Double
        Public SRK_Volume_Translation_Coefficient As Double
        Public Chao_Seader_Acentricity As Double
        Public Chao_Seader_Solubility_Parameter As Double
        Public Chao_Seader_Liquid_Molar_Volume As Double
        Public IG_Entropy_of_Formation_25C As Double
        Public IG_Enthalpy_of_Formation_25C As Double
        Public IG_Gibbs_Energy_of_Formation_25C As Double
        Public Dipole_Moment As Double
        Public Vapor_Pressure_Constant_A As Double
        Public Vapor_Pressure_Constant_B As Double
        Public Vapor_Pressure_Constant_C As Double
        Public Vapor_Pressure_Constant_D As Double
        Public Vapor_Pressure_Constant_E As Double
        Public Vapor_Pressure_TMIN As Double
        Public Vapor_Pressure_TMAX As Double
        Public Ideal_Gas_Heat_Capacity_Const_A As Double
        Public Ideal_Gas_Heat_Capacity_Const_B As Double
        Public Ideal_Gas_Heat_Capacity_Const_C As Double
        Public Ideal_Gas_Heat_Capacity_Const_D As Double
        Public Ideal_Gas_Heat_Capacity_Const_E As Double
        Public Liquid_Viscosity_Const_A As Double
        Public Liquid_Viscosity_Const_B As Double
        Public Liquid_Viscosity_Const_C As Double
        Public Liquid_Viscosity_Const_D As Double
        Public Liquid_Viscosity_Const_E As Double
        Public Liquid_Density_Const_A As Double
        Public Liquid_Density_Const_B As Double
        Public Liquid_Density_Const_C As Double
        Public Liquid_Density_Const_D As Double
        Public Liquid_Density_Const_E As Double
        Public Liquid_Density_Tmin As Double
        Public Liquid_Density_Tmax As Double
        Public Liquid_Heat_Capacity_Const_A As Double
        Public Liquid_Heat_Capacity_Const_B As Double
        Public Liquid_Heat_Capacity_Const_C As Double
        Public Liquid_Heat_Capacity_Const_D As Double
        Public Liquid_Heat_Capacity_Const_E As Double
        Public Liquid_Heat_Capacity_Tmin As Double
        Public Liquid_Heat_Capacity_Tmax As Double
        Public Liquid_Thermal_Conductivity_Const_A As Double
        Public Liquid_Thermal_Conductivity_Const_B As Double
        Public Liquid_Thermal_Conductivity_Const_C As Double
        Public Liquid_Thermal_Conductivity_Const_D As Double
        Public Liquid_Thermal_Conductivity_Const_E As Double
        Public Liquid_Thermal_Conductivity_Tmin As Double
        Public Liquid_Thermal_Conductivity_Tmax As Double
        Public Vapor_Thermal_Conductivity_Const_A As Double
        Public Vapor_Thermal_Conductivity_Const_B As Double
        Public Vapor_Thermal_Conductivity_Const_C As Double
        Public Vapor_Thermal_Conductivity_Const_D As Double
        Public Vapor_Thermal_Conductivity_Const_E As Double
        Public Vapor_Thermal_Conductivity_Tmin As Double
        Public Vapor_Thermal_Conductivity_Tmax As Double
        Public Vapor_Viscosity_Const_A As Double
        Public Vapor_Viscosity_Const_B As Double
        Public Vapor_Viscosity_Const_C As Double
        Public Vapor_Viscosity_Const_D As Double
        Public Vapor_Viscosity_Const_E As Double
        Public Vapor_Viscosity_Tmin As Double
        Public Vapor_Viscosity_Tmax As Double
        Public Solid_Density_Const_A As Double
        Public Solid_Density_Const_B As Double
        Public Solid_Density_Const_C As Double
        Public Solid_Density_Const_D As Double
        Public Solid_Density_Const_E As Double
        Public Solid_Density_Tmin As Double
        Public Solid_Density_Tmax As Double
        Public Surface_Tension_Const_A As Double
        Public Surface_Tension_Const_B As Double
        Public Surface_Tension_Const_C As Double
        Public Surface_Tension_Const_D As Double
        Public Surface_Tension_Const_E As Double
        Public Surface_Tension_Tmin As Double
        Public Surface_Tension_Tmax As Double
        Public Solid_Heat_Capacity_Const_A As Double
        Public Solid_Heat_Capacity_Const_B As Double
        Public Solid_Heat_Capacity_Const_C As Double
        Public Solid_Heat_Capacity_Const_D As Double
        Public Solid_Heat_Capacity_Const_E As Double
        Public Solid_Heat_Capacity_Tmin As Double
        Public Solid_Heat_Capacity_Tmax As Double
        Public Normal_Boiling_Point As Double
        Public ID As Integer
        Public IsPF As Integer = 0
        Public IsHYPO As Integer = 0
        Public HVap_A As Double
        Public HVap_B As Double
        Public HVap_C As Double
        Public HVap_D As Double
        Public HVap_E As Double
        Public HVap_TMIN As Double
        Public HVap_TMAX As Double
        Public UNIQUAC_R As Double
        Public UNIQUAC_Q As Double

        Public IsFPROPSSupported As Boolean = False
        Public IsCOOLPROPSupported As Boolean = False
        Public IsModified As Boolean = False

        Public UNIFACGroups As UNIFACGroupCollection
        Public MODFACGroups As UNIFACGroupCollection
        Public NISTMODFACGroups As UNIFACGroupCollection
        Public Elements As New ElementCollection

        Public VaporPressureEquation As String = ""
        Public IdealgasCpEquation As String = ""
        Public LiquidViscosityEquation As String = ""
        Public VaporViscosityEquation As String = ""
        Public VaporizationEnthalpyEquation As String = ""
        Public LiquidDensityEquation As String = ""
        Public LiquidHeatCapacityEquation As String = ""
        Public LiquidThermalConductivityEquation As String = ""
        Public VaporThermalConductivityEquation As String = ""
        Public SolidDensityEquation As String = ""
        Public SolidHeatCapacityEquation As String = ""
        Public SurfaceTensionEquation As String = ""

        Public PC_SAFT_sigma As Double = 0.0#
        Public PC_SAFT_epsilon_k As Double = 0.0#
        Public PC_SAFT_m As Double = 0.0#

        Public PF_MM As Nullable(Of Double) = Nothing
        Public NBP As Nullable(Of Double) = Nothing
        Public PF_vA As Nullable(Of Double) = Nothing
        Public PF_vB As Nullable(Of Double) = Nothing
        Public PF_Watson_K As Nullable(Of Double) = Nothing
        Public PF_SG As Nullable(Of Double) = Nothing
        Public PF_v1 As Nullable(Of Double) = Nothing
        Public PF_Tv1 As Nullable(Of Double) = Nothing
        Public PF_v2 As Nullable(Of Double) = Nothing
        Public PF_Tv2 As Nullable(Of Double) = Nothing

        'Databases: 'DWSIM', 'CheResources', 'ChemSep' OR 'User'
        'User databases are XML-serialized versions of this base class, and they may include hypos and pseudos.
        Public OriginalDB As String = "DWSIM"
        Public CurrentDB As String = "DWSIM"
        Public CompCreatorStudyFile As String = ""

        'COSMO-SAC's database equivalent name
        Public COSMODBName = ""

        'Electrolyte-related properties
        Public IsIon As Boolean = False
        Public IsSalt As Boolean = False
        Public IsHydratedSalt As Boolean = False
        Public HydrationNumber As Double = 0.0#
        Public Charge As Integer = 0
        Public PositiveIon As String = ""
        Public NegativeIon As String = ""
        Public PositiveIonStoichCoeff As Integer = 0
        Public NegativeIonStoichCoeff As Integer = 0
        Public StoichSum As Integer = 0
        Public Electrolyte_DelGF As Double = 0.0#
        Public Electrolyte_DelHF As Double = 0.0#
        Public Electrolyte_Cp0 As Double = 0.0#
        Public TemperatureOfFusion As Double = 0.0#
        Public EnthalpyOfFusionAtTf As Double = 0.0#
        Public SolidTs As Double = 0.0#
        Public SolidDensityAtTs As Double = 0.0#
        Public StandardStateMolarVolume As Double = 0.0#
        Public MolarVolume_v2i As Double = 0.0#
        Public MolarVolume_v3i As Double = 0.0#
        Public MolarVolume_k1i As Double = 0.0#
        Public MolarVolume_k2i As Double = 0.0#
        Public MolarVolume_k3i As Double = 0.0#
        Public Ion_CpAq_a As Double = 0.0#
        Public Ion_CpAq_b As Double = 0.0#
        Public Ion_CpAq_c As Double = 0.0#

        'Black-Oil Properties

        Public IsBlackOil As Boolean = False

        'gas specific gravity
        Public BO_SGG As Double = 0.0#
        'oil specific gravity
        Public BO_SGO As Double = 0.0#
        'gas-oil ratio (m3/m3 STD)
        Public BO_GOR As Double = 0.0#
        'basic sediments and water (%)
        Public BO_BSW As Double = 0.0#
        'oil viscosity at T1 (m2/s)
        Public BO_OilVisc1 As Double = 0.0#
        'oil viscosity T1 (K)
        Public BO_OilViscTemp1 As Double = 0.0#
        'oil viscosity at T2 (m2/s)
        Public BO_OilVisc2 As Double = 0.0#
        'oil viscosity T2 (K)
        Public BO_OilViscTemp2 As Double = 0.0#
        'oil parafins percentage
        Public BO_PNA_P As Double = 0.0#
        'oil napthenics percentage
        Public BO_PNA_N As Double = 0.0#
        'oil aromatics percentage
        Public BO_PNA_A As Double = 0.0#

        Public Comments As String = ""

        'the following properties are no longer used but kept for compatibility reasons
        <XmlIgnore()> Public UNIFAC_Ri As Double
        <XmlIgnore()> Public UNIFAC_Qi As Double
        <XmlIgnore()> Public UNIFAC_01_CH4 As Integer
        <XmlIgnore()> Public UNIFAC_02_CH3 As Integer
        <XmlIgnore()> Public UNIFAC_03_CH2 As Integer
        <XmlIgnore()> Public UNIFAC_04_CH As Integer
        <XmlIgnore()> Public UNIFAC_05_H2O As Integer
        <XmlIgnore()> Public UNIFAC_06_H2S As Integer
        <XmlIgnore()> Public UNIFAC_07_CO2 As Integer
        <XmlIgnore()> Public UNIFAC_08_N2 As Integer
        <XmlIgnore()> Public UNIFAC_09_O2 As Integer
        <XmlIgnore()> Public UNIFAC_10_OH As Integer
        <XmlIgnore()> Public UNIFAC_11_ACH As Integer
        <XmlIgnore()> Public UNIFAC_12_ACCH2 As Integer
        <XmlIgnore()> Public UNIFAC_13_ACCH3 As Integer
        <XmlIgnore()> Public UNIFAC_14_ACOH As Integer
        <XmlIgnore()> Public UNIFAC_15_CH3CO As Integer
        <XmlIgnore()> Public UNIFAC_16_CH2CO As Integer
        <XmlIgnore()> Public Element_C As Integer = 0
        <XmlIgnore()> Public Element_H As Integer = 0
        <XmlIgnore()> Public Element_O As Integer = 0
        <XmlIgnore()> Public Element_N As Integer = 0
        <XmlIgnore()> Public Element_S As Integer = 0

        Public Sub New()
            UNIFACGroups = New UNIFACGroupCollection
            MODFACGroups = New UNIFACGroupCollection
            NISTMODFACGroups = New UNIFACGroupCollection
        End Sub

        Public Function Clone() As Object Implements System.ICloneable.Clone

            Return ObjectCopy(Me)

        End Function

        Function ObjectCopy(ByVal obj As ConstantProperties) As ConstantProperties

            Dim objMemStream As New MemoryStream(50000)
            Dim objBinaryFormatter As New BinaryFormatter(Nothing, New StreamingContext(StreamingContextStates.Clone))

            objBinaryFormatter.Serialize(objMemStream, obj)

            objMemStream.Seek(0, SeekOrigin.Begin)

            ObjectCopy = objBinaryFormatter.Deserialize(objMemStream)

            objMemStream.Close()

        End Function

        Public Function LoadData(data As System.Collections.Generic.List(Of System.Xml.Linq.XElement)) As Boolean Implements XMLSerializer.Interfaces.ICustomXMLSerialization.LoadData

            XMLSerializer.XMLSerializer.Deserialize(Me, data, True)

            Dim unif As New PropertyPackages.Auxiliary.Unifac
            Dim modf As New PropertyPackages.Auxiliary.Modfac

            For Each xel2 As XElement In (From xel As XElement In data Select xel Where xel.Name = "UNIFACGroups").Elements
                If xel2.@Name Is Nothing Then
                    Me.UNIFACGroups.Collection.Add(xel2.@GroupID.ToString, xel2.@Value)
                Else
                    Dim id As Integer = unif.Group2ID(xel2.@Name)
                    Me.UNIFACGroups.Collection.Add(id.ToString, xel2.@Value)
                End If
            Next

            For Each xel2 As XElement In (From xel As XElement In data Select xel Where xel.Name = "MODFACGroups").Elements
                If xel2.@Name Is Nothing Then
                    Me.MODFACGroups.Collection.Add(xel2.@GroupID.ToString, xel2.@Value)
                Else
                    Dim id As Integer = modf.Group2ID(xel2.@Name)
                    Me.MODFACGroups.Collection.Add(id.ToString, xel2.@Value)
                End If
            Next

            For Each xel2 As XElement In (From xel As XElement In data Select xel Where xel.Name = "NISTMODFACGroups").Elements
                Me.NISTMODFACGroups.Collection.Add(xel2.@GroupID.ToString, xel2.@Value)
            Next

            For Each xel2 As XElement In (From xel As XElement In data Select xel Where xel.Name = "Elements").Elements
                Me.Elements.Collection.Add(xel2.@Name, xel2.@Value)
            Next

            unif = Nothing
            modf = Nothing

            Return True

        End Function

        Public Function SaveData() As System.Collections.Generic.List(Of System.Xml.Linq.XElement) Implements XMLSerializer.Interfaces.ICustomXMLSerialization.SaveData

            Dim xelements As List(Of System.Xml.Linq.XElement) = XMLSerializer.XMLSerializer.Serialize(Me, True)
            Dim ci As CultureInfo = CultureInfo.InvariantCulture

            With xelements

                .Add(New XElement("UNIFACGroups"))

                If Not UNIFACGroups Is Nothing Then

                    For Each key As String In UNIFACGroups.Collection.Keys
                        .Item(xelements.Count - 1).Add(New XElement("Item", New XAttribute("GroupID", key), New XAttribute("Value", UNIFACGroups.Collection(key.ToString))))
                    Next

                End If

                .Add(New XElement("MODFACGroups"))

                If Not MODFACGroups Is Nothing Then

                    For Each key As String In MODFACGroups.Collection.Keys
                        .Item(xelements.Count - 1).Add(New XElement("Item", New XAttribute("GroupID", key), New XAttribute("Value", MODFACGroups.Collection(key.ToString))))
                    Next

                End If

                .Add(New XElement("NISTMODFACGroups"))

                If Not MODFACGroups Is Nothing Then

                    For Each key As String In NISTMODFACGroups.Collection.Keys
                        .Item(xelements.Count - 1).Add(New XElement("Item", New XAttribute("GroupID", key), New XAttribute("Value", NISTMODFACGroups.Collection(key.ToString))))
                    Next

                End If

                .Add(New XElement("Elements"))

                If Not Me.Elements Is Nothing Then

                    For Each key As String In Me.Elements.Collection.Keys
                        .Item(xelements.Count - 1).Add(New XElement("Item", New XAttribute("Name", key), New XAttribute("Value", Me.Elements.Collection(key))))
                    Next

                End If

            End With

            Return xelements

        End Function

    End Class

    <System.Serializable()> Public Class ConstantPropertiesCollection
        Public Collection() As ConstantProperties
    End Class

    <System.Serializable()> Public Class UNIFACGroupCollection
        Public Collection As System.Collections.SortedList
        Sub New()
            Collection = New System.Collections.SortedList
        End Sub
    End Class

    <System.Serializable()> Public Class ElementCollection
        Public Collection As New System.Collections.SortedList
        Sub New()
            Collection = New System.Collections.SortedList
        End Sub
    End Class

#End Region

End Namespace
