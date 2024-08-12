'    Basic Thermodynamic Classes for DWSIM
'    Copyright 2008-2022 Daniel Wagner O. de Medeiros
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
Imports DWSIM.Interfaces.Enums
Imports DWSIM.Interfaces
Imports System.Dynamic
Imports System.Text.RegularExpressions
Imports Mages.Core.EngineExtensions
Imports Mages.Core.FunctionExtensions
Imports DWSIM.SharedClasses

Namespace BaseClasses

    <System.Serializable()> Public Class Compound

        Implements Interfaces.ICustomXMLSerialization, CapeOpen.ICapeIdentification, Interfaces.ICompound

        Public Sub New(ByVal name As String, ByVal description As String)

            Me.Name = name
            Me.ComponentName = name
            Me.ComponentDescription = description

        End Sub

        Public Overrides Function ToString() As String

            Return Name + String.Format(": xm = {0}, xw = {1}, M = {2} mol/s, W = {3} kg/s", MoleFraction.GetValueOrDefault, MassFraction.GetValueOrDefault, MolarFlow.GetValueOrDefault, MassFlow.GetValueOrDefault)

        End Function

        Public Function LoadData(data As System.Collections.Generic.List(Of System.Xml.Linq.XElement)) As Boolean Implements Interfaces.ICustomXMLSerialization.LoadData

            XMLSerializer.XMLSerializer.Deserialize(Me, data)

            ExtraProperties = New ExpandoObject

            Dim xel_d = (From xel2 As XElement In data Select xel2 Where xel2.Name = "DynamicProperties")

            If Not xel_d Is Nothing Then
                Dim dataDyn As List(Of XElement) = xel_d.Elements.ToList
                For Each xel As XElement In dataDyn
                    Try
                        Dim propname = xel.Element("Name").Value
                        Dim proptype = xel.Element("PropertyType").Value
                        Dim ptype As Type = Type.GetType(proptype)
                        Dim propval = Newtonsoft.Json.JsonConvert.DeserializeObject(xel.Element("Data").Value, ptype)
                        DirectCast(ExtraProperties, IDictionary(Of String, Object))(propname) = propval
                    Catch ex As Exception
                    End Try
                Next
            End If

            Return True

        End Function

        Public Function SaveData() As System.Collections.Generic.List(Of System.Xml.Linq.XElement) Implements Interfaces.ICustomXMLSerialization.SaveData

            Dim elements As System.Collections.Generic.List(Of System.Xml.Linq.XElement) = XMLSerializer.XMLSerializer.Serialize(Me)

            elements.Add(New XElement("DynamicProperties"))
            Dim extraprops = DirectCast(ExtraProperties, IDictionary(Of String, Object))
            For Each item In extraprops
                Try
                    elements.Item(elements.Count - 1).Add(New XElement("Property", {New XElement("Name", item.Key),
                                                                           New XElement("PropertyType", item.Value.GetType.ToString),
                                                                           New XElement("Data", Newtonsoft.Json.JsonConvert.SerializeObject(item.Value))}))
                Catch ex As Exception
                End Try
            Next

            Return elements

        End Function

        Public Property ComponentDescription As String = "" Implements CapeOpen.ICapeIdentification.ComponentDescription

        Public Property ComponentName As String = "" Implements CapeOpen.ICapeIdentification.ComponentName

        Public Property ActivityCoeff As Double? = 0.0# Implements Interfaces.ICompound.ActivityCoeff

        Public Property PetroleumFraction As Boolean Implements Interfaces.ICompound.PetroleumFraction

        Public Property MassFraction As Double? = 0.0# Implements Interfaces.ICompound.MassFraction

        Public Property MoleFraction As Double? = 0.0# Implements Interfaces.ICompound.MoleFraction

        Public Property Molarity As Double? = 0.0# Implements Interfaces.ICompound.Molarity

        Public Property Molality As Double? = 0.0# Implements Interfaces.ICompound.Molality

        Public Property FugacityCoeff As Double? = 0.0# Implements Interfaces.ICompound.FugacityCoeff

        Public Property Kvalue As Double = 0.0# Implements Interfaces.ICompound.Kvalue

        Public Property lnKvalue As Double = 0.0# Implements Interfaces.ICompound.lnKvalue

        Public Property MassFlow As Double? = 0.0# Implements Interfaces.ICompound.MassFlow

        Public Property MolarFlow As Double? = 0.0# Implements Interfaces.ICompound.MolarFlow

        Public Property Name As String = "" Implements Interfaces.ICompound.Name

        Public Property PartialPressure As Double? = 0.0# Implements Interfaces.ICompound.PartialPressure

        Public Property PartialVolume As Double? = 0.0# Implements Interfaces.ICompound.PartialVolume

        Public Property VolumetricFlow As Double? = 0.0# Implements Interfaces.ICompound.VolumetricFlow

        Public Property VolumetricFraction As Double? = 0.0# Implements Interfaces.ICompound.VolumetricFraction

        <XmlIgnore> Public Property ConstantProperties As Interfaces.ICompoundConstantProperties = New ConstantProperties Implements Interfaces.ICompound.ConstantProperties

        Public Property DiffusionCoefficient As Double? Implements Interfaces.ICompound.DiffusionCoefficient

        Public Property ExtraProperties As New ExpandoObject Implements ICompound.ExtraProperties

        Public Property EnthalpyF_Dmol As Double? = 0.0 Implements Interfaces.ICompound.EnthalpyF_Dmol

        Public Property EntropyF_Dmol As Double? = 0.0 Implements Interfaces.ICompound.EntropyF_Dmol

    End Class

    <System.Serializable()> Public Class Phase

        Implements Interfaces.ICustomXMLSerialization

        Implements Interfaces.IPhase

        Public Property Properties As New PhaseProperties

        Public Sub New(ByVal name As String, ByVal description As String)

            Me.Name = name
            Me.ComponentName = name
            Me.ComponentDescription = description

        End Sub

        Public Overrides Function ToString() As String
            If Name <> "" Then
                Return Name
            Else
                Return MyBase.ToString()
            End If
        End Function

        Public Function LoadData(data As System.Collections.Generic.List(Of System.Xml.Linq.XElement)) As Boolean Implements Interfaces.ICustomXMLSerialization.LoadData

            XMLSerializer.XMLSerializer.Deserialize(Me, data)

            Dim datac As List(Of XElement) = (From xel As XElement In data Select xel Where xel.Name = "Compounds").Elements.ToList

            For Each xel As XElement In datac
                Dim s As New Compound("", "")
                s.LoadData(xel.Elements.ToList)
                Me.Compounds.Add(s.Name, s)
            Next

            If (From xel As XElement In data Select xel Where xel.Name = "SPMProperties").Count > 0 Then
                ' DWSIM 3
                XMLSerializer.XMLSerializer.Deserialize(Me.Properties, (From xel As XElement In data Select xel Where xel.Name = "SPMProperties").Elements.ToList)
            Else
                ' DWSIM 4
                XMLSerializer.XMLSerializer.Deserialize(Me.Properties, (From xel As XElement In data Select xel Where xel.Name = "Properties").Elements.ToList)
            End If

        End Function

        Public Function SaveData() As System.Collections.Generic.List(Of System.Xml.Linq.XElement) Implements Interfaces.ICustomXMLSerialization.SaveData

            Dim elements As New List(Of System.Xml.Linq.XElement)
            Dim ci As CultureInfo = CultureInfo.InvariantCulture

            With elements

                .Add(New XElement("Compounds"))

                For Each kvp As KeyValuePair(Of String, Interfaces.ICompound) In Me.Compounds
                    elements(elements.Count - 1).Add(New XElement("Compound", DirectCast(kvp.Value, Interfaces.ICustomXMLSerialization).SaveData().ToArray()))
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

            End With

            Return elements

        End Function

        Public Property ComponentDescription As String = "" Implements Interfaces.IPhase.ComponentDescription

        Public Property ComponentName As String = "" Implements Interfaces.IPhase.ComponentName

        Public Property Compounds As Dictionary(Of String, Interfaces.ICompound) = New Dictionary(Of String, Interfaces.ICompound) Implements Interfaces.IPhase.Compounds

        Public Property Name As String = "" Implements Interfaces.IPhase.Name

        Public ReadOnly Property Properties1 As Interfaces.IPhaseProperties Implements Interfaces.IPhase.Properties
            Get
                Return Properties
            End Get
        End Property

    End Class


    <System.Serializable()> <XmlRoot(ElementName:="Reaction")>
    Public Class Reaction

        Implements ICloneable, Interfaces.ICustomXMLSerialization

        Implements Interfaces.IReaction

        Public _Components As Dictionary(Of String, Interfaces.IReactionStoichBase)

        <XmlIgnore> <NonSerialized> Public ExpContext As New Ciloci.Flee.ExpressionContext
        <XmlIgnore> <NonSerialized> Public Expr As Ciloci.Flee.IGenericExpression(Of Double)

        <XmlIgnore> <NonSerialized> Private MEngine As Mages.Core.Engine
        <XmlIgnore> <NonSerialized> Private KFunc As Mages.Core.Function
        <XmlIgnore> <NonSerialized> Private _ExpressionChanged As Boolean = True

#Region "    DWSIM Specific"

        Public Function EvaluateK(ByVal T As Double, ByVal pp As PropertyPackages.PropertyPackage) As Double

            'equilibrium constant calculation

            Select Case KExprType

                Case KOpt.Constant

                    Return ConstantKeqValue

                Case KOpt.Expression

                    If MEngine Is Nothing Then
                        MEngine = New Mages.Core.Engine()
                        KFunc = MEngine.Interpret("(T) => " + Expression)
                    End If
                    If _ExpressionChanged Then
                        _ExpressionChanged = False
                        KFunc = MEngine.Interpret("(T) => " + Expression)
                    End If

                    Return Math.Exp(KFunc.Call(Of Double)(T))

                Case KOpt.Gibbs

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

        End Function

        'Initializers

        Public Sub New()
            Me._Components = New Dictionary(Of String, Interfaces.IReactionStoichBase)
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

        Public Overrides Function ToString() As String
            If Name <> "" Then
                Return Name
            Else
                Return MyBase.ToString()
            End If
        End Function

        Public Function LoadData(data As System.Collections.Generic.List(Of System.Xml.Linq.XElement)) As Boolean Implements Interfaces.ICustomXMLSerialization.LoadData

            XMLSerializer.XMLSerializer.Deserialize(Me, data)

            Dim ci As CultureInfo = CultureInfo.InvariantCulture
            For Each xel2 As XElement In (From xel As XElement In data Select xel Where xel.Name = "Compounds").Elements
                Me._Components.Add(xel2.@Name, New ReactionStoichBase(xel2.@Name, Double.Parse(xel2.@StoichCoeff, ci), xel2.@IsBaseReactant, Double.Parse(xel2.@DirectOrder, ci), Double.Parse(xel2.@ReverseOrder, ci)))
            Next

        End Function

        Public Function SaveData() As System.Collections.Generic.List(Of System.Xml.Linq.XElement) Implements Interfaces.ICustomXMLSerialization.SaveData

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

        Public Property BaseReactant As String = "" Implements Interfaces.IReaction.BaseReactant

        Public ReadOnly Property Components As Dictionary(Of String, Interfaces.IReactionStoichBase) Implements Interfaces.IReaction.Components
            Get
                'Return _Components.ToDictionary(Of String, Interfaces.IReactionStoichBase)(Function(k) k.Key, Function(k) k.Value)
                Return _Components
            End Get
        End Property

        Public Property Description As String = "" Implements Interfaces.IReaction.Description

        Public Property Equation As String = "" Implements Interfaces.IReaction.Equation

        Public Property ID As String = "" Implements Interfaces.IReaction.ID

        Public Property Name As String = "" Implements Interfaces.IReaction.Name

        Public Property ReactionBasis As Interfaces.Enums.ReactionBasis = ReactionBasis.Fugacity Implements Interfaces.IReaction.ReactionBasis

        Public Property ReactionHeat As Double Implements Interfaces.IReaction.ReactionHeat

        Public Property ReactionHeatCO As Double Implements Interfaces.IReaction.ReactionHeatCO

        Public Property ReactionPhase As Interfaces.Enums.ReactionPhase Implements Interfaces.IReaction.ReactionPhase

        Public Property ReactionType As Interfaces.Enums.ReactionType Implements Interfaces.IReaction.ReactionType

        Public Property StoichBalance As Double Implements Interfaces.IReaction.StoichBalance

        Public Property A_Forward As Double Implements Interfaces.IReaction.A_Forward

        Public Property A_Reverse As Double Implements Interfaces.IReaction.A_Reverse

        Public Property Approach As Double Implements Interfaces.IReaction.Approach

        Public Property ConcUnit As String = "" Implements Interfaces.IReaction.ConcUnit

        Public Property ConstantKeqValue As Double Implements Interfaces.IReaction.ConstantKeqValue

        Public Property E_Forward As Double Implements Interfaces.IReaction.E_Forward

        Public Property E_Reverse As Double Implements Interfaces.IReaction.E_Reverse

        Private _Expression As String = ""

        Public Property Expression As String Implements Interfaces.IReaction.Expression
            Get
                Return _Expression
            End Get
            Set(value As String)
                _Expression = value
                _ExpressionChanged = True
            End Set
        End Property
        Public Property KExprType As Interfaces.Enums.KOpt Implements Interfaces.IReaction.KExprType

        Public Property Kvalue As Double Implements Interfaces.IReaction.Kvalue

        Public Property Rate As Double Implements Interfaces.IReaction.Rate

        Public Property RateEquationDenominator As String = "" Implements Interfaces.IReaction.RateEquationDenominator

        Public Property RateEquationNumerator As String = "" Implements Interfaces.IReaction.RateEquationNumerator

        Public Property ReactionGibbsEnergy As Double Implements Interfaces.IReaction.ReactionGibbsEnergy

        Public Property Tmax As Double = 2000.0 Implements Interfaces.IReaction.Tmax

        Public Property Tmin As Double = 0.0 Implements Interfaces.IReaction.Tmin

        Public Property VelUnit As String = "" Implements Interfaces.IReaction.VelUnit

        Public Property ReactionKinFwdType As ReactionKineticType = ReactionKineticType.Arrhenius Implements IReaction.ReactionKinFwdType

        Public Property ReactionKinRevType As ReactionKineticType = ReactionKineticType.Arrhenius Implements IReaction.ReactionKinRevType

        Public Property ReactionKinFwdExpression As String = "" Implements IReaction.ReactionKinFwdExpression

        Public Property ReactionKinRevExpression As String = "" Implements IReaction.ReactionKinRevExpression

        Public Property E_Forward_Unit As String = "J/mol" Implements IReaction.E_Forward_Unit

        Public Property E_Reverse_Unit As String = "J/mol" Implements IReaction.E_Reverse_Unit

        Public Property ReactionKinetics As ReactionKinetics = ReactionKinetics.Expression Implements IReaction.ReactionKinetics

        Public Property ScriptTitle As String = "" Implements IReaction.ScriptTitle

        Public Property EquilibriumReactionBasisUnits As String = "Pa" Implements IReaction.EquilibriumReactionBasisUnits

        Public Function EvaluateK1(T As Double, PP As Interfaces.IPropertyPackage) As Double Implements Interfaces.IReaction.EvaluateK
            Return EvaluateK(T, PP)
        End Function

        Public Function GetPropertyList() As String() Implements IReaction.GetPropertyList

            Return New String() {"Kinetic_A_Forward", "Kinetic_A_Reverse", "Kinetic_E_Forward", "Kinetic_E_Reverse", "Tmin", "Tmax", "Equilibrium_ConstantKeqValue", "Conversion_Value"}

        End Function

        Public Function GetPropertyValue(prop As String) As Double Implements IReaction.GetPropertyValue

            Select Case prop
                Case "Kinetic_A_Forward"
                    Return A_Forward
                Case "Kinetic_A_Reverse"
                    Return A_Reverse
                Case "Kinetic_E_Forward"
                    Return E_Forward
                Case "Kinetic_E_Reverse"
                    Return E_Reverse
                Case "Tmin"
                    Return Tmin
                Case "Tmax"
                    Return Tmax
                Case "Equilibrium_ConstantKeqValue"
                    Return ConstantKeqValue
                Case "Conversion_Value"
                    If Expression.IsValidDoubleExpression() Then Return Expression.ToDoubleFromInvariant Else Return 0.0
                Case Else
                    Return 0.0
            End Select

        End Function

        Public Sub SetPropertyValue(prop As String, value As Double) Implements IReaction.SetPropertyValue

            Select Case prop
                Case "Kinetic_A_Forward"
                    A_Forward = value
                Case "Kinetic_A_Reverse"
                    A_Reverse = value
                Case "Kinetic_E_Forward"
                    E_Forward = value
                Case "Kinetic_E_Reverse"
                    E_Reverse = value
                Case "Tmin"
                    Tmin = value
                Case "Tmax"
                    Tmax = value
                Case "Equilibrium_ConstantKeqValue"
                    ConstantKeqValue = value
                Case "Conversion_Value"
                    If value.IsValidDouble Then Expression = value.ToString(Globalization.CultureInfo.InvariantCulture)
            End Select

        End Sub

    End Class

    <Runtime.InteropServices.ComVisible(True)> <System.Serializable()> Public Class ReactionSet

        Implements ICloneable, Interfaces.ICustomXMLSerialization

        'CAPE-OPEN Reaction Package Interfaces
        Implements CapeOpen.ICapeIdentification
        Implements CapeOpen.ICapeUtilities, CapeOpen.ICapeCollection, CapeOpen.ICapeReactionsRoutine, CapeOpen.ICapeReactionChemistry
        Implements CapeOpen.ICapeThermoContext, CapeOpen.ICapeKineticReactionContext, CapeOpen.ICapeReactionProperties
        Implements CapeOpen.ICapeThermoMaterialContext

        Implements Interfaces.IReactionSet

        Protected m_reactionset As Dictionary(Of String, Interfaces.IReactionSetBase)

#Region "    DWSIM Specific"

        Public Function GetIDbyName(ByVal reactname As String)
            Dim ID As String = ""
            For Each r As Reaction In Me.m_pme.Reactions.Values
                If r.Name = reactname Then
                    ID = r.ID
                    Exit For
                End If
            Next
            Return ID
        End Function

        Public ReadOnly Property Reactions() As Dictionary(Of String, Interfaces.IReactionSetBase) Implements Interfaces.IReactionSet.Reactions
            Get
                Return m_reactionset
            End Get
        End Property

        Public Property ID() As String = "" Implements Interfaces.IReactionSet.ID

        Public Property Name() As String = "" Implements Interfaces.IReactionSet.Name

        Public Property Description() As String = "" Implements Interfaces.IReactionSet.Description

        Sub New()
            MyBase.New()
            Me.m_reactionset = New Dictionary(Of String, Interfaces.IReactionSetBase)
        End Sub

        Sub New(ByVal id As String, ByVal name As String, ByVal description As String)
            Me.New()
            Me.ID = id
            Me.Name = name
            Me.Description = description
        End Sub

        Public Overrides Function ToString() As String
            If Name <> "" Then
                Return Name
            Else
                Return MyBase.ToString()
            End If
        End Function

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

        Protected m_params As New CapeOpen.ParameterCollection
        Protected m_str As Streams.MaterialStream
        <System.NonSerialized()> Protected m_pme As Interfaces.IFlowsheet
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
            Return Me.m_pme.Reactions(GetIDbyName(reacId)).BaseReactant
        End Function

        ''' <summary>
        ''' Gets the number of compounds occurring in a particular reaction within a Reactions Package.
        ''' </summary>
        ''' <param name="reacID">The reaction identifier</param>
        ''' <returns></returns>
        ''' <remarks></remarks>
        Public Function GetNumberOfReactionCompounds(ByVal reacID As String) As Integer Implements CapeOpen.ICapeReactionChemistry.GetNumberOfReactionCompounds
            Return Me.m_pme.Reactions(GetIDbyName(reacID)).Components.Count
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
            Dim comps = m_pme.SelectedCompounds.Values.ToList()
            Dim n As Integer = comps.Count - 1
            For Each c As ReactionStoichBase In Me.m_pme.Reactions(GetIDbyName(reacId)).Components.Values
                With Me.m_pme.SelectedCompounds(c.CompName)
                    For i = 0 To n
                        If comps(i).CAS_Number = .CAS_Number Then
                            narr.Add(comps(i).Name)
                            carr.Add(comps(i).CAS_Number)
                            charr.Add(Convert.ToDouble(comps(i).Charge))
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
            Select Case Me.m_pme.Reactions(GetIDbyName(reacId)).ReactionBasis
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
                Case ReactionBasis.PartialPress
                    Return "partialpressure"
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
            Select Case Me.m_pme.Reactions(GetIDbyName(reacId)).ReactionPhase
                Case PhaseName.Vapor
                    Return "Vapor"
                Case PhaseName.Liquid
                    Return "Liquid"
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
                narr.Add(Me.m_pme.Reactions(r.ReactionID).Name)
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
            Select Case Me.m_pme.Reactions(GetIDbyName(reacID)).ReactionType
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
            For Each c As ReactionStoichBase In Me.m_pme.Reactions(GetIDbyName(reacId)).Components.Values
                narr.Add(c.StoichCoeff)
            Next
            Dim sc(narr.Count - 1) As Double
            Array.Copy(narr.ToArray, sc, narr.Count)
            Return sc
        End Function

        Public Sub Edit() Implements CapeOpen.ICapeUtilities.Edit
            'Dim rm As New FormReacManager
            'rm.Show()
            Throw New NotImplementedException
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
                Dim ro As Reaction = Me.m_pme.Reactions(GetIDbyName(rid))
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
                Dim ro As Reaction = Me.m_pme.Reactions(GetIDbyName(rid))
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

                Dim ro As Reaction = Me.m_pme.Reactions(GetIDbyName(rid))

                With ro
                    For Each p As String In props
                        Select Case p.ToLower
                            Case "reactionrate"

                                Dim ims As Streams.MaterialStream = Me.m_str
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
                                    Case KOpt.Constant
                                        .Kvalue = .ConstantKeqValue
                                    Case KOpt.Expression
                                        If .ExpContext Is Nothing Then
                                            .ExpContext = New Ciloci.Flee.ExpressionContext
                                            .ExpContext.Options.ParseCulture = Globalization.CultureInfo.InvariantCulture
                                            With .ExpContext
                                                .Imports.AddType(GetType(System.Math))
                                            End With
                                        End If
                                        .ExpContext.Options.ParseCulture = Globalization.CultureInfo.InvariantCulture
                                        .ExpContext.Variables.Clear()
                                        .ExpContext.Variables.Add("T", T)
                                        .Expr = .ExpContext.CompileGeneric(Of Double)(.Expression)
                                        .Kvalue = Math.Exp(.Expr.Evaluate)
                                    Case KOpt.Gibbs
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
                Dim myms As Interfaces.IMaterialStream = Me.m_pme.SimulationObjects(id)
                'proceed with copy
                Me.m_str = myms
            End If
        End Sub

        Public Property ComponentDescription() As String = "" Implements CapeOpen.ICapeIdentification.ComponentDescription

        Public Property ComponentName() As String = "" Implements CapeOpen.ICapeIdentification.ComponentName

        Public Sub SetMaterial1(ByVal material As Object) Implements CapeOpen.ICapeThermoMaterialContext.SetMaterial
            If Not System.Runtime.InteropServices.Marshal.IsComObject(material) Then
                Me.m_str = material
            Else
                'get ID
                Dim id As String = CType(material, CapeOpen.ICapeIdentification).ComponentDescription
                Dim myms As Interfaces.IMaterialStream = Me.m_pme.SimulationObjects(id)
                'proceed with copy
                Me.m_str = myms
            End If

        End Sub

        Public Sub UnsetMaterial() Implements CapeOpen.ICapeThermoMaterialContext.UnsetMaterial
            Me.m_str = Nothing
        End Sub

#End Region

        Public Function LoadData(data As System.Collections.Generic.List(Of System.Xml.Linq.XElement)) As Boolean Implements Interfaces.ICustomXMLSerialization.LoadData

            Me.ID = (From xel As XElement In data Select xel Where xel.Name = "ID").SingleOrDefault.Value
            Me.Name = (From xel As XElement In data Select xel Where xel.Name = "Name").SingleOrDefault.Value
            Me.Description = (From xel As XElement In data Select xel Where xel.Name = "Description").SingleOrDefault.Value

            For Each xel2 As XElement In (From xel As XElement In data Select xel Where xel.Name = "Reactions").Elements
                Me.m_reactionset.Add(xel2.@Key, New ReactionSetBase(xel2.@ReactionID, xel2.@Rank, xel2.@IsActive))
            Next

        End Function

        Public Function SaveData() As System.Collections.Generic.List(Of System.Xml.Linq.XElement) Implements Interfaces.ICustomXMLSerialization.SaveData

            Dim elements As New List(Of System.Xml.Linq.XElement)
            Dim ci As CultureInfo = CultureInfo.InvariantCulture

            With elements

                .Add(New XElement("ID", ID))
                .Add(New XElement("Name", Name))
                .Add(New XElement("Description", Description))

                .Add(New XElement("Reactions"))

                For Each kvp As KeyValuePair(Of String, Interfaces.IReactionSetBase) In Reactions
                    .Item(.Count - 1).Add(New XElement("Reaction", New XAttribute("Key", kvp.Key),
                                                                    New XAttribute("ReactionID", kvp.Value.ReactionID),
                                                                    New XAttribute("Rank", kvp.Value.Rank),
                                                                    New XAttribute("IsActive", kvp.Value.IsActive)))
                Next

            End With

            Return elements

        End Function

        Public Sub SetReactionRank(reactionID As String, rank As Integer) Implements IReactionSet.SetReactionRank
            Reactions(reactionID).Rank = rank
        End Sub

        Public Sub EnableReaction(reactionID As String) Implements IReactionSet.EnableReaction
            Reactions(reactionID).IsActive = True
        End Sub

        Public Sub DisableReaction(reactionID As String) Implements IReactionSet.DisableReaction
            Reactions(reactionID).IsActive = False
        End Sub

    End Class

    <System.Serializable()> Public Class ReactionSetBase

        Implements Interfaces.IReactionSetBase

        Sub New()

        End Sub

        Sub New(ByVal id As String, ByVal rank As Integer, ByVal isactive As Boolean)
            Me.IsActive = isactive
            Me.Rank = rank
            Me.ReactionID = id
        End Sub

        Public Property IsActive As Boolean Implements Interfaces.IReactionSetBase.IsActive

        Public Property Rank As Integer Implements Interfaces.IReactionSetBase.Rank

        Public Property ReactionID As String = "" Implements Interfaces.IReactionSetBase.ReactionID

    End Class

    <System.Serializable()> Public Class ReactionStoichBase

        Implements Interfaces.IReactionStoichBase

        Public Sub New(ByVal name As String, ByVal stoichcoeff As Double, ByVal isbasereactant As Boolean, ByVal directorder As Double, ByVal reversorder As Double)
            Me.CompName = name
            Me.StoichCoeff = stoichcoeff
            Me.IsBaseReactant = isbasereactant
            Me.DirectOrder = directorder
            Me.ReverseOrder = reversorder
        End Sub

        Public Property CompName As String = "" Implements Interfaces.IReactionStoichBase.CompName

        Public Property DirectOrder As Double Implements Interfaces.IReactionStoichBase.DirectOrder

        Public Property IsBaseReactant As Boolean Implements Interfaces.IReactionStoichBase.IsBaseReactant

        Public Property ReverseOrder As Double Implements Interfaces.IReactionStoichBase.ReverseOrder

        Public Property StoichCoeff As Double Implements Interfaces.IReactionStoichBase.StoichCoeff

    End Class

    <System.Serializable()> Public Class ReactionsCollection
        Public Collection() As Reaction
        Sub New()

        End Sub
    End Class

#Region "Subclasses"

    <System.Serializable()> Public Class PhaseProperties

        Implements Interfaces.IPhaseProperties

        Public Sub New()

        End Sub

        Public Property activity As Double? Implements Interfaces.IPhaseProperties.activity

        Public Property activityCoefficient As Double? Implements Interfaces.IPhaseProperties.activityCoefficient

        Public Property bubblePressure As Double? Implements Interfaces.IPhaseProperties.bubblePressure

        Public Property bubbleTemperature As Double? Implements Interfaces.IPhaseProperties.bubbleTemperature

        Public Property compressibility As Double? Implements Interfaces.IPhaseProperties.compressibility

        Public Property compressibilityFactor As Double? Implements Interfaces.IPhaseProperties.compressibilityFactor

        Public Property density As Double? Implements Interfaces.IPhaseProperties.density

        Public Property dewPressure As Double? Implements Interfaces.IPhaseProperties.dewPressure

        Public Property dewTemperature As Double? Implements Interfaces.IPhaseProperties.dewTemperature

        Public Property enthalpy As Double? Implements Interfaces.IPhaseProperties.enthalpy

        Public Property enthalpyF As Double? Implements Interfaces.IPhaseProperties.enthalpyF

        Public Property entropy As Double? Implements Interfaces.IPhaseProperties.entropy

        Public Property entropyF As Double? Implements Interfaces.IPhaseProperties.entropyF

        Public Property excessEnthalpy As Double? Implements Interfaces.IPhaseProperties.excessEnthalpy

        Public Property excessEntropy As Double? Implements Interfaces.IPhaseProperties.excessEntropy

        Public Property freezingPoint As Double? Implements Interfaces.IPhaseProperties.freezingPoint

        Public Property freezingPointDepression As Double? Implements Interfaces.IPhaseProperties.freezingPointDepression

        Public Property fugacity As Double? Implements Interfaces.IPhaseProperties.fugacity

        Public Property fugacityCoefficient As Double? Implements Interfaces.IPhaseProperties.fugacityCoefficient

        Public Property heatCapacityCp As Double? Implements Interfaces.IPhaseProperties.heatCapacityCp

        Public Property heatCapacityCv As Double? Implements Interfaces.IPhaseProperties.heatCapacityCv

        Public Property ionicStrength As Double? Implements Interfaces.IPhaseProperties.ionicStrength

        Public Property jouleThomsonCoefficient As Double? Implements Interfaces.IPhaseProperties.jouleThomsonCoefficient

        Public Property kinematic_viscosity As Double? Implements Interfaces.IPhaseProperties.kinematic_viscosity

        Public Property kvalue As Double? Implements Interfaces.IPhaseProperties.kvalue

        Public Property logFugacityCoefficient As Double? Implements Interfaces.IPhaseProperties.logFugacityCoefficient

        Public Property logKvalue As Double? Implements Interfaces.IPhaseProperties.logKvalue

        Public Property mean_ionic_acitivty_coefficient As Double? Implements Interfaces.IPhaseProperties.mean_ionic_acitivty_coefficient

        Public Property massflow As Double? Implements Interfaces.IPhaseProperties.massflow

        Public Property massfraction As Double? Implements Interfaces.IPhaseProperties.massfraction

        Public Property molar_enthalpy As Double? Implements Interfaces.IPhaseProperties.molar_enthalpy

        Public Property molar_enthalpyF As Double? Implements Interfaces.IPhaseProperties.molar_enthalpyF

        Public Property molar_entropy As Double? Implements Interfaces.IPhaseProperties.molar_entropy

        Public Property molar_entropyF As Double? Implements Interfaces.IPhaseProperties.molar_entropyF

        Public Property molarflow As Double? Implements Interfaces.IPhaseProperties.molarflow

        Public Property molarfraction As Double? Implements Interfaces.IPhaseProperties.molarfraction

        Public Property molecularWeight As Double? Implements Interfaces.IPhaseProperties.molecularWeight

        Public Property osmoticCoefficient As Double? Implements Interfaces.IPhaseProperties.osmoticCoefficient

        Public Property pH As Double? Implements Interfaces.IPhaseProperties.pH

        Public Property pressure As Double? Implements Interfaces.IPhaseProperties.pressure

        Public Property speedOfSound As Double? Implements Interfaces.IPhaseProperties.speedOfSound

        Public Property surfaceTension As Double? Implements Interfaces.IPhaseProperties.surfaceTension

        Public Property temperature As Double? Implements Interfaces.IPhaseProperties.temperature

        Public Property thermalConductivity As Double? Implements Interfaces.IPhaseProperties.thermalConductivity

        Public Property viscosity As Double? Implements Interfaces.IPhaseProperties.viscosity

        Public Property volumetric_flow As Double? Implements Interfaces.IPhaseProperties.volumetric_flow

        Public Property bulk_modulus As Double? Implements Interfaces.IPhaseProperties.bulk_modulus

        Public Property gibbs_free_energy As Double? Implements Interfaces.IPhaseProperties.gibbs_free_energy

        Public Property helmholtz_energy As Double? Implements Interfaces.IPhaseProperties.helmholtz_energy

        Public Property internal_energy As Double? Implements Interfaces.IPhaseProperties.internal_energy

        Public Property isothermal_compressibility As Double? Implements Interfaces.IPhaseProperties.isothermal_compressibility

        Public Property molar_gibbs_free_energy As Double? Implements Interfaces.IPhaseProperties.molar_gibbs_free_energy

        Public Property molar_helmholtz_energy As Double? Implements Interfaces.IPhaseProperties.molar_helmholtz_energy

        Public Property molar_internal_energy As Double? Implements Interfaces.IPhaseProperties.molar_internal_energy

        Public Property idealGasHeatCapacityCp As Double? Implements IPhaseProperties.idealGasHeatCapacityCp

        Public Property idealGasHeatCapacityRatio As Double? Implements IPhaseProperties.idealGasHeatCapacityRatio

        Public Property CO2loading As Double? Implements IPhaseProperties.CO2loading

        Public Property CO2partialpressure As Double? Implements IPhaseProperties.CO2partialpressure

        Public Property H2Sloading As Double? Implements IPhaseProperties.H2Sloading

        Public Property H2Spartialpressure As Double? Implements IPhaseProperties.H2Spartialpressure

    End Class

    <System.Serializable()> Public Class InteractionParameter

        Implements ICloneable, Interfaces.ICustomXMLSerialization
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

        Public Function LoadData(data As System.Collections.Generic.List(Of System.Xml.Linq.XElement)) As Boolean Implements Interfaces.ICustomXMLSerialization.LoadData

            XMLSerializer.XMLSerializer.Deserialize(Me, data, True)
            Return True

        End Function

        Public Function SaveData() As System.Collections.Generic.List(Of System.Xml.Linq.XElement) Implements Interfaces.ICustomXMLSerialization.SaveData

            Dim elements As List(Of System.Xml.Linq.XElement) = XMLSerializer.XMLSerializer.Serialize(Me, True)
            Dim ci As CultureInfo = CultureInfo.InvariantCulture

            Return elements

        End Function

    End Class

    <System.Serializable()> Public Class ConstantProperties

        Implements ICloneable, Interfaces.ICustomXMLSerialization, Interfaces.ICompoundConstantProperties

        Private Shared _unif As New PropertyPackages.Auxiliary.Unifac
        Private Shared _modf As New PropertyPackages.Auxiliary.Modfac

        Public Sub New()

        End Sub

        Public Overrides Function ToString() As String

            Return Name + String.Format(": {0}, {1}, NBP = {2} K, Tc = {3} K, Pc = {4} Pa, AF = {5}", Formula, CAS_Number, NBP, Critical_Temperature, Critical_Pressure, Acentric_Factor)

        End Function

        Public Sub UpdateElements()

            Dim el As New Dictionary(Of String, Double)

            Dim _molecule = Formula

            Dim useParenthesis As Boolean = _molecule.Contains("(") And _molecule.Contains(")")
            Dim findMatches = Regex.Matches(_molecule, "\(?[A-Z][a-z]?\d*\)?")

            ' Get all elements

            If useParenthesis Then
                Dim strval = If(Regex.IsMatch(_molecule, "\)\d+"), Regex.Match(_molecule, "\)\d+").Value.Remove(0, 1), "1")
                Dim endNumber As Double = Double.Parse(strval, NumberStyles.AllowDecimalPoint, System.Globalization.CultureInfo.InvariantCulture)
                ' Finds the number after the ')'
                For Each i As Match In findMatches
                    Dim element As String = Regex.Match(i.Value, "[A-Z][a-z]?").Value
                    ' Gets the element
                    Dim amountOfElement As Double = 0
                    If Regex.IsMatch(i.Value, "[\(\)]") Then
                        If Not Double.TryParse(Regex.Replace(i.Value, "(\(|\)|[A-Z]|[a-z])", ""), amountOfElement) Then
                            ' If the element has either '(' or ')' and doesn't specify an amount, then set it equal to the endnumber
                            amountOfElement = endNumber
                        Else
                            ' If the element has either '(' or ')' and specifies an amount, then multiply it by the end number
                            amountOfElement = amountOfElement * endNumber
                        End If
                    Else
                        amountOfElement = Double.Parse(If(String.IsNullOrWhiteSpace(i.Value.Replace(element, "")), "1", i.Value.Replace(element, "")))
                    End If
                    If el.ContainsKey(element) Then
                        el(element) += amountOfElement
                    Else
                        el.Add(element, amountOfElement)
                    End If
                Next
            Else
                Dim elementRegex = "([A-Z][a-z]*)([0-9]*)"
                Dim validateRegex = "^(" + elementRegex + ")+$"
                For Each match In Regex.Matches(_molecule, elementRegex)
                    Dim name = match.Groups(1).Value
                    Dim count = If(match.Groups(2).Value <> "", Integer.Parse(match.Groups(2).Value), 1)
                    If el.ContainsKey(name) Then
                        el(name) += count
                    Else
                        el.Add(name, count)
                    End If
                Next
            End If

            Elements = New SortedList()

            For Each item In el
                Elements.Add(item.Key, item.Value)
            Next

        End Sub

        Public Function Clone() As Object Implements System.ICloneable.Clone

            Dim comp = ObjectCopy(Me)

            comp.ExtraProperties = New ExpandoObject()

            Return comp

        End Function

        Function ObjectCopy(ByVal obj As ConstantProperties) As ConstantProperties

            Dim objMemStream As New MemoryStream(50000)
            Dim objBinaryFormatter As New BinaryFormatter(Nothing, New StreamingContext(StreamingContextStates.Clone))

            objBinaryFormatter.Serialize(objMemStream, obj)

            objMemStream.Seek(0, SeekOrigin.Begin)

            ObjectCopy = objBinaryFormatter.Deserialize(objMemStream)

            objMemStream.Close()

        End Function

        Public Function LoadData(data As System.Collections.Generic.List(Of System.Xml.Linq.XElement)) As Boolean Implements Interfaces.ICustomXMLSerialization.LoadData

            XMLSerializer.XMLSerializer.Deserialize(Me, data)

            ExtraProperties = New ExpandoObject

            Dim xel_d = (From xel2 As XElement In data Select xel2 Where xel2.Name = "DynamicProperties")

            If Not xel_d Is Nothing Then
                Dim dataDyn As List(Of XElement) = xel_d.Elements.ToList
                For Each xel As XElement In dataDyn
                    Try
                        Dim propname = xel.Element("Name").Value
                        Dim proptype = xel.Element("PropertyType").Value
                        Dim ptype As Type = Type.GetType(proptype)
                        Dim propval = Newtonsoft.Json.JsonConvert.DeserializeObject(xel.Element("Data").Value, ptype)
                        DirectCast(ExtraProperties, IDictionary(Of String, Object))(propname) = propval
                    Catch ex As Exception
                    End Try
                Next
            End If

            Me.UNIFACGroups.Clear()
            Me.MODFACGroups.Clear()

            For Each xel2 As XElement In (From xel As XElement In data Select xel Where xel.Name = "UNIFACGroups").Elements
                If xel2.@Name Is Nothing Then
                    Me.UNIFACGroups.Add(xel2.@GroupID.ToString, xel2.@Value)
                Else
                    Dim id As Integer = _unif.Group2ID(xel2.@Name)
                    Me.UNIFACGroups.Add(id.ToString, xel2.@Value)
                End If
            Next

            For Each xel2 As XElement In (From xel As XElement In data Select xel Where xel.Name = "MODFACGroups").Elements
                If xel2.@Name Is Nothing Then
                    Me.MODFACGroups.Add(xel2.@GroupID.ToString, xel2.@Value)
                Else
                    Dim id As Integer = _modf.Group2ID(xel2.@Name)
                    Me.MODFACGroups.Add(id.ToString, xel2.@Value)
                End If
            Next

            For Each xel2 As XElement In (From xel As XElement In data Select xel Where xel.Name = "NISTMODFACGroups").Elements
                Me.NISTMODFACGroups.Add(xel2.@GroupID.ToString, xel2.@Value)
            Next

            For Each xel2 As XElement In (From xel As XElement In data Select xel Where xel.Name = "Elements").Elements
                If Not Me.Elements.ContainsKey(xel2.@Name) Then
                    Me.Elements.Add(xel2.@Name, xel2.@Value)
                End If
            Next

            Return True

        End Function

        Public Function SaveData() As System.Collections.Generic.List(Of System.Xml.Linq.XElement) Implements Interfaces.ICustomXMLSerialization.SaveData

            Dim xelements As List(Of System.Xml.Linq.XElement) = XMLSerializer.XMLSerializer.Serialize(Me)
            Dim ci As CultureInfo = CultureInfo.InvariantCulture

            With xelements

                .Add(New XElement("DynamicProperties"))
                Dim extraprops = DirectCast(ExtraProperties, IDictionary(Of String, Object))
                For Each item In extraprops
                    Try
                        .Item(.Count - 1).Add(New XElement("Property", {New XElement("Name", item.Key),
                                                                               New XElement("PropertyType", item.Value.GetType.ToString),
                                                                               New XElement("Data", Newtonsoft.Json.JsonConvert.SerializeObject(item.Value))}))
                    Catch ex As Exception
                    End Try
                Next

                .Add(New XElement("UNIFACGroups"))

                If Not UNIFACGroups Is Nothing Then

                    For Each key As String In UNIFACGroups.Keys
                        .Item(xelements.Count - 1).Add(New XElement("Item", New XAttribute("GroupID", key), New XAttribute("Value", UNIFACGroups(key.ToString))))
                    Next

                End If

                .Add(New XElement("MODFACGroups"))

                If Not MODFACGroups Is Nothing Then

                    For Each key As String In MODFACGroups.Keys
                        .Item(xelements.Count - 1).Add(New XElement("Item", New XAttribute("GroupID", key), New XAttribute("Value", MODFACGroups(key.ToString))))
                    Next

                End If

                .Add(New XElement("NISTMODFACGroups"))

                If Not MODFACGroups Is Nothing Then

                    For Each key As String In NISTMODFACGroups.Keys
                        .Item(xelements.Count - 1).Add(New XElement("Item", New XAttribute("GroupID", key), New XAttribute("Value", NISTMODFACGroups(key.ToString))))
                    Next

                End If

                .Add(New XElement("Elements"))

                If Not Me.Elements Is Nothing Then

                    For Each key As String In Me.Elements.Keys
                        .Item(xelements.Count - 1).Add(New XElement("Item", New XAttribute("Name", key), New XAttribute("Value", Me.Elements(key))))
                    Next

                End If

            End With

            Return xelements

        End Function

        Public Function ExportToJSON() As String Implements ICompoundConstantProperties.ExportToJSON

            Return Newtonsoft.Json.JsonConvert.SerializeObject(Me, Newtonsoft.Json.Formatting.Indented)

        End Function

        Public Sub ImportFromJSON(data As String) Implements ICompoundConstantProperties.ImportFromJSON

            Dim obj = Newtonsoft.Json.JsonConvert.DeserializeObject(Of ConstantProperties)(data)

            Me.LoadData(obj.SaveData())

        End Sub

        Public Function GetVaporPressure(T As Double, Optional ByRef message As String = "") As Double Implements ICompoundConstantProperties.GetVaporPressure

            If IsPF = 1 Then
                message = "Estimated using Lee-Kesler correlation."
                Return PropertyPackages.Auxiliary.PROPS.Pvp_leekesler(T, Critical_Temperature, Critical_Pressure, Acentric_Factor)
            Else
                If OriginalDB = "DWSIM" Or
                    OriginalDB = "" Then
                    Dim A, B, C, D, E, result As Double
                    A = Vapor_Pressure_Constant_A
                    B = Vapor_Pressure_Constant_B
                    C = Vapor_Pressure_Constant_C
                    D = Vapor_Pressure_Constant_D
                    E = Vapor_Pressure_Constant_E
                    message = "Calculated using Experimental/Regressed data."
                    result = Math.Exp(A + B / T + C * Math.Log(T) + D * T ^ E)
                    Return result
                ElseIf OriginalDB = "CheResources" Then
                    Dim A, B, C, result As Double
                    A = Vapor_Pressure_Constant_A
                    B = Vapor_Pressure_Constant_B
                    C = Vapor_Pressure_Constant_C
                    '[LN(P)=A-B/(T+C), P(mmHG) T(K)]
                    message = "Calculated using Experimental/Regressed data."
                    result = Math.Exp(A - B / (T + C)) * 133.322368 'mmHg to Pascal
                    Return result
                ElseIf OriginalDB = "ChemSep" Or
                OriginalDB = "CoolProp" Or
                OriginalDB = "User" Or
                OriginalDB = "ChEDL Thermo" Or
                OriginalDB = "KDB" Then
                    Dim A, B, C, D, E, result As Double
                    Dim eqno As String = VaporPressureEquation
                    Dim mw As Double = Molar_Weight
                    A = Vapor_Pressure_Constant_A
                    B = Vapor_Pressure_Constant_B
                    C = Vapor_Pressure_Constant_C
                    D = Vapor_Pressure_Constant_D
                    E = Vapor_Pressure_Constant_E
                    '<vp_c name="Vapour pressure"  units="Pa" >
                    If eqno = "0" Then
                        message = "Estimated using Lee-Kesler correlation."
                        Return PropertyPackages.Auxiliary.PROPS.Pvp_leekesler(T, Critical_Temperature, Critical_Pressure, Acentric_Factor)
                    Else
                        If Integer.TryParse(eqno, New Integer) Then
                            message = "Calculated using Experimental/Regressed data."
                            result = PropertyPackages.PropertyPackage.CalcCSTDepProp(eqno, A, B, C, D, E, T, 0) 'Pa
                        Else
                            If eqno = "" Then
                                message = "Estimated using Lee-Kesler correlation."
                                Return PropertyPackages.Auxiliary.PROPS.Pvp_leekesler(T, Critical_Temperature, Critical_Pressure, Acentric_Factor)
                            Else
                                message = "Calculated using Experimental/Regressed data."
                                result = PropertyPackages.PropertyPackage.ParseEquation(eqno, A, B, C, D, E, T) 'Pa
                            End If
                        End If
                    End If
                    Return result
                ElseIf OriginalDB = "Biodiesel" Then
                    Dim A, B, C, D, E, result As Double
                    Dim eqno As String = VaporPressureEquation
                    A = Vapor_Pressure_Constant_A
                    B = Vapor_Pressure_Constant_B
                    C = Vapor_Pressure_Constant_C
                    D = Vapor_Pressure_Constant_D
                    E = Vapor_Pressure_Constant_E
                    result = PropertyPackages.PropertyPackage.CalcCSTDepProp(eqno, A, B, C, D, E, T, 0) 'kPa
                    message = "Calculated using Experimental/Regressed data."
                    Return result * 1000
                Else
                    message = "Estimated using Lee-Kesler correlation."
                    Return PropertyPackages.Auxiliary.PROPS.Pvp_leekesler(T, Critical_Temperature, Critical_Pressure, Acentric_Factor)
                End If
            End If

        End Function

        Public Function GetIdealGasHeatCapacity(T As Double, Optional ByRef message As String = "") As Double Implements ICompoundConstantProperties.GetIdealGasHeatCapacity

            Dim db As String = OriginalDB

            If IsPF = 1 Then

                message = "Estimated using Lee-Kesler correlation."
                Return PropertyPackages.Auxiliary.PROPS.Cpig_lk(PF_Watson_K, Acentric_Factor, T) '* .Molar_Weight

            Else

                If db = "DWSIM" Or db = "" Then
                    Dim A, B, C, D, E, result As Double
                    A = Ideal_Gas_Heat_Capacity_Const_A
                    B = Ideal_Gas_Heat_Capacity_Const_B
                    C = Ideal_Gas_Heat_Capacity_Const_C
                    D = Ideal_Gas_Heat_Capacity_Const_D
                    E = Ideal_Gas_Heat_Capacity_Const_E
                    'Cp = A + B*T + C*T^2 + D*T^3 + E*T^4 where Cp in kJ/kg-mol , T in K 
                    message = "Calculated using Experimental/Regressed data."
                    result = A + B * T + C * T ^ 2 + D * T ^ 3 + E * T ^ 4
                    Return result / Molar_Weight 'kJ/kg.K
                ElseIf db = "CheResources" Then
                    Dim A, B, C, D, E, result As Double
                    A = Ideal_Gas_Heat_Capacity_Const_A
                    B = Ideal_Gas_Heat_Capacity_Const_B
                    C = Ideal_Gas_Heat_Capacity_Const_C
                    D = Ideal_Gas_Heat_Capacity_Const_D
                    E = Ideal_Gas_Heat_Capacity_Const_E
                    'CAL/MOL.K [CP=A+(B*T)+(C*T^2)+(D*T^3)], T in K
                    message = "Calculated using Experimental/Regressed data."
                    result = A + B * T + C * T ^ 2 + D * T ^ 3
                    Return result / Molar_Weight * 4.1868 'kJ/kg.K
                ElseIf db = "ChemSep" Or db = "User" Then
                    Dim A, B, C, D, E, result As Double
                    Dim eqno As String = IdealgasCpEquation
                    Dim mw As Double = Molar_Weight
                    A = Ideal_Gas_Heat_Capacity_Const_A
                    B = Ideal_Gas_Heat_Capacity_Const_B
                    C = Ideal_Gas_Heat_Capacity_Const_C
                    D = Ideal_Gas_Heat_Capacity_Const_D
                    E = Ideal_Gas_Heat_Capacity_Const_E
                    message = "Calculated using Experimental/Regressed data."
                    If Integer.TryParse(eqno, New Integer) Then
                        result = PropertyPackages.PropertyPackage.CalcCSTDepProp(eqno, A, B, C, D, E, T, 0) / 1000 / mw 'kJ/kg.K
                    Else
                        result = PropertyPackages.PropertyPackage.ParseEquation(eqno, A, B, C, D, E, T) / mw
                    End If
                    If result = 0.0 Then
                        message = "Couldn't calculate Ideal Gas Cp."
                    End If
                    Return result
                ElseIf db = "ChEDL Thermo" Then
                    Dim A, B, C, D, E, result As Double
                    Dim eqno As String = IdealgasCpEquation
                    A = Ideal_Gas_Heat_Capacity_Const_A
                    B = Ideal_Gas_Heat_Capacity_Const_B
                    C = Ideal_Gas_Heat_Capacity_Const_C
                    D = Ideal_Gas_Heat_Capacity_Const_D
                    E = Ideal_Gas_Heat_Capacity_Const_E
                    message = "Calculated using Experimental/Regressed data."
                    result = PropertyPackages.PropertyPackage.CalcCSTDepProp(eqno, A, B, C, D, E, T, 0) 'kJ/kg.K
                    Return result
                ElseIf db = "CoolProp" Then
                    Dim A, B, C, D, E, result As Double
                    Dim eqno As String = IdealgasCpEquation
                    Dim mw As Double = Molar_Weight
                    A = Ideal_Gas_Heat_Capacity_Const_A
                    B = Ideal_Gas_Heat_Capacity_Const_B
                    C = Ideal_Gas_Heat_Capacity_Const_C
                    D = Ideal_Gas_Heat_Capacity_Const_D
                    E = Ideal_Gas_Heat_Capacity_Const_E
                    message = "Calculated using Experimental/Regressed data."
                    result = PropertyPackages.PropertyPackage.CalcCSTDepProp(eqno, A, B, C, D, E, T, 0) 'kJ/kg.K
                    Return result
                ElseIf db = "Biodiesel" Then
                    Dim A, B, C, D, E, result As Double
                    Dim eqno As String = IdealgasCpEquation
                    A = Ideal_Gas_Heat_Capacity_Const_A
                    B = Ideal_Gas_Heat_Capacity_Const_B
                    C = Ideal_Gas_Heat_Capacity_Const_C
                    D = Ideal_Gas_Heat_Capacity_Const_D
                    E = Ideal_Gas_Heat_Capacity_Const_E
                    message = "Calculated using Experimental/Regressed data."
                    result = PropertyPackages.PropertyPackage.CalcCSTDepProp(eqno, A, B, C, D, E, T, 0) 'kJ/kg.K
                    Return result
                ElseIf db = "KDB" Then
                    Dim A, B, C, D, E As Double
                    Dim eqno As String = IdealgasCpEquation
                    A = Ideal_Gas_Heat_Capacity_Const_A
                    B = Ideal_Gas_Heat_Capacity_Const_B
                    C = Ideal_Gas_Heat_Capacity_Const_C
                    D = Ideal_Gas_Heat_Capacity_Const_D
                    E = Ideal_Gas_Heat_Capacity_Const_E
                    Dim mw As Double = Molar_Weight
                    message = "Calculated using Experimental/Regressed data."
                    Return PropertyPackages.PropertyPackage.ParseEquation(eqno, A, B, C, D, E, T) / mw
                Else
                    message = "Couldn't calculate Ideal Gas Cp."
                    Return 0.0
                End If

            End If


        End Function

        Public Function GetEnthalpyOfVaporization(T As Double, Optional ByRef message As String = "") As Double Implements ICompoundConstantProperties.GetEnthalpyOfVaporization

            Dim A, B, C, D, E, Tr, result As Double
            A = HVap_A
            B = HVap_B
            C = HVap_C
            D = HVap_D
            E = HVap_E

            Tr = T / Critical_Temperature

            If Tr >= 1 Then Return 0.0#

            If OriginalDB = "DWSIM" Or OriginalDB = "" Then
                message = "Calculated using Experimental/Regressed data."
                If IsHYPO = 1 Or
                IsPF = 1 Then
                    Dim tr1 As Double
                    tr1 = Normal_Boiling_Point / Critical_Temperature
                    result = HVap_A * ((1 - Tr) / (1 - tr1)) ^ 0.375
                    Return result 'kJ/kg
                Else
                    result = A * (1 - Tr) ^ (B + C * Tr + D * Tr ^ 2)
                    Return result / Molar_Weight / 1000 'kJ/kg
                End If
            ElseIf OriginalDB = "CheResources" Or OriginalDB = "CoolProp" Or OriginalDB = "User" Or OriginalDB = "KDB" Then
                Dim tr1 As Double
                If OriginalDB = "KDB" Then
                    tr1 = HVap_B / Critical_Temperature
                Else
                    tr1 = Normal_Boiling_Point / Critical_Temperature
                End If
                If HVap_A = 0.0# Then
                    message = "Estimated using Vetere correlation."
                    HVap_A = New Utilities.Hypos.Methods.HYP().DHvb_Vetere(Critical_Temperature, Critical_Pressure, Normal_Boiling_Point)
                    HVap_A /= Molar_Weight
                End If
                result = HVap_A * ((1 - Tr) / (1 - tr1)) ^ 0.375
                Return result 'kJ/kg
            ElseIf OriginalDB = "ChemSep" Then
                Dim eqno As String = VaporizationEnthalpyEquation
                message = "Calculated using Experimental/Regressed data."
                result = PropertyPackages.PropertyPackage.CalcCSTDepProp(eqno, A, B, C, D, E, T, T / Tr) / Molar_Weight / 1000 'kJ/kg
                Return result
            ElseIf OriginalDB = "ChEDL Thermo" Then
                Dim eqno As String = VaporizationEnthalpyEquation
                message = "Calculated using Experimental/Regressed data."
                result = PropertyPackages.PropertyPackage.CalcCSTDepProp(eqno, A, B, C, D, E, T, T / Tr) 'kJ/kg
                Return result
            Else

            End If

        End Function

        Public Function GetVaporViscosity(T As Double, Optional ByRef message As String = "") As Double Implements ICompoundConstantProperties.GetVaporViscosity

            Dim val As Double

            If VaporViscosityEquation <> "" And VaporViscosityEquation <> "0" And Not IsIon And Not IsSalt Then
                message = "Calculated using Experimental/Regressed data."
                If Integer.TryParse(VaporViscosityEquation, New Integer) Then
                    val = PropertyPackages.PropertyPackage.CalcCSTDepProp(VaporViscosityEquation, Vapor_Viscosity_Const_A, Vapor_Viscosity_Const_B, Vapor_Viscosity_Const_C, Vapor_Viscosity_Const_D, Vapor_Viscosity_Const_E, T, Critical_Temperature)
                Else
                    val = PropertyPackages.PropertyPackage.ParseEquation(VaporViscosityEquation, Vapor_Viscosity_Const_A, Vapor_Viscosity_Const_B, Vapor_Viscosity_Const_C, Vapor_Viscosity_Const_D, Vapor_Viscosity_Const_E, T)
                End If
            ElseIf IsIon Or IsSalt Then
                val = 0.0#
            Else
                If Critical_Temperature > 0.0# Then
                    message = "Estimated using Lucas correlation."
                    val = PropertyPackages.Auxiliary.PROPS.viscg_lucas(T, Critical_Temperature, Critical_Pressure, Acentric_Factor, Molar_Weight)
                Else
                    val = 0.0#
                End If
            End If

            Return val

        End Function

        Public Function GetVaporThermalConductivity(T As Double, Optional ByRef message As String = "") As Double Implements ICompoundConstantProperties.GetVaporThermalConductivity

            Dim val As Double

            If VaporThermalConductivityEquation <> "" And VaporThermalConductivityEquation <> "0" And Not IsIon And Not IsSalt Then
                message = "Calculated using Experimental/Regressed data."
                val = PropertyPackages.PropertyPackage.CalcCSTDepProp(VaporThermalConductivityEquation, Vapor_Thermal_Conductivity_Const_A, Vapor_Thermal_Conductivity_Const_B, Vapor_Thermal_Conductivity_Const_C, Vapor_Thermal_Conductivity_Const_D, Vapor_Thermal_Conductivity_Const_E, T, Critical_Temperature)
            ElseIf IsIon Or IsSalt Then
                val = 0.0#
            Else
                message = "Estimated using Ely-Hanley correlation."
                val = PropertyPackages.Auxiliary.PROPS.condtg_elyhanley(T, Critical_Temperature, Critical_Volume / 1000, Critical_Compressibility, Acentric_Factor, Molar_Weight, GetIdealGasHeatCapacity(T) * Molar_Weight - 8.314)
            End If

            Return val

        End Function

        Public Function GetLiquidViscosity(T As Double, Optional ByRef message As String = "") As Double Implements ICompoundConstantProperties.GetLiquidViscosity

            If IsPF = 1 Then
                Dim dens = GetLiquidDensity(T)
                Dim visc = PropertyPackages.Auxiliary.PROPS.oilvisc_twu(T, PF_Tv1, PF_Tv2, PF_v1, PF_v2)
                If Double.IsNaN(visc) Then
                    Dim Tc, Pc, w, Mw As Double
                    Tc = Critical_Temperature
                    Pc = Critical_Pressure
                    w = Acentric_Factor
                    Mw = Molar_Weight
                    visc = PropertyPackages.Auxiliary.PROPS.viscl_letsti(T, Tc, Pc, w, Mw)
                End If
                message = "Estimated using Twu correlation."
                Return visc * dens
            Else
                If OriginalDB = "DWSIM" Or
                    OriginalDB = "" Then
                    Dim A, B, C, D, E, result As Double
                    A = Liquid_Viscosity_Const_A
                    B = Liquid_Viscosity_Const_B
                    C = Liquid_Viscosity_Const_C
                    D = Liquid_Viscosity_Const_D
                    E = Liquid_Viscosity_Const_E
                    message = "Calculated using Experimental/Regressed data."
                    result = Math.Exp(A + B / T + C * Math.Log(T) + D * T ^ E)
                    Return result
                ElseIf OriginalDB = "CheResources" Then
                    Dim B, C, result As Double
                    B = Liquid_Viscosity_Const_B
                    C = Liquid_Viscosity_Const_C
                    '[LOG(V)=B*(1/T-1/C), T(K) V(CP)]
                    message = "Calculated using Experimental/Regressed data."
                    result = Math.Exp(B * (1 / T - 1 / C)) * 0.001
                    Return result
                ElseIf OriginalDB = "ChemSep" Or
                    OriginalDB = "CoolProp" Or
                    OriginalDB = "User" Or
                    OriginalDB = "ChEDL Thermo" Or
                    OriginalDB = "KDB" Then
                    Dim A, B, C, D, E, result As Double
                    Dim eqno As String = LiquidViscosityEquation
                    Dim mw As Double = Molar_Weight
                    A = Liquid_Viscosity_Const_A
                    B = Liquid_Viscosity_Const_B
                    C = Liquid_Viscosity_Const_C
                    D = Liquid_Viscosity_Const_D
                    E = Liquid_Viscosity_Const_E
                    '<lvsc name="Liquid viscosity"  units="Pa.s" >
                    If eqno = "0" Or eqno = "" Then
                        Dim Tc, Pc, w As Double
                        Tc = Critical_Temperature
                        Pc = Critical_Pressure
                        w = Acentric_Factor
                        mw = Molar_Weight
                        message = "Estimated using Lestou-Stiel correlation."
                        result = PropertyPackages.Auxiliary.PROPS.viscl_letsti(T, Tc, Pc, w, mw)
                    Else
                        message = "Calculated using Experimental/Regressed data."
                        If Integer.TryParse(eqno, New Integer) Then
                            result = PropertyPackages.PropertyPackage.CalcCSTDepProp(eqno, A, B, C, D, E, T, 0) 'Pa.s
                        Else
                            result = PropertyPackages.PropertyPackage.ParseEquation(eqno, A, B, C, D, E, T) 'Pa.s
                        End If
                    End If
                    Return result
                ElseIf OriginalDB = "Biodiesel" Then
                    Dim result As Double
                    Dim Tc, Pc, w, Mw As Double
                    Tc = Critical_Temperature
                    Pc = Critical_Pressure
                    w = Acentric_Factor
                    Mw = Molar_Weight
                    message = "Estimated using Lestou-Stiel correlation."
                    result = PropertyPackages.Auxiliary.PROPS.viscl_letsti(T, Tc, Pc, w, Mw)
                    Return result
                End If
            End If

        End Function

        Public Function GetLiquidThermalConductivity(T As Double, Optional ByRef message As String = "") As Double Implements ICompoundConstantProperties.GetLiquidThermalConductivity

            Dim val As Double

            If LiquidThermalConductivityEquation <> "" And LiquidThermalConductivityEquation <> "0" And Not IsIon And Not IsSalt Then
                message = "Calculated using Experimental/Regressed data."
                val = PropertyPackages.PropertyPackage.CalcCSTDepProp(LiquidThermalConductivityEquation, Liquid_Thermal_Conductivity_Const_A, Liquid_Thermal_Conductivity_Const_B, Liquid_Thermal_Conductivity_Const_C, Liquid_Thermal_Conductivity_Const_D, Liquid_Thermal_Conductivity_Const_E, T, Critical_Temperature)
            ElseIf IsIon Or IsSalt Then
                val = 0.0#
            Else
                message = "Estimated using Latini correlation."
                val = PropertyPackages.Auxiliary.PROPS.condl_latini(T, Normal_Boiling_Point, Critical_Temperature, Molar_Weight, "")
            End If

            Return val

        End Function

        Public Function GetLiquidHeatCapacity(T As Double, Optional ByRef message As String = "") As Double Implements ICompoundConstantProperties.GetLiquidHeatCapacity

            Dim val As Double
            If LiquidHeatCapacityEquation <> "" And LiquidHeatCapacityEquation <> "0" And Not IsIon And Not IsSalt Then
                message = "Calculated using Experimental/Regressed data."
                If Integer.TryParse(LiquidHeatCapacityEquation, New Integer) Then
                    val = PropertyPackages.PropertyPackage.CalcCSTDepProp(LiquidHeatCapacityEquation, Liquid_Heat_Capacity_Const_A, Liquid_Heat_Capacity_Const_B, Liquid_Heat_Capacity_Const_C, Liquid_Heat_Capacity_Const_D, Liquid_Heat_Capacity_Const_E, T, Critical_Temperature)
                Else
                    val = PropertyPackages.PropertyPackage.ParseEquation(LiquidHeatCapacityEquation, Liquid_Heat_Capacity_Const_A, Liquid_Heat_Capacity_Const_B, Liquid_Heat_Capacity_Const_C, Liquid_Heat_Capacity_Const_D, Liquid_Heat_Capacity_Const_E, T) / Molar_Weight
                End If
                If OriginalDB <> "CoolProp" And OriginalDB <> "ChEDL Thermo" Then val = val / 1000 / Molar_Weight 'kJ/kg.K
            Else
                'estimate using Rownlinson/Bondi correlation
                message = "Estimated using Rowlinson/Bondi correlation."
                val = PropertyPackages.Auxiliary.PROPS.Cpl_rb(GetIdealGasHeatCapacity(T), T, Critical_Temperature, Acentric_Factor, Molar_Weight) 'kJ/kg.K
            End If

            Return val

        End Function

        Public Function GetLiquidDensity(T As Double, Optional ByRef message As String = "") As Double Implements ICompoundConstantProperties.GetLiquidDensity

            Dim val As Double

            If LiquidDensityEquation <> "" And LiquidDensityEquation <> "0" And Not IsIon And Not IsSalt Then
                message = "Calculated using Experimental/Regressed data."
                If Integer.TryParse(LiquidDensityEquation, New Integer) Then
                    val = PropertyPackages.PropertyPackage.CalcCSTDepProp(LiquidDensityEquation, Liquid_Density_Const_A, Liquid_Density_Const_B, Liquid_Density_Const_C, Liquid_Density_Const_D, Liquid_Density_Const_E, T, Critical_Temperature)
                Else
                    val = PropertyPackages.PropertyPackage.ParseEquation(LiquidDensityEquation, Liquid_Density_Const_A, Liquid_Density_Const_B, Liquid_Density_Const_C, Liquid_Density_Const_D, Liquid_Density_Const_E, T)
                End If
                If OriginalDB <> "CoolProp" And OriginalDB <> "User" And OriginalDB <> "ChEDL Thermo" Then val = Molar_Weight * val
            Else
                message = "Estimated using Rackett correlation."
                val = PropertyPackages.Auxiliary.PROPS.liq_dens_rackett(T, Critical_Temperature, Critical_Pressure, Acentric_Factor, Molar_Weight, Z_Rackett)
            End If

            Return val 'kg/m3

        End Function

        Public Function GetLiquidSurfaceTension(T As Double, Optional ByRef message As String = "") As Double Implements ICompoundConstantProperties.GetLiquidSurfaceTension

            Dim val As Double

            If SurfaceTensionEquation <> "" And SurfaceTensionEquation <> "0" And Not IsIon And Not IsSalt Then
                message = "Calculated using Experimental/Regressed data."
                If Integer.TryParse(SurfaceTensionEquation, New Integer) Then
                    val = PropertyPackages.PropertyPackage.CalcCSTDepProp(SurfaceTensionEquation, Surface_Tension_Const_A, Surface_Tension_Const_B, Surface_Tension_Const_C, Surface_Tension_Const_D, Surface_Tension_Const_E, T, Critical_Temperature)
                Else
                    val = PropertyPackages.PropertyPackage.ParseEquation(SurfaceTensionEquation, Surface_Tension_Const_A, Surface_Tension_Const_B, Surface_Tension_Const_C, Surface_Tension_Const_D, Surface_Tension_Const_E, T)
                End If
            Else
                message = "Estimated using Rackett correlation."
                val = PropertyPackages.Auxiliary.PROPS.sigma_bb(T, Normal_Boiling_Point, Critical_Temperature, Critical_Pressure)
            End If

            Return val 'N/m

        End Function

        Public Function GetSolidDensity(T As Double, Optional ByRef message As String = "") As Double Implements ICompoundConstantProperties.GetSolidDensity

            Dim val As Double

            If OriginalDB = "ChemSep" Or (OriginalDB = "User" And SolidDensityEquation <> "") Then
                Dim A, B, C, D, E, result As Double
                Dim eqno As String = SolidDensityEquation
                Dim mw As Double = Molar_Weight
                A = Solid_Density_Const_A
                B = Solid_Density_Const_B
                C = Solid_Density_Const_C
                D = Solid_Density_Const_D
                E = Solid_Density_Const_E
                message = "Calculated using Experimental/Regressed data."
                If eqno <> "" Then
                    If Integer.TryParse(SurfaceTensionEquation, New Integer) Then
                        result = PropertyPackages.PropertyPackage.CalcCSTDepProp(eqno, A, B, C, D, E, T, 0) 'kmol/m3
                    Else
                        result = PropertyPackages.PropertyPackage.ParseEquation(eqno, A, B, C, D, E, T) 'kmol/m3
                    End If
                End If
                val = 1 / (result * mw)
            ElseIf OriginalDB = "ChEDL Thermo" Then
                Dim A, B, C, D, E, result As Double
                Dim eqno As String = SolidDensityEquation
                A = Solid_Density_Const_A
                B = Solid_Density_Const_B
                C = Solid_Density_Const_C
                D = Solid_Density_Const_D
                E = Solid_Density_Const_E
                message = "Calculated using Experimental/Regressed data."
                If eqno <> "" Then result = result = PropertyPackages.PropertyPackage.CalcCSTDepProp(eqno, A, B, C, D, E, T, 0) 'kg/m3
                val = 1 / (result)
            Else
                message = "Using stored value at Ts."
                If SolidDensityAtTs <> 0.0# Then
                    val = 1 / SolidDensityAtTs
                Else
                    val = 1.0E+20
                End If
            End If

            Return 1 / val

        End Function

        Public Function GetSolidHeatCapacity(T As Double, Optional ByRef message As String = "") As Double Implements ICompoundConstantProperties.GetSolidHeatCapacity

            Dim val As Double

            If OriginalDB = "ChemSep" Or (OriginalDB = "User" And SolidHeatCapacityEquation <> "") Then
                Dim A, B, C, D, E, result As Double
                Dim eqno As String = SolidHeatCapacityEquation
                Dim mw As Double = Molar_Weight
                A = Solid_Heat_Capacity_Const_A
                B = Solid_Heat_Capacity_Const_B
                C = Solid_Heat_Capacity_Const_C
                D = Solid_Heat_Capacity_Const_D
                E = Solid_Heat_Capacity_Const_E
                message = "Calculated using Experimental/Regressed data."
                If Integer.TryParse(SurfaceTensionEquation, New Integer) Then
                    result = PropertyPackages.PropertyPackage.CalcCSTDepProp(eqno, A, B, C, D, E, T, 0) 'J/kmol/K
                Else
                    result = PropertyPackages.PropertyPackage.ParseEquation(eqno, A, B, C, D, E, T) 'J/kmol/K
                End If
                val = result / 1000 / mw 'kJ/kg.K
            ElseIf OriginalDB = "ChEDL Thermo" Then
                Dim A, B, C, D, E As Double
                Dim eqno As String = SolidHeatCapacityEquation
                A = Solid_Heat_Capacity_Const_A
                B = Solid_Heat_Capacity_Const_B
                C = Solid_Heat_Capacity_Const_C
                D = Solid_Heat_Capacity_Const_D
                E = Solid_Heat_Capacity_Const_E
                message = "Calculated using Experimental/Regressed data."
                val = PropertyPackages.PropertyPackage.CalcCSTDepProp(eqno, A, B, C, D, E, T, 0) 'kJ/kg/K
            Else
                message = "Using default value when no data is available."
                val = 3 ' replacement if no params available
            End If

            Return val

        End Function

        Public Property Acentric_Factor As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.Acentric_Factor

        Public Property BO_BSW As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.BO_BSW

        Public Property BO_GOR As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.BO_GOR

        Public Property BO_OilVisc1 As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.BO_OilVisc1

        Public Property BO_OilVisc2 As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.BO_OilVisc2

        Public Property BO_OilViscTemp1 As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.BO_OilViscTemp1

        Public Property BO_OilViscTemp2 As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.BO_OilViscTemp2

        Public Property BO_PNA_A As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.BO_PNA_A

        Public Property BO_PNA_N As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.BO_PNA_N

        Public Property BO_PNA_P As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.BO_PNA_P

        Public Property BO_SGG As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.BO_SGG

        Public Property BO_SGO As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.BO_SGO

        Public Property CAS_Number As String = "" Implements Interfaces.ICompoundConstantProperties.CAS_Number

        Public Property Chao_Seader_Acentricity As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.Chao_Seader_Acentricity

        Public Property Chao_Seader_Liquid_Molar_Volume As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.Chao_Seader_Liquid_Molar_Volume

        Public Property Chao_Seader_Solubility_Parameter As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.Chao_Seader_Solubility_Parameter

        Public Property Charge As Integer Implements Interfaces.ICompoundConstantProperties.Charge

        Public Property ChemicalStructure As String = "" Implements Interfaces.ICompoundConstantProperties.ChemicalStructure

        Public Property Comments As String = "" Implements Interfaces.ICompoundConstantProperties.Comments

        Public Property CompCreatorStudyFile As String = "" Implements Interfaces.ICompoundConstantProperties.CompCreatorStudyFile

        Public Property COSMODBName As Object Implements Interfaces.ICompoundConstantProperties.COSMODBName

        Public Property Critical_Compressibility As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.Critical_Compressibility

        Public Property Critical_Pressure As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.Critical_Pressure

        Public Property Critical_Temperature As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.Critical_Temperature

        Public Property Critical_Volume As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.Critical_Volume

        Public Property CurrentDB As String = "DWSIM" Implements Interfaces.ICompoundConstantProperties.CurrentDB

        Public Property Dipole_Moment As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.Dipole_Moment

        Public Property Electrolyte_Cp0 As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.Electrolyte_Cp0

        Public Property Electrolyte_DelGF As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.Electrolyte_DelGF

        Public Property Electrolyte_DelHF As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.Electrolyte_DelHF

        Public Property EnthalpyOfFusionAtTf As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.EnthalpyOfFusionAtTf

        Private _formula As String = ""

        Public Property Formula As String Implements Interfaces.ICompoundConstantProperties.Formula
            Get
                Return _formula
            End Get
            Set(value As String)
                _formula = value
                Try
                    UpdateElements()
                Catch ex As Exception
                End Try
            End Set
        End Property

        Public Property HVap_A As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.HVap_A

        Public Property HVap_B As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.HVap_B

        Public Property HVap_C As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.HVap_C

        Public Property HVap_D As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.HVap_D

        Public Property HVap_E As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.HVap_E

        Public Property HVap_TMAX As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.HVap_TMAX

        Public Property HVap_TMIN As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.HVap_TMIN

        Public Property HydrationNumber As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.HydrationNumber

        Public Property ID As Integer Implements Interfaces.ICompoundConstantProperties.ID

        Public Property Ideal_Gas_Heat_Capacity_Const_A As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.Ideal_Gas_Heat_Capacity_Const_A

        Public Property Ideal_Gas_Heat_Capacity_Const_B As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.Ideal_Gas_Heat_Capacity_Const_B

        Public Property Ideal_Gas_Heat_Capacity_Const_C As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.Ideal_Gas_Heat_Capacity_Const_C

        Public Property Ideal_Gas_Heat_Capacity_Const_D As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.Ideal_Gas_Heat_Capacity_Const_D

        Public Property Ideal_Gas_Heat_Capacity_Const_E As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.Ideal_Gas_Heat_Capacity_Const_E

        Public Property IdealgasCpEquation As String = "" Implements Interfaces.ICompoundConstantProperties.IdealgasCpEquation

        Public Property IG_Enthalpy_of_Formation_25C As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.IG_Enthalpy_of_Formation_25C

        Public Property IG_Entropy_of_Formation_25C As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.IG_Entropy_of_Formation_25C

        Public Property IG_Gibbs_Energy_of_Formation_25C As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.IG_Gibbs_Energy_of_Formation_25C

        Public Property InChI As String = "" Implements Interfaces.ICompoundConstantProperties.InChI

        Public Property Ion_CpAq_a As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.Ion_CpAq_a

        Public Property Ion_CpAq_b As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.Ion_CpAq_b

        Public Property Ion_CpAq_c As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.Ion_CpAq_c

        Public Property IsBlackOil As Boolean Implements Interfaces.ICompoundConstantProperties.IsBlackOil

        Public Property IsCOOLPROPSupported As Boolean Implements Interfaces.ICompoundConstantProperties.IsCOOLPROPSupported

        Public Property IsFPROPSSupported As Boolean Implements Interfaces.ICompoundConstantProperties.IsFPROPSSupported

        Public Property IsHydratedSalt As Boolean Implements Interfaces.ICompoundConstantProperties.IsHydratedSalt

        Public Property IsHYPO As Integer Implements Interfaces.ICompoundConstantProperties.IsHYPO

        Public Property IsIon As Boolean Implements Interfaces.ICompoundConstantProperties.IsIon

        Public Property IsModified As Boolean Implements Interfaces.ICompoundConstantProperties.IsModified

        Public Property IsPF As Integer Implements Interfaces.ICompoundConstantProperties.IsPF

        Public Property IsSalt As Boolean Implements Interfaces.ICompoundConstantProperties.IsSalt

        Public Property Liquid_Density_Const_A As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.Liquid_Density_Const_A

        Public Property Liquid_Density_Const_B As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.Liquid_Density_Const_B

        Public Property Liquid_Density_Const_C As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.Liquid_Density_Const_C

        Public Property Liquid_Density_Const_D As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.Liquid_Density_Const_D

        Public Property Liquid_Density_Const_E As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.Liquid_Density_Const_E

        Public Property Liquid_Density_Tmax As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.Liquid_Density_Tmax

        Public Property Liquid_Density_Tmin As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.Liquid_Density_Tmin

        Public Property Liquid_Heat_Capacity_Const_A As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.Liquid_Heat_Capacity_Const_A

        Public Property Liquid_Heat_Capacity_Const_B As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.Liquid_Heat_Capacity_Const_B

        Public Property Liquid_Heat_Capacity_Const_C As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.Liquid_Heat_Capacity_Const_C

        Public Property Liquid_Heat_Capacity_Const_D As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.Liquid_Heat_Capacity_Const_D

        Public Property Liquid_Heat_Capacity_Const_E As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.Liquid_Heat_Capacity_Const_E

        Public Property Liquid_Heat_Capacity_Tmax As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.Liquid_Heat_Capacity_Tmax

        Public Property Liquid_Heat_Capacity_Tmin As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.Liquid_Heat_Capacity_Tmin

        Public Property Liquid_Thermal_Conductivity_Const_A As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.Liquid_Thermal_Conductivity_Const_A

        Public Property Liquid_Thermal_Conductivity_Const_B As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.Liquid_Thermal_Conductivity_Const_B

        Public Property Liquid_Thermal_Conductivity_Const_C As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.Liquid_Thermal_Conductivity_Const_C

        Public Property Liquid_Thermal_Conductivity_Const_D As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.Liquid_Thermal_Conductivity_Const_D

        Public Property Liquid_Thermal_Conductivity_Const_E As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.Liquid_Thermal_Conductivity_Const_E

        Public Property Liquid_Thermal_Conductivity_Tmax As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.Liquid_Thermal_Conductivity_Tmax

        Public Property Liquid_Thermal_Conductivity_Tmin As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.Liquid_Thermal_Conductivity_Tmin

        Public Property Liquid_Viscosity_Const_A As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.Liquid_Viscosity_Const_A

        Public Property Liquid_Viscosity_Const_B As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.Liquid_Viscosity_Const_B

        Public Property Liquid_Viscosity_Const_C As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.Liquid_Viscosity_Const_C

        Public Property Liquid_Viscosity_Const_D As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.Liquid_Viscosity_Const_D

        Public Property Liquid_Viscosity_Const_E As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.Liquid_Viscosity_Const_E

        Public Property LiquidDensityEquation As String = "" Implements Interfaces.ICompoundConstantProperties.LiquidDensityEquation

        Public Property LiquidHeatCapacityEquation As String = "" Implements Interfaces.ICompoundConstantProperties.LiquidHeatCapacityEquation

        Public Property LiquidThermalConductivityEquation As String = "" Implements Interfaces.ICompoundConstantProperties.LiquidThermalConductivityEquation

        Public Property LiquidViscosityEquation As String = "" Implements Interfaces.ICompoundConstantProperties.LiquidViscosityEquation

        Public Property Molar_Weight As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.Molar_Weight

        Public Property MolarVolume_k1i As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.MolarVolume_k1i

        Public Property MolarVolume_k2i As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.MolarVolume_k2i

        Public Property MolarVolume_k3i As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.MolarVolume_k3i

        Public Property MolarVolume_v2i As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.MolarVolume_v2i

        Public Property MolarVolume_v3i As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.MolarVolume_v3i

        Public Property Name As String = "" Implements Interfaces.ICompoundConstantProperties.Name

        Public Property NBP As Double? Implements Interfaces.ICompoundConstantProperties.NBP

        Public Property NegativeIon As String = "" Implements Interfaces.ICompoundConstantProperties.NegativeIon

        Public Property NegativeIonStoichCoeff As Integer Implements Interfaces.ICompoundConstantProperties.NegativeIonStoichCoeff

        Public Property Normal_Boiling_Point As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.Normal_Boiling_Point

        Public Property OriginalDB As String = "DWSIM" Implements Interfaces.ICompoundConstantProperties.OriginalDB

        Public Property PC_SAFT_epsilon_k As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.PC_SAFT_epsilon_k

        Public Property PC_SAFT_m As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.PC_SAFT_m

        Public Property PC_SAFT_sigma As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.PC_SAFT_sigma

        Public Property PF_MM As Double? Implements Interfaces.ICompoundConstantProperties.PF_MM

        Public Property PF_SG As Double? Implements Interfaces.ICompoundConstantProperties.PF_SG

        Public Property PF_Tv1 As Double? Implements Interfaces.ICompoundConstantProperties.PF_Tv1

        Public Property PF_Tv2 As Double? Implements Interfaces.ICompoundConstantProperties.PF_Tv2

        Public Property PF_v1 As Double? Implements Interfaces.ICompoundConstantProperties.PF_v1

        Public Property PF_v2 As Double? Implements Interfaces.ICompoundConstantProperties.PF_v2

        Public Property PF_vA As Double? Implements Interfaces.ICompoundConstantProperties.PF_vA

        Public Property PF_vB As Double? Implements Interfaces.ICompoundConstantProperties.PF_vB

        Public Property PF_Watson_K As Double? Implements Interfaces.ICompoundConstantProperties.PF_Watson_K

        Public Property PositiveIon As String = "" Implements Interfaces.ICompoundConstantProperties.PositiveIon

        Public Property PositiveIonStoichCoeff As Integer Implements Interfaces.ICompoundConstantProperties.PositiveIonStoichCoeff

        Public Property PR_Volume_Translation_Coefficient As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.PR_Volume_Translation_Coefficient

        Public Property SMILES As String = "" Implements Interfaces.ICompoundConstantProperties.SMILES

        Public Property Solid_Density_Const_A As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.Solid_Density_Const_A

        Public Property Solid_Density_Const_B As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.Solid_Density_Const_B

        Public Property Solid_Density_Const_C As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.Solid_Density_Const_C

        Public Property Solid_Density_Const_D As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.Solid_Density_Const_D

        Public Property Solid_Density_Const_E As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.Solid_Density_Const_E

        Public Property Solid_Density_Tmax As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.Solid_Density_Tmax

        Public Property Solid_Density_Tmin As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.Solid_Density_Tmin

        Public Property Solid_Heat_Capacity_Const_A As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.Solid_Heat_Capacity_Const_A

        Public Property Solid_Heat_Capacity_Const_B As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.Solid_Heat_Capacity_Const_B

        Public Property Solid_Heat_Capacity_Const_C As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.Solid_Heat_Capacity_Const_C

        Public Property Solid_Heat_Capacity_Const_D As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.Solid_Heat_Capacity_Const_D

        Public Property Solid_Heat_Capacity_Const_E As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.Solid_Heat_Capacity_Const_E

        Public Property Solid_Heat_Capacity_Tmax As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.Solid_Heat_Capacity_Tmax

        Public Property Solid_Heat_Capacity_Tmin As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.Solid_Heat_Capacity_Tmin

        Public Property SolidDensityAtTs As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.SolidDensityAtTs

        Public Property SolidDensityEquation As String = "" Implements Interfaces.ICompoundConstantProperties.SolidDensityEquation

        Public Property SolidHeatCapacityEquation As String = "" Implements Interfaces.ICompoundConstantProperties.SolidHeatCapacityEquation

        Public Property SolidTs As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.SolidTs

        Public Property SRK_Volume_Translation_Coefficient As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.SRK_Volume_Translation_Coefficient

        Public Property StandardStateMolarVolume As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.StandardStateMolarVolume

        Public Property StoichSum As Integer Implements Interfaces.ICompoundConstantProperties.StoichSum

        Public Property Surface_Tension_Const_A As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.Surface_Tension_Const_A

        Public Property Surface_Tension_Const_B As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.Surface_Tension_Const_B

        Public Property Surface_Tension_Const_C As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.Surface_Tension_Const_C

        Public Property Surface_Tension_Const_D As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.Surface_Tension_Const_D

        Public Property Surface_Tension_Const_E As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.Surface_Tension_Const_E

        Public Property Surface_Tension_Tmax As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.Surface_Tension_Tmax

        Public Property Surface_Tension_Tmin As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.Surface_Tension_Tmin

        Public Property SurfaceTensionEquation As String = "" Implements Interfaces.ICompoundConstantProperties.SurfaceTensionEquation

        Public Property TemperatureOfFusion As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.TemperatureOfFusion

        Public Property UNIQUAC_Q As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.UNIQUAC_Q

        Public Property UNIQUAC_R As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.UNIQUAC_R

        Public Property Vapor_Pressure_Constant_A As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.Vapor_Pressure_Constant_A

        Public Property Vapor_Pressure_Constant_B As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.Vapor_Pressure_Constant_B

        Public Property Vapor_Pressure_Constant_C As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.Vapor_Pressure_Constant_C

        Public Property Vapor_Pressure_Constant_D As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.Vapor_Pressure_Constant_D

        Public Property Vapor_Pressure_Constant_E As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.Vapor_Pressure_Constant_E

        Public Property Vapor_Pressure_TMAX As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.Vapor_Pressure_TMAX

        Public Property Vapor_Pressure_TMIN As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.Vapor_Pressure_TMIN

        Public Property Vapor_Thermal_Conductivity_Const_A As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.Vapor_Thermal_Conductivity_Const_A

        Public Property Vapor_Thermal_Conductivity_Const_B As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.Vapor_Thermal_Conductivity_Const_B

        Public Property Vapor_Thermal_Conductivity_Const_C As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.Vapor_Thermal_Conductivity_Const_C

        Public Property Vapor_Thermal_Conductivity_Const_D As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.Vapor_Thermal_Conductivity_Const_D

        Public Property Vapor_Thermal_Conductivity_Const_E As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.Vapor_Thermal_Conductivity_Const_E

        Public Property Vapor_Thermal_Conductivity_Tmax As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.Vapor_Thermal_Conductivity_Tmax

        Public Property Vapor_Thermal_Conductivity_Tmin As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.Vapor_Thermal_Conductivity_Tmin

        Public Property Vapor_Viscosity_Const_A As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.Vapor_Viscosity_Const_A

        Public Property Vapor_Viscosity_Const_B As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.Vapor_Viscosity_Const_B

        Public Property Vapor_Viscosity_Const_C As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.Vapor_Viscosity_Const_C

        Public Property Vapor_Viscosity_Const_D As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.Vapor_Viscosity_Const_D

        Public Property Vapor_Viscosity_Const_E As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.Vapor_Viscosity_Const_E

        Public Property Vapor_Viscosity_Tmax As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.Vapor_Viscosity_Tmax

        Public Property Vapor_Viscosity_Tmin As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.Vapor_Viscosity_Tmin

        Public Property VaporizationEnthalpyEquation As String = "" Implements Interfaces.ICompoundConstantProperties.VaporizationEnthalpyEquation

        Public Property VaporPressureEquation As String = "" Implements Interfaces.ICompoundConstantProperties.VaporPressureEquation

        Public Property VaporThermalConductivityEquation As String = "" Implements Interfaces.ICompoundConstantProperties.VaporThermalConductivityEquation

        Public Property VaporViscosityEquation As String = "" Implements Interfaces.ICompoundConstantProperties.VaporViscosityEquation

        Public Property Z_Rackett As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.Z_Rackett

        Public Property Elements As SortedList = New SortedList() Implements Interfaces.ICompoundConstantProperties.Elements

        Public Property MODFACGroups As SortedList = New SortedList() Implements Interfaces.ICompoundConstantProperties.MODFACGroups

        Public Property NISTMODFACGroups As SortedList = New SortedList() Implements Interfaces.ICompoundConstantProperties.NISTMODFACGroups

        Public Property UNIFACGroups As SortedList = New SortedList() Implements Interfaces.ICompoundConstantProperties.UNIFACGroups

        Public Property FullerDiffusionVolume As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.FullerDiffusionVolume

        Public Property LennardJonesDiameter As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.LennardJonesDiameter

        Public Property LennardJonesEnergy As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.LennardJonesEnergy

        Public Property Parachor As Double = 0.0# Implements Interfaces.ICompoundConstantProperties.Parachor

        Public Property Tag As String = "" Implements ICompoundConstantProperties.Tag

        <NonSerialized> Private _ep As New ExpandoObject

        Public Property ExtraProperties As ExpandoObject Implements ICompoundConstantProperties.ExtraProperties
            Get
                Return _ep
            End Get
            Set(value As ExpandoObject)
                _ep = value
            End Set
        End Property

        Public Property COSTALD_SRK_Acentric_Factor As Double = 0.0 Implements ICompoundConstantProperties.COSTALD_SRK_Acentric_Factor

        Public Property COSTALD_Characteristic_Volume As Double = 0.0 Implements ICompoundConstantProperties.COSTALD_Characteristic_Volume

        Public Property IsSolid As Boolean = False Implements ICompoundConstantProperties.IsSolid

        Public Property ChemSepFamily As Integer = 1000 Implements ICompoundConstantProperties.ChemSepFamily

        Public Property Vapor_Pressure_Regression_Fit As Double = 0.0 Implements ICompoundConstantProperties.Vapor_Pressure_Regression_Fit

        Public Property Vapor_Pressure_Tabular_Data As ITabularData = New TabularData() Implements ICompoundConstantProperties.Vapor_Pressure_Tabular_Data

        Public Property Ideal_Gas_Heat_Capacity_Regression_Fit As Double = 0.0 Implements ICompoundConstantProperties.Ideal_Gas_Heat_Capacity_Regression_Fit

        Public Property Ideal_Gas_Heat_Capacity_Tabular_Data As ITabularData = New TabularData() Implements ICompoundConstantProperties.Ideal_Gas_Heat_Capacity_Tabular_Data

        Public Property Liquid_Viscosity_Regression_Fit As Double = 0.0 Implements ICompoundConstantProperties.Liquid_Viscosity_Regression_Fit

        Public Property Liquid_Viscosity_Tabular_Data As ITabularData = New TabularData() Implements ICompoundConstantProperties.Liquid_Viscosity_Tabular_Data

        Public Property Liquid_Density_Regression_Fit As Double = 0.0 Implements ICompoundConstantProperties.Liquid_Density_Regression_Fit

        Public Property Liquid_Density_Tabular_Data As ITabularData = New TabularData() Implements ICompoundConstantProperties.Liquid_Density_Tabular_Data

        Public Property Liquid_Heat_Capacity_Regression_Fit As Double = 0.0 Implements ICompoundConstantProperties.Liquid_Heat_Capacity_Regression_Fit

        Public Property Liquid_Heat_Capacity_Tabular_Data As ITabularData = New TabularData() Implements ICompoundConstantProperties.Liquid_Heat_Capacity_Tabular_Data

        Public Property Liquid_Thermal_Conductivity_Regression_Fit As Double = 0.0 Implements ICompoundConstantProperties.Liquid_Thermal_Conductivity_Regression_Fit

        Public Property Liquid_Thermal_Conductivity_Tabular_Data As ITabularData = New TabularData() Implements ICompoundConstantProperties.Liquid_Thermal_Conductivity_Tabular_Data

        Public Property Vapor_Thermal_Conductivity_Regression_Fit As Double = 0.0 Implements ICompoundConstantProperties.Vapor_Thermal_Conductivity_Regression_Fit

        Public Property Vapor_Thermal_Conductivity_Tabular_Data As ITabularData = New TabularData() Implements ICompoundConstantProperties.Vapor_Thermal_Conductivity_Tabular_Data

        Public Property Vapor_Viscosity_Regression_Fit As Double = 0.0 Implements ICompoundConstantProperties.Vapor_Viscosity_Regression_Fit

        Public Property Vapor_Viscosity_Tabular_Data As ITabularData = New TabularData() Implements ICompoundConstantProperties.Vapor_Viscosity_Tabular_Data

        Public Property Solid_Density_Regression_Fit As Double = 0.0 Implements ICompoundConstantProperties.Solid_Density_Regression_Fit

        Public Property Solid_Density_Tabular_Data As ITabularData = New TabularData() Implements ICompoundConstantProperties.Solid_Density_Tabular_Data

        Public Property Surface_Tension_Regression_Fit As Double = 0.0 Implements ICompoundConstantProperties.Surface_Tension_Regression_Fit

        Public Property Surface_Tension_Tabular_Data As ITabularData = New TabularData() Implements ICompoundConstantProperties.Surface_Tension_Tabular_Data

        Public Property Solid_Heat_Capacity_Regression_Fit As Double = 0.0 Implements ICompoundConstantProperties.Solid_Heat_Capacity_Regression_Fit

        Public Property Solid_Heat_Capacity_Tabular_Data As ITabularData = New TabularData() Implements ICompoundConstantProperties.Solid_Heat_Capacity_Tabular_Data

        Public Property Enthalpy_Of_Vaporization_Regression_Fit As Double = 0.0 Implements ICompoundConstantProperties.Enthalpy_Of_Vaporization_Regression_Fit

        Public Property Enthalpy_Of_Vaporization_Tabular_Data As ITabularData = New TabularData() Implements ICompoundConstantProperties.Enthalpy_Of_Vaporization_Tabular_Data

        Public Property StandardHeatOfCombustion_LHV As Double = 0.0 Implements ICompoundConstantProperties.StandardHeatOfCombustion_LHV

        Public Sub ExportToXLSX(filepath As String) Implements ICompoundConstantProperties.ExportToXLSX

            Using xcl As New OfficeOpenXml.ExcelPackage()

                Dim mybook As OfficeOpenXml.ExcelWorkbook = xcl.Workbook

                Dim mysheet As OfficeOpenXml.ExcelWorksheet = mybook.Worksheets.Add("Constant Properties")

                With mysheet

                    .Cells(1, 1).Value = "BASIC PROPERTIES"
                    .Cells(2, 1).Value = "Name"
                    .Cells(3, 1).Value = "Database"
                    .Cells(4, 1).Value = "Type"
                    .Cells(5, 1).Value = "CAS ID"
                    .Cells(6, 1).Value = "Molecular Weight"
                    .Cells(7, 1).Value = "Critical Temperature"
                    .Cells(8, 1).Value = "Critical Pressure"
                    .Cells(9, 1).Value = "Critical Volume"
                    .Cells(10, 1).Value = "Critical Compressibility"
                    .Cells(11, 1).Value = "Acentric Factor"
                    .Cells(12, 1).Value = "Gibbs Energy of Formation (Ideal Gas at 298.15 K)"
                    .Cells(13, 1).Value = "Enthalpy of Formation (Ideal Gas at 298.15 K)"
                    .Cells(14, 1).Value = "Normal Boiling Point"
                    .Cells(15, 1).Value = "Temperature of Fusion"
                    .Cells(16, 1).Value = "Enthalpy of Fusion @ Tf"
                    .Cells(17, 1).Value = "Reference Temperature for Solid Density"
                    .Cells(18, 1).Value = "Solid Density @ Tref"

                    .Cells(20, 1).Value = "MODEL-SPECIFIC PROPERTIES"
                    .Cells(21, 1).Value = "Chao Seader Acentric Factor"
                    .Cells(22, 1).Value = "Chao Seader Solubility Parameter"
                    .Cells(23, 1).Value = "Chao Seader Liquid Molar Volume"
                    .Cells(24, 1).Value = "Rackett Compressibility"
                    .Cells(25, 1).Value = "PR Volume Translation Coefficient"
                    .Cells(26, 1).Value = "SRK Volume Translation Coefficient"
                    .Cells(27, 1).Value = "UNIQUAC R"
                    .Cells(28, 1).Value = "UNIQUAC Q"

                    .Cells(30, 1).Value = "ELECTROLYTE-RELATED PROPERTIES"
                    .Cells(31, 1).Value = "Charge"
                    .Cells(32, 1).Value = "Hydration Number"
                    .Cells(33, 1).Value = "Positive Ion"
                    .Cells(34, 1).Value = "Negative Ion"
                    .Cells(35, 1).Value = "Electrolyte Gibbs Energy of Formation"
                    .Cells(36, 1).Value = "Electrolyte Enthalpy of Formation"
                    .Cells(37, 1).Value = "Standard State Heat Capacity"
                    .Cells(38, 1).Value = "Standard State Molar Volume"

                    .Cells(40, 1).Value = "BLACK-OIL-RELATED PROPERTIES"
                    .Cells(41, 1).Value = "Specific Gravity (Gas)"
                    .Cells(42, 1).Value = "Specific Gravity (Oil)"
                    .Cells(43, 1).Value = "Gas-Oil Ratio"
                    .Cells(44, 1).Value = "Basic Sediments and Water"
                    .Cells(45, 1).Value = "Temperature for Viscosity Data Point 1"
                    .Cells(46, 1).Value = "Viscosity @ T1"
                    .Cells(47, 1).Value = "Temperature for Viscosity Data Point 2"
                    .Cells(48, 1).Value = "Viscosity @ T2"
                    .Cells(49, 1).Value = "PNA - Paraffins"
                    .Cells(50, 1).Value = "PNA - Napthenes"
                    .Cells(51, 1).Value = "PNA - Aromatics"

                    .Cells(53, 1).Value = "PSEUDOCOMPOUND (PETROLEUM FRACTION)-RELATED PROPERTIES"
                    .Cells(54, 1).Value = "Specific Gravity"
                    .Cells(55, 1).Value = "Watson K"
                    .Cells(56, 1).Value = "Temperature for Viscosity Data Point 1"
                    .Cells(57, 1).Value = "Viscosity @ T1"
                    .Cells(58, 1).Value = "Temperature for Viscosity Data Point 2"
                    .Cells(59, 1).Value = "Viscosity @ T2"

                    .Cells(2, 2).Value = Name
                    .Cells(3, 2).Value = OriginalDB

                    If IsBlackOil Then
                        .Cells(4, 2).Value = "Black-Oil"
                    ElseIf IsPF Then
                        .Cells(4, 2).Value = "Petroleum Fraction (Pseudocompound)"
                    ElseIf IsIon Then
                        .Cells(4, 2).Value = "Ion"
                    ElseIf IsSalt Then
                        .Cells(4, 2).Value = "Salt"
                    ElseIf IsHydratedSalt Then
                        .Cells(4, 2).Value = "Hydrated Salt"
                    ElseIf IsHydratedSalt Then
                        .Cells(4, 2).Value = "Default"
                    End If

                    .Cells(5, 2).Value = CAS_Number
                    .Cells(6, 2).Value = Molar_Weight
                    .Cells(7, 2).Value = Critical_Temperature
                    .Cells(8, 2).Value = Critical_Pressure
                    .Cells(9, 2).Value = Critical_Volume
                    .Cells(10, 2).Value = Critical_Compressibility
                    .Cells(11, 2).Value = Acentric_Factor
                    .Cells(12, 2).Value = IG_Gibbs_Energy_of_Formation_25C
                    .Cells(13, 2).Value = IG_Enthalpy_of_Formation_25C
                    .Cells(14, 2).Value = Normal_Boiling_Point
                    .Cells(15, 2).Value = TemperatureOfFusion
                    .Cells(16, 2).Value = EnthalpyOfFusionAtTf
                    .Cells(17, 2).Value = SolidTs
                    .Cells(18, 2).Value = SolidDensityAtTs

                    .Cells(21, 2).Value = Chao_Seader_Acentricity
                    .Cells(22, 2).Value = Chao_Seader_Solubility_Parameter
                    .Cells(23, 2).Value = Chao_Seader_Liquid_Molar_Volume
                    .Cells(24, 2).Value = Z_Rackett
                    .Cells(25, 2).Value = PR_Volume_Translation_Coefficient
                    .Cells(26, 2).Value = SRK_Volume_Translation_Coefficient
                    .Cells(27, 2).Value = UNIQUAC_R
                    .Cells(28, 2).Value = UNIQUAC_Q

                    .Cells(31, 2).Value = Charge
                    .Cells(32, 2).Value = HydrationNumber
                    .Cells(33, 2).Value = PositiveIon
                    .Cells(34, 2).Value = NegativeIon
                    .Cells(35, 2).Value = Electrolyte_DelGF
                    .Cells(36, 2).Value = Electrolyte_DelHF
                    .Cells(37, 2).Value = Electrolyte_Cp0
                    .Cells(38, 2).Value = StandardStateMolarVolume

                    .Cells(41, 2).Value = BO_SGG
                    .Cells(42, 2).Value = BO_SGO
                    .Cells(43, 2).Value = BO_GOR
                    .Cells(44, 2).Value = BO_BSW
                    .Cells(45, 2).Value = BO_OilViscTemp1
                    .Cells(46, 2).Value = BO_OilVisc1
                    .Cells(47, 2).Value = BO_OilViscTemp2
                    .Cells(48, 2).Value = BO_OilVisc2
                    .Cells(49, 2).Value = BO_PNA_P
                    .Cells(50, 2).Value = BO_PNA_N
                    .Cells(51, 2).Value = BO_PNA_A

                    .Cells(54, 2).Value = PF_SG
                    .Cells(55, 2).Value = PF_Watson_K
                    .Cells(56, 2).Value = PF_Tv1
                    .Cells(57, 2).Value = PF_v1
                    .Cells(58, 2).Value = PF_Tv2
                    .Cells(59, 2).Value = PF_v2

                    .Cells(7, 3).Value = "K"
                    .Cells(8, 3).Value = "Pa"
                    .Cells(9, 3).Value = "m3/kmol"
                    .Cells(12, 3).Value = "kJ/kg"
                    .Cells(13, 3).Value = "kJ/kg"
                    .Cells(14, 3).Value = "K"
                    .Cells(15, 3).Value = "K"
                    .Cells(16, 3).Value = "kJ/mol"
                    .Cells(17, 3).Value = "K"
                    .Cells(18, 3).Value = "kg/m3"

                    .Cells(35, 3).Value = "kJ/mol"
                    .Cells(36, 3).Value = "kJ/mol"
                    .Cells(37, 3).Value = "kJ/[mol.K]"
                    .Cells(38, 3).Value = "cm3/mol"

                    .Cells(43, 3).Value = "m3/m3"
                    .Cells(44, 3).Value = "%"
                    .Cells(45, 3).Value = "K"
                    .Cells(46, 3).Value = "Pa.s"
                    .Cells(47, 3).Value = "K"
                    .Cells(48, 3).Value = "Pa.s"
                    .Cells(49, 3).Value = "%"
                    .Cells(50, 3).Value = "%"
                    .Cells(51, 3).Value = "%"

                    .Cells(56, 3).Value = "K"
                    .Cells(57, 3).Value = "Pa.s"
                    .Cells(58, 3).Value = "K"
                    .Cells(59, 3).Value = "Pa.s"

                End With


                mysheet = mybook.Worksheets.Add("T-Dep Properties")

                With mysheet

                    'igcp

                    .Cells(1, 1).Value = "IDEAL GAS HEAT CAPACITY"
                    .Cells(2, 1).Value = "Equation ID"
                    .Cells(3, 1).Value = "Equation String"
                    .Cells(4, 1).Value = "Temperature Units"
                    .Cells(5, 1).Value = "Heat Capacity Units"
                    .Cells(6, 1).Value = "A"
                    .Cells(7, 1).Value = "B"
                    .Cells(8, 1).Value = "C"
                    .Cells(9, 1).Value = "D"
                    .Cells(10, 1).Value = "E"
                    .Cells(11, 1).Value = "Tmin"
                    .Cells(12, 1).Value = "Tmax"

                    If Integer.TryParse(IdealgasCpEquation, New Integer) Then
                        .Cells(2, 2).Value = IdealgasCpEquation
                        .Cells(3, 2).Value = PropertyPackages.PropertyPackage.GetEquationString(IdealgasCpEquation)
                    ElseIf IdealgasCpEquation = "" Then
                        .Cells(2, 2).Value = "Estimated"
                        .Cells(3, 2).Value = ""
                    Else
                        .Cells(2, 2).Value = "User-Defined"
                        .Cells(3, 2).Value = IdealgasCpEquation
                    End If
                    .Cells(4, 2).Value = "K"
                    Select Case OriginalDB
                        Case "DWSIM"
                            .Cells(5, 2).Value = "kJ/[kmol.K]"
                        Case "ChemSep", "ChEDL Thermo", "User"
                            .Cells(5, 2).Value = "J/[kmol.K]"
                    End Select
                    .Cells(6, 2).Value = Ideal_Gas_Heat_Capacity_Const_A
                    .Cells(7, 2).Value = Ideal_Gas_Heat_Capacity_Const_B
                    .Cells(8, 2).Value = Ideal_Gas_Heat_Capacity_Const_C
                    .Cells(9, 2).Value = Ideal_Gas_Heat_Capacity_Const_D
                    .Cells(10, 2).Value = Ideal_Gas_Heat_Capacity_Const_E
                    .Cells(11, 2).Value = "N/A"
                    .Cells(12, 2).Value = "N/A"

                    .Cells(14, 1).Value = "TABULATED DATA"
                    .Cells(15, 1).Value = "T (K)"
                    .Cells(15, 2).Value = "IG Cp (kJ/[kg.K])"

                    Dim Tmin, Tmax, Tit As Double
                    If TemperatureOfFusion > 0 Then Tmin = TemperatureOfFusion Else Tmin = 0.3 * Normal_Boiling_Point
                    Tmax = 2 * Critical_Temperature

                    Tit = Tmin
                    Dim i As Integer = 1
                    While Tit <= Tmax
                        .Cells(15 + i, 1).Value = Tit
                        .Cells(15 + i, 2).Value = GetIdealGasHeatCapacity(Tit)
                        Tit += (Tmax - Tmin) / 50.0
                        i += 1
                    End While

                    'vapor pressure

                    .Cells(1, 4).Value = "VAPOR PRESSURE"
                    .Cells(2, 4).Value = "Equation ID"
                    .Cells(3, 4).Value = "Equation String"
                    .Cells(4, 4).Value = "Temperature Units"
                    .Cells(5, 4).Value = "Vapor Pressure Units"
                    .Cells(6, 4).Value = "A"
                    .Cells(7, 4).Value = "B"
                    .Cells(8, 4).Value = "C"
                    .Cells(9, 4).Value = "D"
                    .Cells(10, 4).Value = "E"
                    .Cells(11, 4).Value = "Tmin"
                    .Cells(12, 4).Value = "Tmax"

                    If Integer.TryParse(VaporPressureEquation, New Integer) Then
                        .Cells(2, 5).Value = VaporPressureEquation
                        .Cells(3, 5).Value = PropertyPackages.PropertyPackage.GetEquationString(VaporPressureEquation)
                    ElseIf VaporPressureEquation = "" Then
                        .Cells(2, 5).Value = "Estimated"
                        .Cells(3, 5).Value = "Lee-Kesler Vapor Pressure Correlation"
                    Else
                        .Cells(2, 5).Value = "User-Defined"
                        .Cells(3, 5).Value = VaporPressureEquation
                    End If
                    .Cells(4, 5).Value = "K"
                    .Cells(5, 5).Value = "Pa"
                    .Cells(6, 5).Value = Vapor_Pressure_Constant_A
                    .Cells(7, 5).Value = Vapor_Pressure_Constant_B
                    .Cells(8, 5).Value = Vapor_Pressure_Constant_C
                    .Cells(9, 5).Value = Vapor_Pressure_Constant_D
                    .Cells(10, 5).Value = Vapor_Pressure_Constant_E
                    .Cells(11, 5).Value = Vapor_Pressure_TMIN
                    .Cells(12, 5).Value = Vapor_Pressure_TMAX

                    .Cells(14, 4).Value = "TABULATED DATA"
                    .Cells(15, 4).Value = "T (K)"
                    .Cells(15, 5).Value = "Vapor Pressure (Pa)"

                    If TemperatureOfFusion > 0 Then Tmin = TemperatureOfFusion Else Tmin = 0.3 * Normal_Boiling_Point
                    Tmax = Critical_Temperature

                    Tit = Tmin
                    i = 1
                    While Tit <= Tmax
                        .Cells(15 + i, 4).Value = Tit
                        .Cells(15 + i, 5).Value = GetVaporPressure(Tit)
                        Tit += (Tmax - Tmin) / 50.0
                        i += 1
                    End While

                    'liquid density

                    .Cells(1, 7).Value = "LIQUID DENSITY"
                    .Cells(2, 7).Value = "Equation ID"
                    .Cells(3, 7).Value = "Equation String"
                    .Cells(4, 7).Value = "Temperature Units"
                    .Cells(5, 7).Value = "Liquid Density Units"
                    .Cells(6, 7).Value = "A"
                    .Cells(7, 7).Value = "B"
                    .Cells(8, 7).Value = "C"
                    .Cells(9, 7).Value = "D"
                    .Cells(10, 7).Value = "E"
                    .Cells(11, 7).Value = "Tmin"
                    .Cells(12, 7).Value = "Tmax"

                    If Integer.TryParse(LiquidDensityEquation, New Integer) Then
                        .Cells(2, 8).Value = LiquidDensityEquation
                        .Cells(3, 8).Value = PropertyPackages.PropertyPackage.GetEquationString(LiquidDensityEquation)
                    ElseIf LiquidDensityEquation = "" Then
                        .Cells(2, 8).Value = "Estimated"
                        .Cells(3, 8).Value = "Rackett Correlation"
                    Else
                        .Cells(2, 8).Value = "User-Defined"
                        .Cells(3, 8).Value = LiquidDensityEquation
                    End If
                    .Cells(4, 8).Value = "K"
                    .Cells(5, 8).Value = "kg/m3"
                    .Cells(6, 8).Value = Liquid_Density_Const_A
                    .Cells(7, 8).Value = Liquid_Density_Const_B
                    .Cells(8, 8).Value = Liquid_Density_Const_C
                    .Cells(9, 8).Value = Liquid_Density_Const_D
                    .Cells(10, 8).Value = Liquid_Density_Const_E
                    .Cells(11, 8).Value = Liquid_Density_Tmin
                    .Cells(12, 8).Value = Liquid_Density_Tmax

                    .Cells(14, 7).Value = "TABULATED DATA"
                    .Cells(15, 7).Value = "T (K)"
                    .Cells(15, 8).Value = "Liquid Density (kg/m3)"

                    If TemperatureOfFusion > 0 Then Tmin = TemperatureOfFusion Else Tmin = 0.3 * Normal_Boiling_Point
                    Tmax = Critical_Temperature

                    Tit = Tmin
                    i = 1
                    While Tit <= Tmax
                        .Cells(15 + i, 7).Value = Tit
                        .Cells(15 + i, 8).Value = GetLiquidDensity(Tit)
                        Tit += (Tmax - Tmin) / 50.0
                        i += 1
                    End While

                    'liquid viscosity

                    .Cells(1, 10).Value = "LIQUID VISCOSITY"
                    .Cells(2, 10).Value = "Equation ID"
                    .Cells(3, 10).Value = "Equation String"
                    .Cells(4, 10).Value = "Temperature Units"
                    .Cells(5, 10).Value = "Liquid Viscosity Units"
                    .Cells(6, 10).Value = "A"
                    .Cells(7, 10).Value = "B"
                    .Cells(8, 10).Value = "C"
                    .Cells(9, 10).Value = "D"
                    .Cells(10, 10).Value = "E"
                    .Cells(11, 10).Value = "Tmin"
                    .Cells(12, 10).Value = "Tmax"

                    If Integer.TryParse(LiquidViscosityEquation, New Integer) Then
                        .Cells(2, 11).Value = LiquidViscosityEquation
                        .Cells(3, 11).Value = PropertyPackages.PropertyPackage.GetEquationString(LiquidViscosityEquation)
                    ElseIf LiquidViscosityEquation = "" Then
                        .Cells(2, 11).Value = "Estimated"
                        .Cells(3, 11).Value = "Letsou-Stiel Correlation"
                    Else
                        .Cells(2, 11).Value = "User-Defined"
                        .Cells(3, 11).Value = LiquidViscosityEquation
                    End If
                    .Cells(4, 11).Value = "K"
                    .Cells(5, 11).Value = "Pa.s"
                    .Cells(6, 11).Value = Liquid_Viscosity_Const_A
                    .Cells(7, 11).Value = Liquid_Viscosity_Const_B
                    .Cells(8, 11).Value = Liquid_Viscosity_Const_C
                    .Cells(9, 11).Value = Liquid_Viscosity_Const_D
                    .Cells(10, 11).Value = Liquid_Viscosity_Const_E
                    .Cells(11, 11).Value = "N/A"
                    .Cells(12, 11).Value = "N/A"

                    .Cells(14, 10).Value = "TABULATED DATA"
                    .Cells(15, 10).Value = "T (K)"
                    .Cells(15, 11).Value = "Liquid Viscosity (Pa.s)"

                    If TemperatureOfFusion > 0 Then Tmin = TemperatureOfFusion Else Tmin = 0.3 * Normal_Boiling_Point
                    Tmax = Critical_Temperature

                    Tit = Tmin
                    i = 1
                    While Tit <= Tmax
                        .Cells(15 + i, 10).Value = Tit
                        .Cells(15 + i, 11).Value = GetLiquidViscosity(Tit)
                        Tit += (Tmax - Tmin) / 50.0
                        i += 1
                    End While

                    'liquid heat capacity

                    .Cells(1, 13).Value = "LIQUID HEAT CAPACITY"
                    .Cells(2, 13).Value = "Equation ID"
                    .Cells(3, 13).Value = "Equation String"
                    .Cells(4, 13).Value = "Temperature Units"
                    .Cells(5, 13).Value = "Heat Capacity Units"
                    .Cells(6, 13).Value = "A"
                    .Cells(7, 13).Value = "B"
                    .Cells(8, 13).Value = "C"
                    .Cells(9, 13).Value = "D"
                    .Cells(10, 13).Value = "E"
                    .Cells(11, 13).Value = "Tmin"
                    .Cells(12, 13).Value = "Tmax"

                    If Integer.TryParse(LiquidHeatCapacityEquation, New Integer) Then
                        .Cells(2, 14).Value = LiquidHeatCapacityEquation
                        .Cells(3, 14).Value = PropertyPackages.PropertyPackage.GetEquationString(LiquidHeatCapacityEquation)
                    ElseIf LiquidHeatCapacityEquation = "" Then
                        .Cells(2, 14).Value = "Unavailable"
                        .Cells(3, 14).Value = ""
                    Else
                        .Cells(2, 14).Value = "User-Defined"
                        .Cells(3, 14).Value = LiquidHeatCapacityEquation
                    End If
                    .Cells(4, 14).Value = "K"
                    Select Case OriginalDB
                        Case "DWSIM"
                            .Cells(5, 14).Value = "kJ/[kmol.K]"
                        Case "ChemSep", "ChEDL Thermo", "User"
                            .Cells(5, 14).Value = "J/[kmol.K]"
                    End Select
                    .Cells(6, 14).Value = Liquid_Heat_Capacity_Const_A
                    .Cells(7, 14).Value = Liquid_Heat_Capacity_Const_B
                    .Cells(8, 14).Value = Liquid_Heat_Capacity_Const_C
                    .Cells(9, 14).Value = Liquid_Heat_Capacity_Const_D
                    .Cells(10, 14).Value = Liquid_Heat_Capacity_Const_E
                    .Cells(11, 14).Value = "N/A"
                    .Cells(12, 14).Value = "N/A"

                    .Cells(14, 13).Value = "TABULATED DATA"
                    .Cells(15, 13).Value = "T (K)"
                    .Cells(15, 14).Value = "Liquid Cp (kJ/[kg.K])"

                    If TemperatureOfFusion > 0 Then Tmin = TemperatureOfFusion Else Tmin = 0.3 * Normal_Boiling_Point
                    Tmax = 2 * Critical_Temperature

                    Tit = Tmin
                    i = 1
                    While Tit <= Tmax
                        .Cells(15 + i, 13).Value = Tit
                        .Cells(15 + i, 14).Value = GetLiquidHeatCapacity(Tit)
                        Tit += (Tmax - Tmin) / 50.0
                        i += 1
                    End While

                    'liquid thermal conductivity

                    .Cells(1, 16).Value = "LIQUID THERMAL CONDUCTIVITY"
                    .Cells(2, 16).Value = "Equation ID"
                    .Cells(3, 16).Value = "Equation String"
                    .Cells(4, 16).Value = "Temperature Units"
                    .Cells(5, 16).Value = "Thermal Conductivity Units"
                    .Cells(6, 16).Value = "A"
                    .Cells(7, 16).Value = "B"
                    .Cells(8, 16).Value = "C"
                    .Cells(9, 16).Value = "D"
                    .Cells(10, 16).Value = "E"
                    .Cells(11, 16).Value = "Tmin"
                    .Cells(12, 16).Value = "Tmax"

                    If Integer.TryParse(LiquidThermalConductivityEquation, New Integer) Then
                        .Cells(2, 17).Value = LiquidThermalConductivityEquation
                        .Cells(3, 17).Value = PropertyPackages.PropertyPackage.GetEquationString(LiquidThermalConductivityEquation)
                    ElseIf LiquidThermalConductivityEquation = "" Then
                        .Cells(2, 17).Value = "Estimated"
                        .Cells(3, 17).Value = "Latini Correlation"
                    Else
                        .Cells(2, 17).Value = "User-Defined"
                        .Cells(3, 17).Value = LiquidThermalConductivityEquation
                    End If
                    .Cells(4, 17).Value = "K"
                    .Cells(5, 17).Value = "W/[m.K]"
                    .Cells(6, 17).Value = Liquid_Thermal_Conductivity_Const_A
                    .Cells(7, 17).Value = Liquid_Thermal_Conductivity_Const_B
                    .Cells(8, 17).Value = Liquid_Thermal_Conductivity_Const_C
                    .Cells(9, 17).Value = Liquid_Thermal_Conductivity_Const_D
                    .Cells(10, 17).Value = Liquid_Thermal_Conductivity_Const_E
                    .Cells(11, 17).Value = Liquid_Thermal_Conductivity_Tmin
                    .Cells(12, 17).Value = Liquid_Thermal_Conductivity_Tmax

                    .Cells(14, 16).Value = "TABULATED DATA"
                    .Cells(15, 16).Value = "T (K)"
                    .Cells(15, 17).Value = "Liquid Thermal Conductivity (W/[m.K])"

                    If TemperatureOfFusion > 0 Then Tmin = TemperatureOfFusion Else Tmin = 0.3 * Normal_Boiling_Point
                    Tmax = 2 * Critical_Temperature

                    Tit = Tmin
                    i = 1
                    While Tit <= Tmax
                        .Cells(15 + i, 16).Value = Tit
                        .Cells(15 + i, 17).Value = GetLiquidThermalConductivity(Tit)
                        Tit += (Tmax - Tmin) / 50.0
                        i += 1
                    End While

                    'solid density

                    .Cells(1, 19).Value = "SOLID DENSITY"
                    .Cells(2, 19).Value = "Equation ID"
                    .Cells(3, 19).Value = "Equation String"
                    .Cells(4, 19).Value = "Temperature Units"
                    .Cells(5, 19).Value = "Solid Density Units"
                    .Cells(6, 19).Value = "A"
                    .Cells(7, 19).Value = "B"
                    .Cells(8, 19).Value = "C"
                    .Cells(9, 19).Value = "D"
                    .Cells(10, 19).Value = "E"
                    .Cells(11, 19).Value = "Tmin"
                    .Cells(12, 19).Value = "Tmax"

                    If Integer.TryParse(SolidDensityEquation, New Integer) Then
                        .Cells(2, 20).Value = SolidDensityEquation
                        .Cells(3, 20).Value = PropertyPackages.PropertyPackage.GetEquationString(SolidDensityEquation)
                    ElseIf SolidDensityEquation = "" Then
                        .Cells(2, 20).Value = "Estimated"
                        .Cells(3, 20).Value = ""
                    Else
                        .Cells(2, 20).Value = "User-Defined"
                        .Cells(3, 20).Value = SolidDensityEquation
                    End If
                    .Cells(4, 20).Value = "K"
                    .Cells(5, 20).Value = "kg/m3"
                    .Cells(6, 20).Value = Solid_Density_Const_A
                    .Cells(7, 20).Value = Solid_Density_Const_B
                    .Cells(8, 20).Value = Solid_Density_Const_C
                    .Cells(9, 20).Value = Solid_Density_Const_D
                    .Cells(10, 20).Value = Solid_Density_Const_E
                    .Cells(11, 20).Value = Solid_Density_Tmin
                    .Cells(12, 20).Value = Solid_Density_Tmax

                    .Cells(14, 19).Value = "TABULATED DATA"
                    .Cells(15, 19).Value = "T (K)"
                    .Cells(15, 20).Value = "Solid Density (kg/m3)"

                    If TemperatureOfFusion > 0 Then Tmax = TemperatureOfFusion Else Tmax = 0.3 * Normal_Boiling_Point
                    Tmin = Tmax * 0.2

                    Tit = Tmin
                    i = 1
                    While Tit <= Tmax
                        .Cells(15 + i, 19).Value = Tit
                        .Cells(15 + i, 20).Value = GetSolidDensity(Tit)
                        Tit += (Tmax - Tmin) / 50.0
                        i += 1
                    End While

                    'solid heat capacity

                    .Cells(1, 22).Value = "SOLID HEAT CAPACITY"
                    .Cells(2, 22).Value = "Equation ID"
                    .Cells(3, 22).Value = "Equation String"
                    .Cells(4, 22).Value = "Temperature Units"
                    .Cells(5, 22).Value = "Heat Capacity Units"
                    .Cells(6, 22).Value = "A"
                    .Cells(7, 22).Value = "B"
                    .Cells(8, 22).Value = "C"
                    .Cells(9, 22).Value = "D"
                    .Cells(10, 22).Value = "E"
                    .Cells(11, 22).Value = "Tmin"
                    .Cells(12, 22).Value = "Tmax"

                    If Integer.TryParse(SolidHeatCapacityEquation, New Integer) Then
                        .Cells(2, 23).Value = SolidHeatCapacityEquation
                        .Cells(3, 23).Value = PropertyPackages.PropertyPackage.GetEquationString(SolidHeatCapacityEquation)
                    ElseIf SolidHeatCapacityEquation = "" Then
                        .Cells(2, 23).Value = "Estimated"
                        .Cells(3, 23).Value = ""
                    Else
                        .Cells(2, 23).Value = "User-Defined"
                        .Cells(3, 23).Value = SolidHeatCapacityEquation
                    End If
                    .Cells(4, 23).Value = "K"
                    Select Case OriginalDB
                        Case "ChemSep", "User"
                            .Cells(5, 23).Value = "J/[kmol.K]"
                        Case "ChEDL Thermo"
                            .Cells(5, 23).Value = "kJ/[kg.K]"
                    End Select
                    .Cells(6, 23).Value = Solid_Heat_Capacity_Const_A
                    .Cells(7, 23).Value = Solid_Heat_Capacity_Const_B
                    .Cells(8, 23).Value = Solid_Heat_Capacity_Const_C
                    .Cells(9, 23).Value = Solid_Heat_Capacity_Const_D
                    .Cells(10, 23).Value = Solid_Heat_Capacity_Const_E
                    .Cells(11, 23).Value = Solid_Heat_Capacity_Tmin
                    .Cells(12, 23).Value = Solid_Heat_Capacity_Tmax

                    .Cells(14, 22).Value = "TABULATED DATA"
                    .Cells(15, 22).Value = "T (K)"
                    .Cells(15, 23).Value = "Solid Cp (kJ/[kg.K])"

                    If TemperatureOfFusion > 0 Then Tmax = TemperatureOfFusion Else Tmax = 0.3 * Normal_Boiling_Point
                    Tmin = Tmax * 0.2

                    Tit = Tmin
                    i = 1
                    While Tit <= Tmax
                        .Cells(15 + i, 22).Value = Tit
                        .Cells(15 + i, 23).Value = GetSolidHeatCapacity(Tit)
                        Tit += (Tmax - Tmin) / 50.0
                        i += 1
                    End While

                End With

                xcl.SaveAs(New FileInfo(filepath))

            End Using

        End Sub

    End Class

    <System.Serializable()> Public Class ConstantPropertiesCollection
        Public Collection() As ConstantProperties
    End Class

#End Region

End Namespace
